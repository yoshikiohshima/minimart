#lang racket/base

(require racket/match)
(require (prefix-in tcp: racket/tcp))
(require (only-in racket/port read-bytes-avail!-evt))
(require (only-in web-server/private/util exn->string))
(require "../main.rkt")
(require "../demand-matcher.rkt")

(require racket/unit)
(require net/tcp-sig)
(require net/tcp-unit)

(provide (struct-out tcp-address)
	 (struct-out tcp-handle)
	 (struct-out tcp-listener)
	 (struct-out tcp-channel)
	 spawn-tcp-driver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct tcp-address (host port) #:prefab)
(struct tcp-handle (id) #:prefab)
(struct tcp-listener (port) #:prefab)

(struct tcp-channel (source destination subpacket) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct tcp-accepted (remote-addr local-addr cin cout) #:prefab)
;;      tcp-channel does double-duty as a ground-level message as well

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (spawn-tcp-driver)
  (list (spawn-demand-matcher (tcp-channel ? (?! (tcp-listener ?)) ?)
			      #:demand-is-subscription? #t
			      #:demand-level 1
			      #:supply-level 2
			      spawn-tcp-listener)
	(spawn-demand-matcher (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?)
			      spawn-tcp-connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (control-ch server-addr) #:transparent)

(define (tcp-listener-thread control-ch listener server-addr)
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt (tcp:tcp-accept-evt listener)
			  (lambda (cin+cout)
			    (match-define (list cin cout) cin+cout)
			    (define-values (local-hostname local-port remote-hostname remote-port)
			      (tcp:tcp-addresses cin #t))
			    (send-ground-message
			     (tcp-accepted (tcp-address remote-hostname remote-port)
					   server-addr
					   cin
					   cout))
			    (loop blocked?))))))
  (tcp:tcp-close listener))

(define (tcp-listener-behavior e state)
  (match e
    [(routing-update g)
     (match-define (listener-state control-ch server-addr) state)
     (and control-ch
	  (if (gestalt-empty? (gestalt-filter g (pub (tcp-channel ? server-addr ?) #:level 2)))
	      (begin (channel-put control-ch 'quit)
		     (transition (struct-copy listener-state state [control-ch #f]) (quit)))
	      (begin (channel-put control-ch 'unblock)
		     #f)))]
    [(message (tcp-accepted remote-addr _ cin cout) 1 #f)
     (transition state (spawn-connection (listener-state-server-addr state)
					 remote-addr
					 cin
					 cout))]
    [_ #f]))

(define (spawn-tcp-listener server-addr)
  (match-define (tcp-listener port) server-addr)
  (define listener (tcp:tcp-listen port 128 #t))
  (define control-ch (make-channel))
  (thread (lambda () (tcp-listener-thread control-ch listener server-addr)))
  (spawn tcp-listener-behavior
	 (listener-state control-ch server-addr)
	 (gestalt-union (pub (tcp-channel ? server-addr ?) #:level 2)
			(sub (tcp-accepted ? server-addr ? ?) #:meta-level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Outbound Connection

(define (spawn-tcp-connection local-addr remote-addr)
  (match-define (tcp-address remote-hostname remote-port) remote-addr)
  (define-values (cin cout)
    (with-handlers ([exn:fail:network? (lambda (e)
					 ;; TODO: it'd be nice to
					 ;; somehow communicate the
					 ;; actual error to the local
					 ;; peer.
					 (log-error "~a" (exn->string e))
					 (define o (open-output-string))
					 (close-output-port o)
					 (values (open-input-string "")
						 o))])
      (tcp:tcp-connect remote-hostname remote-port)))
  (spawn-connection local-addr remote-addr cin cout))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection

(struct connection-state (seen-peer? control-ch cout) #:transparent)

(define (read-bytes-avail-evt len input-port)
  (guard-evt
   (lambda ()
     (let ([bstr (make-bytes len)])
       (handle-evt
        (read-bytes-avail!-evt bstr input-port)
        (lambda (v)
          (if (number? v)
              (if (= v len) bstr (subbytes bstr 0 v))
              v)))))))

(define (tcp-connection-thread remote-addr local-addr control-ch cin)
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt (read-bytes-avail-evt 32768 cin)
			  (lambda (eof-or-bs)
			    (send-ground-message (tcp-channel remote-addr local-addr eof-or-bs))
			    (loop (or blocked? (eof-object? eof-or-bs))))))))
  (close-input-port cin))

(define (shutdown-connection state)
  (match-define (connection-state _ control-ch cout) state)
  (when control-ch (channel-put control-ch 'quit))
  (when cout (close-output-port cout))
  (transition (struct-copy connection-state state [control-ch #f] [cout #f]) (quit)))

(define (tcp-connection e state)
  (with-handlers [((lambda (exn) #t)
		   (lambda (exn)
		     (shutdown-connection state)
		     (raise exn)))]
    (match e
      [(message (tcp-channel remote-addr local-addr (? eof-object?)) 1 #f)
       (shutdown-connection state)]
      [(message (tcp-channel remote-addr local-addr (? bytes? bs)) 1 #f)
       (transition state (send (tcp-channel remote-addr local-addr bs)))]
      [(message (tcp-channel _ _ bs) 0 #f)
       (write-bytes bs (connection-state-cout state))
       (flush-output (connection-state-cout state))
       #f]
      [(routing-update g)
       (cond
	[(and (connection-state-seen-peer? state) (gestalt-empty? g))
	 (shutdown-connection state)]
	[(and (not (connection-state-seen-peer? state)) (not (gestalt-empty? g)))
	 (channel-put (connection-state-control-ch state) 'unblock)
	 (transition (struct-copy connection-state state [seen-peer? #t]) '())]
	[else
	 #f])]
      [#f #f])))

(define (spawn-connection local-addr remote-addr cin cout)
  (define control-ch (make-channel))
  (thread (lambda () (tcp-connection-thread remote-addr local-addr control-ch cin)))
  (spawn tcp-connection
	 (connection-state #f control-ch cout)
	 (gestalt-union (pub (tcp-channel remote-addr local-addr ?))
			(sub (tcp-channel local-addr remote-addr ?))
			(pub (tcp-channel remote-addr local-addr ?) #:level 1)
			(sub (tcp-channel remote-addr local-addr ?) #:meta-level 1))))
