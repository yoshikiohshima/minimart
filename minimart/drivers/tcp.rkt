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
;; Driver

(define (spawn-tcp-driver)
  (list (spawn-demand-matcher (tcp-channel ? (?! (tcp-listener ?)) ?)
			      #:demand-level 1
			      #:supply-level 2
			      spawn-tcp-listener)
	(spawn-demand-matcher (tcp-channel (?! (tcp-handle ?)) (?! (tcp-address ? ?)) ?)
			      #:demand-is-subscription? #f
			      spawn-tcp-connection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (listener server-addr) #:transparent)

(define (tcp-listener-behavior e state)
  (match e
    [(routing-update g)
     (match-define (listener-state listener server-addr) state)
     (if (gestalt-empty? (gestalt-filter g (pub (tcp-channel ? server-addr ?) #:level 2)))
	 (begin (when listener (tcp:tcp-close listener))
		(transition (struct-copy listener-state state [listener #f]) (quit)))
	 #f)]
    [(message (event _ (list (list cin cout))) 1 #f)
     (define-values (local-hostname local-port remote-hostname remote-port) (tcp:tcp-addresses cin #t))
     (transition state
		 (spawn-connection (listener-state-server-addr state)
				   (tcp-address remote-hostname remote-port)
				   cin
				   cout))]
    [_ #f]))

(define (spawn-tcp-listener server-addr)
  (match-define (tcp-listener port) server-addr)
  (define listener (tcp:tcp-listen port 4 #t))
  (spawn tcp-listener-behavior
	 (listener-state listener server-addr)
	 (gestalt-union (pub (tcp-channel ? server-addr ?) #:level 2)
			(sub (event (tcp:tcp-accept-evt listener) ?) #:meta-level 1))))

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

(struct connection-state (seen-peer? local-addr remote-addr cin cout) #:transparent)

(define (shutdown-connection state)
  (define cin (connection-state-cin state))
  (define cout (connection-state-cout state))
  (when cin (close-input-port cin))
  (when cout (close-output-port cout))
  (transition (struct-copy connection-state state [cin #f] [cout #f]) (quit)))

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

(define (tcp-connection e state)
  (with-handlers [((lambda (exn) #t)
		   (lambda (exn)
		     (shutdown-connection state)
		     (raise exn)))]
    (match e
      [(message (event _ (list (? eof-object?))) 1 #f)
       (shutdown-connection state)]
      [(message (event _ (list (? bytes? bs))) 1 #f)
       (transition state (send (tcp-channel (connection-state-remote-addr state)
					    (connection-state-local-addr state)
					    bs)))]
      [(message (tcp-channel _ _ bs) 0 #f)
       (write-bytes bs (connection-state-cout state))
       (flush-output (connection-state-cout state))
       #f]
      [(routing-update g)
       (cond
	[(and (connection-state-seen-peer? state) (gestalt-empty? g))
	 (shutdown-connection state)]
	[(and (not (connection-state-seen-peer? state)) (not (gestalt-empty? g)))
	 (define new-state (struct-copy connection-state state [seen-peer? #t]))
	 (transition new-state (routing-update (connection-gestalt new-state)))]
	[else
	 #f])]
      [#f #f])))

(define (connection-gestalt state)
  (match-define (connection-state seen-peer? local-addr remote-addr cin _) state)
  (gestalt-union (pub (tcp-channel remote-addr local-addr ?))
		 (sub (tcp-channel local-addr remote-addr ?))
		 (pub (tcp-channel remote-addr local-addr ?) #:level 1)
		 (if seen-peer?
		     (sub (event (read-bytes-avail-evt 32768 cin) ?) #:meta-level 1)
		     (gestalt-empty))))

(define (spawn-connection local-addr remote-addr cin cout)
  (define state (connection-state #f local-addr remote-addr cin cout))
  (spawn tcp-connection state (connection-gestalt state)))
