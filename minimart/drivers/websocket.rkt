#lang racket/base

(require racket/match)
(require net/rfc6455)
(require (only-in net/rfc6455/conn-api ws-conn-base-ip))
(require "../main.rkt")
(require "../demand-matcher.rkt")

(require racket/unit)
(require net/tcp-sig)
(require net/tcp-unit)
(require net/ssl-tcp-unit)

(provide (struct-out websocket-remote-client)
	 (struct-out websocket-local-server)
	 (struct-out websocket-ssl-options)
	 (struct-out websocket-message)
	 spawn-websocket-driver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct websocket-remote-client (id) #:prefab)
(struct websocket-local-server (port ssl-options) #:prefab)
(struct websocket-ssl-options (cert-file key-file) #:prefab)
(struct websocket-message (from to body) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct websocket-accepted (id connection control-ch) #:prefab)
(struct websocket-incoming-message (id message) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (spawn-websocket-driver)
  (spawn-demand-matcher (websocket-message ? (?! (websocket-local-server ? ?)) ?)
			#:demand-is-subscription? #t
			#:demand-level 1
			#:supply-level 2
			spawn-websocket-listener))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Listener

(struct listener-state (shutdown-procedure server-addr) #:transparent)

(define (websocket-listener e state)
  (match e
    [(routing-update g)
     (match-define (listener-state shutdown-procedure server-addr) state)
     (if (gestalt-empty? (gestalt-filter g (pub (websocket-message ? server-addr ?) #:level 2)))
	 (begin (when shutdown-procedure (shutdown-procedure))
		(transition (struct-copy listener-state state [shutdown-procedure #f]) (quit)))
	 #f)]
    [(message (websocket-accepted id c control-ch) 1 #f)
     (transition state
		 (spawn-connection (listener-state-server-addr state) id c control-ch))]
    [_ #f]))

(define (connection-handler c dummy-state)
  (define control-ch (make-channel))
  (define c-input-port (ws-conn-base-ip c))
  (define id (gensym 'ws))
  (send-ground-message (websocket-accepted id c control-ch))
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt c-input-port
			  (lambda (dummy)
			    (define msg (ws-recv c #:payload-type 'text))
			    (send-ground-message (websocket-incoming-message id msg))
			    (loop (or blocked? (eof-object? msg))))))))
  (ws-close! c))

(define (ssl-options->ssl-tcp@ ssl-options)
  (match-define (websocket-ssl-options cert-file key-file) ssl-options)
  (define-unit-binding ssl-tcp@
    (make-ssl-tcp@ cert-file key-file #f #f #f #f #f)
    (import)
    (export tcp^))
  ssl-tcp@)

(define (spawn-websocket-listener server-addr)
  (match-define (websocket-local-server port ssl-options) server-addr)
  (define shutdown-procedure (ws-serve #:port port
				       #:tcp@ (if ssl-options
						  (ssl-options->ssl-tcp@ ssl-options)
						  tcp@)
				       connection-handler))
  (spawn websocket-listener
	 (listener-state shutdown-procedure server-addr)
	 (gestalt-union (pub (websocket-message ? server-addr ?) #:level 2)
			(sub (websocket-accepted ? ? ?) #:meta-level 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection

(struct connection-state (seen-peer? local-addr server-addr c control-ch) #:transparent)

(define (shutdown-connection state)
  (transition (match (connection-state-control-ch state)
		[#f state]
		[ch (channel-put ch 'quit)
		    (struct-copy connection-state state [control-ch #f])])
	      (quit)))

(define (websocket-connection e state)
  (with-handlers [((lambda (exn) #t)
		   (lambda (exn) (shutdown-connection state)))]
    (match e
      [(message (websocket-incoming-message _ m) 1 #f)
       (if (eof-object? m)
	   (shutdown-connection state)
	   (transition state (send (websocket-message (connection-state-local-addr state)
						      (connection-state-server-addr state)
						      m))))]
      [(message (websocket-message _ _ m) 0 #f)
       (ws-send! (connection-state-c state) m)
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

(define (spawn-connection server-addr id c control-ch)
  (define local-addr (websocket-remote-client id))
  (spawn websocket-connection
	 (connection-state #f local-addr server-addr c control-ch)
	 (gestalt-union (pub (websocket-message local-addr server-addr ?))
			(sub (websocket-message server-addr local-addr ?))
			(sub (websocket-message server-addr local-addr ?) #:level 1)
			(sub (websocket-incoming-message id ?) #:meta-level 1))))
