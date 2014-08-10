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
(require net/url)

(provide (struct-out websocket-remote-client)
	 (struct-out websocket-local-server)
	 (struct-out websocket-local-client)
	 (struct-out websocket-remote-server)
	 (struct-out websocket-ssl-options)
	 (struct-out websocket-message)
	 spawn-websocket-driver)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol messages

(struct websocket-remote-client (id) #:prefab)
(struct websocket-local-server (port ssl-options) #:prefab)
(struct websocket-local-client (id) #:prefab)
(struct websocket-remote-server (url) #:prefab)
(struct websocket-ssl-options (cert-file key-file) #:prefab)
(struct websocket-message (from to body) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ground-level communication messages

(struct websocket-accepted (id server-addr connection control-ch) #:prefab)
(struct websocket-incoming-message (id message) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Driver

(define (spawn-websocket-driver)
  (list
   (spawn-demand-matcher (websocket-message ? (?! (websocket-local-server ? ?)) ?)
			 #:demand-is-subscription? #t
			 #:demand-level 1
			 #:supply-level 2
			 spawn-websocket-listener)
   (spawn-demand-matcher (websocket-message (?! (websocket-local-client ?))
					    (?! (websocket-remote-server ?))
					    ?)
			 spawn-websocket-connection)))

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
    [(message (websocket-accepted id _ c control-ch) 1 #f)
     (transition state
		 (spawn-connection (listener-state-server-addr state)
				   (websocket-remote-client id)
				   id
				   c
				   control-ch))]
    [_ #f]))

(define ((connection-handler server-addr) c dummy-state)
  (define control-ch (make-channel))
  (define id (gensym 'ws))
  (send-ground-message (websocket-accepted id server-addr c control-ch))
  (connection-thread-loop control-ch c id))

(define (connection-thread-loop control-ch c id)
  (define c-input-port (ws-conn-base-ip c))
  (let loop ((blocked? #t))
    (sync (handle-evt control-ch
		      (match-lambda
		       ['unblock (loop #f)]
		       ['quit (void)]))
	  (if blocked?
	      never-evt
	      (handle-evt c-input-port
			  (lambda (dummy)
			    (define msg
			      (with-handlers ([exn:fail:network? (lambda (e) eof)])
				(ws-recv c #:payload-type 'text)))
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
				       (connection-handler server-addr)))
  (spawn websocket-listener
	 (listener-state shutdown-procedure server-addr)
	 (gestalt-union (pub (websocket-message ? server-addr ?) #:level 2)
			(sub (websocket-accepted ? server-addr ? ?) #:meta-level 1))))

(define (spawn-websocket-connection local-addr remote-addr)
  (match-define (websocket-remote-server url) remote-addr)
  (define c (ws-connect (string->url url)))
  (define control-ch (make-channel))
  (define id (gensym 'ws))
  (thread (lambda () (connection-thread-loop control-ch c id)))
  (spawn-connection local-addr remote-addr id c control-ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connection

(struct connection-state (seen-peer? local-addr remote-addr c control-ch) #:transparent)

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
	   (transition state (send (websocket-message (connection-state-remote-addr state)
						      (connection-state-local-addr state)
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

(define (spawn-connection local-addr remote-addr id c control-ch)
  (spawn websocket-connection
	 (connection-state #f local-addr remote-addr c control-ch)
	 (gestalt-union (pub (websocket-message remote-addr local-addr ?))
			(sub (websocket-message local-addr remote-addr ?))
			(sub (websocket-message local-addr remote-addr ?) #:level 1)
			(sub (websocket-incoming-message id ?) #:meta-level 1))))
