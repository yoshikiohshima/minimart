#lang minimart

(require "../drivers/websocket.rkt")
(require "../demand-matcher.rkt")

(spawn-websocket-driver)

(define any-client (websocket-remote-client ?))
(define server-id (websocket-local-server 8081 (websocket-ssl-options "server-cert.pem"
								      "private-key.pem")))

(define (spawn-connection-handler c)
  (log-info "spawn-connection-handler ~v" c)
  (define (connection-handler e n)
    (when e (log-info "connection-handler ~v: ~v /// ~v" c e n))
    (match e
      [(routing-update (? gestalt-empty?)) (transition n (quit))]
      [_
       (if (< n 20)
	   (transition (+ n 1) (send (websocket-message server-id c (format "msg ~v" n))))
	   #f)]))
  (spawn connection-handler
	 0
	 (gestalt-union (sub (websocket-message c server-id ?))
			(sub (websocket-message c server-id ?) #:level 1)
			(pub (websocket-message server-id c ?)))))

(spawn-demand-matcher (websocket-message (?! any-client) server-id ?)
		      #:demand-is-subscription? #f
		      spawn-connection-handler
		      (lambda (c)
			(log-info "Connection handler ~v decided to exit" c)
			'()))
