#lang minimart

(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(spawn-tcp-driver)

(define server-id (tcp-listener 5999))

(define (spawn-connection-handler c)
  (log-info "spawn-connection-handler ~v" c)
  (define (connection-handler e n)
    (when e (log-info "connection-handler ~v: ~v /// ~v" c e n))
    (match e
      [(routing-update (? gestalt-empty?)) (transition n (quit))]
      [_
       (if (< n 20)
	   (transition (+ n 1) (send (tcp-channel server-id c (string->bytes/utf-8 (format "msg ~v\n" n)))))
	   #f)]))
  (spawn connection-handler
	 0
	 (gestalt-union (sub (tcp-channel c server-id ?))
			(sub (tcp-channel c server-id ?) #:level 1)
			(pub (tcp-channel server-id c ?)))))

(spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) server-id ?)
		      spawn-connection-handler
		      (lambda (c)
			(log-info "Connection handler ~v decided to exit" c)
			'()))
