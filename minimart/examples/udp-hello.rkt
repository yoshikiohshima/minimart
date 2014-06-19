#lang minimart

(require "../drivers/udp.rkt")

(spawn-udp-driver)

(spawn (lambda (e s)
	 (match e
	   [(message (udp-packet src dst body) _ _)
	    (log-info "Got packet from ~v: ~v" src body)
	    (transition s (send (udp-packet dst
					    src
					    (string->bytes/utf-8 (format "You said: ~a" body)))))]
	   [_ #f]))
       (void)
       (gestalt-union (sub (udp-packet ? (udp-listener 5999) ?))))
