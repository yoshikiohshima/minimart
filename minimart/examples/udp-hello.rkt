#lang minimart

(require "../drivers/udp.rkt")

(spawn-udp-driver)

(actor #:name echoer
       (subscribe (udp-packet ($ src) ($ dst (udp-listener 5999)) ($ body))
	 (log-info "Got packet from ~v: ~v" src body)
	 (define reply (string->bytes/utf-8 (format "You said: ~a" body)))
	 (send (udp-packet dst src reply))))
