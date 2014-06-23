#lang minimart

(require (only-in racket/port read-bytes-line-evt))
(require "../drivers/tcp.rkt")

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))

(spawn-tcp-driver)

(actor #:name chat-client
       #:state [seen-remote? #f]

       (define read-evt (read-bytes-line-evt (current-input-port) 'any))
       (subscribe (event read-evt (list ($ line)))
	 #:meta-level 1
	 (if (eof-object? line)
	     (quit)
	     (send (tcp-channel local-handle remote-handle line))))

       (advertise (tcp-channel local-handle remote-handle ?))
       (subscribe (tcp-channel remote-handle local-handle ($ bs))
	 (write-bytes bs)
	 (flush-output))

       (observe-advertisers (tcp-channel remote-handle local-handle ?)
	 #:presence remote-present?
	 (when (and (not remote-present?) seen-remote?) (quit))
	 #:update [seen-remote? (or seen-remote? remote-present?)]))
