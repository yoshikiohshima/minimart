#lang minimart

(require (only-in racket/port read-bytes-line-evt))
(require "../drivers/tcp.rkt")

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))

(spawn-tcp-driver)

(spawn (lambda (e seen-remote?)
	 (match e
	   [(message (event _ (list (? eof-object?))) 1 #f)
	    (transition seen-remote? (quit))]
	   [(message (event _ (list line)) 1 #f)
	    (transition seen-remote? (send (tcp-channel local-handle remote-handle line)))]
	   [(message (tcp-channel _ _ bs) 0 #f)
	    (write-bytes bs)
	    (flush-output)
	    #f]
	   [(routing-update g)
	    (define remote-present? (not (gestalt-empty? g)))
	    (transition (or seen-remote? remote-present?)
			(when (and (not remote-present?) seen-remote?) (quit)))]
	   [#f #f]))
       #f
       (gestalt-union (sub (event (read-bytes-line-evt (current-input-port) 'any) ?) #:meta-level 1)
		      (sub (tcp-channel remote-handle local-handle ?))
		      (pub (tcp-channel local-handle remote-handle ?))
		      (pub (tcp-channel local-handle remote-handle ?) #:level 1)))
