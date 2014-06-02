#lang minimart

(require (only-in racket/port read-bytes-line-evt))
(require "../drivers/tcp.rkt")
(require "../userland.rkt")

(define local-handle (tcp-handle 'chat))
(define remote-handle (tcp-address "localhost" 5999))

(spawn-tcp-driver)

(userland-thread
 #:gestalt (gestalt-union (sub (event (read-bytes-line-evt (current-input-port) 'any) ?) #:meta-level 1)
			  (sub (tcp-channel remote-handle local-handle ?))
			  (pub (tcp-channel local-handle remote-handle ?))
			  (pub (tcp-channel local-handle remote-handle ?) #:level 1))
 (wait-for-gestalt (pub (tcp-channel local-handle remote-handle ?) #:level 1))
 (let loop ()
   (match (next-event)
     [(message (event _ (list (? eof-object?))) 1 #f) (do (quit))]
     [(message (event _ (list line)) 1 #f) (do (send (tcp-channel local-handle remote-handle line)))]
     [(message (tcp-channel _ _ bs) 0 #f) (write-bytes bs) (flush-output)]
     [(routing-update g) (when (gestalt-empty? g) (do (quit)))])
   (loop)))
