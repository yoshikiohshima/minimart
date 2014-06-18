#lang minimart

(require racket/set)
(require (only-in racket/string string-trim))
(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")
(require "../userland.rkt")

(define (spawn-session them us)
  (define (send-to-remote fmt . vs)
    (do (send #:meta-level 1 (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs))))))

  (define tcp-gestalt (gestalt-union (pub (tcp-channel us them ?) #:meta-level 1)
				     (sub (tcp-channel them us ?) #:meta-level 1 #:level 1)
				     (sub (tcp-channel them us ?) #:meta-level 1)))

  (define (decode-input bs) (string-trim (bytes->string/utf-8 bs)))
  (define (read-chunk) (receive [(message (tcp-channel _ _ bs) 1 #f) (decode-input bs)]))

  (userland-thread #:gestalt tcp-gestalt

   (wait-for-gestalt tcp-gestalt)
   (send-to-remote "What is your name? > ")
   (define user (read-chunk))
   (send-to-remote "Welcome, ~a.\n" user)

   (do (routing-update (gestalt-union tcp-gestalt
				      (sub `(,? says ,?) #:level 1)
				      (sub `(,? says ,?))
				      (pub `(,user says ,?)))))

   (define (say who fmt . vs)
     (unless (equal? who user)
       (send-to-remote "~a ~a\n" who (apply format fmt vs))))

   (let loop ((old-peers (set)))
     (match (next-event)
       [(message (tcp-channel _ _ bs) 1 #f)
	(do (send `(,user says ,(decode-input bs))))
	(loop old-peers)]
       [(message `(,who says ,what) 0 #f)
	(say who "says: ~a" what)
	(loop old-peers)]
       [(routing-update g)
	(when (gestalt-empty? (gestalt-filter g tcp-gestalt)) (do (quit)))
	(define new-peers (gestalt-project/single g (project-pubs `(,(?!) says ,?))))
	(for/list [(who (set-subtract new-peers old-peers))] (say who "arrived."))
	(for/list [(who (set-subtract old-peers new-peers))] (say who "departed."))
	(loop new-peers)]))))

(spawn-tcp-driver)
(spawn-world
 (spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 5999)) ?)
		       #:meta-level 1
		       spawn-session))
