#lang minimart

(require racket/set)
(require (only-in racket/string string-trim))
(require "../drivers/tcp.rkt")
(require "../demand-matcher.rkt")

(struct session (seen-remote? peers) #:transparent)

(define (spawn-session them us)
  (define user (gensym 'user))
  (define remote-detector (compile-gestalt-projection (?!)))
  (define peer-detector (compile-gestalt-projection `(,(?!) says ,?)))
  (define (send-to-remote fmt . vs)
    (send #:meta-level 1 (tcp-channel us them (string->bytes/utf-8 (apply format fmt vs)))))
  (define (say who fmt . vs)
    (unless (equal? who user) (send-to-remote "~a ~a\n" who (apply format fmt vs))))
  (list (send-to-remote "Welcome, ~a.\n" user)
	(spawn (lambda (e state)
		 (match e
		   [(message (tcp-channel _ _ bs) 1 #f)
		    (transition state (send `(,user says ,(string-trim (bytes->string/utf-8 bs)))))]
		   [(message `(,who says ,what) 0 #f)
		    (transition state (say who "says: ~a" what))]
		   [(routing-update g)
		    (match-define (session seen-remote? old-peers) state)
		    (define remote-present?
		      (not (matcher-empty? (gestalt-project g 1 0 #f remote-detector))))
		    (define new-peers (matcher-key-set/single (gestalt-project g 0 0 #t peer-detector)))
		    (transition
		     (struct-copy session state
		       [seen-remote? (or remote-present? seen-remote?)]
		       [peers new-peers])
		     (list (when (and seen-remote? (not remote-present?)) (quit))
			   (for/list [(who (set-subtract new-peers old-peers))] (say who "arrived."))
			   (for/list [(who (set-subtract old-peers new-peers))] (say who "departed."))))]
		   [#f #f]))
	       (session #f (set))
	       (gestalt-union (sub `(,? says ,?))
			      (sub `(,? says ,?) #:level 1)
			      (pub `(,user says ,?))
			      (sub (tcp-channel them us ?) #:meta-level 1)
			      (pub (tcp-channel us them ?) #:meta-level 1)
			      (pub (tcp-channel us them ?) #:meta-level 1 #:level 1)))))

(spawn-tcp-driver)
(spawn-world
 (spawn-demand-matcher (tcp-channel (?! (tcp-address ? ?)) (?! (tcp-listener 5999)) ?)
		       #:meta-level 1
		       #:demand-is-subscription? #f
		       spawn-session))
