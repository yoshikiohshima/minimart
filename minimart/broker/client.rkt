#lang racket/base
;; Remote VM link.

(provide spawn-broker-client)

(require racket/match)
(require net/rfc6455)
(require "../main.rkt")
(require "../route.rkt")
(require "../gestalt.rkt")
(require "../drivers/timer.rkt")
(require "../drivers/websocket.rkt")
(require "../deduplicator.rkt")
(require json)
(require "protocol.rkt")

(define (collect-matchers label advertisements? level g)
  (define projector (if advertisements? project-pubs project-subs))
  (define extract-metalevels (projector (list label (?!) ?) #:level level))
  (define mls (gestalt-project/single g extract-metalevels))
  (for/fold [(result (gestalt-empty))] [(metalevel mls)]
    (define m (gestalt-project g (projector (list label metalevel (?!)))))
    (gestalt-union result (simple-gestalt advertisements? (embedded-matcher m) level metalevel))))

(define (lift-matcher-into-labelled-space m label metalevel)
  (pattern->matcher #t (list label metalevel (embedded-matcher m))))

(define (lift-gestalt-into-labelled-space g label)
  (gestalt-transform g (lambda (ml l matchers)
			 (cons (lift-matcher-into-labelled-space (car matchers) label ml)
			       (lift-matcher-into-labelled-space (cdr matchers) label ml)))))

(define (spawn-broker-client label url)
  (define client-id (websocket-local-client (list 'broker-client label)))
  (define server-id (websocket-remote-server url))
  (actor #:name broker-client
	 #:state [local-gestalt (gestalt-empty)]
	 #:state [peer-gestalt (gestalt-empty)]
	 #:state [deduplicator (make-deduplicator)]
	 #:state [seen-remote? #f]

	 (send (set-timer client-id (ping-interval) 'relative))
	 (subscribe (timer-expired client-id ?)
	   (send (set-timer client-id (ping-interval) 'relative))
	   (send-action 'ping))

	 (advertise (websocket-message client-id server-id ?))
	 (subscribe (websocket-message server-id client-id ($ data))
	   #:run-transition
	   (match (drop-json-event (string->jsexpr data))
	     [(routing-update new-peer-gestalt)
	      (begin-transition
		#:run-transition (if (equal? peer-gestalt new-peer-gestalt)
				     (begin-transition)
				     (begin-transition
				       #:update [peer-gestalt new-peer-gestalt]
				       #:update-routes)))]
	     [(? message? m (message body meta-level feedback?))
	      (begin-transition
		(define-values (fresh? d) (deduplicator-accept deduplicator m))
		#:update [deduplicator d]
		(when fresh? (message (list label meta-level body) 0 feedback?)))]
	     ['ping
	      (begin-transition (send-action 'pong))]
	     ['pong
	      (begin-transition)]))

	 (observe-advertisers (websocket-message server-id client-id ?)
	   #:presence peer-connected?
	   (when (and seen-remote? (not peer-connected?)) (quit)) ;; TODO: reconnect
	   #:update [seen-remote? (or seen-remote? peer-connected?)])

	 (observe-gestalt
	     (gestalt-union (pub (list label ? ?) #:level 10)
			    (sub (list label ? ?) #:level 10)
			    ;; TODO: ^ level 10 is ad-hoc; support
			    ;; infinity at some point in future
			    (lift-gestalt-into-labelled-space peer-gestalt label))
	   [(routing-update g)
	    (local-require "../trace.rkt")
	    (define current-pid (car (trace-pid-stack))) ;; EWWWWW
	    ;; TODO: gross - erasing by pid!
	    (define level-count (gestalt-level-count g 0))
	    (define to-subtract (label-gestalt (gestalt-full 1 level-count) current-pid))
	    #:run-transition
	    (let ((g (gestalt-subtract g to-subtract)))
	      (define new-local-gestalt
		(for/fold [(new-local-gestalt (gestalt-empty))] [(level level-count)]
		  (gestalt-union new-local-gestalt
				 (collect-matchers label #f level g)
				 (collect-matchers label #t level g))))
	      (if (equal? local-gestalt new-local-gestalt)
		  (begin-transition)
		  (begin-transition
		    #:update [local-gestalt new-local-gestalt]
		    (send-action (routing-update local-gestalt)))))]
	   [(message (list (== label) meta-level body) 0 feedback?)
	    (define m (message body meta-level feedback?))
	    (define-values (fresh? d) (deduplicator-accept deduplicator m))
	    #:update [deduplicator d]
	    (when fresh? (send-action m))])

	 (define (send-action e)
	   (define s (jsexpr->string (lift-json-action e)))
	   (send (websocket-message client-id server-id s)))))
