#lang racket/base
;; Generic relay for WebSockets/TCP/etc-based participation in a network.

(provide spawn-broker-server)

(require racket/set)
(require racket/match)
(require net/rfc6455)
(require "../main.rkt")
(require "../demand-matcher.rkt")
(require "../drivers/timer.rkt")
(require "../drivers/websocket.rkt")
(require json)
(require "protocol.rkt")

;; Depends on timer driver and websocket driver running at metalevel 1.
(define (spawn-broker-server port [ssl-options #f])
  (define server-id (websocket-local-server port ssl-options))
  (spawn-demand-matcher (websocket-message (?! (websocket-remote-client ?)) server-id ?)
			#:meta-level 1
			(lambda (c) (spawn-connection-handler c server-id))))

(define (spawn-connection-handler c server-id)
  (actor #:name relay
	 #:state [tunnelled-gestalt (gestalt-empty)]

	 (send #:meta-level 1 (set-timer c (ping-interval) 'relative))
	 (subscribe (timer-expired c ?)
	   #:meta-level 1
	   (send #:meta-level 1 (set-timer c (ping-interval) 'relative))
	   (send-event 'ping))

	 (observe-advertisers (websocket-message c server-id ?)
	   #:meta-level 1
	   #:presence peer-connected?
	   (when (not peer-connected?) (quit)))

	 (advertise (websocket-message server-id c ?) #:meta-level 1)
	 (subscribe (websocket-message c server-id ($ data))
	   #:meta-level 1
	   #:run-transition (handle-incoming (drop-json-action (string->jsexpr data))))

	 (define (handle-incoming data)
	   (match data
	     [(routing-update g-unfiltered)
	      (define g (gestalt-transform g-unfiltered
	 				   (lambda (ml l p) (if (zero? ml) p '(#f . #f)))))
	      (begin-transition
	 	#:update [tunnelled-gestalt g]
	 	#:update-routes)]
	     [(? message? m)
	      (begin-transition
	 	(when (zero? (message-meta-level m)) m))]
	     ['ping
	      (begin-transition (send-event 'pong))]
	     ['pong
	      (begin-transition)]))

	 (observe-gestalt tunnelled-gestalt
	   [event ;; routing-update or message, prefiltered by tunnelled-gestalt
	    (send-event event)])

	 (define (send-event e)
	   (send #:meta-level 1
		 (websocket-message server-id c (jsexpr->string (lift-json-event e)))))))
