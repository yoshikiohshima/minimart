#lang racket/base
;; Generic relay for WebSockets/TCP/etc-based participation in a network.

(provide spawn-websocket-relay)

(require racket/set)
(require racket/match)
(require net/rfc6455)
(require "main.rkt")
(require "demand-matcher.rkt")
(require "drivers/timer.rkt")
(require "drivers/websocket.rkt")
(require json)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main: start WebSocket server

;; Depends on timer driver and websocket driver running at metalevel 1.
(define (spawn-websocket-relay port [ssl-options #f])
  (define server-id (websocket-local-server port ssl-options))
  (spawn-demand-matcher (websocket-message (?! (websocket-remote-client ?)) server-id ?)
			#:meta-level 1
			(lambda (c) (spawn-connection-handler c server-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wire protocol representation of events and actions

(define (drop-json-action j)
  (match j
    ["ping" 'ping]
    ["pong" 'pong]
    [`("routes" ,gj) (routing-update (jsexpr->gestalt gj (lambda (v) (set 'peer))))]
    [`("message" ,body ,meta-level ,feedback?) (message body meta-level feedback?)]))

(define (lift-json-event j)
  (match j
    ['ping "ping"]
    ['pong "pong"]
    [(routing-update g) `("routes" ,(gestalt->jsexpr g (lambda (v) #t)))]
    [(message body meta-level feedback?) `("message" ,body ,meta-level ,feedback?)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections

(struct connection-state (client-id tunnelled-gestalt) #:transparent)

(define (ping-interval) (* 1000 (max (- (ws-idle-timeout) 10) (* (ws-idle-timeout) 0.8))))

(define (spawn-connection-handler c server-id)
  (define (send-event e s)
    (send (websocket-message server-id
			     (connection-state-client-id s)
			     (jsexpr->string (lift-json-event e)))
	  #:meta-level 1))
  (define ((handle-connection-routing-change g) s)
    (if (gestalt-empty? g)
	(transition s (quit)) ;; websocket connection closed
	#f))
  (define ((handle-tunnelled-routing-change g) s)
    (transition s (send-event (routing-update g) s)))
  (define ((handle-tunnellable-message m) s)
    (if (gestalt-accepts? (connection-state-tunnelled-gestalt s) m)
	(transition s (send-event m s))
	(transition s '())))
  (define relay-connections
    (gestalt-union (sub (timer-expired c ?) #:meta-level 1)
		   (sub (websocket-message c server-id ?) #:meta-level 1)
		   (sub (websocket-message c server-id ?) #:meta-level 1 #:level 1)
		   (pub (websocket-message server-id c ?) #:meta-level 1)))
  (define (connection-handler e s)
    (match e
      [(routing-update g)
       (sequence-transitions
	(transition s '())
	(handle-connection-routing-change (gestalt-filter g relay-connections))
	(handle-tunnelled-routing-change
	 (gestalt-filter g (connection-state-tunnelled-gestalt s))))]
      [(? message? m)
       (sequence-transitions
	(match m
	  [(message (websocket-message from to data) 1 #f)
	   (match (drop-json-action (string->jsexpr data))
	     [(routing-update g-unfiltered)
	      (define g (gestalt-transform g-unfiltered
					   (lambda (ml l p) (if (zero? ml) p '(#f . #f)))))
	      (transition (struct-copy connection-state s [tunnelled-gestalt g])
			  (routing-update (gestalt-union g relay-connections)))]
	     [(? message? m)
	      (transition s (if (zero? (message-meta-level m)) m '()))]
	     ['ping
	      (transition s (send-event 'pong s))]
	     ['pong
	      (transition s '())])]
	  [(message (timer-expired _ _) 1 #f)
	   (transition s (list (send (set-timer c (ping-interval) 'relative) #:meta-level 1)
			       (send-event 'ping s)))]
	  [_
	   (transition s '())])
	(handle-tunnellable-message m))]
      [#f #f]))
  (list (send (set-timer c (ping-interval) 'relative) #:meta-level 1)
	(spawn connection-handler
	       (connection-state c (gestalt-empty))
	       relay-connections)))
