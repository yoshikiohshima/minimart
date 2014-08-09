#lang racket/base
;; Generic protocol for WebSockets/TCP/etc-based participation in a network.

(provide drop-json-action
	 lift-json-event
	 lift-json-action
	 drop-json-event
	 ping-interval)

(require net/rfc6455)
(require racket/set)
(require racket/match)
(require "../main.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wire protocol representation of events and actions

(define (drop j)
  (match j
    ["ping" 'ping]
    ["pong" 'pong]
    [`("routes" ,gj) (routing-update (jsexpr->gestalt gj (lambda (v) (set 'peer))))]
    [`("message" ,body ,meta-level ,feedback?) (message body meta-level feedback?)]))

(define (lift j)
  (match j
    ['ping "ping"]
    ['pong "pong"]
    [(routing-update g) `("routes" ,(gestalt->jsexpr g (lambda (v) #t)))]
    [(message body meta-level feedback?) `("message" ,body ,meta-level ,feedback?)]))

(define drop-json-action drop)
(define lift-json-event lift)
(define lift-json-action lift)
(define drop-json-event drop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Connections

(define (ping-interval) (* 1000 (max (- (ws-idle-timeout) 10) (* (ws-idle-timeout) 0.8))))
