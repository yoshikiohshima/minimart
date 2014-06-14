#lang racket/base
;; Breaking the infinite tower of nested Worlds, connecting to the "real" world at the fracture line.

(require racket/async-channel)
(require racket/set)
(require racket/match)
(require racket/list)
(require "core.rkt")
(require "gestalt.rkt")

(provide (struct-out event)
	 send-ground-message
	 run-ground)

;; A GroundEvent is a pair of a Racket (evt?) event and its yielded
;; results.
;;  - (event RacketEvent (Listof Any))
(struct event (descriptor values) #:prefab)

;; (Parameterof (Option AsyncChannel))
;; Communication channel from auxiliary (usually driver) threads to
;; the currently-active ground VM.
(define current-ground-event-async-channel (make-parameter #f))

;; Any -> Void
;; Sends a (non-feedback) message at the ground-VM metalevel.
(define (send-ground-message body)
  (match (current-ground-event-async-channel)
    [(? async-channel? ch) (async-channel-put ch (send body))]
    [_ (error 'send-ground-message "Called outside dynamic scope of run-ground")]))

;; RacketEvent -> RacketEvent
;; Wraps a CML-style Racket event with a handler that sends the event
;; results via the ground VM.
(define (event-handler descriptor)
  (handle-evt descriptor (lambda vs (send (event descriptor vs)))))

;; CompiledProjection
;; Used to extract event descriptors and results from subscriptions
;; from the ground VM's contained World.
(define event-projection (compile-gestalt-projection (event (?!) ?)))

;; Gestalt -> (Listof RacketEvent)
;; Projects out the active event subscriptions from the given gestalt.
(define (extract-active-events gestalt)
  (define es (matcher-key-set (gestalt-project gestalt 0 0 #f event-projection)))
  ;; TODO: how should the following error be handled, ideally?
  ;; In principle, security restrictions should make it impossible.
  ;; But absent those, what should be done? Should an offending
  ;; process be identified and terminated?
  (unless es (error 'extract-active-events "User program subscribed to wildcard event"))
  (for/list [(ev (in-set es))]
    (match-define (list e) ev)
    (event-handler e)))

;; RacketEvent
;; Used only when the system is not provably inert, in order to let it
;; take further internal reductions.
(define idle-handler
  (handle-evt (system-idle-evt) (lambda _ #f)))

;; Action* -> Void
;; Runs a ground VM, booting the outermost World with the given Actions.
(define (run-ground . boot-actions)
  (parameterize ((current-ground-event-async-channel (make-async-channel)))
    (let await-interrupt ((inert? #f) (p (spawn-world boot-actions)) (active-events '()))
      (define active-gestalt (process-gestalt p))
      (define event-list (if inert?
			     active-events
			     (cons idle-handler active-events)))
      (if (and (null? event-list) (gestalt-empty? active-gestalt))
	  (begin (log-info "run-ground: Terminating because inert")
		 (void))
	  (let ((e (apply sync (current-ground-event-async-channel) event-list)))
	    (match (deliver-event e -2 p)
	      [#f ;; inert
	       (await-interrupt #t p active-events)]
	      [(transition new-state actions)
	       (let process-actions ((actions (flatten actions)) (g active-gestalt))
		 (match actions
		   ['()
		    (await-interrupt #f
				     (struct-copy process p
				       [gestalt g]
				       [state new-state])
				     (extract-active-events g))]
		   [(cons a actions)
		    (match a
		      [(routing-update gestalt)
		       (process-actions actions gestalt)]
		      [(quit)
		       (log-info "run-ground: Terminating by request")
		       (void)]
		      [_
		       (log-warning "run-ground: ignoring useless meta-action ~v" a)
		       (process-actions actions g)])]))]))))))
