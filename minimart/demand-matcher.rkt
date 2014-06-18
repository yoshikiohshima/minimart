#lang racket/base
;; A structure (and process!) for matching supply to demand via Gestalts.

(require racket/set)
(require racket/match)
(require "core.rkt")
(require "gestalt.rkt")
(require (only-in "route.rkt" matcher-key-set))
(require "drivers/timer.rkt")

(provide (except-out (struct-out demand-matcher) demand-matcher)
	 (rename-out [make-demand-matcher demand-matcher])
	 demand-matcher-update
	 spawn-demand-matcher
	 on-gestalt)

;; A DemandMatcher keeps track of demand for services based on some
;; Projection over a Gestalt, as well as a collection of functions
;; that can be used to increase supply in response to increased
;; demand, or handle a sudden drop in supply for which demand still
;; exists.
(struct demand-matcher (demand-is-subscription?	;; Boolean
			pattern			;; Pattern
			spec			;; CompiledProjection
			meta-level		;; Nat
			demand-level		;; Nat
			supply-level		;; Nat
			increase-handler	;; ChangeHandler
			decrease-handler	;; ChangeHandler
			current-demand		;; (Setof (Listof Any))
			current-supply)		;; (Setof (Listof Any))
	#:transparent)

;; A ChangeHandler is a ((Constreeof Action) Any* -> (Constreeof Action)).
;; It is called with an accumulator of actions so-far-computed as its
;; first argument, and with a value for each capture in the
;; DemandMatcher's projection as the remaining arguments.

;; ChangeHandler
;; Default handler of unexpected supply decrease.
(define (default-decrease-handler state . removed-captures)
  state)

;; Constructs a DemandMatcher. The projection yields both the Pattern
;; and CompiledProjection stored in the DemandMatcher.
(define (make-demand-matcher demand-is-subscription?	;; Boolean
			     projection			;; Projection
			     meta-level			;; Nat
			     demand-level		;; Nat
			     supply-level		;; Nat
			     increase-handler		;; ChangeHandler
			     [decrease-handler default-decrease-handler]) ;; ChangeHandler
  (demand-matcher demand-is-subscription?
		  (matcher-projection->pattern projection)
		  (compile-matcher-projection projection)
		  meta-level
		  demand-level
		  supply-level
		  increase-handler
		  decrease-handler
		  (set)
		  (set)))

;; DemandMatcher (Constreeof Action) Gestalt -> (Values DemandMatcher (Constreeof Actions))
;; Given a new Gestalt from the environment, projects it into supply and demand sets.
;; Computes the differences between the new sets and the currently-cached sets, and
;; calls the ChangeHandlers in response to increased unsatisfied demand and decreased
;; demanded supply.
(define (demand-matcher-update d s g)
  (match-define (demand-matcher demand-is-sub? _ spec ml dl sl inc-h dec-h old-demand old-supply) d)
  (define new-demand (matcher-key-set (gestalt-project* g ml dl (not demand-is-sub?) spec)))
  (define new-supply (matcher-key-set (gestalt-project* g ml sl demand-is-sub? spec)))
  (define demand+ (set-subtract (set-subtract new-demand old-demand) new-supply))
  (define supply- (set-intersect (set-subtract old-supply new-supply) new-demand))
  (define new-d (struct-copy demand-matcher d
		  [current-demand new-demand]
		  [current-supply new-supply]))
  (let* ((s (for/fold [(s s)] [(captures (in-set demand+))] (apply inc-h s captures)))
	 (s (for/fold [(s s)] [(captures (in-set supply-))] (apply dec-h s captures))))
    (values new-d s)))

;; Behavior :> (Option Event) DemandMatcher -> Transition
;; Handles events from the environment. Only cares about routing-updates.
(define (demand-matcher-handle-event e d)
  (match e
    [(routing-update gestalt)
     (define-values (new-d actions) (demand-matcher-update d '() gestalt))
     (transition new-d actions)]
    [_ #f]))

;; Any* -> (Constreeof Action)
;; Default handler of unexpected supply decrease.
;; Ignores the situation.
(define (unexpected-supply-decrease . removed-captures)
  '())

;; Projection (Any* -> (Constreeof Action)) [(Any* -> (Constreeof Action))] -> Action
;; Spawns a demand matcher actor.
(define (spawn-demand-matcher projection
			      increase-handler
			      [decrease-handler unexpected-supply-decrease]
			      #:demand-is-subscription? [demand-is-subscription? #f]
			      #:meta-level [meta-level 0]
			      #:demand-level [demand-level 0]
			      #:supply-level [supply-level 0])
  (define d (make-demand-matcher demand-is-subscription?
				 projection
				 meta-level
				 demand-level
				 supply-level
				 (lambda (acs . rs) (cons (apply increase-handler rs) acs))
				 (lambda (acs . rs) (cons (apply decrease-handler rs) acs))))
  (define observer-pattern (demand-matcher-pattern d))
  (define observer-level (+ 1 (max demand-level supply-level)))
  (spawn demand-matcher-handle-event
	 d
	 (gestalt-union (sub observer-pattern #:meta-level meta-level #:level observer-level)
			(pub observer-pattern #:meta-level meta-level #:level observer-level))))

;; (Gestalt (Option (Setof (Listof Value))) ... -> (Option (Constreeof Action)))
;;    Gestalt GestaltProjection ...
;; -> Action
;; Spawns a process that observes the given projections. Any time the
;; environment's gestalt changes in a relevant way, calls
;; check-and-maybe-spawn-fn with the aggregate gestalt and the
;; projection results. If check-and-maybe-spawn-fn returns #f,
;; continues to wait; otherwise, takes the action(s) returned, and
;; quits.
(define (on-gestalt #:timeout-msec [timeout-msec #f]
		    #:on-timeout [timeout-handler (lambda () '())]
		    check-and-maybe-spawn-fn
		    base-gestalt
		    . gestalt-projections)
  (define timer-id (gensym 'on-gestalt))
  (define aggregate-gestalt
    (apply gestalt-union
	   base-gestalt
	   (map projection->gestalt gestalt-projections)))
  (list
   (when timeout-msec (send (set-timer timer-id timeout-msec 'relative)))
   (spawn (lambda (e s)
	    (match e
	      [(routing-update g)
	       (define projection-results
		 (map (lambda (p) (gestalt-project/keys g p)) gestalt-projections))
	       (define maybe-spawn (apply check-and-maybe-spawn-fn
					  aggregate-gestalt
					  projection-results))
	       (transition s (when maybe-spawn (list maybe-spawn (quit))))]
	      [(message (timer-expired _ _) _ _)
	       (transition s (list (timeout-handler) (quit)))]
	      [_ #f]))
	  (void)
	  (gestalt-union aggregate-gestalt
			 (sub (timer-expired timer-id ?))))))
