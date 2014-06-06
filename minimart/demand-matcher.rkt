#lang racket/base

(require racket/set)
(require racket/match)
(require "core.rkt")
(require "gestalt.rkt")

(provide (except-out (struct-out demand-matcher) demand-matcher)
	 (rename-out [make-demand-matcher demand-matcher])
	 demand-matcher-update
	 spawn-demand-matcher)

(struct demand-matcher (demand-is-subscription?
			pattern
			spec
			meta-level
			demand-level
			supply-level
			increase-handler
			decrease-handler
			current-demand
			current-supply)
	#:transparent)

(define (unexpected-supply-decrease . removed-captures)
  '())

(define (default-decrease-handler state . removed-captures)
  state)

(define (make-demand-matcher demand-is-subscription?
			     projection
			     meta-level
			     demand-level
			     supply-level
			     increase-handler
			     [decrease-handler default-decrease-handler])
  (demand-matcher demand-is-subscription?
		  (projection->pattern projection)
		  (compile-gestalt-projection projection)
		  meta-level
		  demand-level
		  supply-level
		  increase-handler
		  decrease-handler
		  (set)
		  (set)))

(define (demand-matcher-update d s g)
  (match-define (demand-matcher demand-is-sub? _ spec ml dl sl inc-h dec-h old-demand old-supply) d)
  (define new-demand (matcher-key-set (gestalt-project g ml dl (not demand-is-sub?) spec)))
  (define new-supply (matcher-key-set (gestalt-project g ml sl demand-is-sub? spec)))
  (define demand+ (set-subtract (set-subtract new-demand old-demand) new-supply))
  (define supply- (set-intersect (set-subtract old-supply new-supply) new-demand))
  (define new-d (struct-copy demand-matcher d
		  [current-demand new-demand]
		  [current-supply new-supply]))
  (let* ((s (for/fold [(s s)] [(captures (in-set demand+))] (apply inc-h s captures)))
	 (s (for/fold [(s s)] [(captures (in-set supply-))] (apply dec-h s captures))))
    (values new-d s)))

(define (demand-matcher-handle-event e d)
  (match e
    [(routing-update gestalt)
     (define-values (new-d actions) (demand-matcher-update d '() gestalt))
     (transition new-d actions)]
    [_ #f]))

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
