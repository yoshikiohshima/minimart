#lang racket/base
;; Poor-man's memoization.

(provide memoize1)

(define sentinel (cons #f #f))

(define (memoize1 f)
  (define results (make-weak-hash))
  (lambda (arg)
    (hash-ref results arg (lambda ()
			    (define val (f arg))
			    (hash-set! results arg val)
			    val))))

(module+ test
  (require rackunit)

  (define call-counter 0)

  (define (raw x)
    (set! call-counter (+ call-counter 1))
    (gensym 'raw-result))

  (define cooked (memoize1 raw))

  ;; These tests will *likely* pass, but if garbage collection strikes
  ;; at an inopportune moment, they may fail.

  (collect-garbage)

  (define v (cons 1 2))

  (check-equal? call-counter 0)
  (check-eq? (cooked v) (cooked v))
  (check-equal? call-counter 1)

  (set! v (cons 1 2))

  (check-equal? call-counter 1)
  (check-equal? (cooked v) (cooked v))
  (check-equal? call-counter 1)

  (set! v (cons 1 2))

  (collect-garbage)
  (collect-garbage)
  (collect-garbage)

  (check-equal? call-counter 1)
  (check-equal? (cooked v) (cooked v))
  (check-equal? call-counter 2))
