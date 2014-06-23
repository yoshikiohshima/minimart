#lang racket/base
;; Timer driver.

;; Uses mutable state internally, but because the scope of the
;; mutation is limited to each timer process alone, it's easy to show
;; correct linear use of the various pointers.

(require racket/set)
(require racket/match)
(require data/heap)
(require "../main.rkt")

(struct pending-timer (deadline label) #:transparent)

(provide (struct-out set-timer)
	 (struct-out timer-expired)
	 spawn-timer-driver)

(struct set-timer (label msecs kind) #:prefab)
(struct timer-expired (label msecs) #:prefab)

(define (spawn-timer-driver)
  (actor #:name timer-driver
	 #:state [count 0]

	 (define control-ch (make-channel))
	 (thread (lambda () (timer-driver-thread-main control-ch)))

	 (subscribe ($ expiry (timer-expired ? ?))
	   #:meta-level 1
	   #:when (positive? count)
	   (send expiry)
	   #:update [count (- count 1)]
	   #:update-routes) ;; TODO: only update-routes when count is zero

	 (subscribe ($ instruction (set-timer ? ? ?))
	   (channel-put control-ch instruction)
	   #:update [count (+ count 1)]
	   #:update-routes))) ;; TODO: only update-routes when count was zero

(define (timer-driver-thread-main control-ch)
  (define heap (make-timer-heap))
  (let loop ()
    (sync (match (next-timer heap)
	    [#f never-evt]
	    [t (handle-evt (timer-evt (pending-timer-deadline t))
			   (lambda (now)
			     (for-each send-ground-message (fire-timers! heap now))
			     (loop)))])
	  (handle-evt control-ch
		      (match-lambda
		       [(set-timer label msecs 'relative)
			(install-timer! heap label (+ (current-inexact-milliseconds) msecs))
			(loop)]
		       [(set-timer label msecs 'absolute)
			(install-timer! heap label msecs)
			(loop)]
		       ['quit (void)])))))

(define (make-timer-heap)
  (make-heap (lambda (t1 t2) (<= (pending-timer-deadline t1) (pending-timer-deadline t2)))))

(define (next-timer heap)
  (and (positive? (heap-count heap))
       (heap-min heap)))

(define (fire-timers! heap now)
  (if (zero? (heap-count heap))
      '()
      (let ((m (heap-min heap)))
	(if (<= (pending-timer-deadline m) now)
	    (begin (heap-remove-min! heap)
		   (cons (timer-expired (pending-timer-label m) now)
			 (fire-timers! heap now)))
	    '()))))

(define (install-timer! heap label deadline)
  (define now (current-inexact-milliseconds))
  (heap-add! heap (pending-timer deadline label)))

;; Racket's alarm-evt is almost the right design for timeouts: its
;; synchronisation value should be the (or some) value of the clock
;; after the asked-for time. That way it serves as timeout and
;; clock-reader in one.
(define (timer-evt msecs)
  (handle-evt (alarm-evt msecs)
	      (lambda (_) (current-inexact-milliseconds))))
