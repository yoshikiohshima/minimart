#lang racket/base

(require (for-syntax syntax/parse))
(require (for-syntax racket/base))

(require racket/match)
(require "main.rkt")
(require "functional-queue.rkt")

(provide userland-thread
	 receive
	 do
	 next-event
	 all-queued-events
	 pushback-events!
	 wait-for-gestalt)

(struct do-command (actions k) #:transparent)
(struct receive-command (single? k) #:transparent)
(struct pushback-command (events k) #:transparent)

(define-syntax userland-thread
  (lambda (stx)
    (syntax-parse stx
      [(_ (~or (~optional (~seq #:gestalt g) #:defaults ([g #'(gestalt-empty)]) #:name "#:gestalt")) ...
	  body ...)
       #`(spawn-userland* (lambda () body ...) g)])))

(define (spawn-userland* main [initial-gestalt (gestalt-empty)])
  (spawn (lambda (e k) (k e))
	 (lambda (first-event)
	   (interpret-command (make-queue)
			      (list->queue (list first-event))
			      ((reply-to (lambda (dummy)
					   (main)
					   (do (quit))))
			       (void))))
	 initial-gestalt))

(define-syntax-rule (receive [pat clausebody ...] ...)
  (receive* (lambda (e sentinel) (match e [pat clausebody ...] ... [_ sentinel]))))

(define sentinel (cons 'sentinel '()))
(define (receive* f)
  (let loop ((events (all-queued-events)) (discarded-rev '()))
    (match events
      [(cons e rest)
       (call-with-values (lambda () (f e sentinel))
	 (lambda vs
	   (if (equal? vs (list sentinel))
	       (loop rest (cons e discarded-rev))
	       (begin (pushback-events! (reverse discarded-rev))
		      (apply values vs)))))]
      ['()
       (loop (all-queued-events) discarded-rev)])))

(define (wait-for-gestalt probe)
  (receive [(routing-update g)
	    (if (gestalt-empty? (gestalt-filter g probe))
		(wait-for-gestalt probe)
		g)]))

(define (do . actions) (call-in-raw-context (lambda (k) (do-command actions k))))
(define (next-event) (call-in-raw-context (lambda (k) (receive-command #t k))))
(define (all-queued-events) (call-in-raw-context (lambda (k) (receive-command #f k))))
(define (pushback-events! events) (call-in-raw-context (lambda (k) (pushback-command events k))))

(define prompt (make-continuation-prompt-tag 'minimart-userland))

(define (reply-to k)
  (lambda (reply)
    (call-with-continuation-prompt (lambda () (k reply)) prompt)))

(define (call-in-raw-context proc)
  (call-with-composable-continuation
   (lambda (k) (abort-current-continuation prompt (lambda () (proc (reply-to k)))))
   prompt))

(define (interpret-command actions events command)
  (match command
    [(do-command new-action-chunk k)
     (interpret-command (enqueue actions new-action-chunk) events (k (void)))]
    [(receive-command single? k)
     (cond
      [(queue-empty? events)
       (transition (lambda (e) (and e (interpret-command (make-queue) (list->queue (list e)) command)))
		   (queue->list actions))]
      [single?
       (define-values (e rest) (dequeue events))
       (interpret-command actions rest (k e))]
      [else
       (interpret-command actions (make-queue) (k (queue->list events)))])]
    [(pushback-command events-to-push k)
     (interpret-command actions (queue-append (list->queue events-to-push) events) (k (void)))]))
