#lang racket/base

(provide (struct-out deduplicator)
	 make-deduplicator
	 deduplicator-accept
	 deduplicator-expire)

(require racket/set)
(require racket/match)
(require "functional-queue.rkt")

(struct deduplicator (queue table ttl) #:transparent)

(define (make-deduplicator [ttl 10000])
  (deduplicator (make-queue) (set) ttl))

(define (deduplicator-expire d)
  (define now (current-inexact-milliseconds))
  (let loop ((d d))
    (match-define (deduplicator queue table ttl) d)
    (if (queue-empty? queue)
	d
	(let-values (((v q1) (dequeue queue)))
	  (if (<= (car v) now)
	      (loop (deduplicator q1 (set-remove table (cdr v)) ttl))
	      d)))))

(define (deduplicator-accept d incoming)
  (let* ((d (deduplicator-expire d)))
    (match-define (deduplicator queue table ttl) d)
    (if (set-member? table incoming)
	(values #f d)
	(values #t (deduplicator (enqueue queue
					  (cons (+ (current-inexact-milliseconds) ttl) incoming))
				 (set-add table incoming)
				 ttl)))))
