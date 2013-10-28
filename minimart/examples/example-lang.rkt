#lang minimart

(require (only-in racket/port read-line-evt))

(define (r e s)
  (match e
    [(message body _ _) (transition s (send `(got ,body) #:meta-level 1))]
    [_ #f]))

(define (b e n)
  (match e
    [#f (if (< n 10)
	    (transition (+ n 1) (send `(hello ,n)))
	    #f)]
    [_ #f]))

(spawn-world (spawn r (void) (list (sub ?)))
	     (spawn b 0))

(define (spy e s)
  (when e (log-info "SPY: ~v" e))
  #f)

(spawn spy (void) (list (sub ? #:level 1000) (pub ? #:level 1000)))

(define (echoer e s)
  (match e
    [(message (event _ (list (? eof-object?))) _ _) (transition s (quit))]
    [(message (event _ (list line)) _ _) (transition s (send `(got-line ,line)))]
    [_ #f]))

(spawn echoer (void) (list (sub (event (read-line-evt (current-input-port) 'any) ?)
				#:meta-level 1)))