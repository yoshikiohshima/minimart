#lang racket/base

(require racket/set)
(require racket/match)
(require racket/list)
(require "route.rkt")
(require "gestalt.rkt")
(require "functional-queue.rkt")
(require (only-in web-server/private/util exn->string))

(provide (struct-out routing-update)
	 (struct-out message)
	 (struct-out quit)
	 (struct-out process)
	 (struct-out transition)

	 ;; imported from route.rkt:
	 ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 pretty-print-matcher
	 matcher-key-set
	 projection->pattern

	 sub
	 pub

	 spawn
	 send
	 feedback
	 spawn-world
	 deliver-event
	 transition-bind
	 sequence-transitions
	 log-events-and-actions?
	 routing-implementation)

(define pid-stack (make-parameter '()))
(define log-events-and-actions? (make-parameter #f))

;; TODO: support +Inf.0 as a level number

;; Events
(struct routing-update (gestalt) #:prefab)
(struct message (body meta-level feedback?) #:prefab)

;; Actions (in addition to Events)
;; (spawn is just process)
(struct quit () #:prefab)

;; Intra-world signalling
(struct pending-routing-update (aggregate affected-subgestalt known-targets) #:prefab)

;; Actors and Configurations
(struct process (gestalt behavior state) #:transparent)
(struct world (next-pid			;; Natural, PID for next-spawned process
	       event-queue		;; Queue of Event
	       runnable-pids		;; Set of PIDs
	       partial-gestalt		;; Gestalt from local processes only; maps to PID
	       full-gestalt		;; Union of partial-gestalt and downward-gestalt
	       process-table		;; Hash from PID to Process
	       downward-gestalt		;; Gestalt representing interests of outside world
	       process-actions		;; Queue of (cons PID Action)
	       ) #:transparent)

;; Behavior : maybe event * state -> transition
(struct transition (state actions) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol and utilities

(define (sub p #:meta-level [ml 0] #:level [l 0]) (simple-gestalt #f p l ml))
(define (pub p #:meta-level [ml 0] #:level [l 0]) (simple-gestalt #t p l ml))

(define (spawn behavior state [gestalt (gestalt-empty)]) (process gestalt behavior state))
(define (send body #:meta-level [ml 0]) (message body ml #f))
(define (feedback body #:meta-level [ml 0]) (message body ml #t))

(define (spawn-world . boot-actions)
  (spawn world-handle-event
	 (enqueue-actions (world 0
				 (make-queue)
				 (set)
				 (gestalt-empty)
				 (gestalt-empty)
				 (hash)
				 (gestalt-empty)
				 (make-queue))
			  -1
			  boot-actions)))

(define (event? x) (or (routing-update? x) (message? x)))
(define (action? x) (or (event? x) (process? x) (quit? x)))

(define (transition-bind k t0)
  (match-define (transition state0 actions0) t0)
  (match-define (transition state1 actions1) (k state0))
  (transition state1 (cons actions0 actions1)))

(define (sequence-transitions t0 . steps)
  (foldl transition-bind t0 steps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Trigger guards

;; Trigger-guards only pass through routing updates if there has been
;; a change.
(struct trigger-guard (gestalt handler state) #:transparent)

(define (trigger-guard-handle e s0)
  (match-define (trigger-guard old-gestalt handler old-state) s0)
  (define (deliver s)
    (match (handler e old-state)
      [#f
       (if (eq? s s0) #f (transition s '()))]
      [(transition new-state actions)
       (transition (struct-copy trigger-guard s [state new-state]) actions)]))
  (match e
    [(routing-update new-gestalt)
     (if (equal? new-gestalt old-gestalt)
	 #f
	 (deliver (struct-copy trigger-guard s0 [gestalt new-gestalt])))]
    [_ (deliver s0)]))

(define (trigger-guard-process p)
  (match-define (process _ b s) p)
  (struct-copy process p [behavior trigger-guard-handle] [state (trigger-guard #f b s)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; World implementation

;; Each time a world is handed an event from its environment, it:
;;  1. dispatches events
;;      a. removing them one-at-a-time from the queue
;;      b. dispatching them to processes
;;      c. updating process states and accumulating actions in the queue
;;      d. any process that returned non-#f is considered "non-idle" for step 3.
;;  2. performs actions
;;      a. removing them one-at-a-time from the queue
;;      b. interpreting them
;;      c. updating world state and accumulating events in the queue
;;  3. steps non-idle processes
;;      a. runs through the set of processes accumulated in 1d. above
;;      b. any process that returned non-#f is put in the "non-idle" set for next time
;;  4. yields updated world state and world actions to environment.
;;
;; Note that routing-update actions are queued internally as
;; pending-routing-update structures, in order to preserve and
;; communicate transient gestalt states to processes. In addition, the
;; known-targets field of a pending-routing-update structure is used
;; to provide NC's initial gestalt signal to a newly-spawned process.
;;
;; TODO: should step 3 occur before step 1?

(define (enqueue-actions w pid actions)
  (struct-copy world w
    [process-actions (queue-append-list (world-process-actions w)
					(filter-map (lambda (a) (and (action? a) (cons pid a)))
						    (flatten actions)))]))

;; The code is written to maintain the runnable-pids set carefully, to
;; ensure we can locally decide whether we're inert or not without
;; having to search the whole deep process tree.
(define (inert? w)
  (and (queue-empty? (world-event-queue w))
       (queue-empty? (world-process-actions w))
       (set-empty? (world-runnable-pids w))))

(define (deliver-event e pid p)
  (parameterize ((pid-stack (cons pid (pid-stack))))
    (when (and (log-events-and-actions?) e)
      (log-info "~a: ~v --> ~v ~v"
		(reverse (pid-stack))
		e
		(process-behavior p)
		(if (world? (process-state p))
		    "#<world>"
		    (process-state p))))
    (with-handlers ([(lambda (exn) #t)
		     (lambda (exn)
		       (log-error "Process ~a died with exception:\n~a" pid (exn->string exn))
		       (transition (process-state p) (list (quit))))])
      (match (with-continuation-mark 'minimart-process
				     pid ;; TODO: debug-name, other user annotation
				     ((process-behavior p) e (process-state p)))
	[#f #f] ;; inert.
	[(? transition? t) t] ;; potentially runnable.
	[x
	 (log-error "Process ~a returned non-#f, non-transition: ~v" pid x)
	 (transition (process-state p) (list (quit)))]))))

(define (mark-pid-runnable w pid)
  (struct-copy world w [runnable-pids (set-add (world-runnable-pids w) pid)]))

(define (apply-transition pid t w)
  (match t
    [#f w]
    [(transition new-state new-actions)
     (let* ((w (transform-process pid w
				  (lambda (p)
				    (when (and (log-events-and-actions?)
					       (not (null? (flatten new-actions))))
				      (log-info "~a: ~v <-- ~v ~v"
						(reverse (cons pid (pid-stack)))
						new-actions
						(process-behavior p)
						(if (world? new-state)
						    "#<world>"
						    new-state)))
				    (struct-copy process p [state new-state])))))
       (enqueue-actions (mark-pid-runnable w pid) pid new-actions))]))

(define (enqueue-event e w)
  (struct-copy world w [event-queue (enqueue (world-event-queue w) e)]))

(define (perform-actions w)
  (for/fold ([t (transition (struct-copy world w [process-actions (make-queue)]) '())])
      ((entry (in-list (queue->list (world-process-actions w)))))
    (match-define (cons pid a) entry)
    (transition-bind (perform-action pid a) t)))

(define (dispatch-events w)
  (transition (for/fold ([w (struct-copy world w [event-queue (make-queue)])])
		  ((e (in-list (queue->list (world-event-queue w)))))
		(dispatch-event e w))
              '()))

(define (transform-process pid w fp)
  (define pt (world-process-table w))
  (match (hash-ref pt pid)
    [#f w]
    [p (struct-copy world w [process-table (hash-set pt pid (fp p))])]))

(define (update-full-gestalt w)
  (struct-copy world w [full-gestalt
			(gestalt-union (world-partial-gestalt w) (world-downward-gestalt w))]))

(define (issue-local-routing-update w relevant-gestalt known-targets)
  (enqueue-event (pending-routing-update (world-full-gestalt w)
					 relevant-gestalt
					 known-targets)
		 w))

(define (issue-routing-update w relevant-gestalt known-targets)
  (transition (issue-local-routing-update w relevant-gestalt known-targets)
              (routing-update (drop-gestalt (world-partial-gestalt w)))))

(define (apply-and-issue-routing-update w old-gestalt new-gestalt known-targets)
  (define new-partial
    (gestalt-union (gestalt-erase-path (world-partial-gestalt w) old-gestalt) new-gestalt))
  (issue-routing-update (update-full-gestalt (struct-copy world w [partial-gestalt new-partial]))
			(gestalt-union old-gestalt new-gestalt)
			known-targets))

(define ((perform-action pid a) w)
  (match a
    [(? process? new-p)
     (let* ((new-pid (world-next-pid w))
	    (new-p (trigger-guard-process new-p))
	    (new-gestalt (label-gestalt (process-gestalt new-p) new-pid))
	    (new-p (struct-copy process new-p [gestalt new-gestalt]))
	    (w (struct-copy world w
		 [next-pid (+ new-pid 1)]
		 [process-table (hash-set (world-process-table w) new-pid new-p)])))
       (log-info "Spawned process ~a ~v ~v" new-pid (process-behavior new-p) (process-state new-p))
       (apply-and-issue-routing-update w (gestalt-empty) new-gestalt (set new-pid)))]
    [(quit)
     (define pt (world-process-table w))
     (define p (hash-ref pt pid (lambda () #f)))
     (if p
	 (let* ((w (struct-copy world w [process-table (hash-remove pt pid)])))
	   (log-info "Process ~a terminating; ~a processes remain"
		     pid
		     (hash-count (world-process-table w)))
	   (apply-and-issue-routing-update w (process-gestalt p) (gestalt-empty) (set pid)))
	 (transition w '()))]
    [(routing-update gestalt)
     (define pt (world-process-table w))
     (define p (hash-ref pt pid (lambda () #f)))
     (if p
	 (let* ((old-gestalt (process-gestalt p))
		(new-gestalt (label-gestalt gestalt pid))
		(new-p (struct-copy process p [gestalt new-gestalt]))
		(w (struct-copy world w [process-table (hash-set pt pid new-p)])))
	   (apply-and-issue-routing-update w old-gestalt new-gestalt (set)))
	 (transition w '()))]
    [(message body meta-level feedback?)
     (if (zero? meta-level)
	 (transition (enqueue-event a w) '())
	 (transition w (message body (- meta-level 1) feedback?)))]))

(define (dispatch-event e w)
  (match e
    [(message body meta-level feedback?)
     (define pids (gestalt-match-value (world-partial-gestalt w) body meta-level feedback?))
     (define pt (world-process-table w))
     (for/fold ([w w]) [(pid (in-set pids))]
       (apply-transition pid (deliver-event e pid (hash-ref pt pid)) w))]
    [(pending-routing-update g affected-subgestalt known-targets)
     (define affected-pids (gestalt-match affected-subgestalt g))
     (define pt (world-process-table w))
     (for/fold ([w w]) [(pid (in-set (set-union known-targets affected-pids)))]
       (match (hash-ref pt pid (lambda () #f))
	 [#f w]
	 [p (define g1 (gestalt-filter g (process-gestalt p)))
	    (apply-transition pid (deliver-event (routing-update g1) pid p) w)]))]))

;; This is roughly the "schedule" rule of the calculus.
(define (step-children w)
  (define runnable-pids (world-runnable-pids w))
  (if (set-empty? runnable-pids)
      #f ;; world is inert.
      (transition (for/fold ([w (struct-copy world w [runnable-pids (set)])])
		      [(pid (in-set runnable-pids))]
		    (define p (hash-ref (world-process-table w) pid (lambda () #f)))
		    (if (not p) w (apply-transition pid (deliver-event #f pid p) w)))
		  '()))) ;; world needs another check to see if more can happen.

(define (world-handle-event e w)
  (if (or e (not (inert? w)))
      (sequence-transitions (transition (inject-event e w) '())
			    dispatch-events
			    perform-actions
			    (lambda (w) (or (step-children w) (transition w '()))))
      (step-children w)))

(define (inject-event e w)
  (match e
    [#f w]
    [(routing-update g)
     (define old-downward (world-downward-gestalt w))
     (define new-downward (lift-gestalt (label-gestalt g 'out)))
     (issue-local-routing-update (update-full-gestalt
				  (struct-copy world w [downward-gestalt new-downward]))
				 (gestalt-union old-downward new-downward)
				 (set))]
    [(message body meta-level feedback?)
     (enqueue-event (message body (+ meta-level 1) feedback?) w)]))

(define routing-implementation 'fastrouting)
