#lang racket/base
;; Implements a nested-word-like automaton mapping sets of messages to sets of other values.
;; A kind of "regular-expression"-keyed multimap.

;; TODO: More global purpose statement.
;; TODO: Some examples showing the idea(s).

;; TODO: rename to matcher.rkt or similar.
;; TODO: Ontology

;; TODO: (generally) interpretations for data definitions

(provide ;; Patterns and Projections
         ?
	 wildcard?
	 ?!
	 (struct-out capture)
	 (struct-out embedded-matcher)

	 matcher? ;; expensive; see implementation
	 matcher-empty
	 matcher-empty?
	 pattern->matcher
	 pattern->matcher*
	 matcher-union
	 matcher-intersect
	 matcher-subtract
	 matcher-match-value
	 matcher-match-matcher
	 matcher-append
	 matcher-relabel

	 ;; Projections
	 compile-projection
	 compile-projection*
	 projection->pattern
	 matcher-project
	 matcher-key-set
	 matcher-key-set/single

	 ;; Printing and Serialization
	 pretty-print-matcher
	 matcher->pretty-string
	 matcher->jsexpr
	 jsexpr->matcher

	 matcher-union-successes
	 matcher-intersect-successes
	 matcher-subtract-successes
	 matcher-match-matcher-successes
	 matcher-match-matcher-unit
	 matcher-project-success)

(require racket/set)
(require racket/match)
(require (only-in racket/port call-with-output-string with-output-to-string))
(require (only-in racket/class object?))
(require "canonicalize.rkt")
(require "treap.rkt")
(require data/order)

(require rackunit)

;; TODO: perhaps avoid the parameters on the fast-path, if they are
;; causing measurable slowdown.
;; TODO: should these even be parameterizable?

(define matcher-union-successes
  (make-parameter
   (lambda (v1 v2)
     (match* (v1 v2)
       [(#t v) v]
       [(v #t) v]
       [(v1 v2) (set-union v1 v2)]))))

(define matcher-intersect-successes (make-parameter set-union))

(define matcher-subtract-successes
  (make-parameter
   (lambda (s1 s2)
     (define r (set-subtract s1 s2))
     (if (set-empty? r) #f r))))

(define matcher-match-matcher-successes
  (make-parameter
   (lambda (v1 v2 a)
     (cons (set-union (car a) v1)
	   (set-union (cdr a) v2)))))

(define matcher-match-matcher-unit (make-parameter (cons (set) (set))))

;; The project-success function should return #f to signal "no success values".
(define matcher-project-success (make-parameter values))

;; Constructs a structure type and a singleton instance of it.
(define-syntax-rule (define-singleton-struct singleton-name struct-name print-representation)
  (begin
    (struct struct-name ()
	    #:transparent
	    #:property prop:custom-write
	    (lambda (v port mode) (display print-representation port)))
    (define singleton-name (struct-name))))

;; A Matcher is either
;; - #f, indicating no further matches possible
;; - (success Any), representing a successful match (if the end of
;;   the input has been reached)
;; - (Treap (U Sigma Wildcard) Matcher), {TODO}
;; TODO::: reimplement to use (ordinary-state (Option Matcher) (Treap Sigma Matcher)), {TODO}
;; - (wildcard-sequence Matcher), {TODO}
;; If, in a treap matcher, a wild key is present, it is intended
;; to catch all and ONLY those keys not otherwise present in the
;; table.
;; INVARIANT: if a key is present in a treap, then the
;;            corresponding value MUST NOT be equal to the wildcard
;;            continuation, bearing in mind that
;;             - if the wildcard is absent, it is implicitly #f;
;;             - (key-open?) keys imply rwildseq of the wild continuation
;;             - (key-close?) keys imply runwildseq of the wild continuation
;; INVARIANT: success only appears right at the end. Never in the middle. Never unbalanced parens. TODO
;;            TODO as part of this: figure out whether we can get rid of the seemingly mandatory EOS-success
;;                 pattern that always shows up
(struct success (value) #:transparent)
(struct wildcard-sequence (matcher) #:transparent)

;; A Sigma is, roughly, a token in a value being matched. It is one of:
;;  - a struct-type, signifying the start of a struct.
;;  - SOL, signifying the start of a list.
;;  - SOV, signifying the start of a vector.
;;  - ILM, signifying the transition into the cdr position of a pair
;;  - EOS, signifying the notional close-paren at the end of a compound.
;;  - any other value, representing itself.
;; N.B. treaps cannot be Sigmas at present.
(define-singleton-struct SOL start-of-list "<")
(define-singleton-struct SOV start-of-vector "<vector")
(define-singleton-struct ILM improper-list-marker "|")
(define-singleton-struct EOS end-of-sequence ">")

;; A Pattern is an atom, the special wildcard value (?), an
;; (embedded-matcher Matcher), or a Racket compound (struct, pair, or
;; vector) containing Patterns.
(define-singleton-struct ? wildcard "★") ;; alternative printing: ¿
(struct embedded-matcher (matcher) #:transparent)

;; A Projection is an atom, the special wildcard value (?), a (capture
;; Pattern), or a Racket compound (struct, pair, or vector) containing
;; Projections. A Projection is much like a Pattern, but may include
;; captures, and may not include embedded matchers.
;;
;; When projecting a matcher, the capturing wildcard can be used.
(struct capture (pattern) #:transparent)

;; [Pattern] -> Projection
;; Construct a capture with default pattern of wildcard.
(define (?! [pattern ?]) (capture pattern))

;; A CompiledProjection is a (Listof (U Sigma ? SOC EOC)). Compiled
;; projections include start-of-capture and end-of-capture elements.
(define-singleton-struct SOC start-of-capture "{{")
(define-singleton-struct EOC end-of-capture "}}")

;; Any -> Boolean
;; Predicate recognising Matchers. Expensive!
(define (matcher? x)
  (or (eq? x #f)
      (success? x)
      (wildcard-sequence? x)
      (and (treap? x)
	   (for/and ([v (treap-values x)])
	     (matcher? v)))))

;; -> Matcher
;; The empty Matcher
(define (matcher-empty) #f)

;; Matcher -> Boolean
;; True iff the argument is the empty matcher
(define (matcher-empty? r) (not r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart constructors & accessors
;;
;; Maintain this INVARIANT: A Matcher is non-empty iff it contains
;; some keys that map to some Values. Essentially, don't bother
;; prepending tokens to a Matcher unless there's some possibility it
;; can map to one or more Values.

;; Matcher Matcher -> Boolean
;; Exploits canonicalization to replace an expensive equal? check with eq?.
(define (requal? a b)
  (eq? a b))

;; (Option Value) -> Matcher
;; If the argument is #f, returns the empty matcher; otherwise, a success Matcher.
(define (rsuccess v)
  (and v (canonicalize (success v))))

;; Order for sigmas
(define (sigma-order a b)
  (define sta? (struct-type? a))
  (define stb? (struct-type? b))
  (cond
   [(and sta? stb?) (datum-order (struct-type-name a) (struct-type-name b))]
   [sta? '<]
   [stb? '>]
   [else (datum-order a b)]))

;; (Treap (U Sigma Wildcard) Matcher)
;; The empty branch-matcher
(define empty-smap (treap-empty sigma-order))

;; (U Sigma Wildcard) Matcher -> Matcher
;; Prepends e to r, if r is non-empty.
(define (rseq e r)
  (if (matcher-empty? r)
      r
      (treap-insert empty-smap e r)))

;; [ (U Sigma Wildcard) Matcher ] ... -> Matcher
(define (rseq-multi . ers)
  (let walk ((ers ers))
    (match ers
      [(list* e r rest) (treap-insert (walk rest) e r)]
      [(list) empty-smap])))

;; Matcher -> Matcher
;; Prepends the wildcard pseudo-Sigma to r, if r is non-empty.
(define (rwild r)
  (rseq ? r))

;; Matcher -> Matcher
;; If r is non-empty, returns a matcher that consumes input up to and
;; including EOS, then continuing with r.
(define (rwildseq r)
  (if (matcher-empty? r) r (canonicalize (wildcard-sequence r))))

;; Matcher -> Matcher
;; If r is a wildcard-sequence, return the continuation expected after
;; the wilds and EOS. Otherwise, return the empty/failing matcher.
(define (runwildseq r)
  (match r
    [(wildcard-sequence k) k]
    [_ #f]))

;; Matcher (U Sigma Wildcard) Matcher -> Matcher
;; r must be a treap matcher. Retrieves the continuation after
;; accepting key. If key is absent, returns wild-edge-value, modified
;; depending on key.
(define (rlookup r key wild-edge-value)
  (treap-get r key (lambda ()
			  (cond
			   [(key-open? key) (rwildseq wild-edge-value)]
			   [(key-close? key) (runwildseq wild-edge-value)]
			   [else wild-edge-value]))))

;; (Option (Treap (U Sigma Wildcard) Matcher)) Sigma Matcher -> Matcher
;; Updates (installs or removes) a continuation in the Matcher r. r
;; must be either #f or a treap matcher. key MUST NOT be ?.
;; Preserves invariant that a key is never added if its continuation
;; is the same as the wildcard's continuation (which is implicitly #f
;; if absent, of course).
(define (rupdate r0 key k)
  (when (eq? key ?) (error 'rupdate "Internal error: supplied wildcard as key"))
  (define r (or r0 empty-smap))
  (empty-smap-guard
   (let ((old-wild (treap-get r ? (lambda () #f))))
     (if (cond [(key-open? key)
		(if (wildcard-sequence? k)
		    (requal? (wildcard-sequence-matcher k) old-wild)
		    (matcher-empty? k))]
	       [(key-close? key)
		(if (wildcard-sequence? old-wild)
		    (requal? (wildcard-sequence-matcher old-wild) k)
		    (matcher-empty? k))]
	       [else
		(requal? k old-wild)])
	 (treap-delete r key)
	 (treap-insert r key k)))))

;; Treap -> Matcher
;; If the argument is empty, returns the canonical empty matcher;
;; otherwise, returns the argument.
(define (empty-smap-guard h)
  (and (positive? (treap-size h)) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pattern compilation

;; Value (Listof Pattern) -> Matcher
;; Compiles a sequence of patterns into a matcher that accepts input
;; matching that sequence, yielding v.
(define (pattern->matcher* v ps0)
  ;; Pattern Matcher -> Matcher
  ;; acc is the continuation-matcher for the matcher created from ps.
  (define (walk-pair-chain ps acc)
    (match ps
      ['() (rseq EOS acc)]
      [(cons p ps) (walk p (walk-pair-chain ps acc))]
      [other (rseq ILM (walk other (rseq EOS acc)))]))

  ;; Pattern Matcher -> Matcher
  ;; acc is the continuation-matcher for the matcher created from p.
  (define (walk p acc)
    (match p
      [(capture sub) (error 'pattern->matcher* "Embedded capture in one of the patterns ~v" ps0)]
      [(== ?) (rwild acc)]
      [(cons p1 p2) (rseq SOL (walk p1 (walk-pair-chain p2 acc)))]
      [(? vector? v) (rseq SOV (vector-foldr walk (rseq EOS acc) v))]
      [(embedded-matcher m) (matcher-append m (lambda (_mv) acc))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'pattern->matcher "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (rseq (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      [other (rseq other acc)]))

  (walk-pair-chain ps0 (rsuccess v)))

;; Value Pattern* -> Matcher
;; Convenience form of pattern->matcher*.
(define (pattern->matcher v . ps)
  (pattern->matcher* v ps))

;; Structure -> StructType
;; Errors when given any struct that isn't completely transparent/prefab.
(define (struct->struct-type p)
  (define-values (t skipped?) (struct-info p))
  (when skipped? (error 'struct->struct-type "Cannot reflect on struct instance ~v" p))
  t)

;; Any -> Boolean
;; Racket objects are structures, so we reject them explicitly for
;; now, leaving them opaque to unification.
(define (non-object-struct? x)
  (and (struct? x)
       (not (object? x))))

;; (A B -> B) B (Vectorof A) -> B
(define (vector-foldr kons knil v)
  (for/fold [(acc knil)] [(elem (in-vector v (- (vector-length v) 1) -1 -1))]
    (kons elem acc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matcher combinators

;; Matcher Matcher -> Matcher
;; Computes the union of the multimaps passed in.
(define (matcher-union re1 re2)
  (matcher-recurse re1
		   re2
		   matcher-union
		   (matcher-union-successes)
		   values
		   values
		   values
		   values))

;; (A B -> C) -> A B -> B A -> C
(define ((flip f) a b) (f b a))

;; Matcher Matcher -> Matcher
;; Computes the intersection of the multimaps passed in.
(define (matcher-intersect re1 re2)
  (matcher-recurse re1
		   re2
		   matcher-intersect
		   (matcher-intersect-successes)
		   (lambda (r) #f)
		   (lambda (r) #f)
		   (lambda (h) #f)
		   (lambda (h) #f)))

;; Matcher Matcher -> Matcher
;; Removes re2's mappings from re1.
;; The combine-successes function should return #f to signal "no remaining success values".
(define (matcher-subtract re1 re2)
  (matcher-recurse re1
		   re2
		   matcher-subtract
		   (matcher-subtract-successes)
		   (lambda (r) #f)
		   values
		   (lambda (h) #f)
		   values))

(define (matcher-recurse re1 re2 f vf left-false right-false right-base left-base)
  (match* (re1 re2)
    [(#f r) (left-false r)]
    [(r #f) (right-false r)]
    [((wildcard-sequence r1) (wildcard-sequence r2)) (rwildseq (f r1 r2))]
    [((wildcard-sequence r1) r2) (f (expand-wildseq r1) r2)]
    [(r1 (wildcard-sequence r2)) (f r1 (expand-wildseq r2))]
    [((success v1) (success v2)) (rsuccess (vf v1 v2))]
    [((? treap? h1) (? treap? h2))
     (fold-over-keys h1 h2 f (left-base h1) (right-base h2))]))

(define (fold-over-keys h1 h2 f left-base right-base)
  (define w1 (rlookup h1 ? #f))
  (define w2 (rlookup h2 ? #f))
  (collapse-wildcard-sequences
   (cond
    [(and w1 w2)
     (for/fold [(acc (rwild (f w1 w2)))]
	 [(key (set-remove (set-union (treap-keys h1) (treap-keys h2)) ?))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [w1
     (for/fold [(acc left-base)] [(key (treap-keys h2))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [w2
     (for/fold [(acc right-base)] [(key (treap-keys h1))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [(< (treap-size h1) (treap-size h2))
     (for/fold [(acc right-base)] [(key (treap-keys h1))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))]
    [else
     (for/fold [(acc left-base)] [(key (treap-keys h2))]
       (rupdate acc key (f (rlookup h1 key w1) (rlookup h2 key w2))))])))

;; Matcher -> Matcher
;; When a matcher contains only entries for (EOS -> m') and (★ ->
;; (wildcard-sequence m')), it is equivalent to (wildcard-sequence m')
;; itself. This is the inverse of expand-wildseq.
;;
;; In addition, we rewrite (★ -> (wildcard-sequence m')) to
;; (wildcard-sequence m'), since matcher-match-value will fall back to
;; ★ if EOS is missing, and rlookup adjusts appropriately.
(define (collapse-wildcard-sequences m)
  (if (treap? m)
      (case (treap-size m)
	[(2)
	 (if (and (treap-has-key? m ?)
		  (treap-has-key? m EOS))
	     (let ((w (treap-get m ?))
		   (k (treap-get m EOS)))
	       (if (and (wildcard-sequence? w)
			(requal? (wildcard-sequence-matcher w) k))
		   w
		   m))
	     m)]
	[(1)
	 (if (treap-has-key? m ?)
	     (let ((w (treap-get m ?)))
	       (if (wildcard-sequence? w)
		   w
		   m))
	     m)]
	[else m])
      m))

;; Sigma -> Boolean
;; True iff k represents the start of a compound datum.
(define (key-open? k)
  (or (eq? k SOL)
      (eq? k SOV)
      (struct-type? k)))

;; Sigma -> Boolean
;; True iff k represents the end of a compound datum.
(define (key-close? k)
  (eq? k EOS))

;; Matcher -> Matcher
;; Unrolls the implicit recursion in a wildcard-sequence.
(define (expand-wildseq r)
  (treap-insert (treap-insert empty-smap ? (rwildseq r)) EOS r))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Matching single keys into a multimap

;; (Listof Sigma) -> (Listof Sigma)
;; Hackish support for improper lists. TODO: revisit
;; Converts an improper list into a proper one with ILM in the penultimate position.
(define (transform-list-value xs)
  (match xs
    ['() '()]
    [(cons x xs) (cons x (transform-list-value xs))]
    [other (cons ILM (cons other '()))]))

;; Matcher InputValue [Value] -> Value
;; Converts the nested structure v on-the-fly into a sequence of
;; Sigmas and runs them through the Matcher r. If v leads to a success
;; Matcher, returns the values contained in the success Matcher;
;; otherwise, returns failure-result.
(define (matcher-match-value r v [failure-result (set)])
  (let walk ((vs (list v)) (stack '(())) (r r))
    (match r
      [#f failure-result]
      [(wildcard-sequence k)
       (match stack
	 ['() failure-result]
	 [(cons rest stack1) (walk rest stack1 k)])]
      [(success result)
       (if (and (null? vs)
		(null? stack))
	   result
	   failure-result)]
      [(? treap?)
       (define (get key) (treap-get r key (lambda () #f)))
       (match vs
	 ['()
	  (match stack
	    ['() failure-result]
	    [(cons rest stack1)
	     (walk rest stack1 (rlookup r EOS (get ?)))])]
	 [(cons (== ?) rest)
	  (error 'matcher-match-value "Cannot match wildcard as a value")]
	 [(cons (cons v1 v2) rest)
	  (match (get SOL)
	    [#f (walk rest stack (get ?))]
	    [k (walk (cons v1 (transform-list-value v2)) (cons rest stack) k)])]
	 [(cons (vector vv ...) rest)
	  (match (get SOV)
	    [#f (walk rest stack (get ?))]
	    [k (walk vv (cons rest stack) k)])]
	 [(cons (? non-object-struct? s) rest)
	  (match (get (struct->struct-type s))
	    [#f (walk rest stack (get ?))]
	    [k (walk (cdr (vector->list (struct->vector s))) (cons rest stack) k)])]
	 [(cons v rest)
	  (walk rest stack (rlookup r v (get ?)))])])))

;; Matcher Matcher -> Value
;;
;; Similar to matcher-match-value, but instead of a single key,
;; accepts a Matcher serving as *multiple* simultaneously-examined
;; keys. Returns the union of all successful values reached by the
;; probe.
(define (matcher-match-matcher re1 re2)
  (let walk ((re1 re1) (re2 re2) (acc (matcher-match-matcher-unit)))
    (match* (re1 re2)
      [(#f _) acc]
      [(_ #f) acc]
      [((wildcard-sequence r1) (wildcard-sequence r2)) (walk r1 r2 acc)]
      [((wildcard-sequence r1) r2) (walk (expand-wildseq r1) r2 acc)]
      [(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2) acc)]
      [((success v1) (success v2)) ((matcher-match-matcher-successes) v1 v2 acc)]
      [((? treap? h1) (? treap? h2))
       (define w1 (rlookup h1 ? #f))
       (define w2 (rlookup h2 ? #f))
       (define r (walk w1 w2 acc))
       (for/fold [(r r)]
	   [(key (cond
		  [(and w1 w2) (set-remove (set-union (treap-keys h1) (treap-keys h2)) ?)]
		  [w1 (treap-keys h2)]
		  [w2 (treap-keys h1)]
		  [(< (treap-size h1) (treap-size h2)) (treap-keys h1)]
		  [else (treap-keys h2)]))]
	 (walk (rlookup h1 key w1) (rlookup h2 key w2) r))])))

;; Matcher × (Value → Matcher) → Matcher
;; Since Matchers accept *sequences* of input values, this appends two
;; matchers into a single matcher that accepts their concatenation.
;; Because matchers map inputs to values, the second matcher is
;; expressed as a function from success-values from the first matcher
;; to a second matcher.
(define (matcher-append m0 m-tail-fn)
  (let walk ((m m0))
    (match m
      [#f #f]
      [(success v) (error 'matcher-append "Ill-formed matcher: ~v" m0)]
      [(wildcard-sequence m1) (rwildseq (walk m1))]
      [(? treap?) (for/fold [(acc (rwild (walk (rlookup m ? #f))))]
			   [(kv (treap-to-alist m)) #:when (not (eq? (car kv) ?))]
			 (match-define (cons k v) kv)
			 (if (and (key-close? k) (success? v))
			     (matcher-union acc (m-tail-fn (success-value v)))
			     (rupdate acc k (walk v))))])))

;; Matcher (Value -> (Option Value)) -> Matcher
;; Maps f over success values in m.
(define (matcher-relabel m f)
  (let walk ((m m))
    (match m
      [#f #f]
      [(success v) (rsuccess (f v))]
      [(wildcard-sequence m1) (rwildseq (walk m1))]
      [(? treap?) (for/fold [(acc (rwild (walk (rlookup m ? #f))))]
			   [(kv (treap-to-alist m)) #:when (not (eq? (car kv) ?))]
			 (rupdate acc (car kv) (walk (cdr kv))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Projection

;; (Listof Projection) -> CompiledProjection
;; Compiles a sequence of projections into a single CompiledProjection
;; for use with matcher-project.
(define (compile-projection* ps0)
  (define (walk-pair-chain ps acc)
    (match ps
      ['() (cons EOS acc)]
      [(cons p ps) (walk p (walk-pair-chain ps acc))]
      [other (cons ILM (walk other (cons EOS acc)))]))

  (define (walk p acc)
    (match p
      [(capture sub) (cons SOC (walk sub (cons EOC acc)))] ;; TODO: enforce non-nesting here
      [(== ?) (cons ? acc)]
      [(cons p1 p2) (cons SOL (walk p1 (walk-pair-chain p2 acc)))]
      [(? vector? v) (cons SOV (vector-foldr walk (cons EOS acc) v))]
      [(embedded-matcher m) (error 'compile-projection "Cannot embed matcher in projection")]
      ;; TODO: consider options for treating treaps as compounds rather than (useless) atoms
      [(? treap?) (error 'compile-projection "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (cons (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      [other (cons other acc)]))

  (walk-pair-chain ps0 '()))

;; Projection* -> CompiledProjection
;; Convenience form of compile-projection*.
(define (compile-projection . ps)
  (compile-projection* ps))

;; Projection -> Pattern
;; Strips captures from its argument, returning an equivalent non-capturing pattern.
(define (projection->pattern p)
  (let walk ((p p))
    (match p
      [(capture sub) sub] ;; TODO: maybe enforce non-nesting here too?
      [(cons p1 p2) (cons (walk p1) (walk p2))]
      [(? vector? v) (for/vector [(e (in-vector v))] (walk e))]
      ;; TODO: consider options for treating treaps as compounds
      ;; rather than (useless) atoms
      [(? treap?) (error 'projection->pattern "Cannot match on treaps at present")]
      [(? non-object-struct?)
       (apply (struct-type-make-constructor (struct->struct-type p))
	      (map walk (cdr (vector->list (struct->vector p)))))]
      [other other])))

;; Matcher × CompiledProjection -> Matcher
;; The result matches a sequence of inputs of length equal to the number of captures.
(define matcher-project
  (let ()
    (define (drop-match m spec) (general-match values drop-edge drop-sigma drop-balanced m spec))
    (define (take-match m spec) (general-match rwild  rupdate   rseq       take-balanced m spec))

    (define (drop-balanced m k) (general-balanced values   values drop-edge m k))
    (define (take-balanced m k) (general-balanced rwildseq rwild  rupdate   m k))

    (define (drop-edge acc key k) (matcher-union acc k))
    (define (drop-sigma sigma k) k)

    (define (general-match add-wild add-edge add-sigma balanced m spec)
      (let walk ((m m) (spec spec))
	(match spec
	  ['()
	   (match m
	     [(success v) (rseq EOS (rsuccess ((matcher-project-success) v)))]
	     [_ (matcher-empty)])]

	  [(cons (== EOC) k) (drop-match m k)]
	  [(cons (== SOC) k) (take-match m k)]

	  [(cons (== ?) k)
	   (match m
	     [(wildcard-sequence _) (add-wild (walk m k))]
	     [(? treap?)
	      (for/fold [(acc (add-wild (walk (rlookup m ? #f) k)))]
		  [(key-mk (treap-to-alist m)) #:when (not (eq? (car key-mk) ?))]
		(match-define (cons key mk) key-mk)
		(add-edge acc key (cond
				   [(key-open? key) (balanced mk (lambda (mk) (walk mk k)))]
				   [(key-close? key) #f]
				   [else (walk mk k)])))]
	     [_ (matcher-empty)])]

	  [(cons sigma k)
	   (add-sigma sigma
		      (match m
			[(wildcard-sequence mk)
			 (cond
			  [(key-open? sigma) (walk (rwildseq m) k)]
			  [(key-close? sigma) (walk mk k)]
			  [else (walk m k)])]
			[(? treap?) (walk (rlookup m sigma (rlookup m ? #f)) k)]
			[_ (matcher-empty)]))])))

    (define (general-balanced add-wildseq add-wild add-edge m k)
      (let walk ((m m) (k k))
	(match m
	  [(wildcard-sequence mk) (add-wildseq (k mk))]
	  [(? treap?)
	   (for/fold [(acc (add-wild (walk (rlookup m ? #f) k)))]
	       [(key-mk (treap-to-alist m)) #:when (not (eq? (car key-mk) ?))]
	     (match-define (cons key mk) key-mk)
	     (add-edge acc key (cond
				[(key-open? key) (walk mk (lambda (mk) (walk mk k)))]
				[(key-close? key) (k mk)]
				[else (walk mk k)])))]
	  [_ (matcher-empty)])))

    drop-match))

;; (Listof Sigma) -> (Listof Sigma)
;; Hackish support for improper lists. TODO: revisit
;; Undoes the transformation of transform-list-value, converting
;; ILM-marked proper lists back into improper ones.
(define (untransform-list-value vs)
  (match vs
    ['() '()]
    [(cons (== ILM) (cons v '())) v]
    [(cons (== ILM) _) (error 'untransform-list-value "Illegal use of ILM" vs)]
    [(cons v vs) (cons v (untransform-list-value vs))]))

;; Matcher → (Option (Setof (Listof Value)))
;; Extracts the "keys" in its argument multimap m, representing input
;; sequences as lists. Multiplies out unions. Returns #f if any
;; dimension of m is infinite.
(define matcher-key-set
  (let ()
    ;; Matcher (Value Matcher -> (Setof Value)) -> (Option (Setof Value))
    ;; Calls k with each possible atomic value at this matcher
    ;; position, and accumulates the results.
    (define (walk m k)
      (match m
	[(wildcard-sequence _) #f]
	[(? treap?)
	 (and (not (treap-has-key? m ?))
	      (for/fold [(acc (set))] [(key-mk (treap-to-alist m))]
		(match-define (cons key mk) key-mk)
		(maybe-union
		 acc
		 (cond
		  [(key-open? key)
		   (walk-seq mk (lambda (vss vsk)
				  (for/fold [(acc (set))] [(vs (in-set vss))]
				    (maybe-union acc
						 (k (transform-seqs vs key) vsk)))))]
		  [(key-close? key)
		   (error 'matcher-key-set "Internal error: unexpected key-close")]
		 [else
		  (k key mk)]))))]
	[_ (set)]))

    ;; Matcher (Value Matcher -> (Setof (Listof Value))) -> (Option (Setof (Listof Value)))
    ;; Calls k with each possible sequence of atomic values at this
    ;; matcher position, and accumulates the results.
    (define (walk-seq m k)
      (match m
	[(wildcard-sequence _) #f]
	[(? treap?)
	 (and (not (treap-has-key? m ?))
	      (for/fold [(acc (set))] [(key-mk (treap-to-alist m))]
		(match-define (cons key mk) key-mk)
		(maybe-union acc (cond
				  [(key-close? key) (k (set '()) mk)]
				  [else (walk (rseq key mk)
					      (lambda (v vk)
						(walk-seq vk (lambda (vss vsk)
							       (k (for/set [(vs (in-set vss))]
								    (cons v vs))
								  vsk)))))]))))]
	[_ (k (set) #f)]))

    ;; (Listof Value) Sigma -> Value
    (define (transform-seqs vs opener)
      (cond
       [(eq? opener SOL) (untransform-list-value vs)]
       [(eq? opener SOV) (list->vector vs)]
       [(struct-type? opener) (apply (struct-type-make-constructor opener) vs)]))

    ;; (Option (Setof A)) (Option (Setof A)) -> (Option (Setof A))
    (define (maybe-union s1 s2) (and s1 s2 (set-union s1 s2)))

    (lambda (m)
      (walk-seq m (lambda (vss vsk) vss)))))

;; Matcher → (Option (Setof Value))
;; As matcher-key-set, but extracts just the first captured subvalue.
(define (matcher-key-set/single m)
  (define vss (matcher-key-set m))
  (and vss (for/set [(vs (in-set vss))] (car vs))))

;; struct-type -> Symbol
;; Extract just the name of the given struct-type.
(define (struct-type-name st)
  (define-values (name x2 x3 x4 x5 x6 x7 x8) (struct-type-info st))
  name)

;; Matcher [OutputPort] [#:indent Nat] -> Void
;; Pretty-prints the given matcher on the given port, with
;; second-and-subsequent lines indented by the given amount.
(define (pretty-print-matcher m [port (current-output-port)] #:indent [initial-indent 0])
  (define (d x) (display x port))
  (define (walk i m)
    (match m
      [#f
       (d "::: no further matches possible")]
      [(wildcard-sequence k)
       (d " ...>")
       (walk (+ i 5) k)]
      [(success vs)
       (d "{")
       (d vs)
       (d "}")]
      [(? treap? h)
       (if (zero? (treap-size h))
	   (d " ::: empty treap!")
	   (for/fold [(need-sep? #f)] [(key-k (treap-to-alist h))]
	     (match-define (cons key k) key-k)
	     (when need-sep?
	       (newline port)
	       (d (make-string i #\space)))
	     (d " ")
	     (define keystr (call-with-output-string
			     (lambda (p)
			       (cond
				[(struct-type? key)
				 (display "<s:" p)
				 (display (struct-type-name key) p)]
				[else
				 (write key p)]))))
	     (d keystr)
	     (walk (+ i 1 (string-length keystr)) k)
	     #t))]))
  (walk initial-indent m)
  (newline port)
  m)

(define (matcher->pretty-string m #:indent [initial-indent 0])
  (with-output-to-string (lambda () (pretty-print-matcher m #:indent initial-indent))))

;; Matcher (Value -> JSExpr) -> JSExpr
;; Serializes a matcher to a JSON expression.
(define (matcher->jsexpr m success->jsexpr)
  (let walk ((m m))
    (match m
      [#f '()]
      [(success v) (list "" (success->jsexpr v))]
      [(wildcard-sequence m1) (list "...)" (walk m1))]
      [(? treap?)
       (for/list [(kv (treap-to-alist m))]
	 (match-define (cons k v) kv)
	 (list (match k
		 [(== ?) (list "__")]
		 [(== SOL) (list "(")]
		 [(== SOV) (list "#(")]
		 [(== EOS) (list ")")]
		 [(? struct-type? t)
		  (list (string-append (symbol->string (struct-type-name t)) "("))]
		 [else k])
	       (walk v)))])))

;; String -> String
;; Undoes the encoding of struct-type names used in the JSON serialization of Matchers.
(define (deserialize-struct-type-name stn)
  (define expected-paren-pos (- (string-length stn) 1))
  (and (char=? (string-ref stn expected-paren-pos) #\()
       (substring stn 0 expected-paren-pos)))

;; JSExpr (JSExpr -> Value) [String -> (Option struct-type)] -> Matcher
;; Deserializes a matcher from a JSON expression.
(define (jsexpr->matcher j jsexpr->success [struct-type-name->struct-type (lambda () #f)])
  (let walk ((j j))
    (match j
      ['() #f]
      [(list "" vj) (rsuccess (jsexpr->success vj))]
      [(list "...)" j1) (rwildseq (walk j1))]
      [(list (list kjs vjs) ...)
       (for/fold [(acc empty-smap)]
	   [(kj kjs) (vj vjs)]
	 (treap-insert acc
			    (match kj
			      [(list "__") ?]
			      [(list "(") SOL]
			      [(list "#(") SOV]
			      [(list ")") EOS]
			      [(list (? string? s))
			       (match (deserialize-struct-type-name s)
				 [#f (error 'jsexpr->matcher
					    "Illegal open-parenthesis mark ~v"
					    kj)]
				 [tn (match (struct-type-name->struct-type tn)
				       [#f (error 'jsexpr->matcher
						  "Unexpected struct type ~v"
						  tn)]
				       [t t])])]
			      [other other])
			    (walk vj)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/pretty)

  (define SA (set 'A))
  (define SB (set 'B))
  (define SC (set 'C))
  (define SD (set 'D))
  (define Sfoo (set 'foo))
  (define S+ (set '+))
  (define SX (set 'X))
  (define (E v) (rseq EOS (rsuccess v)))
  (check-equal? (pattern->matcher SA 123) (rseq 123 (E SA)))
  (check-equal? (pattern->matcher SA (cons 1 2))
		(rseq SOL (rseq 1 (rseq ILM (rseq 2 (rseq EOS (E SA)))))))
  (check-equal? (pattern->matcher SA (cons ? 2))
		(rseq SOL (rseq ? (rseq ILM (rseq 2 (rseq EOS (E SA)))))))
  (check-equal? (pattern->matcher SA (list 1 2)) (rseq SOL (rseq 1 (rseq 2 (rseq EOS (E SA))))))
  (check-equal? (pattern->matcher SA (list ? 2)) (rseq SOL (rseq ? (rseq 2 (rseq EOS (E SA))))))
  (check-equal? (pattern->matcher SA SOL) (rseq struct:start-of-list (rseq EOS (E SA))))
  (check-equal? (pattern->matcher SA ?) (rseq ? (E SA)))
  )

(module+ test
  (define (check-matches matcher . tests)
    (let walk ((tests tests))
      (match tests
	['() (void)]
	[(list* message expectedstr rest)
	 (define actualset (matcher-match-value matcher message))
	 (printf "~v ==> ~v\n" message actualset)
	 (check-equal? actualset
		       (apply set (map (lambda (c) (string->symbol (string c)))
				       (string->list expectedstr))))
	 (walk rest)])))

  (check-matches
   #f
   (list 'z 'x) ""
   'foo ""
   (list (list 'z (list 'z))) "")

  (define (pretty-print-matcher* m)
    (newline)
    (pretty-print-matcher m)
    (flush-output)
    m)
 
  (void (pretty-print-matcher*
	 (matcher-union (pattern->matcher SA (list (list ?) 'x))
			(pattern->matcher SB (list (list ?) 'y)))))

  (void (pretty-print-matcher*
	 (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
			(pattern->matcher SB (list (list 'c 'd) 'y)))))

  (void (pretty-print-matcher*
	 (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
			(pattern->matcher SB (list (list  ?  ?) 'y)))))

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
		   (pattern->matcher SB (list (list  ?  ?) 'x))))
   (list 'z 'x) ""
   (list (list 'z 'z) 'x) "B"
   (list (list 'z (list 'z)) 'x) "B"
   (list (list 'a 'b) 'x) "AB")

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
		   (pattern->matcher SB (list (list ?)     'y))))
   (list 'z 'y) ""
   (list (list 'z 'z) 'y) ""
   (list (list 'z 'z) 'x) ""
   (list (list 'a 'b) 'x) "A")

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
		   (pattern->matcher SB (list ? 'y))))
   (list 'z 'y) "B"
   (list (list 'z 'z) 'y) "B"
   (list (list 'a 'b) 'x) "A")

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list 'a 'b))
		   (pattern->matcher SB (list 'c 'd))))
   (list 'a 'b) "A"
   (list 'c 'd) "B"
   (list 'a 'd) ""
   (list 'c 'b) "")

  (void (pretty-print-matcher* (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
					      ;; Note: this is a largely nonsense matcher,
					      ;; since it expects no input at all
					      (rseq EOS (rsuccess (set 'B))))))

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list (list 'a 'b) 'x))
		   (pattern->matcher SB ?)))
   (list (list 'a 'b) 'x) "AB"
   'p "B"
   (list 'p) "B")

  (check-matches
   (pretty-print-matcher*
    (matcher-union (pattern->matcher SA (list 'a ?))
		   (pattern->matcher SB (list 'a (list 'b)))))

   (list 'a (list 'b)) "AB"
   (list 'a (list 'b 'b)) "A"
   (list 'a (list 'c 'c)) "A"
   (list 'a (list 'c)) "A"
   (list 'a (list (list))) "A"
   (list 'a (list)) "A"
   (list 'a 'x) "A")

  (check-matches
   (pretty-print-matcher*
    (matcher-union (matcher-union (pattern->matcher SA (list 'a ?))
				  (pattern->matcher SA (list 'q ?)))
		   (pattern->matcher SB (list 'a (list 'b)))))
   (list 'a (list 'b)) "AB"
   (list 'q (list 'b)) "A"
   (list 'a 'x) "A"
   (list 'q 'x) "A"
   (list 'a (list)) "A"
   (list 'q (list)) "A"
   (list 'z (list)) "")

  (define (bigdemo)
    (define ps
      (for/list ((c (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")))
	(define csym (string->symbol (string c)))
	(pattern->matcher (set csym) (list csym ?))))
    (matcher-union (foldr matcher-union (matcher-empty) ps)
		   (pattern->matcher S+ (list 'Z (list ? '- ?)))))

  (newline)
  (printf "Plain bigdemo\n")

  (void (pretty-print-matcher* (bigdemo)))
  (check-matches
   (bigdemo)
   (list 'a '-) "a"
   (list 'Z '-) "Z"
   (list '? '-) ""
   (list 'a (list '- '- '-)) "a"
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "a"
   (list 'Z) ""
   (list 'Z 'x) "Z"
   (list 'Z (list)) "Z"
   (list 'Z (list '-)) "Z"
   (list 'Z (list '- '-)) "Z"
   (list 'Z (list '- '- '-)) "Z+"
   (list 'Z (list '- '- '- '-)) "Z"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "Z"
   (list 'Z '((()) - -)) "Z+"
   (list '? (list '- '- '-)) "")

  ;; ;; Having switched from pair-based matching to list-based matching,
  ;; ;; it's no longer supported to match with a wildcard in the cdr of a
  ;; ;; pair. Or rather, it is, but it won't work reliably: when the
  ;; ;; value to be matched is a proper list, it will fail to match.
  ;; ;; Consequently: Don't Do That.
  ;; (check-matches (pretty-print-matcher* (pattern->matcher SA (list* 'a 'b ?)))
  ;; 		 (list 'a 'b 'c 'd 'e 'f) "A"
  ;; 		 (list 'b 'c 'd 'e 'f 'a) ""
  ;; 		 3 "")

  (newline)
  (printf "bigdemo with matcher-intersect 'a -> SA | 'b -> SB\n")

  (void (pretty-print-matcher* (matcher-intersect (pattern->matcher SA (list 'a))
						  (pattern->matcher SB (list 'b)))))

  (newline)
  (printf "various unions and intersections\n")

  (let ((r1 (matcher-union (pattern->matcher SA (list  ? 'b))
			   (pattern->matcher SA (list  ? 'c))))
	(r2 (matcher-union (pattern->matcher SB (list 'a  ?))
			   (pattern->matcher SB (list 'b  ?)))))
    (pretty-print-matcher* (matcher-union r1 r2))
    (pretty-print-matcher* (matcher-union r1 r1))
    (pretty-print-matcher* (matcher-union r2 r2))
    (pretty-print-matcher* (matcher-intersect r1 r2))
    (pretty-print-matcher* (matcher-intersect r1 r1))
    (pretty-print-matcher* (matcher-intersect r2 r2))
    (void))

  (newline)
  (printf "bigdemo with matcher-intersect ('m 'n) -> SX\n")

  (check-matches
   (pretty-print-matcher* (matcher-intersect (bigdemo) (pattern->matcher SX (list 'm 'n))))
   (list 'm '-) ""
   (list 'm 'n) "mX"
   (list 'x '-) ""
   (list 'x 'n) "")

  (newline)
  (printf "bigdemo with matcher-intersect ('Z ?) -> SX\n")

  (check-matches
   (pretty-print-matcher* (matcher-intersect (bigdemo) (pattern->matcher SX (list 'Z ?))))
   (list 'a '-) ""
   (list 'Z '-) "XZ"
   (list '? '-) ""
   (list 'a (list '- '- '-)) ""
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) ""
   (list 'Z) ""
   (list 'Z 'x) "XZ"
   (list 'Z (list)) "XZ"
   (list 'Z (list '-)) "XZ"
   (list 'Z (list '- '-)) "XZ"
   (list 'Z (list '- '- '-)) "XZ+"
   (list 'Z (list '- '- '- '-)) "XZ"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "XZ"
   (list 'Z '((()) - -)) "XZ+"
   (list '? (list '- '- '-)) "")

  (newline)
  (printf "bigdemo with matcher-intersect ('Z ?) -> SX and changed success function\n")

  (check-matches
   (pretty-print-matcher* (parameterize ((matcher-intersect-successes (lambda (a b) b)))
			    (matcher-intersect (bigdemo) (pattern->matcher SX (list 'Z ?)))))
   (list 'a '-) ""
   (list 'Z '-) "X"
   (list '? '-) ""
   (list 'a (list '- '- '-)) ""
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) ""
   (list 'Z) ""
   (list 'Z 'x) "X"
   (list 'Z (list)) "X"
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

  (newline)
  (printf "bigdemo with matcher-intersect ? -> SX and changed success function\n")

  (check-matches
   (pretty-print-matcher* (parameterize ((matcher-intersect-successes (lambda (a b) b)))
			    (matcher-intersect (bigdemo) (pattern->matcher SX ?))))
   (list 'a '-) "X"
   (list 'Z '-) "X"
   (list '? '-) ""
   (list 'a (list '- '- '-)) "X"
   (list 'a (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z) ""
   (list 'Z 'x) "X"
   (list 'Z (list)) "X"
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

  (newline)
  (printf "subtraction basics\n")

  (let* ((r1 (pattern->matcher SA (list  ? 'b)))
	 (r2 (pattern->matcher SB (list 'a  ?)))
	 (r12 (matcher-union r1 r2)))
    (printf "\n-=-=-=-=-=-=-=-=- erase1\n")
    (pretty-print-matcher* r1)
    (pretty-print-matcher* r2)
    (pretty-print-matcher* r12)
    (pretty-print-matcher* (matcher-subtract r12 r1))
    (pretty-print-matcher* (matcher-subtract r12 r2))
    (void))

  (let* ((r1 (matcher-union (pattern->matcher SA (list 'a ?))
			    (pattern->matcher SA (list 'b ?))))
	 (r2 (pattern->matcher SB (list 'b ?)))
	 (r12 (matcher-union r1 r2)))
    (printf "\n-=-=-=-=-=-=-=-=- erase2\n")
    (pretty-print-matcher* r12)
    (pretty-print-matcher* (matcher-subtract r12 r1))
    (pretty-print-matcher* (matcher-subtract r12 r2))
    (pretty-print-matcher* (matcher-subtract r12 (pattern->matcher SA ?)))
    (void))

  )

(module+ test
  (struct a (x) #:prefab)
  (struct b (x) #:transparent)

  (define (intersect a b)
    (matcher-intersect (pattern->matcher SA a)
		       (pattern->matcher SB b)))

  (define EAB (E (set 'A 'B)))

  (define (rseq* x . xs)
    (let walk ((xs (cons x xs)))
      (match xs
	[(list r) r]
	[(cons e xs1) (rseq e (walk xs1))])))

  (define-syntax-rule (check-requal? actual expected)
    (check-eq? actual expected))

  (check-requal? (intersect ? ?) (rwild EAB))
  (check-requal? (intersect 'a ?) (rseq 'a EAB))
  (check-requal? (intersect 123 ?) (rseq 123 EAB))
  (check-requal? (intersect (cons ? 2) (cons 1 ?)) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list ? 2) (list 1 ?)) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect (cons 1 2) ?) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list 1 2) ?) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect 1 2) #f)
  (check-requal? (intersect (cons 1 2) (cons ? 2)) (rseq* SOL 1 ILM 2 EOS EAB))
  (check-requal? (intersect (list 1 2) (list ? 2)) (rseq* SOL 1 2 EOS EAB))
  (check-requal? (intersect (cons 1 2) (cons 3 2)) #f)
  (check-requal? (intersect (cons 1 2) (cons 1 3)) #f)
  (check-requal? (intersect (vector 1 2) (vector 1 2)) (rseq* SOV 1 2 EOS EAB))
  (check-requal? (intersect (vector 1 2) (vector 1 2 3)) #f)

  (check-requal? (intersect (a 'a) (a 'b)) #f)
  (check-requal? (intersect (a 'a) (a 'a)) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (a 'a) (a ?)) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (a 'a) ?) (rseq* struct:a 'a EOS EAB))
  (check-requal? (intersect (b 'a) (b 'b)) #f)
  (check-requal? (intersect (b 'a) (b 'a)) (rseq* struct:b 'a EOS EAB))
  (check-requal? (intersect (b 'a) (b ?)) (rseq* struct:b 'a EOS EAB))
  (check-requal? (intersect (b 'a) ?) (rseq* struct:b 'a EOS EAB))

  (check-requal? (intersect (a 'a) (b 'a)) #f)

  (check-exn #px"Cannot match on treaps at present"
	     (lambda ()
	       (define (h a b c d)
		 (treap-insert (treap-insert empty-smap a b) c d))
	       (intersect (h 'a 1 'b ?)
			  (h 'a ? 'b 2))))

  (let ((H rseq-multi))
    (newline)
    (printf "Checking that intersection with wildcard is identity-like\n")
    (define m1 (pretty-print-matcher*
		(foldr matcher-union (matcher-empty)
		       (list (pattern->matcher SA (list 'a ?))
			     (pattern->matcher SB (list 'b ?))
			     (pattern->matcher SC (list 'b 'c))))))
    (define m2 (pretty-print-matcher* (pattern->matcher SD ?)))
    (define mi (pretty-print-matcher* (matcher-intersect m1 m2)))
    (check-requal? mi
		   (H SOL (H 'a (H  ? (H EOS (E (set 'A 'D))))
			     'b (H  ? (H EOS (E (set 'B 'D)))
				    'c (H EOS (E (set 'B 'C 'D)))))))
    (check-requal? (pretty-print-matcher*
		    (parameterize ((matcher-intersect-successes (lambda (v1 v2) v1)))
		      (matcher-intersect m1 m2)))
		   m1))
  )

(module+ test
  (define (matcher-match-matcher-list m1 m2)
    (match-define (cons s1 s2) (matcher-match-matcher m1 m2))
    (list s1 s2))
  (let ((abc (foldr matcher-union (matcher-empty)
		    (list (pattern->matcher SA (list 'a ?))
			  (pattern->matcher SB (list 'b ?))
			  (pattern->matcher SC (list 'c ?)))))
	(bcd (foldr matcher-union (matcher-empty)
		    (list (pattern->matcher SB (list 'b ?))
			  (pattern->matcher SC (list 'c ?))
			  (pattern->matcher SD (list 'd ?))))))
    (check-equal? (matcher-match-matcher-list abc abc)
		  (list (set 'A 'B 'C) (set 'A 'B 'C)))
    (check-equal? (parameterize ((matcher-match-matcher-successes (lambda (v1 v2 a)
								    (set-union v2 a)))
				 (matcher-match-matcher-unit (set)))
		    (matcher-match-matcher abc abc))
		  (set 'A 'B 'C))
    (check-equal? (matcher-match-matcher-list abc (matcher-relabel bcd (lambda (old) (set #t))))
		  (list (set 'B 'C) (set #t)))
    (check-equal? (matcher-match-matcher-list abc (pattern->matcher Sfoo ?))
		  (list (set 'A 'B 'C) (set 'foo)))
    (check-equal? (matcher-match-matcher-list abc (pattern->matcher Sfoo (list ? ?)))
		  (list (set 'A 'B 'C) (set 'foo)))
    (check-equal? (matcher-match-matcher-list abc (pattern->matcher Sfoo (list ? 'x)))
		  (list (set 'A 'B 'C) (set 'foo)))
    (check-equal? (matcher-match-matcher-list abc (pattern->matcher Sfoo (list ? 'x ?)))
		  (list (set) (set)))))

(module+ test
  (check-equal? (compile-projection (cons 'a 'b))
		(list SOL 'a ILM 'b EOS EOS))
  (check-equal? (compile-projection (cons 'a (?!)))
		(list SOL 'a ILM SOC ? EOC EOS EOS))
  (check-equal? (compile-projection (list 'a 'b))
		(list SOL 'a 'b EOS EOS))
  (check-equal? (compile-projection (list 'a (?!)))
		(list SOL 'a SOC ? EOC EOS EOS))

  (parameterize ((matcher-project-success (lambda (v) #t)))
    (check-requal? (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a 'b)))
				    (compile-projection (list 'a (?!))))
		   (matcher-union (pattern->matcher #t 'a)
				  (pattern->matcher #t 'b)))

    (check-requal? (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a (vector 'b 'c 'd))))
				    (compile-projection (list 'a (?!))))
		   (matcher-union (pattern->matcher #t 'a)
				  (pattern->matcher #t (vector 'b 'c 'd))))

    (check-requal? (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a (vector 'b ? 'd))))
				    (compile-projection (list 'a (?!))))
		   (matcher-union (pattern->matcher #t 'a)
				  (pattern->matcher #t (vector 'b ? 'd))))

    (check-equal? (matcher-key-set
		   (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a 'b)))
				    (compile-projection (list 'a (?!)))))
		  (set '(a) '(b)))

    (check-equal? (matcher-key-set
		   (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a (vector 'b 'c 'd))))
				    (compile-projection (list 'a (?!)))))
		  (set '(a) '(#(b c d))))

    (check-equal? (matcher-key-set
		   (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a (vector 'b ? 'd))))
				    (compile-projection (list 'a (?!)))))
		  #f)

    (check-equal? (matcher-key-set
		   (matcher-project (matcher-union (pattern->matcher SA (list 'a 'a))
						   (pattern->matcher SB (list 'a (vector 'b ? 'd))))
				    (compile-projection (list 'a (?! 'a)))))
		  (set '(a)))

    (check-requal? (matcher-project (matcher-union (pattern->matcher SA (cons 1 2))
						   (pattern->matcher SB (cons 3 4)))
				    (compile-projection (cons (?!) (?!))))
		   (matcher-union (pattern->matcher #t 1 2)
				  (pattern->matcher #t 3 4)))

    (check-requal? (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SB (cons 1 4))
						 (pattern->matcher SC (cons 3 4))))
				    (compile-projection (cons (?!) (?!))))
		   (foldr matcher-union (matcher-empty)
			  (list (pattern->matcher #t 1 2)
				(pattern->matcher #t 1 4)
				(pattern->matcher #t 3 4))))

    (check-requal? (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SB (cons 1 4))
						 (pattern->matcher SC (cons 3 4))))
				    (compile-projection (?! (cons ? ?))))
		   (foldr matcher-union (matcher-empty)
			  (list (pattern->matcher #t (cons 1 2))
				(pattern->matcher #t (cons 1 4))
				(pattern->matcher #t (cons 3 4)))))

    (check-requal? (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SB (cons 1 4))
						 (pattern->matcher SC (cons 3 4))))
				    (compile-projection (?! (cons 1 ?))))
		   (foldr matcher-union (matcher-empty)
			  (list (pattern->matcher #t (cons 1 2))
				(pattern->matcher #t (cons 1 4)))))

    (check-requal? (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SB (cons 1 4))
						 (pattern->matcher SC (cons 3 4))))
				    (compile-projection (cons (?! 1) (?!))))
		   (foldr matcher-union (matcher-empty)
			  (list (pattern->matcher #t 1 2)
				(pattern->matcher #t 1 4))))

    (check-requal? (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SB (cons 1 4))
						 (pattern->matcher SC (cons 3 4))))
				    (compile-projection (cons (?!) (?! 4))))
		   (foldr matcher-union (matcher-empty)
			  (list (pattern->matcher #t 1 4)
				(pattern->matcher #t 3 4))))

    (check-equal? (matcher-key-set
		   (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons 1 2))
						 (pattern->matcher SC (cons ? 3))
						 (pattern->matcher SB (cons 3 4))))
				    (compile-projection (cons (?!) (?!)))))
		  #f)

    (check-equal? (matcher-key-set
		   (matcher-project (foldr matcher-union (matcher-empty)
					   (list (pattern->matcher SA (cons ? 2))
						 (pattern->matcher SC (cons 1 3))
						 (pattern->matcher SB (cons 3 4))))
				    (compile-projection (cons ? (?!)))))
		  (set '(2) '(3) '(4)))

    (check-equal? (matcher-key-set
		   (matcher-project (matcher-union (pattern->matcher SA (cons 1 2))
						   (pattern->matcher SB (cons 3 4)))
				    (compile-projection (cons (?!) (?!)))))
		  (set '(1 2) '(3 4))))

  (check-requal? (matcher-project (matcher-union (pattern->matcher SA ?)
						 (pattern->matcher SB (list 'a)))
				  (compile-projection (?! (list (list ?)))))
		 (pattern->matcher SA (list (list ?))))

  (check-equal? (projection->pattern (list 'a 'b)) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a ?)) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?!))) (list 'a ?))
  (check-equal? (projection->pattern (list 'a (?! 'b))) (list 'a 'b))
  (check-equal? (projection->pattern (list 'a (?! (vector 'b)))) (list 'a (vector 'b)))
  (check-equal? (projection->pattern (list 'a (?! (vector ? ?)))) (list 'a (vector ? ?)))
  )

(module+ test
  (newline)
  (printf "Checking that subtraction from union is identity-like\n")

  (let ((A (pattern->matcher SA ?))
	(B (pattern->matcher SB (list (list (list (list 'foo)))))))
    (check-requal? (pretty-print-matcher* (matcher-subtract (matcher-union A B) B))
		   A))
  (let ((A (pattern->matcher SA ?))
	(B (matcher-union (pattern->matcher SB (list (list (list (list 'foo)))))
			  (pattern->matcher SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-matcher* (matcher-subtract (matcher-union A B) B))
		   A))
  (let ((A (pattern->matcher SA ?))
	(B (matcher-union (pattern->matcher SB (list (list (list (list 'foo)))))
			  (pattern->matcher SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-matcher* (matcher-subtract (matcher-union A B) A))
		   B)))

(module+ test
  (let ((M (foldr matcher-union (matcher-empty)
		  (list (pattern->matcher SA (list ? 2))
			(pattern->matcher SC (list 1 3))
			(pattern->matcher SD (list ? 3))
			(pattern->matcher SB (list 3 4)))))
	(S '((("(")
	      ((1      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D" "C")))))))))
	       (3      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D")))))))
			(4 (((")") (((")") ("" ("B")))))))))
	       (("__") ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D"))))))))))))))
    (check-equal? (matcher->jsexpr M (lambda (v) (map symbol->string (set->list v)))) S)
    (check-requal? (jsexpr->matcher S (lambda (v) (list->set (map string->symbol v)))) M)))

(module+ test
  (check-requal? (pretty-print-matcher*
		  (pattern->matcher SA (list 1
					     (embedded-matcher
					      (pattern->matcher SB (list 2 3)))
					     4)))
		 (pattern->matcher SA (list 1 (list 2 3) 4)))

  (check-requal? (pretty-print-matcher*
		  (pattern->matcher SA
				    (list (embedded-matcher (pattern->matcher SB (list 1 2)))
					  (embedded-matcher (pattern->matcher SC (list 3 4))))))
		 (pattern->matcher SA (list (list 1 2) (list 3 4)))))

(module+ test
  (void
   (pretty-print-matcher* (matcher-union (rwild (rsuccess SA))
					 (rseq-multi ? (rsuccess SB)
						     3 (rsuccess SC))))))

(module+ test
  (void
   (pretty-print-matcher* (matcher-union (pattern->matcher SA ?)
					 (pattern->matcher SB (list ? '- ?))))))
