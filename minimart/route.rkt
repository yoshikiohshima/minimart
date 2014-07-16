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

	 matcher? ;; expensive; see implementation
	 matcher-empty
	 matcher-empty?
	 pattern->matcher
	 pattern->matcher*
	 matcher-union
	 matcher-intersect
	 matcher-erase-path
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
	 matcher-erase-path-successes
	 matcher-match-matcher-successes
	 matcher-match-matcher-unit
	 matcher-project-success)

(require racket/set)
(require racket/match)
(require (only-in racket/port call-with-output-string with-output-to-string))
(require (only-in racket/class object?))
(require "canonicalize.rkt")

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

(define matcher-erase-path-successes
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
;; - (HashTable (U Sigma Wildcard) Matcher), {TODO}
;; TODO::: reimplement to use (ordinary-state (Option Matcher) (HashTable Sigma Matcher)), {TODO}
;; - (wildcard-sequence Matcher), {TODO}
;; If, in a hashtable matcher, a wild key is present, it is intended
;; to catch all and ONLY those keys not otherwise present in the
;; table.
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
;; N.B. hash-tables cannot be Sigmas at present.
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
      (and (hash? x)
	   (for/and ([v (in-hash-values x)])
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

;; (U Sigma Wildcard) Matcher -> Matcher
;; Prepends e to r, if r is non-empty.
(define (rseq e r)
  (if (matcher-empty? r) r (canonicalize (hash e r))))

;; [ (U Sigma Wildcard) Matcher ] ... -> Matcher
(define (rseq-multi . ers)
  (canonicalize (apply hash ers)))

;; Matcher -> Matcher
;; Prepends the wildcard pseudo-Sigma to r, if r is non-empty.
(define (rwild r)
  (rseq ? r))

;; Matcher -> Matcher
;; If r is non-empty, returns a matcher that consumes input up to and
;; including EOS, then continuing with r.
(define (rwildseq r)
  (if (matcher-empty? r) r (canonicalize (wildcard-sequence r))))

;; Matcher (U Sigma Wildcard) -> Matcher
;; r must be a hashtable matcher. Retrieves the continuation after
;; accepting key. If key is absent, returns the failing/empty matcher.
(define (rlookup r key)
  (hash-ref r key (lambda () #f)))

;; (Option (HashTable (U Sigma Wildcard) Matcher)) (U Sigma Wildcard) Matcher -> Matcher
;; Updates (installs or removes) a continuation in the Matcher r. r must
;; be either #f or a hashtable matcher.
(define (rupdate r key k)
  (empty-hash-guard
   (if (matcher-empty? k)
       (hash-remove (or r (hash)) key)
       (hash-set (or r (hash)) key k))))

;; Hash -> Matcher
;; If the argument is empty, returns the canonical empty matcher;
;; otherwise, returns the argument.
(define (empty-hash-guard h)
  (and (positive? (hash-count h)) (canonicalize h)))

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
      [(? non-object-struct?)
       (rseq (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      ;; TODO: consider options for treating hash tables as compounds
      ;; rather than (useless) atoms
      [(? hash?) (error 'pattern->matcher "Cannot match on hash tables at present")]
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
(define matcher-union
  (let ()
    (define (walk re1 re2)
      (match* (re1 re2)
	[(#f #f) #f]
	[(#f r) r]
	[(r #f) r]
	[((wildcard-sequence r1) (wildcard-sequence r2)) (rwildseq (walk r1 r2))]
	[((wildcard-sequence r1) r2) (walk (expand-wildseq r1) r2)]
	[(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2))]
	[((success v1) (success v2)) (rsuccess ((matcher-union-successes) v1 v2))]
	[((? hash? h1) (? hash? h2))
	 (define w (walk (rlookup h1 ?) (rlookup h2 ?)))
	 (if w (walk/wildcard w h1 h2) (walk/no-wildcard h1 h2))]))
    (define (walk/wildcard w h1 h2)
      (for/fold [(acc (rwild w))]
	  [(key (set-remove (set-union (hash-keys h1) (hash-keys h2)) ?))]
	(define k (walk (rlookup h1 key) (rlookup h2 key)))
	(rupdate acc
		 key
		 (cond
		  [(key-open? key) (walk (rwildseq w) k)]
		  [(key-close? key) (if (wildcard-sequence? w)
					(walk (wildcard-sequence-matcher w) k)
					k)]
		  [else (walk w k)]))))
    (define (walk/no-wildcard h1 h2)
      (define-values (walk-fn smaller-h larger-h)
	(if (< (hash-count h1) (hash-count h2))
	    (values walk h1 h2)
	    (values (flip walk) h2 h1)))
      (for/fold [(acc larger-h)] [((key k1) (in-hash smaller-h))]
	(rupdate acc key (walk-fn k1 (rlookup larger-h key)))))
    walk))

;; (A B -> C) -> A B -> B A -> C
(define ((flip f) a b) (f b a))

;; Matcher Matcher -> Matcher
;; Computes the intersection of the multimaps passed in.
(define matcher-intersect
  (let ()
    ;; INVARIANT: re1 is a part of the original re1, and likewise for
    ;; re2. This is so that the first arg to combine-success-values
    ;; always comes from re1, and the second from re2.
    (define (walk re1 re2)
      (match* (re1 re2)
	[((wildcard-sequence r1) (wildcard-sequence r2)) (rwildseq (walk r1 r2))]
	[((wildcard-sequence r1) r2) (walk (expand-wildseq r1) r2)]
	[(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2))]
	[((success v1) (success v2)) (rsuccess ((matcher-intersect-successes) v1 v2))]
	[((? hash? h1) (? hash? h2))
	 (define w1 (rlookup h1 ?))
	 (define w2 (rlookup h2 ?))
	 (define w (and w1 w2 (walk w1 w2)))
	 (define (examine-key acc key)
	   (rupdate acc
		    key
		    (match* ((rlookup h1 key) (rlookup h2 key))
		      [(#f #f) #f]
		      [(#f k2) (walk-wild       walk  w1 key k2)]
		      [(k1 #f) (walk-wild (flip walk) w2 key k1)]
		      [(k1 k2) (walk k1 k2)])))
	 ;; If, say, w1 is #f, then we don't need to examine
	 ;; every key in h2. So there are four cases:
	 ;;  - both false -> examine the intersection of the key sets
	 ;;                  (done by enumerating keys in the smaller hash)
	 ;;  - one nonfalse -> examine only the keys in the other
	 ;;  - both nonfalse -> examine the union of the key sets
	 ;; This is important for avoiding examination of the whole
	 ;; structure when wildcards aren't being used.
	 (match* (w1 w2)
	   [(#f #f) (for/fold [(acc #f)] [(key (in-hash-keys (smaller-hash h1 h2)))]
		      (examine-key acc key))]
	   [(#f _) (for/fold [(acc #f)] [(key (in-hash-keys h1))] (examine-key acc key))]
	   [(_ #f) (for/fold [(acc #f)] [(key (in-hash-keys h2))] (examine-key acc key))]
	   [(_ _) (for/fold [(acc (rwild w))] [(key (set-remove (set-union (hash-keys h1)
									   (hash-keys h2))
								?))]
		    (examine-key acc key))])]))
    (define (walk-wild walk-fn w key k)
      (and w (cond
	      [(key-open? key) (walk-fn (rwildseq w) k)]
	      [(key-close? key) (if (wildcard-sequence? w)
				    (walk-fn (wildcard-sequence-matcher w) k)
				    #f)]
	      [else (walk-fn w k)])))
    (lambda (re1 re2)
      (match* (re1 re2)
	[(#f r) #f]
	[(r #f) #f]
	[(r1 r2) (walk r1 r2)]))))

;; Matcher Matcher -> Matcher
;; Removes re2's mappings from re1. Assumes re2 has previously been union'd into re1.
;; The combine-successes function should return #f to signal "no remaining success values".
(define (matcher-erase-path original1 original2)
  (let ()
    (define (cofinite-pattern)
      (error 'matcher-erase-path "Cofinite pattern required subtracting:\n     ~a\nfrom ~a"
	     (matcher->pretty-string original2 #:indent 5)
	     (matcher->pretty-string original1 #:indent 5)))
    (define (erase-path r1 r2)
      (match* (r1 r2)
	[(#f #f) #f]
	[(r #f) r]
	[(#f r) (cofinite-pattern)]
	[(r1 r2) (walk r1 r2)]))
    (define (walk path aggregate)
      (match* (path aggregate)
	[((wildcard-sequence r1) (wildcard-sequence r2)) (rwildseq (walk r1 r2))]
	[((wildcard-sequence r1) r2) (cofinite-pattern)]
	[(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2))]
	[((success v1) (success v2)) (rsuccess ((matcher-erase-path-successes) v1 v2))]
	[((? hash? h1) (? hash? h2))
	 (define w1 (rlookup h1 ?))
	 (define w2 (rlookup h2 ?))
	 (define w (erase-path w1 w2))
	 (define (examine-key acc key)
	   (define updated-k (match* ((rlookup h1 key) (rlookup h2 key))
			       [(#f #f) #f]
			       [(#f k2) (cofinite-pattern)]
			       [(k1 #f) (walk-wild key k1 w2)]
			       [(k1 k2) (walk k1 k2)]))
	   ;; Here we ensure a "minimal" remainder in cases where
	   ;; after an erasure, a particular key's continuation is the
	   ;; same as the wildcard's continuation.
	   (cond
	    [(key-open? key)
	     (if (and (wildcard-sequence? updated-k)
		      (requal? (wildcard-sequence-matcher updated-k) w))
		 (rupdate acc key #f)
		 (rupdate acc key updated-k))]
	    [(key-close? key)
	     ;; We will check whether this can be removed later, in collapse-wildcard-sequences.
	     (rupdate acc key updated-k)]
	    [else
	     (rupdate acc key (if (requal? updated-k w) #f updated-k))]))
	 ;; We only need to examine all keys of h1 if w2 nonfalse.
	 (collapse-wildcard-sequences
	  (if w2
	      (for/fold [(acc (rwild w))] [(key (set-remove (set-union (hash-keys h1)
								       (hash-keys h2))
							    ?))]
		(examine-key acc key))
	      (for/fold [(acc h1)] [(key (in-hash-keys h2))]
		(examine-key acc key))))]))
    (define (walk-wild key k w)
      (if w
	  (cond
	   [(key-open? key) (walk k (rwildseq w))]
	   [(key-close? key) (if (wildcard-sequence? w)
				 (walk k (wildcard-sequence-matcher w))
				 k)]
	   [else (walk k w)])
	  k))
    (erase-path original1 original2)))

;; Matcher -> Matcher
;; Checks for redundant branches in its argument: when a matcher
;; contains only entries for (EOS -> (wildcard-sequence m')) and
;; (★ -> (wildcard-sequence m')), it is equivalent to
;; (wildcard-sequence m') itself. This is in a way the inverse of
;; expand-wildseq.
(define (collapse-wildcard-sequences m)
  (match m
    [(? hash? h)
     (define w (rlookup h ?))
     (if (and (wildcard-sequence? w)
	      (= (hash-count h) 2))
	 (match (set->list (set-remove (hash-keys h) ?))
	   [(list (? key-close? other-key))
	    (define k (rlookup h other-key))
	    (if (requal? k (wildcard-sequence-matcher w))
		w
		h)]
	   [_ h])
	 h)]
    [other other]))

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
;; Exploits the fact that (rwildseq r) === (matcher-union (rwild (rwildseq r)) (rseq EOS r)).
(define (expand-wildseq r)
  (matcher-union (rwild (rwildseq r))
		 (rseq EOS r)))

;; Hashtable Hashtable -> Hashtable
;; Returns the smaller of its arguments.
(define (smaller-hash h1 h2)
  (if (< (hash-count h1) (hash-count h2))
      h1
      h2))

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
  (if (matcher-empty? r)
      failure-result
      (let walk ((vs (list v)) (stack '(())) (r r))
	(define (walk-wild vs stack)
	  (match (rlookup r ?)
	    [#f failure-result]
	    [k (walk vs stack k)]))
	(match r
	  [(wildcard-sequence k)
	   (match stack
	     ['() failure-result]
	     [(cons rest stack1) (walk rest stack1 k)])]
	  [(success result)
	   (if (and (null? vs)
		    (null? stack))
	       result
	       failure-result)]
	  [(? hash?)
	   (match vs
	     ['()
	      (match stack
		['() failure-result]
		[(cons rest stack1)
		 (match (rlookup r EOS)
		   [#f failure-result]
		   [k (walk rest stack1 k)])])]
	     [(cons (== ?) rest)
	      (error 'matcher-match-value "Cannot match wildcard as a value")]
	     [(cons (cons v1 v2) rest)
	      (match (rlookup r SOL)
		[#f (walk-wild rest stack)]
		[k (walk (cons v1 (transform-list-value v2)) (cons rest stack) k)])]
	     [(cons (vector vv ...) rest)
	      (match (rlookup r SOV)
		[#f (walk-wild rest stack)]
		[k (walk vv (cons rest stack) k)])]
	     [(cons (? non-object-struct? s) rest)
	      (match (rlookup r (struct->struct-type s))
		[#f (walk-wild rest stack)]
		[k (walk (cdr (vector->list (struct->vector s))) (cons rest stack) k)])]
	     [(cons v rest)
	      (match (rlookup r v)
		[#f (walk-wild rest stack)]
		[k (walk rest stack k)])])]))))

;; Matcher Matcher -> Value
;;
;; Similar to matcher-match-value, but instead of a single key,
;; accepts a Matcher serving as *multiple* simultaneously-examined
;; keys. Returns the union of all successful values reached by the
;; probe.
(define matcher-match-matcher
  (let ()
    (define (walk re1 re2 acc)
      (match* (re1 re2)
	[((wildcard-sequence r1) (wildcard-sequence r2)) (walk r1 r2 acc)]
	[((wildcard-sequence r1) r2) (walk (expand-wildseq r1) r2 acc)]
	[(r1 (wildcard-sequence r2)) (walk r1 (expand-wildseq r2) acc)]
	[((success v1) (success v2)) ((matcher-match-matcher-successes) v1 v2 acc)]
	[((? hash? h1) (? hash? h2))
	 (define w1 (rlookup h1 ?))
	 (define w2 (rlookup h2 ?))
	 (define r (if (and w1 w2)
		       (walk w1 w2 acc)
		       acc))
	 (define (examine-key r key)
	   (match* ((rlookup h1 key) (rlookup h2 key))
	     [(#f #f) r]
	     [(#f k2) (walk-wild walk w1 key k2 r)]
	     [(k1 #f) (walk-wild (lambda (re2 re1 acc) (walk re1 re2 acc)) w2 key k1 r)]
	     [(k1 k2) (walk k1 k2 r)]))
	 ;; We optimize as described in matcher-intersect.
	 (match* (w1 w2)
	   [(#f #f) (for/fold [(r r)] [(key (in-hash-keys (smaller-hash h1 h2)))] (examine-key r key))]
	   [(#f _) (for/fold [(r r)] [(key (in-hash-keys h1))] (examine-key r key))]
	   [(_ #f) (for/fold [(r r)] [(key (in-hash-keys h2))] (examine-key r key))]
	   [(_ _) (for/fold [(r r)] [(key (set-remove (set-union (hash-keys h1) (hash-keys h2)) ?))]
		    (examine-key r key))])]))
    (define (walk-wild walker w key k acc)
      (if w
	  (cond
	   [(key-open? key) (walker (rwildseq w) k acc)]
	   [(key-close? key) (if (wildcard-sequence? w)
				 (walker (wildcard-sequence-matcher w) k acc)
				 acc)]
	   [else (walker w k acc)])
	  acc))
    (lambda (re1 re2)
      (match* (re1 re2)
	[(#f r) (matcher-match-matcher-unit)]
	[(r #f) (matcher-match-matcher-unit)]
	[(r1 r2) (walk r1 r2 (matcher-match-matcher-unit))]))))

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
      [(? hash?) (for/fold [(acc #f)] [((k v) (in-hash m))]
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
      [(? hash?) (for/fold [(acc #f)] [((k v) (in-hash m))] (rupdate acc k (walk v)))])))

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
      [(? non-object-struct?)
       (cons (struct->struct-type p)
	     (walk-pair-chain (cdr (vector->list (struct->vector p)))
			      acc))]
      ;; TODO: consider options for treating hash tables as compounds rather than (useless) atoms
      [(? hash?) (error 'compile-projection "Cannot match on hash tables at present")]
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
      [(? non-object-struct?)
       (apply (struct-type-make-constructor (struct->struct-type p))
	      (map walk (cdr (vector->list (struct->vector p)))))]
      ;; TODO: consider options for treating hash tables as compounds
      ;; rather than (useless) atoms
      [(? hash?) (error 'projection->pattern "Cannot match on hash tables at present")]
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
	     [(? hash?)
	      (for/fold [(acc (add-wild (walk (rlookup m ?) k)))] [((key mk) (in-hash m))]
		(if (eq? key ?)
		    acc
		    (add-edge acc key (cond
				       [(key-open? key) (balanced mk (lambda (mk) (walk mk k)))]
				       [(key-close? key) #f]
				       [else (walk mk k)]))))]
	     [_ (matcher-empty)])]

	  [(cons sigma k)
	   (add-sigma sigma
		      (match m
			[(wildcard-sequence mk)
			 (cond
			  [(key-open? sigma) (walk (rwildseq m) k)]
			  [(key-close? sigma) (walk mk k)]
			  [else (walk m k)])]
			[(? hash?)
			 (matcher-union (walk (rlookup m sigma) k)
					(cond
					 [(key-open? sigma) (walk (rwildseq (rlookup m ?)) k)]
					 [(key-close? sigma) #f]
					 [else (walk (rlookup m ?) k)]))]
			[_ (matcher-empty)]))])))

    (define (general-balanced add-wildseq add-wild add-edge m k)
      (let walk ((m m) (k k))
	(match m
	  [(wildcard-sequence mk) (add-wildseq (k mk))]
	  [(? hash?)
	   (for/fold [(acc (add-wild (walk (rlookup m ?) k)))] [((key mk) (in-hash m))]
	     (if (eq? key ?)
		 acc
		 (add-edge acc key (cond
				    [(key-open? key) (walk mk (lambda (mk) (walk mk k)))]
				    [(key-close? key) (k mk)]
				    [else (walk mk k)]))))]
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
	[(? hash?)
	 (and (not (hash-has-key? m ?))
	      (for/fold [(acc (set))] [((key mk) (in-hash m))]
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
	[(? hash?)
	 (and (not (hash-has-key? m ?))
	      (for/fold [(acc (set))] [((key mk) (in-hash m))]
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
       (d "...>")
       (walk (+ i 4) k)]
      [(success vs)
       (d "{")
       (d vs)
       (d "}")]
      [(? hash? h)
       (if (zero? (hash-count h))
	   (d " ::: empty hash!")
	   (for/fold [(need-sep? #f)] [((key k) (in-hash h))]
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
      [(? hash?) (for/list [((k v) (in-hash m))]
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
       (canonicalize
	(for/hash [(kj kjs) (vj vjs)]
	  (values (match kj
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
		  (walk vj))))])))

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

  (void (pretty-print-matcher* (matcher-intersect (pattern->matcher SA (list 'a))
						  (pattern->matcher SB (list 'b)))))

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

  (void (pretty-print-matcher* (matcher-intersect (bigdemo) (pattern->matcher SX (list 'm 'n)))))

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
   (list 'Z (list '-)) "XZ"
   (list 'Z (list '- '-)) "XZ"
   (list 'Z (list '- '- '-)) "XZ+"
   (list 'Z (list '- '- '- '-)) "XZ"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "XZ"
   (list 'Z '((()) - -)) "XZ+"
   (list '? (list '- '- '-)) "")

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
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

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
   (list 'Z (list '-)) "X"
   (list 'Z (list '- '-)) "X"
   (list 'Z (list '- '- '-)) "X"
   (list 'Z (list '- '- '- '-)) "X"
   (list 'Z (list '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '- '-)) "X"
   (list 'Z '((()) - -)) "X"
   (list '? (list '- '- '-)) "")

  (let* ((r1 (pattern->matcher SA (list  ? 'b)))
	 (r2 (pattern->matcher SB (list 'a  ?)))
	 (r12 (matcher-union r1 r2)))
    (printf "\n-=-=-=-=-=-=-=-=- erase1\n")
    (pretty-print-matcher* r1)
    (pretty-print-matcher* r2)
    (pretty-print-matcher* r12)
    (pretty-print-matcher* (matcher-erase-path r12 r1))
    (pretty-print-matcher* (matcher-erase-path r12 r2))
    (void))

  (let* ((r1 (matcher-union (pattern->matcher SA (list 'a ?))
			    (pattern->matcher SA (list 'b ?))))
	 (r2 (pattern->matcher SB (list 'b ?)))
	 (r12 (matcher-union r1 r2)))
    (printf "\n-=-=-=-=-=-=-=-=- erase2\n")
    (pretty-print-matcher* r12)
    (pretty-print-matcher* (matcher-erase-path r12 r1))
    (pretty-print-matcher* (matcher-erase-path r12 r2))
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

  (define (check-requal? actual expected)
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

  (check-exn #px"Cannot match on hash tables at present"
	     (lambda ()
	       (intersect (canonicalize (hash 'a 1 'b ?))
			  (canonicalize (hash 'a ? 'b 2)))))
  ;; (check-requal? (intersect (rseq 'a 1 'b ?) (rseq 'a ? 'b 2)) (rseq 'a 1 'b 2))
  ;; (check-requal? (intersect (rseq 'a 1 'b ?) (rseq 'a ?)) (void))
  ;; (check-requal? (intersect (rseq 'a 1 'b ?) (rseq 'a 1 'b ?)) (rseq 'a 1 'b ?))
  ;; (check-requal? (intersect (rseq 'a 1 'b ?) (rseq 'a ? 'c ?)) (void))

  ;; (check-requal? (intersect (rseq 'a 1 'b ?) (rseq 'a 1 'b (list 2 ?)))
  ;; 		(rseq 'a 1 'b (list 2 ?)))
  ;; (check-requal? (intersect (rseq 'a 1 'b (list ? 3)) (rseq 'a 1 'b (list 2 ?)))
  ;; 		(rseq 'a 1 'b (list 2 3)))

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
  (let ((A (pattern->matcher SA ?))
	(B (pattern->matcher SB (list (list (list (list 'foo)))))))
    (check-requal? (pretty-print-matcher* (matcher-erase-path (matcher-union A B) B))
		   A))
  (let ((A (pattern->matcher SA ?))
	(B (matcher-union (pattern->matcher SB (list (list (list (list 'foo)))))
			  (pattern->matcher SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-matcher* (matcher-erase-path (matcher-union A B) B))
		   A))
  (let ((A (pattern->matcher SA ?))
	(B (matcher-union (pattern->matcher SB (list (list (list (list 'foo)))))
			  (pattern->matcher SB (list (list (list (list 'bar))))))))
    (check-requal? (pretty-print-matcher* (matcher-erase-path (matcher-union A B) A))
		   B)))

(module+ test
  (let ((M (foldr matcher-union (matcher-empty)
		  (list (pattern->matcher SA (list ? 2))
			(pattern->matcher SC (list 1 3))
			(pattern->matcher SD (list ? 3))
			(pattern->matcher SB (list 3 4)))))
	(S '((("(")
	      ((("__") ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D")))))))))
	       (1      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D" "C")))))))))
	       (3      ((2 (((")") (((")") ("" ("A")))))))
			(3 (((")") (((")") ("" ("D")))))))
			(4 (((")") (((")") ("" ("B"))))))))))))))
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
