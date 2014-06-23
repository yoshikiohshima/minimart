#lang racket/base

(provide actor
	 observe-subscribers
	 observe-advertisers
	 advertise
	 subscribe
	 for/advertise
	 for/subscribe
	 define-transition
	 begin-transition)

;; (require (for-syntax racket/pretty))
;; (require (for-syntax racket/trace))

(require racket/set)
(require racket/match)

(require (for-syntax racket/match))
(require (for-syntax racket/list))
(require (for-syntax racket/base))
(require (for-syntax syntax/stx))

(require (for-syntax racket/stxparam))
(require racket/stxparam)

;; (require (for-syntax syntax/parse))

(require "core.rkt")
(require "gestalt.rkt")

(define-syntax (actor stx)
  (syntax-case stx ()
    [(_ forms ...)
     (analyze-actor #f '() '() '() '() '() '() #'(forms ...))]))

(define-syntax (observe-subscribers stx) (raise-syntax-error #f "Use of observe-subscribers outside actor form" stx))
(define-syntax (observe-advertisers stx) (raise-syntax-error #f "Use of observe-advertisers outside actor form" stx))
(define-syntax (advertise stx) (raise-syntax-error #f "Use of advertise outside actor form" stx))
(define-syntax (subscribe stx) (raise-syntax-error #f "Use of subscribe outside actor form" stx))
(define-syntax (for/advertise stx) (raise-syntax-error #f "Use of for/advertise outside actor form" stx))
(define-syntax (for/subscribe stx) (raise-syntax-error #f "Use of for/subscribe outside actor form" stx))

(define-syntax-parameter update-state-struct #f)
(define-syntax-parameter match-state #f)
(define-syntax-parameter compute-gestalt #f)

(define-syntax-rule (define-transition head tail ...)
  (define head (begin-transition tail ...)))

(define-syntax (begin-transition stx)
  (syntax-case stx ()
    [(_ forms ...)
     (let ()
       (define result (accumulate-actions (gensym 'begin-transition) '() '() #'(forms ...)))
       ;; (pretty-print `(result ,(syntax->datum result)))
       result)]))

(begin-for-syntax
 (struct observer (condition level meta-level presence-name set-name set-exp added-name removed-name) #:transparent)
 (struct participator (condition meta-level) #:transparent)

 (define (defbinding name-stx init-name-stx init-exp)
   (list #`(define #,init-name-stx #,init-exp)
	 #`(define-syntax-parameter #,name-stx (syntax-id-rules () [_ #,init-name-stx]))))

 (define (analyze-actor actor-name
			states
			gestalt-observers
			gestalt-computers
			message-handlers
			action-ids
			body-forms
			forms-stx)

   (define-syntax-rule (analyze-body* self body-stx struct-type o [keyword accessor fieldname] ...)
     (syntax-case body-stx ()
       [(keyword v rest (... ...))
	(if (accessor o)
	    (raise-syntax-error #f (format "duplicate ~a clause" 'keyword) body-stx)
	    (self #'(rest (... ...)) (struct-copy struct-type o [fieldname #'v])))]
       ...
       [other (values o #'other)]))

   (define (analyze-observer-body body-stx o)
     (analyze-body* analyze-observer-body body-stx observer o
		    [#:when observer-condition condition]
		    [#:level observer-level level]
		    [#:meta-level observer-meta-level meta-level]
		    [#:presence observer-presence-name presence-name]
		    [#:name observer-set-name set-name]
		    [#:set observer-set-exp set-exp]
		    [#:added observer-added-name added-name]
		    [#:removed observer-removed-name removed-name]))

   (define (analyze-participator-body body-stx p)
     (analyze-body* analyze-participator-body body-stx participator p
		    [#:when participator-condition condition]
		    [#:meta-level participator-meta-level meta-level]))

   (define (analyze-pattern pat-stx)
     (syntax-case pat-stx ($ quasiquote unquote quote)
       ;; Extremely limited support for quasiquoting and quoting
       [(quasiquote (unquote p)) (analyze-pattern #'p)]
       [(quasiquote (p ...)) (analyze-pattern #'(list (quasiquote p) ...))]
       [(quasiquote p) (values #''p #''p #''p '())]
       [(quote p) (values #''p #''p #''p '())]

       [($ v)
	(values #'(?!)
		#'?
		#'v
		(list #'v))]
       [($ v p)
	(let ()
	  (define-values (pr g m bs) (analyze-pattern #'p))
	  (when (not (null? bs)) (raise-syntax-error #f "nested bindings not supported" pat-stx))
	  (values #`(?! #,pr)
		  g
		  #`(and v #,m)
		  (list #'v)))]
       [(ctor p ...)
	(let ()
	  (define parts (if (identifier? #'ctor) #'(p ...) #'(ctor p ...)))
	  (define-values (pr g m bs)
	    (for/fold [(pr '()) (g '()) (m '()) (bs '())] [(p (syntax->list parts))]
	      (define-values (pr1 g1 m1 bs1) (analyze-pattern p))
	      (values (cons pr1 pr)
		      (cons g1 g)
		      (cons m1 m)
		      (append bs1 bs))))
	  (if (identifier? #'ctor)
	      (values (cons #'ctor (reverse pr))
		      (cons #'ctor (reverse g))
		      (cons #'ctor (reverse m))
		      bs)
	      (values (reverse pr)
		      (reverse g)
		      (reverse m)
		      bs)))]
       [non-pair
	(if (and (identifier? #'non-pair)
		 (free-identifier=? #'non-pair #'?))
	    (values #'?
		    #'?
		    #'_
		    '())
	    (values #'non-pair
		    #'non-pair
		    #'(== non-pair)
		    '()))]))
   ;; (trace analyze-pattern)

   (define (analyze-observation pat-stx body-stx pub? forms-stx)
     (define-values (o remaining-stx) (analyze-observer-body body-stx (observer #f #f #f #f #f #f #f #f)))
     (match-define (observer condition level meta-level presence-name set-name set-exp added-name removed-name) o)
     (when (and (not set-name) (or set-exp added-name removed-name))
       (define stx (or set-exp added-name removed-name))
       (raise-syntax-error #f "#:name is required when using #:set, #:added and/or #:removed" stx))
     (match-define (list presence-init set-init projector-name gestalt-name set-temp)
       (generate-temporaries (list presence-name set-name pat-stx pat-stx set-name)))
     (define-values (projector-stx gestalt-stx matcher-stx binders) (analyze-pattern pat-stx))
     (analyze-actor actor-name
		    (append (if presence-name (list (list presence-name presence-init)) '())
			    (if set-name (list (list set-name set-init)) '())
			    states)
		    (append (if presence-name
				(list (lambda (g-stx)
					#`(#:update [#,presence-name
						     (not (gestalt-empty? (gestalt-filter #,g-stx #,gestalt-name)))])))
				'())
			    (if (or set-name added-name removed-name)
				(list (lambda (g-stx)
					#`((define #,set-temp
					     #,(if set-exp
						   #`(for/set [(e (in-set
								   (gestalt-project/keys #,g-stx #,projector-name)))]
						       (match-define (list #,@binders) e)
						       #,set-exp)
						   #`(gestalt-project/keys #,g-stx #,projector-name)))
					   #,@(if added-name
						  #`((define #,added-name (set-subtract #,set-temp #,set-name)))
						  #'())
					   #,@(if removed-name
						  #`((define #,removed-name (set-subtract #,set-name #,set-temp)))
						  #'())
					   #,@(if set-name
						  #`(#:update [#,set-name #,set-temp])
						  #'()))))
				'())
			    (list (lambda (g-stx) remaining-stx))
			    gestalt-observers)
		    (append (if condition
				(list #`(if #,condition #,gestalt-name (gestalt-empty)))
				(list gestalt-name))
			    gestalt-computers)
		    message-handlers
		    action-ids
		    (cons #`(begin
			      #,@(if presence-name (defbinding presence-name presence-init #'#f) #'())
			      #,@(if set-name (defbinding set-name set-init #'(set)) #'())
			      (define #,projector-name (#,(if pub? #'project-subs #'project-pubs) #,projector-stx
							#:level #,(or level 0) #:meta-level #,(or meta-level 0)))
			      (define #,gestalt-name (projection->gestalt #,projector-name)))
			  body-forms)
		    forms-stx))

   (define (analyze-participation pat-stx body-stx pub? forms-stx)
     (define-values (p remaining-stx) (analyze-participator-body body-stx (participator #f #f)))
     (match-define (participator condition meta-level) p)
     (match-define (list gestalt-name) (generate-temporaries (list pat-stx)))
     (define-values (projector-stx gestalt-stx matcher-stx binders) (analyze-pattern pat-stx))
     (analyze-actor actor-name
		    states
		    gestalt-observers
		    (append (if condition
				(list #`(if #,condition #,gestalt-name (gestalt-empty)))
				(list gestalt-name))
			    gestalt-computers)
		    (append (list (lambda (e-stx)
				    #`[(message #,matcher-stx (== #,(or meta-level 0)) #,pub?)
				       (begin-transition #,@remaining-stx)]))
			    message-handlers)
		    action-ids
		    (cons #`(define #,gestalt-name (#,(if pub? #'pub #'sub) #,gestalt-stx
						    #:meta-level #,(or meta-level 0)))
			  body-forms)
		    forms-stx))

   (define (analyze-group-participation loopspecs-stx pat-stx body-stx pub? forms-stx)
     (define-values (p remaining-stx) (analyze-participator-body body-stx (participator #f #f)))
     (match-define (participator condition meta-level) p)
     (match-define (list projector-name gestalt-name) (generate-temporaries (list pat-stx pat-stx)))
     (define-values (projector-stx gestalt-stx matcher-stx binders) (analyze-pattern pat-stx))
     (unless (stx-null? remaining-stx)
       (raise-syntax-error #f "for/advertise and for/subscribe cannot currently install message handlers"))
     (analyze-actor actor-name
		    states
		    gestalt-observers
		    (append (list #`(gestalt-union* (for/list #,loopspecs-stx
						      #,@(if condition
							     #`(#:when #,condition)
							     #'())
						      (#,(if pub? #'pub #'sub) #,gestalt-stx
						       #:meta-level #,meta-level))))
			    gestalt-computers)
		    message-handlers
		    action-ids
		    body-forms
		    forms-stx))

   (define (accumulate-action action-stx forms-stx)
     (define temp (car (generate-temporaries (list action-stx))))
     (analyze-actor actor-name
		    states
		    gestalt-observers
		    gestalt-computers
		    message-handlers
		    (cons temp action-ids)
		    (cons #`(define #,temp #,action-stx) body-forms)
		    forms-stx))

   (syntax-case forms-stx (observe-subscribers
			   observe-advertisers
			   advertise
			   subscribe
			   for/advertise
			   for/subscribe)
     [()
      (let ((actor-name (or actor-name #'anonymous-actor)))
	(define state-struct-name
	  (datum->syntax actor-name (string->symbol (format "~a-state" (syntax->datum actor-name)))))
	(match-define (list e-stx state-stx g-stx compute-gestalt-stx)
	  (generate-temporaries (list #'event #'state #'gestalt #'compute-gestalt)))
	(define statevars (map car states))
	(define stateinits (map cadr states))
	(define result
	  #`(let ()
	      (struct #,state-struct-name (#,@statevars) #:prefab)
	      #,@(reverse body-forms)
	      (syntax-parameterize
	       ((update-state-struct (syntax-rules () [(_ v [n e] (... ...))
						       (struct-copy #,state-struct-name v [n e] (... ...))]))
		(match-state (syntax-rules () [(_ id body (... ...))
					       (match-lambda
						[(and id (struct #,state-struct-name (#,@stateinits)))
						 (syntax-parameterize (#,@(for/list ([sv statevars] [si stateinits])
									    #`(#,sv (syntax-id-rules () [_ #,si]))))
								      body (... ...))])])))
	       (let ((#,compute-gestalt-stx (match-state #,state-stx
					      (gestalt-union #,@gestalt-computers)))
		     (#,state-stx (#,state-struct-name #,@statevars)))
		 (syntax-parameterize
		  ((compute-gestalt (syntax-rules () [(_ state) (#,compute-gestalt-stx state)])))
		  (spawn #:boot (begin-transition #,@(reverse action-ids))
			 (procedure-rename
			  (lambda (#,e-stx #,state-stx)
			    ((match-state #,state-stx
					  ((match #,e-stx
					     [(routing-update #,g-stx)
					      (begin-transition
						#,@(append-map
						    (lambda (p) (syntax->list (p g-stx)))
						    gestalt-observers))]
					     #,@(map (lambda (p) (p e-stx)) message-handlers)
					     [_ (lambda (state) #f)])
					   #,state-stx))
			     #,state-stx))
			  '#,actor-name)
			 #,state-stx
			 (#,compute-gestalt-stx #,state-stx)))))))
	;; (pretty-print `(result ,(syntax->datum result)))
	result)]

     [(#:name name rest ...) ;; TODO: named processes
      (let ()
	(when actor-name (raise-syntax-error #f "duplicate actor #:name" forms-stx))
	(unless (identifier? #'name) (raise-syntax-error #f "actor #:name must be an identifier" #'name))
	(analyze-actor #'name states gestalt-observers gestalt-computers message-handlers action-ids body-forms #'(rest ...)))]

     [(#:arguments [arg ...] rest ...) ;; TODO arguments
      (analyze-actor actor-name states gestalt-observers gestalt-computers message-handlers action-ids body-forms  #'(rest ...))]

     [(#:state [statevar stateexp] rest ...)
      (let ()
	(match-define (list stateinit) (generate-temporaries (list #'statevar)))
	(analyze-actor actor-name
		       (cons (list #'statevar stateinit) states)
		       gestalt-observers
		       gestalt-computers
		       message-handlers
		       action-ids
		       (append (defbinding #'statevar stateinit #'stateexp) body-forms)
		       #'(rest ...)))]

     [((observe-subscribers pat body ...) rest ...)
      (analyze-observation #'pat #'(body ...) #t #'(rest ...))]

     [((observe-advertisers pat body ...) rest ...)
      (analyze-observation #'pat #'(body ...) #f #'(rest ...))]

     [((advertise pat body ...) rest ...)
      (analyze-participation #'pat #'(body ...) #t #'(rest ...))]

     [((subscribe pat body ...) rest ...)
      (analyze-participation #'pat #'(body ...) #f #'(rest ...))]

     [((for/advertise [loopspec ...] pat body ...) rest ...)
      (analyze-group-participation #'(loopspec ...) #'pat #'(body ...) #t #'(rest ...))]

     [((for/subscribe [loopspec ...] pat body ...) rest ...)
      (analyze-group-participation #'(loopspec ...) #'pat #'(body ...) #f #'(rest ...))]

     [(expr rest ...)
      (syntax-case (expand-in-context (gensym 'actor-initialization) #'expr) ()
	[(head inner-rest ...)
	 (if (or (free-identifier=? #'head #'begin)
		 (free-identifier=? #'head #'begin-transition))
	     (analyze-actor actor-name
			    states
			    gestalt-observers
			    gestalt-computers
			    message-handlers
			    action-ids
			    body-forms
			    #'(inner-rest ... rest ...))
	     (if (ormap (lambda (i) (free-identifier=? #'head i))
			(syntax->list #'(define-values define-syntaxes begin-for-syntax
					  module module*
					  #%module-begin 
					  #%require #%provide)))
		 (analyze-actor actor-name
				states
				gestalt-observers
				gestalt-computers
				message-handlers
				action-ids
				(cons #'expr body-forms)
				#'(rest ...))
		 (accumulate-action #'expr #'(rest ...))))]
	[non-pair-syntax (accumulate-action #'expr #'(rest ...))])]))

 (define (expand-in-context context-id stx)
   (local-expand stx
		 (list context-id)
		 (syntax->list #'(quote quote-syntax lambda case-lambda let-values letrec-values
					begin begin0 set! with-continuation-mark if #%app #%expression
					define-values define-syntaxes begin-for-syntax #%require #%provide
					#%variable-reference))))

 (define (accumulate-actions context-id action-ids final-forms forms)
   (syntax-case forms ()
     [()
      #`(match-state state 
	  #,@(reverse final-forms)
	  (transition state (list #,@(reverse action-ids))))]

     [(#:run-transition exp rest ...)
      #`(match-state _state
	  #,@(reverse final-forms)
	  (sequence-transitions (transition state (list #,@(reverse action-ids)))
				exp
				(begin-transition rest ...)))]

     [(#:update [statevar stateval] rest ...)
      #`(match-state state
	  #,@(reverse final-forms)
	  (sequence-transitions (transition (update-state-struct state [statevar stateval])
					    (list #,@(reverse action-ids)))
				(begin-transition rest ...)))]

     [(#:update-routes rest ...)
      #`(match-state state
	  #,@(reverse final-forms)
	  (sequence-transitions (transition state (list #,@(reverse action-ids)
							(routing-update (compute-gestalt state))))
				(begin-transition rest ...)))]

     [(expr rest ...)
      (syntax-case (expand-in-context context-id #'expr) ()
	[(head inner-rest ...)
	 (if (or (free-identifier=? #'head #'begin)
		 (free-identifier=? #'head #'begin-transition))
	     (accumulate-actions context-id
				 action-ids
				 final-forms
				 (append (syntax->list #'(inner-rest ...)) #'(rest ...)))
	     (if (ormap (lambda (i) (free-identifier=? #'head i))
			(syntax->list #'(define-values define-syntaxes begin-for-syntax
					  module module*
					  #%module-begin 
					  #%require #%provide)))
		 (accumulate-actions context-id
				     action-ids
				     (cons #'expr final-forms)
				     #'(rest ...))
		 (accumulate-action #'expr
				    context-id
				    action-ids
				    final-forms
				    #'(rest ...))))]
	[non-pair-syntax
	 (accumulate-action #'expr
			    context-id
			    action-ids
			    final-forms
			    #'(rest ...))])]))

 (define (accumulate-action action-stx context-id action-ids final-forms remaining-forms)
   (define temp (car (generate-temporaries (list action-stx))))
   (accumulate-actions context-id
		       (cons temp action-ids)
		       (cons #`(define #,temp #,action-stx) final-forms)
		       remaining-forms)))

;;; Local Variables:
;;; eval: (put 'begin-transition 'scheme-indent-function 0)
;;; eval: (put 'observe-subscribers 'scheme-indent-function 1)
;;; eval: (put 'observe-advertisers 'scheme-indent-function 1)
;;; eval: (put 'subscribe 'scheme-indent-function 1)
;;; eval: (put 'advertise 'scheme-indent-function 1)
;;; End:
