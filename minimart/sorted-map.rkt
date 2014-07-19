#lang racket/base
;; Matt Might's red-black tree code from
;; http://matt.might.net/articles/red-black-delete/code/sorted-map.rkt
;;
;; Modified by Tony Garnock-Jones, July 2014:
;;  - trees are hashconsed
;;  - sorted-map-size is made constant-time

(provide (struct-out sorted-map)
	 sorted-map-empty
	 sorted-map-modify-at
	 sorted-map-insert
	 sorted-map-insert*
	 sorted-map-to-tree
	 sorted-map-to-alist
	 sorted-map-submap?
	 sorted-map-get
	 sorted-map-size
	 sorted-map-max
	 sorted-map-delete
	 sorted-map-has-key?
	 sorted-map-keys
	 sorted-map-values
	 )

(require "canonicalize.rkt")
(require "memoize.rkt")
(require racket/set)

; A purely functional sorted-map library.

; Provides logarithmic insert, update, get & delete.

; Based on Okasaki's red-black trees
; with purely functional red-black delete.

; Author: Matthew Might
; Site:   http://matt.might.net/
; Page:   http://matt.might.net/articles/red-black-delete/

(require (except-in racket/match define/match))
  
; Syntactic sugar for define forms
; with match as their body:
(define-syntax define/match 
  (syntax-rules ()
    [(_ (id name) clause ...)
     ; =>
     (define (id name)
       (match name clause ...))]))
  
(define-syntax define/match*
  (syntax-rules ()
    [(_ (id name ...) clause ...)
     ; =>
     (define (id name ...)
       (match* (name ...)
               clause ...))]))

; A form for matching the result of a comparison:
;; tonyg 20140718: changed to use the order? convention from data/order
(define-syntax switch-compare 
  (syntax-rules (= < >)
    [(_ (cmp v1 v2)
        [< action1 ...] 
        [= action2 ...]
        [> action3 ...])
     ; =>
     (let ((dir (cmp v1 v2)))
       (case dir
         [(<) action1 ...]
         [(=) action2 ...]
         [(>) action3 ...]))]))
    
;; tonyg 20140718: for hash-consing, we have to be able to compare
;; trees using equal?, which necessitates use of #:transparent in our
;; struct definitions.

; Struct definition for sorted-map:
(define-struct sorted-map (compare) #:transparent)

;  Internal nodes:
(define-struct (T sorted-map)
  (color left key value right) #:transparent)

;  Leaf nodes:
(define-struct (L sorted-map) () #:transparent)

;  Double-black leaf nodes:
(define-struct (BBL sorted-map) () #:transparent)

  
; Color manipulators.

; Turns a node black.
(define/match (blacken node)
  [(T cmp _ l k v r)  (canonicalize (T cmp 'B l k v r))]
  [(BBL cmp)          (canonicalize (L cmp))]
  [(L _)              node])

; Turns a node red.
(define/match (redden node)
  [(T cmp _ l k v r)   (canonicalize (T cmp 'R l k v r))]
  [(L _)               (error "Can't redden leaf.")])


; Color arithmetic.
(define/match (black+1 color-or-node)
  [(T cmp c l k v r)  (canonicalize (T cmp (black+1 c) l k v r))]
  [(L cmp)            (canonicalize (BBL cmp))]
  ['-B 'R]
  ['R  'B]
  ['B  'BB])

(define/match (black-1 color-or-node)
  [(T cmp c l k v r)  (canonicalize (T cmp (black-1 c) l k v r))]
  [(BBL cmp)          (canonicalize (L cmp))]
  ['R   '-B]
  ['B    'R]
  ['BB   'B])



; Creates an empty map:
(define (sorted-map-empty compare)
  (canonicalize (L compare)))


;; Custom patterns.
   
; Matches internal nodes:
(define-match-expander T!
  (syntax-rules ()
    [(_)            (T _ _ _ _ _ _)]
    [(_ l r)        (T _ _ l _ _ r)]
    [(_ c l r)      (T _ c l _ _ r)]
    [(_ l k v r)    (T _ _ l k v r)]
    [(_ c l k v r)  (T _ c l k v r)]))

; Matches leaf nodes: 
(define-match-expander L!
  (syntax-rules ()
    [(_)     (L _)]))

; Matches black nodes (leaf or internal):
(define-match-expander B
  (syntax-rules ()
    [(_)              (or (T _ 'B _ _ _ _)
                          (L _))]
    [(_ cmp)          (or (T cmp 'B _ _ _ _)
                          (L cmp))]
    [(_ l r)          (T _ 'B l _ _ r)]
    [(_ l k v r)      (T _ 'B l k v r)]
    [(_ cmp l k v r)  (T cmp 'B l k v r)]))

; Matches red nodes:
(define-match-expander R
  (syntax-rules ()
    [(_)              (T _ 'R _ _ _ _)]
    [(_ cmp)          (T cmp 'R _ _ _ _)]
    [(_ l r)          (T _ 'R l _ _ r)]
    [(_ l k v r)      (T _ 'R l k v r)]
    [(_ cmp l k v r)  (T cmp 'R l k v r)]))

; Matches negative black nodes:
(define-match-expander -B
  (syntax-rules ()
    [(_)                (T _ '-B _ _ _ _)]
    [(_ cmp)            (T cmp '-B _ _ _ _)]
    [(_ l k v r)        (T _ '-B l k v r)]
    [(_ cmp l k v r)    (T cmp '-B l k v r)]))

; Matches double-black nodes (leaf or internal):
(define-match-expander BB
  (syntax-rules ()
    [(_)              (or (T _ 'BB _ _ _ _)
                          (BBL _))]
    [(_ cmp)          (or (T cmp 'BB _ _ _ _)
                          (BBL _))]
    [(_ l k v r)      (T _ 'BB l k v r)]
    [(_ cmp l k v r)  (T cmp 'BB l k v r)]))

(define/match (double-black? node)
  [(BB) #t]
  [_    #f])


; Turns a black-balanced tree with invalid colors
; into a black-balanced tree with valid colors:
(define (balance-node node)
  (define cmp (sorted-map-compare node))
  (match node
    [(or (T! (or 'B 'BB) (R (R a xk xv b) yk yv c) zk zv d)
         (T! (or 'B 'BB) (R a xk xv (R b yk yv c)) zk zv d)
         (T! (or 'B 'BB) a xk xv (R (R b yk yv c) zk zv d))
         (T! (or 'B 'BB) a xk xv (R b yk yv (R c zk zv d))))
     ; =>
     (canonicalize (T cmp
		      (black-1 (T-color node))
		      (canonicalize (T cmp 'B a xk xv b))
		      yk
		      yv
		      (canonicalize (T cmp 'B c zk zv d))))]
    
    [(BB a xk xv (-B (B b yk yv c) zk zv (and d (B))))
     ; =>
     (canonicalize (T cmp
		      'B
		      (canonicalize (T cmp 'B a xk xv b))
		      yk
		      yv
		      (balance cmp 'B c zk zv (redden d))))]
    
    [(BB (-B (and a (B)) xk xv (B b yk yv c)) zk zv d)
     ; =>
     (canonicalize (T cmp
		      'B
		      (balance cmp 'B (redden a) xk xv b)
		      yk
		      yv
		      (canonicalize (T cmp 'B c zk zv d))))]
    
    [else     node]))
  
(define (balance cmp c l k v r)
  (balance-node (canonicalize (T cmp c l k v r))))


; Moves to a location in the map and
; peformes an update with the function:
;; tonyg 20140718 added on-missing argument
(define (sorted-map-modify-at node key f [on-missing (lambda () #f)])
  
  (define (internal-modify-at node key f)
    (match node 
      [(T cmp c l k v r)
       ; =>
       (switch-compare (cmp key k)
        [<  (balance cmp c (internal-modify-at l key f) k v r)]
        [=  (canonicalize (T cmp c l k (f k v) r))]
        [>  (balance cmp c l k v (internal-modify-at r key f))])]
      
      [(L cmp)
       ; =>
       (canonicalize (T cmp 'R node key (f key (on-missing)) node))]))

  (blacken (internal-modify-at node key f)))

  
; Inserts an element into the map
(define (sorted-map-insert node key value)
  (sorted-map-modify-at node key (lambda (k v) value)))

  
; Inserts several elements into the map:
(define (sorted-map-insert* node keys values)
  (if (or (not (pair? keys))
          (not (pair? values)))
      node
      (sorted-map-insert* 
       (sorted-map-insert node (car keys) (car values)) 
       (cdr keys) (cdr values))))


; Coverts a sorted map into a tree:
(define/match (sorted-map-to-tree node)
  [(L!)             'L]
  [(T! c l k v r)   `(,c ,(sorted-map-to-tree l) ,k ,v ,(sorted-map-to-tree r))]
  [else              node])


; Converts a sorted map into an alist:
(define (sorted-map-to-alist node)
  
  (define (sorted-map-prepend-as-alist node alist)
    (match node
      [(T! l k v r)
       ; =>
       (sorted-map-prepend-as-alist 
        l
        (cons (cons k v)
              (sorted-map-prepend-as-alist r alist)))]
            
      [(L _)
       ; =>
       alist]))
  
  (sorted-map-prepend-as-alist node '()))
  

; Tests whether this map is a submap of another map:
(define (sorted-map-submap? map1 map2 #:by [by equal?])
  (define amap1 (sorted-map-to-alist map1))
  (define amap2 (sorted-map-to-alist map2))
  (define cmp   (sorted-map-compare map1))
  
  (define (compare-alists amap1 amap2)
    (match* (amap1 amap2)
      [(`((,k1 . ,v1) . ,rest1)
        `((,k2 . ,v2) . ,rest2))
       ; =>
       (switch-compare (cmp k1 k2)
          [< #f]
          [= (and (by v1 v2) (compare-alists rest1 rest2))]
          [> (compare-alists amap1 rest2)])]
      
      [('() '())   #t]
      
      [(_ '())     #f]
      
      [('() _)     #t]))
  
  (compare-alists amap1 amap2))


; Gets an element from a sorted map:
;; tonyg 20140718 add on-missing argument
(define (sorted-map-get node key [on-missing (lambda () #f)])
  (let walk ((node node))
    (match node
      [(L!)    (on-missing)]
    
      [(T cmp c l k v r)
       ; =>
       (switch-compare (cmp key k)
	  [<   (walk l)]
	  [=   v]
	  [>   (walk r)])])))


; Returns the size of the sorted map:
;; tonyg 20140718 this is memoized to run in O(1) for every smap
(define sorted-map-size
  (memoize1
   (lambda (smap)
     (match smap
       [(T! l r)   (+ 1 (sorted-map-size l)
		        (sorted-map-size r))]
       [(L!)       0]))))


; Returns the maxium (key . value) pair:
(define/match (sorted-map-max node)
  [(T! _ k v (L!))   (cons k v)]
  [(T! _     r)      (sorted-map-max r)])


; Performs a check to see if both invariants are met:
(define (sorted-map-is-legal? node)
  
  ; Calculates the max black nodes on path:
  (define/match (max-black-height node)
    [(T! c l r)
     ; =>
     (+ (if (eq? c 'B) 1 0) (max (max-black-height l) 
                                 (max-black-height r)))]
    
    [(L!)     1])
  
  ; Calculates the min black nodes on a path:
  (define/match (min-black-height node)
    [(T! c l r)
     ; =>
     (+ (if (eq? c 'B) 1 0) (min (min-black-height l) 
                                 (min-black-height r)))]
    
    [(L!)     1])
  
  ; Is this tree black-balanced?
  (define (black-balanced? node)
    (= (max-black-height node)
       (min-black-height node)))
  
  ; Does this tree contain a red child of red?
  (define/match (no-red-red? node)
    [(or (B l r)
         (R (and l (B)) (and r (B))))
     ; =>
     (and (no-red-red? l) (no-red-red? r))]
    
    [(L!)    #t]
    [else    #f])
  
  (let ((colored?  (no-red-red? node))
        (balanced? (black-balanced? node)))
    (and colored? balanced?)))
    

; Deletes a key from this map:
(define (sorted-map-delete node key)
    
  (define cmp (sorted-map-compare node))
  
  ; Finds the node to be removed:
  (define/match (del node)
    [(T! c l k v r)
     ; =>
     (switch-compare (cmp key k)
       [<   (bubble c (del l) k v r)]
       [=   (remove node)]
       [>   (bubble c l k v (del r))])]
    
    [else     node])

  ; Removes this node; it might
  ; leave behind a double-black node:
  (define/match (remove node)
    ; Leaves are easiest to kill:
    [(R (L!) (L!))     (canonicalize (L cmp))]
    [(B (L!) (L!))     (canonicalize (BBL cmp))]
    
    ; Killing a node with one child;
    ; parent or child is red:
    [(or (R child (L!))
         (R (L!)  child))
     ; =>
     child]
    
    [(or (B (R l k v r) (L!))
         (B (L!) (R l k v r)))
     ; =>
     (canonicalize (T cmp 'B l k v r))]
    
    ; Killing a black node with one black child:
    [(or (B (L!) (and child (B)))
         (B (and child (B)) (L!)))
     ; =>
     (black+1 child)]
    
    ; Killing a node with two sub-trees:
    [(T! c (and l (T!)) (and r (T!)))
     ; =>
     (match-let (((cons k v) (sorted-map-max l))
                 (l*         (remove-max l)))
       (bubble c l* k v r))])
  
  ; Kills a double-black, or moves it to the top:
  (define (bubble c l k v r)
    (cond
      [(or (double-black? l) (double-black? r))
       ; =>
       (balance cmp (black+1 c) (black-1 l) k v (black-1 r))]
      
      [else (canonicalize (T cmp c l k v r))]))
  
  ; Removes the max node:
  (define/match (remove-max node)
    [(T!   l     (L!))  (remove node)]
    [(T! c l k v r   )  (bubble c l k v (remove-max r))])
   
  ; Delete the key, and color the new root black:
  (blacken (del node)))


;; tonyg 20140718 True iff key is in node
(define (sorted-map-has-key? node key)
  (let walk ((node node))
    (match node
      [(L!)    #f]
      [(T cmp c l k v r)
       (switch-compare (cmp key k)
	  [<   (walk l)]
	  [=   #t]
	  [>   (walk r)])])))

;; tonyg 20140718 Retrieve a set of the keys of smap
(define (sorted-map-keys smap [empty-set (set)])
  (let walk ((node smap) (acc empty-set))
    (match node
      [(T! l k v r) (walk l (set-add (walk r acc) k))]
      [(L _) acc])))

;; tonyg 20140718 Retrieve a list of the values of smap
(define (sorted-map-values smap)
  (let walk ((node smap) (acc '()))
    (match node
      [(T! l k v r) (walk l (cons v (walk r acc)))]
      [(L _) acc])))
