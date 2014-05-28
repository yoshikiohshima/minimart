#lang racket/base
;; Exploring capabilities and SPKI/SDSI-style certificates in a
;; minimart setting. See RFC 2693.

(require racket/match)
(require "pattern.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model of public-key cryptographic boxing and signing.
;;
;; Obviously, not really securing anything at all; users are supposed
;; to avert their eyes from the fields of the envelope and signature
;; structs.

(struct sk (id) #:transparent)
(struct pk (id) #:transparent)

(struct envelope (contents from-id to-id)
	#:property prop:custom-write
	(lambda (v port mode)
	  (match-define (envelope contents from-id to-id) v)
	  (fprintf port "#[BOX ~a->~a ~v]" from-id to-id (equal-hash-code contents))))
(struct signature (doc id)
	#:property prop:custom-write
	(lambda (v port mode)
	  (match-define (signature doc id) v)
	  (fprintf port "#[SIGNATURE ~a ~v]" id (equal-hash-code doc))))

(define (seed->keypair seed)
  (values (sk seed) (pk seed)))

(define (make-keypair)
  (define id (gensym 'kp))
  (values (sk id) (pk id)))

(define (encrypt what from to)
  (envelope what (sk-id from) (pk-id to)))

(define (decrypt what from to)
  (and (equal? (pk-id from) (envelope-from-id what))
       (equal? (sk-id to) (envelope-to-id what))
       (envelope-contents what)))

(define (sign what who)
  (signature what (sk-id who)))

(define (verify sig doc who)
  (and (equal? (signature-doc sig) doc)
       (equal? (signature-id sig) (pk-id who))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Model of certificates.

;; From RFC 2693:
;;
;; CERTIFICATE: a signed instrument that empowers the Subject. It
;; contains at least an Issuer and a Subject. It can contain validity
;; conditions, authorization and delegation information. Certificates
;; come in three categories: ID (mapping <name,key>), Attribute
;; (mapping <authorization,name>), and Authorization (mapping
;; <authorization,key>). An SPKI authorization or attribute
;; certificate can pass along all the empowerment it has received from
;; the Issuer or it can pass along only a portion of that empowerment.
;;
;; ISSUER: the signer of a certificate and the source of empowerment
;; that the certificate is communicating to the Subject.
;;
;; SUBJECT: the thing empowered by a certificate or ACL entry. This
;; can be in the form of a key, a name (with the understanding that
;; the name is mapped by certificate to some key or other object), a
;; hash of some object, or a set of keys arranged in a threshold
;; function.

;; ** SKETCHY
(struct identity (name key) #:transparent)
(struct attribute (authorization name) #:transparent)
(struct authorization (authorization key) #:transparent)
(struct certificate (issuer-pk body signature) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simplified 5-tuple reduction
;;
;; AUTHORIZATION TRIPLES
;;
;; We assume that SPKI's "delegation" flag is always set TRUE; that
;; is, delegation is always permitted. For simplicity at this point,
;; we also omit validity dates (not-before/not-after). This leaves us
;; with triples: <ISSUER, SUBJECT, AUTHORIZATION>, which reads as
;; "ISSUER authorizes SUBJECT, based on ISSUER's own authorizations,
;; to perform actions covered by AUTHORIZATION."
;;
;;   ISSUER :: Key
;;   SUBJECT :: Name reference
;;   AUTHORIZATION :: Pattern
;;
;; The reduction rule (really, more of an inference rule) then becomes:
;;
;;   < I0, I1, A1 > + < I1, I2, A2 > → < I0, I2, AIntersect( A1, A2 ) >
;;
;; where AIntersect is essentially minimart's pattern-intersection/
;; unification logic.
;;
;; N.B. this rule seems to throw away the left-hand-sides, which isn't
;; necessarily the right idea unless we have a particular goal in
;; mind. It might be better to either specify the goal we're headed
;; toward ("Can I2 perform some request that's a subset of
;; AIntersect(A1, A2)?") or be more rigorous about nonlinear use of
;; the left-hand-sides.
;;
;; NAME DEFINITION TRIPLES
;;
;; In addition, the various identities may be referred to by primitive
;; key ID or by symbolic name. SPKI's 4-tuples map symbolic names down
;; to keys; here we again simplify, keeping only <ISSUER, NAME,
;; SUBJECT>, which reads as "ISSUER defines NAME within ISSUER's
;; namespace to denote SUBJECT." Now, SUBJECT may in turn be a name
;; reference.
;;
;;   ISSUER :: Key
;;   NAME :: String
;;   SUBJECT :: Name reference
;;
;; RFC 2693 takes special care to suggest resolving chains of name
;; definitions by iteratively rewriting definitions using only those
;; definitions mapping to base keys. Doing so ensures termination and
;; detection of naming loops. (When no further base-key-applications
;; are applicable, any remaining rules not mapping names to base keys
;; must involve either an undefined name or a definition loop.) See
;; RFC 2693 section 6.4, "4-tuple Reduction".
;;
;; NAME REFERENCES
;;
;; Symbolic names are referenced with respect to some namespace, which
;; is itself identified simply by a key.
;;
;;   Name reference ::= Key | (Name reference, String)

;; QUESTIONS:
;;   - should "names" within a scope/namespace be *patterns* instead of strings?
