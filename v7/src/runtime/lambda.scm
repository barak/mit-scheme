;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/lambda.scm,v 13.42 1987/03/17 18:51:08 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Lambda Abstraction

(declare (usual-integrations))

(define lambda?)
(define make-lambda)
(define lambda-components)
(define lambda-body)
(define set-lambda-body!)
(define lambda-bound)

(define lambda-package
  (let ((slambda-type (microcode-type 'LAMBDA))
	(slexpr-type (microcode-type 'LEXPR))
	(xlambda-type (microcode-type 'EXTENDED-LAMBDA))
	(internal-lambda-tag (make-named-tag "INTERNAL-LAMBDA"))
	(internal-lexpr-tag (make-named-tag "INTERNAL-LEXPR"))
	(lambda-optional-tag (make-interned-symbol "#!OPTIONAL"))
	(lambda-rest-tag (make-interned-symbol "#!REST")))

(define internal-lambda-tags
  (list internal-lambda-tag internal-lexpr-tag))

;;;; Hairy Advice Wrappers

;;; The body of a LAMBDA object can be modified by transformation.
;;; This has the advantage that the body can be transformed many times,
;;; but the original state will always remain.

;;; **** Note:  this stuff was implemented for the advice package.
;;;      Please don't use it for anything else since it will just
;;;      confuse things.

(define lambda-body-procedures
  (let ((wrapper-tag '(LAMBDA-WRAPPER))
	(wrapper-body comment-expression)
	(set-wrapper-body! set-comment-expression!))

    (define (make-wrapper original-body new-body state)
      (make-comment (vector wrapper-tag original-body state)
		    new-body))

    (define (wrapper? object)
      (and (comment? object)
	   (let ((text (comment-text object)))
	     (and (vector? text)
		  (not (zero? (vector-length text)))
		  (eq? (vector-ref text 0) wrapper-tag)))))
    
    (define (wrapper-state wrapper)
      (vector-ref (comment-text wrapper) 2))

    (define (set-wrapper-state! wrapper new-state)
      (vector-set! (comment-text wrapper) 2 new-state))

    (define (wrapper-original-body wrapper)
      (vector-ref (comment-text wrapper) 1))

    (define (set-wrapper-original-body! wrapper new-body)
      (vector-set! (comment-text wrapper) 1 new-body))

    (named-lambda (lambda-body-procedures physical-body set-physical-body!
		    receiver)
      (receiver

       (named-lambda (wrap-body! lambda transform)
	 (let ((physical-body (physical-body lambda)))
	   (if (wrapper? physical-body)
	       (transform (wrapper-body physical-body)
			  (wrapper-state physical-body)
			  (lambda (new-body new-state)
			    (set-wrapper-body! physical-body new-body)
			    (set-wrapper-state! physical-body new-state)))
	       (transform physical-body
			  '()
			  (lambda (new-body new-state)
			    (set-physical-body! lambda
						(make-wrapper physical-body
							      new-body
							      new-state)))))))

       (named-lambda (wrapper-components lambda receiver)
	 (let ((physical-body (physical-body lambda)))
	   (if (wrapper? physical-body)
	       (receiver (wrapper-original-body physical-body)
			 (wrapper-state physical-body))
	       (receiver physical-body
			 '()))))

       (named-lambda (unwrap-body! lambda)
	 (let ((physical-body (physical-body lambda)))
	   (if (wrapper? physical-body)
	       (set-physical-body! lambda
				   (wrapper-original-body physical-body)))))

       (named-lambda (unwrapped-body lambda)
	 (let ((physical-body (physical-body lambda)))
	   (if (wrapper? physical-body)
	       (wrapper-original-body physical-body)
	       physical-body)))

       (named-lambda (set-unwrapped-body! lambda new-body)
	 (if (wrapper? (physical-body lambda))
	     (set-wrapper-original-body! (physical-body lambda) new-body)
	     (set-physical-body! lambda new-body)))

       ))
    ))

;;;; Compound Lambda

(define (make-clambda name required auxiliary body)
  (make-slambda name
		required
		(if (null? auxiliary)
		    body
		    (make-combination (make-slambda internal-lambda-tag
						    auxiliary
						    body)
				      (map (lambda (auxiliary)
					     (make-unassigned-object))
					   auxiliary)))))

(define (clambda-components clambda receiver)
  (slambda-components clambda
    (lambda (name required body)
      (let ((unwrapped-body (clambda-unwrapped-body clambda)))
	(if (combination? body)
	    (let ((operator (combination-operator body)))
	      (if (is-internal-lambda? operator)
		  (slambda-components operator
		    (lambda (tag auxiliary body)
		      (receiver name required '() '() auxiliary
				unwrapped-body)))
		  (receiver name required '() '() '() unwrapped-body)))
	    (receiver name required '() '() '() unwrapped-body))))))

(define (clambda-bound clambda)
  (slambda-components clambda
    (lambda (name required body)
      (if (combination? body)
	  (let ((operator (combination-operator body)))
	    (if (is-internal-lambda? operator)
		(slambda-components operator
		  (lambda (tag auxiliary body)
		    (append required auxiliary)))
		required))
	  required))))

(define (clambda-has-internal-lambda? clambda)
  (let ((body (slambda-body clambda)))
    (and (combination? body)
	 (let ((operator (combination-operator body)))
	   (and (is-internal-lambda? operator)
		operator)))))

(define clambda-wrap-body!)
(define clambda-wrapper-components)
(define clambda-unwrap-body!)
(define clambda-unwrapped-body)
(define set-clambda-unwrapped-body!)

(lambda-body-procedures (lambda (clambda)
			  (slambda-body
			   (or (clambda-has-internal-lambda? clambda)
			       clambda)))
			(lambda (clambda new-body)
			  (set-slambda-body!
			   (or (clambda-has-internal-lambda? clambda)
			       clambda)
			   new-body))
  (lambda (wrap-body! wrapper-components unwrap-body!
		      unwrapped-body set-unwrapped-body!)
    (set! clambda-wrap-body! wrap-body!)
    (set! clambda-wrapper-components wrapper-components)
    (set! clambda-unwrap-body! unwrap-body!)
    (set! clambda-unwrapped-body unwrapped-body)
    (set! set-clambda-unwrapped-body! set-unwrapped-body!)))

;;;; Compound Lexpr

(define (make-clexpr name required rest auxiliary body)
  (make-slexpr name
	       required
	       (make-combination (make-slambda internal-lexpr-tag
					       (cons rest auxiliary)
					       body)
				 (cons (let ((e (make-the-environment)))
					 (make-combination
					  system-subvector-to-list
					  (list e
						(+ (length required) 3)
						(make-combination
						 system-vector-size
						 (list e)))))
				       (map (lambda (auxiliary)
					      (make-unassigned-object))
					    auxiliary)))))

(define (clexpr-components clexpr receiver)
  (slexpr-components clexpr
    (lambda (name required body)
      (slambda-components (combination-operator body)
	(lambda (tag auxiliary body)
	  (receiver name
		    required
		    '()
		    (car auxiliary)
		    (cdr auxiliary)
		    (clexpr-unwrapped-body clexpr)))))))

(define (clexpr-bound clexpr)
  (slexpr-components clexpr
    (lambda (name required body)
      (slambda-components (combination-operator body)
	(lambda (tag auxiliary body)
	  (append required auxiliary))))))

(define (clexpr-has-internal-lambda? clexpr)
  (combination-operator (slexpr-body clexpr)))

(define clexpr-wrap-body!)
(define clexpr-wrapper-components)
(define clexpr-unwrap-body!)
(define clexpr-unwrapped-body)
(define set-clexpr-unwrapped-body!)

(lambda-body-procedures (lambda (clexpr)
			  (slambda-body (clexpr-has-internal-lambda? clexpr)))
			(lambda (clexpr new-body)
			  (set-slambda-body!
			   (clexpr-has-internal-lambda? clexpr)
			   new-body))
  (lambda (wrap-body! wrapper-components unwrap-body!
		      unwrapped-body set-unwrapped-body!)
    (set! clexpr-wrap-body! wrap-body!)
    (set! clexpr-wrapper-components wrapper-components)
    (set! clexpr-unwrap-body! unwrap-body!)
    (set! clexpr-unwrapped-body unwrapped-body)
    (set! set-clexpr-unwrapped-body! set-unwrapped-body!)))

;;;; Extended Lambda

(define (make-xlambda name required optional rest auxiliary body)
  (&typed-triple-cons xlambda-type
		      body
		      (list->vector
		       `(,name ,@required
			       ,@optional
			       ,@(if (null? rest)
				     auxiliary
				     (cons rest auxiliary))))
		      (make-non-pointer-object
		       (+ (length optional)
			  (* 256
			     (+ (length required)
				(if (null? rest) 0 256)))))))

(define (xlambda-components xlambda receiver)
  (let ((qr1 (integer-divide (primitive-datum (&triple-third xlambda)) 256)))
    (let ((qr2 (integer-divide (car qr1) 256)))
      (let ((ostart (1+ (cdr qr2))))
	(let ((rstart (+ ostart (cdr qr1))))
	  (let ((astart (+ rstart (car qr2)))
		(bound (&triple-second xlambda)))
	    (receiver (vector-ref bound 0)
		      (subvector->list bound 1 ostart)
		      (subvector->list bound ostart rstart)
		      (if (zero? (car qr2))
			  '()
			  (vector-ref bound rstart))
		      (subvector->list bound
				       astart
				       (vector-length bound))
		      (xlambda-unwrapped-body xlambda))))))))

(define (xlambda-bound xlambda)
  (let ((names (&triple-second xlambda)))
    (subvector->list names 1 (vector-length names))))

(define (xlambda-has-internal-lambda? xlambda)
  false)

(define xlambda-wrap-body!)
(define xlambda-wrapper-components)
(define xlambda-unwrap-body!)
(define xlambda-unwrapped-body)
(define set-xlambda-unwrapped-body!)

(lambda-body-procedures &triple-first &triple-set-first!
  (lambda (wrap-body! wrapper-components unwrap-body!
		      unwrapped-body set-unwrapped-body!)
    (set! xlambda-wrap-body! wrap-body!)
    (set! xlambda-wrapper-components wrapper-components)
    (set! xlambda-unwrap-body! unwrap-body!)
    (set! xlambda-unwrapped-body unwrapped-body)
    (set! set-xlambda-unwrapped-body! set-unwrapped-body!)))

;;;; Generic Lambda

(set! lambda?
(named-lambda (lambda? object)
  (or (primitive-type? slambda-type object)
      (primitive-type? slexpr-type object)
      (primitive-type? xlambda-type object))))

(define (is-internal-lambda? lambda)
  (and (primitive-type? slambda-type lambda)
       (memq (slambda-name lambda) internal-lambda-tags)))

(set! make-lambda
(named-lambda (make-lambda name required optional rest auxiliary
			   declarations body)
  (let ((body* (if (null? declarations)
		   body
		   (make-sequence (list (make-block-declaration declarations)
					body)))))
    (cond ((and (< (length required) 256)
		(< (length optional) 256)
		(or (not (null? optional))
		    (not (null? rest))
		    (not (null? auxiliary))))
	   (make-xlambda name required optional rest auxiliary body*))
	  ((not (null? optional))
	   (error "Optionals not implemented" 'MAKE-LAMBDA))
	  ((null? rest)
	   (make-clambda name required auxiliary body*))
	  (else
	   (make-clexpr name required rest auxiliary body*))))))

(set! lambda-components
(named-lambda (lambda-components lambda receiver)
  (&lambda-components lambda
    (lambda (name required optional rest auxiliary body)
      (let ((actions (and (sequence? body)
			  (sequence-actions body))))
	(if (and actions
		 (block-declaration? (car actions)))
	    (receiver name required optional rest auxiliary
		      (block-declaration-text (car actions))
		      (make-sequence (cdr actions)))
	    (receiver name required optional rest auxiliary '() body)))))))

(define ((dispatch-0 op-name clambda-op clexpr-op xlambda-op) lambda)
  ((cond ((primitive-type? slambda-type lambda) clambda-op)
	 ((primitive-type? slexpr-type lambda) clexpr-op)
	 ((primitive-type? xlambda-type lambda) xlambda-op)
	 (else (error "Not a lambda" op-name lambda)))
   lambda))

(define ((dispatch-1 op-name clambda-op clexpr-op xlambda-op) lambda arg)
  ((cond ((primitive-type? slambda-type lambda) clambda-op)
	 ((primitive-type? slexpr-type lambda) clexpr-op)
	 ((primitive-type? xlambda-type lambda) xlambda-op)
	 (else (error "Not a lambda" op-name lambda)))
   lambda arg))

(define &lambda-components
  (dispatch-1 'LAMBDA-COMPONENTS
	      clambda-components
	      clexpr-components
	      xlambda-components))

(define has-internal-lambda?
  (dispatch-0 'HAS-INTERNAL-LAMBDA?
	      clambda-has-internal-lambda?
	      clexpr-has-internal-lambda?
	      xlambda-has-internal-lambda?))

(define lambda-wrap-body!
  (dispatch-1 'LAMBDA-WRAP-BODY!
	      clambda-wrap-body!
	      clexpr-wrap-body!
	      xlambda-wrap-body!))

(define lambda-wrapper-components
  (dispatch-1 'LAMBDA-WRAPPER-COMPONENTS
	      clambda-wrapper-components
	      clexpr-wrapper-components
	      xlambda-wrapper-components))

(define lambda-unwrap-body!
  (dispatch-0 'LAMBDA-UNWRAP-BODY!
	      clambda-unwrap-body!
	      clexpr-unwrap-body!
	      xlambda-unwrap-body!))

(set! lambda-body
      (dispatch-0 'LAMBDA-BODY
		  clambda-unwrapped-body
		  clexpr-unwrapped-body
		  xlambda-unwrapped-body))

(set! set-lambda-body!
      (dispatch-1 'SET-LAMBDA-BODY!
		  set-clambda-unwrapped-body!
		  set-clexpr-unwrapped-body!
		  set-xlambda-unwrapped-body!))

(set! lambda-bound
      (dispatch-0 'LAMBDA-BOUND
		  clambda-bound
		  clexpr-bound
		  xlambda-bound))

;;;; Simple Lambda/Lexpr

(define (make-slambda name required body)
  (&typed-pair-cons slambda-type body (list->vector (cons name required))))

(define (slambda-components slambda receiver)
  (let ((bound (&pair-cdr slambda)))
    (receiver (vector-ref bound 0)
	      (subvector->list bound 1 (vector-length bound))
	      (&pair-car slambda))))

(define (slambda-name slambda)
  (vector-ref (&pair-cdr slambda) 0))

(define slambda-body &pair-car)
(define set-slambda-body! &pair-set-car!)

(define (make-slexpr name required body)
  (&typed-pair-cons slexpr-type body (list->vector (cons name required))))

(define slexpr-components slambda-components)
(define slexpr-body slambda-body)

;;; end LAMBDA-PACKAGE.
(the-environment)))

;;;; Alternative Component Views

(define (make-lambda* name required optional rest body)
  (scan-defines body
    (lambda (auxiliary declarations body*)
      (make-lambda name required optional rest auxiliary declarations body*))))

(define (lambda-components* lambda receiver)
  (lambda-components lambda
    (lambda (name required optional rest auxiliary declarations body)
      (receiver name required optional rest
		(make-open-block auxiliary declarations body)))))

(define (lambda-components** lambda receiver)
  (lambda-components* lambda
    (lambda (name required optional rest body)
      (receiver (vector name required optional rest)
		(append required optional (if (null? rest) '() (list rest)))
		body))))

(define (lambda-pattern/name pattern)
  (vector-ref pattern 0))

(define (lambda-pattern/required pattern)
  (vector-ref pattern 1))

(define (lambda-pattern/optional pattern)
  (vector-ref pattern 2))

(define (lambda-pattern/rest pattern)
  (vector-ref pattern 3))

(define (make-lambda** pattern bound body)

  (define (split pattern bound receiver)
    (cond ((null? pattern)
	   (receiver '() bound))
	  (else
	   (split (cdr pattern) (cdr bound)
	     (lambda (copy tail)
	       (receiver (cons (car bound) copy)
			 tail))))))

  (split (lambda-pattern/required pattern) bound
    (lambda (required tail)
      (split (lambda-pattern/optional pattern) tail
	(lambda (optional rest)
	  (make-lambda* (lambda-pattern/name pattern)
			required
			optional
			(if (null? rest) rest (car rest))
			body))))))