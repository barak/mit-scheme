#| -*-Scheme-*-

$Id: lambda.scm,v 14.20 2007/01/05 15:33:09 cph Exp $

Copyright (c) 1988-2000, 2002 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Lambda Abstraction
;;; package: (runtime lambda-abstraction)

(declare (usual-integrations))

(define (initialize-package!)
  (lambda-body-procedures clambda/physical-body clambda/set-physical-body!
    (lambda (wrap-body! wrapper-components unwrap-body!
			unwrapped-body set-unwrapped-body!)
      (set! clambda-wrap-body! wrap-body!)
      (set! clambda-wrapper-components wrapper-components)
      (set! clambda-unwrap-body! unwrap-body!)
      (set! clambda-unwrapped-body unwrapped-body)
      (set! set-clambda-unwrapped-body! set-unwrapped-body!)))
  (lambda-body-procedures clexpr/physical-body clexpr/set-physical-body!
    (lambda (wrap-body! wrapper-components unwrap-body!
			unwrapped-body set-unwrapped-body!)
      (set! clexpr-wrap-body! wrap-body!)
      (set! clexpr-wrapper-components wrapper-components)
      (set! clexpr-unwrap-body! unwrap-body!)
      (set! clexpr-unwrapped-body unwrapped-body)
      (set! set-clexpr-unwrapped-body! set-unwrapped-body!)))
  (lambda-body-procedures xlambda/physical-body xlambda/set-physical-body!
    (lambda (wrap-body! wrapper-components unwrap-body!
			unwrapped-body set-unwrapped-body!)
      (set! xlambda-wrap-body! wrap-body!)
      (set! xlambda-wrapper-components wrapper-components)
      (set! xlambda-unwrap-body! unwrap-body!)
      (set! xlambda-unwrapped-body unwrapped-body)
      (set! set-xlambda-unwrapped-body! set-unwrapped-body!)))
  (set! &lambda-components
	(dispatch-1 'LAMBDA-COMPONENTS
		    clambda-components
		    clexpr-components
		    xlambda-components))
  (set! has-internal-lambda?
	(dispatch-0 'HAS-INTERNAL-LAMBDA?
		    clambda-has-internal-lambda?
		    clexpr-has-internal-lambda?
		    xlambda-has-internal-lambda?))
  (set! lambda-wrap-body!
	(dispatch-1 'LAMBDA-WRAP-BODY!
		    clambda-wrap-body!
		    clexpr-wrap-body!
		    xlambda-wrap-body!))
  (set! lambda-wrapper-components
	(dispatch-1 'LAMBDA-WRAPPER-COMPONENTS
		    clambda-wrapper-components
		    clexpr-wrapper-components
		    xlambda-wrapper-components))
  (set! lambda-unwrap-body!
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
  (set! lambda-names-vector
	(dispatch-0 'LAMBDA-NAME
		    slambda-names-vector
		    slexpr-names-vector
		    xlambda-names-vector))
  (set! lambda-name
	(dispatch-0 'LAMBDA-NAME
		    slambda-name
		    slexpr-name
		    xlambda-name))
  (set! lambda-bound
	(dispatch-0 'LAMBDA-BOUND
		    clambda-bound
		    clexpr-bound
		    xlambda-bound)))

;;;; Hairy Advice Wrappers

;;; The body of a LAMBDA object can be modified by transformation.
;;; This has the advantage that the body can be transformed many times,
;;; but the original state will always remain.

;;; **** Note:  this stuff was implemented for the advice package.
;;; Please don't use it for anything else.

(define (lambda-body-procedures physical-body set-physical-body! receiver)
  (receiver
   (named-lambda (wrap-body! *lambda transform)
     (let ((physical-body (physical-body *lambda)))
       (if (wrapper? physical-body)
	   (transform (wrapper-body physical-body)
		      (wrapper-state physical-body)
		      (lambda (new-body new-state)
			(set-wrapper-body! physical-body new-body)
			(set-wrapper-state! physical-body new-state)))
	   (transform physical-body
		      '()
		      (lambda (new-body new-state)
			(set-physical-body! *lambda
					    (make-wrapper physical-body
							  new-body
							  new-state)))))))
   (named-lambda (wrapper-components *lambda receiver)
     (let ((physical-body (physical-body *lambda)))
       (if (wrapper? physical-body)
	   (receiver (wrapper-original-body physical-body)
		     (wrapper-state physical-body))
	   (receiver physical-body '()))))
   (named-lambda (unwrap-body! *lambda)
     (let ((physical-body (physical-body *lambda)))
       (if (wrapper? physical-body)
	   (set-physical-body! *lambda
			       (wrapper-original-body physical-body)))))
   (named-lambda (unwrapped-body *lambda)
     (let ((physical-body (physical-body *lambda)))
       (if (wrapper? physical-body)
	   (wrapper-original-body physical-body)
	   physical-body)))
   (named-lambda (set-unwrapped-body! *lambda new-body)
     (if (wrapper? (physical-body *lambda))
	 (set-wrapper-original-body! (physical-body *lambda) new-body)
	 (set-physical-body! *lambda new-body)))))

(define-integrable (make-wrapper original-body new-body state)
  (make-comment (vector wrapper-tag original-body state) new-body))

(define (wrapper? object)
  (and (comment? object)
       (let ((text (comment-text object)))
	 (and (vector? text)
	      (not (zero? (vector-length text)))
	      (eq? (vector-ref text 0) wrapper-tag)))))

(define wrapper-tag
  '(LAMBDA-WRAPPER))

(define-integrable (wrapper-body wrapper)
  (comment-expression wrapper))

(define-integrable (set-wrapper-body! wrapper body)
  (set-comment-expression! wrapper body))

(define-integrable (wrapper-state wrapper)
  (vector-ref (comment-text wrapper) 2))

(define-integrable (set-wrapper-state! wrapper new-state)
  (vector-set! (comment-text wrapper) 2 new-state))

(define-integrable (wrapper-original-body wrapper)
  (vector-ref (comment-text wrapper) 1))

(define-integrable (set-wrapper-original-body! wrapper body)
  (vector-set! (comment-text wrapper) 1 body))

;;;; Compound Lambda

(define (make-clambda name required auxiliary body)
  (make-slambda name
		required
		(if (null? auxiliary)
		    body
		    (make-combination (make-internal-lambda auxiliary body)
				      (make-unassigned auxiliary)))))

(define (clambda-components clambda receiver)
  (slambda-components clambda
    (lambda (name required body)
      (receiver name required '() '#F  ;;! '()
		(lambda-body-auxiliary body)
		(clambda-unwrapped-body clambda)))))

(define (clambda-bound clambda)
  (slambda-components clambda
    (lambda (name required body)
      name
      (append required (lambda-body-auxiliary body)))))

(define (clambda-has-internal-lambda? clambda)
  (lambda-body-has-internal-lambda? (slambda-body clambda)))

(define (lambda-body-auxiliary body)
  (if (combination? body)
      (let ((operator (combination-operator body)))
	(if (internal-lambda? operator)
	    (slambda-auxiliary operator)
	    '()))
      '()))

(define (lambda-body-has-internal-lambda? body)
  (and (combination? body)
       (let ((operator (combination-operator body)))
	 (and (internal-lambda? operator)
	      operator))))

(define clambda-wrap-body!)
(define clambda-wrapper-components)
(define clambda-unwrap-body!)
(define clambda-unwrapped-body)
(define set-clambda-unwrapped-body!)

(define (clambda/physical-body clambda)
  (slambda-body (or (clambda-has-internal-lambda? clambda) clambda)))

(define (clambda/set-physical-body! clambda body)
  (set-slambda-body! (or (clambda-has-internal-lambda? clambda) clambda) body))

;;;; Compound Lexpr

(define (make-clexpr name required rest auxiliary body)
  (make-slexpr name
	       required
	       (make-combination
		(make-internal-lexpr
		 (list rest)
		 (if (null? auxiliary)
		     body
		     (make-combination (make-internal-lambda auxiliary body)
				       (make-unassigned auxiliary))))
		(list (let ((environment (make-the-environment)))
			(make-combination
			 system-subvector->list
			 (list environment
			       (+ (length required) 3)
			       (make-combination system-vector-length
						 (list environment)))))))))

(define (clexpr-components clexpr receiver)
  (slexpr-components clexpr
    (lambda (name required body)
      (let ((internal (combination-operator body)))
	(let ((auxiliary (slambda-auxiliary internal)))
	  (receiver name
		    required
		    '()
		    (car auxiliary)
		    (append (cdr auxiliary)
			    (lambda-body-auxiliary (slambda-body internal)))
		    (clexpr-unwrapped-body clexpr)))))))

(define (clexpr-bound clexpr)
  (slexpr-components clexpr
    (lambda (name required body)
      name
      (let ((internal (combination-operator body)))
	(append required
		(slambda-auxiliary internal)
		(lambda-body-auxiliary (slambda-body internal)))))))

(define (clexpr-has-internal-lambda? clexpr)
  (let ((internal (combination-operator (slexpr-body clexpr))))
    (or (lambda-body-has-internal-lambda? (slambda-body internal))
	internal)))

(define clexpr-wrap-body!)
(define clexpr-wrapper-components)
(define clexpr-unwrap-body!)
(define clexpr-unwrapped-body)
(define set-clexpr-unwrapped-body!)

(define (clexpr/physical-body clexpr)
  (slambda-body (clexpr-has-internal-lambda? clexpr)))

(define (clexpr/set-physical-body! clexpr body)
  (set-slambda-body! (clexpr-has-internal-lambda? clexpr) body))

;;;; Extended Lambda

(define-integrable xlambda-type
  (ucode-type extended-lambda))

(define (make-xlambda name required optional rest auxiliary body)
  (&typed-triple-cons
   xlambda-type
   (if (null? auxiliary)
       body
       (make-combination (make-internal-lambda auxiliary body)
			 (make-unassigned auxiliary)))
   (list->vector
    (cons name (append required optional (if rest (list rest) '()))))
   (make-non-pointer-object
    (+ (length optional)
       (* 256
	  (+ (length required)
	     (if rest 256 0)))))))

(define-integrable (xlambda? object)
  (object-type? xlambda-type object))

(define (xlambda-components xlambda receiver)
  (let ((qr1 (integer-divide (object-datum (&triple-third xlambda)) 256)))
    (let ((qr2 (integer-divide (car qr1) 256)))
      (let ((ostart (1+ (cdr qr2))))
	(let ((rstart (+ ostart (cdr qr1))))
	  (let ((astart (+ rstart (car qr2)))
		(bound (&triple-second xlambda)))
	    (receiver (vector-ref bound 0)
		      (subvector->list bound 1 ostart)
		      (subvector->list bound ostart rstart)
		      (if (zero? (car qr2))
			  #F ;;!'()
			  (vector-ref bound rstart))
		      (append
		       (subvector->list bound astart (vector-length bound))
		       (lambda-body-auxiliary (&triple-first xlambda)))
		      (xlambda-unwrapped-body xlambda))))))))

(define (xlambda-names-vector xlambda)
  (&triple-second xlambda))

(define (xlambda-name xlambda)
  (vector-ref (&triple-second xlambda) 0))

(define (xlambda-bound xlambda)
  (append (let ((names (&triple-second xlambda)))
	    (subvector->list names 1 (vector-length names)))
	  (lambda-body-auxiliary (&triple-first xlambda))))

(define (xlambda-has-internal-lambda? xlambda)
  (lambda-body-has-internal-lambda? (&triple-first xlambda)))

(define xlambda-wrap-body!)
(define xlambda-wrapper-components)
(define xlambda-unwrap-body!)
(define xlambda-unwrapped-body)
(define set-xlambda-unwrapped-body!)

(define (xlambda/physical-body xlambda)
  (let ((internal (xlambda-has-internal-lambda? xlambda)))
    (if internal
	(slambda-body internal)
	(&triple-first xlambda))))

(define (xlambda/set-physical-body! xlambda body)
  (let ((internal (xlambda-has-internal-lambda? xlambda)))
    (if internal
	(set-slambda-body! internal body)
	(&triple-set-first! xlambda body))))

;;;; Generic Lambda

(define (lambda? object)
  (or (slambda? object)
      (slexpr? object)
      (xlambda? object)))

(define (make-lambda name required optional rest auxiliary declarations body)
  (let ((interface (append required optional (if rest (list rest) '()))))
    (let ((dup-interface (find-list-duplicates interface))
	  (dup-auxiliary (find-list-duplicates auxiliary)))
      (cond ((not (null? dup-interface))
	     ;; Syntax.scm gets this case in usual usage
	     (error "duplicate parameters" dup-interface
		    (error-irritant/noise " in") interface))
	    ((not (null? dup-auxiliary))
	     (error "duplicate internal definitions for" dup-auxiliary
		    (error-irritant/noise " in")
		    name)))))
  (let ((body*
	 (if (null? declarations)
	     body
	     (make-sequence (list (make-block-declaration declarations)
				  body)))))
    (cond ((and (< (length required) 256)
		(< (length optional) 256)
		(or (not (null? optional))
		    rest
		    (not (null? auxiliary))))
	   (make-xlambda name required optional rest auxiliary body*))
	  ((not (null? optional))
	   (error "Optionals not implemented" 'MAKE-LAMBDA))
	  (rest
	   (make-clexpr name required rest auxiliary body*))
	  (else
	   (make-clambda name required auxiliary body*)))))

(define (lambda-components *lambda receiver)
  (&lambda-components *lambda
    (lambda (name required optional rest auxiliary body)
      (let ((actions (and (sequence? body) (sequence-actions body))))
	(if (and actions (block-declaration? (car actions)))
	    (receiver name required optional rest auxiliary
		      (block-declaration-text (car actions))
		      (make-sequence (cdr actions)))
	    (receiver name required optional rest auxiliary '() body))))))

(define (find-list-duplicates items)
  (let loop ((items items) (duplicates '()))
    (cond ((null? items)
	   (reverse! duplicates))
	  ((memq (car items) (cdr items))
	   (if (memq (car items) duplicates)
	       (loop (cdr items) duplicates)
	       (loop (cdr items) (cons (car items) duplicates))))
	  (else
	   (loop (cdr items) duplicates)))))

(define ((dispatch-0 op-name clambda-op clexpr-op xlambda-op) *lambda)
  ((cond ((slambda? *lambda) clambda-op)
	 ((slexpr? *lambda) clexpr-op)
	 ((xlambda? *lambda) xlambda-op)
	 (else (error:wrong-type-argument *lambda "SCode lambda" op-name)))
   *lambda))

(define ((dispatch-1 op-name clambda-op clexpr-op xlambda-op) *lambda arg)
  ((cond ((slambda? *lambda) clambda-op)
	 ((slexpr? *lambda) clexpr-op)
	 ((xlambda? *lambda) xlambda-op)
	 (else (error:wrong-type-argument *lambda "SCode lambda" op-name)))
   *lambda arg))

(define &lambda-components)
(define has-internal-lambda?)
(define lambda-wrap-body!)
(define lambda-wrapper-components)
(define lambda-unwrap-body!)
(define lambda-body)
(define set-lambda-body!)
(define lambda-names-vector)
(define lambda-name)
(define lambda-bound)

(define-structure (block-declaration
		   (type vector)
		   (named ((ucode-primitive string->symbol)
			   "#[Block Declaration]")))
  (text #f read-only #t))

;;;; Simple Lambda/Lexpr

(define-integrable slambda-type
  (ucode-type lambda))

(define-integrable (make-slambda name required body)
  (&typed-pair-cons slambda-type body (list->vector (cons name required))))

(define-integrable (slambda? object)
  (object-type? slambda-type object))

(define (slambda-components slambda receiver)
  (let ((bound (&pair-cdr slambda)))
    (receiver (vector-ref bound 0)
	      (subvector->list bound 1 (vector-length bound))
	      (&pair-car slambda))))

(define (slambda-names-vector slambda)
  (&pair-cdr slambda))

(define-integrable (slambda-name slambda)
  (vector-ref (&pair-cdr slambda) 0))

(define (slambda-auxiliary slambda)
  (let ((bound (&pair-cdr slambda)))
    (subvector->list bound 1 (vector-length bound))))

(define-integrable (slambda-body slambda)
  (&pair-car slambda))

(define-integrable (set-slambda-body! slambda body)
  (&pair-set-car! slambda body))

(define-integrable slexpr-type
  (ucode-type lexpr))

(define-integrable (make-slexpr name required body)
  (&typed-pair-cons slexpr-type body (list->vector (cons name required))))

(define-integrable (slexpr? object)
  (object-type? slexpr-type object))

(define (slexpr-components slexpr receiver)
  (let ((bound (&pair-cdr slexpr)))
    (receiver (vector-ref bound 0)
	      (subvector->list bound 1 (vector-length bound))
	      (&pair-car slexpr))))

(define (slexpr-names-vector slexpr)
  (&pair-cdr slexpr))

(define-integrable (slexpr-name slexpr)
  (vector-ref (&pair-cdr slexpr) 0))

(define-integrable (slexpr-body slexpr)
  (&pair-car slexpr))

;;;; Internal Lambda

(define-integrable lambda-tag:internal-lambda
  ((ucode-primitive string->symbol) "#[internal-lambda]"))

(define-integrable lambda-tag:internal-lexpr
  ((ucode-primitive string->symbol) "#[internal-lexpr]"))

(define-integrable (make-internal-lambda names body)
  (make-slambda lambda-tag:internal-lambda names body))

(define-integrable (make-internal-lexpr names body)
  (make-slambda lambda-tag:internal-lexpr names body))

(define (internal-lambda? *lambda)
  (and (slambda? *lambda)
       (or (eq? (slambda-name *lambda) lambda-tag:internal-lambda)
	   (eq? (slambda-name *lambda) lambda-tag:internal-lexpr))))

(define (make-unassigned auxiliary)
  (map (lambda (auxiliary)
	 auxiliary
	 (make-unassigned-reference-trap))
       auxiliary))