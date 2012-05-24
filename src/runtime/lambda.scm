#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

(define lambda-body)
(define set-lambda-body!)
(define lambda-bound)
(define lambda-bound?)
(define lambda-interface)
(define lambda-name)

;;; A lambda is an abstract 7-tuple consisting of these elements:
;;;  name          name of the lambda
;;;  required      list of symbols, required arguments in order (null if no required)
;;;  optional      list of symbols, optional arguments in order, (null if no optionals)
;;;  rest          symbol, rest argument, #F if no rest argument
;;;  auxiliary     list of auxiliaries to be bound to unassigned, (null if no auxiliaries)
;;;  declarations  list of declarations for the lexical block
;;;  body          an expression.  If there are auxiliaries, the body typically
;;;                begins with the appropriate assignments.

;;; A lambda has a concrete representation of either
;;; (ucode-type lambda) or (ucode-type extended-lambda),
;;; auxiliaries are implemented as an `internal' lambda
;;; of a compound lambda.

(define (initialize-package!)
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

  (lambda-body-procedures clambda/physical-body clambda/set-physical-body!
    (lambda (wrap-body! wrapper-components unwrap-body!
			unwrapped-body set-unwrapped-body!)
      (set! clambda-wrap-body! wrap-body!)
      (set! clambda-wrapper-components wrapper-components)
      (set! clambda-unwrap-body! unwrap-body!)
      (set! clambda-unwrapped-body unwrapped-body)
      (set! set-clambda-unwrapped-body! set-unwrapped-body!)))
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
  (set! lambda-arity
	(dispatch-1 'LAMBDA-ARITY
		    slambda-arity
		    slexpr-arity
		    xlambda-arity))
  (set! lambda-body
	(dispatch-0 'LAMBDA-BODY
		    clambda-unwrapped-body
		    clexpr/physical-body
		    xlambda-unwrapped-body))
  (set! lambda-bound
	(dispatch-0 'LAMBDA-BOUND
		    clambda-bound
		    clexpr-bound
		    xlambda-bound))
  (set! lambda-bound?
	(dispatch-1 'LAMBDA-BOUND?
		    clambda-bound?
		    clexpr-bound?
		    xlambda-bound?))
  (set! lambda-immediate-body
	(dispatch-0 'LAMBDA-IMMEDIATE-BODY
		    slambda-body
		    slexpr-body
		    xlambda-body))
  (set! lambda-interface
	(dispatch-0 'LAMBDA-INTERFACE
		    slambda-interface
		    clexpr-interface
		    xlambda-interface))
  (set! lambda-name
	(dispatch-0 'LAMBDA-NAME
		    slambda-name
		    slexpr-name
		    xlambda-name))
  (set! lambda-names-vector
	(dispatch-0 'LAMBDA-NAMES-VECTOR
		    slambda-names-vector
		    slexpr-names-vector
		    xlambda-names-vector))
  (set! lambda-unwrap-body!
	(dispatch-0 'LAMBDA-UNWRAP-BODY!
		    clambda-unwrap-body!
		    (lambda (*lambda)
		      *lambda
		      (error "Cannot advise clexprs."))
		    xlambda-unwrap-body!))
  (set! lambda-wrap-body!
	(dispatch-1 'LAMBDA-WRAP-BODY!
		    clambda-wrap-body!
		    (lambda (*lambda transform)
		      *lambda transform
		      (error "Cannot advise clexprs."))
		    xlambda-wrap-body!))
  (set! lambda-wrapper-components
	(dispatch-1 'LAMBDA-WRAPPER-COMPONENTS
		    clambda-wrapper-components
		    (lambda (*lambda receiver)
		      *lambda receiver
		      (error "Cannot advise clexprs."))
		    xlambda-wrapper-components))
  (set! set-lambda-body!
	(dispatch-1 'SET-LAMBDA-BODY!
		    set-clambda-unwrapped-body!
		    (lambda (*lambda new-body)
		      *lambda new-body
		      (error "Cannot advise clexprs."))
		    set-xlambda-unwrapped-body!)))

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
  (make-slambda name required (make-auxiliary-lambda auxiliary body)))

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

(define (clambda-bound? clambda symbol)
  (or (slambda-bound? clambda symbol)
      (auxiliary-bound? (slambda-body clambda) symbol)))

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

(define (auxiliary-bound? body symbol)
  (and (combination? body)
       (let ((operator (combination-operator body)))
	 (and (internal-lambda? operator)
	      (internal-lambda-bound? operator symbol)))))

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

;;; TODO(jrm):  I'm removing constructor so new SCode won't contain
;;; these, although given the conditions it is unlikely there were
;;; any.  In the next release we can remove the accessors etc.

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
		    (clexpr/physical-body clexpr)))))))

(define (clexpr-bound clexpr)
  (slexpr-components clexpr
    (lambda (name required body)
      name
      (let ((internal (combination-operator body)))
	(append required
		(slambda-auxiliary internal)
		(lambda-body-auxiliary (slambda-body internal)))))))

(define (clexpr-bound? clexpr symbol)
  (or (slexpr-bound? clexpr symbol)
      (clexpr-internal-bound? clexpr symbol)))

(define (clexpr-interface clexpr)
  (slexpr-components clexpr
    (lambda (name required body)
      name
      (let ((internal (combination-operator body)))
	(let ((auxiliary (slambda-auxiliary internal)))
	  (make-lambda-list required '() (car auxiliary) '()))))))

(define (clexpr-has-internal-lambda? clexpr)
  (let ((internal (combination-operator (slexpr-body clexpr))))
    (or (lambda-body-has-internal-lambda? (slambda-body internal))
	internal)))

(define (clexpr-internal-bound? clexpr symbol)
  (let ((body (slexpr-body clexpr)))
    (and (combination? body)
	 (let ((operator (combination-operator body)))
	   (and (internal-lambda? operator)
		(internal-lambda-bound? operator symbol))))))

(define (clexpr/physical-body clexpr)
  (slambda-body (clexpr-has-internal-lambda? clexpr)))

(define (clexpr/set-physical-body! clexpr body)
  (set-slambda-body! (clexpr-has-internal-lambda? clexpr) body))

;;;; Extended Lambda

(define (xlambda? object)
  (object-type? (ucode-type extended-lambda) object))

(define-guarantee xlambda "an extended lambda")

(define (%xlambda-body xlambda)
  (&triple-first xlambda))

(define (%xlambda-names-vector xlambda)
  (&triple-second xlambda))

(define (%xlambda-encoded-arity xlambda)
  (object-datum (&triple-third xlambda)))

(define (xlambda-body xlambda)
  (guarantee-xlambda xlambda 'xlambda-body)
  (%xlambda-body xlambda))

(define (xlambda-names-vector xlambda)
  (guarantee-xlambda xlambda 'xlambda-names-vector)
  (%xlambda-names-vector xlambda))

(define (xlambda-encoded-arity xlambda)
  (guarantee-xlambda xlambda 'xlambda-encoded-arity)
  (%xlambda-encoded-arity xlambda))

(define (encode-xlambda-arity n-required n-optional rest?)
  (+ n-optional (* 256 (+ n-required (if rest? 256 0)))))

(define (decode-xlambda-arity arity receiver)
  (let ((qr1 (integer-divide arity 256)))
    (let ((qr2 (integer-divide (car qr1) 256)))
      (receiver (cdr qr2)
		(cdr qr1)
		(= (car qr2) 1)))))

(define (make-xlambda name required optional rest auxiliary body)
  (&typed-triple-cons
   (ucode-type extended-lambda)
   (make-auxiliary-lambda auxiliary body)
   (list->vector
    (cons name (append required optional (if rest (list rest) '()))))
   (make-non-pointer-object
    (encode-xlambda-arity (length required) (length optional) rest))))

(define (xlambda-components xlambda receiver)
  (guarantee-xlambda xlambda 'xlambda-components)
  (decode-xlambda-arity
   (%xlambda-encoded-arity xlambda)
   (lambda (n-required n-optional rest?)
      (let ((ostart (1+ n-required)))
	(let ((rstart (+ ostart n-optional)))
	  (let ((astart (+ rstart (if rest? 1 0)))
		(bound (%xlambda-names-vector xlambda)))
	    (receiver (vector-ref bound 0)
		      (subvector->list bound 1 ostart)
		      (subvector->list bound ostart rstart)
		      (if rest?
			  (vector-ref bound rstart)
			  #F) ;;!'()
		      (append
		       (subvector->list bound astart (vector-length bound))
		       (lambda-body-auxiliary (&triple-first xlambda)))
		      (xlambda-unwrapped-body xlambda))))))))

(define (xlambda-arity xlambda offset)
  (xlambda-components xlambda
    (lambda (name required optional rest auxiliary decl body)
      name auxiliary decl body
      (make-lambda-arity (length required)
			 (length optional)
			 rest
			 offset))))

(define (%xlambda-interface xlambda)
  (decode-xlambda-arity
   (%xlambda-encoded-arity xlambda)
   (lambda (n-required n-optional rest?)
     (let ((bound (%xlambda-names-vector xlambda)))
       (make-lambda-list
	(subvector->list bound 1 (+ n-required 1))
	(subvector->list bound (+ n-required 1) (+ n-optional n-required 1))
	(and rest? (vector-ref bound (+ n-optional n-required 1)))
	'())))))

(define (xlambda-name xlambda)
  (guarantee-xlambda xlambda 'xlambda-name)
  (vector-ref (%xlambda-names-vector xlambda) 0))

(define (xlambda-interface xlambda)
  (guarantee-xlambda xlambda 'xlambda-interface)
  (%xlambda-interface xlambda))

(define (xlambda-bound xlambda)
  (guarantee-xlambda xlambda 'xlambda-bound)
  (append (let ((names (%xlambda-names-vector xlambda)))
	    (subvector->list names 1 (vector-length names)))
	  (lambda-body-auxiliary (%xlambda-body xlambda))))

(define (xlambda-bound? xlambda symbol)
  (guarantee-xlambda xlambda 'xlambda-bound?)
  (or (let ((bound (%xlambda-names-vector xlambda)))
	(subvector-find-next-element bound 1 (vector-length bound) symbol))
      (auxiliary-bound? (%xlambda-body xlambda) symbol)))

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
		    rest))
	   (make-xlambda name required optional rest auxiliary body*))
	  ((not (null? optional))
	   (error "Optionals not implemented" 'MAKE-LAMBDA))
	  (rest
	   (error "You want how many arguments?  AND a rest arg?"))
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


(define &lambda-components)
(define has-internal-lambda?)
(define lambda-arity)
(define lambda-wrap-body!)
(define lambda-wrapper-components)
(define lambda-unwrap-body!)
(define lambda-immediate-body)
(define lambda-names-vector)

(define-structure (block-declaration
		   (type vector)
		   (named ((ucode-primitive string->symbol)
			   "#[Block Declaration]")))
  (text #f read-only #t))

;;;; Simple Lambda
(define (slambda-arity slambda offset)
  (guarantee-slambda slambda 'slambda-arity)
  (%slambda-arity slambda offset))

(define (slambda-auxiliary slambda)
  (guarantee-slambda slambda 'slambda-auxiliary)
  (%slambda-auxiliary slambda))

(define (slambda-body slambda)
  (guarantee-slambda slambda 'slambda-body)
  (%slambda-body slambda))

(define (set-slambda-body! slambda new-body)
  (guarantee-slambda slambda 'set-slambda-body!)
  (%set-slambda-body! slambda new-body))

(define (slambda-components slambda receiver)
  (guarantee-slambda slambda 'slambda-components)
  (%slambda-components slambda receiver))

(define (slambda-interface slambda)
  (guarantee-slambda slambda 'slambda-interface)
  (%slambda-interface slambda))

(define (slambda-name slambda)
  (guarantee-slambda slambda 'slambda-name)
  (%slambda-name slambda))

(define (slambda-names-vector slambda)
  (guarantee-slambda slambda 'slambda-names-vector)
  (%slambda-names-vector slambda))

(define (make-slambda name required body)
  (&typed-pair-cons (ucode-type lambda)
		    body (list->vector (cons name required))))

(define-integrable (slambda? object)
  (object-type? (ucode-type lambda) object))

(define-guarantee slambda "simple lambda")

(define-integrable (%slambda-body slambda)
  (&pair-car slambda))

(define-integrable (%set-slambda-body! slambda body)
  (&pair-set-car! slambda body))

(define-integrable (%slambda-names-vector slambda)
  (&pair-cdr slambda))

(define (%slambda-arity slambda offset)
  (make-lambda-arity
   (- (vector-length (%slambda-names-vector slambda)) 1)
   0
   #f
   offset))

(define-integrable (%slambda-auxiliary slambda)
  (let ((bound (%slambda-names-vector slambda)))
    (subvector->list bound 1 (vector-length bound))))

(define-integrable (%slambda-interface slambda)
  (let ((bound (%slambda-names-vector slambda)))
    (make-lambda-list
     (subvector->list bound 1 (vector-length bound))
     '()
     #f
     '())))

(define (slambda-bound? slambda symbol)
  (let ((bound (%slambda-names-vector slambda)))
    (subvector-find-next-element bound 1 (vector-length bound) symbol)))

(define-integrable (%slambda-name slambda)
  (vector-ref (%slambda-names-vector slambda) 0))

(define (%slambda-components slambda receiver)
  (receiver (%slambda-name slambda)
	    (%slambda-interface slambda)
	    (%slambda-body slambda)))

;;;; Simple lexpr

;;; TODO(jrm):  I've removed the constructor so new SCode won't
;;; contain these.  In the next release we can remove the accessors
;;; etc.

(define-integrable slexpr-type
  (ucode-type lexpr))

(define-integrable (slexpr? object)
  (object-type? slexpr-type object))

(define (slexpr-components slexpr receiver)
  (let ((bound (&pair-cdr slexpr)))
    (receiver (vector-ref bound 0)
	      (subvector->list bound 1 (vector-length bound))
	      (&pair-car slexpr))))

(define (slexpr-interface slexpr)
  (let ((bound (&pair-cdr slexpr)))
    (subvector->list bound 1 (vector-length bound))))

(define (slexpr-arity slexpr offset)
  (let ((bound (&pair-cdr slexpr)))
    (make-lambda-arity (- (vector-length bound) 2) 0 #t offset)))

(define (slexpr-names-vector slexpr)
  (&pair-cdr slexpr))

(define (slexpr-bound? slexpr symbol)
  (let ((bound (&pair-cdr slexpr)))
    (subvector-find-next-element bound 1 (vector-length bound) symbol)))

(define-integrable (slexpr-name slexpr)
  (vector-ref (&pair-cdr slexpr) 0))

(define-integrable (slexpr-body slexpr)
  (&pair-car slexpr))

;;;; Internal Lambda

(define-integrable lambda-tag:internal-lambda
  ((ucode-primitive string->symbol) "#[internal-lambda]"))

(define-integrable lambda-tag:internal-lexpr
  ((ucode-primitive string->symbol) "#[internal-lexpr]"))

(define-integrable (%make-internal-lambda names body)
  (make-slambda lambda-tag:internal-lambda names body))

(define (make-auxiliary-lambda auxiliary body)
  (if (null? auxiliary)
      body
      (make-combination (%make-internal-lambda auxiliary body)
			(make-unassigned auxiliary))))

(define (internal-lambda? *lambda)
  (and (slambda? *lambda)
       (or (eq? (slambda-name *lambda) lambda-tag:internal-lambda)
	   (eq? (slambda-name *lambda) lambda-tag:internal-lexpr))))

(define (internal-lambda-bound? *lambda symbol)
  (and (slambda? *lambda)
       (slambda-bound? *lambda symbol)))

(define (make-unassigned auxiliary)
  (map (lambda (auxiliary)
	 auxiliary
	 (make-unassigned-reference-trap))
       auxiliary))

(define (make-lambda-arity required-count optional-count rest? offset)
  (let ((r (fix:- required-count offset)))
    (cond (rest?
	   (make-procedure-arity (fix:max 0 r) #f))
	  ((fix:>= r 0)
	   (make-procedure-arity r (fix:+ r optional-count)))
	  (else
	   (error "Illegal arity for entity:"
		  (list required-count optional-count rest? offset))))))