#| -*-Scheme-*-

$Id: inlate.scm,v 1.2 1994/11/22 03:49:09 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Scode->KMP Scheme
;;; package: (compiler midend)

(declare (usual-integrations))

(define (inlate/top-level scode)
  (inlate/remember (inlate/scode scode)
		   (new-dbg-expression/make scode)))

(define-macro (define-inlator scode-type components . body)
  (let ((proc-name (symbol-append 'INLATE/ scode-type))
	(destructor (symbol-append scode-type '-COMPONENTS)))
    `(define ,proc-name
       (let ((handler (lambda ,components ,@body)))
	 (named-lambda (,proc-name form)
	   (inlate/remember (,destructor form handler)
			    (new-dbg-expression/make form)))))))

(define (inlate/sequence+ form)
  ;; Kludge
  (if (not (open-block? form))
      (inlate/sequence form)
      (inlate/remember
       (let ((form* (open-block-components form unscan-defines)))
	 (if (sequence? form*)
	     (beginnify (lmap inlate/scode (sequence-actions form*)))
	     (inlate/scode form*)))
       (new-dbg-expression/make form))))

(define (inlate/constant object)
  `(QUOTE ,(if (unassigned-reference-trap? object) %unassigned object)))

(define-inlator VARIABLE (name)
  `(LOOKUP ,name))

(define-inlator ASSIGNMENT (name svalue)
  `(SET! ,name ,(inlate/scode svalue)))

(define-inlator DEFINITION (name svalue)
  `(DEFINE ,name ,(inlate/scode svalue)))

(define-inlator THE-ENVIRONMENT ()
  `(THE-ENVIRONMENT))

(define (inlate/lambda form)
  (lambda-components form
    (lambda (name req opt rest aux decls sbody)
      name				; Not used
      (let* ((lambda-list
	      (append req
		      (if (null? opt)
			  '()
			  (cons '#!OPTIONAL opt))
		      (if (not rest)
			  '()
			  (list '#!REST rest))
		      (if (null? aux)
			  '()
			  (cons '#!AUX aux))))
	     (new
	      `(LAMBDA ,(cons (new-continuation-variable) lambda-list)
		 ,(let ((body (inlate/scode sbody)))
		    (if (null? decls)
			body
			(beginnify
			 (list `(DECLARE ,@decls)
			       body)))))))
	(inlate/remember new
			 (new-dbg-procedure/make
			  form
			  (cons name lambda-list)))))))
#|
(define (inlate/lambda* name req opt rest aux decls sbody)
  name					; ignored
  `(LAMBDA ,(append (cons (new-continuation-variable) req)
		    (if (null? opt)
			'()
			(cons '#!OPTIONAL opt))
		    (if (not rest)
			'()
			(list '#!REST rest))
		    (if (null? aux)
			'()
			(cons '#!AUX aux)))
     ,(let ((body (inlate/scode sbody)))
	(if (null? decls)
	    body
	    (beginnify
	     (list `(DECLARE ,@decls)
		   body))))))
|#

(define-inlator IN-PACKAGE (environment expression)
  `(IN-PACKAGE ,(inlate/scode environment)
     ,(inlate/scode expression)))

(define-inlator COMBINATION (rator rands)
  (let-syntax ((ucode-primitive
		(macro (name)
		  (make-primitive-procedure name))))
    (let-syntax ((is-operator?
		  (macro (value name)
		    `(or (eq? ,value (ucode-primitive ,name))
			 (and (absolute-reference? ,value)
			      (eq? (absolute-reference-name ,value)
				   ',name))))))
      (if (and (is-operator? rator LEXICAL-UNASSIGNED?)
	       (not (null? rands))
	       (the-environment? (car rands))
	       (not (null? (cdr rands)))
	       (symbol? (cadr rands)))
	  `(UNASSIGNED? ,(cadr rands))
	  `(CALL ,(inlate/scode rator)
		 (QUOTE #F)		; continuation
		 ,@(lmap inlate/scode rands))))))

(define-inlator COMMENT (text body)
  text					; ignored
  (inlate/scode body))

(define-inlator SEQUENCE (actions)
  (beginnify (lmap inlate/scode actions)))
     
(define-inlator CONDITIONAL (pred conseq alt)
  `(IF ,(inlate/scode pred)
       ,(inlate/scode conseq)
       ,(inlate/scode alt)))

(define-inlator DISJUNCTION (pred alt)
  `(OR ,(inlate/scode pred)
       ,(inlate/scode alt)))

(define-inlator ACCESS (environment name)
  `(ACCESS ,name ,(inlate/scode environment)))

(define-inlator DELAY (expression)
  `(DELAY ,(inlate/scode expression)))

(define inlate/scode
  (let ((dispatch-vector
	 (make-vector (microcode-type/code-limit) inlate/constant)))

    (let-syntax
	((dispatch-entry
	  (macro (type handler)
	    `(VECTOR-SET! DISPATCH-VECTOR ,(microcode-type type)
			  (LAMBDA (EXPR)
			    (,handler EXPR))))))

      (let-syntax
	  ((dispatch-entries
	    (macro (types handler)
	      `(BEGIN ,@(map (lambda (type)
			       `(DISPATCH-ENTRY ,type ,handler))
			     types))))
	   (standard-entry
	    (macro (name)
	      `(DISPATCH-ENTRY ,name ,(symbol-append 'INLATE/ name)))))

	;; quotations are treated as constants.
	(standard-entry access)
	(standard-entry assignment)
	(standard-entry comment)
	(standard-entry conditional)
	(standard-entry definition)
	(standard-entry delay)
	(standard-entry disjunction)
	(standard-entry variable)
	(standard-entry in-package)
	(standard-entry the-environment)
	(dispatch-entries (combination-1 combination-2 combination
					 primitive-combination-0
					 primitive-combination-1
					 primitive-combination-2
					 primitive-combination-3)
			  inlate/combination)
	(dispatch-entries (lambda lexpr extended-lambda) inlate/lambda)
	(dispatch-entries (sequence-2 sequence-3) inlate/sequence+))

      (named-lambda (inlate/expression expression)
	((vector-ref dispatch-vector (object-type expression))
	 expression)))))

;; Utilities

(define (inlate/remember new old)
  (code-rewrite/remember* new old))