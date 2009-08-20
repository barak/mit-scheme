#| -*-Scheme-*-

$Id$

Copyright (c) 1994-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Scode->KMP Scheme
;;; package: (compiler midend)

(declare (usual-integrations))

(define (inlate/top-level scode)
  (inlate/remember (inlate/scode scode #F)
		   (new-dbg-expression/make scode #F)))

(define-macro (define-inlator scode-type components . body)
  (let ((proc-name (symbol-append 'INLATE/ scode-type))
	(destructor (symbol-append scode-type '-COMPONENTS)))
    `(DEFINE ,proc-name
       (NAMED-LAMBDA (,proc-name FORM OUTER-FORM)
	 (LET ((HANDLER (LAMBDA ,components ,@body)))
	   (INLATE/REMEMBER (,destructor FORM HANDLER)
			    (NEW-DBG-EXPRESSION/MAKE FORM OUTER-FORM)))))))

(define (inlate/sequence+ form outer-form)
  ;; Kludge
  (if (not (open-block? form))
      (inlate/sequence form outer-form)
      (inlate/remember
       (let ((form* (open-block-components form unscan-defines)))
	 (if (sequence? form*)
	     (beginnify
	      (inlate/map-declarations
	       (map (lambda (action) (inlate/scode action form))
		    (sequence-actions form*))))
	     (inlate/scode form* form)))
       (new-dbg-expression/make form outer-form))))

(define (inlate/constant object outer-form)
  outer-form
  `(QUOTE ,(if (unassigned-reference-trap? object) %unassigned object)))

(define (inlate/map-declarations exprs)
  (let loop ((exprs exprs))
    (cond ((null? exprs) '())
	  ((and (QUOTE/? (car exprs))
		(block-declaration? (quote/text (car exprs))))
	   (cons `(DECLARE ,@(block-declaration-text (quote/text (car exprs))))
		 (loop (cdr exprs))))		 
	(else
	 (cons (car exprs) (loop (cdr exprs)))))))

(define-inlator VARIABLE (name)
  `(LOOKUP ,name))

(define-inlator ASSIGNMENT (name svalue)
  `(SET! ,name ,(inlate/scode svalue form)))

(define-inlator DEFINITION (name svalue)
  `(DEFINE ,name ,(inlate/scode svalue form)))

(define-inlator THE-ENVIRONMENT ()
  `(THE-ENVIRONMENT))

(define (inlate/lambda form outer-form)
  outer-form				; ignored
  (lambda-components form
    (lambda (name req opt rest aux decls sbody)
      name				; Not used
      (let* ((lambda-list
	      (append req
		      (if (null? opt)
			  '()
			  (cons #!optional opt))
		      (if (not rest)
			  '()
			  (list #!rest rest))
		      (if (null? aux)
			  '()
			  (cons #!aux aux))))
	     (new
	      `(LAMBDA ,(cons (new-continuation-variable) lambda-list)
		 ,(let ((body (inlate/scode sbody #F)))
		    (if (null? decls)
			body
			(beginnify
			 (list `(DECLARE ,@decls)
			       body)))))))
	(inlate/remember new (new-dbg-procedure/make form))))))
#|
(define (inlate/lambda* name req opt rest aux decls sbody)
  name					; ignored
  `(LAMBDA ,(append (cons (new-continuation-variable) req)
		    (if (null? opt)
			'()
			(cons #!optional opt))
		    (if (not rest)
			'()
			(list #!rest rest))
		    (if (null? aux)
			'()
			(cons #!aux aux)))
     ,(let ((body (inlate/scode sbody)))
	(if (null? decls)
	    body
	    (beginnify
	     (list `(DECLARE ,@decls)
		   body))))))
|#

(define-inlator IN-PACKAGE (environment expression)
  `(IN-PACKAGE ,(inlate/scode environment form)
     ,(inlate/scode expression #F)))

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
	  `(CALL ,(inlate/scode rator form)
		 (QUOTE #F)		; continuation
		 ,@(map (lambda (rand) (inlate/scode rand form))
			rands))))))

(define-inlator COMMENT (text body)
  text					; ignored
  (inlate/scode body form))

(define-inlator SEQUENCE (actions)
  (beginnify
   (map (lambda (action) (inlate/scode action form))
	actions)))
     
(define-inlator CONDITIONAL (pred conseq alt)
  `(IF ,(inlate/scode pred form)
       ,(inlate/scode conseq form)
       ,(inlate/scode alt form)))

(define-inlator DISJUNCTION (pred alt)
  `(OR ,(inlate/scode pred form)
       ,(inlate/scode alt form)))

(define-inlator ACCESS (environment name)
  `(ACCESS ,name ,(inlate/scode environment form)))

(define-inlator DELAY (expression)
  `(DELAY ,(inlate/scode expression form)))

(define inlate/scode
  (let ((dispatch-vector
	 (make-vector (microcode-type/code-limit) inlate/constant)))

    (let-syntax
	((dispatch-entry
	  (macro (type handler)
	    `(VECTOR-SET! DISPATCH-VECTOR ,(microcode-type type)
			  (LAMBDA (EXPR OUTER-FORM)
			    (,handler EXPR OUTER-FORM))))))

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

      (named-lambda (inlate/expression expression outer-form)
	((vector-ref dispatch-vector (object-type expression))
	 expression
	 outer-form)))))

;; Utilities

(define (inlate/remember new old)
  (code-rewrite/remember* new old))