;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/clsmac.scm,v 1.1 1989/03/14 07:59:42 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
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

;;;; Class/Object System

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define class-syntax-table
  (make-syntax-table edwin-syntax-table))

(define ((scode-macro-spreader transform) expression)
  (apply transform (cdr expression)))

(syntax-table-define class-syntax-table 'DEFINE-CLASS
  (lambda (name superclass variables)
    (guarantee-symbol "Class name" name)
    (if (not (null? superclass))
	(guarantee-symbol "Class name" superclass))
    ;; Compile-time definition.
    (make-class name
		(if (null? superclass) false (name->class superclass))
		variables)
    ;; Load-time definition.
    `(DEFINE ,name
       (MAKE-CLASS ',name
		   ,(if (null? superclass) false superclass)
		   ',variables))))

(syntax-table-define class-syntax-table 'DEFINE-METHOD
  (lambda (class bvl . body)
    (syntax-class-definition class bvl body
      (lambda (name expression)
	(make-syntax-closure
	 (make-method-definition class name expression))))))

(syntax-table-define class-syntax-table 'WITH-INSTANCE-VARIABLES
  (lambda (class self free-names . body)
    (guarantee-symbol "Self name" self)
    (make-syntax-closure
     (syntax-class-expression class self free-names body))))

(syntax-table-define class-syntax-table '=>
  (lambda (object operation . arguments)
    (guarantee-symbol "Operation name" operation)
    (let ((obname (string->uninterned-symbol "object")))
      `(LET ((,obname ,object))
	 ((CLASS-METHODS/REF (OBJECT-METHODS ,obname) ',operation)
	  ,obname
	  ,@arguments)))))

(syntax-table-define class-syntax-table 'USUAL=>
  (lambda (object operation . arguments)
    (guarantee-symbol "Operation name" operation)
    (if (not *class-name*)
	(error "Not inside class expression: USUAL=>" operation))
    `((CLASS-METHODS/REF (CLASS-METHODS (CLASS-SUPERCLASS ,*class-name*))
			 ',operation)
      ,object
      ,@arguments)))

(define (syntax-class-definition class bvl body receiver)
  (parse-definition bvl body
    (lambda (name expression)
      (receiver name (syntax expression)))
    (lambda (bvl body)
      (let ((operation (car bvl))
	    (self (cadr bvl)))
	(guarantee-symbol "Operation name" operation)
	(guarantee-symbol "Self name" self)
	(receiver operation
		  (syntax-class-expression class
					   self
					   '()
					   `((NAMED-LAMBDA ,bvl ,@body))))))))

(define (parse-definition bvl body simple compound)
  (define (loop bvl body)
    (if (pair? (car bvl))
	(loop (car bvl)
	      `((LAMBDA ,(cdr bvl) ,@body)))
	(compound bvl body)))
  (if (symbol? bvl)
      (begin (if (not (null? (cdr body)))
		 (error "Multiple forms in definition body" body))
	     (simple bvl (car body)))
      (loop bvl body)))

(define *class-name* false)

(define (syntax-class-expression class-name self free-names expression)
  (guarantee-symbol "Class name" class-name)
  (fluid-let ((*class-name* class-name))
    (transform-instance-variables
     (class-instance-transforms (name->class class-name))
     self
     free-names
     (syntax* expression))))

(define (make-method-definition class operation expression)
  (make-comb (make-variable 'CLASS-METHOD-DEFINE)
	     (make-variable class)
	     operation
	     expression))

(define (make-comb operator . operands)
  (make-combination operator operands))

(define (guarantee-symbol s x)
  (if (not (symbol? x))
      (error (string-append s " must be a symbol") x)))