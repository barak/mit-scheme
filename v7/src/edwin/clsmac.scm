;;; -*-Scheme-*-
;;;
;;;$Id: clsmac.scm,v 1.3 1999/01/02 06:11:34 cph Exp $
;;;
;;; Copyright (c) 1986, 1989, 1999 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

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