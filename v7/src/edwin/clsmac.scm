;;; -*-Scheme-*-
;;;
;;;$Id: clsmac.scm,v 1.7 2001/12/23 17:20:58 cph Exp $
;;;
;;; Copyright (c) 1986, 1989, 1999, 2001 Massachusetts Institute of Technology
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Class/Object System

(declare (usual-integrations))

;;; ******************************************************************
;;; This software is intended for use in the Edwin window system only.
;;; Don't think about using it for anything else, since it is not, and
;;; likely will not ever, be supported as a part of the Scheme system.
;;; ******************************************************************

(define-syntax define-class
  (non-hygienic-macro-transformer
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
		    ',variables)))))

(define-syntax define-method
  (non-hygienic-macro-transformer
   (lambda (class bvl . body)
     (syntax-class-definition class bvl body
       (lambda (name expression)
	 (make-syntax-closure
	  (make-method-definition class name expression)))))))

(define-syntax with-instance-variables
  (non-hygienic-macro-transformer
   (lambda (class self free-names . body)
     (guarantee-symbol "Self name" self)
     (make-syntax-closure
      (syntax-class-expression class self free-names body)))))

(define-syntax =>
  (non-hygienic-macro-transformer
   (lambda (object operation . arguments)
     (guarantee-symbol "Operation name" operation)
     (let ((obname (string->uninterned-symbol "object")))
       `(LET ((,obname ,object))
	  ((CLASS-METHODS/REF (OBJECT-METHODS ,obname) ',operation)
	   ,obname
	   ,@arguments))))))

(define-syntax usual=>
  (non-hygienic-macro-transformer
   (lambda (object operation . arguments)
     (guarantee-symbol "Operation name" operation)
     (if (not *class-name*)
	 (error "Not inside class expression: USUAL=>" operation))
     `((CLASS-METHODS/REF (CLASS-METHODS (CLASS-SUPERCLASS ,*class-name*))
			  ',operation)
       ,object
       ,@arguments))))

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
  (make-comb (make-scode-variable 'CLASS-METHOD-DEFINE)
	     (make-scode-variable class)
	     operation
	     expression))

(define (make-comb operator . operands)
  (make-combination operator operands))

(define (guarantee-symbol s x)
  (if (not (symbol? x))
      (error (string-append s " must be a symbol") x)))