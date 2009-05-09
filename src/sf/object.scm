#| -*-Scheme-*-

$Id: object.scm,v 4.22 2008/01/30 20:02:38 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Data Types
;;; package: (scode-optimizer)

(declare (usual-integrations))

;;;; Enumerations

(define (enumeration/make names)
  (let ((enumerands 
	 (let loop ((names names) (index 0))
	   (if (pair? names)
	       (cons (vector #f (car names) index)
		     (loop (cdr names) (1+ index)))
	       '()))))
    (let ((enumeration
	   (cons (list->vector enumerands)
		 (map (lambda (enumerand)
			(cons (enumerand/name enumerand) enumerand))
		      enumerands))))
      (for-each (lambda (enumerand)
		  (vector-set! enumerand 0 enumeration))
		enumerands)
      enumeration)))

(define-structure (enumerand (type vector)
			     (conc-name enumerand/))
  (enumeration #f read-only #t)
  (name #f read-only #t)
  (index #f read-only #t))

(define-integrable (enumeration/cardinality enumeration)
  (vector-length (car enumeration)))

(define-integrable (enumeration/index->enumerand enumeration index)
  (vector-ref (car enumeration) index))

(define (enumeration/name->enumerand enumeration name)
  (cdr (or (assq name (cdr enumeration))
	   (error "Unknown enumeration name:" name))))

(define (enumeration/name->index enumeration name)
  (enumerand/index (enumeration/name->enumerand enumeration name)))

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     (let ((enumeration-name (cadr form))
	   (enumerand-names (caddr form)))
       `(BEGIN
	  (DEFINE ,enumeration-name
	    (ENUMERATION/MAKE ',enumerand-names))
	  ,@(map (lambda (enumerand-name)
		   `(DEFINE ,(symbol-append enumerand-name '/ENUMERAND)
		      (ENUMERATION/NAME->ENUMERAND
		       ,(close-syntax enumeration-name environment)
		       ',enumerand-name)))
		 enumerand-names))))))

(define-enumeration enumeration/random
  (block
   delayed-integration
   variable))
(define-enumeration enumeration/expression
  (access
   assignment
   combination
   conditional
   constant
   declaration
   delay
   disjunction
   open-block
   procedure
   quotation
   reference
   sequence
   the-environment))

;;;; Records

(define-structure (block (type vector)
			 (named block/enumerand)
			 (conc-name block/)
			 (constructor %block/make
				      (parent safe? bound-variables)))
  parent
  (children '())
  safe?
  (declarations (declarations/make-null))
  bound-variables
  (flags '()))

(define-structure (delayed-integration
		   (type vector)
		   (named delayed-integration/enumerand)
		   (conc-name delayed-integration/)
		   (constructor delayed-integration/make (operations value)))
  (state 'NOT-INTEGRATED)
  (environment #f)
  operations
  value)

(define-syntax define-simple-type
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (slots (caddr form))
	   (scode? (if (pair? (cdddr form)) (cadddr form) #t)))
       `(DEFINE-STRUCTURE
	    (,name
	     (TYPE VECTOR)
	     (NAMED
	      ,(close-syntax (symbol-append name '/ENUMERAND) environment))
	     (TYPE-DESCRIPTOR ,(symbol-append 'RTD: name))
	     (CONC-NAME ,(symbol-append name '/))
	     (CONSTRUCTOR ,(symbol-append name '/MAKE)))
	  ,@(if scode?
		`((scode #f read-only #t))
		`())
	  ,@slots)))))

(define-simple-type variable (block name flags) #F)
(define-simple-type access (environment name))
(define-simple-type assignment (block variable value))
(define-simple-type combination (block operator operands))
(define-simple-type conditional (predicate consequent alternative))
(define-simple-type constant (value))
(define-simple-type declaration (declarations expression))
(define-simple-type delay (expression))
(define-simple-type disjunction (predicate alternative))
(define-simple-type open-block (block variables values actions optimized))
(define-simple-type procedure (block name required optional rest body))
(define-simple-type quotation (block expression))
(define-simple-type reference (block variable))
(define-simple-type sequence (actions))
(define-simple-type the-environment (block))

;; Abstraction violations

(define-integrable (object/enumerand object)
  (vector-ref object 0))

(define-integrable (set-object/enumerand! object enumerand)
  (vector-set! object 0 enumerand))

(define-integrable (object/scode object)
  (vector-ref object 1))

(define (with-new-scode scode object)
  (let ((new (vector-copy object)))
    (vector-set! new 1 scode)
    new))

;;;; Miscellany

(define-syntax define-flag
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((name (cadr form))
	   (tester (caddr form))
	   (setter (cadddr form)))
       `(BEGIN
	  (DEFINE (,tester VARIABLE)
	    (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
	  (DEFINE (,setter VARIABLE)
	    (IF (NOT (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
		(SET-VARIABLE/FLAGS!
		 VARIABLE
		 (CONS ',name (VARIABLE/FLAGS VARIABLE))))))))))

(define-flag SIDE-EFFECTED variable/side-effected variable/side-effect!)
(define-flag REFERENCED    variable/referenced    variable/reference!)
(define-flag INTEGRATED    variable/integrated    variable/integrated!)
(define-flag CAN-IGNORE    variable/can-ignore?   variable/can-ignore!)

(define open-block/value-marker
  ;; This must be an interned object because we will fasdump it and
  ;; fasload it back in.
  (intern "#[(scode-optimizer)open-block/value-marker]"))

(define (expression/make-dispatch-vector)
  (make-vector (enumeration/cardinality enumeration/expression)))

(define (expression/make-method-definer dispatch-vector)
  (lambda (type-name method)
    (vector-set! dispatch-vector
		 (enumeration/name->index enumeration/expression type-name)
		 method)))

(define-integrable (expression/method dispatch-vector expression)
  (vector-ref dispatch-vector (enumerand/index (object/enumerand expression))))

(define-integrable (name->method dispatch-vector name)
  ;; Useful for debugging
  (vector-ref dispatch-vector
	      (enumeration/name->index enumeration/expression name)))

(define-integrable (global-ref/make name)
  (access/make #f
	       (constant/make #f system-global-environment)
	       name))

(define (global-ref? object)
  (and (access? object)
       (constant? (access/environment object))
       (eq? system-global-environment
	    (constant/value (access/environment object)))
       (access/name object)))

(define-integrable (constant->integration-info constant)
  (make-integration-info (constant/make #f constant)))

(define-integrable (integration-info? object)
  (and (pair? object)
       (eq? integration-info-tag (car object))))

(define-integrable (make-integration-info expression)
  (cons integration-info-tag expression))

(define-integrable (integration-info/expression integration-info)
  (cdr integration-info))

(define integration-info-tag
  (string-copy "integration-info"))