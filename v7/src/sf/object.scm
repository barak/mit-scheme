#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/object.scm,v 3.0 1987/03/10 13:25:07 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Data Types

(declare (usual-integrations))

(let-syntax ()

(define-syntax define-type
  (macro (name enumeration slots)
    (let ((enumerand (symbol-append name '/ENUMERAND)))
      `(BEGIN
	 (DEFINE ,enumerand
	   (NAME->ENUMERAND ,(symbol-append 'ENUMERATION/ enumeration) ',name))
	 ((ACCESS ADD-UNPARSER-SPECIAL-OBJECT! UNPARSER-PACKAGE) ,enumerand
	  (LAMBDA (OBJECT)
	    (UNPARSE-WITH-BRACKETS
	     (LAMBDA ()
	       (WRITE ',name)
	       (WRITE-STRING " ")
	       (WRITE (HASH OBJECT))))))
	 (DEFINE ,(symbol-append name '?) (OBJECT/PREDICATE ,enumerand))
	 ,@(let loop ((slots slots) (index 1))
	     (if (null? slots)
		 '()
		 (let ((slot (car slots)))
		   (let ((ref-name (symbol-append name '/ slot))
			 (set-name (symbol-append name '/SET- slot '!)))
		     `((DECLARE (INTEGRATE-OPERATOR ,ref-name ,set-name))
		       (DEFINE (,ref-name ,name)
			 (DECLARE (INTEGRATE ,name))
			 (VECTOR-REF ,name ,index))
		       (DEFINE (,set-name ,name ,slot)
			 (DECLARE (INTEGRATE ,name ,slot))
			 (VECTOR-SET! ,name ,index ,slot))
		       ,@(loop (cdr slots) (1+ index)))))))))))

(define-syntax define-simple-type
  (macro (name enumeration slots)
    (let ((make-name (symbol-append name '/MAKE)))
      `(BEGIN (DECLARE (INTEGRATE-OPERATOR ,make-name))
	      (DEFINE (,make-name ,@slots)
		(DECLARE (INTEGRATE ,@slots))
		(OBJECT/ALLOCATE ,(symbol-append name '/ENUMERAND) ,@slots))
	      (DEFINE-TYPE ,name ,enumeration ,slots)))))

(declare (integrate object/allocate)
	 (integrate-operator object/enumerand))

(define object/allocate vector)

(define (object/enumerand object)
  (declare (integrate object))
  (vector-ref object 0))

(define (object/predicate enumerand)
  (lambda (object)
    (and (vector? object)
	 (not (zero? (vector-length object)))
	 (eq? enumerand (vector-ref object 0)))))

;;;; Enumerations

(define (enumeration/make names)
  (let ((enumeration (make-vector (length names))))
    (let loop ((names names) (index 0))
      (if (not (null? names))
	  (begin
	    (vector-set! enumeration index
			 (vector enumeration (car names) index))
	    (loop (cdr names) (1+ index)))))
    enumeration))

(declare (integrate-operator enumerand/enumeration enumerand/name
			     enumerand/index enumeration/cardinality
			     index->enumerand))

(define (enumerand/enumeration enumerand)
  (declare (integrate enumerand))
  (vector-ref enumerand 0))

(define (enumerand/name enumerand)
  (declare (integrate enumerand))
  (vector-ref enumerand 1))

(define (enumerand/index enumerand)
  (declare (integrate enumerand))
  (vector-ref enumerand 2))

(define (enumeration/cardinality enumeration)
  (declare (integrate enumeration))
  (vector-length enumeration))

(define (index->enumerand enumerand index)
  (declare (integrate enumerand index))
  (vector-ref enumerand index))

(define (name->enumerand enumeration name)
  (let ((length (enumeration/cardinality enumeration)))
    (let loop ((index 0))
      (and (< index length)
	   (let ((enumerand (index->enumerand enumeration index)))
	     (if (eqv? name (enumerand/name enumerand))
		 enumerand
		 (loop (1+ index))))))))

;;;; Random Types

(define enumeration/random
  (enumeration/make
   '(BLOCK
     DELAYED-INTEGRATION
     VARIABLE
     )))

(define-type block random
  (parent children safe? declarations bound-variables expression))

(define (block/make parent safe?)
  (let ((block
	 (object/allocate block/enumerand parent '() safe? '() '()
			  false)))
    (if parent
	(block/set-children! parent (cons block (block/children parent))))
    block))

(define-type delayed-integration random
  (state environment operations value))

(define (delayed-integration/make operations expression)
  (object/allocate delayed-integration/enumerand 'NOT-INTEGRATED false
		   operations expression))

(define-simple-type variable random
  (block name))

(define (variable/make&bind! block name)
  (let ((variable (variable/make block name)))
    (block/set-bound-variables! block
				(cons variable
				      (block/bound-variables block)))
    variable))

(define open-block/value-marker
  "value marker")

;;;; Expression Types

(define enumeration/expression
  (enumeration/make
   '(ACCESS
     ASSIGNMENT
     COMBINATION
     CONDITIONAL
     CONSTANT
     DECLARATION
     DELAY
     DISJUNCTION
     IN-PACKAGE
     OPEN-BLOCK
     PROCEDURE
     QUOTATION
     REFERENCE
     SEQUENCE
     THE-ENVIRONMENT
     )))

(define (expression/make-dispatch-vector)
  (make-vector (enumeration/cardinality enumeration/expression)))

(define (expression/make-method-definer dispatch-vector)
  (lambda (type-name method)
    (vector-set! dispatch-vector
		 (enumerand/index
		  (name->enumerand enumeration/expression type-name))
		 method)))

(declare (integrate-operator expression/method name->method))

(define (expression/method dispatch-vector expression)
  (declare (integrate dispatch-vector expression))
  (vector-ref dispatch-vector (enumerand/index (object/enumerand expression))))

(define (name->method dispatch-vector name)
  ;; Useful for debugging
  (declare (integrate dispatch-vector name))
  (vector-ref dispatch-vector
	      (enumerand/index (name->enumerand enumeration/expression name))))

(define-simple-type access expression (environment name))
(define-simple-type assignment expression (block variable value))
(define-simple-type combination expression (operator operands))
(define-simple-type conditional expression (predicate consequent alternative))
(define-simple-type constant expression (value))
(define-simple-type declaration expression (declarations expression))
(define-simple-type delay expression (expression))
(define-simple-type disjunction expression (predicate alternative))
(define-simple-type in-package expression (environment quotation))
(define-simple-type open-block expression (block variables values actions))
(define-simple-type procedure expression
  (block name required optional rest body))
(define-simple-type quotation expression (block expression))
(define-simple-type reference expression (block variable))
(define-simple-type sequence expression (actions))
(define-simple-type the-environment expression (block))

;;; end LET-SYNTAX
)