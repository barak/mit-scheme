;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
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

(define class-macros
  (make-environment

(define ((scode-macro-spreader transform) expression)
  (apply transform (cdr expression)))

(syntax-table-define class-syntax-table 'DEFINE-CLASS
  (macro (name superclass variables)
    (guarantee-symbol "Class name" name)
    (if (not (null? superclass))
	(guarantee-symbol "Class name" superclass))
    ;; Compile-time definition.
    (make-class name
		(if (null? superclass) #!FALSE (name->class superclass))
		variables)
    ;; Load-time definition.
    `(DEFINE ,name
       (MAKE-CLASS ',name
		   ,(if (null? superclass) '#!FALSE superclass)
		   ',variables))))

(syntax-table-define class-syntax-table 'DEFINE-METHOD
  (scode-macro-spreader
   (lambda (class bvl . body)
     (syntax-class-definition class bvl body
       (lambda (name expression)
	 (make-method-definition class name expression))))))

(syntax-table-define class-syntax-table 'DEFINE-PROCEDURE
  (scode-macro-spreader
   (lambda (class bvl . body)
     (syntax-class-definition class bvl body
       (lambda (name expression)
	 (make-definition name expression))))))

(syntax-table-define class-syntax-table 'WITH-INSTANCE-VARIABLES
  (scode-macro-spreader
   (lambda (class self . body)
     (guarantee-symbol "Self name" self)
     (syntax-class-expression class self body))))

(syntax-table-define class-syntax-table '=>
  (macro (object operation . arguments)
    (guarantee-symbol "Operation name" operation)
    (if (symbol? object)
	`((ACCESS ,operation (OBJECT-METHODS ,object)) ,object ,@arguments)
	(let ((obname (string->uninterned-symbol "object")))
	  `(LET ((,obname ,object))
	     ((ACCESS ,operation (OBJECT-METHODS ,obname)) ,obname
	      ,@arguments))))))

(syntax-table-define class-syntax-table 'USUAL=>
  (macro (object operation . arguments)
    (guarantee-symbol "Operation name" operation)
    (if (not *class-name*)
	(error "Not inside class expression: USUAL=>" operation))
    `((ACCESS ,operation (CLASS-METHODS (CLASS-SUPERCLASS ,*class-name*)))
      ,object ,@arguments)))

(define (syntax-class-definition class bvl body receiver)
  (parse-definition bvl body
    (lambda (name expression)
      (receiver bvl (syntax expression)))
    (lambda (bvl body)
      (let ((operation (car bvl))
	    (self (cadr bvl)))
	(guarantee-symbol "Operation name" operation)
	(guarantee-symbol "Self name" self)
	(receiver operation
		  (syntax-class-expression class self
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

(define *class-name* #!FALSE)

(define (syntax-class-expression class-name self expression)
  (guarantee-symbol "Class name" class-name)
  (fluid-let ((*class-name* class-name))
    (transform-instance-variables
     (class-instance-transforms (name->class class-name))
     self
     (syntax* expression))))

(define (make-method-definition class operation expression)
  (make-comb (make-variable 'CLASS-METHOD-DEFINE) (make-variable class) operation expression))

(define (make-comb operator . operands)
  (make-combination operator operands))

(define (guarantee-symbol s x)
  (if (not (symbol? x))
      (error (string-append s " must be a symbol") x)))

;;; end CLASS-MACROS
))

(define make-class)
(define class?)
(define name->class)
(let ()

(set! make-class
(named-lambda (make-class name superclass variables)
  (let ((class (and (not (lexical-unreferenceable? class-descriptors name))
		    (lexical-reference class-descriptors name)))
	(object-size (if superclass
			 (+ (length variables) (class-object-size superclass))
			 (1+ (length variables))))
	(transforms (make-instance-transforms superclass variables)))
    (if (and class (eq? (class-superclass class) superclass))
	(begin (with-output-to-port console-output-port
		 (lambda ()
		   (newline) (write-string "Warning!  Redefining class ")
		   (write name)))
	       (vector-set! class 3 object-size)
	       (vector-set! class 4 transforms)
	       class)
	(let ((class
	       (vector class-tag name superclass object-size transforms
		       ;; **** MAKE-PACKAGE used here because
		       ;; MAKE-ENVIRONMENT is being flushed by the
		       ;; cross-syntaxer for no good reason.
		       (if superclass
			   (in-package (class-methods superclass)
			     (make-package methods ()))
			   ;; **** Because IN-PACKAGE NULL-ENVIRONMENT broken.
			   (make-package methods ()
			    ((access system-environment-remove-parent!
				     environment-package)
			     (the-environment)))))))
	  ((access add-unparser-special-object! unparser-package)
	   class object-unparser)
	  (local-assignment class-descriptors name class)
	  class)))))

(set! class?
(named-lambda (class? x)
  (and (vector? x)
       (not (zero? (vector-length x)))
       (eq? class-tag (vector-ref x 0)))))

(set! name->class
(named-lambda (name->class name)
  (lexical-reference class-descriptors name)))

(define class-tag "Class")

(define (make-instance-transforms superclass variables)
  (define (generate variables n tail)
    (if (null? variables)
	tail
	(cons (cons (car variables) n)
	      (generate (cdr variables) (1+ n) tail))))
  (if superclass
      (generate variables
		(class-object-size superclass)
		(class-instance-transforms superclass))
      (generate variables 1 '())))

((access add-unparser-special-object! unparser-package)
 class-tag
 (lambda (class)
   (write-string "#[Class ")
   (write (class-name class))
   (write-string "]")))

(define (object-unparser object)
  (let ((methods (object-methods object)))
    (if (lexical-unreferenceable? methods ':print-object)
	(begin (write-string "#[")
	       (write (class-name (object-class object)))
	       (write-string " ")
	       (write (primitive-datum object))
	       (write-string "]"))
	((lexical-reference methods ':print-object) object))))

(define class-descriptors
  (make-package class-descriptors ()
    ((access system-environment-remove-parent! environment-package)
     (the-environment))))

)

(declare (integrate class-name class-superclass class-object-size
		    class-instance-transforms class-methods
		    class-method usual-method))

(define (class-name class)
  (declare (integrate class))
  (vector-ref class 1))

(define (class-superclass class)
  (declare (integrate class))
  (vector-ref class 2))

(define (class-object-size class)
  (declare (integrate class))
  (vector-ref class 3))

(define (class-instance-transforms class)
  (declare (integrate class))
  (vector-ref class 4))

(define (class-methods class)
  (declare (integrate class))
  (vector-ref class 5))

(define (class-method class name)
  (declare (integrate class name))
  (lexical-reference (class-methods class) name))

(define (class-method-define class name method)
  (local-assignment (class-methods class) name method))

(define (usual-method class name)
  (declare (integrate class name))
  (class-method (class-superclass class) name))

(define (subclass? class class*)
  (define (loop class)
    (and class
	 (or (eq? class class*)
	     (loop (class-superclass class)))))
  (or (eq? class class*)
      (loop (class-superclass class))))

(declare (integrate object-class object-methods object-method))

(define (make-object class)
  (if (not (class? class)) (error "MAKE-OBJECT: Not a class" class))
  (let ((object (vector-cons (class-object-size class) #!FALSE)))
    (vector-set! object 0 class)
    object))

(define (object? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (class? (vector-ref object 0))))

(define (object-of-class? class object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? class (vector-ref object 0))))

(define (object-class object)
  (declare (integrate object))
  (vector-ref object 0))

(define (object-methods object)
  (declare (integrate object))
  (class-methods (object-class object)))

(define (object-method object name)
  (declare (integrate object name))
  (class-method (object-class object) name))

(define (send object operation . args)
  (apply (object-method object operation) object args))

(define (send-if-handles object operation . args)
  (let ((methods (object-methods object)))
    (and (not (lexical-unreferenceable? methods operation))
	 (apply (lexical-reference methods operation) object args))))

(define (send-usual class object operation . args)
  (apply (usual-method class operation) object args))