;;; -*-Scheme-*-
;;;
;;; $Id: instance.scm,v 1.4 1997/06/15 06:41:34 cph Exp $
;;;
;;; Copyright (c) 1995-97 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

;;;; Instances

(declare (usual-integrations))

;;;; Instance Constructor

;;; First define macros to be used below, because the syntaxer
;;; requires them to appear before their first reference.

(define-macro (constructor-case nx low high fixed default)
  `(CASE ,nx
     ,@(let loop ((i low))
	 (if (= i high)
	     '()
	     `(((,i) (,fixed ,i))
	       ,@(loop (+ i 1)))))
     (ELSE ,default)))

(define-macro (fixed-if-initialization n)
  (let ((indexes
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "n" (number->string index))))))
	(initializers
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "i" (number->string index)))))))
    `(LET (,@(make-initialized-list n
	       (lambda (index)
		 `(,(list-ref indexes index)
		   (LIST-REF INDEXES ,index))))
	   ,@(make-initialized-list n
	       (lambda (index)
		 `(,(list-ref initializers index)
		   (LIST-REF INITIALIZERS ,index)))))
       (LAMBDA (INSTANCE)
	 ,@(map (lambda (index initializer)
		  `(%RECORD-SET! INSTANCE ,index (,initializer)))
		indexes
		initializers)))))

(define-macro (fixed-iv-initialization n)
  (let ((indexes
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "n" (number->string index))))))
	(initial-values
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "i" (number->string index)))))))
    `(LET (,@(make-initialized-list n
	       (lambda (index)
		 `(,(list-ref indexes index)
		   (LIST-REF INDEXES ,index))))
	   ,@(make-initialized-list n
	       (lambda (index)
		 `(,(list-ref initial-values index)
		   (LIST-REF INITIAL-VALUES ,index)))))
       (LAMBDA (INSTANCE)
	 ,@(map (lambda (index initial-value)
		  `(%RECORD-SET! INSTANCE ,index ,initial-value))
		indexes
		initial-values)))))

(define-macro (fixed-arity-constructor n)
  (let ((indexes
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "i" (number->string index))))))
	(values
	 (make-initialized-list n
	   (lambda (index)
	     (intern (string-append "v" (number->string index)))))))
    `(LET ,(make-initialized-list n
	     (lambda (index)
	       `(,(list-ref indexes index)
		 (LIST-REF INDEXES ,index))))
       ,(let loop
	    ((alist '((IF-INIT . IF-INIT)
		      (IV-INIT . IV-INIT)
		      (CALL-INIT-INSTANCE? . INITIALIZE-INSTANCE)))
	     (exprs '()))
	  (if (null? alist)
	      `(LAMBDA ,values
		 (LET ((INSTANCE
			(OBJECT-NEW-TYPE
			 (UCODE-TYPE RECORD)
			 (MAKE-VECTOR INSTANCE-LENGTH
				      RECORD-SLOT-UNINITIALIZED))))
		   (%RECORD-SET! INSTANCE 0 INSTANCE-TAG)
		   ,@(map (lambda (index value)
			    `(%RECORD-SET! INSTANCE ,index ,value))
			  indexes
			  values)
		   ,@(reverse exprs)
		   INSTANCE))
	      `(IF ,(caar alist)
		   ,(loop (cdr alist) `((,(cdar alist) INSTANCE) ,@exprs))
		   ,(loop (cdr alist) exprs)))))))

(define (instance-constructor class slot-names #!optional call-init-instance?)
  (let ((slots (map (lambda (name) (class-slot class name #t)) slot-names))
	(call-init-instance?
	 (if (default-object? call-init-instance?) #f call-init-instance?))
	(instance-length (fix:+ (length (class-slots class)) 1))
	(instance-tag (class->dispatch-tag class)))
    (let ((n-values (length slots))
	  (if-init (make-if-initialization class slots))
	  (iv-init (make-iv-initialization class slots)))
      (let ((indexes (map slot-index slots)))
	(constructor-case n-values 0 8 fixed-arity-constructor
	  (letrec
	      ((procedure
		(lambda values
		  (if (not (fix:= n-values (length values)))
		      (error:wrong-number-of-arguments procedure
						       n-values values))
		  (let ((instance
			 (object-new-type
			  (ucode-type record)
			  (make-vector instance-length
				       record-slot-uninitialized))))
		    (%record-set! instance 0 instance-tag)
		    (if if-init (if-init instance))
		    (if iv-init (iv-init instance))
		    (if call-init-instance? (initialize-instance instance))
		    (do ((indexes indexes (cdr indexes))
			 (values values (cdr values)))
			((null? indexes))
		      (%record-set! instance (car indexes) (car values)))
		    instance))))
	    procedure))))))

(define (make-if-initialization class arg-slots)
  (let ((slots
	 (list-transform-positive (class-slots class)
	   (lambda (slot)
	     (and (slot-initializer slot)
		  (not (memq slot arg-slots)))))))
    (and (not (null? slots))
	 (let ((indexes (map slot-index slots))
	       (initializers (map slot-initializer slots)))
	   (constructor-case (length slots) 1 8 fixed-if-initialization
	     (lambda (instance)
	       (do ((initializers initializers (cdr initializers))
		    (indexes indexes (cdr indexes)))
		   ((null? initializers) unspecific)
		 (%record-set! instance
			       (car indexes)
			       ((car initializers))))))))))

(define (make-iv-initialization class arg-slots)
  (let ((slots
	 (list-transform-positive (class-slots class)
	   (lambda (slot)
	     (and (slot-initial-value? slot)
		  (not (memq slot arg-slots)))))))
    (and (not (null? slots))
	 (let ((indexes (map slot-index slots))
	       (initial-values (map slot-initial-value slots)))
	   (constructor-case (length slots) 1 8 fixed-iv-initialization
	     (lambda (instance)
	       (do ((initial-values initial-values (cdr initial-values))
		    (indexes indexes (cdr indexes)))
		   ((null? initial-values) unspecific)
		 (%record-set! instance
			       (car indexes)
			       (car initial-values)))))))))

(define initialize-instance
  (make-generic-procedure 1 'INITIALIZE-INSTANCE))

(define (instance? object)
  (and (tagged-vector? object)
       (class? (dispatch-tag-contents (tagged-vector-tag object)))))

(define (instance-class instance)
  (dispatch-tag-contents (tagged-vector-tag instance)))

(define (instance-predicate class)
  (if (not (class? class))
      (error:wrong-type-argument class "class" 'INSTANCE-PREDICATE))
  (let ((predicate (make-generic-procedure 1)))
    (let ((add
	   (lambda (c v)
	     (add-method predicate
			 (make-method (list c) (lambda (object) object v))))))
      (add <object> #f)
      (add class #t))
    predicate))

(define (instance-of? object class)
  (and (subclass? (object-class object) class)
       #t))