;;; -*-Scheme-*-
;;;
;;; $Id: instance.scm,v 1.11 2001/12/20 03:13:05 cph Exp $
;;;
;;; Copyright (c) 1995-2000 Massachusetts Institute of Technology
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

;;;; Instances

(declare (usual-integrations))

;;;; Instance Constructor

;;; First define macros to be used below, because the syntaxer
;;; requires them to appear before their first reference.

(define-macro (constructor-case n low high generator . generator-args)
  ;; Assumes that (< LOW HIGH).
  (let loop ((low low) (high high))
    (let ((mid (quotient (+ high low) 2)))
      (if (= mid low)
	  `(,generator ,@generator-args ,low)
	  `(IF (< ,n ,mid)
	       ,(loop low mid)
	       ,(loop mid high))))))

(define-macro (instance-constructor-1 n-slots)
  `(IF N-INIT-ARGS
       (IF (< N-INIT-ARGS 4)
	   (CONSTRUCTOR-CASE N-INIT-ARGS 0 4 INSTANCE-CONSTRUCTOR-2 ,n-slots)
	   (INSTANCE-CONSTRUCTOR-2 ,n-slots #F))
       (INSTANCE-CONSTRUCTOR-2 ,n-slots NO-INITIALIZE-INSTANCE)))

(define-macro (instance-constructor-2 n-slots n-init-args)
  (let ((make-names
	 (lambda (n prefix)
	   (make-initialized-list n
	     (lambda (index)
	       (intern (string-append prefix (number->string index))))))))
    (call-with-values
	(lambda ()
	  (cond ((eq? 'NO-INITIALIZE-INSTANCE n-init-args)
		 (values '() '()))
		(n-init-args
		 (let ((ivs (make-names n-init-args "iv")))
		   (values ivs `((INITIALIZE-INSTANCE INSTANCE ,@ivs)))))
		(else
		 (values 'IVS `((APPLY INITIALIZE-INSTANCE INSTANCE IVS))))))
      (lambda (ivs ixs)
	(let ((generator
	       (lambda (initialization)
		 (let ((sis (make-names n-slots "si"))
		       (svs (make-names n-slots "sv")))
		   (let ((l
			  `(LAMBDA (,@svs . ,ivs)
			     (LET ((INSTANCE
				    (OBJECT-NEW-TYPE
				     (UCODE-TYPE RECORD)
				     (MAKE-VECTOR INSTANCE-LENGTH
						  RECORD-SLOT-UNINITIALIZED))))
			       (%RECORD-SET! INSTANCE 0 INSTANCE-TAG)
			       ,@(map (lambda (index value)
					`(%RECORD-SET! INSTANCE ,index ,value))
				      sis
				      svs)
			       ,@initialization
			       ,@ixs
			       INSTANCE))))
		     (if (null? sis)
			 l
			 `(LET (,@(make-initialized-list n-slots
				    (lambda (i)
				      `(,(list-ref sis i)
					(LIST-REF INDEXES ,i)))))
			    ,l)))))))
	  `(IF INITIALIZATION
	       ,(generator '((INITIALIZATION INSTANCE)))
	       ,(generator '())))))))

(define-macro (ucode-type . arguments)
  (apply microcode-type arguments))

(define-macro (instance-constructor-3 test arity initialization ixs)
  `(LETREC
       ((PROCEDURE
	 (LAMBDA ARGS
	   (IF (NOT (,@test (LENGTH ARGS)))
	       (ERROR:WRONG-NUMBER-OF-ARGUMENTS PROCEDURE ,arity ARGS))
	   (LET ((INSTANCE
		  (OBJECT-NEW-TYPE
		   (UCODE-TYPE RECORD)
		   (MAKE-VECTOR INSTANCE-LENGTH
				RECORD-SLOT-UNINITIALIZED))))
	     (%RECORD-SET! INSTANCE 0 INSTANCE-TAG)
	     (DO ((INDEXES INDEXES (CDR INDEXES))
		  (ARGS ARGS (CDR ARGS)))
		 ((NULL? INDEXES)
		  ,@initialization
		  ,@ixs)
	       (%RECORD-SET! INSTANCE (CAR INDEXES) (CAR ARGS)))
	     INSTANCE))))
     PROCEDURE))

(define (instance-constructor class slot-names #!optional init-arg-names)
  (if (not (subclass? class <instance>))
      (error:bad-range-argument class 'INSTANCE-CONSTRUCTOR))
  (let ((slots (map (lambda (name) (class-slot class name #t)) slot-names))
	(n-init-args
	 (cond ((or (default-object? init-arg-names)
		    (eq? #t init-arg-names))
		#t)
	       ((or (eq? 'NO-INIT init-arg-names)
		    (eq? 'NO-INITIALIZE-INSTANCE init-arg-names))
		#f)
	       ((and (list? init-arg-names)
		     (for-all? init-arg-names symbol?))
		(length init-arg-names))
	       ((exact-nonnegative-integer? init-arg-names)
		init-arg-names)
	       (else
		(error:bad-range-argument init-arg-names
					  'INSTANCE-CONSTRUCTOR))))
	(instance-length (+ (length (class-slots class)) 1))
	(instance-tag (class->dispatch-tag class)))
    (let ((n-slots (length slots))
	  (indexes (map slot-index slots))
	  (initialization (make-initialization class slots)))
      (cond ((eq? #t n-init-args)
	     (if initialization
		 (instance-constructor-3
		  (fix:<= n-slots) (cons n-slots #f)
		  ((initialization instance))
		  ((apply initialize-instance instance args)))
		 (instance-constructor-3
		  (fix:<= n-slots) (cons n-slots #f)
		  ()
		  ((apply initialize-instance instance args)))))
	    ((< n-slots 8)
	     (constructor-case n-slots 0 8 instance-constructor-1))
	    (n-init-args
	     (let ((n-args (+ n-slots n-init-args)))
	       (if initialization
		   (instance-constructor-3
		    (fix:= n-args) n-args
		    ((initialization instance))
		    ((apply initialize-instance instance args)))
		   (instance-constructor-3
		    (fix:= n-args) n-args
		    ()
		    ((apply initialize-instance instance args))))))
	    (initialization
	     (instance-constructor-3 (fix:= n-slots) n-slots
				     ((initialization instance))
				     ()))
	    (else
	     (instance-constructor-3 (fix:= n-slots) n-slots () ()))))))

(define-macro (make-initialization-1 if-n)
  `(IF (< IV-N 8)
       (CONSTRUCTOR-CASE IV-N 0 8 MAKE-INITIALIZATION-2 ,if-n)
       (MAKE-INITIALIZATION-2 ,if-n #F)))

(define-macro (make-initialization-2 if-n iv-n)
  (if (and if-n iv-n)
      (let ((generate
	     (let ((make-names
		    (lambda (n prefix)
		      (make-initialized-list n
			(lambda (index)
			  (intern (string-append prefix
						 (number->string index))))))))
	       (lambda (n prefix isn vsn fv)
		 (let ((is (make-names n (string-append prefix "i")))
		       (vs (make-names n (string-append prefix "v"))))
		   (values
		    (append (make-initialized-list n
			      (lambda (i)
				`(,(list-ref is i) (LIST-REF ,isn ,i))))
			    (make-initialized-list n
			      (lambda (i)
				`(,(list-ref vs i) (LIST-REF ,vsn ,i)))))
		    (make-initialized-list n
		      (lambda (i)
			`(%RECORD-SET! INSTANCE
				       ,(list-ref is i)
				       ,(fv (list-ref vs i)))))))))))

      (call-with-values
	  (lambda ()
	    (generate if-n "f" 'IF-INDEXES 'INITIALIZERS
		      (lambda (expr) `(,expr))))
	(lambda (if-bindings if-body)
	  (call-with-values
	      (lambda ()
		(generate iv-n "v" 'IV-INDEXES 'INITIAL-VALUES
			  (lambda (expr) expr)))
	    (lambda (iv-bindings iv-body)
	      (if (and (null? if-bindings) (null? iv-bindings))
		  '#F
		  `(LET (,@if-bindings ,@iv-bindings)
		     (LAMBDA (INSTANCE)
		       ,@if-body
		       ,@iv-body))))))))
      `(LAMBDA (INSTANCE)
	 (DO ((IS IF-INDEXES (CDR IS))
	      (VS INITIALIZERS (CDR VS)))
	     ((NULL? IS) UNSPECIFIC)
	   (%RECORD-SET! INSTANCE (CAR IS) ((CAR VS))))
	 (DO ((IS IV-INDEXES (CDR IS))
	      (VS INITIAL-VALUES (CDR VS)))
	     ((NULL? IS) UNSPECIFIC)
	   (%RECORD-SET! INSTANCE (CAR IS) (CAR VS))))))

(define (make-initialization class arg-slots)
  (let ((if-slots
	 (list-transform-positive (class-slots class)
	   (lambda (slot)
	     (and (slot-initializer slot)
		  (not (memq slot arg-slots))))))
	(iv-slots
	 (list-transform-positive (class-slots class)
	   (lambda (slot)
	     (and (slot-initial-value? slot)
		  (not (memq slot arg-slots)))))))
    (let ((if-n (length if-slots))
	  (iv-n (length iv-slots))
	  (if-indexes (map slot-index if-slots))
	  (initializers (map slot-initializer if-slots))
	  (iv-indexes (map slot-index iv-slots))
	  (initial-values (map slot-initial-value iv-slots)))
      (if (< if-n 4)
	  (constructor-case if-n 0 4 make-initialization-1)
	  (make-initialization-1 #f)))))

(define initialize-instance
  (make-generic-procedure '(1 . #F) 'INITIALIZE-INSTANCE))

(define (instance? object)
  (and (tagged-vector? object)
       (class? (dispatch-tag-contents (tagged-vector-tag object)))))

(define (instance-class instance)
  (dispatch-tag-contents (tagged-vector-tag instance)))

(define (instance-predicate specializer)
  (if (not (specializer? specializer))
      (error:wrong-type-argument specializer "specializer"
				 'INSTANCE-PREDICATE))
  (let ((predicate (make-generic-procedure 1)))
    (let ((add
	   (lambda (c v)
	     (add-method predicate
			 (make-method (list c) (lambda (object) object v))))))
      (add <object> #f)
      (add specializer #t))
    predicate))

(define (instance-of? object specializer)
  (subclass? (object-class object) specializer))