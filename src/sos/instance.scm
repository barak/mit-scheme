#| -*-Scheme-*-

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

;;;; Instances

(declare (usual-integrations))

;;;; Instance Constructor

;;; First define macros to be used below, because the syntaxer
;;; requires them to appear before their first reference.

(define-syntax constructor-case
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((n (cadr form))
	   (low (caddr form))
	   (high (cadddr form))
	   (generator (cddddr form))
	   (r-if (close-syntax 'IF environment))
	   (r-< (close-syntax '< environment)))
       ;; Assumes that (< LOW HIGH).
       (let loop ((low low) (high high))
	 (let ((mid (quotient (+ high low) 2)))
	   (if (= mid low)
	       `(,@generator ,low)
	       `(,r-if (,r-< ,n ,mid)
		       ,(loop low mid)
		       ,(loop mid high)))))))))

(define-syntax instance-constructor-1
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((n-slots (cadr form))
	   (r-if (close-syntax 'IF environment))
	   (r-< (close-syntax '< environment))
	   (r-cc (close-syntax 'CONSTRUCTOR-CASE environment))
	   (r-ic2 (close-syntax 'INSTANCE-CONSTRUCTOR-2 environment)))
       `(,r-if N-INIT-ARGS
	       (,r-if (,r-< N-INIT-ARGS 4)
		      (,r-cc N-INIT-ARGS 0 4 ,r-ic2 ,n-slots)
		      (,r-ic2 ,n-slots #F))
	       (,r-ic2 ,n-slots NO-INITIALIZE-INSTANCE))))))

(define-syntax instance-constructor-2
  (sc-macro-transformer
   (lambda (form environment)
     (let ((n-slots (cadr form))
	   (n-init-args (caddr form))
	   (make-names
	    (lambda (n prefix)
	      (make-initialized-list n
		(lambda (index)
		  (intern (string-append prefix (number->string index))))))))
       (call-with-values
	   (lambda ()
	     (cond ((eq? 'NO-INITIALIZE-INSTANCE n-init-args)
		    (values '() '()))
		   ((not n-init-args)
		    (values 'IVS `((APPLY INITIALIZE-INSTANCE INSTANCE IVS))))
		   (else
		    (let ((ivs (make-names n-init-args "iv")))
		      (values ivs `((INITIALIZE-INSTANCE INSTANCE ,@ivs)))))))
	 (lambda (ivs ixs)
	   (let ((generator
		  (let ((instance-length
			 (close-syntax 'INSTANCE-LENGTH environment)))
		    (lambda (initialization)
		      (let ((sis (make-names n-slots "si"))
			    (svs (make-names n-slots "sv")))
			(let ((l
			       `(LAMBDA (,@svs . ,ivs)
				  (LET ((INSTANCE
					 (OBJECT-NEW-TYPE
					  (UCODE-TYPE RECORD)
					  (MAKE-VECTOR
					   ,instance-length
					   RECORD-SLOT-UNINITIALIZED))))
				    (%RECORD-SET! INSTANCE 0
						  ,(close-syntax 'INSTANCE-TAG
								 environment))
				    ,@(map (lambda (index value)
					     `(%RECORD-SET! INSTANCE
							    ,index
							    ,value))
					   sis
					   svs)
				    ,@initialization
				    ,@ixs
				    INSTANCE))))
			  (if (null? sis)
			      l
			      `(LET (,@(make-initialized-list n-slots
					 (let ((indexes
						(close-syntax 'INDEXES
							      environment)))
					   (lambda (i)
					     `(,(list-ref sis i)
					       (LIST-REF ,indexes ,i))))))
				 ,l)))))))
		 (initialization (close-syntax 'INITIALIZATION environment)))
	     `(IF ,initialization
		  ,(generator `((,initialization INSTANCE)))
		  ,(generator '())))))))))

(define-syntax instance-constructor-3
  (sc-macro-transformer
   (lambda (form environment)
     (let ((test
	    (map (lambda (form) (close-syntax form environment))
		 (cadr form)))
	   (arity (close-syntax (caddr form) environment))
	   (initialization
	    (map (lambda (form)
		   (make-syntactic-closure environment '(INSTANCE) form))
		 (cadddr form)))
	   (ixs
	    (map (lambda (form)
		   (make-syntactic-closure environment '(INSTANCE ARGS) form))
		 (car (cddddr form))))
	   (instance-length (close-syntax 'INSTANCE-LENGTH environment))
	   (instance-tag (close-syntax 'INSTANCE-TAG environment))
	   (indexes (close-syntax 'INDEXES environment)))
       `(LETREC
	    ((PROCEDURE
	      (LAMBDA ARGS
		(IF (NOT (,@test (LENGTH ARGS)))
		    (ERROR:WRONG-NUMBER-OF-ARGUMENTS PROCEDURE ,arity ARGS))
		(LET ((INSTANCE
		       (OBJECT-NEW-TYPE
			(UCODE-TYPE RECORD)
			(MAKE-VECTOR ,instance-length
				     RECORD-SLOT-UNINITIALIZED))))
		  (%RECORD-SET! INSTANCE 0 ,instance-tag)
		  (DO ((INDEXES ,indexes (CDR INDEXES))
		       (ARGS ARGS (CDR ARGS)))
		      ((NOT (PAIR? INDEXES))
		       ,@initialization
		       ,@ixs)
		    (%RECORD-SET! INSTANCE (CAR INDEXES) (CAR ARGS)))
		  INSTANCE))))
	  PROCEDURE)))))

(define-syntax ucode-type
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply microcode-type (map strip-syntactic-closures (cdr form))))))

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
		  (fix:<= n-slots)
		  (cons n-slots #f)
		  ((initialization instance))
		  ((apply initialize-instance instance args)))
		 (instance-constructor-3
		  (fix:<= n-slots)
		  (cons n-slots #f)
		  ()
		  ((apply initialize-instance instance args)))))
	    ((< n-slots 8)
	     (constructor-case n-slots 0 8 instance-constructor-1))
	    (n-init-args
	     (let ((n-args (+ n-slots n-init-args)))
	       (if initialization
		   (instance-constructor-3
		    (fix:= n-args)
		    n-args
		    ((initialization instance))
		    ((apply initialize-instance instance args)))
		   (instance-constructor-3
		    (fix:= n-args)
		    n-args
		    ()
		    ((apply initialize-instance instance args))))))
	    (initialization
	     (instance-constructor-3 (fix:= n-slots)
				     n-slots
				     ((initialization instance))
				     ()))
	    (else
	     (instance-constructor-3 (fix:= n-slots)
				     n-slots
				     ()
				     ()))))))

(define-syntax make-initialization-1
  (rsc-macro-transformer
   (lambda (form environment)
     (let ((if-n (cadr form))
	   (r-if (close-syntax 'IF environment))
	   (r-< (close-syntax '< environment))
	   (r-cc (close-syntax 'CONSTRUCTOR-CASE environment))
	   (r-mi2 (close-syntax 'MAKE-INITIALIZATION-2 environment)))
       `(,r-if (,r-< IV-N 8)
	       (,r-cc IV-N 0 8 ,r-mi2 ,if-n)
	       (,r-mi2 ,if-n #F))))))

(define-syntax make-initialization-2
  (sc-macro-transformer
   (lambda (form environment)
     (let ((if-n (close-syntax (cadr form) environment))
	   (iv-n (close-syntax (caddr form) environment))
	   (if-indexes (close-syntax 'IF-INDEXES environment))
	   (initializers (close-syntax 'INITIALIZERS environment))
	   (iv-indexes (close-syntax 'IV-INDEXES environment))
	   (initial-values (close-syntax 'INITIAL-VALUES environment)))
       (if (and if-n iv-n)
	   (let ((generate
		  (let ((make-names
			 (lambda (n prefix)
			   (make-initialized-list n
			     (lambda (index)
			       (intern
				(string-append prefix
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
		 (generate if-n "f" if-indexes initializers
			   (lambda (expr) `(,expr))))
	     (lambda (if-bindings if-body)
	       (call-with-values
		   (lambda ()
		     (generate iv-n "v" iv-indexes initial-values
			       (lambda (expr) expr)))
		 (lambda (iv-bindings iv-body)
		   (if (and (null? if-bindings) (null? iv-bindings))
		       '#F
		       `(LET (,@if-bindings ,@iv-bindings)
			  (LAMBDA (INSTANCE)
			    ,@if-body
			    ,@iv-body))))))))
	   `(LAMBDA (INSTANCE)
	      (DO ((IS ,if-indexes (CDR IS))
		   (VS ,initializers (CDR VS)))
		  ((NULL? IS) UNSPECIFIC)
		(%RECORD-SET! INSTANCE (CAR IS) ((CAR VS))))
	      (DO ((IS ,iv-indexes (CDR IS))
		   (VS ,initial-values (CDR VS)))
		  ((NULL? IS) UNSPECIFIC)
		(%RECORD-SET! INSTANCE (CAR IS) (CAR VS)))))))))

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