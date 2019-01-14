#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Generic Procedures
;;; package: (sos generic-procedure)

(declare (usual-integrations))

;;;; Generic Procedures

(define (make-generic-procedure arity #!optional name tag generator)
  (let ((name (if (default-object? name) #f name))
	(tag (if (default-object? tag) #f tag))
	(generator (if (default-object? generator) #f generator)))
    (if (and name (not (symbol? name)))
	(error:wrong-type-argument name "symbol" 'MAKE-GENERIC-PROCEDURE))
    (if tag (guarantee dispatch-tag? tag 'MAKE-GENERIC-PROCEDURE))
    (guarantee procedure-arity? arity 'MAKE-GENERIC-PROCEDURE)
    (if (not (fix:> (procedure-arity-min arity) 0))
	(error:bad-range-argument arity 'MAKE-GENERIC-PROCEDURE))
    (guarantee-generator generator 'MAKE-GENERIC-PROCEDURE)
    (let ((record
	   (make-generic-record (predicate->dispatch-tag generic-procedure?)
				(procedure-arity-min arity)
				(procedure-arity-max arity)
				generator
				name
				(new-cache (procedure-arity-min arity)))))
      (let ((generic (compute-apply-generic record)))
	(set-generic-record/procedure! record generic)
	(with-thread-mutex-lock generic-procedure-records-mutex
	  (lambda ()
	    (eqht/put! generic-procedure-records generic record)))
	generic))))

(define-structure (generic-record
		   (conc-name generic-record/)
		   (constructor make-generic-record
				(tag arity-min arity-max generator name
				     cache)))
  (tag #f read-only #t)
  (arity-min #f read-only #t)
  (arity-max #f read-only #t)
  (generator #f)
  (name #f read-only #t)
  cache
  procedure)

(define (generic-procedure? object)
  (with-thread-mutex-lock generic-procedure-records-mutex
    (lambda ()
      (if (eqht/get generic-procedure-records object #f) #t #f))))
(register-predicate! generic-procedure? 'generic-procedure '<= procedure?)

(define (generic-record/arity record)
  (make-procedure-arity (generic-record/arity-min record)
			(generic-record/arity-max record)
			#t))

(define (generic-procedure-arity generic)
  (generic-record/arity
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY)))

(define (generic-procedure-arity-min generic)
  (generic-record/arity-min
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY-MIN)))

(define (generic-procedure-arity-max generic)
  (generic-record/arity-max
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-ARITY-MAX)))

(define (generic-procedure-name generic)
  (generic-record/name
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-NAME)))

(define (generic-procedure-generator generic)
  (generic-record/generator
   (guarantee-generic-procedure generic 'GENERIC-PROCEDURE-GENERATOR)))

(define (set-generic-procedure-generator! generic generator)
  (let ((record
	 (guarantee-generic-procedure generic
				      'SET-GENERIC-PROCEDURE-GENERATOR!)))
    (guarantee-generator generator 'SET-GENERIC-PROCEDURE-GENERATOR!)
    (without-interruption
     (lambda ()
       (set-generic-record/generator! record generator)
       (%reset-generic-procedure-cache! record)))))

(define (purge-generic-procedure-cache generic #!optional filter)
  (let ((operator
	 (if (or (default-object? filter)
		 (eq? 'ALL-ENTRIES filter))
	     (lambda (generic record)
	       generic
	       (%reset-generic-procedure-cache! record))
	     (lambda (generic record)
	       (%purge-generic-procedure-cache! generic record filter)))))
    (if (eq? 'ALL-PROCEDURES generic)
	(with-thread-mutex-lock generic-procedure-records-mutex
	  (lambda ()
	    (eqht/for-each generic-procedure-records operator)))
	(operator
	 generic
	 (guarantee-generic-procedure generic
				      'PURGE-GENERIC-PROCEDURE-CACHE)))))

(define (%reset-generic-procedure-cache! record)
  (set-generic-record/cache! record
			     (new-cache (generic-record/arity-min record))))

(define (%purge-generic-procedure-cache! generic record filter)
  ;; This might have events blocked for a long time, and thus might
  ;; benefit by using something like a semaphore to control access.
  (without-interruption
   (lambda ()
     (set-generic-record/cache!
      record
      (purge-cache-entries (generic-record/cache record)
			   (lambda (tags) (filter generic tags)))))))

(define (guarantee-generic-procedure generic caller)
  (or (with-thread-mutex-lock generic-procedure-records-mutex
	(lambda ()
	  (eqht/get generic-procedure-records generic #f)))
      (error:wrong-type-argument generic "generic procedure" caller)))

(define (guarantee-generator generator caller)
  (if (not (or (not generator)
	       (and (procedure? generator)
		    (procedure-arity-valid? generator 2))))
      (error:wrong-type-argument generator
				 "generic procedure generator"
				 caller)))

;;;; Generic Procedure Application

(define (compute-apply-generic record)
  (let ((arity (generic-record/arity-min record)))
    (if (and (eqv? arity (generic-record/arity-max record))
	     (fix:<= arity 4))
	(case arity
	  ((1) (apply-generic-1 record))
	  ((2) (apply-generic-2 record))
	  ((3) (apply-generic-3 record))
	  (else (apply-generic-4 record)))
	(apply-generic record))))

(define (apply-generic record)
  (let ((arity-min (generic-record/arity-min record))
	(arity-max (generic-record/arity-max record)))
    (let ((extra (and arity-max (fix:- arity-max arity-min))))
      (letrec
	  ((generic
	    (lambda args
	      (let loop ((args* args) (n arity-min) (tags '()))
		(if (fix:= n 0)
		    (begin
		      (if (and extra
			       (let loop ((args* args*) (n extra))
				 (and (pair? args*)
				      (or (fix:= n 0)
					  (loop (cdr args*)
						(fix:- n 1))))))
			  (wna args))
		      (let ((procedure
			     (probe-cache (generic-record/cache record)
					  tags)))
			(if procedure
			    (apply procedure args)
			    (compute-method-and-store record args))))
		    (begin
		      (if (not (pair? args*))
			  (wna args))
		      (loop (cdr args*)
			    (fix:- n 1)
			    (cons (object->dispatch-tag (car args*)) tags)))))))
	   (wna
	    (lambda (args)
	      (error:wrong-number-of-arguments generic
					       (generic-record/arity record)
					       args))))
	generic))))

(define (generic-procedure-applicable? procedure arguments)
  (let ((record
	 (guarantee-generic-procedure procedure
				      'GENERIC-PROCEDURE-APPLICABLE?))
	(tags (map object->dispatch-tag arguments)))
    (let ((generator (generic-record/generator record))
	  (arity-min (generic-record/arity-min record))
	  (arity-max (generic-record/arity-max record))
	  (n-args (length tags)))
      (and generator
	   (if (fix:= n-args arity-min)
	       (generator procedure tags)
	       (and (fix:> n-args arity-min)
		    (or (not arity-max)
			(fix:<= n-args arity-max))
		    (generator procedure (list-head tags arity-min))))))))

(define (apply-generic-1 record)
  (lambda (a1)
    (let ((procedure
	   (probe-cache-1 (generic-record/cache record)
			  (object->dispatch-tag a1))))
      (if procedure
	  (procedure a1)
	  (compute-method-and-store record (list a1))))))

(define (apply-generic-2 record)
  (lambda (a1 a2)
    (let ((procedure
	   (probe-cache-2 (generic-record/cache record)
			  (object->dispatch-tag a1)
			  (object->dispatch-tag a2))))
      (if procedure
	  (procedure a1 a2)
	  (compute-method-and-store record (list a1 a2))))))

(define (apply-generic-3 record)
  (lambda (a1 a2 a3)
    (let ((procedure
	   (probe-cache-3 (generic-record/cache record)
			  (object->dispatch-tag a1)
			  (object->dispatch-tag a2)
			  (object->dispatch-tag a3))))
      (if procedure
	  (procedure a1 a2 a3)
	  (compute-method-and-store record (list a1 a2 a3))))))

(define (apply-generic-4 record)
  (lambda (a1 a2 a3 a4)
    (let ((procedure
	   (probe-cache-4 (generic-record/cache record)
			  (object->dispatch-tag a1)
			  (object->dispatch-tag a2)
			  (object->dispatch-tag a3)
			  (object->dispatch-tag a4))))
      (if procedure
	  (procedure a1 a2 a3 a4)
	  (compute-method-and-store record (list a1 a2 a3 a4))))))

(define (compute-method-and-store record args)
  (let ((tags
	 (let ((p (list 'TAGS)))
	   (do ((args args (cdr args))
		(p p (cdr p))
		(i (generic-record/arity-min record) (fix:- i 1)))
	       ((not (fix:> i 0)))
	     (set-cdr! p (list (object->dispatch-tag (car args)))))
	   (cdr p))))
    (let ((procedure
	   (let ((generator (generic-record/generator record))
		 (generic (generic-record/procedure record)))
	     (or (and generator (generator generic tags))
		 (error:no-applicable-methods generic args)))))
      (without-interruption
       (lambda ()
	 (set-generic-record/cache!
	  record
	  (fill-cache (generic-record/cache record) tags procedure))))
      (apply procedure args))))

(define generic-procedure-records (make-eqht))
(define generic-procedure-records-mutex (make-thread-mutex))

(define condition-type:no-applicable-methods
  (make-condition-type 'NO-APPLICABLE-METHODS condition-type:error
		       '(OPERATOR OPERANDS)
    (lambda (condition port)
      (write-string "No applicable methods for " port)
      (write (access-condition condition 'OPERATOR) port)
      (write-string " with these arguments: " port)
      (write (access-condition condition 'OPERANDS) port)
      (write-string "." port))))

(define error:no-applicable-methods
  (condition-signaller condition-type:no-applicable-methods
		       '(OPERATOR OPERANDS)
		       standard-error-handler))