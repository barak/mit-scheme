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

;;;; Garbage Finalization
;;; package: (runtime gc-finalizer)

;;; These will cause problems on interpreted systems, due to the
;;; consing of the interpreter.  For now we'll only run this compiled.

(declare (usual-integrations))

(define-structure (gc-finalizer (constructor %make-gc-finalizer))
  (procedure #f read-only #t)
  (object? #f read-only #t)
  (object-context #f read-only #t)
  (set-object-context! #f read-only #t)
  (mutex #f read-only #t)
  items)

(define (guarantee-gc-finalizer object procedure)
  (if (not (gc-finalizer? object))
      (error:wrong-type-argument object "GC finalizer" procedure)))

(define (make-gc-finalizer procedure
			   object?
			   object-context
			   set-object-context!)
  (if (not (procedure? procedure))
      (error:wrong-type-argument procedure "procedure" 'make-gc-finalizer))
  (if (not (procedure-arity-valid? procedure 1))
      (error:bad-range-argument procedure 'make-gc-finalizer))
  (let ((finalizer
	 (%make-gc-finalizer procedure
			     object?
			     object-context
			     set-object-context!
			     (make-thread-mutex)
			     '())))
    (with-thread-mutex-lock gc-finalizers-mutex
      (lambda ()
	(set! gc-finalizers (weak-cons finalizer gc-finalizers))))
    finalizer))

(define (add-to-gc-finalizer! finalizer object)
  (guarantee-gc-finalizer finalizer 'add-to-gc-finalizer!)
  (if (not ((gc-finalizer-object? finalizer) object))
      (error:wrong-type-argument object
				 "finalized object"
				 'add-to-gc-finalizer!))
  (with-finalizer-lock finalizer
    (lambda ()
      (let ((context ((gc-finalizer-object-context finalizer) object)))
	(if (not context)
	    (error:bad-range-argument object 'add-to-gc-finalizer!))
	(set-gc-finalizer-items! finalizer
				 (cons (weak-cons object context)
				       (gc-finalizer-items finalizer))))))
  object)

(define (remove-from-gc-finalizer! finalizer object)
  (guarantee-gc-finalizer finalizer 'remove-from-gc-finalizer!)
  (let ((object? (gc-finalizer-object? finalizer)))
    (if (not (object? object))
	(error:wrong-type-argument object
				   "finalized object"
				   'remove-from-gc-finalizer!)))
  (with-finalizer-lock finalizer
    (lambda ()
      (remove-from-locked-gc-finalizer! finalizer object))))

(define (remove-from-locked-gc-finalizer! finalizer object)
  (let ((procedure (gc-finalizer-procedure finalizer))
	(object-context (gc-finalizer-object-context finalizer))
	(set-object-context! (gc-finalizer-set-object-context! finalizer)))
    (let ((context (object-context object)))
      (if (not context)
	  (error:bad-range-argument object 'remove-from-gc-finalizer!))
      (let loop ((items (gc-finalizer-items finalizer)) (prev #f))
	(if (not (pair? items))
	    (error:bad-range-argument object 'remove-from-gc-finalizer!))
	(if (eq? object (weak-car (car items)))
	    (let ((next (cdr items)))
	      (if prev
		  (set-cdr! prev next)
		  (set-gc-finalizer-items! finalizer next))
	      (set-object-context! object #f)
	      (procedure context))
	    (loop (cdr items) items))))))

(define (with-finalizer-lock finalizer thunk)
  (without-interruption
   (lambda ()
     (with-thread-mutex-lock (gc-finalizer-mutex finalizer)
       thunk))))

(define (with-gc-finalizer-lock finalizer thunk)
  (guarantee-gc-finalizer finalizer 'with-gc-finalizer-lock)
  (with-finalizer-lock finalizer thunk))

(define (remove-all-from-gc-finalizer! finalizer)
  (guarantee-gc-finalizer finalizer 'remove-all-from-gc-finalizer!)
  (let ((procedure (gc-finalizer-procedure finalizer))
	(object-context (gc-finalizer-object-context finalizer))
	(set-object-context! (gc-finalizer-set-object-context! finalizer)))
    (with-finalizer-lock finalizer
      (lambda ()
	(let loop ()
	  (let ((items (gc-finalizer-items finalizer)))
	    (if (pair? items)
		(let ((item (car items)))
		  (set-gc-finalizer-items! finalizer (cdr items))
		  (let ((object (weak-car item)))
		    (let ((context (object-context object)))
		      (if context
			  (begin
			    (if object
				(set-object-context! object #f))
			    (procedure context)))))
		  (loop)))))))))

(define (search-gc-finalizer finalizer predicate)
  (guarantee-gc-finalizer finalizer 'search-gc-finalizer)
  (with-thread-mutex-lock (gc-finalizer-mutex finalizer)
    (lambda ()
      (let loop ((items (gc-finalizer-items finalizer)))
	(and (pair? items)
	     (let ((object (weak-car (car items))))
	       (if (and object (predicate object))
		   object
		   (loop (cdr items)))))))))

(define (gc-finalizer-elements finalizer)
  (guarantee-gc-finalizer finalizer 'gc-finalizer-elements)
  (with-thread-mutex-lock (gc-finalizer-mutex finalizer)
    (lambda ()
      (let loop ((items (gc-finalizer-items finalizer)) (objects '()))
	(if (pair? items)
	    (loop (cdr items)
		  (let ((object (weak-car (car items))))
		    (if object
			(cons object objects)
			objects)))
	    (reverse! objects))))))

(define (make-gc-finalized-object finalizer get-context context->object)
  ;; A bunch of hair to permit microcode descriptors be opened with
  ;; interrupts turned on, yet not leave a dangling descriptor around
  ;; if the open is interrupted before the runtime system's data
  ;; structures are updated.
  (guarantee-gc-finalizer finalizer 'make-gc-finalized-object)
  (let ((p (weak-cons #f #f)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (get-context p)
       (let ((context (weak-cdr p)))
	 (let ((object (context->object context)))
	   (with-finalizer-lock finalizer
	     (lambda ()
	       (weak-set-car! p object)
	       (set-gc-finalizer-items!
		finalizer
		(cons p (gc-finalizer-items finalizer)))))
	   object)))
     (lambda ()
       (if (and (not (weak-pair/car? p)) (weak-cdr p))
	   (begin
	     ((gc-finalizer-procedure finalizer) (weak-cdr p))
	     (weak-set-cdr! p #f)))))))

(define gc-finalizers)
(define gc-finalizers-mutex)

(define (reset-gc-finalizers)
  (walk-gc-finalizers-list/unsafe
   (lambda (finalizer)
     (set-gc-finalizer-items! finalizer '()))))

(define (run-gc-finalizers)
  (with-thread-mutex-try-lock
   gc-finalizers-mutex
   (lambda ()
     (walk-gc-finalizers-list/unsafe
      (lambda (finalizer)
	(with-thread-mutex-try-lock
	    (gc-finalizer-mutex finalizer)
	  (lambda ()
	    (without-interruption
	     (lambda ()
	       (let ((procedure (gc-finalizer-procedure finalizer)))
		 (let loop ((items (gc-finalizer-items finalizer)) (prev #f))
		   (if (pair? items)
		       (if (weak-pair/car? (car items))
			   (loop (cdr items) items)
			   (let ((context (weak-cdr (car items)))
				 (next (cdr items)))
			     (if prev
				 (set-cdr! prev next)
				 (set-gc-finalizer-items! finalizer next))
			     (procedure context)
			     (loop next prev)))))))))
	  (lambda ()
	    unspecific)))))
   (lambda ()
     unspecific)))

(define (walk-gc-finalizers-list/unsafe procedure)
  (let loop ((finalizers gc-finalizers) (prev #f))
    (if (weak-pair? finalizers)
	(let ((finalizer (weak-car finalizers)))
	  (if finalizer
	      (begin
		(procedure finalizer)
		(loop (weak-cdr finalizers) finalizers))
	      (let ((next (weak-cdr finalizers)))
		(if prev
		    (weak-set-cdr! prev next)
		    (set! gc-finalizers next))
		(loop next prev)))))))

(define (initialize-package!)
  (set! gc-finalizers '())
  (set! gc-finalizers-mutex (make-thread-mutex))
  (add-gc-daemon! run-gc-finalizers))

(define (initialize-events!)
  (add-event-receiver! event:after-restore reset-gc-finalizers))