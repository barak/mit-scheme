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
  (items '()))

(define (guarantee-gc-finalizer object procedure)
  (if (not (gc-finalizer? object))
      (error:wrong-type-argument object "GC finalizer" procedure)))

(define (make-gc-finalizer procedure
			   object?
			   object-context
			   set-object-context!)
  (if (not (procedure? procedure))
      (error:wrong-type-argument procedure "procedure" 'MAKE-GC-FINALIZER))
  (if (not (procedure-arity-valid? procedure 1))
      (error:bad-range-argument procedure 'MAKE-GC-FINALIZER))
  (let ((finalizer
	 (%make-gc-finalizer procedure
			     object?
			     object-context
			     set-object-context!
			     '())))
    (set! gc-finalizers (weak-cons finalizer gc-finalizers))
    finalizer))

(define (add-to-gc-finalizer! finalizer object)
  (guarantee-gc-finalizer finalizer 'ADD-TO-GC-FINALIZER!)
  (if (not ((gc-finalizer-object? finalizer) object))
      (error:wrong-type-argument object
				 "finalized object"
				 'ADD-TO-GC-FINALIZER!))
  (without-interrupts
   (lambda ()
     (let ((context ((gc-finalizer-object-context finalizer) object)))
       (if (not context)
	   (error:bad-range-argument object 'ADD-TO-GC-FINALIZER!))
       (set-gc-finalizer-items! finalizer
				(cons (weak-cons object context)
				      (gc-finalizer-items finalizer))))))
  object)

(define (remove-from-gc-finalizer! finalizer object)
  (guarantee-gc-finalizer finalizer 'REMOVE-FROM-GC-FINALIZER!)
  (let ((procedure (gc-finalizer-procedure finalizer))
	(object? (gc-finalizer-object? finalizer))
	(object-context (gc-finalizer-object-context finalizer))
	(set-object-context! (gc-finalizer-set-object-context! finalizer)))
    (if (not (object? object))
	(error:wrong-type-argument object
				   "finalized object"
				   'REMOVE-FROM-GC-FINALIZER!))
    (without-interrupts
     (lambda ()
       (let ((context (object-context object)))
	 (if (not context)
	     (error:bad-range-argument object 'REMOVE-FROM-GC-FINALIZER!))
	 (let loop ((items (gc-finalizer-items finalizer)) (prev #f))
	   (if (not (pair? items))
	       (error:bad-range-argument object 'REMOVE-FROM-GC-FINALIZER!))
	   (if (eq? object (weak-car (car items)))
	       (let ((next (cdr items)))
		 (if prev
		     (set-cdr! prev next)
		     (set-gc-finalizer-items! finalizer next))
		 (set-object-context! object #f)
		 (procedure context))
	       (loop (cdr items) items))))))))

(define (remove-all-from-gc-finalizer! finalizer)
  (guarantee-gc-finalizer finalizer 'REMOVE-ALL-FROM-GC-FINALIZER!)
  (let ((procedure (gc-finalizer-procedure finalizer))
	(object-context (gc-finalizer-object-context finalizer))
	(set-object-context! (gc-finalizer-set-object-context! finalizer)))
    (without-interrupts
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
  (guarantee-gc-finalizer finalizer 'SEARCH-GC-FINALIZER)
  (without-interrupts
   (lambda ()
     (let loop ((items (gc-finalizer-items finalizer)))
       (and (pair? items)
	    (let ((object (weak-car (car items))))
	      (if (and object (predicate object))
		  object
		  (loop (cdr items)))))))))

(define (gc-finalizer-elements finalizer)
  (guarantee-gc-finalizer finalizer 'GC-FINALIZER-ELEMENTS)
  (without-interrupts
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
  (guarantee-gc-finalizer finalizer 'MAKE-GC-FINALIZED-OBJECT)
  (let ((p (weak-cons #f #f)))
    (dynamic-wind
     (lambda () unspecific)
     (lambda ()
       (get-context p)
       (let ((context (weak-cdr p)))
	 (let ((object (context->object context)))
	   (without-interrupts
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

(define (reset-gc-finalizers)
  (without-interrupts
   (lambda ()
     (walk-gc-finalizers-list
      (lambda (finalizer)
	(set-gc-finalizer-items! finalizer '()))))))

(define (run-gc-finalizers)
  (walk-gc-finalizers-list
   (lambda (finalizer)
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

(define (walk-gc-finalizers-list procedure)
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
  (add-gc-daemon! run-gc-finalizers))

(define (initialize-events!)
  (add-event-receiver! event:after-restore reset-gc-finalizers))