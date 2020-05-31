#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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
  (items #f read-only #t))

(define (make-gc-finalizer procedure
			   object?
			   object-context
			   set-object-context!)
  (guarantee unary-procedure? procedure 'make-gc-finalizer)
  (let ((finalizer
	 (%make-gc-finalizer procedure
			     object?
			     object-context
			     set-object-context!
			     (make-thread-mutex)
			     (weak-alist-table eq? procedure))))
    (with-thread-mutex-lock gc-finalizers-mutex
      (lambda ()
	(weak-list-set-add! finalizer gc-finalizers)))
    finalizer))

(define (add-to-gc-finalizer! finalizer object)
  (guarantee gc-finalizer? finalizer 'add-to-gc-finalizer!)
  (guarantee (gc-finalizer-object? finalizer) object 'add-to-gc-finalizer!)
  (with-finalizer-lock finalizer
    (lambda ()
      (let ((context ((gc-finalizer-object-context finalizer) object)))
	(if (not context)
	    (error:bad-range-argument object 'add-to-gc-finalizer!))
	(weak-alist-table-set! (gc-finalizer-items finalizer) object context))))
  object)

(define (remove-from-gc-finalizer! finalizer object)
  (guarantee gc-finalizer? finalizer 'remove-from-gc-finalizer!)
  (guarantee (gc-finalizer-object? finalizer) object 'remove-from-gc-finalizer!)
  (with-finalizer-lock finalizer
    (lambda ()
      (remove-from-locked-gc-finalizer! finalizer object))))

(define (remove-from-locked-gc-finalizer! finalizer object)
  (let ((context
	 (weak-alist-table-delete! (gc-finalizer-items finalizer) object #f)))
    (if (not context)
	(error:bad-range-argument object 'remove-from-gc-finalizer!))
    ((gc-finalizer-set-object-context! finalizer) object #f)
    ((gc-finalizer-procedure finalizer) context)))

(define (with-gc-finalizer-lock finalizer thunk)
  (guarantee gc-finalizer? finalizer 'with-gc-finalizer-lock)
  (with-finalizer-lock finalizer thunk))

(define-integrable (with-finalizer-lock finalizer thunk)
  (without-interruption
   (lambda ()
     (with-thread-mutex-lock (gc-finalizer-mutex finalizer)
       thunk))))

(define (remove-all-from-gc-finalizer! finalizer)
  (guarantee gc-finalizer? finalizer 'remove-all-from-gc-finalizer!)
  (let ((object-context (gc-finalizer-object-context finalizer))
	(set-object-context! (gc-finalizer-set-object-context! finalizer))
	(procedure (gc-finalizer-procedure finalizer)))
    (with-finalizer-lock finalizer
      (lambda ()
	(weak-alist-table-prune! (lambda (object context)
				   (declare (ignore context))
				   (let ((context (object-context object)))
				     (if context
					 (begin
					   (set-object-context! object #f)
					   (procedure context))))
				   #t)
				 (gc-finalizer-items finalizer))))))

(define (search-gc-finalizer finalizer predicate)
  (guarantee gc-finalizer? finalizer 'search-gc-finalizer)
  (with-finalizer-lock finalizer
    (lambda ()
      (weak-alist-table-search (gc-finalizer-items finalizer) predicate
	(lambda (object context) (declare (ignore context)) object)
	(lambda () #f)))))

(define (gc-finalizer-elements finalizer)
  (guarantee gc-finalizer? finalizer 'gc-finalizer-elements)
  (with-finalizer-lock finalizer
    (lambda ()
      (%gc-finalizer-elements finalizer))))

(define (%gc-finalizer-elements finalizer)
  (weak-alist-table-fold-right (lambda (key value acc)
				 (declare (ignore value))
				 (cons key acc))
			       '()
			       (gc-finalizer-items finalizer)))

(define (make-gc-finalized-object finalizer get-context context->object)
  ;; A bunch of hair to permit microcode descriptors be opened with
  ;; interrupts turned on, yet not leave a dangling descriptor around
  ;; if the open is interrupted before the runtime system's data
  ;; structures are updated.
  (guarantee gc-finalizer? finalizer 'make-gc-finalized-object)
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
	       (%weak-alist-table-add-pair! (gc-finalizer-items finalizer) p)))
	   object)))
     (lambda ()
       (if (and (not (weak-car p)) (weak-cdr p))
	   (begin
	     ((gc-finalizer-procedure finalizer) (weak-cdr p))
	     (weak-set-cdr! p #f)))))))

(define gc-finalizers (weak-list-set eq?))
(define gc-finalizers-mutex (make-thread-mutex))

(define (reset-gc-finalizers)
  (walk-gc-finalizers-list/unsafe
   (lambda (finalizer)
     (weak-alist-table-clear! (gc-finalizer-items finalizer)))))
(add-event-receiver! event:after-restore reset-gc-finalizers)

(define (run-gc-finalizers)
  (with-thread-mutex-try-lock gc-finalizers-mutex
    (lambda ()
      (walk-gc-finalizers-list/unsafe
       (lambda (finalizer)
	 (with-thread-mutex-try-lock (gc-finalizer-mutex finalizer)
	   (lambda ()
	     (without-interruption
	      (lambda ()
		(weak-alist-table-clean! (gc-finalizer-items finalizer)))))
	   (lambda ()
	     unspecific)))))
    (lambda ()
      unspecific)))
(add-boot-init!
 (lambda ()
   (add-gc-daemon! run-gc-finalizers)))

(define (walk-gc-finalizers-list/unsafe procedure)
  (weak-list-set-for-each procedure gc-finalizers))