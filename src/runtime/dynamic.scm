#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Fluids and Parameters
;;; package: (runtime dynamic)

(declare (usual-integrations))


;; The current thread's fluid and parameter bindings.
(define bindings '())

;;;; Fluids

(define-structure fluid
  value)

(define (guarantee-fluid f operator)
  (if (not (fluid? f))
      (error:wrong-type-argument f "a fluid" operator)))

(define (fluid f)
  (guarantee-fluid f 'FLUID)
  (let ((entry (assq f bindings)))
    (if entry (cdr entry) (fluid-value f))))

(define (set-fluid! f val)
  (guarantee-fluid f 'SET-FLUID!)
  (let ((entry (assq f bindings)))
    (if entry (set-cdr! entry val) (set-fluid-value! f val))))

(define (let-fluid fluid value thunk)
  (guarantee-fluid fluid 'LET-FLUID)
  (guarantee-thunk thunk 'LET-FLUID)
  (fluid-let ((bindings (cons (cons fluid value) bindings)))
    (thunk)))

(define (let-fluids . args)
  (let loop ((args args)
	     (new-bindings '()))
    (if (null? (cdr args))
	(begin
	  (guarantee-thunk (car args) 'LET-FLUIDS)
	  (fluid-let ((bindings (append! new-bindings bindings)))
	    ((car args))))
	(begin
	  (guarantee-fluid (car args) 'LET-FLUIDS)
	  (loop (cddr args)
		(cons (cons (car args) (cadr args)) new-bindings))))))

;;;; Parameters

(define-structure %parameter
  value converter)

(define (parameter? p)
  (and (entity? p) (%parameter? (entity-extra p))))

(define (guarantee-parameter p operator)
  (if (not (parameter? p))
      (error:wrong-type-argument p "a parameter" operator)))

(define (make-parameter init #!optional converter)
  (if (not (default-object? converter))
      (guarantee-procedure-of-arity converter 1 'MAKE-PARAMETER))
  (make-entity (lambda (self)
		 (let ((entry (assq self bindings)))
		   (if entry
		       (cdr entry)
		       (%parameter-value (entity-extra self)))))
	       (make-%parameter (if (default-object? converter)
				    init
				    (converter init))
				(if (default-object? converter)
				    identity-procedure
				    converter))))

(define (set-parameter! p v)
  (guarantee-parameter p 'PARAMETER-SET!)
  (let ((%p (entity-extra p)))
    (let ((%v ((%parameter-converter %p) v))
	  (entry (assq p bindings)))
      (if entry
	  (set-cdr! entry %v)
	  (set-%parameter-value! %p %v)))))

(define (parameter-converter p)
  (%parameter-converter (entity-extra p)))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((PARAM VALUE) BINDING ...) BODY ...)
     (parameterize-helper ((PARAM VALUE) BINDING ...) () BODY ...))))

(define-syntax parameterize-helper
  (syntax-rules ()
    ((_ ((PARAM VALUE) BINDING ...) (EXTENSION ...) BODY ...)
     (parameterize-helper (BINDING ...)
			  ((cons PARAM VALUE) EXTENSION ...)
			  BODY ...))
    ((_ () (EXTENSION ...) BODY ...)
     (parameterize* (list EXTENSION ...) (lambda () BODY ...)))))

(define (parameterize* new-bindings thunk)
  (fluid-let
      ((bindings
	(let loop ((new new-bindings))
	  (if (null? new)
	      bindings
	      (if (and (pair? new)
		       (pair? (car new)))
		  (let ((p (caar new))
			(v (cdar new)))
		    (cons (if (parameter? p)
			      (cons p ((parameter-converter p) v))
			      (let ((p* (error:wrong-type-argument
					 p "parameter" 'parameterize*)))
				(cons p* ((parameter-converter p*) v))))
			  (loop (cdr new))))
		  (error:wrong-type-argument
		   new-bindings "alist" 'parameterize*))))))
    (thunk)))