#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

(define (apply-bindings new-bindings thunk)
  (let ((swap!
	 (lambda ()
	   (set! bindings (set! new-bindings (set! bindings)))
	   unspecific)))
    (shallow-fluid-bind swap! thunk swap!)))

;;;; Fluids

(define-structure fluid
  value)

(define-guarantee fluid "fluid")

(define (fluid f)
  (guarantee-fluid f 'fluid)
  (let ((entry (assq f bindings)))
    (if entry
	(cdr entry)
	(fluid-value f))))

(define (set-fluid! f val)
  (guarantee-fluid f 'set-fluid!)
  (let ((entry (assq f bindings)))
    (if entry
	(set-cdr! entry val)
	(set-fluid-value! f val))))

(define (let-fluid fluid value thunk)
  (guarantee-fluid fluid 'let-fluid)
  (guarantee-thunk thunk 'let-fluid)
  (apply-bindings (cons (cons fluid value) bindings)
		  thunk))

(define (let-fluids . args)
  (let loop
      ((args args)
       (new-bindings '()))
    (if (not (pair? args))
	(error "Ill-formed let-fluids arguments:" args))
    (if (pair? (cdr args))
	(begin
	  (guarantee-fluid (car args) 'let-fluids)
	  (loop (cddr args)
		(cons (cons (car args) (cadr args))
		      new-bindings)))
	(begin
	  (guarantee-thunk (car args) 'let-fluids)
	  (apply-bindings (append! new-bindings bindings)
			  (car args))))))

;;;; Parameters

(define-structure parameter-metadata
  value
  converter)

(define (parameter? p)
  (and (entity? p)
       (parameter-metadata? (entity-extra p))))

(define-guarantee parameter "parameter")

(define (make-parameter init #!optional converter)
  (let ((converter
	 (if (default-object? converter)
	     identity-procedure
	     (begin
	       (guarantee-procedure-of-arity converter 1 'make-parameter)
	       converter))))
    (make-entity (lambda (self)
		   (let ((entry (assq self bindings)))
		     (if entry
			 (cdr entry)
			 (parameter-metadata-value (entity-extra self)))))
		 (make-parameter-metadata (converter init)
					  converter))))

(define (set-parameter! p v)
  (let ((metadata (entity-extra p)))
    (let ((entry (assq p bindings))
	  (converted ((parameter-metadata-converter metadata) v)))
      (if entry
	  (set-cdr! entry converted)
	  (set-parameter-metadata-value! metadata converted)))))

(define (parameter-converter p)
  (parameter-metadata-converter (entity-extra p)))

(define-syntax parameterize
  (syntax-rules ()
    ((_ ((param value) binding ...) body ...)
     (parameterize-helper ((param value) binding ...) () body ...))))

(define-syntax parameterize-helper
  (syntax-rules ()
    ((_ ((param value) binding ...) (extension ...) body ...)
     (parameterize-helper (binding ...)
			  ((cons param value) extension ...)
			  body ...))
    ((_ () (extension ...) body ...)
     (parameterize* (list extension ...)
		    (lambda () body ...)))))

(define (parameterize* new-bindings thunk)
  (guarantee-alist new-bindings 'parameterize*)
  (apply-bindings
   (append! (map (lambda (p)
		   (let ((parameter (car p))
			 (value (cdr p)))
		     (cons parameter
			   ((parameter-converter parameter) value))))
		 new-bindings)
	    bindings)
   thunk))