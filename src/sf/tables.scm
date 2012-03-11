#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; SCode Optimizer: Tables
;;; package: (scode-optimizer)

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Environment

;; An environment is implemented as an alist mapping a variable
;; to one of three things, a value, an unknown-value marker, or
;; a delayed integration.

(define (environment/make)
  '())

(define (environment/bind environment variable value)
  (guarantee-variable variable 'environment/bind)
  (alist-cons variable value environment))

(define-integrable (environment/bind-multiple environment variables values)
  (map* environment cons variables values))

(define (environment/lookup environment variable if-found if-unknown if-not)
  (let ((association (assq variable environment)))
    (if association
	(if (eq? (cdr association) *unknown-value)
	    (if-unknown)
	    (if-found (cdr association)))
	(if-not))))

(define *unknown-value (string-copy "Unknown Value"))

;; Extend the environment with bindings for the formal parameters.
;; Each binding is given the *unknown-value object.
(define (simulate-unknown-application environment procedure)
  (define (bind-required environment required)
    (if (null? required)
	(bind-optional environment (procedure/optional procedure))
	(bind-required
	 (environment/bind environment (car required) *unknown-value)
	 (cdr required))))

  (define (bind-optional environment optional)
    (if (null? optional)
	(bind-rest environment (procedure/rest procedure))
	(bind-optional
	 (environment/bind environment (car optional) *unknown-value)
	 (cdr optional))))

  (define (bind-rest environment rest)
    (if rest
	(environment/bind environment rest *unknown-value)
	environment))

  (bind-required environment (procedure/required procedure)))

;; Extend the environment with actual bindings for the formal
;; parameters.  If the arity does not match, issue a warning
;; and fall back to the unknown case.
(define (simulate-application base-environment block procedure operands)
  (define (procedure->pretty procedure)
    (if (procedure/scode procedure)
	(unsyntax (procedure/scode procedure))
	(let ((arg-list (make-lambda-list
			 (procedure/required procedure)
			 (procedure/optional procedure)
			 (procedure/rest procedure)
			 '())))
	  (if (procedure/name procedure)
	      `(named-lambda (,(procedure/name procedure) ,@arg-list)
		 ...)
	      `(lambda ,arg-list
		 ...)))))

  (define (fail message . irritants)
    (apply warn message irritants)
    (simulate-unknown-application base-environment procedure))

  (define (match-required environment required remaining-operands)
    (cond ((pair? required)
	   (cond ((pair? remaining-operands)
		  (match-required (environment/bind environment
						    (car required)
						    (car remaining-operands))
				  (cdr required)
				  (cdr remaining-operands)))
		 ((null? remaining-operands)
		  (fail "Too few operands in call to procedure"
			procedure
			(procedure->pretty procedure)))
		 (else
		  (fail "Improper list of operands in application"
			procedure
			(procedure->pretty procedure)
			operands))))

	  ((null? required)
	   (match-optional environment
			   (procedure/optional procedure)
			   remaining-operands))

	  ;; impossible?
	  (else (error "INTERNAL ERROR: Required argument list is improper"
		       required))))

  (define (match-optional environment optional remaining-operands)
    (cond ((pair? optional)
	   (cond ((pair? remaining-operands)
		  (match-optional (environment/bind environment
						    (car optional)
						    (car remaining-operands))
				  (cdr optional)
				  (cdr remaining-operands)))
		 ((null? remaining-operands)
		  (match-rest environment (procedure/rest procedure) '()))
		 (else
		  (fail "Improper list of operands in application"
			procedure
			(procedure->pretty procedure)
			operands))))

	  ((null? optional)
	   (match-rest environment (procedure/rest procedure) remaining-operands))
	  ;; impossible?
	  (else (error "INTERNAL ERROR: Optional argument list is improper"
		       optional))))

  (define (listify-tail operands)
    (fold-right
     (lambda (operand tail)
       (combination/make #f
			 block
			 (constant/make #f (ucode-primitive cons))
			 (list operand tail)))
     (constant/make #f '())
     operands))

  (define (match-rest environment rest remaining-operands)
    (cond (rest
	   (environment/bind environment rest (listify-tail remaining-operands)))
	  ((null? remaining-operands)
	   environment)
	  (else
	   (fail "Too many operands in call to procedure"
		  procedure
		  (procedure->pretty procedure)
		  operands))))

  (match-required base-environment (procedure/required procedure) operands))

;;;; Operations

;; An operations table is a triple of three alists.  The first alist
;; contains the lexically visible operations, the second contains
;; the top-level operations, the third contains the global operations.

;; The global operations are installed by the `usual-integrations'
;; declarations, external operations are installed in the top-level
;; operations.  This allows us to lookup the appropriate operation
;; when integrating an expression like (access foo #f) where there
;; is an external integration that *also* is called foo.

(define (operations/make)
  (vector '() '() '()))

(define (operations/lookup operations variable if-found if-not)
  (guarantee-variable variable 'operations/lookup)
  (let ((entry (assq variable (vector-ref operations 0))))
    (if entry
	(if (cdr entry)
	    (if-found (cadr entry) (cddr entry))
	    (if-not))
	(let ((entry (assq variable (vector-ref operations 1))))
	  (if entry
	      (if (cdr entry)
		  (if-found (cadr entry) (cddr entry))
		  (if-not))
	      (let ((entry (assq variable (vector-ref operations 2))))
		(if entry
		    (if-found (cadr entry) (cddr entry))
		    (if-not))))))))

;; When processing a global reference, we only have a name.
(define (operations/lookup-global operations name if-found if-not)
  (guarantee-symbol name 'operations/lookup-global)
  (let ((probe (find (lambda (entry)
		       (eq? (variable/name (car entry)) name))
		     (vector-ref operations 2))))
    (if probe
	(if-found (cadr probe) (cddr probe))
	(if-not))))

(define (operations/shadow operations variables)
  (vector (map* (vector-ref operations 0)
		(lambda (variable)
		  (guarantee-variable variable 'operations/shadow)
		  (cons variable false))
		variables)
	  (vector-ref operations 1)
	  (vector-ref operations 2)))

(define (operations/bind operations operation variable value)
  (guarantee-known-declaration operation 'operations/bind)
  (guarantee-variable variable 'operations/bind)
  (vector (cons (cons* variable operation value)
		(vector-ref operations 0))
	  (vector-ref operations 1)
	  (vector-ref operations 2)))

(define (operations/bind-top-level operations operation variable value)
  (guarantee-known-declaration operation 'operations/bind-top-level)
  (guarantee-variable variable 'operations/bind-top-level)
  (vector (vector-ref operations 0)
	  (cons (cons* variable operation value)
		(vector-ref operations 1))
	  (vector-ref operations 2)))

(define (operations/bind-global operations operation variable value)
  (guarantee-known-declaration operation 'operations/bind-global)
  (guarantee-variable variable 'operations/bind-global)
  (vector (vector-ref operations 0)
	  (vector-ref operations 1)
	  (cons (cons* variable operation value)
		(vector-ref operations 2))))

(define (operations/map-external operations procedure)
  (let loop ((elements (vector-ref operations 0)))
    (cond ((null? elements)
	   '())
	  ((cdar elements)
	   (cons (procedure (cadar elements) (caar elements) (cddar elements))
		 (loop (cdr elements))))
	  (else
	   (loop (cdr elements))))))
