#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Flow Graph Generation: Declarations

(declare (usual-integrations))

;;; A block's declarations are processed in two phases: before and
;;; after the flow graph is generated for the block's children.  See
;;; GENERATE/BODY in fggen/fggen.scm.  Some declarations need to refer
;;; to information about variables bound by the block, so they use
;;; post-declarations; others need to establish information that the
;;; children can inherit from, so they use pre-declarations.

(define (process-top-level-declarations! block declarations handlers)
  (process-declarations!
   block
   (let loop
       ((declarations declarations)
	(defaults compiler:default-top-level-declarations))
     (if (null? defaults)
	 declarations
	 (loop (if (assq (caar defaults) declarations)
		   declarations
		   (cons (car defaults) declarations))
	       (cdr defaults))))
   handlers))

(define (process-declarations! block declarations handlers)
  (for-each (lambda (declaration)
	      (process-declaration! block declaration handlers))
	    declarations))

(define (process-declaration! block declaration handlers)
  (let ((entry (assq (car declaration) handlers)))
    (if entry
	((cdr entry) block (car declaration) (cdr declaration))
	(warn "Unknown declaration name" (car declaration)))))

(define (declaration-processor get-handlers)
  (lambda (block declarations)
    (process-top-level-declarations! block declarations (get-handlers))))

(define (declaration-definer get-handlers set-handlers!)
  (lambda (keyword handler)
    (let ((handlers (get-handlers)))
      (cond ((assq keyword handlers)
	     => (lambda (entry)
		  (set-cdr! entry handler)))
	    (else
	     (set-handlers! (cons (cons keyword handler) handlers)))))
    keyword))

(define pre-declarations '())
(define post-declarations '())

(define process-pre-declarations!
  (declaration-processor (lambda () pre-declarations)))

(define process-post-declarations!
  (declaration-processor (lambda () post-declarations)))

(define define-pre-declaration
  (declaration-definer (lambda () pre-declarations)
		       (lambda (handlers) (set! pre-declarations handlers))))

(define define-post-declaration
  (declaration-definer (lambda () post-declarations)
		       (lambda (handlers) (set! post-declarations handlers))))

(define (define-pre-only-declaration keyword handler)
  (define-pre-declaration keyword handler)
  (define-post-declaration keyword ignored-declaration))

(define (define-post-only-declaration keyword handler)
  (define-pre-declaration keyword ignored-declaration)
  (define-post-declaration keyword handler))

(define ignored-declaration
  (lambda (block keyword parameters)
    block keyword parameters		;ignore
    unspecific))

(package (boolean-variable-property)

(define-export (boolean-variable-property block keyword body)
  (if (and (pair? body) (null? (cdr body)))
      (for-each (lambda (variable)
		  (if (not (memq keyword (variable-declarations variable)))
		      (set-variable-declarations!
		       variable
		       (cons keyword (variable-declarations variable)))))
		(evaluate-variable-specification block (car body)))
      (warn "Misformed declaration" (cons keyword body))))

(define (evaluate-variable-specification block specification)
  (let loop ((specification specification))
    (cond ((eq? specification 'BOUND) (block-bound-variables block))
	  ((eq? specification 'FREE) (block-free-variables block))
	  ((eq? specification 'ASSIGNED)
	   (list-transform-positive
	       (append (block-bound-variables block)
		       (block-free-variables block))
	     variable-assigned?))
	  ((eq? specification 'NONE) '())
	  ((eq? specification 'ALL)
	   (append (block-bound-variables block)
		   (block-free-variables block)))
	  ((and (pair? specification)
		(assq (car specification) binary-operators)
		(pair? (cdr specification))
		(pair? (cddr specification))
		(null? (cdddr specification)))
	   ((cdr (assq (car specification) binary-operators))
	    (loop (cadr specification))
	    (loop (caddr specification))))
	  ((and (pair? specification)
		(eq? (car specification) 'SET)
		(symbol-list? (cdr specification)))
	   (let loop ((symbols (cdr specification)))
	     (if (null? symbols)
		 '()
		 (let ((entry
			(or (variable-assoc (car symbols)
					    (block-bound-variables block))
			    (variable-assoc (car symbols)
					    (block-free-variables block)))))
		   (if entry
		       (cons entry (loop (cdr symbols)))
		       (loop (cdr symbols)))))))
	  (else
	   (warn "Misformed variable specification" specification)
	   '()))))

(define binary-operators
  `((DIFFERENCE . ,eq-set-difference)
    (INTERSECTION . ,eq-set-intersection)
    (UNION . ,eq-set-union)))

(define (symbol-list? object)
  (or (null? object)
      (and (pair? object)
	   (symbol? (car object))
	   (symbol-list? (cdr object)))))

)

(define-post-only-declaration 'UUO-LINK boolean-variable-property)
(define-post-only-declaration 'CONSTANT boolean-variable-property)
(define-post-only-declaration 'IGNORE-REFERENCE-TRAPS
  boolean-variable-property)
(define-post-only-declaration 'IGNORE-ASSIGNMENT-TRAPS
  boolean-variable-property)
(define-post-only-declaration 'USUAL-DEFINITION boolean-variable-property)
(define-post-only-declaration 'SIDE-EFFECT-FREE boolean-variable-property)
(define-post-only-declaration 'PURE-FUNCTION boolean-variable-property)

;;;; Safety Check Declarations

(let ()
  (define (check-property block-checks set-block-checks! enable?)
    (lambda (block keyword primitives)
      keyword				;ignore
      (set-block-checks!
       block
       (let ((checks (block-checks block)))
	 (if (null? primitives)
	     enable?
	     (if (boolean? checks)
		 (if (eqv? checks enable?)
		     checks
		     (if enable?
			 (list checks primitives '())
			 (list checks '() primitives)))
		 (let ((default (car checks))
		       (do-check (cadr checks))
		       (dont-check (caddr checks)))
		   (if enable?
		       (list default
			     (eq-set-adjoin primitives do-check)
			     dont-check)
		       (list default
			     do-check
			     (eq-set-adjoin primitives dont-check))))))))))
  (define-pre-only-declaration 'TYPE-CHECKS
    (check-property block-type-checks set-block-type-checks! #t))
  (define-pre-only-declaration 'NO-TYPE-CHECKS
    (check-property block-type-checks set-block-type-checks! #f))
  (define-pre-only-declaration 'RANGE-CHECKS
    (check-property block-range-checks set-block-range-checks! #t))
  (define-pre-only-declaration 'NO-RANGE-CHECKS
    (check-property block-range-checks set-block-range-checks! #f)))
