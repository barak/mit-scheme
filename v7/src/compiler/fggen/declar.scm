#| -*-Scheme-*-

$Id: declar.scm,v 1.7 2002/11/20 19:45:49 cph Exp $

Copyright (c) 1987, 1988, 1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Flow Graph Generation: Declarations

(declare (usual-integrations))

(define (process-top-level-declarations! block declarations)
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
	       (cdr defaults))))))

(define (process-declarations! block declarations)
  (for-each (lambda (declaration)
	      (process-declaration! block declaration))
	    declarations))

(define (process-declaration! block declaration)
  (let ((entry (assq (car declaration) known-declarations)))
    (if entry
	((cdr entry) block (car declaration) (cdr declaration))
	(warn "Unknown declaration name" (car declaration)))))

(define known-declarations
  '())

(define (define-declaration keyword handler)
  (let ((entry (assq keyword known-declarations)))
    (if entry
	(set-cdr! entry handler)
	(set! known-declarations
	      (cons (cons keyword handler)
		    known-declarations))))
  keyword)

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

(define-declaration 'UUO-LINK boolean-variable-property)
(define-declaration 'CONSTANT boolean-variable-property)
(define-declaration 'IGNORE-REFERENCE-TRAPS boolean-variable-property)
(define-declaration 'IGNORE-ASSIGNMENT-TRAPS boolean-variable-property)
(define-declaration 'USUAL-DEFINITION boolean-variable-property)
(define-declaration 'SIDE-EFFECT-FREE boolean-variable-property)
(define-declaration 'PURE-FUNCTION boolean-variable-property)