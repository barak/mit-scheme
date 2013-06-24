#| -*-Scheme-*-

$Id: urtrap.scm,v 14.13 2002/01/08 05:06:46 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Reference Traps
;;; package: (runtime reference-trap)

(declare (usual-integrations))

(define-structure (reference-trap
		   (type vector)
		   (named ((ucode-primitive string->symbol)
			   "#[(runtime reference-trap)reference-trap]"))
		   (print-procedure
		    (standard-unparser-method 'REFERENCE-TRAP
		      (lambda (trap port)
			(write-char #\space port)
			(write (let ((kind (reference-trap-kind trap)))
				 (or (reference-trap-kind-name kind)
				     kind))
			       port)))))
  (kind #f read-only #t)
  (extra #f read-only #t))

(define-primitives
  primitive-object-type?
  primitive-object-set-type
  primitive-object-ref)

(define (map-reference-trap getter)
  (if (primitive-object-type? (ucode-type reference-trap) (getter))
      (let ((index (object-datum (getter))))
	(if (<= index trap-max-immediate)
	    (make-reference-trap index #f)
	    (make-reference-trap (primitive-object-ref (getter) 0)
				 (primitive-object-ref (getter) 1))))
      (getter)))

(define (unmap-reference-trap trap)
  (if (reference-trap? trap)
      (primitive-object-set-type
       (ucode-type reference-trap)
       (if (<= (reference-trap-kind trap) trap-max-immediate)
	   (reference-trap-kind trap)
	   (cons (reference-trap-kind trap)
		 (reference-trap-extra trap))))
      trap))

;;; The following must agree with the microcode.
(define-integrable trap-max-immediate 9)

(define (reference-trap-kind-name kind)
  (case kind
    ((0) 'UNASSIGNED)
    ((2) 'UNBOUND)
    ((6) 'EXPENSIVE)
    ((14) 'COMPILER-CACHED)
    ((15) 'MACRO)
    (else #f)))

(define (make-unassigned-reference-trap)
  (make-reference-trap 0 #f))

(define (unassigned-reference-trap? object)
  (and (reference-trap? object)
       (fix:= 0 (reference-trap-kind object))))

(define (make-unmapped-unassigned-reference-trap)
  (primitive-object-set-type (ucode-type reference-trap) 0))

(define (unmapped-unassigned-reference-trap? getter)
  (and (primitive-object-type? (ucode-type reference-trap) (getter))
       (fix:= 0 (object-datum (getter)))))

(define (make-unbound-reference-trap)
  (make-reference-trap 2 #f))

(define (unbound-reference-trap? object)
  (and (reference-trap? object)
       (fix:= 2 (reference-trap-kind object))))

(define (make-unmapped-unbound-reference-trap)
  (primitive-object-set-type (ucode-type reference-trap) 2))

(define (unmapped-unbound-reference-trap? getter)
  (and (primitive-object-type? (ucode-type reference-trap) (getter))
       (fix:= 2 (object-datum (getter)))))

(define (cached-reference-trap? object)
  (and (reference-trap? object)
       (fix:= 14 (reference-trap-kind object))))

(define (cached-reference-trap-value trap)
  (if (not (cached-reference-trap? trap))
      (error:wrong-type-argument trap "cached reference trap"
				 'CACHED-REFERENCE-TRAP-VALUE))
  (map-reference-trap
   (let ((cache (reference-trap-extra trap)))
     (lambda ()
       (primitive-object-ref cache 0)))))

(define (map-reference-trap-value getter)
  (let ((value (map-reference-trap getter)))
    (if (cached-reference-trap? value)
	(cached-reference-trap-value value)
	value)))

(define (make-macro-reference-trap transformer)
  (make-reference-trap 15 transformer))

(define (macro-reference-trap? object)
  (and (reference-trap? object)
       (fix:= 15 (reference-trap-kind object))))

(define (macro-reference-trap-transformer trap)
  (if (not (macro-reference-trap? trap))
      (error:wrong-type-argument trap "macro reference trap"
				 'MACRO-REFERENCE-TRAP-TRANSFORMER))
  (reference-trap-extra trap))

(define (make-unmapped-macro-reference-trap transformer)
  (primitive-object-set-type (ucode-type reference-trap)
			     (cons 15 transformer)))

(define (unmapped-macro-reference-trap? getter)
  (and (primitive-object-type? (ucode-type reference-trap) (getter))
       (let ((index (object-datum (getter))))
	 (and (> index trap-max-immediate)
	      (fix:= 15 (primitive-object-ref (getter) 0))))))

(define (make-macro-reference-trap-expression transformer)
  (make-combination primitive-object-set-type
		    (list (ucode-type reference-trap)
			  (make-combination cons (list 15 transformer)))))

(define (macro-reference-trap-expression? expression)
  (and (combination? expression)
       (eq? (combination-operator expression) primitive-object-set-type)
       (let ((operands (combination-operands expression)))
	 (and (pair? operands)
	      (eqv? (car operands) (ucode-type reference-trap))
	      (pair? (cdr operands))
	      (let ((expression (cadr operands)))
		(and (combination? expression)
		     (eq? (combination-operator expression) cons)
		     (let ((operands (combination-operands expression)))
		       (and (pair? operands)
			    (eqv? (car operands) 15)
			    (pair? (cdr operands))
			    (null? (cddr operands))))))
	      (null? (cddr operands))))))

(define (macro-reference-trap-expression-transformer expression)
  (cadr (combination-operands (cadr (combination-operands expression)))))