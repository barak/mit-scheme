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

;;;; Reference Traps
;;; package: (runtime reference-trap)

(declare (usual-integrations))

(define-structure (reference-trap
		   (type vector)
		   (named '|#[(runtime reference-trap)reference-trap]|)
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
	    (make-immediate-reference-trap index)
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

(define (make-immediate-reference-trap kind)
  (or (vector-ref cached-traps kind)
      (let ((trap (make-reference-trap kind #f)))
	(vector-set! cached-traps kind trap)
	trap)))

(define cached-traps
  (make-vector (fix:+ trap-max-immediate 1) #f))

(define (make-unassigned-reference-trap)
  (make-immediate-reference-trap 0))

(define (unassigned-reference-trap? object)
  (and (reference-trap? object)
       (fix:= 0 (reference-trap-kind object))))

(define (make-unmapped-unassigned-reference-trap)
  (primitive-object-set-type (ucode-type reference-trap) 0))

(define (unmapped-unassigned-reference-trap? getter)
  (and (primitive-object-type? (ucode-type reference-trap) (getter))
       (fix:= 0 (object-datum (getter)))))

(define (make-unbound-reference-trap)
  (make-immediate-reference-trap 2))

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
  (make-combination (ucode-primitive primitive-object-set-type)
		    (list (ucode-type reference-trap)
			  (make-combination (ucode-primitive cons)
					    (list 15 transformer)))))

(define (macro-reference-trap-expression? expression)
  (and (combination? expression)
       (eq? (combination-operator expression)
	    (ucode-primitive primitive-object-set-type))
       (let ((operands (combination-operands expression)))
	 (and (pair? operands)
	      (eqv? (car operands) (ucode-type reference-trap))
	      (pair? (cdr operands))
	      (let ((expression (cadr operands)))
		(and (combination? expression)
		     (eq? (combination-operator expression)
			  (ucode-primitive cons))
		     (let ((operands (combination-operands expression)))
		       (and (pair? operands)
			    (eqv? (car operands) 15)
			    (pair? (cdr operands))
			    (null? (cddr operands))))))
	      (null? (cddr operands))))))

(define (macro-reference-trap-expression-transformer expression)
  (cadr (combination-operands (cadr (combination-operands expression)))))