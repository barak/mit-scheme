#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/urtrap.scm,v 14.2 1988/06/13 11:59:56 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Reference Traps
;;; package: (runtime reference-trap)

(declare (usual-integrations))

(define-structure (reference-trap
		   (print-procedure
		    (unparser/standard-method 'REFERENCE-TRAP
		      (lambda (state trap)
			(unparse-object state (reference-trap-kind trap))))))
  (kind false read-only true)
  (extra false read-only true))

(define-primitives
  primitive-object-type?
  primitive-object-set-type
  primitive-object-ref)

(define (map-reference-trap getter)
  (if (primitive-object-type? (ucode-type reference-trap) (getter))
      (let ((index (object-datum (getter))))
	(if (<= index trap-max-immediate)
	    (make-reference-trap index false)
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

(define (reference-trap-kind-name kind)
  (or (and (< kind (vector-length trap-kind-names))
	   (vector-ref trap-kind-names kind))
      'UNKNOWN))

(define (make-unassigned-reference-trap)
  (make-reference-trap 0 false))

(define (unassigned-reference-trap? object)
  (and (reference-trap? object)
       (memq (reference-trap-kind-name (reference-trap-kind object))
	     '(UNASSIGNED UNASSIGNED-DANGEROUS))))

(define (make-unbound-reference-trap)
  (make-reference-trap 2 false))

(define (unbound-reference-trap? object)
  (and (reference-trap? object)
       (memq (reference-trap-kind-name (reference-trap-kind object))
	     '(UNBOUND UNBOUND-DANGEROUS))))
      
;;; The following must agree with the microcode.

(define-integrable trap-max-immediate 9)

(define-integrable trap-kind-names
  '#(UNASSIGNED				;0
     UNASSIGNED-DANGEROUS		;1
     UNBOUND				;2
     UNBOUND-DANGEROUS			;3
     ILLEGAL				;4
     ILLEGAL-DANGEROUS			;5
     #F					;6
     #F					;7
     #F					;8
     #F					;9
     NOP				;10
     DANGEROUS				;11
     FLUID				;12
     FLUID-DANGEROUS			;13
     COMPILER-CACHED			;14
     COMPILER-CACHED-DANGEROUS		;15
     ))