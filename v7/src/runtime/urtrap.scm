#| -*-Scheme-*-

$Id: urtrap.scm,v 14.5 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
			(write (reference-trap-kind trap) port)))))
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