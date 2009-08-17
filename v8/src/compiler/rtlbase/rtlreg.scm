#| -*-Scheme-*-

$Id: 8aa5fe9c690222511df0b8ef30f668bf00ebd09a $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; RTL Registers

(declare (usual-integrations))

(define *machine-register-map*)

(define (initialize-machine-register-map!)
  (set! *machine-register-map*
	(let ((map (make-vector number-of-machine-registers)))
	  (let loop ((n 0))
	    (if (< n number-of-machine-registers)
		(begin (vector-set! map n (%make-register n))
		       (loop (1+ n)))))
	  map)))

(define-integrable (rtl:make-machine-register n)
  (vector-ref *machine-register-map* n))

(define-integrable (machine-register? register)
  (< register number-of-machine-registers))

(define (for-each-machine-register procedure)
  (let ((limit number-of-machine-registers))
    (define (loop register)
      (if (< register limit)
	  (begin (procedure register)
		 (loop (1+ register)))))
    (loop 0)))

(define (rtl:make-pseudo-register)
  (let ((n (rgraph-n-registers *current-rgraph*)))
    (set-rgraph-n-registers! *current-rgraph* (1+ n))
    (%make-register n)))

(define-integrable (pseudo-register? register)
  (>= register number-of-machine-registers))

(define (for-each-pseudo-register procedure)
  (let ((n-registers (rgraph-n-registers *current-rgraph*)))
    (define (loop register)
      (if (< register n-registers)
	  (begin (procedure register)
		 (loop (1+ register)))))
    (loop number-of-machine-registers)))

(let-syntax
    ((define-register-references
       (macro (slot)
	 (let ((name (symbol-append 'REGISTER- slot)))
	   (let ((vector `(,(symbol-append 'RGRAPH- name) *CURRENT-RGRAPH*)))
	     `(BEGIN (DEFINE-INTEGRABLE (,name REGISTER)
		       (VECTOR-REF ,vector REGISTER))
		     (DEFINE-INTEGRABLE
		       (,(symbol-append 'SET- name '!) REGISTER VALUE)
		       (VECTOR-SET! ,vector REGISTER VALUE))))))))
  (define-register-references bblock)
  (define-register-references n-refs)
  (define-register-references n-deaths)
  (define-register-references live-length)
  (define-register-references renumber))

(define-integrable (reset-register-n-refs! register)
  (set-register-n-refs! register 0))

(define (increment-register-n-refs! register)
  (set-register-n-refs! register (1+ (register-n-refs register))))

(define-integrable (reset-register-n-deaths! register)
  (set-register-n-deaths! register 0))

(define (increment-register-n-deaths! register)
  (set-register-n-deaths! register (1+ (register-n-deaths register))))

(define-integrable (reset-register-live-length! register)
  (set-register-live-length! register 0))

(define (increment-register-live-length! register)
  (set-register-live-length! register (1+ (register-live-length register))))

(define (decrement-register-live-length! register)
  (set-register-live-length! register (-1+ (register-live-length register))))

(define (register-crosses-call? register)
  (bit-string-ref (rgraph-register-crosses-call? *current-rgraph*) register))

(define (register-crosses-call! register)
  (bit-string-set! (rgraph-register-crosses-call? *current-rgraph*) register))

(define (pseudo-register-value-class register)
  (vector-ref (rgraph-register-value-classes *current-rgraph*) register))

(define (pseudo-register-known-value register)
  (vector-ref (rgraph-register-known-values *current-rgraph*) register))

(define (pseudo-register-known-expression register)
  (vector-ref (rgraph-register-known-expressions *current-rgraph*) register))

(define (register-value-class register)
  (if (machine-register? register)
      (machine-register-value-class register)
      (pseudo-register-value-class register)))

(define (register-known-value register)
  (if (machine-register? register)
      (machine-register-known-value register)
      (pseudo-register-known-value register)))

(define (register-known-expression register)
  (if (machine-register? register)
      #F
      (pseudo-register-known-expression register)))