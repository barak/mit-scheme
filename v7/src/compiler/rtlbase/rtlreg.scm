#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlreg.scm,v 4.3 1988/04/25 21:45:08 markf Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

(define-integrable (register-crosses-call? register)
  (bit-string-ref (rgraph-register-crosses-call? *current-rgraph*) register))

(define-integrable (register-crosses-call! register)
  (bit-string-set! (rgraph-register-crosses-call? *current-rgraph*) register))

(define-integrable (register-contains-address? register)
  (memq register (rgraph-address-registers *current-rgraph*)))

(define-integrable (register-contains-fixnum? register)
  (memq register (rgraph-fixnum-registers *current-rgraph*)))