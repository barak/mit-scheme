#| -*-Scheme-*-

$Id: pcsintrp.scm,v 1.1 1995/07/28 14:14:08 adams Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

;;;; PC Sample Interrupt System
;;; package: (pc-sample interrupt-handler)

(declare (usual-integrations))

(define (initialize-package!)
  (install))

(define-primitives
  (clear-interrupts! 1)
  set-fixed-objects-vector!
  )

;; Slots 0--8 are reserved by the system (for GC and overflow et al)

(define-integrable IPPB-flush-slot	         9) ; pc-sample
(define-integrable IPPB-extend-slot       	10) ; pc-sample
(define-integrable PCBPB-flush-slot	        11) ; pc-sample
(define-integrable PCBPB-extend-slot       	12) ; pc-sample
(define-integrable HCBPB-flush-slot	        13) ; pc-sample
(define-integrable HCBPB-extend-slot       	14) ; pc-sample

;; Slot 15 is the dreaded illegal-interrupt-slot


;;;; Miscellaneous PC Sample Interrupts: buffer flush and extend requests

(define (IPPB-flush-request-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (interp-proc-profile-buffer/flush)
  (clear-interrupts! interrupt-bit/IPPB-flush))

(define (IPPB-extend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (interp-proc-profile-buffer/extend)
  (clear-interrupts! interrupt-bit/IPPB-extend))

(define (PCBPB-flush-request-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (purified-code-block-profile-buffer/flush)
  (clear-interrupts! interrupt-bit/PCBPB-flush))

(define (PCBPB-extend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (purified-code-block-profile-buffer/extend)
  (clear-interrupts! interrupt-bit/PCBPB-extend))

(define (HCBPB-flush-request-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (heathen-code-block-profile-buffer/flush)
  (clear-interrupts! interrupt-bit/HCBPB-flush))

(define (HCBPB-extend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (heathen-code-block-profile-buffer/extend)
  (clear-interrupts! interrupt-bit/HCBPB-extend))

;;;; Keyboard Interrupts

(define (install)
  (without-interrupts
   (lambda ()
     (let ((system-interrupt-vector
	    (vector-ref (get-fixed-objects-vector) index:interrupt-vector))
	   (interrupt-mask-vector
	    (vector-ref (get-fixed-objects-vector)
			index:interrupt-mask-vector)))

       (vector-set! system-interrupt-vector IPPB-flush-slot ; pc-sample
		    IPPB-flush-request-handler)
       (vector-set! interrupt-mask-vector   IPPB-flush-slot ; pc-sample
		    interrupt-mask/gc-ok)

       (vector-set! system-interrupt-vector IPPB-extend-slot ; pc-sample
		    IPPB-extend-interrupt-handler)
       (vector-set! interrupt-mask-vector   IPPB-extend-slot ; pc-sample
		    interrupt-mask/gc-ok)

       (vector-set! system-interrupt-vector PCBPB-flush-slot ; pc-sample
		    PCBPB-flush-request-handler)
       (vector-set! interrupt-mask-vector   PCBPB-flush-slot ; pc-sample
		    interrupt-mask/gc-ok)

       (vector-set! system-interrupt-vector PCBPB-extend-slot ; pc-sample
		    PCBPB-extend-interrupt-handler)
       (vector-set! interrupt-mask-vector   PCBPB-extend-slot ; pc-sample
		    interrupt-mask/gc-ok)

       (vector-set! system-interrupt-vector HCBPB-flush-slot ; pc-sample
		    HCBPB-flush-request-handler)
       (vector-set! interrupt-mask-vector   HCBPB-flush-slot ; pc-sample
		    interrupt-mask/gc-ok)

       (vector-set! system-interrupt-vector HCBPB-extend-slot ; pc-sample
		    HCBPB-extend-interrupt-handler)
       (vector-set! interrupt-mask-vector   HCBPB-extend-slot ; pc-sample
		    interrupt-mask/gc-ok)

       #|
       ;; Nop
       (set-fixed-objects-vector! (get-fixed-objects-vector))
       |#
       ))))

;;; fini
