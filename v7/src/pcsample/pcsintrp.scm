#| -*-Scheme-*-

$Id: pcsintrp.scm,v 1.3 2002/11/20 19:46:18 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
