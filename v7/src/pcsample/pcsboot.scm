#| -*-Scheme-*-

$Id: pcsboot.scm,v 1.1 1995/07/28 14:14:08 adams Exp $

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

;;;; PC Sample Interrupt Bits (for consistency w/ .../runtime/boot.scm)
;;; package: (pc-sample interrupt-handler)

(declare (usual-integrations))

(define-integrable interrupt-bit/IPPB-flush	#x0200) ; pc-sample
(define-integrable interrupt-bit/IPPB-extend 	#x0400) ; pc-sample
(define-integrable interrupt-bit/PCBPB-flush	#x0800) ; pc-sample
(define-integrable interrupt-bit/PCBPB-extend 	#x1000) ; pc-sample
(define-integrable interrupt-bit/HCBPB-flush	#x2000) ; pc-sample
(define-integrable interrupt-bit/HCBPB-extend 	#x4000) ; pc-sample


;;; fini

