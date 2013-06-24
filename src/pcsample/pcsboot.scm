#| -*-Scheme-*-

$Id: pcsboot.scm,v 1.2 1999/01/02 06:11:34 cph Exp $

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

