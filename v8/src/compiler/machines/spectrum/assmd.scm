#| -*-Scheme-*-

$MC68020-Header: assmd.scm,v 1.36 89/08/28 18:33:33 GMT cph Exp $

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

;;;; Assembler Machine Dependencies

(declare (usual-integrations))

(let-syntax ((ucode-type (macro (name) `',(microcode-type name))))

(define-integrable maximum-padding-length
  ;; Instruction length is always a multiple of 32 bits
  ;; Would 0 work here?
  32)

(define padding-string
  ;; Pad with `DIAG SCM' instructions
  (unsigned-integer->bit-string maximum-padding-length
				#b00010100010100110100001101001101))

(define-integrable block-offset-width
  ;; Block offsets are always 16 bit words
  16)

(define-integrable maximum-block-offset
  ;; PC always aligned on longword boundary.  Use the extra bit.
  (- (expt 2 (1+ block-offset-width)) 4))

(define (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width
				(+ (quotient offset 2)
				   (if start? 0 1))))

(define (make-nmv-header n)
  (bit-string-append (unsigned-integer->bit-string scheme-datum-width n)
		     nmv-type-string))

(define nmv-type-string
  (unsigned-integer->bit-string scheme-type-width
				(ucode-type manifest-nm-vector)))

(define (object->bit-string object)
  (bit-string-append
   (unsigned-integer->bit-string scheme-datum-width
				 (careful-object-datum object))
   (unsigned-integer->bit-string scheme-type-width (object-type object))))

;;; Machine dependent instruction order

(define (instruction-insert! bits block position receiver)
  (declare (integrate receiver))
  (let* ((l (bit-string-length bits))
	 (new-position (- position l)))
    (bit-substring-move-right! bits 0 l block new-position)
    (receiver new-position)))

(define instruction-initial-position bit-string-length)
(define-integrable instruction-append bit-string-append-reversed)

(declare (integrate-operator instruction-insert!))
;;; end let-syntax
)