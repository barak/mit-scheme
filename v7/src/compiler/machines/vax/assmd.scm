#| -*-Scheme-*-

$Id: assmd.scm,v 4.11 2003/02/14 18:28:07 cph Exp $

Copyright (c) 1987, 1989, 1991, 1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Assembler Machine Dependencies.  DEC Vax version

(declare (usual-integrations))

(let-syntax ((ucode-type
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 (apply microcode-type (cdr form))))))

(define-integrable maximum-padding-length
  ;; Instructions can be any number of bytes long.
  ;; Thus the maximum padding is 3 bytes.
  24)

(define-integrable padding-string
  ;; Pad with HALT instructions
  (unsigned-integer->bit-string 8 #x00))

(define-integrable block-offset-width
  ;; Block offsets are encoded words
  16)

(define maximum-block-offset
  (- (expt 2 (-1+ block-offset-width)) 1))

(define-integrable (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width
				(+ (* 2 offset)
				   (if start? 0 1))))


(define-integrable nmv-type-string
  (unsigned-integer->bit-string scheme-type-width
				(ucode-type manifest-nm-vector)))

(define (make-nmv-header n)
  (bit-string-append (unsigned-integer->bit-string scheme-datum-width n)
		     nmv-type-string))

;;; Machine dependent instruction order

(define (instruction-insert! bits block position receiver)
  (let ((l (bit-string-length bits)))
    (bit-substring-move-right! bits 0 l block position)
    (receiver (+ position l))))

(define-integrable (instruction-initial-position block)
  block					; ignored
  0)

(define-integrable instruction-append bit-string-append)

;;; end let-syntax
)