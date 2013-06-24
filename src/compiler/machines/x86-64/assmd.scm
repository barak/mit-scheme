#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Assembler Machine Dependencies.  AMD x86-64 version

(declare (usual-integrations))

(let-syntax
    ((ucode-type
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 (apply microcode-type (cdr form))))))

(define-integrable maximum-padding-length
  ;; Instructions can be any number of bytes long.
  ;; Thus the maximum padding is 7 bytes.
  56)

(define-integrable padding-string
  ;; Pad with HLT instructions
  (unsigned-integer->bit-string 8 #xf4))

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