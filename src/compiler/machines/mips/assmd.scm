#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Assembler Machine Dependencies

(declare (usual-integrations))

(let-syntax ((ucode-type
	      (sc-macro-transformer
	       (lambda (form environment)
		 environment
		 (apply microcode-type (cdr form))))))

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

(define (instruction-initial-position block)
  (if (eq? endianness 'LITTLE)
      0
      (bit-string-length block)))

(define (instruction-insert! bits block position receiver)
  (let ((l (bit-string-length bits)))
    (if (eq? endianness 'LITTLE)
	(begin
	  (bit-substring-move-right! bits 0 l block position)
	  (receiver (+ position l)))
	(let ((new-position (- position l)))
	  (bit-substring-move-right! bits 0 l block new-position)
	  (receiver new-position)))))

(define (instruction-append x y)
  (if (eq? endianness 'LITTLE)
      (bit-string-append x y)
      (bit-string-append-reversed x y)))

;;; end let-syntax
)