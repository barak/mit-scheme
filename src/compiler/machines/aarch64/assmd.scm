#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Assembler Machine Dependencies.  AArch64 version

(declare (usual-integrations))

(define-integrable maximum-padding-length
  ;; Instruction length is always a multiple of 32 bits
  32)

(define-integrable padding-string
  ;; Pad with HLT #0 instructions
  (unsigned-integer->bit-string 32 #xd4400000))

(define-integrable block-offset-width
  ;; Block offsets are always 16 bit words
  16)

(define-integrable maximum-block-offset
  ;; PC always aligned on 32-bit boundary.  Use the extra bit.
  (- (expt 2 (1+ block-offset-width)) 4))

(define-integrable (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width
                                (+ (shift-left (quotient offset 4) 1)
                                   (if start? 0 1))))

;;; Machine dependent instruction order

(define (instruction-initial-position block)
  (case endianness
    ((BIG) (bit-string-length block))
    ((LITTLE) 0)
    (else (error "Unknown endianness:" endianness))))

(define (instruction-insert! bits block position receiver)
  (let ((l (bit-string-length bits)))
    (case endianness
      ((BIG)
       (let ((new-position (- position l)))
         (bit-substring-move-right! bits 0 l block new-position)
         (receiver new-position)))
      ((LITTLE)
       (let ((new-position (+ position l)))
         (bit-substring-move-right! bits 0 l block position)
         (receiver new-position)))
      (else
       (error "Unknown endianness:" endianness)))))

(define (instruction-append x y)
  (case endianness
    ((BIG) (bit-string-append-reversed x y))
    ((LITTLE) (bit-string-append x y))
    (else (error "Unknown endianness:" endianness))))
