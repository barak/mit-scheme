#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Bit String Primitives
;;; package: (runtime bit-string)

(declare (usual-integrations))

(define-primitives
  bit-string-allocate make-bit-string bit-string?
  bit-string-length bit-string-ref bit-string-clear! bit-string-set!
  bit-string-zero? bit-string=?
  bit-string-fill! bit-string-move! bit-string-movec!
  bit-string-or! bit-string-and! bit-string-andc!
  bit-string-xor! bit-substring-move-right!
  bit-string->unsigned-integer unsigned-integer->bit-string
  read-bits! write-bits!
  bit-substring-find-next-set-bit)

(define (bit-string-copy bit-string)
  (let ((result (bit-string-allocate (bit-string-length bit-string))))
    (bit-string-move! result bit-string)
    result))

(define (bit-string-not bit-string)
  (let ((result (bit-string-allocate (bit-string-length bit-string))))
    (bit-string-movec! result bit-string)
    result))

(define (bit-string-or x y)
  (let ((result (bit-string-allocate (bit-string-length x))))
    (bit-string-move! result x)
    (bit-string-or! result y)
    result))

(define (bit-string-and x y)
  (let ((result (bit-string-allocate (bit-string-length x))))
    (bit-string-move! result x)
    (bit-string-and! result y)
    result))

(define (bit-string-andc x y)
  (let ((result (bit-string-allocate (bit-string-length x))))
    (bit-string-move! result x)
    (bit-string-andc! result y)
    result))

(define (bit-string-xor x y)
  (let ((result (bit-string-allocate (bit-string-length x))))
    (bit-string-move! result x)
    (bit-string-xor! result y)
    result))

(define (bit-substring bit-string start end)
  (let ((result (bit-string-allocate (- end start))))
    (bit-substring-move-right! bit-string start end result 0)
    result))

(define (bit-substring-extend string start end length)
  ;; Assumption: (<= (- end start) length)
  (let ((result (make-bit-string length false)))
    (bit-substring-move-right! string start end result 0)
    result))

(define (bit-string-append x y)
  (declare (integrate x y))
  (let ((x-length (bit-string-length x))
	(y-length (bit-string-length y)))
    (let ((result (bit-string-allocate (+ x-length y-length))))
      (bit-substring-move-right! x 0 x-length result 0)
      (bit-substring-move-right! y 0 y-length result x-length)
      result)))

(define (bit-string-append-reversed x y)
  (declare (integrate bit-string-append))
  (bit-string-append y x))

(define (signed-integer->bit-string nbits number)
  (unsigned-integer->bit-string
   nbits
   (cond ((negative? number)
	  (if (>= number (- (expt 2 (-1+ nbits))))
	      (+ number (expt 2 nbits))
	      (error "Integer too small to be encoded" number)))
	 ((< number (expt 2 (-1+ nbits))) number)
	 (else (error "Integer too large to be encoded" number)))))

(define (bit-string->signed-integer bit-string)
  (let ((unsigned-result (bit-string->unsigned-integer bit-string))
	(nbits (bit-string-length bit-string)))
    (if (bit-string-ref bit-string (-1+ nbits))	;Sign bit.
	(- unsigned-result (expt 2 nbits))
	unsigned-result)))