#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/bitstr.scm,v 14.2 1990/01/17 19:06:35 jinx Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Bit String Primitives
;;; package: ()

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