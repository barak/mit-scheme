#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/assmd.scm,v 4.5 1989/05/17 20:27:46 jinx Rel $
$MC68020-Header: assmd.scm,v 1.35 88/08/31 05:55:31 GMT cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; Assembler Machine Dependencies.  DEC Vax version

(declare (usual-integrations))

(let-syntax ((fold
	      (macro (expression)
		(eval expression system-global-environment))))

(define-integrable addressing-granularity 8)
(define-integrable scheme-object-width 32)
(define-integrable endianness 'LITTLE)

(define-integrable maximum-padding-length
  ;; Instructions can be any number of bytes long.
  ;; Thus the maximum padding is 3 bytes.
  24)

(define-integrable padding-string
  ;; Pad with HALT instructions
  (fold (unsigned-integer->bit-string 8 #x00)))

(define-integrable block-offset-width
  ;; Block offsets are encoded words
  16)

(define maximum-block-offset
  (fold (- (expt 2 15) 1)))

(define-integrable (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width
				(+ (* 2 offset)
				   (if start? 0 1))))

(define-integrable nmv-type-string
  (fold (unsigned-integer->bit-string 8 (microcode-type 'MANIFEST-NM-VECTOR))))

(define (make-nmv-header n)
  (bit-string-append (unsigned-integer->bit-string 24 n) nmv-type-string))

(define (object->bit-string object)
  (bit-string-append
   (unsigned-integer->bit-string 24 (primitive-datum object))
   (unsigned-integer->bit-string 8 (primitive-type object))))

;;; Machine dependent instruction order

(define-integrable (instruction-initial-position block)
  block					; ignored
  0)

(define (instruction-insert! bits block position receiver)
  (let ((l (bit-string-length bits)))
    (bit-substring-move-right! bits 0 l block position)
    (receiver (+ position l))))

(define-integrable instruction-append
  bit-string-append)

;;; end let-syntax
)