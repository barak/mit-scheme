#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/assmd.scm,v 1.36 1989/08/28 18:33:33 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Assembler Machine Dependencies

(declare (usual-integrations))

(let-syntax ((ucode-type (macro (name) `',(microcode-type name))))

(define-integrable maximum-padding-length
  ;; Instruction length is always a multiple of 16 bits
  16)

(define padding-string
  ;; Pad with ILLEGAL instructions
  (unsigned-integer->bit-string maximum-padding-length #b0100101011111100))

(define-integrable block-offset-width
  ;; Block offsets are always 16 bit words
  16)

(define-integrable maximum-block-offset
  (- (expt 2 block-offset-width) 2))

(define (block-offset->bit-string offset start?)
  (unsigned-integer->bit-string block-offset-width (+ offset (if start? 0 1))))

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
  (let* ((l (bit-string-length bits))
	 (new-position (- position l)))
    (bit-substring-move-right! bits 0 l block new-position)
    (receiver new-position)))

(define instruction-initial-position bit-string-length)
(define-integrable instruction-append bit-string-append-reversed)

;;; end let-syntax
)