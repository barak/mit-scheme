#| -*-Scheme-*-

$Id: dbca90829377a64d042eb7e5828322f5852263bd $
$MC68020-Header: dassm3.scm,v 4.6 88/08/29 22:40:41 GMT cph Exp $

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

;;;; VAX Disassembler: Internals

(declare (usual-integrations))

;;;; Bit String Manipulation

(define (make-fetcher size-in-bits)
  (let ((size-in-bytes (quotient size-in-bits 8)))
    (lambda ()
      (let ((word (read-bits *current-offset size-in-bits)))
	(set! *current-offset (+ *current-offset size-in-bytes))
	word))))

(define get-byte (make-fetcher 8))
(define get-word (make-fetcher 16))
(define get-longword (make-fetcher 32))

(define-integrable (get-immediate-byte)
  (extract+ (get-byte) 0 8))

(define-integrable (get-immediate-word)
  (extract+ (get-word) 0 16))

(define-integrable (get-immediate-longword)
  (extract+ (get-longword) 0 32))

(define-integrable (extract bit-string start end)
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

(define-integrable (extract+ bit-string start end)
  (bit-string->signed-integer (bit-substring bit-string start end)))

;;;; Operand decoding

(define operand-dispatch
  (let ((short-literal
	 (lambda (*or* *os*)
	   *os*				; ignored
	   `(S ,(extract *or* 0 6))))
	(index-operand
	 (lambda (*or* *os*)
	   (let ((index-reg (extract *or* 0 4)))
	     `(X ,index-reg ,(decode-operand *os*)))))
	(standard-operand
	 (lambda (if-reg if-pc)
	   (lambda (*or* *os*)
	     (let ((reg (extract *or* 0 4)))
	       (if (= #xF reg)
		   (if-pc *os*)
		   (if-reg reg))))))
	(simple-operand
	 (lambda (keyword)
	   (lambda (*or* *os*)
	     *os*			; ignored
	     `(,keyword ,(make-register (extract *or* 0 4)))))))
    (let ((offset-operand
	   (lambda (deferred? size get)
	     (standard-operand
	      (lambda (reg)
		(make-offset deferred? reg size (get)))
	      (lambda (*os*)
		*os*			; ignored
		(make-pc-relative deferred? size (get)))))))
      (vector
       short-literal			;0 short immediate
       short-literal			;1 "     "
       short-literal			;2 "     "
       short-literal			;3 "     "
       index-operand			;4 indexed
       (simple-operand 'R)		;5 register
       (simple-operand '@R)		;6 register deferred
       (simple-operand '@-R)		;7 autodecrement
       (standard-operand		;8 autoincrement/immediate
	(lambda (reg)
	  `(@R+ ,(make-register reg)))
	(lambda (*os*)
	  `(&
	    ,(case *os*
	       ((B) (get-immediate-byte))
	       ((W) (get-immediate-word))
	       ((L) (get-immediate-longword))))))
       (standard-operand		;9 autoincrement deferred/absolute
	(lambda (reg)
	  `(@@R+ ,(make-register reg)))
	(lambda (*os*)
	  *os*				; ignored
	  `(@& , (extract+ (get-longword) 0 32))))
       (offset-operand false 'B		;a byte offset
		       get-immediate-byte)
       (offset-operand true 'B		;b byte offset deferred
		       get-immediate-byte)
       (offset-operand false 'W		;c word offset
		       get-immediate-word)
       (offset-operand true 'W		;d word offset deferred
		       get-immediate-word)
       (offset-operand false 'L		;e long offset
		       get-immediate-longword)
       (offset-operand true 'L		;f long offset deferred
		       get-immediate-longword)))))

;;;; Instruction decoding

(define (decode-operand size)
  (let ((*or* (get-byte)))
    ((vector-ref operand-dispatch (extract *or* 4 8))
     *or* size)))

(define (decode-displacement size)
  (case size
    ((8) (make-pc-relative false 'B (get-immediate-byte)))
    ((16) (make-pc-relative false 'W (get-immediate-word)))
    ((32) (make-pc-relative false 'L (get-immediate-longword)))
    (else (error "decode-displacement: bad size" size))))

(define opcode-dispatch
  (make-vector 256 undefined-instruction))

(define secondary-opcode-dispatch
  (make-vector 256 undefined-instruction))

(define (define-standard-instruction opcode handler)
  (vector-set! opcode-dispatch opcode handler))

(define (define-extended-instruction opcode handler)
  (vector-set! secondary-opcode-dispatch opcode handler))

(define-standard-instruction #xFD
  (lambda ()
    ((vector-ref secondary-opcode-dispatch (get-immediate-byte)))))

;; Most of the instructions decoders are generated from from the
;; assembler tables, but branch instructions are treated separately.

(define (displacement-decoder size)
  (define (make-decoder keyword getter)
    (lambda ()
      (make-pc-relative false keyword (getter))))

  (case size
    ((8) (make-decoder 'B get-immediate-byte))
    ((16) (make-decoder 'W get-immediate-word))
    ((32) (make-decoder 'L get-immediate-longword))
    (else (error "displacement-decoder: bad size" size))))

(define (define-branch-instruction opcode prefix size)
  (let ((decoder (displacement-decoder size)))
    (define-standard-instruction opcode
      (lambda ()
	`(,@prefix ,(decoder))))))

;; Conditional branches

(define-branch-instruction #x12 '(B B NEQ) 8)
(define-branch-instruction #x13 '(B B EQL) 8)
(define-branch-instruction #x14 '(B B GTR) 8)
(define-branch-instruction #x15 '(B B LEQ) 8)
(define-branch-instruction #x18 '(B B GEQ) 8)
(define-branch-instruction #x19 '(B B LSS) 8)
(define-branch-instruction #x1A '(B B GTRU) 8)
(define-branch-instruction #x1B '(B B LEQU) 8)
(define-branch-instruction #x1C '(B B VC) 8)
(define-branch-instruction #x1D '(B B VS) 8)
(define-branch-instruction #x1E '(B B CC) 8)
(define-branch-instruction #x1F '(B B CS) 8)

;; Unconditional branches

(define-branch-instruction #x11 '(BR B) 8)
(define-branch-instruction #x31 '(BR W) 16)
(define-branch-instruction #x10 '(BSB B) 8)
(define-branch-instruction #x30 '(BSB W) 16)

