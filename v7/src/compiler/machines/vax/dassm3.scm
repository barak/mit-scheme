#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/dassm3.scm,v 1.1 1988/01/07 16:48:49 bal Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; VAX Disassembler

(declare (usual-integrations))

;;; Insides of the disassembler

(define (make-fetcher size-in-bits)
  (let ((size-in-bytes (quotient size-in-bits 8)))
    (lambda ()
      (let ((word (bit-string-allocate size-in-bits)))
	(with-interrupt-mask interrupt-mask-none
          (lambda (old)
	    (read-bits! (+ (primitive-datum *block) *current-offset) 0 word)))
	(set! *current-offset (+ *current-offset size-in-bytes))
	word))))

(define get-byte (make-fetcher 8))
(define get-word (make-fetcher 16))
(define get-longword (make-fetcher 32))

(define (get-immediate-byte)
  (extract+ (get-byte) 0 8))

(define (get-immediate-word)
  (extract+ (get-word) 0 16))

(define (get-immediate-longword)
  (extract+ (get-longword) 0 32))

(define-integrable (extract bit-string start end)
  (bit-string->unsigned-integer (bit-substring bit-string start end)))

(define-integrable (extract+ bit-string start end)
  (bit-string->signed-integer (bit-substring bit-string start end)))

;;;; Instruction decoding

(define opcode-dispatch
  (vector-cons 256 undefined-instruction))

(define secondary-opcode-dispatch
  (vector-cons 256 undefined-instruction))

(define (define-standard-instruction opcode handler)
  (vector-set! opcode-dispatch opcode handler))

(define (define-extended-instruction opcode handler)
  (vector-set! secondary-opcode-dispatch opcode handler))

(define-standard-instruction #xFD
  (lambda ()
    ((vector-ref secondary-opcode-dispatch (get-immediate-byte)))))

(define (define-branch-instruction opcode prefix size)
  (define-standard-instruction opcode
    (lambda ()
      (append prefix (list (decode-displacement size))))))

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

;;;; Operand decoding

(define (decode-displacement size)
  (case size
    ((8) (make-pc-relative false 'B (get-immediate-byte)))
    ((16) (make-pc-relative false 'W (get-immediate-word)))
    ((32) (make-pc-relative false 'L (get-immediate-longword)))
    (else (error "decode-displacement: bad size" size))))

(define (decode-operand size)
  (let ((*or* (get-byte)))
    ((vector-ref operand-dispatch (extract *or* 4 8))
     *or* size)))

(define (short-literal *or* *os*)
  `(S ,(extract *or* 0 6)))

(define operand-dispatch
  (vector-cons 16 short-literal))

(define (define-operand! mode handler)
  (vector-set! operand-dispatch mode handler))

(define (define-standard-operand! mode if-reg if-pc)
  (define-operand! mode
    (lambda (*or* *os*)
      (let ((reg (extract *or* 0 4)))
	(if (= #xF reg)
	    (if-pc *os*)
	    (if-reg reg))))))

(define (define-simple-operand! mode keyword)
  (define-operand! mode
    (lambda (*or* *os*)
      `(,keyword ,(make-register (extract *or* 0 4))))))

(define (define-offset-operand! mode deferred? size get)
  (define-standard-operand! mode
    (lambda (reg)
      (make-offset deferred? reg size (get)))
    (lambda (*os*)
      (make-pc-relative deferred? size (get)))))

;;;; Actual operand handlers (except short literal, above).

(define-operand! 4			;index mode
  (lambda (*or* *os*)
    (let ((index-reg (extract *or* 0 4)))
      `(X ,index-reg ,(decode-operand *os*)))))

(define-simple-operand! 5 'R)		;register
(define-simple-operand! 6 '@R)		;register deferred
(define-simple-operand! 7 '@-R)		;autodecrement

(define-standard-operand! 8		;autoincrement
  (lambda (reg)
    `(@R+ ,(make-register reg)))
  (lambda (*os*)			;immediate
    `(&
      ,(case *os*
	 ((B) (get-immediate-byte))
	 ((W) (get-immediate-word))
	 ((L) (get-immediate-longword))))))

(define-standard-operand! 9		;autoincrement deferred
  (lambda (reg)
    `(@@R+ ,(make-register reg)))
  (lambda (*os*)			;absolute
    `(@& , (extract+ (get-longword) 0 32))))

(define-offset-operand! 10 false 'B get-immediate-byte)
(define-offset-operand! 11 true 'B get-immediate-byte)
(define-offset-operand! 12 false 'W get-immediate-word)
(define-offset-operand! 13 true 'W get-immediate-word)
(define-offset-operand! 15 false 'L get-immediate-longword)
(define-offset-operand! 15 true 'L get-immediate-longword)