#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/instr2.scm,v 1.1 1987/08/14 05:05:08 jinx Exp $

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

;;;; VAX Instruction Set Description, Part 2

;;; The ordering is essentially that in "Vax Architecture Handbook" 1981.

(declare (usual-integrations))

(define-instruction CVT
  ((B W (? src ea-r-b) (? dst ea-w-w))
   (BYTE (8 #x99))
   (OPERAND src)
   (OPERAND dst))

  ((B L (? src ea-r-b) (? dst ea-w-l))
   (BYTE (8 #x98))
   (OPERAND src)
   (OPERAND dst))

  ((W B (? src ea-r-w) (? dst ea-w-b))
   (BYTE (8 #x33))
   (OPERAND src)
   (OPERAND dst))

  ((W L (? src ea-r-w) (? dst ea-w-l))
   (BYTE (8 #x32))
   (OPERAND src)
   (OPERAND dst))

  ((L B (? src ea-r-l) (? dst ea-w-b))
   (BYTE (8 #xF6))
   (OPERAND src)
   (OPERAND dst))

  ((L W (? src ea-r-l) (? dst ea-w-w))
   (BYTE (8 #xF7))
   (OPERAND src)
   (OPERAND dst))

  ((B F (? src ea-r-b) (? dst ea-w-f))
   (BYTE (8 #x4C))
   (OPERAND src)
   (OPERAND dst))

  ((B D (? src ea-r-b) (? dst ea-w-d))
   (BYTE (8 #x6C))
   (OPERAND src)
   (OPERAND dst))

  ((B G (? src ea-r-b) (? dst ea-w-g))
   (BYTE (16 #x4CFD))
   (OPERAND src)
   (OPERAND dst))

  ((B H (? src ea-r-b) (? dst ea-w-h))
   (BYTE (16 #x6CFD))
   (OPERAND src)
   (OPERAND dst))

  ((W F (? src ea-r-w) (? dst ea-w-f))
   (BYTE (8 #x4D))
   (OPERAND src)
   (OPERAND dst))

  ((W D (? src ea-r-w) (? dst ea-w-d))
   (BYTE (8 #x6D))
   (OPERAND src)
   (OPERAND dst))

  ((W G (? src ea-r-w) (? dst ea-w-g))
   (BYTE (16 #x4DFD))
   (OPERAND src)
   (OPERAND dst))

  ((W H (? src ea-r-w) (? dst ea-w-h))
   (BYTE (16 #x6DFD))
   (OPERAND src)
   (OPERAND dst))

  ((L F (? src ea-r-l) (? dst ea-w-f))
   (BYTE (8 #x4E))
   (OPERAND src)
   (OPERAND dst))

  ((L D (? src ea-r-l) (? dst ea-w-d))
   (BYTE (8 #x6E))
   (OPERAND src)
   (OPERAND dst))

  ((L G (? src ea-r-l) (? dst ea-w-g))
   (BYTE (16 #x4EFD))
   (OPERAND src)
   (OPERAND dst))

  ((L H (? src ea-r-l) (? dst ea-w-h))
   (BYTE (16 #x6EFD))
   (OPERAND src)
   (OPERAND dst))

  ((F B (? src ea-r-f) (? dst ea-w-b))
   (BYTE (8 #x48))
   (OPERAND src)
   (OPERAND dst))

  ((D B (? src ea-r-d) (? dst ea-w-b))
   (BYTE (8 #x68))
   (OPERAND src)
   (OPERAND dst))

  ((G B (? src ea-r-g) (? dst ea-w-b))
   (BYTE (16 #x48FD))
   (OPERAND src)
   (OPERAND dst))

  ((H B (? src ea-r-h) (? dst ea-w-b))
   (BYTE (16 #x68FD))
   (OPERAND src)
   (OPERAND dst))

  ((F W (? src ea-r-f) (? dst ea-w-w))
   (BYTE (8 #x49))
   (OPERAND src)
   (OPERAND dst))

  ((D W (? src ea-r-d) (? dst ea-w-w))
   (BYTE (8 #x69))
   (OPERAND src)
   (OPERAND dst))

  ((G W (? src ea-r-g) (? dst ea-w-w))
   (BYTE (16 #x49FD))
   (OPERAND src)
   (OPERAND dst))

  ((H W (? src ea-r-h) (? dst ea-w-w))
   (BYTE (16 #x69FD))
   (OPERAND src)
   (OPERAND dst))

  ((F L T (? src ea-r-f) (? dst ea-w-l))
   (BYTE (8 #x4A))
   (OPERAND src)
   (OPERAND dst))

  ((F L R (? src ea-r-f) (? dst ea-w-l))
   (BYTE (8 #x4B))
   (OPERAND src)
   (OPERAND dst))

  ((D L T (? src ea-r-d) (? dst ea-w-l))
   (BYTE (8 #x6A))
   (OPERAND src)
   (OPERAND dst))

  ((D L R (? src ea-r-d) (? dst ea-w-l))
   (BYTE (8 #x6B))
   (OPERAND src)
   (OPERAND dst))

  ((G L T (? src ea-r-g) (? dst ea-w-l))
   (BYTE (16 #x4AFD))
   (OPERAND src)
   (OPERAND dst))

  ((G L R (? src ea-r-g) (? dst ea-w-l))
   (BYTE (16 #x48FD))
   (OPERAND src)
   (OPERAND dst))

  ((H L T (? src ea-r-h) (? dst ea-w-l))
   (BYTE (16 #x6AFD))
   (OPERAND src)
   (OPERAND dst))

  ((H L R (? src ea-r-h) (? dst ea-w-l))
   (BYTE (16 #x6BFD))
   (OPERAND src)
   (OPERAND dst))

  ((F D (? src ea-r-f) (? dst ea-w-d))
   (BYTE (8 #x56))
   (OPERAND src)
   (OPERAND dst))

  ((F G (? src ea-r-f) (? dst ea-w-g))
   (BYTE (16 #x99FD))
   (OPERAND src)
   (OPERAND dst))

  ((F H (? src ea-r-f) (? dst ea-w-h))
   (BYTE (16 #x98FD))
   (OPERAND src)
   (OPERAND dst))

  ((D F (? src ea-r-d) (? dst ea-w-f))
   (BYTE (16 #x76))
   (OPERAND src)
   (OPERAND dst))

  ((D H (? src ea-r-d) (? dst ea-w-h))
   (BYTE (16 #x32FD))
   (OPERAND src)
   (OPERAND dst))

  ((G F (? src ea-r-g) (? dst ea-w-f))
   (BYTE (16 #x33FD))
   (OPERAND src)
   (OPERAND dst))

  ((G H (? src ea-r-g) (? dst ea-w-h))
   (BYTE (16 #x56FD))
   (OPERAND src)
   (OPERAND dst))

  ((H F (? src ea-r-h) (? dst ea-w-f))
   (BYTE (16 #xF6FD))
   (OPERAND src)
   (OPERAND dst))

  ((H D (? src ea-r-h) (? dst ea-w-d))
   (BYTE (16 #xF7FD))
   (OPERAND src)
   (OPERAND dst))

  ((H G (? src ea-r-h) (? dst ea-w-g))
   (BYTE (16 #x76FD))
   (OPERAND src)
   (OPERAND dst)))

(define-instruction CMP
  ((B (? src1 ea-r-b) (? src2 ea-r-b))
   (BYTE (8 #x91))
   (OPERAND src1)
   (OPERAND src2))

  ((W (? src1 ea-r-w) (? src2 ea-r-w))
   (BYTE (8 #xB1))
   (OPERAND src1)
   (OPERAND src2))

  ((L (? src1 ea-r-l) (? src2 ea-r-l))
   (BYTE (8 #xD1))
   (OPERAND src1)
   (OPERAND src2))

  ((F (? src1 ea-r-f) (? src2 ea-r-f))
   (BYTE (8 #x51))
   (OPERAND src1)
   (OPERAND src2))

  ((D (? src1 ea-r-d) (? src2 ea-r-d))
   (BYTE (8 #x71))
   (OPERAND src1)
   (OPERAND src2))

  ((G (? src1 ea-r-g) (? src2 ea-r-g))
   (BYTE (16 #x51FD))
   (OPERAND src1)
   (OPERAND src2))

  ((H (? src1 ea-r-h) (? src2 ea-r-h))
   (BYTE (16 #x71FD))
   (OPERAND src1)
   (OPERAND src2)))

(define-instruction MOVZ
  ((B W (? src ea-r-b) (? dst ea-w-w))
   (BYTE (8 #x9B))
   (OPERAND src)
   (OPERAND dst))

  ((B L (? src ea-r-b) (? dst ea-w-l))
   (BYTE (8 #x9A))
   (OPERAND src)
   (OPERAND dst))

  ((W L (? src ea-r-w) (? dst ea-w-l))
   (BYTE (8 #x3C))
   (OPERAND src)
   (OPERAND dst)))

(define-instruction TST
  ((B (? src ea-r-b))
   (BYTE (8 #x95))
   (OPERAND src))

  ((W (? src ea-r-w))
   (BYTE (8 #xB5))
   (OPERAND src))

  ((L (? src ea-r-l))
   (BYTE (8 #xD5))
   (OPERAND src))

  ((F (? src ea-r-f))
   (BYTE (8 #x53))
   (OPERAND src))

  ((D (? src ea-r-d))
   (BYTE (8 #x73))
   (OPERAND src))

  ((G (? src ea-r-g))
   (BYTE (16 #x53FD))
   (OPERAND src))

  ((H (? src ea-r-h))
   (BYTE (16 #x73FD))
   (OPERAND src)))

(let-syntax
    ((define-arithmetic
       (macro (name digit)
	 `(define-instruction ,name
	    ((B (? op ea-r-b) (? res ea-m-b))
	     (BYTE (8 ,(+ #x80 digit)))
	     (OPERAND op)
	     (OPERAND res))

	    ((B (? op1 ea-r-b) (? op2 ea-r-b) (? res ea-w-b))
	     (BYTE (8 ,(+ #x81 digit)))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((W (? op ea-r-w) (? res ea-m-w))
	     (BYTE (8 ,(+ #xA0 digit)))
	     (OPERAND op)
	     (OPERAND res))

	    ((W (? op1 ea-r-w) (? op2 ea-r-w) (? res ea-w-w))
	     (BYTE (8 ,(+ #xA1 digit)))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((L (? op ea-r-l) (? res ea-m-l))
	     (BYTE (8 ,(+ #xC0 digit)))
	     (OPERAND op)
	     (OPERAND res))

	    ((L (? op1 ea-r-l) (? op2 ea-r-l) (? res ea-w-l))
	     (BYTE (8 ,(+ #xC1 digit)))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((F (? op ea-r-f) (? res ea-m-f))
	     (BYTE (8 ,(+ #x40 digit)))
	     (OPERAND op)
	     (OPERAND res))

	    ((F (? op1 ea-r-f) (? op2 ea-r-f) (? res ea-w-f))
	     (BYTE (8 ,(+ #x41 digit)))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((D (? op ea-r-d) (? res ea-m-d))
	     (BYTE (8 ,(+ #x60 digit)))
	     (OPERAND op)
	     (OPERAND res))

	    ((D (? op1 ea-r-d) (? op2 ea-r-d) (? res ea-w-d))
	     (BYTE (8 ,(+ #x61 digit)))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((G (? op ea-r-g) (? res ea-m-g))
	     (BYTE (16 ,(+ #x40FD (* digit #x100))))
	     (OPERAND op)
	     (OPERAND res))

	    ((G (? op1 ea-r-g) (? op2 ea-r-g) (? res ea-w-g))
	     (BYTE (16 ,(+ #x41FD (* digit #x100))))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))

	    ((H (? op ea-r-h) (? res ea-m-h))
	     (BYTE (16 ,(+ #x60FD (* digit #x100))))
	     (OPERAND op)
	     (OPERAND res))

	    ((H (? op1 ea-r-h) (? op2 ea-r-h) (? res ea-w-h))
	     (BYTE (16 ,(+ #x61FD (* digit #x100))))
	     (OPERAND op1)
	     (OPERAND op2)
	     (OPERAND res))))))

  (define-arithmetic ADD #x0)
  (define-arithmetic SUB #x2)
  (define-arithmetic MUL #x4)
  (define-arithmetic DIV #x6))

(define-instruction ADAWI
  (((? add ea-r-w) (? sum m w))
   (BYTE (8 #x58))
   (OPERAND add)
   (OPERAND sum)))

(define-instruction INC
  ((B (? sum ea-m-b))
   (BYTE (8 #x96))
   (OPERAND sum))

  ((W (? sum ea-m-w))
   (BYTE (8 #xB6))
   (OPERAND sum))

  ((L (? sum ea-m-l))
   (BYTE (8 #xD6))
   (OPERAND sum)))

(define-instruction DEC
  ((B (? dif ea-m-b))
   (BYTE (8 #x97))
   (OPERAND dif))

  ((W (? dif ea-m-w))
   (BYTE (8 #xB7))
   (OPERAND dif))

  ((L (? dif ea-m-l))
   (BYTE (8 #xD7))
   (OPERAND dif)))

(define-instruction ADWC
  (((? add ea-r-l) (? sum m l))
   (BYTE (8 #xD8))
   (OPERAND add)
   (OPERAND sum)))

(define-instruction SBWC
  (((? sub ea-r-l) (? dif m l))
   (BYTE (8 #xD9))
   (OPERAND sub)
   (OPERAND dif)))

(define-instruction EMUL
  (((? mul1 ea-r-l) (? mul2 ea-r-l) (? add ea-r-l) (? prod ea-w-q))
   (BYTE (8 #x7A))
   (OPERAND mul1)
   (OPERAND mul2)
   (OPERAND add)
   (OPERAND prod)))

(define-instruction EDIV
  (((? divr ea-r-l) (? divd ea-r-q) (? quo ea-w-l) (? rem ea-w-l))
   (BYTE (8 #x7B))
   (OPERAND divr)
   (OPERAND divd)
   (OPERAND quo)
   (OPERAND rem)))

(define-instruction EMOD
  ((F (? mulr ea-r-f) (? mulrx ea-r-b) (? muld ea-r-f)
      (? int ea-w-l) (? fract ea-w-f))
   (BYTE (8 #x54))
   (OPERAND mulr)
   (OPERAND mulrx)
   (OPERAND muld)
   (OPERAND int)
   (OPERAND fract))

  ((D (? mulr ea-r-d) (? mulrx ea-r-b) (? muld ea-r-d)
      (? int ea-w-l) (? fract ea-w-d))
   (BYTE (8 #x74))
   (OPERAND mulr)
   (OPERAND mulrx)
   (OPERAND muld)
   (OPERAND int)
   (OPERAND fract))

  ((G (? mulr ea-r-g) (? mulrx ea-r-w) (? muld ea-r-g)
      (? int ea-w-l) (? fract ea-w-g))
   (BYTE (16 #x54FD))
   (OPERAND mulr)
   (OPERAND mulrx)
   (OPERAND muld)
   (OPERAND int)
   (OPERAND fract))

  ((H (? mulr ea-r-h) (? mulrx ea-r-w) (? muld ea-r-h)
      (? int ea-w-l) (? fract ea-w-h))
   (BYTE (16 #x74FD))
   (OPERAND mulr)
   (OPERAND mulrx)
   (OPERAND muld)
   (OPERAND int)
   (OPERAND fract)))

(define-instruction BIT
  ((B (? mask ea-r-b) (? src ea-r-b))
   (BYTE (8 #x93))
   (OPERAND mask)
   (OPERAND src))

  ((W (? mask ea-r-w) (? src ea-r-w))
   (BYTE (8 #xB3))
   (OPERAND mask)
   (OPERAND src))

  ((L (? mask ea-r-l) (? src ea-r-l))
   (BYTE (8 #xD3))
   (OPERAND mask)
   (OPERAND src)))

(let-syntax
    ((define-bitwise
       (macro (name opcode)
	 `(define-instruction ,name
	    ((B (? mask ea-r-b) (? dst ea-m-b))
	     (BYTE (8 ,(+ #x80 opcode)))
	     (OPERAND mask)
	     (OPERAND dst))

	    ((B (? mask ea-r-b) (? src ea-r-b) (? dst ea-w-b))
	     (BYTE (8 ,(+ #x81 opcode)))
	     (OPERAND mask)
	     (OPERAND src)
	     (OPERAND dst))

	    ((W (? mask ea-r-w) (? dst ea-m-w))
	     (BYTE (8 ,(+ #xA0 opcode)))
	     (OPERAND mask)
	     (OPERAND dst))

	    ((W (? mask ea-r-w) (? src ea-r-w) (? dst ea-w-w))
	     (BYTE (8 ,(+ #xA1 opcode)))
	     (OPERAND mask)
	     (OPERAND src)
	     (OPERAND dst))
	    
	    ((L (? mask ea-r-l) (? dst ea-m-l))
	     (BYTE (8 ,(+ #xC0 opcode)))
	     (OPERAND mask)
	     (OPERAND dst))

	    ((L (? mask ea-r-l) (? src ea-r-l) (? dst ea-w-l))
	     (BYTE (8 ,(+ #xC1 opcode)))
	     (OPERAND mask)
	     (OPERAND src)
	     (OPERAND dst))))))

  (define-bitwise BIS #x8)
  (define-bitwise BIC #xA)
  (define-bitwise XOR #xC))
