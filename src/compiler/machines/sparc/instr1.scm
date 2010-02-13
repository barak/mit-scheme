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

;;;; MIPS instruction set

;; Branch-tensioned instructions are in instr2.scm
;; Floating point instructions are in instr3.scm

(declare (usual-integrations))

(let-syntax
    ((arithmetic-immediate-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source) (? immediate))
	     (VARIABLE-WIDTH (evaluated-immediate immediate)
	       ((#x-2000 #x1fff)
		(LONG (2 2)
		      (5 destination)
		      (6 ,(caddr form))
		      (5 source)
		      (1 1)
		      (13 evaluated-immediate SIGNED)))
	       ((() ())
		;; SETHI $1, top(immediate)
		;; OR $1, bottom(immediate)
		;; reg-op  $destination, $source, $1
		(LONG (2 0)
		      (5 1)
		      (3 4)
		      (22 evaluated-immediate)	; SETHI
		      (2 2)
		      (5 1)
		      (6 2)
		      (5 1)
		      (1 1)
		      (13 evaluated-immediate SIGNED) ; OR
		      (2 0)
		      (5 destination)
		      (6 ,(caddr form))
		      (5 source)
		      (1 0)
		      (8 0)
		      (5 1)))))))))) ; reg-op
  (arithmetic-immediate-instruction addi 0)
  (arithmetic-immediate-instruction addcci 16)
  (arithmetic-immediate-instruction addxi 8)
  (arithmetic-immediate-instruction addxcci 24)
  (arithmetic-immediate-instruction andi 1)
  (arithmetic-immediate-instruction andcci 17)
  (arithmetic-immediate-instruction andni 5)
  (arithmetic-immediate-instruction andncci 21)
  (arithmetic-immediate-instruction ori 2)
  (arithmetic-immediate-instruction orcci 18)
  (arithmetic-immediate-instruction orni 6)
  (arithmetic-immediate-instruction orncci 22)
  (arithmetic-immediate-instruction xori 3)
  (arithmetic-immediate-instruction xorcci 19)
  (arithmetic-immediate-instruction xnori 7)
  (arithmetic-immediate-instruction xnorcc 23)
  (arithmetic-immediate-instruction subi 4)
  (arithmetic-immediate-instruction subcci 20)
  (arithmetic-immediate-instruction subxi 12)
  (arithmetic-immediate-instruction subxcci 28)
  (arithmetic-immediate-instruction umuli 10)
  (arithmetic-immediate-instruction smuli 11)
  (arithmetic-immediate-instruction umulcci 26)
  (arithmetic-immediate-instruction smulcci 27)
  (arithmetic-immediate-instruction udivi 14)
  (arithmetic-immediate-instruction sdivi 15)
  (arithmetic-immediate-instruction udivcci 30)
  (arithmetic-immediate-instruction sdivcci 31)
  )


(define-instruction lui
  (((? destination) (? immediate))
   (LONG (6 15)
	 (5 0)
	 (5 destination)
	 (16 immediate))))

(define-instruction li
  (((? destination) (? immediate))
   (VARIABLE-WIDTH (evaluated-immediate immediate)
		   ((#x-2000 #x1fff)
		    (LONG (2 2)
			  (5 destination)
			  (6 2)
			  (5 0)
			  (1 1)
			  (13 evaluated-immediate SIGNED)))
		   ((() ())
		    ;; SETHI $1, top(immediate)
		    ;; OR $1, bottom(immediate)
		    (LONG (2 0)
			  (5 1)
			  (3 4)
			  (22 (high-bits evaluated-immediate))	; SETHI
			  (2 2)
			  (5 1)
			  (6 2)
			  (5 1)
			  (1 1)
			  (13 (low-bits evaluated-immediate) SIGNED) ; OR
			  )))))
  

(let-syntax
    ((3-operand-instruction
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source-1) (? source-2))
	     (LONG (2 2)
		   (5 destination)
		   (6 ,(caddr form))
		   (5 source-1)
		   (1 0)
		   (8 0)
		   (5 source-2)
		   )))))))
  (3-operand-instruction add 0)
  (3-operand-instruction addcc 16)
  (3-operand-instruction addx 8)
  (3-operand-instruction addxcc 24)
  (3-operand-instruction andr 1)
  (3-operand-instruction andcc 17)
  (3-operand-instruction andn 5)
  (3-operand-instruction andncc 21)
  (3-operand-instruction orr 2)
  (3-operand-instruction orcc 18)
  (3-operand-instruction orn 6)
  (3-operand-instruction orncc 22)
  (3-operand-instruction xorr 3)
  (3-operand-instruction xorcc 19)
  (3-operand-instruction xnor 7)
  (3-operand-instruction xnorcc 23)
  (3-operand-instruction sllv 37)
  (3-operand-instruction srlv 38)
  (3-operand-instruction srav 39)
  (3-operand-instruction subr 4)
  (3-operand-instruction subcc 20)
  (3-operand-instruction subx 12)
  (3-operand-instruction umul 10)
  (3-operand-instruction smul 11)
  (3-operand-instruction umulcc 26)
  (3-operand-instruction smulcc 27)
  (3-operand-instruction udiv 14)
  (3-operand-instruction sdiv 15)
  (3-operand-instruction udivcc 30)
  (3-operand-instruction sdivcc 31)
  )
  

(let-syntax
    ((shift-instruction-immediate
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(DEFINE-INSTRUCTION ,(cadr form)
	    (((? destination) (? source) (? amount))
	     (LONG (2 2)
		   (5 destination)
		   (6 ,(caddr form))
		   (5 source)
		   (1 1)
		   (8 0)
		   (5 amount)
		   )))))))
  (shift-instruction-immediate sll 37)
  (shift-instruction-immediate srl 38)
  (shift-instruction-immediate sra 39))



(define-instruction jalr
  (((? destination) (? source))
   (LONG (2 2)
	 (5 destination)
	 (6 56)
	 (5 source)
	 (1 0)
	 (8 0)
	 (5 0))))

(define-instruction jr
  (((? source))
   (LONG (2 2)
	 (5 0)
	 (6 56)
	 (5 source)
	 (1 0)
	 (8 0)
	 (5 0))))

(define-instruction jmpl
  (((? destination) (? source1) (? source2))
   (LONG (2 2)
	 (5 destination)
	 (6 56)
	 (5 source1)
	 (1 0)
	 (8 0)
	 (5 source2))))

(define-instruction call
  (((? offset))
   (LONG (2 1)
	 (30 (quotient offset 4) SIGNED))))

(define-instruction sethi
  (((? destination) (? bits))
   (LONG (2 0)
	 (5 destination)
	 (3 4)
	 (22 (top-22-bits bits) UNSIGNED))))
    

;;;; Assembler pseudo-ops

(define-instruction EXTERNAL-LABEL
  ;; External labels provide the garbage collector with header
  ;; information and the runtime system with type, arity, and
  ;; debugging information.
  (((? format-word) (@PCR (? label)))
   (if (eq? endianness 'LITTLE)
       (LONG (16 label BLOCK-OFFSET)
	     (16 format-word UNSIGNED))
       (LONG (16 format-word UNSIGNED)
	     (16 label BLOCK-OFFSET)))))

(define-instruction NOP
  ;; SETHI $0, 0
  (()
   (LONG (2 0)
	 (5 0)
	 (3 4)
	 (22 0))))

(define-instruction LONG
  ((S (? value))
   (LONG (32 value SIGNED)))
  ((U (? value))
   (LONG (32 value UNSIGNED))))