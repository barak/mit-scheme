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

;;;; SVM assembler rules

;;; Primitive types:
;;; unsigned-8
;;; unsigned-16
;;; unsigned-32
;;; signed-8
;;; signed-16
;;; signed-32
;;; float
;;; register
;;; word-register
;;; float-register
;;; type-word (6-bit unsigned)

;;; Names consist of symbols drawn from a restricted character set.
;;; The allowed characters are alphanumerics, hyphen, question mark,
;;; and exclamation point.  The first character must be a letter.
;;; When forming a C name for an opcode, hyphen is rewritten as
;;; underscore, question mark as "_p", and exclamation-point as "_x".

(define-abbreviation (define-implicit-enumeration symbol + symbol)
  (lambda (form)
    `((DEFINE-IMPLICIT-CODING-TYPE ,(cadr form)
	,@(map (lambda (keyword)
		 `(DEFINE-CODE-SEQUENCE ,keyword))
	       (cddr form))))))

(define-abbreviation (define-explicit-enumeration symbol + symbol)
  (lambda (form)
    `((DEFINE-EXPLICIT-CODING-TYPE ,(cadr form) (8 1)
	,@(map (lambda (keyword)
		 `(DEFINE-CODE-SEQUENCE ,keyword))
	       (cddr form))))))

(define-implicit-enumeration scale-factor
  byte
  word
  float)

(define-implicit-coding-type unsigned-integer

  (define-code-sequence (_ value unsigned-8)
    value)

  (define-code-sequence (_ value unsigned-16)
    value)

  (define-code-sequence (_ value unsigned-32)
    value))

(define-implicit-coding-type signed-integer

  (define-code-sequence (_ value signed-8)
    value)

  (define-code-sequence (_ value signed-16)
    value)

  (define-code-sequence (_ value signed-32)
    value))

(define-keyword-abbreviation byte b)
(define-keyword-abbreviation word w)
(define-keyword-abbreviation float f)
(define-keyword-abbreviation indirect indir)
(define-keyword-abbreviation indexed index)
(define-keyword-abbreviation pre-decrement predec)
(define-keyword-abbreviation pre-increment preinc)
(define-keyword-abbreviation post-decrement postdec)
(define-keyword-abbreviation post-increment postinc)
(define-keyword-abbreviation pc-relative pcr)
(define-keyword-abbreviation instruction inst)
(define-keyword-abbreviation address addr)
(define-keyword-abbreviation indirect-jump ijump)
(define-keyword-abbreviation indirect-call icall)
(define-keyword-abbreviation conditional-jump cjump)

(define-explicit-coding-type address (8 1)

  (define-code-sequence (indirect (_ base word-register))
    base)

  (define-code-sequence (offset (_ base word-register)
				(_ offset unsigned-8)
				(_ oscale scale-factor))
    base
    offset
    oscale)

  (define-code-sequence (indexed (_ base word-register)
				 (_ offset unsigned-8)
				 (_ oscale scale-factor)
				 (_ index word-register)
				 (_ iscale scale-factor))
    base
    offset
    oscale
    index
    iscale)

  (define-code-sequence (pre-decrement (_ base word-register)
				       (_ scale scale-factor))
    base
    scale)

  (define-code-sequence (pre-increment (_ base word-register)
				       (_ scale scale-factor))
    base
    scale)

  (define-code-sequence (post-decrement (_ base word-register)
					(_ scale scale-factor))
    base
    scale)

  (define-code-sequence (post-increment (_ base word-register)
					(_ scale scale-factor))
    base
    scale)

  (define-code-sequence (pc-relative (_ n signed-integer))
    n))

(define-explicit-coding-type instruction (8 1))

(define-code-sequence instruction
  (store byte
	 (_ source word-register)
	 (_ target address))
  source
  target)

(define-code-sequence instruction
  (store word
	 (_ source word-register)
	 (_ target address))
  source
  target)

(define-code-sequence instruction
  (store float
	 (_ source float-register)
	 (_ target address))
  source
  target)

(define-code-sequence instruction
  (load byte
	(_ target word-register)
	(_ source address))
  target
  source)

(define-code-sequence instruction
  (load word
	(_ target word-register)
	(_ source address))
  target
  source)

(define-code-sequence instruction
  (load float
	(_ target float-register)
	(_ source address))
  target
  source)

(define-code-sequence instruction
  (load-address (_ target word-register)
		(_ source address))
  target
  source)

(define-code-sequence instruction
  (load-immediate (_ target word-register)
		  (_ value signed-integer))
  target
  value)

(define-code-sequence instruction
  (load-immediate (_ target word-register)
		  (_ value unsigned-integer))
  target
  value)

(define-code-sequence instruction
  (load-immediate (_ target float-register)
		  (_ value float))
  target
  value)

(define-code-sequence instruction
  (copy-block (_ size unsigned-8)
	      word
	      (_ from word-register)
	      (_ to word-register))
  to
  from
  size)

(define-code-sequence instruction
  (copy-block (_ size word-register)
	      word
	      (_ from word-register)
	      (_ to word-register))
  to
  from
  size)

(define-implicit-coding-type type-operand
  (define-code-sequence (_ type type-word)
    type)
  (define-code-sequence (_ source word-register)
    source))

(define-code-sequence instruction
  (load-non-pointer (_ target word-register)
		    (_ type type-operand)
		    (_ datum unsigned-integer))
  target
  type
  datum)

(define-code-sequence instruction
  (load-non-pointer (_ target word-register)
		    (_ type type-operand)
		    (_ datum word-register))
  target
  type
  datum)

(define-code-sequence instruction
  (load-pointer (_ target word-register)
		(_ type type-operand)
		(_ address word-register))
  target
  type
  address)

(define-code-sequence instruction
  (jump (pc-relative (_ offset signed-integer)))
  offset)

(define-code-sequence instruction
  (jump (indirect (_ address word-register)))
  address)

(define-code-sequence instruction
  (indirect-jump (pc-relative (_ offset unsigned-integer)))
  offset)

(define-code-sequence instruction
  (indirect-call (pc-relative (_ offset unsigned-integer)))
  offset)

(define-code-sequence instruction
  (enter-closure (_ index unsigned-16))
  index)

(define-code-sequence instruction
  (conditional-jump (_ condition word-condition-1)
		    (_ source1 word-register)
		    (_ source2 word-register)
		    (pc-relative (_ offset signed-integer)))
  condition
  source1
  source2
  offset)

(define-implicit-enumeration word-condition-1
  eq neq
  lt ge
  gt le
  slt sge
  sgt sle)

(define-code-sequence instruction
  (conditional-jump (_ condition word-condition-2)
		    (_ source word-register)
		    (pc-relative (_ offset signed-integer)))
  condition
  source
  offset)

(define-implicit-enumeration word-condition-2
  eq neq
  slt sge
  sgt sle)

(define-code-sequence instruction
  (conditional-jump (_ condition fixnum-condition)
		    (_ source word-register)
		    (pc-relative (_ offset signed-integer)))
  condition
  source
  offset)

(define-implicit-enumeration fixnum-condition
  fix nfix
  ifix nifix)

(define-code-sequence instruction
  (conditional-jump (_ condition float-condition)
		    (_ source1 float-register)
		    (_ source2 float-register)
		    (pc-relative (_ offset signed-integer)))
  condition
  source1
  source2
  offset)

(define-code-sequence instruction
  (conditional-jump (_ condition float-condition)
		    (_ source float-register)
		    (pc-relative (_ offset signed-integer)))
  condition
  source
  offset)

(define-implicit-enumeration float-condition
  eq neq
  lt gt
  le ge
  cmp ncmp)

(define-code-sequence instruction
  (trap (_ code trap-0))
  code)

(define-explicit-enumeration trap-0
  add
  decrement
  divide
  equal?
  greater?
  increment
  less?
  modulo
  multiply
  negative?
  operator-1-0
  operator-2-0
  operator-2-1
  operator-3-0
  operator-3-1
  operator-3-2
  operator-4-0
  operator-4-1
  operator-4-2
  operator-4-3
  operator-apply
  operator-lexpr
  operator-lookup
  operator-primitive
  positive?
  quotient
  reflect-to-interface
  remainder
  return-to-interpreter
  subtract
  zero?)

(define-code-sequence instruction
  (trap (_ code trap-1)
	(_ arg0 word-register))
  code
  arg0)

(define-explicit-enumeration trap-1
  error
  lookup
  primitive-apply
  primitive-lexpr-apply
  safe-lookup
  unassigned?)

(define-code-sequence instruction
  (trap (_ code trap-2)
	(_ arg0 word-register)
	(_ arg1 word-register))
  code
  arg0
  arg1)

(define-explicit-enumeration trap-2
  apply
  assignment
  lexpr-apply
  primitive-error)

(define-code-sequence instruction
  (trap (_ code trap-3)
	(_ arg0 word-register)
	(_ arg1 word-register)
	(_ arg2 word-register))
  code
  arg0
  arg1
  arg2)

(define-explicit-enumeration trap-3
  cache-reference-apply
  link)

(define-code-sequence instruction (interrupt-test-procedure))
(define-code-sequence instruction (interrupt-test-dynamic-link))
(define-code-sequence instruction (interrupt-test-closure))
(define-code-sequence instruction (interrupt-test-ic-procedure))
(define-code-sequence instruction (interrupt-test-continuation))

(define-code-sequence instruction
  (flonum-header (_ target word-register)
		 (_ n-elts unsigned-integer))
  target
  n-elts)

(define-code-sequence instruction
  (flonum-header (_ target word-register)
		 (_ n-elts word-register))
  target
  n-elts)

(define-code-sequence instruction
  (datum-u8 (_ datum unsigned-8))
  (no-code)
  datum)

(define-code-sequence instruction
  (datum-u16 (_ datum unsigned-16))
  (no-code)
  datum)

(define-code-sequence instruction
  (datum-u32 (_ datum unsigned-32))
  (no-code)
  datum)

(define-code-sequence instruction
  (datum-s8 (_ datum signed-8))
  (no-code)
  datum)

(define-code-sequence instruction
  (datum-s16 (_ datum signed-16))
  (no-code)
  datum)

(define-code-sequence instruction
  (datum-s32 (_ datum signed-32))
  (no-code)
  datum)

(define-abbreviation (define-generic-unary-instruction symbol)
  (lambda (form)
    (let ((name (cadr form)))
      `((DEFINE-CODE-SEQUENCE INSTRUCTION
	  (,name (_ TARGET WORD-REGISTER)
		 (_ SOURCE WORD-REGISTER))
	  TARGET
	  SOURCE)
	(DEFINE-CODE-SEQUENCE INSTRUCTION
	  (,name (_ TARGET FLOAT-REGISTER)
		 (_ SOURCE FLOAT-REGISTER))
	  TARGET
	  SOURCE)))))

(define-generic-unary-instruction copy)
(define-generic-unary-instruction negate)
(define-generic-unary-instruction increment)
(define-generic-unary-instruction decrement)
(define-generic-unary-instruction abs)

(define-abbreviation (define-word-unary-instruction symbol)
  (lambda (form)
    `((DEFINE-CODE-SEQUENCE INSTRUCTION
	(,(cadr form) (_ TARGET WORD-REGISTER)
		      (_ SOURCE WORD-REGISTER))
	TARGET
	SOURCE))))

(define-word-unary-instruction object-type)
(define-word-unary-instruction object-datum)
(define-word-unary-instruction object-address)
(define-word-unary-instruction fixnum->integer)
(define-word-unary-instruction integer->fixnum)
(define-word-unary-instruction not)
(define-word-unary-instruction flonum-align)
(define-word-unary-instruction flonum-length)

(define-abbreviation (define-float-unary-instruction symbol)
  (lambda (form)
    `((DEFINE-CODE-SEQUENCE INSTRUCTION
	(,(cadr form) (_ TARGET FLOAT-REGISTER)
		      (_ SOURCE FLOAT-REGISTER))
	TARGET
	SOURCE))))

(define-float-unary-instruction sqrt)
(define-float-unary-instruction round)
(define-float-unary-instruction ceiling)
(define-float-unary-instruction floor)
(define-float-unary-instruction truncate)
(define-float-unary-instruction log)
(define-float-unary-instruction exp)
(define-float-unary-instruction cos)
(define-float-unary-instruction sin)
(define-float-unary-instruction tan)
(define-float-unary-instruction acos)
(define-float-unary-instruction asin)
(define-float-unary-instruction atan)

(define-abbreviation (define-generic-binary-instruction symbol)
  (lambda (form)
    (let ((name (cadr form)))
      `((DEFINE-CODE-SEQUENCE INSTRUCTION
	  (,name (_ TARGET WORD-REGISTER)
		 (_ SOURCE1 WORD-REGISTER)
		 (_ SOURCE2 WORD-REGISTER))
	  TARGET
	  SOURCE1
	  SOURCE2)
	(DEFINE-CODE-SEQUENCE INSTRUCTION
	  (,name (_ TARGET FLOAT-REGISTER)
		 (_ SOURCE1 FLOAT-REGISTER)
		 (_ SOURCE2 FLOAT-REGISTER))
	  TARGET
	  SOURCE1
	  SOURCE2)))))

(define-generic-binary-instruction +)
(define-generic-binary-instruction -)
(define-generic-binary-instruction *)

(define-abbreviation (define-word-binary-instruction symbol)
  (lambda (form)
    `((DEFINE-CODE-SEQUENCE INSTRUCTION
	(,(cadr form) (_ TARGET WORD-REGISTER)
		      (_ SOURCE1 WORD-REGISTER)
		      (_ SOURCE2 WORD-REGISTER))
	TARGET
	SOURCE1
	SOURCE2))))

(define-word-binary-instruction quotient)
(define-word-binary-instruction remainder)
(define-word-binary-instruction lsh)
(define-word-binary-instruction and)
(define-word-binary-instruction andc)
(define-word-binary-instruction or)
(define-word-binary-instruction xor)
(define-word-binary-instruction max-unsigned)
(define-word-binary-instruction min-unsigned)

(define-abbreviation (define-float-binary-instruction symbol)
  (lambda (form)
    `((DEFINE-CODE-SEQUENCE INSTRUCTION
	(,(cadr form) (_ TARGET FLOAT-REGISTER)
		      (_ SOURCE1 FLOAT-REGISTER)
		      (_ SOURCE2 FLOAT-REGISTER))
	TARGET
	SOURCE1
	SOURCE2))))

(define-float-binary-instruction /)
(define-float-binary-instruction atan2)