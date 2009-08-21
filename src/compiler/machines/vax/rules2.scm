#| -*-Scheme-*-

$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

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

;;;; LAP Generation Rules: Predicates.
;;; Note: All fixnum code is in rulfix.scm.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (set-standard-branches! 'EQL)
  (test-byte type (reference-alias-register! register 'GENERAL)))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (compare-type type (any-register-reference register)))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (OFFSET (REGISTER (? address)) (? offset)))
	     (? type))
  (compare-type type (indirect-reference! address offset)))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (compare/register*register register-1 register-2 'EQL))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQL))

(define-rule predicate
  (EQ-TEST (? memory) (REGISTER (? register)))
  (QUALIFIER (predicate/memory-operand? memory))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQL))

(define-rule predicate
  (EQ-TEST (? memory-1) (? memory-2))
  (QUALIFIER (and (predicate/memory-operand? memory-1)
		  (predicate/memory-operand? memory-2)))
  (compare/memory*memory (predicate/memory-operand-reference memory-1)
			 (predicate/memory-operand-reference memory-2)
			 'EQL))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant memory))

(define-rule predicate
  (EQ-TEST (? memory) (CONSTANT (? constant)))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant memory))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? register)))
  (eq-test/synthesized-constant*register type datum register))

(define-rule predicate
  (EQ-TEST (REGISTER (? register))
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (eq-test/synthesized-constant*register type datum register))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/synthesized-constant*memory type datum memory))

(define-rule predicate
  (EQ-TEST (? memory)
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/synthesized-constant*memory type datum memory))

;;;; Utilities

(define (eq-test/synthesized-constant type datum ea)
  (set-standard-branches! 'EQL)
  (test-non-pointer type datum ea))

(define-integrable (eq-test/synthesized-constant*register type datum reg)
  (eq-test/synthesized-constant type datum
				(any-register-reference reg)))

(define-integrable (eq-test/synthesized-constant*memory type datum memory)
  (eq-test/synthesized-constant type datum
				(predicate/memory-operand-reference memory)))

(define (eq-test/constant*register constant register)
  (if (non-pointer-object? constant)
      (eq-test/synthesized-constant (object-type constant)
				    (careful-object-datum constant)
				    (any-register-reference register))
      (compare/register*memory register
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQL)))

(define (eq-test/constant*memory constant memory)
  (let ((memory (predicate/memory-operand-reference memory)))
    (if (non-pointer-object? constant)
	(eq-test/synthesized-constant (object-type constant)
				      (careful-object-datum constant)
				      memory)
	(compare/memory*memory memory
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQL))))