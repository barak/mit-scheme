#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rules2.scm,v 4.4 1991/02/15 00:42:21 jinx Exp $
$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

Copyright (c) 1987, 1989, 1991 Massachusetts Institute of Technology

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