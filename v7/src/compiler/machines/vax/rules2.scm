#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rules2.scm,v 4.3 1989/05/17 20:31:04 jinx Rel $
$MC68020-Header: rules2.scm,v 4.7 88/12/13 17:45:25 GMT cph Exp $

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

;;;; LAP Generation Rules: Predicates.  DEC VAX version.
;;; Note: All fixnum code has been moved to rulfix.scm.

(declare (usual-integrations))

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NEQ)
  (LAP ,(test-non-pointer (ucode-type false)
			  0
			  (standard-register-reference register false))))

(define-rule predicate
  (TRUE-TEST (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! 'NEQ)
  (LAP ,(test-non-pointer (ucode-type false)
			  0
			  (predicate/memory-operand-reference memory))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQL)
  (LAP ,(test-byte type (reference-alias-register! register 'GENERAL))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQL)
  (with-temporary-register-copy! register 'GENERAL
    (lambda (temp)
      (LAP (ROTL (S 8) ,temp ,temp)
	   ,(test-byte type temp)))
    (lambda (source temp)
      (LAP (ROTL (S 8) ,source ,temp)
	   ,(test-byte type temp)))))

;; This is the split of a 68020 rule which seems wrong for post-increment.

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (OFFSET (REGISTER (? r)) (? offset))) (? type))
  (set-standard-branches! 'EQL)
  (LAP ,(test-byte type (indirect-byte-reference! r (+ 3 (* 4 offset))))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (POST-INCREMENT (REGISTER 14) 1)) (? type))
  (set-standard-branches! 'EQL)
  (let ((temp (reference-temporary-register! 'GENERAL)))
    (LAP (ROTL (S 8) (@R+ 14) ,temp)
	 ,(test-byte type temp))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQL)
  (LAP ,(test-non-pointer (ucode-type unassigned)
			  0
			  (standard-register-reference register false))))

(define-rule predicate
  (UNASSIGNED-TEST (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! 'EQL)
  (LAP ,(test-non-pointer (ucode-type unassigned)
			  0
			  (predicate/memory-operand-reference memory))))

(define-rule predicate
  (OVERFLOW-TEST)
  (set-standard-branches! 'VS)
  (LAP))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (QUALIFIER (and (pseudo-register? register-1)
		  (pseudo-register? register-2)))
  (compare/register*register register-1 register-2 'EQL))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? memory))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQL))

(define-rule predicate
  (EQ-TEST (? memory) (REGISTER (? register)))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
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

(define (eq-test/constant*register constant register)
  (if (non-pointer-object? constant)
      (begin
	(set-standard-branches! 'EQL)
	(LAP ,(test-non-pointer (object-type constant)
				(object-datum constant)
				(standard-register-reference register false))))
      (compare/register*memory register
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQL)))

(define (eq-test/constant*memory constant memory)
  (if (non-pointer-object? constant)
      (begin
	(set-standard-branches! 'EQL)
	(LAP ,(test-non-pointer (object-type constant)
				(object-datum constant)
				memory)))
      (compare/memory*memory memory
			     (INST-EA (@PCR ,(constant->label constant)))
			     'EQL)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (QUALIFIER (pseudo-register? register))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (QUALIFIER (pseudo-register? register))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant
			   (predicate/memory-operand-reference memory)))

(define-rule predicate
  (EQ-TEST (? memory) (CONSTANT (? constant)))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant
			   (predicate/memory-operand-reference memory)))