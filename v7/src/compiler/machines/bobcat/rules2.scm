#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules2.scm,v 1.1.1.1 1987/07/01 21:00:21 jinx Exp $

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

;;;; LAP Generation Rules: Predicates

(declare (usual-integrations))

;;;; Predicates

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false) 0 (coerce->any register))))

(define-rule predicate
  (TRUE-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false) 0
			  (indirect-reference! register offset))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (LAP ,(test-byte type
		   (register-reference (load-alias-register! register 'DATA)))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    (LAP (RO L L (& 8) ,reference)
	 ,(test-byte type reference))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQ)
  (LAP ,(test-non-pointer (ucode-type unassigned) 0
			  (coerce->any register))))

(define-rule predicate
  (UNASSIGNED-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! 'EQ)
  (LAP ,(test-non-pointer (ucode-type unassigned) 0
			  (indirect-reference! register offset))))

(define (eq-test/constant*register constant register)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (LAP ,(test-non-pointer (primitive-type constant)
			      (primitive-datum constant)
			      (coerce->any register)))
      (LAP (CMP L
		(@PCR ,(constant->label constant))
		,(coerce->machine-register register)))))

(define (eq-test/constant*memory constant memory-reference)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (LAP ,(test-non-pointer (primitive-type constant)
			      (primitive-datum constant)
			      memory-reference))
      (let ((temp (reference-temporary-register! false)))
	(LAP (MOVE/SIMPLE L
			  ,memory-reference
			  ,temp)
	     (CMP L
		  (@PCR ,(constant->label constant))
		  ,temp)))))

(define (eq-test/register*register register-1 register-2)
  (set-standard-branches! 'EQ)
  (let ((finish
	 (lambda (register-1 register-2)
	   (LAP (CMP L
		     ,(coerce->any register-2)
		     ,(coerce->machine-register register-1))))))
    (if (or (and (not (register-has-alias? register-1 'DATA))
		 (register-has-alias? register-2 'DATA))
	    (and (not (register-has-alias? register-1 'ADDRESS))
		 (register-has-alias? register-2 'ADDRESS)))
	(finish register-2 register-1)
	(finish register-1 register-2))))

(define (eq-test/register*memory register memory-reference)
  (set-standard-branches! 'EQ)
  (LAP (CMP L
	    ,memory-reference
	    ,(coerce->machine-register register))))

(define (eq-test/memory*memory register-1 offset-1 register-2 offset-2)
  (set-standard-branches! 'EQ)
  (let ((temp (reference-temporary-register! false)))
    (let ((finish
	   (lambda (register-1 offset-1 register-2 offset-2)
	     (LAP (MOVE/SIMPLE L
			       ,(indirect-reference! register-1 offset-1)
			       ,temp)
		  (CMP L
		       ,(indirect-reference! register-2 offset-2)
		       ,temp)))))
      (if (or (and (not (register-has-alias? register-1 'ADDRESS))
		   (register-has-alias? register-2 'ADDRESS))
	      (and (not (register-has-alias? register-1 'DATA))
		   (register-has-alias? register-2 'DATA)))
	  (finish register-2 offset-2 register-1 offset-1)
	  (finish register-1 offset-1 register-2 offset-2)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register)) (? offset)) (CONSTANT (? constant)))
  (eq-test/constant*memory constant (indirect-reference! register offset)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (OFFSET (REGISTER (? register)) (? offset)))
  (eq-test/constant*memory constant (indirect-reference! register offset)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (POST-INCREMENT (REGISTER 15) 1))
  (eq-test/constant*memory constant (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (POST-INCREMENT (REGISTER 15) 1) (CONSTANT (? constant)))
  (eq-test/constant*memory constant (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (eq-test/register*register register-1 register-2))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (REGISTER (? register-2)))
  (eq-test/register*memory register-2
			   (indirect-reference! register-1 offset-1)))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/register*memory register-1
			   (indirect-reference! register-2 offset-2)))

(define-rule predicate
  (EQ-TEST (POST-INCREMENT (REGISTER 15) 1) (REGISTER (? register)))
  (record-pop!)
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (POST-INCREMENT (REGISTER 15) 1))
  (record-pop!)
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/memory*memory register-1 offset-1register-2 offset-2))
