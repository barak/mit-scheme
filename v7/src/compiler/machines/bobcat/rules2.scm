#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules2.scm,v 4.4 1988/06/14 08:48:37 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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
  (LAP ,(test-byte
	 type
	 (register-reference (load-alias-register! register 'DATA)))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    (LAP (RO L L (& 8) ,reference)
	 ,(test-byte type reference))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (OFFSET (REGISTER (? register)) (? offset)))
	     (? type))
  (set-standard-branches! 'EQ)
  (LAP ,(test-byte type (indirect-reference! register offset))))

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
      (LAP ,(test-non-pointer (object-type constant)
			      (object-datum constant)
			      (coerce->any register)))
      (LAP (CMP L (@PCR ,(constant->label constant))
		,(coerce->machine-register register)))))

(define (eq-test/constant*memory constant memory-reference)
  (set-standard-branches! 'EQ)
  (if (non-pointer-object? constant)
      (LAP ,(test-non-pointer (object-type constant)
			      (object-datum constant)
			      memory-reference))
      (let ((temp (reference-temporary-register! false)))
	(LAP (MOV L ,memory-reference ,temp)
	     (CMP L (@PCR ,(constant->label constant))
		  ,temp)))))

(define (eq-test/register*register register-1 register-2)
  (set-standard-branches! 'EQ)
  (let ((finish
	 (lambda (register-1 register-2)
	   (LAP (CMP L ,(coerce->any register-2)
		     ,(coerce->machine-register register-1))))))
    (if (or (and (not (register-has-alias? register-1 'DATA))
		 (register-has-alias? register-2 'DATA))
	    (and (not (register-has-alias? register-1 'ADDRESS))
		 (register-has-alias? register-2 'ADDRESS)))
	(finish register-2 register-1)
	(finish register-1 register-2))))

(define (eq-test/register*memory register memory-reference)
  (set-standard-branches! 'EQ)
  (LAP (CMP L ,memory-reference
	    ,(coerce->machine-register register))))

(define (eq-test/memory*memory register-1 offset-1 register-2 offset-2)
  (set-standard-branches! 'EQ)
  (let ((temp (reference-temporary-register! false)))
    (let ((finish
	   (lambda (register-1 offset-1 register-2 offset-2)
	     (LAP (MOV L ,(indirect-reference! register-1 offset-1)
		       ,temp)
		  (CMP L ,(indirect-reference! register-2 offset-2)
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
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (POST-INCREMENT (REGISTER 15) 1))
  (eq-test/register*memory register (INST-EA (@A+ 7))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? register-1)) (? offset-1))
	   (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (eq-test/memory*memory register-1 offset-1 register-2 offset-2))


;;; fixnum predicates

(define (fixnum-pred/register*register register-1 register-2 cc)
  (let ((finish
	 (lambda (register-1 register-2 maybe-invert)
	   (set-standard-branches! (maybe-invert cc))
	   (LAP (CMP L ,(coerce->any register-1)
		     ,(coerce->machine-register register-2))))))
    (if (or (and (not (register-has-alias? register-1 'DATA))
		 (register-has-alias? register-2 'DATA))
	    (and (not (register-has-alias? register-1 'ADDRESS))
		 (register-has-alias? register-2 'ADDRESS)))
	(finish register-2 register-1 invert-cc)
	(finish register-1 register-2 (lambda (x) x)))))

(define (fixnum-pred/constant*register constant register cc)
  (set-standard-branches! cc)
  (if (non-pointer-object? constant)
      (LAP (CMPI L (& ,(object-datum constant)) ,(coerce->any register)))
      (LAP (CMP L (@PCR ,(constant->label constant))
		,(coerce->machine-register register)))))

(define (fixnum-pred/constant*memory constant memory-reference cc)
  (set-standard-branches! cc)
  (if (non-pointer-object? constant)
      (LAP (CMPI L (& ,(object-datum constant)) ,memory-reference))
      (let ((temp (reference-temporary-register! false)))
	(LAP (MOV L ,memory-reference ,temp)
	     (CMP L (@PCR ,(constant->label constant))
		  ,temp)))))

(define (fixnum-pred/register*memory register memory-reference cc)
  (set-standard-branches! cc)
  (LAP (CMP L ,memory-reference
	    ,(coerce->machine-register register))))

(define (fixnum-pred/memory*memory register-1 offset-1 register-2 offset-2 cc)
  (let ((temp (reference-temporary-register! false)))
    (let ((finish
	   (lambda (register-1 offset-1 register-2 offset-2 maybe-invert)
	     (set-standard-branches! (maybe-invert cc))
	     (LAP (MOV L ,(indirect-reference! register-1 offset-1)
		       ,temp)
		  (CMP L ,(indirect-reference! register-2 offset-2)
		       ,temp)))))
      (if (or (and (not (register-has-alias? register-1 'ADDRESS))
		   (register-has-alias? register-2 'ADDRESS))
	      (and (not (register-has-alias? register-1 'DATA))
		   (register-has-alias? register-2 'DATA)))
	  (finish register-2 offset-2 register-1 offset-1 invert-cc)
	  (finish register-1 offset-1 register-2 offset-2 (lambda (x) x))))))



(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1)) (REGISTER (? register-2)))
  (fixnum-pred/register*register register-2 register-1
				 (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register)) (CONSTANT (? constant)))
  (fixnum-pred/constant*register constant register
				 (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (CONSTANT (? constant)) (REGISTER (? register)))
  (fixnum-pred/constant*register constant register
				 (invert-cc (fixnum-pred->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OFFSET (REGISTER (? register)) (? offset))
		      (CONSTANT (? constant)))
  (fixnum-pred/constant*memory constant (indirect-reference! register offset)
			       (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (CONSTANT (? constant))
		      (OFFSET (REGISTER (? register)) (? offset)))
  (fixnum-pred/constant*memory constant (indirect-reference! register offset)
			       (invert-cc (fixnum-pred->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (CONSTANT (? constant))
		      (POST-INCREMENT (REGISTER 15) 1))
  (fixnum-pred/constant*memory constant (INST-EA (@A+ 7))
			       (invert-cc (fixnum-pred->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (POST-INCREMENT (REGISTER 15) 1) (CONSTANT (? constant)))
  (fixnum-pred/constant*memory constant (INST-EA (@A+ 7))
			       (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OFFSET (REGISTER (? register-1)) (? offset-1))
		      (REGISTER (? register-2)))
  (fixnum-pred/register*memory register-2
			       (indirect-reference! register-1 offset-1)
			       (invert-cc (fixnum-pred->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (fixnum-pred/register*memory register-1
			   (indirect-reference! register-2 offset-2)
			   (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (POST-INCREMENT (REGISTER 15) 1) (REGISTER (? register)))
  (fixnum-pred/register*memory register (INST-EA (@A+ 7))
			       (invert-cc (fixnum-pred->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register)) (POST-INCREMENT (REGISTER 15) 1))
  (fixnum-pred/register*memory register (INST-EA (@A+ 7))
			       (fixnum-pred->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OFFSET (REGISTER (? register-1)) (? offset-1))
		      (OFFSET (REGISTER (? register-2)) (? offset-2)))
  (fixnum-pred/memory*memory register-1 offset-1 register-2 offset-2
			     (fixnum-pred->cc predicate)))


(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (set-standard-branches! (fixnum-pred->cc predicate))
  (test-fixnum (coerce->any register)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (CONSTANT (? constant)))
  (set-standard-branches! (fixnum-pred->cc predicate))
    (if (non-pointer-object? constant)
      (test-fixnum (INST-EA (& ,(object-datum constant))))
      (test-fixnum (INST-EA (@PCR ,(constant->label constant))))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (POST-INCREMENT (REGISTER 15) 1))
  (set-standard-branches! (fixnum-pred->cc predicate))
  (test-fixnum (INST-EA (@A+ 7))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OFFSET (REGISTER (? register)) (? offset)))
  (set-standard-branches! (fixnum-pred->cc predicate))
  (test-fixnum (indirect-reference! offset register)))
