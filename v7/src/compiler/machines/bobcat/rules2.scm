#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules2.scm,v 4.7 1988/12/13 17:45:25 cph Rel $

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

(define (predicate/memory-operand? expression)
  (or (rtl:offset? expression)
      (and (rtl:post-increment? expression)
	   (interpreter-stack-pointer?
	    (rtl:post-increment-register expression)))))

(define (predicate/memory-operand-reference expression)
  (case (rtl:expression-type expression)
    ((OFFSET) (offset->indirect-reference! expression))
    ((POST-INCREMENT) (INST-EA (@A+ 7)))
    (else (error "Illegal memory operand" expression))))

(define (compare/register*register register-1 register-2 cc)
  (let ((finish
	 (lambda (reference-1 reference-2 cc)
	   (set-standard-branches! cc)
	   (LAP (CMP L ,reference-2 ,reference-1)))))
    (let ((finish-1
	   (lambda (alias)
	     (finish (register-reference alias)
		     (standard-register-reference register-2 'DATA)
		     cc)))
	  (finish-2
	   (lambda (alias)
	     (finish (register-reference alias)
		     (standard-register-reference register-1 'DATA)
		     (invert-cc-noncommutative cc)))))
      (let ((try-type
	     (lambda (type continue)
	       (let ((alias (register-alias register-1 type)))
		 (if alias
		     (finish-1 alias)
		     (let ((alias (register-alias register-2 type)))
		       (if alias
			   (finish-2 alias)
			   (continue))))))))
	(try-type 'DATA
	  (lambda ()
	    (try-type 'ADDRESS
	      (lambda ()
		(if (dead-register? register-1)
		    (finish-2 (load-alias-register! register-2 'DATA))
		    (finish-1 (load-alias-register! register-1 'DATA)))))))))))

(define (compare/register*memory register memory cc)
  (let ((reference (standard-register-reference register 'DATA)))
    (if (effective-address/register? reference)
	(begin
	  (set-standard-branches! cc)
	  (LAP (CMP L ,memory ,reference)))
	(compare/memory*memory reference memory cc))))

(define (compare/memory*memory memory-1 memory-2 cc)
  (set-standard-branches! cc)
  (let ((temp (reference-temporary-register! 'DATA)))
    (LAP (MOV L ,memory-1 ,temp)
	 (CMP L ,memory-2 ,temp))))

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false)
			  0
			  (standard-register-reference register false))))

(define-rule predicate
  (TRUE-TEST (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! 'NE)
  (LAP ,(test-non-pointer (ucode-type false)
			  0
			  (predicate/memory-operand-reference memory))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (LAP ,(test-byte type (reference-alias-register! register 'DATA))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! 'EQ)
  (let ((reference (move-to-temporary-register! register 'DATA)))
    (LAP (RO L L (& 8) ,reference)
	 ,(test-byte type reference))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (? memory)) (? type))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! 'EQ)
  (LAP ,(test-byte type (predicate/memory-operand-reference memory))))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (set-standard-branches! 'EQ)
  (LAP ,(test-non-pointer (ucode-type unassigned)
			  0
			  (standard-register-reference register 'DATA))))

(define-rule predicate
  (UNASSIGNED-TEST (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! 'EQ)
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
  (compare/register*register register-1 register-2 'EQ))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? memory))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQ))

(define-rule predicate
  (EQ-TEST (? memory) (REGISTER (? register)))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQ))

(define-rule predicate
  (EQ-TEST (? memory-1) (? memory-2))
  (QUALIFIER (and (predicate/memory-operand? memory-1)
		  (predicate/memory-operand? memory-2)))
  (compare/memory*memory (predicate/memory-operand-reference memory-1)
			 (predicate/memory-operand-reference memory-2)
			 'EQ))

(define (eq-test/constant*register constant register)
  (if (non-pointer-object? constant)
      (begin
	(set-standard-branches! 'EQ)
	(LAP ,(test-non-pointer (object-type constant)
				(object-datum constant)
				(standard-register-reference register 'DATA))))
      (compare/register*memory register
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQ)))

(define (eq-test/constant*memory constant memory)
  (if (non-pointer-object? constant)
      (begin
	(set-standard-branches! 'EQ)
	(LAP ,(test-non-pointer (object-type constant)
				(object-datum constant)
				memory)))
      (compare/memory*memory memory
			     (INST-EA (@PCR ,(constant->label constant)))
			     'EQ)))

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

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum (standard-register-reference register 'DATA)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum (predicate/memory-operand-reference memory)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (REGISTER (? register-2)))
  (QUALIFIER (and (pseudo-register? register-1)
		  (pseudo-register? register-2)))
  (compare/register*register register-1
			     register-2
			     (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (REGISTER (? register)) (? memory))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (? memory) (REGISTER (? register)))
  (QUALIFIER (and (predicate/memory-operand? memory)
		  (pseudo-register? register)))
  (compare/register*memory
   register
   (predicate/memory-operand-reference memory)
   (invert-cc-noncommutative (fixnum-predicate->cc predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (? memory-1) (? memory-2))
  (QUALIFIER (and (predicate/memory-operand? memory-1)
		  (predicate/memory-operand? memory-2)))
  (compare/memory*memory (predicate/memory-operand-reference memory-1)
			 (predicate/memory-operand-reference memory-2)
			 (fixnum-predicate->cc predicate)))

(define (fixnum-predicate/register*constant register constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (let ((reference (standard-register-reference register 'DATA)))
    (if (effective-address/register? reference)
	(LAP (CMP L (& ,(* constant #x100)) ,reference))
	(LAP (CMPI L (& ,(* constant #x100)) ,reference)))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? register))
  (fixnum-predicate/register*constant register
				      constant
				      (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? register)))
  (QUALIFIER (pseudo-register? register))
  (fixnum-predicate/register*constant
   register
   constant
   (invert-cc-noncommutative (fixnum-predicate->cc predicate))))

(define (fixnum-predicate/memory*constant memory constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (LAP (CMPI L (& ,(* constant #x100)) ,memory)))
(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (? memory)
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-predicate/memory*constant (predicate/memory-operand-reference memory)
				    constant
				    (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (fixnum-predicate/memory*constant
   (predicate/memory-operand-reference memory)
   constant
   (invert-cc-noncommutative (fixnum-predicate->cc predicate))))