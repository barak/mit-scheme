#| -*-Scheme-*-

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

;;;; LAP Generation Rules: Predicates
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define (predicate/memory-operand? expression)
  (or (rtl:simple-offset? expression)
      (and (rtl:post-increment? expression)
	   (interpreter-stack-pointer?
	    (rtl:post-increment-register expression)))))

(define (predicate/memory-operand-reference expression)
  (case (rtl:expression-type expression)
    ((OFFSET)
     (offset->reference! expression))
    ((POST-INCREMENT) (INST-EA (@A+ 7)))
    (else
     (error "Illegal memory operand" expression))))

(define (compare/register*register register-1 register-2 cc)
  (let ((finish
	 (lambda (reference-1 reference-2 cc)
	   (set-standard-branches! cc)
	   (LAP (CMP L ,reference-2 ,reference-1)))))
    (let ((finish-1
	   (lambda (alias)
	     (finish (register-reference alias)
		     (standard-register-reference register-2 'DATA true)
		     cc)))
	  (finish-2
	   (lambda (alias)
	     (finish (register-reference alias)
		     (standard-register-reference register-1 'DATA true)
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
  (let ((reference (standard-register-reference register 'DATA true)))
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
  (TYPE-TEST (REGISTER (? register)) (? type))
  (set-standard-branches! 'EQ)
  (test-byte type (reference-alias-register! register 'DATA)))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (set-standard-branches! 'EQ)
  (if (and (zero? type) use-68020-instructions?)
      (LAP (BFTST ,(standard-register-reference register 'DATA false)
		  (& 0)
		  (& ,scheme-type-width)))
      ;; See if we can reuse a source alias, because `object->type'
      ;; can sometimes do a slightly better job when the source and
      ;; temp are the same register.
      (reuse-pseudo-register-alias! register 'DATA
	(lambda (source)
	  (delete-dead-registers!)
	  (need-register! source)
	  (let ((source (register-reference source)))
	    (normal-type-test source source type)))
	(lambda ()
	  (let ((source (standard-register-reference register 'DATA false)))
	    (delete-dead-registers!)
	    (normal-type-test source
			      (reference-temporary-register! 'DATA)
			      type))))))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (? expression rtl:simple-offset?))
	     (? type))
  (set-standard-branches! 'EQ)
  (let ((source (offset->reference! expression)))
    (cond ((= scheme-type-width 8)
	   (test-byte type source))
	  ((and (zero? type) use-68020-instructions?)
	   (LAP (BFTST ,source (& 0) (& ,scheme-type-width))))
	  (else
	   (normal-type-test source
			     (reference-temporary-register! 'DATA)
			     type)))))

(define (normal-type-test source target type)
  (LAP ,@(object->type source target)
       ,@(if (zero? type)
	     (LAP)
	     (test-byte type target))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (compare/register*register register-1 register-2 'EQ))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   'EQ))

(define-rule predicate
  (EQ-TEST (? memory) (REGISTER (? register)))
  (QUALIFIER (predicate/memory-operand? memory))
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

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (eq-test/constant*register constant register))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (eq-test/constant*register constant register))

(define (eq-test/constant*register constant register)
  (if (non-pointer-object? constant)
      (begin
	(set-standard-branches! 'EQ)
	(test-non-pointer-constant
	 constant
	 (standard-register-reference register 'DATA true)))
      (compare/register*memory register
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQ)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant memory))

(define-rule predicate
  (EQ-TEST (? memory) (CONSTANT (? constant)))
  (QUALIFIER (predicate/memory-operand? memory))
  (eq-test/constant*memory constant memory))

(define (eq-test/constant*memory constant memory)
  (let ((memory (predicate/memory-operand-reference memory)))
    (if (non-pointer-object? constant)
	(begin
	  (set-standard-branches! 'EQ)
	  (test-non-pointer-constant constant memory))
	(compare/memory*memory memory
			       (INST-EA (@PCR ,(constant->label constant)))
			       'EQ))))

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

(define (eq-test/synthesized-constant*register type datum register)
  (set-standard-branches! 'EQ)
  (test-non-pointer type
		    datum
		    (standard-register-reference register 'DATA true)))

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

(define (eq-test/synthesized-constant*memory type datum memory)
  (set-standard-branches! 'EQ)
  (test-non-pointer type
		    datum
		    (predicate/memory-operand-reference memory)))

;;;; Fixnum/Flonum Predicates

(define-rule predicate
  (OVERFLOW-TEST)
  (set-standard-branches! 'VS)
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum (standard-register-reference register 'DATA true)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OBJECT->FIXNUM (REGISTER (? register))))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (object->fixnum (standard-move-to-temporary! register 'DATA)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum (predicate/memory-operand-reference memory)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register-1))
		      (REGISTER (? register-2)))
  (compare/register*register register-1
			     register-2
			     (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (REGISTER (? register)) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (compare/register*memory register
			   (predicate/memory-operand-reference memory)
			   (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate) (? memory) (REGISTER (? register)))
  (QUALIFIER (predicate/memory-operand? memory))
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

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-predicate/register*constant register
				      constant
				      (fixnum-predicate->cc predicate)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? register)))
  (fixnum-predicate/register*constant
   register
   constant
   (invert-cc-noncommutative (fixnum-predicate->cc predicate))))

(define (fixnum-predicate/register*constant register constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (let ((reference (standard-register-reference register 'DATA true)))
    (if (effective-address/register? reference)
	(LAP (CMP L (& ,(* constant fixnum-1)) ,reference))
	(LAP (CMPI L (& ,(* constant fixnum-1)) ,reference)))))

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

(define (fixnum-predicate/memory*constant memory constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (LAP (CMPI L (& ,(* constant fixnum-1)) ,memory)))

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (QUALIFIER (register-value-class=float? register))
  (set-flonum-branches! (flonum-predicate->cc predicate))
  (LAP (FTST ,(standard-register-reference register 'FLOAT false))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? register1))
		      (REGISTER (? register2)))
  (QUALIFIER (and (register-value-class=float? register1)
		  (register-value-class=float? register2)))
  (set-flonum-branches! (flonum-predicate->cc predicate))
  (LAP (FCMP ,(standard-register-reference register2 'FLOAT false)
	     ,(standard-register-reference register1 'FLOAT false))))