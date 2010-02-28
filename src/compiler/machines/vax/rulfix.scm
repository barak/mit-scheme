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

;;;; LAP Generation Rules: Fixnum operations.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Making and examining fixnums

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (convert-object/register->register target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (convert-object/register->register target source object->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (convert-object/register->register target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (convert-object/register->register target source fixnum->object))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (convert-object/register->register target source fixnum->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (CONSTANT (? constant)))))
  (convert-object/constant->register target constant
				     address->fixnum ct/address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-fixnum-constant constant (standard-target-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM
	   (OBJECT->ADDRESS (OFFSET (REGISTER (? address)) (? offset)))))
  (convert-object/offset->register target address offset address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (OFFSET (REGISTER (? address)) (? offset))))
  (convert-object/offset->register target address offset object->fixnum))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (FIXNUM->OBJECT (REGISTER (? source))))
  (let* ((source (any-register-reference source))
	 (target (indirect-reference! a n)))
    (fixnum->object source target)))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (fixnum->object (any-register-reference r) (INST-EA (@R+ 12))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (fixnum->object (any-register-reference r) (INST-EA (@-R 14))))

;;;; Fixnum Operations

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (fixnum-1-arg target source (fixnum-1-arg/operate operator)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (fixnum-2-args target source1 source2 (fixnum-2-args/operate operator)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (if (fixnum-2-args/commutative? operator)
      (fixnum-2-args/register*constant operator target source constant)
      (fixnum-2-args/constant*register operator target constant source)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target r n))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (? overflow?)))
  (QUALIFIER (machine-operation-target? target))
  overflow?				; ignored
  (convert-index->fixnum/offset target r n))

#|
;; These could be used for multiply instead of the generic rule used above.
;; They are better when the target is in memory, but they are not worth it.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (fixnum-2-args `(REGISTER ,target)
		 source1 source2
		 (fixnum-2-args/operate 'MULTIPLY-FIXNUM)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (? offset))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (let* ((shift (- 0 scheme-type-width))
	 (target (indirect-reference! base offset))
	 (get-temp (temporary-copy-if-available source1 'GENERAL)))
    (if get-temp
	(let ((source2 (any-register-reference source2))
	      (temp (get-temp)))
	  (LAP (ASH L ,(make-immediate shift) ,temp ,temp)
	       (MUL L ,temp ,source2 ,target)))
	(let ((get-temp (temporary-copy-if-available source2 'GENERAL)))
	  (if get-temp
	      (let ((source1 (any-register-reference source1))
		    (temp (get-temp)))
		(LAP (ASH L ,(make-immediate shift) ,temp ,temp)
		     (MUL L ,source1 ,temp ,target)))
	      (let ((source1 (any-register-reference source1))
		    (source2 (any-register-reference source2))
		    (temp (reference-temporary-register! 'GENERAL)))
		(LAP (ASH L ,(make-immediate shift) ,source1 ,temp)
		     (MUL L ,temp ,source2 ,target))))))))
|#

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum/ea (any-register-reference register)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (OBJECT->FIXNUM (REGISTER (? register))))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (let ((temporary (standard-temporary-reference)))
    (object->fixnum (any-register-reference register) temporary)))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (? memory))
  (QUALIFIER (predicate/memory-operand? memory))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum/ea (predicate/memory-operand-reference memory)))

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

;; This assumes that the last instruction sets the condition code bits
;; correctly.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-standard-branches! 'VS)
  (LAP))

;;;; Utilities

(define-integrable (datum->fixnum source target)
  ;; This drops the type code
  (LAP (ASH L (S ,scheme-type-width) ,source ,target)))

(define-integrable (fixnum->datum source target)
  ;; This maintains the type code, if any.
  (LAP (ROTL (S ,scheme-datum-width) ,source ,target)))

(define (object->fixnum source target)
  (datum->fixnum source target))

(define-integrable (ct/object->fixnum object target)
  (load-fixnum-constant object target))

(define (address->fixnum source target)
  (datum->fixnum source target))

(define-integrable (ct/address->fixnum address target)
  (load-fixnum-constant (careful-object-datum address) target))

(define (fixnum->address source target)
  (fixnum->datum source target))

(define (ct/fixnum->address fixnum target)
  (load-immediate fixnum target))

(define-integrable (target-or-register target)
  (if (effective-address/register? target)
      target
      (standard-temporary-reference)))

(define (fixnum->object source target)
  (let ((rtarget (target-or-register target)))
    (LAP ,@(if (eq? rtarget source)
	       (LAP (BIS L (S ,(ucode-type fixnum)) ,rtarget))
	       (LAP (BIS L (S ,(ucode-type fixnum)) ,source ,rtarget)))
	 ,@(fixnum->datum rtarget target))))

(define-integrable (ct/fixnum->object fixnum target)
  (load-constant fixnum target))

(define-integrable fixnum-1 64)		; (expt 2 scheme-type-width) ***

(define-integrable fixnum-bits-mask
  (-1+ fixnum-1))

(define (load-fixnum-constant constant target)
  (cond ((zero? constant)
	 (LAP (CLR L ,target)))
	((<= 1 constant 63)
	 (LAP (ASH L (S ,scheme-type-width) (S ,constant) ,target)))
	(else
	 (let* ((constant (* constant fixnum-1))
		(size (datum-size constant)))
	   (cond ((not (eq? size 'L))
		  (LAP (CVT ,size L ,(make-immediate constant) ,target)))
		 ((and (positive? constant) (< constant #x10000))
		  (LAP (MOVZ W L ,(make-immediate constant) ,target)))
		 (else
		  (LAP (MOV L ,(make-immediate constant) ,target))))))))

(define (machine-operation-target? target)
  (or (rtl:register? target)
      (and (rtl:offset? target)
	   (rtl:register? (rtl:offset-base target)))))

(define (fixnum-choose-target target operate-on-pseudo operate-on-target)
  (cond ((rtl:register? target)
	 (let ((register (rtl:register-number target)))
	   (if (pseudo-register? register)
	       (operate-on-pseudo register)
	       (operate-on-target (register-reference register)))))
	((rtl:offset? target)
	 (operate-on-target (offset->indirect-reference! target)))
	(else
	 (error "fixnum-choose-target: Not a machine-operation-target"
		target))))

(define (convert-index->fixnum/register target source)
  (fixnum-1-arg
   target source
   (lambda (target source)
     (LAP (ASH L (S ,(+ scheme-type-width 2)) ,source ,target)))))

(define (convert-index->fixnum/offset target address offset)
  (let ((source (indirect-reference! address offset)))
    (fixnum-choose-target
     target
     (lambda (pseudo)
       (let ((target (standard-target-reference pseudo)))
	 (LAP (ASH L (S ,(+ scheme-type-width 2)) ,source ,target))))
     (lambda (target)
       (LAP (ASH L (S ,(+ scheme-type-width 2)) ,source ,target))))))

;;;; Fixnum operation dispatch

(define (define-fixnum-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-fixnum-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-integrable (fixnum-1-arg/operate operator)
  (lookup-fixnum-method operator fixnum-methods/1-arg))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-integrable (fixnum-2-args/operate operator)
  (lookup-fixnum-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args-constant
  (list 'FIXNUM-METHODS/2-ARGS-CONSTANT))

(define-integrable (fixnum-2-args/operate-constant operator)
  (lookup-fixnum-method operator fixnum-methods/2-args-constant))

(define fixnum-methods/2-args-tnatsnoc
  (list 'FIXNUM-METHODS/2-ARGS-TNATSNOC))

(define-integrable (fixnum-2-args/operate-tnatsnoc operator)
  (lookup-fixnum-method operator fixnum-methods/2-args-tnatsnoc))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))

(define (fixnum-1-arg target source operation)
  (fixnum-choose-target
   target
   (lambda (target)
     (cond ((register-copy-if-available source 'GENERAL target)
	    =>
	    (lambda (get-target)
	      (let ((target (get-target)))
		(operation target target))))
	   (else
	    (let* ((source (any-register-reference source))
		   (target (standard-target-reference target)))
	      (operation target source)))))
   (lambda (target)
     (let ((source (any-register-reference source)))
       (operation target source)))))

(define-integrable (commute target source1 source2 recvr1 recvr2)
  (cond ((ea/same? target source1)
	 (recvr1 source2))
	((ea/same? target source2)
	 (recvr1 source1))
	(else
	 (recvr2))))
	     
(define (fixnum-2-args target source1 source2 operation)
  (fixnum-choose-target
   target
   (lambda (target)
     (cond ((register-copy-if-available source1 'GENERAL target)
	    =>
	    (lambda (get-target)
	      (let* ((source2 (any-register-reference source2))
		     (target (get-target)))
		(operation target target source2))))
	   ((register-copy-if-available source2 'GENERAL target)
	    =>
	    (lambda (get-target)
	      (let* ((source1 (any-register-reference source1))
		     (target (get-target)))
		(operation target source1 target))))
	   (else
	    (let* ((source1 (any-register-reference source1))
		   (source2 (any-register-reference source2))
		   (target (standard-target-reference target)))
	      (operation target source1 source2)))))
   (lambda (target)
     (let* ((source1 (any-register-reference source1))
	    (source2 (any-register-reference source2)))
       (operation target source1 source2)))))

(define (fixnum-2-args/register*constant operator target source constant)
  (fixnum-1-arg
   target source
   (lambda (target source)
     ((fixnum-2-args/operate-constant operator) target source constant))))

(define (fixnum-2-args/constant*register operator target constant source)
  (fixnum-1-arg
   target source
   (lambda (target source)
     ((fixnum-2-args/operate-tnatsnoc operator) target constant source))))

(define (integer-power-of-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else
	   (loop (* 2 power) (1+ exponent))))))

(define (word->fixnum/ea source target)
  (if (eq? target source)
      (LAP (BIC B ,(make-immediate fixnum-bits-mask) ,target))
      (LAP (BIC B ,(make-immediate fixnum-bits-mask) ,source ,target))))

;; This is used instead of add-constant/ea because add-constant/ea is not
;; guaranteed to set the overflow flag correctly.

(define (add-fixnum-constant source constant target)
  ;; This ignores instructions like INC and DEC because
  ;; word is always too big.
  (let ((word (* constant fixnum-1)))
    (cond ((zero? word)
	   (ea/copy source target))
	  ((ea/same? source target)
	   (LAP (ADD L ,(make-immediate word) ,target)))
	  (else
	   (LAP (ADD L ,(make-immediate word) ,source ,target))))))

;;;; Arithmetic operations

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source)
    (add-fixnum-constant source 1 target)))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source)
    (add-fixnum-constant source -1 target)))

(define-fixnum-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (target source)
    (let ((rtarget (target-or-register target)))
      (LAP (MCOM L ,source ,rtarget)
	   ,@(word->fixnum/ea rtarget target)))))

(let-syntax
    ((binary/commutative
      (sc-macro-transformer
       (lambda (form environment)
	 `(DEFINE-FIXNUM-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS
	    (LAMBDA (TARGET SOURCE1 SOURCE2)
	      (IF (EA/SAME? SOURCE1 SOURCE2)
		  (,(close-syntax (cadddr form) environment)
		   TARGET
		   (IF (OR (EQ? TARGET SOURCE1)
			   (EQ? TARGET SOURCE2))
		       TARGET
		       SOURCE1))
		  (COMMUTE TARGET SOURCE1 SOURCE2
			   (LAMBDA (SOURCE*)
			     (LAP (,(caddr form) L ,',SOURCE* ,',TARGET)))
			   (LAMBDA ()
			     (LAP (,(caddr form) L ,',SOURCE1 ,',SOURCE2
						 ,',TARGET)))))))))))
  (binary/commutative PLUS-FIXNUM ADD
		      (lambda (target source)
			(if (eq? target source)
			    (LAP (ADD L ,source ,target))
			    (LAP (ADD L ,source ,source ,target)))))
  (binary/commutative FIXNUM-OR BIS
		      (lambda (target source)
			(if (eq? target source)
			    (LAP)
			    (LAP (MOV L ,source ,target)))))
  (binary/commutative FIXNUM-XOR XOR
		      (lambda (target source)
			source		; ignored
			(load-fixnum-constant target))))

(let-syntax
     ((binary/noncommutative
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-FIXNUM-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS
	     (LAMBDA (TARGET SOURCE1 SOURCE2)
	       (COND ((EA/SAME? SOURCE1 SOURCE2)
		      (LOAD-FIXNUM-CONSTANT 0 TARGET))
		     ((EQ? TARGET SOURCE1)
		      (LAP (,(caddr form) L ,',SOURCE2 ,',TARGET)))
		     (ELSE
		      (LAP (,(caddr form) L ,',SOURCE2 ,',SOURCE1
					  ,',TARGET))))))))))
  (binary/noncommutative MINUS-FIXNUM SUB)
  (binary/noncommutative FIXNUM-ANDC BIC))

(define-fixnum-method 'FIXNUM-AND fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (ea/same? source1 source2)
	(ea/copy source1 target)
	(let ((temp (standard-temporary-reference)))
	  (commute target source1 source2
		   (lambda (source*)
		     (LAP (MCOM L ,source* ,temp)
			  (BIC L ,temp ,target)))
		   (lambda ()
		     (LAP (MCOM L ,source1 ,temp)
			  (BIC L ,temp ,source2 ,target))))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (let ((shift (- 0 scheme-type-width)))
    (lambda (target source1 source2)
      (if (not (effective-address/register? target))
	  (let ((temp (standard-temporary-reference)))
	    (commute target source1 source2
		     (lambda (source*)
		       (LAP (ASH L ,(make-immediate shift) ,source* ,temp)
			    (MUL L ,temp ,target)))
		     (lambda ()
		       (LAP (ASH L ,(make-immediate shift) ,source1 ,temp)
			    (MUL L ,temp ,source2 ,target)))))
	  (commute
	   target source1 source2
	   (lambda (source*)
	     (cond ((not (ea/same? target source*))
		    (LAP (ASH L ,(make-immediate shift) ,target ,target)
			 (MUL L ,source* ,target)))
		   ((even? scheme-type-width)
		    (let ((shift (quotient shift 2)))
		      (LAP (ASH L ,(make-immediate shift) ,target ,target)
			   (MUL L ,target ,target))))
		   (else
		    (let ((temp (standard-temporary-reference)))
		      (LAP (ASH L ,(make-immediate shift) ,target ,temp)
			   (MUL L ,temp ,target))))))
	   (lambda ()
	     (LAP (ASH L ,(make-immediate shift) ,source1 ,target)
		  (MUL L ,source2 ,target))))))))

(define (code-fixnum-shift target source1 source2)
  #|
  ;; This does arithmetic shifting, rather than logical!
  (let* ((rtarget (target-or-register target))
	 (temp (if (eq? rtarget target)
		   (standard-temporary-reference)
		   rtarget)))
    (LAP (ASH L ,(make-immediate (- 0 scheme-type-width))
	      ,source2 ,temp)
	 (ASH L ,temp ,source1 ,rtarget)
	 ,@(word->fixnum/ea rtarget target)))
  |#
  ;; This is a kludge that depends on the fact that there are
  ;; always scheme-type-width 0 bits at the bottom.
  (let* ((rtarget (target-or-register target))
	 (temp (standard-temporary-reference)))
    (LAP (ASH L ,(make-immediate (- 0 scheme-type-width))
	      ,source2 ,temp)
	 (ROTL (S 31) ,source1 ,rtarget) ; guarantee sign bit of 0.
	 (ASH L ,temp ,rtarget ,rtarget)
	 (ROTL (S 1) ,rtarget ,rtarget) ; undo effect of previous ROTL.
	 ,@(word->fixnum/ea rtarget target))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args
  code-fixnum-shift)

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (ea/same? source1 source2)
	(load-fixnum-constant 1 target)
	(code-fixnum-quotient target source1 source2))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2)
    (if (ea/same? source1 source2)
	(load-fixnum-constant 0 target)
	(code-fixnum-remainder target source1 source2))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (add-fixnum-constant source n  target)))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (add-fixnum-constant source (- 0 n) target)))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(LAP (MNEG L ,source ,target))
	(LAP (SUB L ,source ,(make-immediate (* n fixnum-1)) ,target)))))

(let-syntax
    ((binary-fixnum/constant
      (sc-macro-transformer
       (lambda (form environment)
	 (let ((->constant (close-syntax (list-ref form 4) environment))
	       (identity? (close-syntax (list-ref form 5) environment)))
	   `(DEFINE-FIXNUM-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS-CONSTANT
	      (LAMBDA (TARGET SOURCE N)
		(COND ((EQV? N ,(cadddr form))
		       (LOAD-FIXNUM-CONSTANT ,(cadddr form) TARGET))
		      ((,identity? N)
		       (EA/COPY SOURCE TARGET))
		      (ELSE
		       (LET ((CONSTANT (* FIXNUM-1 (,->constant N))))
			 (IF (EA/SAME? SOURCE TARGET)
			     (LAP (,(caddr form) L ,',(make-immediate constant)
						 ,',target))
			     (LAP (,(caddr form) L
						 ,',(make-immediate constant)
						 ,',source
						 ,',target)))))))))))))

  (binary-fixnum/constant FIXNUM-OR BIS -1 identity-procedure zero?)
  (binary-fixnum/constant FIXNUM-XOR XOR 'SELF identity-procedure zero?)
  (binary-fixnum/constant FIXNUM-AND BIC 0 fix:not (lambda (n) (= n -1))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((zero? n)
	   (ea/copy source target))
	  ((= n -1)
	   (load-fixnum-constant 0 target))
	  ((eq? target source)
	   (LAP (BIC L ,(make-immediate (* n fixnum-1)) ,target)))
	  (else
	   (LAP (BIC L ,(make-immediate (* n fixnum-1)) ,source ,target))))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(load-fixnum-constant 0 target)
	(LAP (BIC L ,source ,(make-immediate (* n fixnum-1)) ,target)))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((zero? n)
	   (ea/copy source target))
	  ((not (<= (- 0 scheme-datum-width) n scheme-datum-width))
	   (load-fixnum-constant 0 target))
	  ((not (negative? n))
	   (LAP (ASH L ,(make-immediate n) ,source ,target)))
	  ;; The following two cases depend on having scheme-type-width
	  ;; 0 bits at the bottom.
	  ((>= n (- 0 scheme-type-width))
	   (let ((rtarget (target-or-register target)))
	     (LAP (ROTL (S ,(+ 32 n)) ,source ,rtarget)
		  ,@(word->fixnum/ea rtarget target))))
	  (else
	   (let ((rtarget (target-or-register target)))
	     (LAP (ROTL (S 31) ,source ,rtarget)
		  (ASH L ,(make-immediate (1+ n)) ,rtarget ,rtarget)
		  ,@(word->fixnum/ea rtarget target)))))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(load-fixnum-constant 0 target)
	(code-fixnum-shift target (make-immediate (* n fixnum-1)) source))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((zero? n)
	   (load-fixnum-constant 0 target))
	  ((= n 1)
	   (ea/copy source target))
	  ((= n -1)
	   (LAP (MNEG L ,source ,target)))
	  ((integer-power-of-2? (if (negative? n) (- 0 n) n))
	   =>
	   (lambda (expt-of-2)
	     (if (negative? n)
		 (let ((rtarget (target-or-register target)))
		   (LAP (ASH L ,(make-immediate expt-of-2) ,source ,rtarget)
			(MNEG L ,rtarget ,target)))
		 (LAP (ASH L ,(make-immediate expt-of-2) ,source ,target)))))
	  ((eq? target source)
	   (LAP (MUL L ,(make-immediate n) ,target)))
	  (else
	   (LAP (MUL L ,(make-immediate n) ,source ,target))))))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((= n 1)
	   (ea/copy source target))
	  ((= n -1)
	   (LAP (MNEG L ,source ,target)))
	  ((integer-power-of-2? (if (negative? n) (- 0 n) n))
	   =>
	   (lambda (expt-of-2)
	     (let ((label (generate-label 'QUO-SHIFT))
		   (absn (if (negative? n) (- 0 n) n))
		   (rtarget (target-or-register target)))
	       (LAP ,@(if (eq? rtarget source)
			  (LAP (TST L ,rtarget))
			  (LAP (MOV L ,source ,rtarget)))
		    (B GEQ (@PCR ,label))
		    (ADD L ,(make-immediate (* (-1+ absn) fixnum-1)) ,rtarget)
		    (LABEL ,label)
		    (ASH L ,(make-immediate (- 0 expt-of-2)) ,rtarget ,rtarget)
		    ,@(if (negative? n)
			  (LAP ,@(word->fixnum/ea rtarget rtarget)
			       (MNEG L ,rtarget ,target))
			  (word->fixnum/ea rtarget target))))))
	  (else
	   ;; This includes negative n.
	   (code-fixnum-quotient target source
				 (make-immediate (* n fixnum-1)))))))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(load-fixnum-constant 0 target)
	(code-fixnum-quotient target (make-immediate (* n fixnum-1))
			      source))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args-constant
  (lambda (target source n)
    ;; (remainder x y) is 0 or has the sign of x.
    ;; Thus we can always "divide" by (abs y) to make things simpler.
    (let ((n (if (negative? n) (- 0 n) n)))
      (cond ((= n 1)
	     (load-fixnum-constant 0 target))
	    ((integer-power-of-2? n)
	     =>
	     (lambda (expt-of-2)
	       (let ((sign (standard-temporary-reference))
		     (label (generate-label 'REM-MERGE))
		     (nbits (+ scheme-type-width expt-of-2)))
		  ;; This may produce a branch to a branch, but a
		  ;; peephole optimizer should be able to fix this.
		 (LAP (EXTV S (S 31) (S 1) ,source ,sign)
		      (EXTV Z (S 0) (S ,nbits) ,source ,target)
		      (B EQL (@PCR ,label))
		      (INSV ,sign (S ,nbits) (S ,(- 32 nbits)) ,target)
		      (LABEL ,label)))))
	    (else
	     (code-fixnum-remainder target source
				    (make-immediate (* n fixnum-1))))))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(load-fixnum-constant 0 target)
	(code-fixnum-remainder target (make-immediate (* n fixnum-1))
			       source))))

(define (code-fixnum-quotient target source1 source2)
  (let ((rtarget (target-or-register target)))
    (LAP ,@(if (eq? rtarget source1)
	       (LAP (DIV L ,source2 ,rtarget))
	       (LAP (DIV L ,source2 ,source1 ,rtarget)))
	 (ASH L (S ,scheme-type-width) ,rtarget ,target))))

(define (code-fixnum-remainder target source1 source2)
  #|
  ;; This does not work because the second arg to EDIV
  ;; is a quad and we have a long.  It must be sign extended.
  ;; In addition, the compiler does not currently support
  ;; consecutive register allocation so the work must be done
  ;; in memory.
  (LAP (EDIV ,source2 ,source1 ,(standard-temporary-reference)
	     ,target))
  |#
  (define (perform source-reg temp)
    ;; sign extend to quad on the stack
    (LAP (EXTV S (S 31) (S 1) ,source-reg (@-R 14))
	 (PUSHL ,source-reg)
	 (EDIV ,source2 (@R+ 14) ,temp ,target)))

  (let ((temp (standard-temporary-reference)))
    (if (effective-address/register? source1)
	(perform source1 temp)
	(LAP (MOV L ,source1 ,temp)
	     ,@(perform temp temp)))))

;;;; Predicate utilities

(define (signed-fixnum? n)
  (and (integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

(define (unsigned-fixnum? n)
  (and (integer? n)
       (not (negative? n))
       (< n unsigned-fixnum/upper-limit)))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (guarantee-unsigned-fixnum n)
  (if (not (unsigned-fixnum? n)) (error "Not a unsigned fixnum" n))
  n)

(define (fixnum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQL)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LSS)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GTR)
    (else
     (error "FIXNUM-PREDICATE->CC: Unknown predicate" predicate))))

(define-integrable (test-fixnum/ea ea)
  (LAP (TST L ,ea)))

(define (fixnum-predicate/register*constant register constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (if (zero? constant)
      (test-fixnum/ea (any-register-reference register))
      (LAP (CMP L ,(any-register-reference register)
		,(make-immediate (* constant fixnum-1))))))

(define (fixnum-predicate/memory*constant memory constant cc)
  (set-standard-branches! cc)
  (guarantee-signed-fixnum constant)
  (if (zero? constant)
      (test-fixnum/ea memory)
      (LAP (CMP L ,memory ,(make-immediate (* constant fixnum-1))))))