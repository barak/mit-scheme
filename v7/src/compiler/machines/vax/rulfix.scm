#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rulfix.scm,v 1.2 1989/12/20 22:42:20 cph Rel $
$MC68020-Header: rules1.scm,v 4.22 89/04/27 20:06:32 GMT cph Exp $

Copyright (c) 1989 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Fixnum operations.  DEC VAX version.

;;; Note: This corresponds to part of rules1 for MC68020.
;;; Hopefully the MC68020 version will be split along the
;;; same lines.

(declare (usual-integrations))

;;;; Utilities

(define-integrable (standard-fixnum-reference reg)
  (standard-register-reference reg false))

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

(define (load-fixnum-constant constant register-reference)
  (cond ((zero? constant)
	 (INST (CLR L ,register-reference)))
	((and (positive? constant) (< constant 64))
	 (INST (ASH L (S 8) (S ,constant) ,register-reference)))
	(else
	 (let* ((constant (* constant #x100))
		(size (datum-size constant)))
	   (cond ((not (eq? size 'L))
		  (INST (CVT ,size L (& ,constant) ,register-reference)))
		 ((and (positive? constant) (< constant #x10000))
		  (INST (MOVZ W L (& ,constant) ,register-reference)))
		 (else
		  (INST (MOV L (& ,constant) ,register-reference))))))))

(define (test-fixnum effective-address)
  (INST (TST L ,effective-address)))

(define (fixnum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQL)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LSS)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GTR)
    (else (error "FIXNUM-PREDICATE->CC: Unknown predicate" predicate))))

(define (fixnum-operation-target? target)
  (or (rtl:register? target)
      (rtl:offset? target)))

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

(define-integrable (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM MULTIPLY-FIXNUM)))

;;;; Data conversion

(define-integrable (object->fixnum source reg-ref)
  (LAP (ASH L (S 8) ,source ,reg-ref)))

(define-integrable (ct/object->fixnum object target)
  (LAP ,(load-fixnum-constant object target)))
    
(define-integrable (address->fixnum source reg-ref)
  (LAP (ASH L (S 8) ,source ,reg-ref)))

(define-integrable (ct/address->fixnum address target)
  (LAP ,(load-fixnum-constant (object-datum address) target)))

(define-integrable (fixnum->address source reg-ref)
  ;; This assumes that the low bits have 0s.
  (LAP (ROTL (& -8) ,source ,reg-ref)))

(define-integrable (ct/fixnum->address fixnum target)
  (LAP ,(load-immediate fixnum target)))

(define (fixnum->object source reg-ref target)
  (if (eq? source reg-ref)
      (LAP (MOV B (S ,(ucode-type fixnum)) ,reg-ref)
	   (ROTL (& -8) ,reg-ref ,target))
      ;; This assumes that the low 8 bits are 0
      (LAP (BIS L (S ,(ucode-type fixnum)) ,source ,reg-ref)
	   (ROTL (& -8) ,reg-ref ,target))))

(define-integrable (ct/fixnum->object fixnum target)
  (LAP ,(load-constant fixnum target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (CONSTANT (? constant)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/constant->register target constant
				     address->fixnum
				     ct/address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (OFFSET (REGISTER (? address))
						    (? offset)))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (QUALIFIER (pseudo-register? target))
  (load-fixnum-constant constant (standard-target-reference target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source object->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (OFFSET (REGISTER (? address)) (? offset))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/offset->register target address offset object->fixnum))    

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register
   target source
   (lambda (source target)
     (fixnum->object source target target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (convert-object/register->register target source fixnum->address))

(define (register-fixnum->temp->object reg target)
  (with-temporary-register-copy! reg 'GENERAL
    (lambda (temp)
      (fixnum->object temp temp target))
    (lambda (source temp)
      (fixnum->object source temp target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (FIXNUM->OBJECT (REGISTER (? source))))
  (let ((target (indirect-reference! a n)))
    (register-fixnum->temp->object source target)))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 12) 1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (register-fixnum->temp->object r (INST-EA (@R+ 12))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 14) -1)
	  (FIXNUM->OBJECT (REGISTER (? r))))
  (register-fixnum->temp->object r (INST-EA (@-R 14))))

;;;; Arithmetic operations

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2)
    (cond ((eq? source1 target)
	   (LAP (ADD L ,source2 ,target)))
	  ((eq? source2 target)
    	   (LAP (ADD L ,source1 ,target)))
	  (else
	   (LAP (ADD L ,source1 ,source2 ,target))))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((eq? source target)
	   (if (zero? n)
	       (LAP)
	       (LAP (ADD L (& ,(* n #x100)) ,target))))
	  ((zero? n)
	   (LAP (MOV L ,source ,target)))
	  (else
	   (LAP (ADD L (& ,(* n #x100)) ,source ,target))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2)
    (cond ((eq? source1 target)
	   (if (equal? source1 source2)
	       (LAP (ASH L (& -4) ,target ,target)
		    (MUL L ,target ,target))
	       (LAP (ASH L (& -8) ,target ,target)
		    (MUL L ,source2 ,target))))
	  ((eq? source2 target)
	   (LAP (ASH L (& -8) ,target ,target)
		(MUL L ,source1 ,target)))
	  (else
	   (LAP (ASH L (& -8) ,source1 ,target)
		(MUL L ,source2 ,target))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((zero? n)
	   (LAP (CLR L ,target)))
	  ((eq? source target)
	   (cond ((= n 1)
		  (LAP))
		 ((= n -1)
		  (LAP (MNEG L ,target ,target)))
		 ((integer-log-base-2? n)
		  =>
		  (lambda (power-of-2)
		    (LAP (ASH L ,(make-immediate power-of-2)
			      ,target ,target))))
		 (else
		  (LAP (MUL L ,(make-immediate n) ,target)))))
	  ((= n 1)
	   (MOV L ,source ,target))
	  ((= n -1)
	   (LAP (MNEG L ,source ,target)))
	  ((integer-log-base-2? n)
	   =>
	   (lambda (power-of-2)
	     (LAP (ASH L ,(make-immediate power-of-2) ,source ,target))))
	  (else
	   (LAP (MUL L ,(make-immediate n) ,source ,target))))))

(define (integer-log-base-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else (loop (* 2 power) (1+ exponent))))))

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source)
    (if (eq? source target)
	(LAP (ADD L (& #x100) ,target))
	(LAP (ADD L (& #x100) ,source ,target)))))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source)
    (if (eq? source target)
	(LAP (SUB L (& #x100) ,target))
	(LAP (SUB L (& #x100) ,source ,target)))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2)
    (cond ((equal? source1 source2)
	   (LAP (CLR L ,target)))
	  ((eq? source1 target)
	   (LAP (SUB L ,source2 ,target)))
	  (else
	   (LAP (SUB L ,source2 ,source1 ,target))))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target source n)
    (cond ((eq? source target)
	   (if (zero? n)
	       (LAP)
	       (LAP (SUB L (& ,(* n #x100)) ,target))))
	  ((zero? n)
	   (LAP (MOV L ,source ,target)))
	  (else
	   (LAP (SUB L (& ,(* n #x100)) ,source ,target))))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-tnatsnoc
  (lambda (target n source)
    (if (zero? n)
	(LAP (MNEG L ,source ,target))
	(LAP (SUB L ,source (& ,(* n #x100)) ,target)))))

;;;; Operation utilities

(define (fixnum-choose-target target operate-on-pseudo operate-on-target)
  (case (rtl:expression-type target)
    ((REGISTER)
     (let ((register (rtl:register-number target)))
       (if (pseudo-register? register)
	   (operate-on-pseudo register)
	   (operate-on-target (register-reference register)))))
    ((OFFSET)
     (operate-on-target (offset->indirect-reference! target)))
    (else
     (error "fixnum-choose-target: Unknown fixnum target" target))))

(define (fixnum-1-arg target source operation)
  (fixnum-choose-target
   target
   (lambda (target)
     (let ((get-target (register-copy-if-available source 'GENERAL target)))
       (if get-target
	   (let ((target (get-target)))
	     (operation target target))
	   (let* ((source (standard-fixnum-reference source))
		  (target (standard-target-reference target)))
	     (operation target source)))))
   (lambda (target)
     (operation target (standard-fixnum-reference source)))))
	     
(define (fixnum-2-args target source1 source2 operation)
  (fixnum-choose-target
   target
   (lambda (target)
     (let ((get-target (register-copy-if-available source1 'GENERAL target)))
       (if get-target
	   (let* ((source2 (standard-fixnum-reference source2))
		  (target (get-target)))
	     (operation target target source2))
	   (let ((get-target
		  (register-copy-if-available source2 'GENERAL target)))
	     (if get-target
		 (let* ((source1 (standard-fixnum-reference source1))
			(target (get-target)))
		   (operation target source1 target))
		 (let ((source1 (standard-fixnum-reference source1))
		       (source2 (standard-fixnum-reference source2)))
		   (operation (standard-target-reference target)
			      source1
			      source2)))))))
   (lambda (target)
     (let* ((source1 (standard-fixnum-reference source1))
	    (source2 (standard-fixnum-reference source2)))
       (operation target source1 source2)))))

;;;; Operation rules

(define-rule statement
  (ASSIGN (? target) (FIXNUM-1-ARG (? operator) (REGISTER (? source))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (fixnum-1-arg target source (fixnum-1-arg/operate operator)))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (fixnum-2-args/register*constant operator target source constant))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (if (fixnum-2-args/commutative? operator)
      (fixnum-2-args/register*constant operator target source constant)
      (fixnum-2-args/constant*register operator target constant source)))

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

;;; This code is disabled on the MC68020 because of shifting problems.
;; The constant 4 is treated especially because it appears in computed
;; vector-{ref,set!} operations.

(define (convert-index->fixnum/register target source)
  (fixnum-1-arg
   target source
   (lambda (target source)
     (LAP (ASH L (S 10) ,source ,target)))))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (convert-index->fixnum/register target source))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))))
  (QUALIFIER (and (fixnum-operation-target? target) (pseudo-register? source)))
  (convert-index->fixnum/register target source))

(define (convert-index->fixnum/offset target address offset)
  (let ((source (indirect-reference! address offset)))
    (fixnum-choose-target
     target
     (lambda (pseudo)
       (LAP (ASH L (S 10) ,source ,(standard-target-reference pseudo))))
     (lambda (target)
       (LAP (ASH L (S 10) ,source ,target))))))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))))
  (QUALIFIER (fixnum-operation-target? target))
  (convert-index->fixnum/offset target r n))

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (OFFSET (REGISTER (? r)) (? n)))
			 (OBJECT->FIXNUM (CONSTANT 4))))
  (QUALIFIER (fixnum-operation-target? target))
  (convert-index->fixnum/offset target r n))

;;;; General 2 operand rules

(define-rule statement
  (ASSIGN (? target)
	  (FIXNUM-2-ARGS (? operator)
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (QUALIFIER (and (fixnum-operation-target? target)
		  (not (eq? operator 'MULTIPLY-FIXNUM))
		  (pseudo-register? source1)
		  (pseudo-register? source2)))
  (fixnum-2-args target source1 source2 (fixnum-2-args/operate operator)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (QUALIFIER (and (pseudo-register? source1)
		  (pseudo-register? source2)))
  (fixnum-2-args `(REGISTER ,target)
		 source1 source2
		 (fixnum-2-args/operate 'MULTIPLY-FIXNUM)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (? offset))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source1))
			 (REGISTER (? source2))))
  (QUALIFIER (and (pseudo-register? source1)
		  (pseudo-register? source2)))
  (let ((target (indirect-reference! base offset)))
    (let ((get-temp (temporary-copy-if-available source1 'GENERAL)))
      (if get-temp
	  (let ((source2 (standard-fixnum-reference source2))
		(temp (get-temp)))
	    (LAP (ASH L (& -8) ,temp ,temp)
		 (MUL L ,temp ,source2 ,target)))
	  (let ((get-temp (temporary-copy-if-available source2 'GENERAL)))
	    (if get-temp
		(let ((source1 (standard-fixnum-reference source1))
		      (temp (get-temp)))
		  (LAP (ASH L (& -8) ,temp ,temp)
		       (MUL L ,source1 ,temp ,target)))
		(let ((source1 (standard-fixnum-reference source1))
		      (source2 (standard-fixnum-reference source2))
		      (temp (reference-temporary-register! 'GENERAL)))
		  (LAP (ASH L (& -8) ,source1 ,temp)
		       (MUL L ,temp ,source2 ,target)))))))))

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (QUALIFIER (pseudo-register? register))
  (set-standard-branches! (fixnum-predicate->cc predicate))
  (test-fixnum (standard-fixnum-reference register)))

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
  (if (zero? constant)
      (LAP ,(test-fixnum (standard-fixnum-reference register)))
      (LAP (CMP L ,(standard-fixnum-reference register)
		(& ,(* constant #x100))))))

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
  (if (zero? constant)
      (LAP ,(test-fixnum memory))
      (LAP (CMP L ,memory (& ,(* constant #x100))))))

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