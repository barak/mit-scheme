#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/rulfix.scm,v 4.32 1990/01/25 16:44:44 jinx Exp $
$MC68020-Header: rules1.scm,v 4.32 90/01/18 22:43:54 GMT cph Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Fixnum Rules

(declare (usual-integrations))

;;;; Conversions

(define-rule statement
  ;; convert a fixnum object to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target object->fixnum))

(define-rule statement
  ;; load a fixnum constant as a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-fixnum-constant constant (standard-target! target)))

(define-rule statement
  ;; convert a memory address to a "fixnum integer"
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (standard-unary-conversion source target address->fixnum))

(define-rule statement
  ;; convert an object's address to a "fixnum integer"
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  (standard-unary-conversion source target object->fixnum))

(define-rule statement
  ;; convert a "fixnum integer" to a fixnum object
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (standard-unary-conversion source target fixnum->object))

(define-rule statement
  ;; convert a "fixnum integer" to a memory address
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source target fixnum->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 #F))
  (standard-unary-conversion source target object->index-fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (REGISTER (? source)))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 #F))
  (standard-unary-conversion source target object->index-fixnum))

;; This is a patch for the time being.  Probably only one of these pairs
;; of rules is needed.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (OBJECT->FIXNUM (CONSTANT 4))
			 (REGISTER (? source))
			 #F))
  (standard-unary-conversion source target fixnum->index-fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT 4))
			 #F))
  (standard-unary-conversion source target fixnum->index-fixnum))

(define-integrable (fixnum->index-fixnum src tgt)
  (LAP (SHD () ,src 0 30 ,tgt)))

(define-integrable (object->fixnum src tgt)
  (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt)))

(define-integrable (object->index-fixnum src tgt)
  (LAP (SHD () ,src 0 ,(- scheme-datum-width 2) ,tgt)))

(define-integrable (address->fixnum src tgt)
  (LAP (SHD () ,src 0 ,scheme-datum-width ,tgt)))

(define-integrable (fixnum->object src tgt)
  (LAP ,@(load-immediate (ucode-type fixnum) regnum:addil-result)
       (SHD () ,regnum:addil-result ,src ,scheme-type-width ,tgt)))

(define (fixnum->address src tgt)
  (LAP (SHD () ,regnum:quad-bitmask ,src ,scheme-type-width ,tgt)))

(define (load-fixnum-constant constant target)
  (load-immediate (* constant fixnum-1) target))

(define-integrable fixnum-1
  (expt 2 scheme-type-width))

;;;; Arithmetic Operations

(define-rule statement
  ;; execute a unary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      ((fixnum-1-arg/operator operation) target source overflow?))))

(define (fixnum-1-arg/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/1-arg))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (if overflow?
	(LAP (ADDI (NSV) ,fixnum-1 ,src ,tgt))
	(LAP (ADDI () ,fixnum-1 ,src ,tgt)))))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (tgt src overflow?)
    (if overflow?
	(LAP (ADDI (NSV) ,(- fixnum-1) ,src ,tgt))
	(LAP (ADDI () ,(- fixnum-1) ,src ,tgt)))))

(define-rule statement
  ;; execute a binary fixnum operation
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (standard-binary-conversion source1 source2 target
    (lambda (source1 source2 target)
      ((fixnum-2-args/operator operation) target source1 source2 overflow?))))

(define (fixnum-2-args/operator operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(LAP (ADD (NSV) ,src1 ,src2 ,tgt))
	(LAP (ADD () ,src1 ,src2 ,tgt)))))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (tgt src1 src2 overflow?)
    (if overflow?
	(LAP (SUB (NSV) ,src1 ,src2 ,tgt))
	(LAP (SUB () ,src1 ,src2 ,tgt)))))

(define-rule statement
  ;; execute binary fixnum operation with constant second arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source))
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      ((fixnum-2-args/operator/register*constant operation)
       target source constant overflow?))))

(define-rule statement
  ;; execute binary fixnum operation with constant first arg
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (OBJECT->FIXNUM (CONSTANT (? constant)))
			 (REGISTER (? source))
			 (? overflow?)))
  (standard-unary-conversion source target
    (lambda (source target)
      (if (fixnum-2-args/commutative? operation)
	  ((fixnum-2-args/operator/register*constant operation)
	   target source constant overflow?)
	  ((fixnum-2-args/operator/constant*register operation)
	   target constant source overflow?)))))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM MULTIPLY-FIXNUM)))

(define (fixnum-2-args/operator/register*constant operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args/register*constant))

(define fixnum-methods/2-args/register*constant
  (list 'FIXNUM-METHODS/2-ARGS/REGISTER*CONSTANT))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (* constant fixnum-1)))
      (if overflow?
	  (cond ((zero? constant)
		 (LAP (SKIP (TR))))
		((fits-in-11-bits-signed? value)
		 (LAP (ADDI (NSV) ,value ,src ,tgt)))
		(else
		 (let ((temp (standard-temporary!)))
		   (LAP ,@(load-fixnum-constant constant temp)
			(ADD (NSV) ,src ,temp ,tgt)))))
	  (load-offset value src tgt)))))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args/register*constant
  (lambda (tgt src constant overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (- (* constant fixnum-1))))
      (if overflow?
	  (cond ((zero? constant)
		 (LAP (SKIP (TR))))
		((fits-in-11-bits-signed? value)
		 (LAP (ADDI (NSV) ,value ,src ,tgt)))
		(else
		 (let ((temp (standard-temporary!)))
		   (LAP ,@(load-fixnum-constant constant temp)
			(SUB (NSV) ,src ,temp ,tgt)))))
	  (load-offset value src tgt)))))

(define (fixnum-2-args/operator/constant*register operation)
  (lookup-arithmetic-method operation fixnum-methods/2-args/constant*register))

(define fixnum-methods/2-args/constant*register
  (list 'FIXNUM-METHODS/2-ARGS/CONSTANT*REGISTER))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args/constant*register
  (lambda (tgt constant src overflow?)
    (guarantee-signed-fixnum constant)
    (let ((value (* constant fixnum-1)))
      (if (fits-in-11-bits-signed? value)
	  (if overflow?
	      (LAP (SUBI (NSV) ,value ,src ,tgt))
	      (LAP (SUBI () ,value ,src ,tgt)))
	  (let ((temp (standard-temporary!)))
	    (LAP ,@(load-fixnum-constant constant temp)
		 ,@(if overflow?
		       (LAP (SUB (NSV) ,temp ,src ,tgt))
		       (LAP (SUB () ,temp ,src ,tgt)))))))))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

;;;; Predicates

;;; This is a kludge.  It assumes that the last instruction of the
;;; arithmetic operation that may cause an overflow condition will
;;; skip the following instruction if there was no overflow.  Ie., the
;;; last instruction will conditionally nullify using NSV.  The code
;;; for the alternative is a real kludge because we can't force the
;;; arithmetic instruction that precedes this code to use the inverted
;;; condition.  Hopefully the peephole optimizer will fix this if it
;;; is ever generated.  The linearizer attempts not to use this
;;; branch.

(define-rule predicate
  (OVERFLOW-TEST)
  (set-current-branches!
   (lambda (label)
     (LAP (B (N) (@PCR ,label))))
   (lambda (label)
     (LAP (SKIP (TR))
	  (B (N) (@PCR ,label)))))
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (compare (fixnum-pred-1->cc predicate)
	   (standard-source! source)
	   0))

(define (fixnum-pred-1->cc predicate)
  (case predicate
    ((ZERO-FIXNUM?) '=)
    ((NEGATIVE-FIXNUM?) '<)
    ((POSITIVE-FIXNUM?) '>)
    (else (error "unknown fixnum predicate" predicate))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (compare (fixnum-pred-2->cc predicate)
	   (standard-source! source1)
	   (standard-source! source2)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source))
		      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (compare-fixnum/constant*register (invert-condition-noncommutative
				     (fixnum-pred-2->cc predicate))
				    constant
				    (standard-source! source)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (OBJECT->FIXNUM (CONSTANT (? constant)))
		      (REGISTER (? source)))
  (compare-fixnum/constant*register (fixnum-pred-2->cc predicate)
				    constant
				    (standard-source! source)))

(define-integrable (compare-fixnum/constant*register cc n r)
  (guarantee-signed-fixnum n)
  (compare-immediate cc (* n fixnum-1) r))

(define (fixnum-pred-2->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM?) '=)
    ((LESS-THAN-FIXNUM?) '<)
    ((GREATER-THAN-FIXNUM?) '>)
    (else (error "unknown fixnum predicate" predicate))))