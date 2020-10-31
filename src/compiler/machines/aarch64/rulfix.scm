#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (REGISTER (? source))))
  (standard-unary target source object->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FIXNUM (CONSTANT (? constant))))
  (load-signed-immediate (standard-target! target) (* constant fixnum-1)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->OBJECT (REGISTER (? source))))
  (standard-unary target source fixnum->object))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ADDRESS->FIXNUM (REGISTER (? source))))
  (standard-unary target source address->fixnum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FIXNUM->ADDRESS (REGISTER (? source))))
  (standard-unary target source fixnum->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (ADDRESS->FIXNUM (OBJECT->ADDRESS (REGISTER (? source)))))
  ;; Works out the same.
  (standard-unary target source object->fixnum))

(define (object->fixnum target source)
  (LAP (LSL X ,target ,source (&U ,scheme-type-width))))

(define (fixnum->object target source)
  ;; XXX See if ORR can do the trick.
  (LAP (ADD X ,target ,source (&U ,type-code:fixnum))
       (ROR X ,target ,target (&U ,scheme-type-width))))

(define (address->fixnum target source)
  (LAP (LSL X ,target ,source (&U ,scheme-type-width))))

(define (fixnum->address target source)
  (LAP (LSR X ,target ,source (&U ,scheme-type-width))))

(define (word->fixnum target source)
  (LAP (AND X ,target ,source
            (& ,(bitwise-not (- (expt 2 scheme-type-width) 1))))))

(define-integrable fixnum-1
  (shift-left 1 scheme-type-width))

;;;; Unary Fixnum Operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  (standard-unary target source
    (lambda (target source)
      ((fixnum-1-arg/operator operator) target source overflow?))))

(define (fixnum-1-arg/operator operator)
  (lookup-arithmetic-method operator fixnum-methods/1-arg))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-arithmetic-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (target source overflow?)
    (assert (not overflow?))
    (LAP (EOR X ,target ,source (& ,(shift-left -1 scheme-type-width))))))

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source overflow?)
    (fixnum-add-constant target source +1 overflow?)))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source overflow?)
    (fixnum-add-constant target source -1 overflow?)))

(define (fixnum-add-constant target source n overflow?)
  (let ((imm (* fixnum-1 n)))
    (cond ((not overflow?)
           (add-immediate target source imm general-temporary!))
          ((zero? n)
           (set-never-branches!)
           (register->register-transfer source target))
          (else
           (set-overflow-branches!)
           (add-immediate-with-flags target source imm general-temporary!)))))

(define (load-fixnum-constant target n)
  (load-signed-immediate target (* n fixnum-1)))

;;;; Binary Fixnum Operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? operator)
                         (REGISTER (? source1))
                         (REGISTER (? source2))
                         (? overflow?)))
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      ((fixnum-2-args/operator operator) target source1 source2 overflow?))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? operator)
                         (REGISTER (? source1))
                         (OBJECT->FIXNUM (REGISTER (? source2)))
                         (? overflow?)))
  (QUALIFIER (detaggable-operator? operator))
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      ((fixnum-untagged*tagged/operator operator)
       target source1 source2 overflow?))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? operator)
                         (OBJECT->FIXNUM (REGISTER (? source1)))
                         (REGISTER (? source2))
                         (? overflow?)))
  (QUALIFIER
   (and (detaggable-operator? operator)
        (commutative-operator? operator)))
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      ((fixnum-untagged*tagged/operator operator)
       target source2 source1 overflow?))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? operator)
                         (REGISTER (? source))
                         (OBJECT->FIXNUM (CONSTANT (? constant)))
                         (? overflow?)))
  (standard-unary target source
    (lambda (target source)
      ((fixnum-register*constant/operator operator)
       target source constant overflow?))))

(define (fixnum-2-args/operator operator)
  (lookup-arithmetic-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define (fixnum-untagged*tagged/operator operator)
  (lookup-arithmetic-method operator fixnum-methods/untagged*tagged))

(define fixnum-methods/untagged*tagged
  (list 'FIXNUM-METHODS/2-ARGS))

(define (fixnum-register*constant/operator operator)
  (lookup-arithmetic-method operator fixnum-methods/register*constant))

(define fixnum-methods/register*constant
  (list 'FIXNUM-METHODS/REGISTER*CONSTANT))

(define (detaggable-operator? operator)
  (memq operator
        '(PLUS-FIXNUM
          MINUS-FIXNUM
          FIXNUM-AND
          FIXNUM-ANDC
          FIXNUM-OR
          FIXNUM-XOR)))

(define (commutative-operator? operator)
  (memq operator
        '(PLUS-FIXNUM
          FIXNUM-AND
          FIXNUM-OR
          FIXNUM-XOR)))

(define ((untagged*tagged-operator operate) target source1 source2 overflow?)
  (let ((source2 (INST-EA (LSL ,source2 ,scheme-type-width))))
    (operate target source1 source2 overflow?)))

(define (define-additive-method operator add adds sub subs)
  (define ((operate no-flags flags) target source1 source2 overflow?)
    (if overflow?
        (begin
          (set-overflow-branches!)
          (LAP (,flags X ,target ,source1 ,source2)))
        (LAP (,no-flags X ,target ,source1 ,source2))))
  (let ((operate+ (operate add adds))
        (operate- (operate sub subs)))
    (define-arithmetic-method operator fixnum-methods/2-args operate+)
    (define-arithmetic-method operator fixnum-methods/untagged*tagged
      (untagged*tagged-operator operate+))
    (define-arithmetic-method operator fixnum-methods/register*constant
      (lambda (target source constant overflow?)
        (define ((kernel operate) operand)
          (operate target source operand overflow?))
        (immediate-addition (* constant fixnum-1)
                            (kernel operate+)
                            (kernel operate-)
                            general-temporary!)))))

(define (define-bitwise-method operator op)
  (define (operate target source1 source2 overflow?)
    (assert (not overflow?))
    (LAP (,op X ,target ,source1 ,source2)))
  (define-arithmetic-method operator fixnum-methods/2-args operate)
  (define-arithmetic-method operator fixnum-methods/untagged*tagged
    (untagged*tagged-operator operate))
  (define-arithmetic-method operator fixnum-methods/register*constant
    (lambda (target source constant overflow?)
      (assert (not overflow?))
      (let ((imm (* constant fixnum-1)))
        (if (logical-imm-u64 imm)
            (LAP (,op X ,target ,source (&U ,imm)))
            (let ((temp (if (eqv? target source) (general-temporary!) target)))
              (LAP ,@(load-signed-immediate temp imm)
                   (,op X ,target ,source ,temp))))))))

(define-additive-method 'PLUS-FIXNUM 'ADD 'ADDS 'SUB 'SUBS)
(define-additive-method 'MINUS-FIXNUM 'SUB 'SUBS 'ADD 'ADDS)

(define-bitwise-method 'FIXNUM-AND 'AND)
(define-bitwise-method 'FIXNUM-OR 'ORR)
(define-bitwise-method 'FIXNUM-XOR 'EOR)

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (LAP (BIC X ,target ,source1 ,source2)
         ,@(word->fixnum target target))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/untagged*tagged
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (LAP (BIC X ,target ,source1 (LSL ,source2 ,scheme-type-width)))))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/register*constant
  (lambda (target register constant overflow?)
    (assert (not overflow?))
    (let ((imm (* (bitwise-not constant) fixnum-1)))
      (if (logical-imm-u64 imm)
          (LAP (AND X ,target ,register (&U ,imm)))
          (let ((temp (if (eqv? target register) (general-temporary!) target)))
            (LAP ,@(load-signed-immediate temp imm)
                 (AND X ,target ,register ,temp)))))))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    ;; We have x 2^t and y 2^t, and we want x y 2^t, so divide one of
    ;; them first by 2^t.
    (if (not overflow?)
        (LAP (ASR X ,regnum:scratch-0 ,source1 (&U ,scheme-type-width))
             (MUL X ,target ,regnum:scratch-0 ,source2))
        (let* ((mask regnum:scratch-0)
               (hi regnum:scratch-1)
               (temp (general-temporary!)))
          ;; We're going to test whether the high 64-bits is equal to
          ;; the -1 or 0 we expect it to be.  Overflow if not equal, no
          ;; overflow if equal.
          (set-not-equal-branches!)
          ;; Set mask to -1 if same sign, 0 if different sign.  The
          ;; mask is equal to the high 64 bits of a non-overflowing
          ;; multiply, so its xor with the high 64 bits is zero iff no
          ;; overflow.
          (LAP (CMP X ,source1 (&U 0))
               (CSETM X LT ,mask)
               (CMP X ,source2 (&U 0))
               (CINV X LT ,mask ,mask)
               (ASR X ,temp ,source1 (&U ,scheme-type-width))
               (SMULH X ,hi ,temp ,source2)
               (MUL X ,target ,temp ,source2)
               (CMP X ,mask ,hi))))))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (if (= source1 source2)             ;XXX Avoid this earlier on.
        (load-fixnum-constant target 1)
        (LAP (SDIV X ,target ,source1 ,source2)
             ;; source1 = n 2^t, source2 = d 2^t, target = q
             ;; target := q 2^t
             (LSL X ,target ,target (&U ,scheme-type-width))))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (if (= source1 source2)             ;XXX Avoid this earlier on.
        (load-fixnum-constant target 0)
        (LAP (SDIV X ,target ,source1 ,source2)
             ;; source1 = n 2^t, source2 = d 2^t, target = q
             ;; target := (n - d*q) 2^t
             (MSUB X ,target ,source2 ,target ,source1)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS FIXNUM-LSH
                         (REGISTER (? source1))
                         (REGISTER (? source2))
                         (? overflow?)))
  (assert (not overflow?))
  (need-registers! (list r0 r1))
  (let* ((load-r0 (load-machine-register! source1 r0))
         (load-r1 (load-machine-register! source2 r1)))
    (delete-dead-registers!)
    (rtl-target:=machine-register! target r0)
    (LAP ,@load-r0
         ,@load-r1
         ,@(invoke-hook/subroutine entry:compiler-fixnum-shift))))

(define (integer-power-of-2? x)
  (and (zero? (bitwise-and x (- x 1)))
       (bit-count (- x 1))))

(define (fixnum-multiply-constant target source n overflow?)
  (define (do-shift shift)
    (LAP (LSL X ,target ,source (&U ,(abs shift)))
         ,@(if (negative? n)
               (LAP (NEG X ,target ,target))
               (LAP))))
  (cond ((not overflow?)
         ;; XXX Special cases should be dispensed with earlier.
         (cond ((or (= n 0) (>= (abs n) (expt 2 scheme-datum-width)))
                (load-fixnum-constant target 0))
               ((= n 1)
                ;; XXX assign pseudos without transfer
                (register->register-transfer source target))
               ((= n -1)
                (LAP (NEG X ,target ,source)))
               ((integer-power-of-2? (abs n))
                => do-shift)
               (else
                (LAP ,@(load-signed-immediate regnum:scratch-0 n)
                     (MUL X ,target ,source ,regnum:scratch-0)))))
        ((= n 0)
         (set-never-branches!)
         (load-fixnum-constant target 0))
        ((= n 1)
         (set-never-branches!)
         ;; XXX assign pseudos without transfer
         (register->register-transfer source target))
        ((= n -1)
         ;; Overflows only if the source is the most negative fixnum,
         ;; which has no fixnum negation.
         (set-equal-branches!)
         (LAP ,@(cmp-immediate source
                               (* fixnum-1
                                  (shift-left -1 (- scheme-datum-width 1)))
                               general-temporary!)
              (NEG X ,target ,source)))
        ((integer-power-of-2? (abs n))
         => (lambda (shift)
              (let ((temp regnum:scratch-0))
                (set-not-equal-branches!)
                ;; Check whether the bits we're shifting out all
                ;; match the sign bit.
                (LAP (ASR X ,temp ,source
                          (&U ,(- scheme-object-width 1 shift)))
                     (CMP X ,temp (ASR ,source ,(- scheme-object-width 1)))
                     ,@(do-shift shift)))))
        (else
         (let ((mask regnum:scratch-0)
               (hi regnum:scratch-1)
               (temp (if (eqv? target source) (general-temporary!) target)))
           (set-not-equal-branches!)
           (LAP ,@(load-signed-immediate mask (if (negative? n) -1 0))
                (CMP X ,source (&U 0))
                ,@(load-signed-immediate temp n)
                (CINV X LT ,mask ,mask)
                (SMULH X ,hi ,source ,temp)
                (MUL X ,target ,source ,temp)
                (CMP X ,mask ,hi))))))

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/register*constant
  fixnum-multiply-constant)

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/register*constant
  (lambda (q-target n-source d-constant overflow?)
    (assert (not overflow?))
    ;; XXX fast division by multiplication
    (LAP ,@(load-fixnum-constant regnum:scratch-0 d-constant)
         (SDIV X ,q-target ,n-source ,regnum:scratch-0)
         (LSL X ,q-target ,q-target (&U ,scheme-type-width)))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/register*constant
  (lambda (q-target n-source d-constant overflow?)
    (assert (not overflow?))
    ;; XXX fast division by multiplication
    (LAP ,@(load-fixnum-constant regnum:scratch-0 d-constant)
         (SDIV X ,q-target ,n-source ,regnum:scratch-0)
         (MSUB X ,q-target ,regnum:scratch-0 ,q-target ,n-source))))

(define-arithmetic-method 'FIXNUM-LSH fixnum-methods/register*constant
  (lambda (target source n overflow?)
    ;; We only test for overflow in small nonzero left shifts.
    (assert (or (< 0 n (- scheme-datum-width 1)) (not overflow?)))
    (cond ((negative? n)
           (assert (not overflow?))
           (if (<= (- n) scheme-datum-width)
               (LAP (ASR X ,target ,source (&U ,(- n)))
                    ,@(word->fixnum target target))
               (load-fixnum-constant target 0)))
          ((< n scheme-datum-width)
           (fixnum-multiply-constant target source (expt 2 n) overflow?))
          (else
           (assert (not overflow?))
           (load-fixnum-constant target 0)))))

;;;; Fixnum Predicates

(define-rule predicate
  (OVERFLOW-TEST)
  ;; Preceding RTL instruction is always a fixnum operation with
  ;; OVERFLOW? set to true which will generate the right branch.
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (fixnum-branch! (fixnum-predicate/unary->binary predicate))
  (LAP (CMP X ,(standard-source! register) (&U 0))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG FIXNUM-ZERO? (REGISTER (? register)))
  (zero-test! (standard-source! register)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source1))
                      (REGISTER (? source2)))
  (fixnum-branch! predicate)
  (standard-binary-effect source1 source2
    (lambda (source1 source2)
      (LAP (CMP X ,source1 ,source2)))))

(define (fixnum-predicate/unary->binary predicate)
  (case predicate
    ((ZERO-FIXNUM?) 'EQUAL-FIXNUM?)
    ((NEGATIVE-FIXNUM?) 'LESS-THAN-FIXNUM?)
    ((POSITIVE-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    (else (error "Unknown unary predicate:" predicate))))

(define (fixnum-branch! predicate)
  (case predicate
    ((EQUAL-FIXNUM?)
     (set-equal-branches!))
    ((LESS-THAN-FIXNUM?)
     (set-condition-branches! 'LT 'GE))
    ((GREATER-THAN-FIXNUM?)
     (set-condition-branches! 'GT 'LE))
    ((UNSIGNED-LESS-THAN-FIXNUM?)
     (set-condition-branches! 'LO 'HS))
    ((UNSIGNED-GREATER-THAN-FIXNUM?)
     (set-condition-branches! 'HI 'LS))
    (else
     (error "Unknown fixnum predicate:" predicate))))
