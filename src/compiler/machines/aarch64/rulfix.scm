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

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? operator)
                         (OBJECT->FIXNUM (CONSTANT (? constant)))
                         (REGISTER (? source))
                         (? overflow?)))
  (QUALIFIER (commutative-operator? operator))
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
          MULTIPLY-FIXNUM
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

(define (define-bitwise-method operator op opc map-constant)
  (define (operate target source1 source2 overflow?)
    (assert (not overflow?))
    (LAP (,op X ,target ,source1 ,source2)))
  (define-arithmetic-method operator fixnum-methods/2-args operate)
  (define-arithmetic-method operator fixnum-methods/untagged*tagged
    (untagged*tagged-operator operate))
  (define-arithmetic-method operator fixnum-methods/register*constant
    (lambda (target source constant overflow?)
      (assert (not overflow?))
      (let ((imm (* (map-constant constant) fixnum-1)))
        (if (logical-imm-u64 imm)
            (LAP (,opc X ,target ,source (&U ,imm)))
            (let ((temp (if (eqv? target source) (general-temporary!) target)))
              (LAP ,@(load-signed-immediate temp imm)
                   (,opc X ,target ,source ,temp))))))))

(define-additive-method 'PLUS-FIXNUM 'ADD 'ADDS 'SUB 'SUBS)
(define-additive-method 'MINUS-FIXNUM 'SUB 'SUBS 'ADD 'ADDS)

(define-bitwise-method 'FIXNUM-AND 'AND 'AND identity-procedure)
(define-bitwise-method 'FIXNUM-OR 'ORR 'ORR identity-procedure)
(define-bitwise-method 'FIXNUM-XOR 'EOR 'EOR identity-procedure)
(define-bitwise-method 'FIXNUM-ANDC 'BIC 'AND bitwise-not)

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    ;; We have x 2^t and y 2^t, and we want x y 2^t, so divide one of
    ;; them first by 2^t.
    (let ((temp regnum:scratch-0))
      (LAP (ASR X ,temp ,source1 (&U ,scheme-type-width))
           (MUL X ,target ,temp ,source2)
           ,@(if overflow?
                 (let ((hi regnum:scratch-1)
                       (sign-shift (- scheme-object-width 1)))
                   ;; Not equal => high half mismatches sign => overflow.
                   ;; Equal => high half matches sign => no overflow.
                   (set-not-equal-branches!)
                   (LAP (SMULH X ,hi ,temp ,source2)
                        (CMP X ,hi (ASR ,target ,sign-shift))))
                 (LAP))))))

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
           (LAP (ASR X ,target ,source (&U ,(min (- n) 63)))
                ,@(word->fixnum target target)))
          ((< n scheme-datum-width)
           (fixnum-multiply-constant target source (expt 2 n) overflow?))
          (else
           (assert (not overflow?))
           (load-fixnum-constant target 0)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS FIXNUM-LSH
                         (OBJECT->FIXNUM (REGISTER (? source)))
                         (OBJECT->FIXNUM (CONSTANT (? n)))
                         #f))
  (standard-unary target source
    (lambda (target source)
      (cond ((< n 0)
             (let* ((lsb (min (- n) (- scheme-datum-width 1)))
                    (width (- scheme-datum-width lsb)))
               (LAP (SBFX X ,target ,source (&U ,lsb) (&U ,width))
                    (LSL X ,target ,target (&U ,scheme-type-width)))))
            ((< n scheme-datum-width)
             (LAP (LSL X ,target ,source (&U ,(+ n scheme-type-width)))))
            (else
             (load-fixnum-constant target 0))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-1-ARG ONE-PLUS-FIXNUM (REGISTER (? source)) #f)))
  (add-immediate-and-tag target source 1 #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-1-ARG MINUS-ONE-PLUS-FIXNUM (REGISTER (? source)) #f)))
  (add-immediate-and-tag target source -1 #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS PLUS-FIXNUM
                          (REGISTER (? source))
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          #f)))
  (add-immediate-and-tag target source constant #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS PLUS-FIXNUM
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          (REGISTER (? source))
                          #f)))
  (add-immediate-and-tag target source constant #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS MINUS-FIXNUM
                          (REGISTER (? source))
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          #f)))
  (add-immediate-and-tag target source (- constant) #f))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS MINUS-FIXNUM
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          (REGISTER (? source))
                          #f)))
  (add-immediate-and-tag target source constant #t))

(define (add-immediate-and-tag target source n negate?)
  (standard-unary target source
    (lambda (target source)
      (let ((n (+ (* n fixnum-1) type-code:fixnum))
            (intermediate (if negate? target source)))
        (LAP ,@(if negate? (LAP (NEG Q ,intermediate ,source)) (LAP))
             ,@(add-immediate target intermediate n general-temporary!)
             (ROR X ,target ,target (&U ,scheme-type-width)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-1-ARG FIXNUM-NOT
                        (OBJECT->FIXNUM (REGISTER (? source)))
                        #f))
  ;; Intermediate rule
  (standard-unary target source
    (lambda (target source)
      (LAP ,@(object->fixnum target source)
           (EOR X ,target ,target (& ,(shift-left -1 scheme-type-width)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-1-ARG FIXNUM-NOT
                         (OBJECT->FIXNUM (REGISTER (? source)))
                         #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (standard-unary target source
    (lambda (target source)
      (LAP (EOR X ,target ,source (&U ,(bit-mask scheme-datum-width 0)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op detag-distributive-operator)
                         (OBJECT->FIXNUM (REGISTER (? source1)))
                         (OBJECT->FIXNUM (REGISTER (? source2)))
                         #f))
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      (LAP (,op X ,target ,source1 ,source2)
           ,@(object->fixnum target target)))))

(define (detag-distributive-operator operator)
  (case operator
    ((PLUS-FIXNUM) 'ADD)
    ((MINUS-FIXNUM) 'SUB)
    ((MULTIPLY-FIXNUM) 'MUL)
    ((FIXNUM-AND) 'AND)
    ((FIXNUM-ANDC) 'ANDC)
    ((FIXNUM-OR) 'ORR)
    ((FIXNUM-XOR) 'EOR)
    (else #f)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op tag-preserving-operator)
                          (OBJECT->FIXNUM (REGISTER (? source1)))
                          (OBJECT->FIXNUM (REGISTER (? source2)))
                          #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      (LAP (,op X ,target ,source1 ,source2)))))

(define (tag-preserving-operator operator)
  (case operator
    ((FIXNUM-AND) 'AND)
    ((FIXNUM-OR) 'ORR)
    (else #f)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op detag/tag-operator)
                          (OBJECT->FIXNUM (REGISTER (? tagged-source)))
                          (REGISTER (? untagged-source))
                          #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (detag-op-tag op target tagged-source untagged-source))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op commutative-detag/tag-operator)
                          (REGISTER (? untagged-source))
                          (OBJECT->FIXNUM (REGISTER (? tagged-source)))
                          #f)))
  (QUALIFIER compiler:assume-safe-fixnums?)
  (detag-op-tag op target tagged-source untagged-source))

(define (detag/tag-operator operator)
  (case operator
    ((FIXNUM-ANDC) 'BIC)
    ((FIXNUM-OR) 'ORR)
    ((FIXNUM-XOR) 'EOR)
    (else #f)))

(define (commutative-detag/tag-operator operator)
  (and (commutative-operator? operator)
       (detag/tag-operator operator)))

(define (detag-op-tag op target tagged-source untagged-source)
  (assert compiler:assume-safe-fixnums?)
  (standard-binary target tagged-source untagged-source
    (lambda (target tagged-source untagged-source)
      (LAP (,op X ,target ,tagged-source
                (LSR ,untagged-source ,scheme-type-width))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op or-operator)
                          (REGISTER (? source))
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          #f)))
  (QUALIFIER (taggable-constant? constant))
  (or-immediate-and-tag op target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op or-operator)
                          (OBJECT->FIXNUM (CONSTANT (? constant)))
                          (REGISTER (? source))
                          #f)))
  (QUALIFIER (taggable-constant? constant))
  (or-immediate-and-tag op target source constant))

(define (or-operator operator)
  (case operator
    ((FIXNUM-OR) 'ORR)
    ((FIXNUM-XOR) 'EOR)
    (else #f)))

(define (taggable-constant? constant)
  (and (logical-imm-s64 (bitwise-ior (* constant fixnum-1) type-code:fixnum))
       #t))

(define (or-immediate-and-tag op target source constant)
  (let ((n (bitwise-ior (* constant fixnum-1) type-code:fixnum)))
    (LAP (,op X ,target ,source (& ,n))
         (ROR X ,target ,target (&U ,scheme-type-width)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS FIXNUM-AND
                          (OBJECT->FIXNUM (REGISTER (? source)))
                          (OBJECT->FIXNUM (CONSTANT (? n)))
                          #f)))
  (QUALIFIER (and compiler:assume-safe-fixnums? (<= n 0) (logical-imm-s64 n)))
  (logical-immediate-in-place 'AND target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS FIXNUM-AND
                          (OBJECT->FIXNUM (CONSTANT (? n)))
                          (OBJECT->FIXNUM (REGISTER (? source)))
                          #f)))
  (QUALIFIER (and compiler:assume-safe-fixnums? (<= n 0) (logical-imm-s64 n)))
  (logical-immediate-in-place 'AND target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS FIXNUM-ANDC
                          (OBJECT->FIXNUM (REGISTER (? source)))
                          (OBJECT->FIXNUM (REGISTER (? n))))))
  (QUALIFIER
   (and compiler:assume-safe-fixnums?
        (<= 0 n)
        (logical-imm-s64 (bitwise-not n))))
  (logical-immediate-in-place 'AND target source (bitwise-not n)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op or-operator)
                          (OBJECT->FIXNUM (REGISTER (? source)))
                          (OBJECT->FIXNUM (CONSTANT (? n)))
                          #f)))
  (QUALIFIER (and compiler:assume-safe-fixnums? (<= n 0) (logical-imm-s64 n)))
  (logical-immediate-in-place op target source n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM->OBJECT
           (FIXNUM-2-ARGS (? op or-operator)
                          (OBJECT->FIXNUM (CONSTANT (? n)))
                          (OBJECT->FIXNUM (REGISTER (? source)))
                          #f)))
  (QUALIFIER (and (<= n 0) (logical-imm-s64 n)))
  (logical-immediate-in-place op target source n))

(define (logical-immediate-in-place op target source n)
  (assert compiler:assume-safe-fixnums?)
  (assert (logical-imm-s64 n))
  (standard-unary target source
    (lambda (target source)
      (LAP (,op X ,target ,source (& ,n))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op left-shiftable-operator)
                         (REGISTER (? source1))
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (REGISTER (? source2))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         ;; XXX Could handle overflow with ADDS/SUBS,
                         ;; not clear worthwhile.
                         #f))
  (QUALIFIER (<= 0 n))
  (left-shift-op op target source1 source2 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op left-shiftable-operator)
                         (REGISTER (? source1))
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (OBJECT->FIXNUM (REGISTER (? source2)))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         #f))
  (QUALIFIER (<= 0 n))
  (detag-left-shift-op op target source1 source2 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op commutative-left-shiftable-operator)
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (REGISTER (? source1))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         (REGISTER (? source2))
                         #f))
  (QUALIFIER (<= 0 n))
  (left-shift-op op target source2 source1 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op commutative-left-shiftable-operator)
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (OBJECT->FIXNUM (REGISTER (? source1)))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         (REGISTER (? source2))
                         #f))
  (QUALIFIER (<= 0 n))
  (detag-left-shift-op op target source2 source1 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op right-shiftable-operator)
                         (REGISTER (? source1))
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (REGISTER (? source2))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         #f))
  (QUALIFIER (<= n 0))
  (right-shift-op op target source1 source2 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op right-shiftable-operator)
                         (REGISTER (? source1))
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (OBJECT->FIXNUM (REGISTER (? source2)))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         #f))
  (QUALIFIER (<= n 0))
  (detag-right-shift-op op target source1 source2 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op commutative-right-shiftable-operator)
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (REGISTER (? source1))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         (REGISTER (? source2))
                         #f))
  (QUALIFIER (<= n 0))
  (right-shift-op op target source2 source1 n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FIXNUM-2-ARGS (? op commutative-right-shiftable-operator)
                         (FIXNUM-2-ARGS FIXNUM-LSH
                                        (OBJECT->FIXNUM (REGISTER (? source1)))
                                        (OBJECT->FIXNUM (CONSTANT (? n)))
                                        #f)
                         (REGISTER (? source2))
                         #f))
  (QUALIFIER (<= n 0))
  (detag-right-shift-op op target source2 source1 n))

(define (left-shiftable-operator operator)
  (case operator
    ((PLUS-FIXNUM) 'ADD)
    ((MINUS-FIXNUM) 'SUB)
    ((FIXNUM-AND) 'AND)
    ((FIXNUM-ANDC) 'BIC)
    ((FIXNUM-OR) 'ORR)
    ((FIXNUM-XOR) 'EOR)
    (else #f)))

(define (commutative-left-shiftable-operator operator)
  (and (commutative-operator? operator)
       (left-shiftable-operator operator)))

(define (left-shift-op op target source1 source2 n)
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      (LAP (,op X ,target ,source1
                ,(if (>= n scheme-datum-width)
                     (INST-EA Z)
                     (INST-EA (LSL ,source2 ,n))))))))

(define (detag-left-shift-op op target untagged-source tagged-source n)
  (left-shift-op op target untagged-source tagged-source
                 (+ n scheme-type-width)))

(define (right-shiftable-operator operator)
  (case operator
    ((FIXNUM-AND) 'AND)
    ((FIXNUM-ANDC) 'BIC)
    ((FIXNUM-OR) 'ORR)
    ((FIXNUM-XOR) 'EOR)
    (else #f)))

(define (commutative-right-shiftable-operator operator)
  (and (commutative-operator? operator)
       (right-shiftable-operator operator)))

(define (right-shift-op op target source1 source2 n)
  (standard-binary target source1 source2
    (lambda (target source1 source2)
      (LAP (,op X ,target ,source1
                (ASR ,source2 ,(min (- n) (- scheme-datum-width 1))))
           ,@(case op
               ((AND ANDC) (LAP))
               (else (word->fixnum target target)))))))

(define (detag-right-shift-op op target untagged-source tagged-source n)
  (standard-binary target untagged-source tagged-source
    (lambda (target untagged-source tagged-source)
      (let ((temp regnum:scratch-0)
            (lsb (- n))
            (width (+ scheme-datum-width n)))
        (LAP ,@(if (<= scheme-datum-width lsb)
                   ;; XXX Handle this case earlier.
                   (load-fixnum-constant temp -1)
                   (LAP (SBFX X ,temp ,tagged-source (&U ,lsb) (&U ,width))))
             (,op X ,target ,untagged-source
                  (LSL ,temp ,scheme-type-width)))))))

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

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source1))
                      (OBJECT->FIXNUM (REGISTER (? source2))))
  (fixnum-branch! predicate)
  (standard-binary-effect source1 source2
    (lambda (source1 source2)
      (LAP (CMP X ,source1 (LSL ,source2 ,scheme-type-width))))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (OBJECT->FIXNUM (REGISTER (? source1)))
                      (REGISTER (? source2)))
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (standard-binary-effect source1 source2
    (lambda (source1 source2)
      (LAP (CMP X ,source2 (LSL ,source1 ,scheme-type-width))))))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? register))
                      (OBJECT->FIXNUM (CONSTANT (? constant))))
  (fixnum-branch! predicate)
  (let ((register (standard-source! register)))
    (cmp-immediate register (* constant fixnum-1) general-temporary!)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (OBJECT->FIXNUM (CONSTANT (? constant)))
                      (REGISTER (? register)))
  (fixnum-branch! (commute-fixnum-predicate predicate))
  (let ((register (standard-source! register)))
    (cmp-immediate register (* constant fixnum-1) general-temporary!)))

(define (fixnum-predicate/unary->binary predicate)
  (case predicate
    ((ZERO-FIXNUM?) 'EQUAL-FIXNUM?)
    ((NEGATIVE-FIXNUM?) 'LESS-THAN-FIXNUM?)
    ((POSITIVE-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    (else (error "Unknown unary predicate:" predicate))))

(define (commute-fixnum-predicate predicate)
  (case predicate
    ((EQUAL-FIXNUM?) 'EQUAL-FIXNUM?)
    ((LESS-THAN-FIXNUM?) 'GREATER-THAN-FIXNUM?)
    ((GREATER-THAN-FIXNUM?) 'LESS-THAN-FIXNUM?)
    ((UNSIGNED-LESS-THAN-FIXNUM?) 'UNSIGNED-GREATER-THAN-FIXNUM?)
    ((UNSIGNED-GREATER-THAN-FIXNUM?) 'UNSIGNED-LESS-THAN-FIXNUM?)
    (else (error "Unknown binary predicate:" predicate))))

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
