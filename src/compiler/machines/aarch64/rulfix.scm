#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
  (load-immediate (standard-target! target) (* constant fixnum-1) #t))

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
  (LAP (ORR X ,target ,source (&U ,type-code:fixnum))
       (ROR X ,target ,target (&U ,scheme-type-width))))

(define (address->fixnum target source)
  (LAP (LSL X ,target ,source (&U ,scheme-type-width))))

(define (fixnum->address target source)
  (LAP (LSR X ,target ,source (&U ,scheme-type-width))))

(define (word->fixnum target source)
  (LAP (AND X ,target ,source (&U ,(- (expt 2 scheme-type-width) 1)))))

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
    (LAP (MVN X ,target ,source))))

(define-arithmetic-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source overflow?)
    (fixnum-add-constant target source +1 overflow?)))

(define-arithmetic-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (target source overflow?)
    (fixnum-add-constant target source -1 overflow?)))

(define (set-always-branches!)
  (set-current-branches! (lambda (label) (LAP (B (@PCR ,label))))
                         (lambda (label) label (LAP))))

(define (set-never-branches!)
  (set-current-branches! (lambda (label) label (LAP))
                         (lambda (label) (LAP (B (@PCR ,label))))))

(define (set-carry-branches!)
  (set-current-branches! (lambda (label) (LAP (B.CS (@PCR ,label))))
                         (lambda (label) (LAP (B.CC (@PCR ,label))))))

(define (fixnum-add-constant target source n overflow?)
  (let ((imm (* fixnum-1 n)))
    (cond ((not overflow?)
           (add-immediate target source imm))
          ((zero? n)
           (set-never-branches!)
           (register->register-transfer source target))
          (else
           (set-carry-branches!)
           (add-immediate-with-flags target source imm)))))

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

(define (fixnum-2-args/operator operator)
  (lookup-arithmetic-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define ((fixnum-2-args/additive flags no-flags)
         target source1 source2 overflow?)
  (if overflow?
      (begin
        (set-carry-branches!)
        (LAP (,flags ,target ,source1 ,source2)))
      (LAP (,no-flags ,target ,source1 ,source2))))

(define ((fixnum-2-args/bitwise op) target source1 source2 overflow?)
  (assert (not overflow?))
  (LAP (,op ,target ,source1 ,source2)))

(define-arithmetic-method 'PLUS-FIXNUM fixnum-methods/2-args
  (fixnum-2-args/additive 'ADDS 'ADD))

(define-arithmetic-method 'MINUS-FIXNUM fixnum-methods/2-args
  (fixnum-2-args/additive 'SUBS 'SUB))

(define-arithmetic-method 'FIXNUM-AND fixnum-methods/2-args
  (fixnum-2-args/bitwise 'AND))

(define-arithmetic-method 'FIXNUM-ANDC fixnum-methods/2-args
  (fixnum-2-args/bitwise 'BIC))         ;Bitwise Bit Clear

(define-arithmetic-method 'FIXNUM-OR fixnum-methods/2-args
  (fixnum-2-args/bitwise 'ORR))

(define-arithmetic-method 'FIXNUM-XOR fixnum-methods/2-args
  (fixnum-2-args/bitwise 'EOR))         ;fans of Winnie the Pooh

(define-arithmetic-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    ;; We have x 2^t and y 2^t, and we want x y 2^t, so divide one of
    ;; them first by 2^t.
    (if (not overflow?)
        (LAP (ASR ,regnum:scratch-0 ,source1 (&U ,scheme-type-width))
             (MUL ,target ,regnum:scratch-0 ,source2))
        (let* ((mask (allocate-temporary-register! 'GENERAL))
               (hi (allocate-temporary-register! 'GENERAL)))
          ;; We're going to test whether the high 64-bits is equal to
          ;; the -1 or 0 we expect it to be.  Overflow if not equal, no
          ;; overflow if equal.
          (set-not-equal-branches!)
          ;; Set mask to -1 if same sign, 0 if different sign.  The
          ;; mask is equal to the high 64 bits of a non-overflowing
          ;; multiply, so its xor with the high 64 bits is zero iff no
          ;; overflow.
          (LAP (MOVZ X ,mask (&U 0))
               (CMP X ,source1 (&U 0))
               (CINV.LT X ,mask ,mask)
               (CMP X ,source2 (&U 0))
               (CINV.LT X ,mask ,mask)
               (ASR X ,regnum:scratch-0 ,source1 (&U ,scheme-type-width))
               (SMULH ,hi ,regnum:scratch-0 ,source2)
               (MUL X ,target ,regnum:scratch-0 ,source2)
               (CMP X ,mask ,hi))))))

(define-arithmetic-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (if (= source1 source2)             ;XXX Avoid this earlier on.
        (load-fixnum-constant target 1)
        (LAP (SDIV X ,target ,source1 ,source2)
             (LSL X ,target ,target (&U ,scheme-type-width))))))

(define-arithmetic-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source1 source2 overflow?)
    (assert (not overflow?))
    (if (= source1 source2)             ;XXX Avoid this earlier on.
        (load-fixnum-constant target 0)
        (LAP (SDIV X ,target ,source1 ,source2)
             ;; source1 = n, source2 = d, target = q
             ;; target := n - d*q
             (MSUB X ,target ,source1 ,source2 ,target)
             (LSL X ,target ,target (&U ,scheme-type-width))))))

;; XXX Constant operands.
;; XXX Fast division by multiplication.

;;;; Fixnum Predicates

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate) (REGISTER (? register)))
  (fixnum-branch! (fixnum-predicate/unary->binary predicate))
  (LAP (CMP X ,(standard-source! register) (& 0))))

(define-rule predicate
  (FIXNUM-PRED-1-ARG FIXNUM-ZERO? (REGISTER (? register)))
  (zero-test! (standard-source! register)))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source1))
                      (REGISTER (? source2)))
  (fixnum-branch! predicate)
  (standard-unary-effect source1 source2
    (lambda ()
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
     (set-current-branches! (lambda (label) (LAP (B.LT (@PCR ,label))))
                            (lambda (label) (LAP (B.GE (@PCR ,label))))))
    ((GREATER-THAN-THAN-FIXNUM?)
     (set-current-branches! (lambda (label) (LAP (B.GT (@PCR ,label))))
                            (lambda (label) (LAP (B.LE (@PCR ,label))))))
    ((UNSIGNED-LESS-THAN-FIXNUM?)
     (set-current-branches! (lambda (label) (LAP (B.MI (@PCR ,label))))
                            (lambda (label) (LAP (B.PL (@PCR ,label))))))
    ((UNSIGNED-LESS-THAN-FIXNUM?)
     (set-current-branches! (lambda (label) (LAP (B.PL (@PCR ,label))))
                            (lambda (label) (LAP (B.MI (@PCR ,label))))))
    (else
     (error "Unknown fixnum predicate:" predicate))))
