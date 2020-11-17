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

;;;; LAP Generation Rules: Flonum rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;; register: RTL representation of machine (not pseudo) register
;; fpr: LAP representation of SIMD/FP register
;;
;; (not needed for general because the numbers coincide)

(define (float-source-register! register)
  (assert (eq? 'FLOAT (register-type register)))
  (load-alias-register! register 'FLOAT))

(define (float-source-fpr! register)
  (float-register->fpr (float-source-register! register)))

(define (float-move-to-temporary-register! register)
  (assert (eq? 'FLOAT (register-type register)))
  (move-to-temporary-register! register 'FLOAT))

(define (float-move-to-temporary-fpr! register)
  (float-register->fpr (float-move-to-temporary-register! register)))

(define (float-target-register! register)
  (assert (eq? 'FLOAT (register-type register)))
  (delete-dead-registers!)
  (allocate-alias-register! register 'FLOAT))

(define (float-target-fpr! register)
  (float-register->fpr (float-target-register! register)))

(define (float-temporary-register!)
  (allocate-temporary-register! 'FLOAT))

(define (float-temporary-fpr!)
  (float-register->fpr (float-temporary-register!)))

(define (allocate-binary64-label bit-string)
  (allocate-data-label bit-string 'BINARY64 0 8
    (LAP (DATA 64 U ,(bit-string->unsigned-integer bit-string)))))

(define binary64-bits:+inf
  (unsigned-integer->bit-string 64 #x7ff0000000000000))

(define binary64-bits:smallest-normal
  (unsigned-integer->bit-string 64 #x0010000000000000))

;; Layout:
;;
;;      0       manifest-nm-vector [length = 1 word]
;;      8       double (binary64) float
;;      16

;;;; Flonum Loads & Stores

;; Float load/store.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (standard-source! source))
         (target (float-target-fpr! target))
         (temp regnum:scratch-0))
    (LAP ,@(object->address temp source)
         (LDR.V D ,target (+ ,temp (&U (* 8 1)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FLOAT->OBJECT (REGISTER (? source))))
  (let* ((source (float-source-fpr! source))
         (target (standard-target! target))
         (temp regnum:scratch-0)
         (Free regnum:free-pointer))
    (LAP
     ;; target := Free
     ,@(register->register-transfer Free target)
     ;; temp := manifest-nm-vector, length = 1 word
     ,@(load-tagged-immediate temp (ucode-type MANIFEST-NM-VECTOR) 1)
     ;; *Free++ := temp
     (STR X ,temp (POST+ ,Free (& 8)))
     ;; *Free++ := source
     (STR.V D ,source (POST+ ,Free (& 8)))
     ;; Tag it.
     ,@(affix-type target type-code:flonum target (lambda () temp)))))

;; Constant-offset floating vector load/store.

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLOAT-OFFSET (REGISTER (? base))
                        (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (fits-in-unsigned-12? offset))
  (let* ((base (standard-source! base))
         (target (float-target-fpr! target)))
    (LAP (LDR.V D ,target (+ ,base (&U (* 8 ,offset)))))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
                        (MACHINE-CONSTANT (? offset)))
          (REGISTER (? source)))
  (QUALIFIER (fits-in-unsigned-12? offset))
  (let* ((base (standard-source! base))
         (source (float-source-fpr! source)))
    (LAP (STR.V D ,source (+ ,base (&U (* 8 ,offset)))))))

;; Variable-offset floating vector load/store.

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLOAT-OFFSET (REGISTER (? base))
                        (REGISTER (? offset))))
  (let* ((base (standard-source! base))
         (offset (standard-source! offset))
         (target (float-target-fpr! target)))
    (LAP (LDR.V D ,target (+ ,base (LSL ,offset 3))))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
                        (REGISTER (? offset)))
          (REGISTER (? source)))
  (let* ((base (standard-source! base))
         (offset (standard-source! offset))
         (source (float-source-fpr! source)))
    (LAP (STR.V D ,source (+ ,base (LSL ,offset 3))))))

;;;; Flonum Arithmetic -- unary operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  overflow?                             ;ignore
  (let* ((source (float-source-fpr! source))
         (target (float-target-fpr! target)))
    ((flonum-1-arg/operator operator) target source)))

(define-integrable (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define-arithmetic-method 'FLONUM-ABS flonum-methods/1-arg
  (lambda (target source)
    (LAP (FABS D ,target ,source))))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    (LAP (FNEG D ,target ,source))))

(define-arithmetic-method 'FLONUM-SQRT flonum-methods/1-arg
  (lambda (target source)
    (LAP (FSQRT D ,target ,source))))

(define-arithmetic-method 'FLONUM-CEILING flonum-methods/1-arg
  (lambda (target source)
    ;; round toward Plus infinity
    (LAP (FRINTP D ,target ,source))))

(define-arithmetic-method 'FLONUM-FLOOR flonum-methods/1-arg
  (lambda (target source)
    ;; round toward Minus infinity
    (LAP (FRINTM D ,target ,source))))

(define-arithmetic-method 'FLONUM-ROUND flonum-methods/1-arg
  (lambda (target source)
    ;; round to Nearest (ties to even)
    (LAP (FRINTN D ,target ,source))))

(define-arithmetic-method 'FLONUM-TRUNCATE flonum-methods/1-arg
  (lambda (target source)
    ;; round toward Zero
    (LAP (FRINTZ D ,target ,source))))

;;;; Flonum Arithmetic -- binary operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-2-ARGS (? operator)
                         (REGISTER (? source1))
                         (REGISTER (? source2))
                         (? overflow?)))
  overflow?                             ;ignore
  ((flonum-2-args/operator operator) target source1 source2))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define-integrable (flonum-2-args/operator operator)
  (lookup-arithmetic-method operator flonum-methods/2-args))

(define ((flonum-2-args/standard operate) target source1 source2)
  (let* ((source1 (float-source-fpr! source1))
         (source2 (float-source-fpr! source2))
         (target (float-target-fpr! target)))
    (operate target source1 source2)))

(define ((flonum-2-args/temporary operate) target source1 source2)
  ;; Allocate a temporary register that we can write to without
  ;; destroying source1 and source2; then assign it as an alias for the
  ;; target at the end.
  (let* ((source1 (float-source-fpr! source1))
         (source2 (float-source-fpr! source2))
         (temp-register (float-temporary-register!)))
    (delete-dead-registers!)
    (rtl-target:=machine-register! target temp-register)
    (operate (float-register->fpr temp-register) source1 source2)))

(define-arithmetic-method 'FLONUM-ADD flonum-methods/2-args
  (flonum-2-args/standard
   (lambda (target source1 source2)
     (LAP (FADD D ,target ,source1 ,source2)))))

(define-arithmetic-method 'FLONUM-SUBTRACT flonum-methods/2-args
  (flonum-2-args/standard
   (lambda (target source1 source2)
     (LAP (FSUB D ,target ,source1 ,source2)))))

(define-arithmetic-method 'FLONUM-MULTIPLY flonum-methods/2-args
  (flonum-2-args/standard
   (lambda (target source1 source2)
     (LAP (FMUL D ,target ,source1 ,source2)))))

(define-arithmetic-method 'FLONUM-DIVIDE flonum-methods/2-args
  (flonum-2-args/standard
   (lambda (target source1 source2)
     (LAP (FDIV D ,target ,source1 ,source2)))))

(define-arithmetic-method 'FLONUM-COPYSIGN flonum-methods/2-args
  (flonum-2-args/temporary
   (lambda (target source1 source2)
     (let ((tempx regnum:scratch-0))
       (LAP (MOVZ X ,tempx (LSL (&U #x8000) 48))
            (FMOV D ,target X ,tempx)
            ;; target[i] := source2[i] if signbit[i] else source1[i]
            (BSL B8 ,target ,source2 ,source1))))))

;;;; Fused multiply/add/negate

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-3-ARGS FLONUM-FMA
                         (REGISTER (? factor1))
                         (REGISTER (? factor2))
                         (REGISTER (? addend))
                         (? overflow?)))
  overflow?                             ;ignore
  ((flonum-3-args/standard
    (lambda (target factor1 factor2 addend)
      (LAP (FMADD D ,target ,factor1 ,factor2 ,addend))))
   target factor1 factor2 addend))

;;; XXX The following rules are busted because RTL instruction folding
;;; (rcompr.scm) doesn't know how to search through flonum operations
;;; or something -- and FIND-REFERENCE-INSTRUCTION is too mysterious
;;; for me to understand at this hour!

;;; XXX What about (fma x y (- 0. z)) or (- 0. (fma x y z))?  Need to
;;; check sign of zero for results.

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-3-ARGS FLONUM-FMA
                         (REGISTER (? factor1))
                         (REGISTER (? factor2))
                         (FLONUM-1-ARG FLONUM-NEGATE
                                       (REGISTER (? subtrahend))
                                       (? overflow0?))
                         (? overflow1?)))
  overflow0? overflow1?                 ;ignore
  ((flonum-3-args/standard
    (lambda (target factor1 factor2 subtrahend)
      (LAP (FMSUB D ,target ,factor1 ,factor2 ,subtrahend))))
   target factor1 factor2 subtrahend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-1-ARG FLONUM-NEGATE
                        (FLONUM-3-ARGS FLONUM-FMA
                                       (REGISTER (? factor1))
                                       (REGISTER (? factor2))
                                       (REGISTER (? addend))
                                       (? overflow0?))
                        (? overflow1?)))
  overflow0? overflow1?                 ;ignore
  ((flonum-3-args/standard
    (lambda (target factor1 factor2 addend)
      (LAP (FNMADD D ,target ,factor1 ,factor2 ,addend))))
   target factor1 factor2 addend))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-1-ARG FLONUM-NEGATE
                        (FLONUM-3-ARGS FLONUM-FMA
                                       (REGISTER (? factor1))
                                       (REGISTER (? factor2))
                                       (FLONUM-1-ARG FLONUM-NEGATE
                                                     (REGISTER (? subtrahend))
                                                     (? overflow0?))
                                       (? overflow1?))
                        (? overflow2?)))
  overflow0? overflow1? overflow2?      ;ignore
  ((flonum-3-args/standard
    (lambda (target factor1 factor2 subtrahend)
      (LAP (FNMSUB D ,target ,factor1 ,factor2 ,subtrahend))))
   target factor1 factor2 subtrahend))

(define ((flonum-3-args/standard operate) target source1 source2 source3)
  (let* ((source1 (float-source-fpr! source1))
         (source2 (float-source-fpr! source2))
         (source3 (float-source-fpr! source3))
         (target (float-target-fpr! target)))
    (operate target source1 source2 source3)))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (let* ((predicate (flonum-pred-1->2-args predicate))
         (source1 (float-source-fpr! source))
         (source2 'Z))
    (float-branch! predicate)
    (float-comparison predicate source1 source2)))

(define (flonum-pred-1->2-args predicate)
  (case predicate
    ((FLONUM-NEGATIVE?) 'FLONUM-LESS?)
    ((FLONUM-IS-ZERO?) 'FLONUM-IS-EQUAL?)
    ((FLONUM-ZERO?) 'FLONUM-EQUAL?)
    ((FLONUM-POSITIVE?) 'FLONUM-POSITIVE?)
    (else (error "Invalid flonum-pred-1-arg:" predicate))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NEGATIVE? (REGISTER (? source)))
  (let* ((source (float-source-fpr! source))
         ;; Avoid scratch-0, which may be used in the branches.
         (temp regnum:scratch-1))
    (set-test-bit-set-branches! temp 63)
    (LAP (FMOV X ,temp D ,source))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NAN? (REGISTER (? source)))
  (let ((source (float-source-fpr! source)))
    (set-condition-branches! 'VS 'VC)
    (LAP (FCMP D ,source ,source))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-INFINITE? (REGISTER (? source)))
  ;; Discriminate on |x| = +inf, versus |x| =/= +inf or unordered.
  (set-condition-branches! 'EQ 'NE)
  (let ((source (float-source-fpr! source)))
    (delete-dead-registers!)
    (let* ((temp-inf (float-temporary-fpr!))
           (temp-source (float-temporary-fpr!))
           (label (allocate-binary64-label binary64-bits:+inf)))
      (LAP ,@(load-pc-relative-float temp-inf regnum:scratch-0 label)
           (FABS D ,temp-source ,source)
           (FCMP D ,temp-source ,temp-inf)))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-FINITE? (REGISTER (? source)))
  ;; Discriminate on |x| < +inf, versus |x| >= +inf or unordered.
  (set-condition-branches! 'LO 'HS)     ;LO = lt; HS = gt, eq, or un
  (let ((source (float-source-fpr! source)))
    (delete-dead-registers!)
    (let* ((temp-inf (float-temporary-fpr!))
           (temp-source (float-temporary-fpr!))
           (label (allocate-binary64-label binary64-bits:+inf)))
      (LAP ,@(load-pc-relative-float temp-inf regnum:scratch-0 label)
           (FABS D ,temp-source ,source)
           (FCMP D ,temp-source ,temp-inf)))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NORMAL? (REGISTER (? source)))
  ;; Break it into two steps:
  ;;
  ;;    1. Is |x| < +inf, versus |x| >= +inf or unordered?
  ;;    2. Is |x| >= smallest normal, versus |x| < smallest normal?
  ;;
  ;; The branches are:
  ;;
  ;;    GE: gt or eq, when n = v
  ;;    LO: lt, when c = 0
  ;;
  (set-condition-branches! 'GE 'LO)
  (let ((source (float-source-fpr! source)))
    (delete-dead-registers!)
    (let* ((temp-inf (float-temporary-fpr!))
           (temp-norm (float-temporary-fpr!))
           (temp-source (float-temporary-fpr!))
           (label-inf (allocate-binary64-label binary64-bits:+inf))
           (label-norm
            (allocate-binary64-label binary64-bits:smallest-normal)))
      ;; XXX Use LDP to load both registers in one go.
      (LAP ,@(load-pc-relative-float temp-inf regnum:scratch-0 label-inf)
           ,@(load-pc-relative-float temp-norm regnum:scratch-0 label-norm)
           (FABS D ,temp-source ,source)
           (FCMP D ,temp-source ,temp-inf)
           ;; If |x| < +inf, compare source and norm; otherwise, if |x|
           ;; >= +inf or |x| and +inf are unordered, set NCVB := 1000
           ;; so that GE will be not-taken and LO will be taken.
           (FCCMP D ,temp-source ,temp-norm (&U #b1000) LT)))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source1))
                      (REGISTER (? source2)))
  (let* ((source1 (float-source-fpr! source1))
         (source2 (float-source-fpr! source2)))
    (float-branch! predicate)
    (float-comparison predicate source1 source2)))

(define (float-branch! predicate)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-IS-EQUAL?)
     (set-equal-branches!))
    ((FLONUM-LESS? FLONUM-IS-LESS?)
     (set-condition-branches! 'LO 'HS)) ;LO = lt; HS = gt, eq, or un
    ((FLONUM-GREATER? FLONUM-IS-GREATER?)
     (set-condition-branches! 'GT 'LE)) ;LE = le, eq, or un
    ((FLONUM-IS-LESS-OR-EQUAL?)         ;XXX FLONUM-LESS-OR-EQUAL?
     (set-condition-branches! 'LS 'HI)) ;LS = le or eq; HI = gt or un
    ((FLONUM-IS-GREATER-OR-EQUAL?)      ;XXX FLONUM-GREATER-OR-EQUAL?
     (set-condition-branches! 'GE 'LT)) ;LT = lt or un
    ((FLONUM-IS-LESS-OR-GREATER?)       ;XXX FLONUM-LESS-OR-GREATER?
     (set-current-branches!
      (lambda (label)                   ;branch to label if less or greater
        (let ((unordered (generate-label 'UNORDERED)))
          (LAP
           ;; branch if unordered (V=1)
           (B. VS (@PCR ,unordered ,regnum:scratch-0))
           ;; branch if less or greater
           (B. NE (@PCR ,label ,regnum:scratch-0))
           (LABEL ,unordered))))
      (lambda (label)                   ;branch to label if NaN or equal
        (LAP
         ;; branch if unordered (V=1)
         (B. VS (@PCR ,label ,regnum:scratch-0))
         ;; branch if equal
         (B. EQ (@PCR ,label ,regnum:scratch-0))))))
    ((FLONUM-IS-UNORDERED?)
     (set-condition-branches! 'VS 'VC))
    (else
     (error "Unknown float predicate:" predicate))))

(define (float-comparison predicate source1 source2)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-LESS? FLONUM-GREATER?)
     ;; floating-point compare with exception
     (LAP (FCMPE D ,source1 ,source2)))
    ((FLONUM-IS-EQUAL?
      FLONUM-IS-LESS?
      FLONUM-IS-GREATER?
      FLONUM-IS-LESS-OR-EQUAL?
      FLONUM-IS-GREATER-OR-EQUAL?
      FLONUM-IS-LESS-OR-GREATER?
      FLONUM-IS-UNORDERED?)
     ;; floating-point quiet compare
     (LAP (FCMP D ,source1 ,source2)))
    (else
     (error "Invalid float comparison:" predicate source1 source2))))
