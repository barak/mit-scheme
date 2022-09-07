#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(define double-flobits:zero
  (unsigned-integer->bit-string 64 #x0000000000000000))

(define double-flobits:negative-zero
  (unsigned-integer->bit-string 64 #x8000000000000000))

(define double-flobits:positive-inf
  (unsigned-integer->bit-string 64 #x7ff0000000000000))

(define (flonum-source! source)
  (or (register-alias source 'FLOAT)
      (load-alias-register! source 'FLOAT)))

(define-integrable (flonum-source-reference! source)
  (register-reference (flonum-source! source)))

(define (flonum-target! target)
  (delete-dead-registers!)
  (or (register-alias target 'FLOAT)
      (allocate-alias-register! target 'FLOAT)))

(define-integrable (flonum-target-reference! target)
  (register-reference (flonum-target! target)))

;;; FLONUM-DATA-OFFSET is the number of bytes after the location
;;; addressed by a FLONUM-tagged pointer before the actual flonum data
;;; begin.

(define-integrable flonum-data-offset address-units-per-object)

(define-rule statement
  (ASSIGN (REGISTER (? target)) (FLOAT->OBJECT (REGISTER (? source))))
  (let* ((source (flonum-source-reference! source))
         (target (target-register-reference target)))
    (LAP ,@(with-unsigned-immediate-operand
               (make-non-pointer-literal (ucode-type MANIFEST-NM-VECTOR) 1)
             (lambda (operand)
               (LAP (MOV Q (@R ,regnum:free-pointer) ,operand))))
         (MOVF S D (@RO ,regnum:free-pointer ,flonum-data-offset) ,source)
         (MOV Q ,target (&U ,(make-non-pointer-literal (ucode-type FLONUM) 0)))
         (OR Q ,target (R ,regnum:free-pointer))
         (LEA Q (R ,regnum:free-pointer)
              (@RO ,regnum:free-pointer
                   ,(+ flonum-data-offset address-units-per-float))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (move-to-temporary-register! source 'GENERAL))
         (target (flonum-target-reference! target)))
    (LAP ,@(object->address (register-reference source))
         (MOVF S D ,target (@RO ,source ,flonum-data-offset)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:simple-float-offset?))
  (let* ((source (float-offset->reference! expression))
         (target (flonum-target-reference! target)))
    (LAP (MOVF S D ,target ,source))))

(define-rule statement
  (ASSIGN (? expression rtl:simple-float-offset?) (REGISTER (? source)))
  (let ((source (flonum-source-reference! source))
        (target (float-offset->reference! expression)))
    (LAP (MOVF S D ,target ,source))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (? expression rtl:detagged-float-offset?))
  (with-detagged-float-location expression
    (lambda (source)
      (LAP (MOVF S D ,(flonum-target-reference! target) ,source)))))

(define-rule statement
  (ASSIGN (? expression rtl:detagged-float-offset?) (REGISTER (? source)))
  (with-detagged-float-location expression
    (lambda (target)
      (LAP (MOVF S D ,target ,(flonum-source-reference! source))))))

(define (with-detagged-float-location rtl-expression receiver)
  (with-decoded-detagged-float-offset rtl-expression
    (lambda (base float-index object-offset)
      (with-indexed-address base float-index address-units-per-float
          (* address-units-per-object object-offset)
          ;; No general registers to protect -- the target and source
          ;; will always be float registers.
          #f
        receiver))))

;;; These are nearly identical copies of RTL:DETAGGED-OFFSET? and
;;; WITH-DECODED-DETAGGED-OFFSET, with FLOAT-OFFSET substituted for
;;; OFFSET.  It is unfortunate that the RTL doesn't have a clearer
;;; abstraction of offsets and addresses with arbitrary data.

(define (rtl:detagged-float-offset? expression)
  (and (rtl:float-offset? expression)
       (rtl:machine-constant? (rtl:float-offset-offset expression))
       (let ((base (rtl:float-offset-base expression)))
         (and (rtl:float-offset-address? base)
              (rtl:detagged-index? (rtl:float-offset-address-base base)
                                   (rtl:float-offset-address-offset base))))
       expression))

(define (with-decoded-detagged-float-offset expression receiver)
  (let ((base (rtl:float-offset-base expression)))
    (let ((base* (rtl:float-offset-address-base base))
          (index (rtl:float-offset-address-offset base)))
      (receiver
       (rtl:register-number (if (rtl:register? base*)
                                base*
                                (rtl:object->address-expression base*)))
       (rtl:register-number (if (rtl:register? index)
                                index
                                (rtl:object->datum-expression index)))
       (rtl:machine-constant-value (rtl:float-offset-offset expression))))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-1-ARG (? operator) (REGISTER (? source)) (? overflow?)))
  overflow?                             ;ignore
  (let* ((source (flonum-source-reference! source))
         (target (flonum-target-reference! target)))
    ((flonum-1-arg/operator operator) target source)))

(define-integrable (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define ((flonum-unary-operation/target-bits bit-string operate) target source)
  (LAP (MOVF S D ,target (@PCR ,(allocate-double-float-bits-label bit-string)))
       ,@(operate target source)))

(define-arithmetic-method 'FLONUM-ABS flonum-methods/1-arg
  (flonum-unary-operation/target-bits
   (bit-string-not double-flobits:negative-zero)
   (lambda (target source)
     ;; No scalar version, but doing this packed is harmless.
     (LAP (ANDF P D ,target ,source)))))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (flonum-unary-operation/target-bits
   double-flobits:negative-zero
   (lambda (target source)
     ;; No scalar version, but doing this packed is harmless.
     (LAP (XORF P D ,target ,source)))))

(define-arithmetic-method 'FLONUM-SQRT flonum-methods/1-arg
  (lambda (target source)
    (LAP (SQRTF S D ,target ,source))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-2-ARGS (? operator)
                         (REGISTER (? source1))
                         (REGISTER (? source2))
                         (? overflow?)))
  overflow?                             ;ignore
  ((flonum-2-args/operator operator) target source1 source2))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-2-ARGS (? operator)
                         (REGISTER (? source))
                         (OBJECT->FLOAT (CONSTANT (? constant)))
                         (? overflow?)))
  overflow?                             ;ignore
  ((flonum-register*constant/operator operator) target source constant))

(define-rule statement
  (ASSIGN (REGISTER (? target))
          (FLONUM-2-ARGS (? operator)
                         (OBJECT->FLOAT (CONSTANT (? constant)))
                         (REGISTER (? source))
                         (? overflow?)))
  overflow?                             ;ignore
  ((flonum-constant*register/operator operator) target constant source))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define flonum-methods/register*constant
  (list 'FLONUM-METHODS/REGISTER*CONSTANT))

(define flonum-methods/constant*register
  (list 'FLONUM-METHODS/CONSTANT*REGISTER))

(define-integrable (flonum-2-args/operator operator)
  (lookup-arithmetic-method operator flonum-methods/2-args))

(define-integrable (flonum-register*constant/operator operator)
  (lookup-arithmetic-method operator flonum-methods/register*constant))

(define-integrable (flonum-constant*register/operator operator)
  (lookup-arithmetic-method operator flonum-methods/constant*register))

(define ((flonum-2-args/standard commutative? operate) target source1 source2)
  (binary-register-operation operate commutative? 'FLOAT
                             (lambda (target source)
                               (LAP (MOVF S D ,target ,source)))
                             target source1 source2))

(define ((flonum-register*constant/standard operate) target source constant)
  (with-float-operand constant
    (lambda (operand)
      (operate
       (register-reference (move-to-alias-register! source 'FLOAT target))
       operand))))

;++ Possible improvement, not currently easy with the generic register
;++ allocator operations provided: if the constant is zero and we have
;++ a temporary register available, we can zero that with XOR and use
;++ it in the place of loading a PC-relative double in memory.

(define ((flonum-constant*register/commutative operate) target constant source)
  (with-float-operand constant
    (lambda (operand)
      (operate
       (register-reference (move-to-alias-register! source 'FLOAT target))
       operand))))

(define ((flonum-constant*register/noncommutative operate)
         target constant source)
  (let* ((source (flonum-source-reference! source))
         (target (flonum-target-reference! target)))
    (with-float-operand constant
      (lambda (operand)
        (LAP (MOVF S D ,target ,operand)
             ,@(operate target source))))))

(let-syntax
    ((binary-operation
      (sc-macro-transformer
       (lambda (form environment)
         environment                    ;ignore
         (let ((name (cadr form))
               (op (caddr form))
               (commutative? (cadddr form)))
           `(let ((operate
                   (lambda (target source)
                     (LAP (,op S D ,',target ,',source)))))
              (define-arithmetic-method ',name flonum-methods/2-args
                (flonum-2-args/standard ,commutative? operate))
              (define-arithmetic-method ',name flonum-methods/register*constant
                (flonum-register*constant/standard operate))
              (define-arithmetic-method ',name flonum-methods/constant*register
                (,(if commutative?
                      'flonum-constant*register/commutative
                      'flonum-constant*register/noncommutative)
                 operate))))))))
  (binary-operation FLONUM-ADD ADDF #t)
  (binary-operation FLONUM-DIVIDE DIVF #f)
  (binary-operation FLONUM-MULTIPLY MULF #t)
  (binary-operation FLONUM-SUBTRACT SUBF #f))

;;; It is tempting to use memory operands for the masks, but because
;;; OR, AND, &c., are packed, not scalar, the memory operands must be
;;; 128-bit-aligned, and right now compiled code blocks are only
;;; guaranteed to be 64-bit aligned.  We could change that but it would
;;; take a bit of work.

(define-arithmetic-method 'FLONUM-COPYSIGN flonum-methods/2-args
  (flonum-2-args/standard #f
    (lambda (target source)
      (let* ((bits (bit-string-not double-flobits:negative-zero))
             (label (allocate-double-float-bits-label bits))
             (temp (reference-temporary-register! 'FLOAT)))
        (LAP
         ;; Set temp := 0x7fff....
         (MOVF S D ,temp (@PCR ,label))
         ;; target holds arg1.  Set target := arg1 & 0x7fff....
         (ANDF P D ,target ,temp)
         ;; source holds arg2.  Set temp := arg2 & 0x1000....
         (ANDNF P D ,temp ,source)
         ;; Set target := (arg1 & 0x7fff...) & (arg2 & 0x1000...).
         (ORF P D ,target ,temp))))))

(define-arithmetic-method 'FLONUM-COPYSIGN flonum-methods/register*constant
  (lambda (target source constant)
    (let ((target (float-move-to-target! source target)))
      (if (flo:sign-negative? constant)
          (let* ((bits double-flobits:negative-zero)
                 (label (allocate-double-float-bits-label bits))
                 (temp (reference-temporary-register! 'FLOAT)))
            (LAP (MOVF S D ,temp (@PCR ,label))
                 (ORF P D ,target ,temp)))
          (let* ((bits (bit-string-not double-flobits:negative-zero))
                 (label (allocate-double-float-bits-label bits))
                 (temp (reference-temporary-register! 'FLOAT)))
            (LAP (MOVF S D ,temp (@PCR ,label))
                 (ANDF P D ,target ,temp)))))))

(define-arithmetic-method 'FLONUM-COPYSIGN flonum-methods/constant*register
  (lambda (target constant source)
    (with-float-operand (flo:abs constant)
      (lambda (operand)
        (let ((target (float-move-to-target! source target)))
          (let* ((bits double-flobits:negative-zero)
                 (label (allocate-double-float-bits-label bits))
                 (temp (reference-temporary-register! 'FLOAT)))
            ;; target holds arg2, which specifies the sign.
            (LAP
             ;; temp := 0x1000....
             (MOVF S D ,temp (@PCR ,label))
             ;; target := target & 0x1000...
             (ANDF P D ,target ,temp)
             ;; temp := abs(arg1)
             (MOVF S D ,temp ,operand)
             ;; target := (target & 0x1000...) | abs(arg1)
             (ORF P D ,target ,temp))))))))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (flonum-branch!
   (flonum-pred-1->2-args predicate)
   (flonum-source-reference! source)
   (INST-EA (@PCR ,(allocate-double-float-bits-label double-flobits:zero)))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NEGATIVE? (REGISTER (? source)))
  (set-current-branches! (lambda (label)
                           (LAP (JNZ (@PCR ,label))))
                         (lambda (label)
                           (LAP (JZ (@PCR ,label)))))
  (let ((temp (temporary-register-reference)))
    (LAP (MOVMSKF P D ,temp ,(flonum-source-reference! source))
         (TEST B ,temp (&U 1)))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source1))
                      (REGISTER (? source2)))
  (flonum-branch! predicate
                  (flonum-source-reference! source1)
                  (flonum-source-reference! source2)))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
                      (REGISTER (? source))
                      (OBJECT->FLOAT (CONSTANT (? constant))))
  (with-float-operand constant
    (lambda (operand)
      (flonum-branch! predicate (flonum-source-reference! source) operand))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
                      (OBJECT->FLOAT (CONSTANT (? constant)))
                      (REGISTER (? source)))
  (with-float-operand constant
    (lambda (operand)
      (flonum-branch! (commute-flonum-predicate predicate)
                      (flonum-source-reference! source)
                      operand))))

;;; For a predicate giving (if (predicate x) a b), return predicate* so
;;; that (if (predicate* x 0.) a b) is equivalent.

(define (flonum-pred-1->2-args predicate)
  (case predicate
    ((FLONUM-ZERO?) 'FLONUM-EQUAL?)
    ((FLONUM-NEGATIVE?) 'FLONUM-LESS?)
    ((FLONUM-POSITIVE?) 'FLONUM-GREATER?)
    (else (error "Invalid flonum-pred-1-arg:" predicate))))

;;; For predicate giving (if (predicate x y) a b), return predicate* so
;;; that (if (predicate* y x) a b) is equivalent.

(define (commute-flonum-predicate predicate)
  (case predicate
    ((FLONUM-EQUAL?) 'FLONUM-EQUAL?)
    ((FLONUM-LESS?) 'FLONUM-GREATER?)
    ((FLONUM-GREATER?) 'FLONUM-LESS?)
    ((FLONUM-IS-EQUAL?) 'FLONUM-IS-EQUAL?)
    ((FLONUM-IS-LESS?) 'FLONUM-IS-GREATER?)
    ((FLONUM-IS-LESS-OR-EQUAL?) 'FLONUM-IS-GREATER-OR-EQUAL?)
    ((FLONUM-IS-GREATER?) 'FLONUM-IS-LESS?)
    ((FLONUM-IS-GREATER-OR-EQUAL?) 'FLONUM-IS-LESS-OR-EQUAL?)
    ((FLONUM-IS-UNORDERED?) 'FLONUM-IS-UNORDERED?)
    ((FLONUM-IS-LESS-OR-GREATER?) 'FLONUM-IS-LESS-OR-GREATER?)
    (else (error "commute-flonum-predicate: Unknown predicate" predicate))))

(define (flonum-branch! predicate source1 source2)
  ;; (U)COMI sets
  ;; - if unordered:    zf=1 pf=1 cf=1
  ;; - if greater:      zf=0 pf=0 cf=0
  ;; - if less:         zf=0 pf=0 cf=1
  ;; - if equal:        zf=1 pf=0 cf=0
  ;;
  ;; (`Unordered' means NaN.)
  ;;
  ;; Thus:
  ;; - JP (pf=1) is jump if NaN
  ;; - JNP (pf=0) is jump if number
  ;; - JE=JZ (zf=1) is jump if equal or NaN = not (less or greater)
  ;; - JNE=JNZ (zf=0) is jump if not (equal or NaN) = less or greater
  ;; - JA=JNBE (cf=0 & zf=0) is jump if greater = not (less or equal or NaN)
  ;; - JAE=JNB (cf=0) is jump if greater or equal = not (less or NaN)
  ;; - JB=JNAE (cf=1) is jump if less or NaN = not (greater or equal)
  ;; - JBE=JNA (cf=1 | zf=1) is jump if not greater = less or equal or NaN
  ;;
  ;; None of the other (distinct) jump condition codes involve only zf,
  ;; pf, and cf.  (JC=JB=JNAE and JNC=JAE=JNB too.)
  ;;
  ;; Apparently x86 doesn't like <, <=, or = -- they require two
  ;; conditional jumps while all the rest take only one.  Go figure.
  ;;
  ;; XXX Needs more automatic tests.
  ;;
  ;; XXX Support signalling FLONUM-LESS-OR-EQUAL?,
  ;; FLONUM-GREATER-OR-EQUAL?, and FLONUM-LESS-OR-GREATER?.
  ;;
  (define (signalling-comparison)
    (LAP (COMIF S D ,source1 ,source2)))
  (define (quiet-comparison)
    (LAP (UCOMIF S D ,source1 ,source2)))
  (case predicate
    ((FLONUM-EQUAL? FLONUM-IS-EQUAL?)
     (set-current-branches! (lambda (label)
                              (let ((unordered (generate-label 'UNORDERED)))
                                (LAP (JP (@PCR ,unordered))
                                     (JE (@PCR ,label))
                                     (LABEL ,unordered))))
                            (lambda (label)
                              (LAP (JNE (@PCR ,label))
                                   (JP (@PCR ,label)))))
     (case predicate
       ((FLONUM-EQUAL?) (signalling-comparison))
       ((FLONUM-IS-EQUAL?) (quiet-comparison))
       (else (assert #f))))
    ((FLONUM-LESS? FLONUM-IS-LESS?)
     (set-current-branches! (lambda (label)
                              (let ((unordered (generate-label 'UNORDERED)))
                                (LAP (JP (@PCR ,unordered))
                                     (JB (@PCR ,label))
                                     (LABEL ,unordered))))
                            (lambda (label)
                              (LAP (JAE (@PCR ,label))
                                   (JP (@PCR ,label)))))
     (case predicate
       ((FLONUM-LESS?) (signalling-comparison))
       ((FLONUM-IS-LESS?) (quiet-comparison))
       (else (assert #f))))
    ((FLONUM-GREATER? FLONUM-IS-GREATER?)
     (set-current-branches! (lambda (label)
                              (LAP (JA (@PCR ,label))))
                            (lambda (label)
                              (LAP (JBE (@PCR ,label)))))
     (case predicate
       ((FLONUM-GREATER?) (signalling-comparison))
       ((FLONUM-IS-GREATER?) (quiet-comparison))
       (else (assert #f))))
    ((FLONUM-IS-LESS-OR-EQUAL?)
     (set-current-branches! (lambda (label)
                              (let ((unordered (generate-label 'UNORDERED)))
                                (LAP (JP (@PCR ,unordered))
                                     (JBE (@PCR ,label))
                                     (LABEL ,unordered))))
                            (lambda (label)
                              (LAP (JA (@PCR ,label))
                                   (JP (@PCR ,label)))))
     (quiet-comparison))
    ((FLONUM-IS-GREATER-OR-EQUAL?)
     (set-current-branches! (lambda (label)
                              (LAP (JAE (@PCR ,label))))
                            (lambda (label)
                              (LAP (JB (@PCR ,label)))))
     (quiet-comparison))
    ((FLONUM-IS-LESS-OR-GREATER?)
     (set-current-branches! (lambda (label)
                              (LAP (JNE (@PCR ,label))))
                            (lambda (label)
                              (LAP (JE (@PCR ,label)))))
     (quiet-comparison))
    ((FLONUM-IS-UNORDERED?)
     (set-current-branches! (lambda (label)
                              (LAP (JP (@PCR ,label))))
                            (lambda (label)
                              (LAP (JNP (@PCR ,label)))))
     (quiet-comparison))
    (else
     (error "flonum-branch!: Unknown predicate" predicate))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NORMAL? (REGISTER (? source)))
  (set-current-branches! (lambda (label) (LAP (JNZ (@PCR ,label))))
                         (lambda (label) (LAP (JZ (@PCR ,label)))))
  (let* ((source (flonum-source-reference! source))
         (posinf double-flobits:positive-inf)
         (posinf-label (allocate-double-float-bits-label posinf))
         (zero double-flobits:zero)
         (zero-label (allocate-double-float-bits-label zero))
         (tempf (reference-temporary-register! 'FLOAT))
         (temp1 (reference-temporary-register! 'GENERAL))
         (temp2 (reference-temporary-register! 'GENERAL)))
    (LAP
     ;; Set tempf := 0x7ff0..., the exponent mask.
     (MOVF S D ,tempf (@PCR ,posinf-label))
     ;; Set tempf := source & 0x7ff0....
     (ANDF P D ,tempf ,source)
     ;; If source & 0x7ff0... == +inf, then source is either inf or
     ;; NaN, so set the low byte of temp1 to 0; otherwise set it to 1.
     (UCOMIF S D ,tempf (@PCR ,posinf-label))
     (SETNE ,temp1)
     ;; If source & 0x7ff0... == 0, then source is either zero or
     ;; subnormal, so set the low byte of temp2 to 0; otherwise set it
     ;; to 1.
     (UCOMIF S D ,tempf (@PCR ,zero-label))
     (SETNE ,temp2)
     ;; Test temp1 & temp2.
     (TEST B ,temp1 ,temp2))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-FINITE? (REGISTER (? source)))
  (set-current-branches! (lambda (label) (LAP (JNE (@PCR ,label))))
                         (lambda (label) (LAP (JE (@PCR ,label)))))
  (let* ((source (flonum-source-reference! source))
         (mask double-flobits:positive-inf)
         (label (allocate-double-float-bits-label mask))
         (temp (reference-temporary-register! 'FLOAT)))
    (LAP
     ;; Load +inf = 0x7ff0... into temp.
     (MOVF S D ,temp (@PCR ,label))
     ;; Set temp := source & 0x7ff0....
     (ANDF P D ,temp ,source)
     ;; If source was finite, temp will also be finite, and thus will
     ;; compare not-equal to positive infinity.  If source was infinite
     ;; or NaN, it will be equal to positive infinity.
     ;;
     ;; Exceptions: If source was normal, temp will also be normal, and
     ;; if source was subnormal, temp will be zero, so this does not
     ;; spuriously raise the subnormal operand exception.
     (UCOMIF S D ,temp (@PCR ,label)))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-ZERO? (REGISTER (? source)))
  (set-current-branches! (lambda (label) (LAP (JZ (@PCR ,label))))
                         (lambda (label) (LAP (JNZ (@PCR ,label)))))
  ;; Test whether (source & 0x7fff...) == 0x0000....  Work in integer
  ;; registers to avoid invalid operation and subnormal operand
  ;; exceptions in case of signalling NaN or subnormal inputs.
  (let* ((source (flonum-source-reference! source))
         (temp1 (reference-temporary-register! 'GENERAL))
         (temp2 (reference-temporary-register! 'GENERAL)))
    (LAP (MOV Q ,temp1 (&U #x7fffffffffffffff))
         (MOVD Q ,temp2 ,source)      ;source is xmm, can't AND with it
         (TEST Q ,temp1 ,temp2))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-INFINITE? (REGISTER (? source)))
  (set-current-branches! (lambda (label) (LAP (JE (@PCR ,label))))
                         (lambda (label) (LAP (JNE (@PCR ,label)))))
  ;; Test whether (source & 0x7ff0...) == 0x7ff0....  Work in integer
  ;; registers to avoid invalid operation and subnormal operand
  ;; exceptions in case of signalling NaN or subnormal inputs.
  (let* ((source (flonum-source-reference! source))
         (temp1 (reference-temporary-register! 'GENERAL))
         (temp2 (reference-temporary-register! 'GENERAL)))
    (LAP (MOV Q ,temp1 (&U #x7fffffffffffffff))
         (MOVD Q ,temp2 ,source)      ;source is xmm, can't AND with it
         ;; Set temp2 := |source| = source & 0x7fff....
         (AND Q ,temp2 ,temp1)
         ;; Set temp1 := 0x7ff0... = positive infinity.
         (MOV Q ,temp1 (&U #x7ff0000000000000))
         ;; Compute flag bits of (source & 0x7fff...) - 0x7ff0...,
         ;; which is zero iff source is +/-infinity.
         (CMP Q ,temp1 ,temp2))))

(define-rule predicate
  (FLONUM-PRED-1-ARG FLONUM-IS-NAN? (REGISTER (? source)))
  (set-current-branches! (lambda (label) (LAP (JNZ (@PCR ,label))))
                         (lambda (label) (LAP (JZ (@PCR ,label)))))
  ;; Test whether (source & 0x7ff0...) == 0x7ff0... _and_ not all of
  ;; the low 52 bits are zero.  Work in integer registers to avoid
  ;; invalid operation and subnormal operand exceptions in case of
  ;; signalling NaN or subnormal inputs.
  (let* ((source (flonum-source-reference! source))
         (temp1 (reference-temporary-register! 'GENERAL))
         (temp2 (reference-temporary-register! 'GENERAL)))
    (LAP
     ;; Set temp1 := source.
     (MOVD Q ,temp1 ,source)
     ;; Load the trailing significand mask into temp2.
     (MOV Q ,temp2 (&U #x000fffffffffffff))
     ;; Test the trailing significand of source.
     (TEST Q ,temp1 ,temp2)
     ;; Set temp2 := 1 if any bit is one, temp2 := 0 if all bits zero.
     (SETNZ ,temp2)
     ;; Set temp1 := high 16 bits of temp1.
     (SHR Q ,temp1 (&U 48))
     ;; Clear the sign bit and the high nybble of the trailing
     ;; significand.
     (AND Q ,temp1 (&U #x7ff0))
     ;; Check whether what we have is the inf/nan exponent.
     (CMP Q ,temp1 (&U #x7ff0))
     ;; Set temp1 := 1 if yes, temp1 := 0 if no.
     (SETE ,temp1)
     ;; Test temp1 & temp2.
     (TEST B ,temp1 ,temp2))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (CONSTANT (? fp-value))))
  (cond ((not (flo:flonum? fp-value))
         (error "OBJECT->FLOAT: Not a floating-point value" fp-value))
        ((and (flo:= fp-value 0.0)
              ;; XXX Kludgey but expedient test for zero sign.
              (not (flo:negative? (flo:atan2 fp-value -1.))))
         (let ((target (flonum-target-reference! target)))
           (LAP (XORF P D ,target ,target))))
        (else
         (with-float-operand fp-value
           (lambda (operand)
             (LAP (MOVF S D ,(flonum-target-reference! target) ,operand)))))))

(define (with-float-operand fp-value receiver)
  (if (not (flo:flonum? fp-value))
      (error "Invalid constant flonum operand:" fp-value))
  (if compiler:cross-compiling?
      (let ((temp (allocate-temporary-register! 'GENERAL)))
        (LAP ,@(load-constant (register-reference temp) fp-value)
             ,@(object->address (register-reference temp))
             ,@(receiver (INST-EA (@RO ,temp ,flonum-data-offset)))))
      (receiver (INST-EA (@PCR ,(allocate-double-float-label fp-value))))))

(define (allocate-double-float-bits-label bit-string)
  (allocate-data-label bit-string 'DOUBLE-FLOATS 0 8
    (LAP (QUAD U ,(bit-string->unsigned-integer bit-string)))))

(define (allocate-single-float-bits-label bit-string)
  (allocate-data-label bit-string 'SINGLE-FLOATS 0 4
    (LAP (LONG U ,(bit-string->unsigned-integer bit-string)))))

(define (allocate-double-float-label flonum)
  (allocate-double-float-bits-label
   (let ((bit-string (make-bit-string 64 #f)))
     ;; Skip the manifest preceding the flonum data.  Is there a
     ;; better way to express this?
     (let ((flonum-data-offset-in-bits (* 8 (bytes-per-object))))
       (read-bits! flonum flonum-data-offset-in-bits bit-string))
     bit-string)))

(define (float-move-to-target! source target)
  (register-reference (move-to-alias-register! source 'FLOAT target)))