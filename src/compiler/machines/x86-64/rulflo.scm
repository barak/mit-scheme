#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

(define double-flobits:negative-zero
  (let ((bit-string (make-bit-string 64 #f)))
    (bit-string-set! bit-string 63)
    bit-string))

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

;;;; Flonum Predicates

(define double-flobits:zero
  (make-bit-string 64 #f))

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  (flonum-branch!
   predicate
   (flonum-source-reference! source)
   (INST-EA (@PCR ,(allocate-double-float-bits-label double-flobits:zero)))))

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

(define (commute-flonum-predicate predicate)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-ZERO?) 'FLONUM-EQUAL?)
    ((FLONUM-LESS? FLONUM-NEGATIVE?) 'FLONUM-GREATER?)
    ((FLONUM-GREATER? FLONUM-POSITIVE?) 'FLONUM-LESS?)
    (else (error "commute-flonum-predicate: Unknown predicate" predicate))))

(define (flonum-branch! predicate source1 source2)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-ZERO?)
     (set-current-branches! (lambda (label)
                              (let ((unordered (generate-label 'UNORDERED)))
                                (LAP (JP (@PCR ,unordered))
                                     (JE (@PCR ,label))
                                     (LABEL ,unordered))))
                            (lambda (label)
                              (LAP (JNE (@PCR ,label))
                                   (JP (@PCR ,label))))))
    ((FLONUM-LESS? FLONUM-NEGATIVE?)
     (set-current-branches! (lambda (label)
                              (let ((unordered (generate-label 'UNORDERED)))
                                (LAP (JP (@PCR ,unordered))
                                     (JB (@PCR ,label))
                                     (LABEL ,unordered))))
                            (lambda (label)
                              (LAP (JAE (@PCR ,label))
                                   (JP (@PCR ,label))))))
    ((FLONUM-GREATER? FLONUM-POSITIVE?)
     (set-current-branches! (lambda (label)
                              (LAP (JA (@PCR ,label))))
                            (lambda (label)
                              (LAP (JBE (@PCR ,label))))))
    (else
     (error "flonum-branch!: Unknown predicate" predicate)))
  (LAP (UCOMIF S D ,source1 ,source2)))
                                   
(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (CONSTANT (? fp-value))))
  (cond ((not (flo:flonum? fp-value))
         (error "OBJECT->FLOAT: Not a floating-point value" fp-value))
        ((flo:= fp-value 0.0)
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
     (let* ((bytes-per-object (vector-ref (gc-space-status) 0))
            (bits-per-object (* 8 bytes-per-object))
            (flonum-data-offset-in-bits bits-per-object))
       (read-bits! flonum flonum-data-offset-in-bits bit-string))
     bit-string)))
