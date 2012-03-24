#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; RTL Rules for 68020.  Part 1
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define (reference->register-transfer source target)
  (cond ((or (and (effective-address/data-register? source)
		  (= (lap:ea-operand-1 source) target))
	     (and (effective-address/address-register? source)
		  (= (+ 8 (lap:ea-operand-1 source)) target)))
	 (LAP))
	((effective-address/float-register? source)
	 ;; Assume target is a float register
	 (LAP (FMOVE ,source ,(register-reference target))))
	(else
	 (memory->machine-register source target))))

(define (register->register-transfer source target)
  (machine->machine-register source target))

(define (home->register-transfer source target)
  (pseudo->machine-register source target))

(define (register->home-transfer source target)
  (machine->pseudo-register source target))

(define (pseudo-register-home register)
  (offset-reference regnum:regs-pointer (pseudo-register-offset register)))

(define (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list d0 d1 d2 d3 d4 d5 ;; d6 is now compiled code val
	a0 a1 a2 a3
	fp0 fp1 fp2 fp3 fp4 fp5 fp6 fp7))

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(DATA DATA DATA DATA DATA DATA DATA DATA
	     ADDRESS ADDRESS ADDRESS ADDRESS ADDRESS ADDRESS ADDRESS ADDRESS
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT)
	  register))
	((register-value-class=word? register)
	 (if (register-value-class=address? register)
	     'ADDRESS
	     'DATA))
	((register-value-class=float? register)
	 'FLOAT)
	(else
	 (error "unable to determine register type" register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (let loop ((i 0) (j 8))
      (if (< i 8)
	  (begin
	    (vector-set! references i (INST-EA (D ,i)))
	    (vector-set! references j (INST-EA (A ,i)))
	    (loop (1+ i) (1+ j)))))
    (subvector-move-right! '#(FP0 FP1 FP2 FP3 FP4 FP5 FP6 FP7) 0 8
			   references 16)
    (lambda (register)
      (vector-ref references register))))

(define mask-reference
  (register-reference 7))

;;;; Basic Machine Instructions

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-offset register)
  ;; Offset into register block for temporary registers
  (+ (+ (* 16 4) (* 80 8))
     (* 3 (register-renumber register))))

(define (pseudo-float? register)
  (and (pseudo-register? register)
       (value-class=float? (pseudo-register-value-class register))))

(define (pseudo-word? register)
  (and (pseudo-register? register)
       (value-class=word? (pseudo-register-value-class register))))

(define (machine->machine-register source target)
  (guarantee-registers-compatible source target)
  (if (float-register? source)
      (LAP (FMOVE ,(register-reference source)
		  ,(register-reference target)))
      (LAP (MOV L
		,(register-reference source)
		,(register-reference target)))))

(define (machine-register->memory source target)
  (if (float-register? source)
      (LAP (FMOVE D ,(register-reference source) ,target))
      (LAP (MOV L ,(register-reference source) ,target))))

(define (memory->machine-register source target)
  (if (float-register? target)
      (LAP (FMOVE D ,source ,(register-reference target)))
      (LAP (MOV L ,source ,(register-reference target)))))

(define (offset-reference register offset)
  (byte-offset-reference register (* 4 offset)))

(define (byte-offset-reference register offset)
    (if (zero? offset)
	(if (< register 8)
	    (INST-EA (@D ,register))
	    (INST-EA (@A ,(- register 8))))
	(if (< register 8)
	    (INST-EA (@DO ,register ,offset))
	    (INST-EA (@AO ,(- register 8) ,offset)))))

(define (load-dnl n d)
  (cond ((zero? n)
	 (LAP (CLR L (D ,d))))
	((<= -128 n 127)
	 (LAP (MOVEQ (& ,n) (D ,d))))
	(else
	 (LAP (MOV L (& ,n) (D ,d))))))

(define (load-dnw n d)
  (cond ((zero? n)
	 (LAP (CLR W (D ,d))))
	((<= -128 n 127)
	 (LAP (MOVEQ (& ,n) (D ,d))))
	(else
	 (LAP (MOV W (& ,n) (D ,d))))))

(define (ea+=constant ea c)
  (cond ((zero? c)
	 (LAP))
	((<= 1 c 8)
	 (LAP (ADDQ L (& ,c) ,ea)))
	((>= -1 c -8)
	 (LAP (SUBQ L (& (- 0 ,c)) ,ea)))
	((eq? (lap:ea-keyword ea) 'A)
	 (LAP (LEA (@AO ,(lap:ea-operand-1 ea) ,c) ,ea)))
	((<= -128 c 127)
	 (let ((temp (reference-temporary-register! 'DATA)))
	   (LAP (MOVEQ (& ,c) ,temp)
		(ADD L ,temp ,ea))))
	(else
	 (LAP (ADD L (& ,c) ,ea)))))

(define (increment-machine-register register n)
  (ea+=constant (register-reference register) n))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer-constant constant target)
      (LAP (MOV L
		(@PCR ,(constant->label constant))
		,target))))

(define (load-non-pointer-constant constant target)
  (load-non-pointer (object-type constant)
		    (careful-object-datum constant)
		    target))

(define (load-non-pointer type datum target)
  (load-machine-constant (make-non-pointer-literal type datum) target))

(define (load-machine-constant n target)
  (cond ((and (zero? n)
	      (effective-address/data&alterable? target))
	 (LAP (CLR L ,target)))
	((not (effective-address/data-register? target))
	 (LAP (MOV UL (& ,n) ,target)))
	((<= -128 n 127)
	 (LAP (MOVEQ (& ,n) ,target)))
	(else
	 (find-zero-bits n
	  (lambda (zero-bits datum)
	    (cond ((> datum 127)
		   (LAP (MOV UL (& ,n) ,target)))
		  ((<= zero-bits 16)
		   (LAP (MOVEQ (& ,datum) ,target)
			(LS L L (& ,zero-bits) ,target)))
		  (else
		   ;; This is useful for type-code or-masks.
		   ;; It should be extended to handle and-masks.
		   (LAP (MOVEQ (& ,datum) ,target)
			(RO R L (& ,(- 32 zero-bits)) ,target)))))))))
		  
(define (find-zero-bits n receiver)
  (let loop ((bits 0) (n n))
    (let ((result (integer-divide n 2)))
      (if (zero? (integer-divide-remainder result))
	  (loop (1+ bits)
		(integer-divide-quotient result))
	  (receiver bits n)))))

(define (memory-set-type type target)
  (if (= 8 scheme-type-width)
      (LAP (MOV B (& ,type) ,target))
      (LAP (OR B (& ,(* type-scale-factor type)) ,target))))

(define (test-byte n effective-address)
  ;; This is used to test actual bytes.
  ;; Type codes are "preprocessed" by the pertinent rule.
  (if (and (zero? n) (effective-address/data&alterable? effective-address))
      (LAP (TST B ,effective-address))
      (LAP (CMPI B (& ,n) ,effective-address))))

(define (test-non-pointer-constant constant target)
  (test-non-pointer (object-type constant)
		    (careful-object-datum constant)
		    target))

(define (test-non-pointer type datum effective-address)
  (if (and (zero? type)
	   (zero? datum)
	   (effective-address/data&alterable? effective-address))
      (LAP (TST L ,effective-address))
      (LAP (CMPI UL
		 (& ,(make-non-pointer-literal type datum))
		 ,effective-address))))

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc cc) (@PCR ,label))))))

(define (invert-cc cc)
  (cdr (or (assq cc
		 '((T . F) (F . T)
		   (HI . LS) (LS . HI)
		   (HS . LO) (LO . HS)
		   (CC . CS) (CS . CC)
		   (NE . EQ) (EQ . NE)
		   (VC . VS) (VS . VC)
		   (PL . MI) (MI . PL)
		   (GE . LT) (LT . GE)
		   (GT . LE) (LE . GT)
		   ))
	   (error "INVERT-CC: Not a known CC" cc))))

(define (invert-cc-noncommutative cc)
  ;; Despite the fact that the name of this procedure is similar to
  ;; that of `invert-cc', it is quite different.  `invert-cc' is used
  ;; when the branches of a conditional are being exchanged, while
  ;; this is used when the arguments are being exchanged.
  (cdr (or (assq cc
		 '((HI . LO) (LO . HI)
		   (HS . LS) (LS . HS)
		   (CC . LS) (CS . HI)
		   (PL . MI) (MI . PL)
		   (GE . LE) (LE . GE)
		   (GT . LT) (LT . GT)
		   (T . T) (F . F)
		   (NE . NE) (EQ . EQ)
		   (VC . VC) (VS . VS)
		   ))
	   (error "INVERT-CC-NONCOMMUTATIVE: Not a known CC" cc))))

(define-integrable (cc-commutative? cc)
  (memq cc '(T F NE EQ)))

(define-integrable (effective-address/data&alterable? ea)
  (memq (lap:ea-keyword ea) '(D @D @A @A+ @-A @AO @DO @AOX W L)))

(define-integrable (effective-address/register? ea)
  (memq (lap:ea-keyword ea) '(A D)))

(define-integrable (effective-address/data-register? ea)
  (eq? (lap:ea-keyword ea) 'D))

(define-integrable (effective-address/address-register? ea)
  (eq? (lap:ea-keyword ea) 'A))

(define (effective-address/float-register? ea)
  (memq ea '(FP0 FP1 FP2 FP3 FP4 FP5 FP6 FP7)))

(define (standard-target-reference target)
  ;; Our preference for data registers here is a heuristic that works
  ;; reasonably well since if the value is a pointer, we will probably
  ;; want to dereference it, which requires that we first mask it.
  (delete-dead-registers!)
  (register-reference
   (or (register-alias target 'DATA)
       (register-alias target 'ADDRESS)
       (allocate-alias-register! target 'DATA))))

(define (standard-move-to-target! source type target)
  (register-reference (move-to-alias-register! source type target)))

(define (standard-move-to-temporary! source type)
  (register-reference (move-to-temporary-register! source type)))

(define-integrable (preferred-data-register-reference register)
  (register-reference (preferred-data-register register)))

(define (preferred-data-register register)
  (or (register-alias register 'DATA)
      (register-alias register 'ADDRESS)
      (load-alias-register! register 'DATA)))

(define-integrable (preferred-address-register-reference register)
  (register-reference (preferred-address-register register)))

(define (preferred-address-register register)
  (or (register-alias register 'ADDRESS)
      (register-alias register 'DATA)
      (load-alias-register! register 'ADDRESS)))

(define (rtl:simple-byte-offset? expression)
  (and (rtl:byte-offset? expression)
       (let ((base (rtl:byte-offset-base expression))
	     (offset (rtl:byte-offset-offset expression)))
	 (if (rtl:register? base)
	     (or (rtl:machine-constant? offset)
		 (rtl:register? offset))
	     (and (rtl:byte-offset-address? base)
		  (rtl:machine-constant? offset)
		  (rtl:register? (rtl:byte-offset-address-base base))
		  (rtl:register? (rtl:byte-offset-address-offset base)))))
       expression))

(define (byte-offset->reference! offset)
  ;; OFFSET must be a simple byte offset
  (let ((base (rtl:byte-offset-base offset))
	(offset (rtl:byte-offset-offset offset)))
    (cond ((not (rtl:register? base))
	   (indexed-ea (rtl:register-number
			(rtl:byte-offset-address-base base))
		       (rtl:register-number
			(rtl:byte-offset-address-offset base))
		       1
		       (rtl:machine-constant-value offset)))
	  ((rtl:machine-constant? offset)
	   (indirect-byte-reference! (rtl:register-number base)
				     (rtl:machine-constant-value offset)))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       1
		       0)))))

(define (rtl:simple-offset? expression)
  (and (rtl:offset? expression)
       (let ((base (rtl:offset-base expression))
	     (offset (rtl:offset-offset expression)))
	 (if (rtl:register? base)
	     (or (rtl:machine-constant? offset)
		 (rtl:register? offset))
	     (and (rtl:offset-address? base)
		  (rtl:machine-constant? offset)
		  (rtl:register? (rtl:offset-address-base base))
		  (rtl:register? (rtl:offset-address-offset base)))))
       expression))

(define (offset->reference! offset)
  ;; OFFSET must be a simple offset
  (let ((base (rtl:offset-base offset))
	(offset (rtl:offset-offset offset)))
    (cond ((not (rtl:register? base))
	   (indexed-ea (rtl:register-number (rtl:offset-address-base base))
		       (rtl:register-number (rtl:offset-address-offset base))
		       4
		       (* 4 (rtl:machine-constant-value offset))))
	  ((rtl:machine-constant? offset)
	   (indirect-reference! (rtl:register-number base)
				(rtl:machine-constant-value offset)))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       4
		       0)))))

(define (offset->reference!/char offset)
  ;; OFFSET must be a simple offset
  (let ((base (rtl:offset-base offset))
	(offset (rtl:offset-offset offset)))
    (cond ((not (rtl:register? base))
	   (indexed-ea (rtl:register-number (rtl:offset-address-base base))
		       (rtl:register-number (rtl:offset-address-offset base))
		       4
		       (+ 3 (* 4 (rtl:machine-constant-value offset)))))
	  ((rtl:machine-constant? offset)
	   (indirect-byte-reference!
	    (rtl:register-number base)
	    (+ 3 (* 4 (rtl:machine-constant-value offset)))))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       4
		       3)))))

(define (rtl:simple-float-offset? expression)
  (and (rtl:float-offset? expression)
       (let ((base (rtl:float-offset-base expression))
	     (offset (rtl:float-offset-offset expression)))
	 (and (or (rtl:machine-constant? offset)
		  (rtl:register? offset))
	      (or (rtl:register? base)
		  (and (rtl:offset-address? base)
		       (rtl:register? (rtl:offset-address-base base))
		       (rtl:machine-constant?
			(rtl:offset-address-offset base))))))
       expression))

(define (float-offset->reference! offset)
  ;; OFFSET must be a simple float offset
  (let ((base (rtl:float-offset-base offset))
	(offset (rtl:float-offset-offset offset)))
    (cond ((not (rtl:register? base))
	   (let ((base*
		  (rtl:register-number (rtl:offset-address-base base)))
		 (w-offset
		  (rtl:machine-constant-value
		   (rtl:offset-address-offset base))))
	     (if (rtl:machine-constant? offset)
		 (indirect-reference!
		  base*
		  (+ (* 2 (rtl:machine-constant-value offset))
		     w-offset))
		 (indexed-ea base*
			     (rtl:register-number offset)
			     8
			     (* 4 w-offset)))))
	  ((rtl:machine-constant? offset)
	   (indirect-reference! (rtl:register-number base)
				(* 2 (rtl:machine-constant-value offset))))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       8
		       0)))))

(define (indexed-ea base index scale offset)
  (let ((base (allocate-indirection-register! base))
	(index (preferred-data-register-reference index)))
    (INST-EA (@AOXS ,(->areg base) ,offset (,index L ,scale)))))

(define (indirect-reference! register offset)
  (offset-reference (allocate-indirection-register! register) offset))

(define (indirect-byte-reference! register offset)
  (byte-offset-reference (allocate-indirection-register! register) offset))

(define-integrable (allocate-indirection-register! register)
  (load-alias-register! register 'ADDRESS))

(define (generate-n-times n limit instruction-gen with-counter)
  (if (> n limit)
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,@(load-dnw (-1+ n) counter)
		(LABEL ,loop)
		,@(instruction-gen)
		(DB F (D ,counter) (@PCR ,loop))))))
      (let loop ((n n))
	(if (zero? n)
	    (LAP)
	    (LAP ,@(instruction-gen)
		 ,@(loop (-1+ n)))))))

#|

;;; These seem to be fossils --- GJR 7/1/1993

(define (standard-target-expression? target)
  (or (rtl:simple-offset? target)
      (rtl:free-push? target)
      (rtl:stack-push? target)))

(define (standard-target-expression->ea target)
  (cond ((rtl:offset? target) (offset->reference! target))
	((rtl:free-push? target) (INST-EA (@A+ 5)))
	((rtl:stack-push? target) (INST-EA (@-A 7)))
	(else (error "STANDARD-TARGET->EA: Not a standard target" target))))
|#

(define (rtl:free-push? expression)
  (and (rtl:post-increment? expression)
       (interpreter-free-pointer? (rtl:post-increment-register expression))
       (= 1 (rtl:post-increment-number expression))))

(define (rtl:stack-push? expression)
  (and (rtl:pre-increment? expression)
       (interpreter-stack-pointer? (rtl:pre-increment-register expression))
       (= -1 (rtl:pre-increment-number expression))))

;;;; Machine Targets (actually, arithmetic targets)

(define (reuse-and-load-machine-target! type target source operate-on-target)
  (reuse-machine-target! type target
    (lambda (target)
      (operate-on-target
       (register-reference (move-to-alias-register! source type target))))
    (lambda (target)
      (LAP
       ,@(if (eq? type 'FLOAT)
	     (load-float-register
	      (standard-register-reference source type false)
	      target)
	     (LAP (MOV L
		       ,(standard-register-reference source type true)
		       ,target)))
       ,@(operate-on-target target)))))

(define (reuse-machine-target! type
			       target
			       operate-on-pseudo-target
			       operate-on-machine-target)
  (let ((use-temporary
	 (lambda (target)
	   (let ((temp (reference-temporary-register! type)))
	     (LAP ,@(operate-on-machine-target temp)
		  ,@(if (eq? type 'FLOAT)
			(load-float-register temp target)
			(LAP (MOV L ,temp ,target))))))))
    (case (rtl:expression-type target)
      ((REGISTER)
       (let ((register (rtl:register-number target)))
	 (if (pseudo-register? register)
	     (operate-on-pseudo-target register)
	     (let ((target (register-reference register)))
	       (if (eq? type (register-type register))
		   (operate-on-machine-target target)
		   (use-temporary target))))))
       ((OFFSET)
	(use-temporary (offset->reference! target)))
       (else
	(error "Illegal machine target" target)))))

(define (load-float-register source target)
  (if (effective-address/float-register? source)
      (LAP (FMOVE ,source ,target))
      (LAP (FMOVE D ,source ,target))))

(define (reuse-and-operate-on-machine-target! type target operate-on-target)
  (reuse-machine-target! type target
    (lambda (target)
      (operate-on-target (reference-target-alias! target type)))
    operate-on-target))

(define (machine-operation-target? expression)
  (or (rtl:register? expression)
      (rtl:simple-offset? expression)))

(define (two-arg-register-operation
	 operate commutative?
	 target-type source-reference alternate-source-reference
	 target source1 source2)
  (let ((worst-case
	 (lambda (target source1 source2)
	   (LAP ,@(if (eq? target-type 'FLOAT)
		      (load-float-register source1 target)
		      (LAP (MOV L ,source1 ,target)))
		,@(operate target source2)))))
    (reuse-machine-target! target-type target
      (lambda (target)
	(reuse-pseudo-register-alias source1 target-type
	  (lambda (alias)
	    (let ((source2 (if (= source1 source2)
			       (register-reference alias)
			       (source-reference source2))))
	      (delete-register! alias)
	      (delete-dead-registers!)
	      (add-pseudo-register-alias! target alias)
	      (operate (register-reference alias) source2)))
	  (lambda ()
	    (let ((new-target-alias!
		   (lambda ()
		     (let ((source1 (alternate-source-reference source1))
			   (source2 (source-reference source2)))
		       (delete-dead-registers!)
		       (worst-case (reference-target-alias! target target-type)
				   source1
				   source2)))))
	      (if commutative?
		  (reuse-pseudo-register-alias source2 target-type
		    (lambda (alias2)
		      (let ((source1 (source-reference source1)))
			(delete-register! alias2)
			(delete-dead-registers!)
			(add-pseudo-register-alias! target alias2)
			(operate (register-reference alias2) source1)))
		    new-target-alias!)
		  (new-target-alias!))))))
      (lambda (target)
	(worst-case target
		    (alternate-source-reference source1)
		    (source-reference source2))))))

;;;; Fixnum Operators

(define (signed-fixnum? n)
  (and (exact-integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

(define (unsigned-fixnum? n)
  (and (exact-integer? n)
       (not (negative? n))
       (< n unsigned-fixnum/upper-limit)))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (guarantee-unsigned-fixnum n)
  (if (not (unsigned-fixnum? n)) (error "Not a unsigned fixnum" n))
  n)

(define-integrable fixnum-1
  ;; (expt 2 scheme-type-width) ***
  64)

(define (load-fixnum-constant constant register-reference)
  (LAP (MOV L (& ,(* constant fixnum-1)) ,register-reference)))

(define (object->fixnum reg-ref)
  (LAP (LS L L (& ,scheme-type-width) ,reg-ref)))

(define (address->fixnum reg-ref)
  (LAP (LS L L (& ,scheme-type-width) ,reg-ref)))

(define (fixnum->object reg-ref)
  (LAP (OR B (& ,(ucode-type fixnum)) ,reg-ref)
       (RO R L (& ,scheme-type-width) ,reg-ref)))

(define (fixnum->address reg-ref)
  (LAP (LS R L (& ,scheme-type-width) ,reg-ref)))

(define (test-fixnum effective-address)
  (if (effective-address/data&alterable? effective-address)
      (LAP (TST L ,effective-address))
      (LAP (CMPI L (& 0) ,effective-address))))

(define (fixnum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQ)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LT)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GT)
    (else (error "FIXNUM-PREDICATE->CC: Unknown predicate" predicate))))

(define (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM
		   MULTIPLY-FIXNUM
		   FIXNUM-AND
		   FIXNUM-OR
		   FIXNUM-XOR)))

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

(define-integrable fixnum-bits-mask
  (fix:not scheme-type-mask))

(define (word->fixnum target)
  ;; This renormalizes a fixnum after a bit-wise boolean operation.
  (cond ((= scheme-type-width 8)
	 (LAP (CLR B ,target)))
	((< scheme-type-width 8)
	 (LAP (AND B (& ,fixnum-bits-mask) ,target)))
	(else
	 (LAP (AND L (& ,fixnum-bits-mask) ,target)))))

(define (integer-log-base-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else (loop (* 2 power) (1+ exponent))))))

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (reference)
    (LAP (ADD L (& ,fixnum-1) ,reference))))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (reference)
    (LAP (SUB L (& ,fixnum-1) ,reference))))

(define-fixnum-method 'FIXNUM-NOT fixnum-methods/1-arg
  (lambda (reference)
    (LAP (NOT L ,reference)
	 ,@(word->fixnum reference))))

(let-syntax
    ((binary-fixnum
      (sc-macro-transformer
       (lambda (form environment)
	 environment
	 `(BEGIN
	    (DEFINE-FIXNUM-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS
	      (LAMBDA (TARGET SOURCE)
		(LAP (,(caddr form) L ,',SOURCE ,',TARGET))))
	    (DEFINE-FIXNUM-METHOD ',(cadr form) FIXNUM-METHODS/2-ARGS-CONSTANT
	      (LAMBDA (TARGET N)
		(IF (,(cadddr form) N)
		    (LAP)
		    (LAP (,(caddr form) L
					(& ,',(* N FIXNUM-1))
					,',TARGET))))))))))

  (binary-fixnum PLUS-FIXNUM ADD zero?)
  (binary-fixnum FIXNUM-OR OR zero?)
  (binary-fixnum FIXNUM-AND AND
		 (lambda (n)
		   (declare (integrate n))
		   (fix:= n -1))))

;; XOR is weird because the first operand for an EOR instruction
;; must be a D register!

(define-fixnum-method 'FIXNUM-XOR fixnum-methods/2-args
  (lambda (target source)
    (if (effective-address/data-register? source)
	(LAP (EOR L ,source ,target))
	(let ((temp (reference-temporary-register! 'DATA)))
	  (LAP (MOV L ,source ,temp)
	       (EOR L ,temp ,target))))))

(define-fixnum-method 'FIXNUM-XOR fixnum-methods/2-args-constant
  (lambda (target n)
    (if (zero? n)
	(LAP)
	(LAP (EOR L (& ,(* n fixnum-1)) ,target)))))

;; Multiply is hairy, since numbers are shifted by the type code width.
;; Rather than unshift, multiply, and shift, we unshift one and then
;; multiply, but we have to be careful if the source is the same
;; as the destination.

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source)
    (cond ((not (equal? target source))
	   (LAP
	    (AS R L (& ,scheme-type-width) ,target)
	    (MUL S L ,source ,target)))
	  ((even? scheme-type-width)
	    (LAP
	     (AS R L (& ,(quotient scheme-type-width 2)) ,target)
	     (MUL S L ,source ,target)))
	  (else
	    #|
	    ;; This is no good because the MUL instruction is
	    ;; not last, and thus the overflow condition is
	    ;; not set appropriately.
	    (LAP
	     (AS R L (& ,scheme-type-width) ,target)
	     (MUL S L ,source ,target)
	     (AS L L (& ,scheme-type-width) ,target))
	    |#
	    (let ((temp (reference-temporary-register! 'DATA)))
	      (LAP
	       (MOV L ,source ,temp)
	       (AS R L (& ,scheme-type-width) ,target)
	       (MUL S L ,temp ,target)))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n) (LAP (CLR L ,target)))
	  ((= n 1) (LAP))
	  ((= n -1) (LAP (NEG L ,target)))
	  (else
	   (let ((power-of-2 (integer-log-base-2? n)))
	     (cond ((not power-of-2)
		    (LAP (MUL S L (& ,n) ,target)))
		   ((> power-of-2 8)
		    (let ((temp (reference-temporary-register! 'DATA)))
		      (LAP (MOV L (& ,power-of-2) ,temp)
			   (AS L L ,temp ,target))))
		   (else
		    (LAP (AS L L (& ,power-of-2) ,target)))))))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (target source)
    (LAP (SUB L ,source ,target))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (if (zero? n)
	(LAP)
	(LAP (SUB L (& ,(* n fixnum-1)) ,target)))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args
  (lambda (target source)
    (let ((temp (reference-temporary-register! 'DATA)))
      (LAP (MOV L ,source ,temp)
	   (NOT L ,temp)
	   (AND L ,temp ,target)))))

(define-fixnum-method 'FIXNUM-ANDC fixnum-methods/2-args-constant
  (lambda (target n)
    (if (zero? n)
	(LAP)
	(LAP (AND L (& ,(* (fix:not n) fixnum-1)) ,target)))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args
  (lambda (target source)
    (let ((temp (reference-temporary-register! 'DATA))
	  (merge (generate-label 'LSH-MERGE))
	  (nonneg (generate-label 'LSH-NONNEG)))
      (LAP (MOV L ,source ,temp)
	   (AS R L (& ,scheme-type-width) ,temp)
	   (B GE (@PCR ,nonneg))
	   (NEG L ,temp)
	   (LS R L ,temp ,target)
	   ,@(word->fixnum target)
	   (BRA (@PCR ,merge))
	   (LABEL ,nonneg)
	   (LS L L ,temp ,target)
	   (LABEL ,merge)))))

(define-fixnum-method 'FIXNUM-LSH fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n)
	   (LAP))
	  ((negative? n)
	   (let ((m (- 0 n)))
	     (if (< m 9)
		 (LAP (LS R L (& ,m) ,target)
		      ,@(word->fixnum target))
		 (let ((temp (reference-temporary-register! 'DATA)))
		   (LAP ,@(load-dnl m temp)
			(LS R L ,temp ,target)
			,@(word->fixnum target))))))		 
	  (else
	   (if (< n 9)
	       (LAP (LS L L (& ,n) ,target))
	       (let ((temp (reference-temporary-register! 'DATA)))
		 (LAP ,@(load-dnl n temp)
		      (LS L L ,temp ,target))))))))

;;; Quotient is weird because it must shift left the quotient,
;;; to normalize it as a fixnum, and because arithmetic shifting
;;; does not really do the right thing.

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args
  (lambda (target source)
    (LAP
     (DIV S L ,source ,target)
     (AS L L (& ,scheme-type-width) ,target))))

(define-fixnum-method 'FIXNUM-QUOTIENT fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((= n 1) (LAP))
	  ((= n -1) (LAP (NEG L ,target)))
	  ((integer-log-base-2? n)
	   =>
	   (lambda (power-of-2)
	     (let ((label (generate-label 'QUO-SHIFT)))
	       (LAP (TST L ,target)
		    (B GE (@PCR ,label))
		    (ADD L (& ,(* (-1+ n) fixnum-1)) ,target)
		    (LABEL ,label)
		    ,@(if (<= power-of-2 8)
			  (LAP (AS R L (& ,power-of-2) ,target))
			  (let ((temp (reference-temporary-register! 'DATA)))
			    (LAP (MOV L (& ,power-of-2) ,temp)
				 (AS R L ,temp ,target))))
		    ,@(word->fixnum target)))))
	  (else
	   ;; This includes negative n
	   (LAP (DIV S L (& ,(* n fixnum-1)) ,target)
		(AS L L (& ,scheme-type-width) ,target))))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args
  (lambda (target source)
    (let ((temp (reference-temporary-register! 'DATA)))
      (LAP (DIVL S L ,source ,temp ,target)
	   (MOV L ,temp ,target)))))

(define-fixnum-method 'FIXNUM-REMAINDER fixnum-methods/2-args-constant
  (lambda (target n)
    ;; (remainder x y) is 0 or has the sign of x.
    ;; Thus we can always "divide" by (abs y) to make things simpler.
    (let ((n (abs n)))
      (if (= n 1)
	  (LAP (CLR L ,target))
	  (let ((xpt (integer-log-base-2? n)))
	    (if (or (not xpt) (not use-68020-instructions?))
		(let ((temp (reference-temporary-register! 'DATA)))
		  (LAP (DIVL S L (& ,(* n fixnum-1)) ,temp ,target)
		       (MOV L ,temp ,target)))
		(let ((sign (reference-temporary-register! 'DATA))
		      (label (generate-label 'REM-MERGE))
		      (shift (- scheme-datum-width xpt))
		      (nbits (+ scheme-type-width xpt)))
		  #|
		  (LAP (CLR L ,sign)
		       (BFTST ,target (& ,shift) (& ,xpt))
		       (B EQ (@PCR ,label))
		       (BFEXTS ,target (& 0) (& 1) ,sign)
		       (LABEL ,label)
		       (BFINS ,target (& 0) (& ,shift) ,sign))
		  |#
		  ;; This may produce a branch to a branch, but a
		  ;; peephole optimizer should be able to fix this.
		  (LAP (BFEXTS ,target (& 0) (& 1) ,sign)
		       (BFEXTU ,target (& ,(- 32 nbits)) (& ,nbits) ,target)
		       (B EQ (@PCR ,label))
		       (BFINS ,target (& 0) (& ,shift) ,sign)
		       (LABEL ,label)))))))))

;;;; Flonum Operators

(define (define-flonum-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-flonum-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define-integrable (flonum-1-arg/operate operator)
  (lookup-flonum-method operator flonum-methods/1-arg))

;;; Notice the weird ,', syntax here.
;;; If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-FLONUM-METHOD ',(cadr form) FLONUM-METHODS/1-ARG
	     (LAMBDA (SOURCE TARGET)
	       (IF (EFFECTIVE-ADDRESS/FLOAT-REGISTER? SOURCE)
		   (LAP (,(caddr form) ,',SOURCE ,',TARGET))
		   (LAP (,(caddr form) D ,',SOURCE ,',TARGET)))))))))
  (define-flonum-operation flonum-negate fneg)
  (define-flonum-operation flonum-abs fabs)
  (define-flonum-operation flonum-sin fsin)
  (define-flonum-operation flonum-cos fcos)
  (define-flonum-operation flonum-tan ftan)
  (define-flonum-operation flonum-asin fasin)
  (define-flonum-operation flonum-acos facos)
  (define-flonum-operation flonum-atan fatan)
  (define-flonum-operation flonum-exp fetox)
  (define-flonum-operation flonum-log flogn)
  (define-flonum-operation flonum-sqrt fsqrt)
  (define-flonum-operation flonum-round fint)
  (define-flonum-operation flonum-truncate fintrz))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define-integrable (flonum-2-args/operate operator)
  (lookup-flonum-method operator flonum-methods/2-args))

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-FLONUM-METHOD ',(cadr form) FLONUM-METHODS/2-ARGS
	     (LAMBDA (TARGET SOURCE)
	       (IF (EFFECTIVE-ADDRESS/FLOAT-REGISTER? SOURCE)
		   (LAP (,(caddr form) ,',SOURCE ,',TARGET))
		   (LAP (,(caddr form) D ,',SOURCE ,',TARGET)))))))))
  (define-flonum-operation flonum-add fadd)
  (define-flonum-operation flonum-subtract fsub)
  (define-flonum-operation flonum-multiply fmul)
  (define-flonum-operation flonum-divide fdiv))

(define (invert-float-cc cc)
  (cdr (or (assq cc
		'((EQ . NE) (NE . EQ)
		  (GT . NGT) (NGT . GT)
		  (GE . NGE) (NGE . GE)
		  (LT . NLT) (NLT . LT)
		  (LE . NLE) (NLE . LE)
		  (GL . NGL) (NGL . GL)
		  (MI . PL) (PL . MI)))
	   (error "INVERT-FLOAT-CC: Not a known CC" cc))))

(define (set-flonum-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (FB ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (FB ,(invert-float-cc cc) (@PCR ,label))))))

(define (flonum-predicate->cc predicate)
  (case predicate
    ((FLONUM-EQUAL? FLONUM-ZERO?) 'EQ)
    ((FLONUM-LESS? FLONUM-NEGATIVE?) 'LT)
    ((FLONUM-GREATER? FLONUM-POSITIVE?) 'GT)
    (else (error "FLONUM-PREDICATE->CC: Unknown predicate" predicate))))

(define (flonum-2-args/commutative? operator)
  (memq operator '(FLONUM-ADD FLONUM-MULTIPLY)))

;;;; OBJECT->DATUM rules - Mhwu
;;;  Similar to fixnum rules, but no sign extension

#|

;; *** This is believed to be a fossil. ***
;; Left here until the first compilation to make sure that it really is.
;; Can be removed the next time it is seen.

(define (load-constant-datum constant register-ref)
  (if (non-pointer-object? constant)
      (LAP (MOV L (& ,(careful-object-datum constant)) ,register-ref))
      (LAP (MOV L
		(@PCR ,(constant->label constant))
		,register-ref)
	   ,@(object->address register-ref))))

|#

(define (object->address register-reference)
  (LAP (AND L ,mask-reference ,register-reference)))

(define (object->datum register-reference)
  (LAP (AND L ,mask-reference ,register-reference)))

(define-integrable scheme-type-mask
  ;; (-1+ (expt 2 scheme-type-width)) ***
  #x3f)

(define-integrable use-68020-instructions? true)

(define (object->type source target)
  ;; `Source' must be a data register or non-volatile memory reference.
  ;; `Target' must be a data register reference.
  ;; Guarantees that the condition codes are set for a zero-compare.
  (cond (use-68020-instructions?
	 (LAP (BFEXTU ,source (& 0) (& ,scheme-type-width) ,target)))
	((memq (lap:ea-keyword source) '(@D @A @AO @DO @AOX W L))
	 (LAP (CLR L ,target)
	      (MOVE B ,source ,target)
	      ,@(if (= scheme-type-width 8)
		    (LAP)
		    (LAP (LS R B (& ,(- 8 scheme-type-width)) ,target)))))
	(else
	 (LAP ,@(if (equal? source target)
		    (LAP)
		    (LAP (MOVE L ,source ,target)))
	      (RO L L (& ,scheme-type-width) ,target)
	      (AND L (& ,scheme-type-mask) ,target)))))

;;;; CHAR->ASCII rules

(define (coerce->any/byte-reference register)
  #|
  ;; This does not guarantee that the data is in a
  ;; D register, and A registers are no good.
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (indirect-char/ascii-reference!
	     regnum:regs-pointer
	     (pseudo-register-offset register)))))
  |#
  (let ((alias (register-alias register 'DATA)))
    (cond (alias
	   (register-reference alias))
	  ((register-alias register false)
	   (reference-alias-register! register 'DATA))
	  (else
	   ;; Must be in home.
	   (indirect-char/ascii-reference!
	    regnum:regs-pointer
	    (pseudo-register-offset register))))))

(define (indirect-char/ascii-reference! register offset)
  (indirect-byte-reference! register (+ (* offset 4) 3)))

(define (char->signed-8-bit-immediate character)
  (let ((ascii (char->ascii character)))
    (if (< ascii 128) ascii (- ascii 256))))

;;;; Registers/Entries

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

(define (float-register? register)
  (and (< register 24)
       (>= register 16)))

(define-integrable (lap:ea-keyword expression)
  (car expression))

(define-integrable (lap:ea-operand-1 expression)
  (cadr expression))

(define-integrable (lap:ea-operand-2 expression)
  (caddr expression))

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (BRA (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

(define-integrable reg:compiled-memtop (INST-EA (@A 6)))
(define-integrable reg:environment (INST-EA (@AO 6 #x000C)))
(define-integrable reg:lexpr-primitive-arity (INST-EA (@AO 6 #x001C)))
(define-integrable reg:closure-free (INST-EA (@AO 6 #x0024)))
(define-integrable reg:closure-space (INST-EA (@AO 6 #X0028)))
(define-integrable reg:stack-guard (INST-EA (@AO 6 #X002C)))

(let-syntax ((define-codes
	       (sc-macro-transformer
		(lambda (form environment)
		  environment
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (index (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'CODE:COMPILER-
						      (car names))
				      ,index)
				   (loop (cdr names) (+ index 1)))
			     '())))))))
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply primitive-error
    quotient remainder modulo))

(let-syntax ((define-entries
	       (sc-macro-transformer
		(lambda (form environment)
		  environment
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (index (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'ENTRY:COMPILER-
						      (car names))
				      (INST-EA (@AO 6 ,index)))
				   (loop (cdr names) (+ index 8)))
			     '())))))))
  (define-entries #x40
    scheme-to-interface			; Main entry point (only one necessary)
    scheme-to-interface-jsr		; Used by rules3&4, for convenience.
    trampoline-to-interface		; Used by trampolines, for convenience.
    shortcircuit-apply			; Used by rules3, for speed.
    shortcircuit-apply-size-1		; Small frames, save time and space.
    shortcircuit-apply-size-2
    shortcircuit-apply-size-3
    shortcircuit-apply-size-4
    shortcircuit-apply-size-5
    shortcircuit-apply-size-6
    shortcircuit-apply-size-7
    shortcircuit-apply-size-8
    primitive-apply			; Common entries to save space.
    primitive-lexpr-apply
    error
    link
    interrupt-closure
    interrupt-dlink
    interrupt-procedure 
    interrupt-continuation
    assignment-trap
    reference-trap
    safe-reference-trap
    &+
    &-
    &*
    &/
    &=
    &<
    &>
    1+
    -1+
    zero?
    positive?
    negative?
    primitive-error
    allocate-closure		; This doesn't have a code: counterpart.
    closure-hook		; This doesn't have a code: counterpart.
    quotient
    remainder
    modulo
    stack-and-interrupt-check-12 ; This doesn't have a code: counterpart.
    stack-and-interrupt-check-14 ; This doesn't have a code: counterpart.
    stack-and-interrupt-check-18 ; This doesn't have a code: counterpart.
    stack-and-interrupt-check-22 ; This doesn't have a code: counterpart.
    stack-and-interrupt-check-24 ; This doesn't have a code: counterpart.
    set-interrupt-enables	; This doesn't have a code: counterpart.
    ))

(define-integrable (invoke-interface code)
  (LAP (MOVEQ (& ,code) (D 0))
       (JMP ,entry:compiler-scheme-to-interface)))

;; If the entry point scheme-to-interface-jsr were not available,
;; this code should replace the definition below.
;; The others can be handled similarly.
#|
(define-integrable (invoke-interface-jsr code)
  (LAP (MOVEQ (& ,code) (D 0))
       (LEA (@PCO 12) (A 0))
       (MOV L (A 0) (D 1))
       (JMP ,entry:compiler-scheme-to-interface)))
|#

(define-integrable (invoke-interface-jsr code)
  (LAP (MOVEQ (& ,code) (D 0))
       (JSR ,entry:compiler-scheme-to-interface-jsr)))


(define (pre-lapgen-analysis rgraphs)
  rgraphs
  unspecific)