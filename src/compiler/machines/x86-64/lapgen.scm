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

;;;; RTL Rules utilities for i386 and family.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define available-machine-registers
  ;; rsp holds the the stack pointer
  ;; rbp holds the pointer mask
  ;; rsi holds the register array pointer
  ;; rdi holds the free pointer
  ;++ float
  ;; fr7 is not used so that we can always push on the stack once.
  (list rax rcx rdx rbx r8 r9 r10 r11 r12 r13 r14 r15
	;++ float
	;; fr0 fr1 fr2 fr3 fr4 fr5 fr6
	;; mmx0 mmx1 mmx2 mmx3 mmx4 mmx5 mmx6 mmx7
	;; xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
	;; xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15
	))

(define (sort-machine-registers registers)
  registers)

;++ float

#;
(define (sort-machine-registers registers)
  ;; FR0 is preferable to other FPU regs.  We promote it to the front
  ;; if we find another FPU reg in front of it.
  (let loop ((regs registers))
    (cond ((null? regs) registers)	; no float regs at all
	  ((general-register? (car regs)); ignore general regs
	   (loop (cdr regs)))
	  ((= (car regs) fr0)		; found FR0 first
	   registers)
	  ((memq fr0 regs)		; FR0 not first, is it present?
	   (cons fr0 (delq fr0 registers)) ; move to front
	   registers)
	  (else				; FR0 absent
	   registers))))

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     ;++ float
	     ;; FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT   ;x87 fp
	     ;; FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT   ;MMX 64bit
	     ;; MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA   ;XMM 128bit
	     ;; MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA MEDIA
	     )
	  register))
	((register-value-class=word? register)
	 'GENERAL)
	((register-value-class=float? register)
	 'FLOAT)
	(else
	 (error "Unable to determine register type:" register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (do ((i rax (+ i 1)))
	((> i r15))
      (vector-set! references i (INST-EA (R ,i))))
    ;++ float
    ;; (do ((i fr0 (+ i 1)))
    ;; 	((>= i fr7))
    ;;   (vector-set! references i (INST-EA (ST ,(floreg->sti i)))))
    ;; (do ((i mmx0 (+ i 1)))
    ;; 	((>= i mmx7))
    ;;   (vector-set! references i (INST-EA (MMX ...))))
    ;; (do ((i xmm0 (+ i 1)))
    ;; 	((>= i xmm15))
    ;;   (vector-set! references i (INST-EA (XMM ...))))
    (lambda (register)
      (vector-ref references register))))

(define (register->register-transfer source target)
  (machine->machine-register source target))

(define (reference->register-transfer source target)
  (cond ((equal? (register-reference target) source)
	 (LAP))
	;++ float
	((float-register-reference? source)
	 ;; Assume target is a float register
	 (LAP (FLD ,source)))
	(else
	 (memory->machine-register source target))))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (pseudo-register-offset register)))

(define (home->register-transfer source target)
  (pseudo->machine-register source target))

(define (register->home-transfer source target)
  (machine->pseudo-register source target))

;++ float

(define-integrable (float-register-reference? ea)
  ea
  #f
  #;
  (and (pair? ea)
       (eq? (car ea) 'ST)))

;;;; Linearizer interface

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (JMP (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

(define (make-external-label code label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (WORD U ,code)
       (BLOCK-OFFSET ,label)
       (LABEL ,label)))

(define-integrable (make-code-word min max)
  (+ (* #x100 min) max))

(define expression-code-word
  (make-code-word #xff #xff))

;;;; Utilities for the register allocator interface

(define-integrable (machine->machine-register source target)
  (guarantee-registers-compatible source target)
  ;++ float
  (if (not (float-register? source))
      (LAP (MOV Q ,(register-reference target) ,(register-reference source)))
      (let ((ssti (floreg->sti source))
	    (tsti (floreg->sti target)))
	(if (zero? ssti)
	    (LAP (FST (ST ,tsti)))
	    (LAP (FLD (ST ,ssti))
		 (FSTP (ST ,(1+ tsti))))))))

(define (machine-register->memory source target)
  ;++ float
  (if (not (float-register? source))
      (LAP (MOV Q ,target ,(register-reference source)))
      (let ((ssti (floreg->sti source)))
	(if (zero? ssti)
	    (LAP (FST D ,target))
	    (LAP (FLD (ST ,ssti))
		 (FSTP D ,target))))))

(define (memory->machine-register source target)
  ;++ float
  (if (not (float-register? target))
      (LAP (MOV Q ,(register-reference target) ,source))
      (LAP (FLD D ,source)
	   (FSTP (ST ,(1+ (floreg->sti target)))))))

(define-integrable (offset-referenceable? offset)
  (byte-offset-referenceable? (* address-units-per-object offset)))

(define-integrable (offset-reference register offset)
  (byte-offset-reference register (* address-units-per-object offset)))

(define-integrable (byte-offset-referenceable? offset)
  (fits-in-signed-long? offset))

(define (byte-offset-reference register offset)
  (cond ((zero? offset)
	 (INST-EA (@R ,register)))
	((fits-in-signed-byte? offset)
	 (INST-EA (@RO B ,register ,offset)))
	;; Assume that we are in 32-bit mode or in 64-bit mode, in
	;; which case (@RO W ...) doesn't work.
	;; ((fits-in-signed-word? offset)
	;;  (INST-EA (@RO W ,register ,offset)))
	((fits-in-signed-long? offset)
	 (INST-EA (@RO L ,register ,offset)))
	(else
	 (error "Offset too large:" offset))))

(define-integrable (byte-unsigned-offset-referenceable? offset)
  (byte-offset-referenceable? offset))

(define (byte-unsigned-offset-reference register offset)
  (if (< offset 0)
      (error "Negative unsigned offset:" offset))
  ;; We don't have unsigned addressing modes.
  (byte-offset-reference register offset))

;++ This computation is probably not quite right.

(define-integrable (pseudo-register-offset register)
  (+ (+ (* 16 address-units-per-object) (* 80 address-units-per-object))
     (* 3 (register-renumber register))))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

;++ float

(define (general-register? register)
  register
  #t)

(define (float-register? register)
  register
  #f)

(define (floreg->sti reg)
  (error "x87 floating-point not supported:" `(FLOREG->STI ,reg)))

#|
(define-integrable (floreg->sti reg)
  (- reg fr0))

(define-integrable (general-register? register)
  (< register fr0))

(define-integrable (float-register? register)
  (<= fr0 register fr7))
|#

;;;; Utilities for the rules

(define (require-register! machine-reg)
  (flush-register! machine-reg)
  (need-register! machine-reg))

(define-integrable (flush-register! machine-reg)
  (prefix-instructions! (clear-registers! machine-reg)))

(define (rtl-target:=machine-register! rtl-reg machine-reg)
  (if (machine-register? rtl-reg)
      (begin
	(require-register! machine-reg)
	(if (not (= rtl-reg machine-reg))
	    (suffix-instructions!
	     (register->register-transfer machine-reg rtl-reg))))
      (begin
	(delete-register! rtl-reg)
	(flush-register! machine-reg)
	(add-pseudo-register-alias! rtl-reg machine-reg))))

(define (object->machine-register! object mreg)
  ;; This funny ordering allows load-constant to use a pc value in mreg!
  ;; [TRC 20091025: Does this matter, given PC-relative addressing?]
  (let ((code (load-constant->register (INST-EA (R ,mreg)) object)))
    (require-register! mreg)
    code))

(define (assign-register->register target source)
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define (load-pc-relative target label-expr)
  (LAP (MOV Q ,target (@PCR ,label-expr))))

(define (load-pc-relative-address target label-expr)
  (LAP (LEA Q ,target (@PCR ,label-expr))))  

(define (compare/register*register reg1 reg2)
  (cond ((register-alias reg1 'GENERAL)
	 =>
	 (lambda (alias)
	   (LAP (CMP Q ,(register-reference alias) ,(any-reference reg2)))))
	((register-alias reg2 'GENERAL)
	 =>
	 (lambda (alias)
	   (LAP (CMP Q ,(any-reference reg1) ,(register-reference alias)))))
	(else
	 (LAP (CMP Q ,(source-register-reference reg1)
		   ,(any-reference reg2))))))

(define (compare/reference*non-pointer register non-pointer)
  (compare/reference*literal register (non-pointer->literal non-pointer)))

(define (compare/reference*literal reference literal)
  (if (fits-in-signed-long? literal)
      (LAP (CMP Q ,reference (&U ,literal)))
      (let ((temp (temporary-register-reference)))
	(LAP (MOV Q ,temp (&U ,literal))
	     (CMP Q ,reference ,temp)))))

;;;; Literals and Constants

;;; These are slightly tricky because most instructions don't admit
;;; 64-bit operands.

(define (convert-object/constant->register target object conversion)
  (let ((target (target-register-reference target)))
    (if (non-pointer-object? object)
	;; Is this correct if conversion is object->address ?
	(load-non-pointer-constant->register target object)
	(LAP ,@(load-pointer-constant->register target object)
	     ,@(conversion target)))))

(define (load-constant->register register object)
  (if (non-pointer-object? object)
      (load-non-pointer-constant->register register object)
      (load-pointer-constant->register register object)))

(define (load-pointer-constant->register register object)
  (LAP (MOV Q ,register (@PCR ,(constant->label object)))))

(define (load-non-pointer-constant->register register object)
  (load-non-pointer-literal->register register (non-pointer->literal object)))

(define (load-non-pointer-constant->offset register object)
  (load-non-pointer-literal->offset register (non-pointer->literal object)))

(define (load-non-pointer->register register type datum)
  (load-non-pointer-literal->register register
				      (make-non-pointer-literal type datum)))

(define (load-non-pointer->offset register type datum)
  (load-non-pointer-literal->offset register
				      (make-non-pointer-literal type datum)))

(define (load-non-pointer-literal->register register literal)
  (load-unsigned-immediate->register register literal))

(define (load-non-pointer-literal->offset register literal)
  (load-unsigned-immediate->offset register literal))

(define (non-pointer->literal object)
  (make-non-pointer-literal (object-type object)
			    (careful-object-datum object)))

(define (load-signed-immediate->register target immediate)
  (cond ((zero? immediate)
	 (LAP (XOR Q ,target ,target)))
	((fits-in-signed-quad? immediate)
	 (LAP (MOV Q ,target (& ,immediate))))
	(else
	 (error "Signed immediate too large:" immediate))))

(define (load-unsigned-immediate->register target immediate)
  (cond ((zero? immediate)
	 (LAP (XOR Q ,target ,target)))
	((fits-in-unsigned-quad? immediate)
	 (LAP (MOV Q ,target (&U ,immediate))))
	(else
	 (error "Unsigned immediate too large:" immediate))))

(define (load-signed-immediate->offset offset immediate)
  (if (fits-in-signed-long? immediate)
      (LAP (MOV Q ,(offset->reference! offset) (& ,immediate)))
      (let* ((temporary (temporary-register-reference))
	     (target (offset->reference! offset)))
	(LAP ,@(load-signed-immediate->register temporary immediate)
	     (MOV Q ,target ,temporary)))))

(define (load-unsigned-immediate->offset offset immediate)
  (if (fits-in-unsigned-long? immediate)
      (LAP (MOV Q ,(offset->reference! offset) (&U ,immediate)))
      (let* ((temporary (temporary-register-reference))
	     (target (offset->reference! offset)))
	(LAP ,@(load-unsigned-immediate->register temporary immediate)
	     (MOV Q ,target ,temporary)))))

(define (target-register target)
  (delete-dead-registers!)
  (or (register-alias target 'GENERAL)
      (allocate-alias-register! target 'GENERAL)))  

(define-integrable (target-register-reference target)
  (register-reference (target-register target)))

(define-integrable (temporary-register-reference)
  (reference-temporary-register! 'GENERAL))

(define (source-register source)
   (or (register-alias source 'GENERAL)
       (load-alias-register! source 'GENERAL)))

(define-integrable (source-register-reference source)
  (register-reference (source-register source)))

(define-integrable (any-reference rtl-reg)
  (standard-register-reference rtl-reg 'GENERAL true))

(define (standard-move-to-temporary! source)
  (register-reference (move-to-temporary-register! source 'GENERAL)))

(define (standard-move-to-target! source target)
  (register-reference (move-to-alias-register! source 'GENERAL target)))

(define (indirect-reference! rtl-reg offset)
  (offset-reference (allocate-indirection-register! rtl-reg)
		    offset))

(define (indirect-byte-reference! register offset)
  (byte-offset-reference (allocate-indirection-register! register) offset))

(define-integrable (allocate-indirection-register! register)
  (load-alias-register! register 'GENERAL))

(define (with-indexed-address base* index* scale b-offset protect recvr)
  (let* ((base (allocate-indirection-register! base*))
	 (index (source-register index*))
	 (with-address-temp
	   (lambda (temp)
	     (let ((tref (register-reference temp))
		   (ea (indexed-ea-mode base index scale b-offset)))
	       (LAP (LEA Q ,tref ,ea)
		    ,@(object->address tref)
		    ,@(recvr (INST-EA (@R ,temp)))))))
	 (with-reused-temp
	   (lambda (temp)
	     (need-register! temp)
	     (with-address-temp temp)))	       
	 (fail-index
	  (lambda ()
	    (with-address-temp
	      (allocate-temporary-register! 'GENERAL))))
	 (fail-base
	  (lambda ()
	    (if (and protect (= index* protect))
		(fail-index)
		(reuse-pseudo-register-alias! index*
					      'GENERAL
					      with-reused-temp
					      fail-index)))))
    (if (and protect (= base* protect))
	(fail-base)
	(reuse-pseudo-register-alias! base*
				      'GENERAL
				      with-reused-temp
				      fail-base))))

(define (indexed-ea base index scale offset)
  (indexed-ea-mode (allocate-indirection-register! base)
		   (source-register index)
		   scale
		   offset))

(define (indexed-ea-mode base index scale offset)
  (cond ((zero? offset)
	 (INST-EA (@RI ,base ,index ,scale)))
	((fits-in-signed-byte? offset)
	 (INST-EA (@ROI B ,base ,offset ,index ,scale)))
	((fits-in-signed-long? offset)
	 (INST-EA (@ROI L ,base ,offset ,index ,scale)))
	(else
	 (error "Offset too large:" offset))))

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
		       address-units-per-object
		       (* address-units-per-object
			  (rtl:machine-constant-value offset))))
	  ((rtl:machine-constant? offset)
	   (indirect-reference! (rtl:register-number base)
				(rtl:machine-constant-value offset)))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       address-units-per-object
		       0)))))

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

(define (rtl:detagged-index? base offset)
  (let ((o-ok? (and (rtl:object->datum? offset)
		    (rtl:register? (rtl:object->datum-expression offset)))))
    (if (and (rtl:object->address? base)
	     (rtl:register? (rtl:object->address-expression base)))
	(or o-ok? (rtl:register? offset))
	(and o-ok? (rtl:register? base)))))

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
	(offset (rtl:float-offset-offset offset))
	(objects-per-float
	 (quotient address-units-per-float address-units-per-object)))
    (cond ((not (rtl:register? base))
	   (let ((base*
		  (rtl:register-number (rtl:offset-address-base base)))
		 (w-offset
		  (rtl:machine-constant-value
		   (rtl:offset-address-offset base))))
	     (if (rtl:machine-constant? offset)
		 (indirect-reference!
		  base*
		  (+ (* objects-per-float (rtl:machine-constant-value offset))
		     w-offset))
		 (indexed-ea base*
			     (rtl:register-number offset)
			     address-units-per-float
			     (* address-units-per-object w-offset)))))
	  ((rtl:machine-constant? offset)
	   (indirect-reference! (rtl:register-number base)
				(* objects-per-float
				   (rtl:machine-constant-value offset))))
	  (else
	   (indexed-ea (rtl:register-number base)
		       (rtl:register-number offset)
		       address-units-per-object
		       0)))))

(define (object->type target)
  (LAP (SHR Q ,target (&U ,scheme-datum-width))))

(define (object->datum target)
  (LAP (AND Q ,target (R ,regnum:datum-mask))))

(define (object->address target)
  (declare (integrate-operator object->datum))
  (object->datum target))

(define (interpreter-call-argument? expression)
  (or (rtl:register? expression)
      (and (rtl:cons-pointer? expression)
	   (rtl:machine-constant? (rtl:cons-pointer-type expression))
	   (rtl:machine-constant? (rtl:cons-pointer-datum expression)))
      (rtl:simple-offset? expression)))

(define (interpreter-call-argument->machine-register! expression register)
  (let ((target (register-reference register)))
    (case (car expression)
      ((REGISTER)
       (load-machine-register! (rtl:register-number expression) register))
      ((CONS-POINTER)
       (LAP ,@(clear-registers! register)
	    ,@(load-non-pointer->register
	       target
	       (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	       (rtl:machine-constant-value
		(rtl:cons-pointer-datum expression)))))
      ((OFFSET)
       (let ((source-reference (offset->reference! expression)))
	 (LAP ,@(clear-registers! register)
	      (MOV Q ,target ,source-reference))))
      (else
       (error "Unknown expression type" (car expression))))))

;;;; Named registers, codes, and entries

(define reg:compiled-memtop
  (offset-reference regnum:regs-pointer
		    register-block/memtop-offset))

(define reg:environment
  (offset-reference regnum:regs-pointer
		    register-block/environment-offset))

(define reg:dynamic-link
  (offset-reference regnum:regs-pointer
		    register-block/dynamic-link-offset))

(define reg:lexpr-primitive-arity
  (offset-reference regnum:regs-pointer
		    register-block/lexpr-primitive-arity-offset))

(define reg:utility-arg-4
  (offset-reference regnum:regs-pointer
		    register-block/utility-arg4-offset))

(define reg:stack-guard
  (offset-reference regnum:regs-pointer
		    register-block/stack-guard-offset))


(define-syntax define-codes
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
		'()))))))

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
  quotient remainder modulo)

(define-integrable (invoke-hook entry)
  (LAP (JMP ,entry)))

(define-integrable (invoke-hook/call entry)
  (LAP (CALL ,entry)))

(define-integrable (invoke-interface code)
  (LAP (MOV B (R ,rax) (& ,code))
       ,@(invoke-hook entry:compiler-scheme-to-interface)))

(define-integrable (invoke-interface/call code)
  (LAP (MOV B (R ,rax) (& ,code))
       ,@(invoke-hook/call entry:compiler-scheme-to-interface/call)))

;++ This uses a kludge to number entries by byte offsets from the
;++ registers block, but that works only in the 32-bit i386 version;
;++ for x86-64 version, all the entries' byte indices exceed the range
;++ of signed bytes.  But this works for now.

(define-syntax define-entries
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
	,@(let loop
	      ((names (cdddr form))
	       (index (cadr form))
	       (high (caddr form)))
	    (if (pair? names)
		(if (< index high)
		    (cons `(DEFINE-INTEGRABLE
			     ,(symbol-append 'ENTRY:COMPILER-
					     (car names))
			     (BYTE-OFFSET-REFERENCE REGNUM:REGS-POINTER
						    ,index))
			  (loop (cdr names) (+ index 8) high))
		    (begin
		      (warn "define-entries: Too many for byte offsets.")
		      (loop names index (+ high 32000))))
		'()))))))

(define-entries #x80 #x100		; (* 16 8)
  scheme-to-interface			; Main entry point (only one necessary)
  scheme-to-interface/call		; Used by rules3&4, for convenience.
  trampoline-to-interface		; Used by trampolines, for convenience.
  interrupt-procedure
  interrupt-continuation
  interrupt-closure
  interrupt-dlink
  primitive-apply
  primitive-lexpr-apply
  assignment-trap
  reference-trap
  safe-reference-trap
  link
  error
  primitive-error
  short-primitive-apply)

(define-entries #x-100 0
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
  quotient
  remainder
  modulo
  shortcircuit-apply			; Used by rules3, for speed.
  shortcircuit-apply-size-1		; Small frames, save time and space.
  shortcircuit-apply-size-2
  shortcircuit-apply-size-3
  shortcircuit-apply-size-4
  shortcircuit-apply-size-5
  shortcircuit-apply-size-6
  shortcircuit-apply-size-7
  shortcircuit-apply-size-8
  interrupt-continuation-2
  conditionally-serialize)

;; Operation tables

(define (define-arithmetic-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-arithmetic-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define (pre-lapgen-analysis rgraphs)
  (for-each (lambda (rgraph)
	      (for-each (lambda (edge)
			  (determine-interrupt-checks (edge-right-node edge)))
			(rgraph-entry-edges rgraph)))
	    rgraphs))