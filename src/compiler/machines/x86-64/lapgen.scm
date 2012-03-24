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

;;;; RTL Rules utilities for AMD x86-64 and family.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define available-machine-registers
  ;; rsp holds the the stack pointer
  ;; rbp holds the pointer mask
  ;; rsi holds the register array pointer
  ;; rdi holds the free pointer
  (list rax rcx rdx rbx r8 r9 r10 r11 r12 r13 r14 r15
	xmm0 xmm1 xmm2 xmm3 xmm4 xmm5 xmm6 xmm7
	xmm8 xmm9 xmm10 xmm11 xmm12 xmm13 xmm14 xmm15))

(define (sort-machine-registers registers)
  registers)

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT)
	  register))
	((register-value-class=word? register)
	 'GENERAL)
	((register-value-class=float? register)
	 'FLOAT)
	(else
	 (error "Unable to determine register type:" register))))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (do ((r rax (+ r 1))
	 (i 0 (+ i 1)))
	((> r r15))
      (vector-set! references r (INST-EA (R ,i))))
    (do ((r xmm0 (+ r 1))
	 (i 0 (+ i 1)))
	((> r xmm15))
      (vector-set! references r (INST-EA (XMM ,i))))
    (lambda (register)
      (vector-ref references register))))

(define (register->register-transfer source target)
  (machine->machine-register source target))

(define (reference->register-transfer source target)
  (cond ((equal? (register-reference target) source)
	 (LAP))
	((float-register-reference? source)
	 ;; Assume target is a float register
	 (LAP (MOVF S D ,(register-reference target) ,source)))
	(else
	 (memory->machine-register source target))))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (pseudo-register-offset register)))

(define (home->register-transfer source target)
  (pseudo->machine-register source target))

(define (register->home-transfer source target)
  (machine->pseudo-register source target))

(define-integrable (float-register-reference? ea)
  (and (pair? ea)
       (eq? (car ea) 'XMM)))

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

(define (generate-move register source-ref target-ref)
  (if (float-register? register)
      (LAP (MOVF S D ,target-ref ,source-ref))
      (LAP (MOV Q ,target-ref ,source-ref))))

(define (machine->machine-register source target)
  (guarantee-registers-compatible source target)
  (generate-move source
		 (register-reference source)
		 (register-reference target)))

(define (machine-register->memory source target)
  (generate-move source (register-reference source) target))

(define (memory->machine-register source target)
  (generate-move target source (register-reference target)))

(define-integrable (offset-referenceable? offset)
  (byte-offset-referenceable? (* address-units-per-object offset)))

(define-integrable (offset-reference register offset)
  (byte-offset-reference register (* address-units-per-object offset)))

(define-integrable (byte-offset-referenceable? offset)
  (fits-in-signed-long? offset))

(define (byte-offset-reference register offset)
  (if (zero? offset)
      (INST-EA (@R ,register))
      (INST-EA (@RO ,register ,offset))))

(define-integrable (byte-unsigned-offset-referenceable? offset)
  (byte-offset-referenceable? offset))

(define (byte-unsigned-offset-reference register offset)
  (if (< offset 0)
      (error "Negative unsigned offset:" offset))
  ;; We don't have unsigned addressing modes.
  (byte-offset-reference register offset))

;;; This returns an offset in objects, not bytes.

(define-integrable (pseudo-register-offset register)
  (+ (+ 16 80)				;Sixteen fixed, eighty hooks.
     (register-renumber register)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (general-register? register)
  (< register xmm0))

(define-integrable (float-register? register)
  (>= register xmm0))

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

;;; OBJECT->MACHINE-REGISTER! takes only general registers, not float
;;; registers.  Otherwise, (INST-EA (R ,mreg)) would need to be
;;; (register-reference mreg).

(define (object->machine-register! object mreg)
  ;; This ordering allows LOAD-CONSTANT to use MREG as a temporary.
  (let ((code (load-constant (INST-EA (R ,mreg)) object)))
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
  (with-unsigned-immediate-operand literal
    (lambda (operand)
      (LAP (CMP Q ,reference ,operand)))))

;;;; Literals and Constants

;;; These are slightly tricky because most instructions don't admit
;;; 64-bit operands.

(define (load-converted-constant target object conversion)
  (let ((target (target-register-reference target)))
    (if (non-pointer-object? object)
	;; Assumption: CONVERSION fetches the datum of the object,
	;; which is the same as the address of the object.
	(load-non-pointer target 0 (careful-object-datum object))
	(LAP ,@(load-pointer-constant target object)
	     ,@(conversion target)))))

(define (load-constant register object)
  (if (non-pointer-object? object)
      (load-non-pointer-constant register object)
      (load-pointer-constant register object)))

(define (load-pointer-constant register object)
  (LAP (MOV Q ,register (@PCR ,(constant->label object)))))

(define (load-non-pointer-constant register object)
  (load-non-pointer-literal register (non-pointer->literal object)))

(define (load-non-pointer register type datum)
  (load-non-pointer-literal register (make-non-pointer-literal type datum)))

(define (load-non-pointer-literal register literal)
  (load-unsigned-immediate register literal))

(define (store-non-pointer-constant register object)
  (store-non-pointer-literal register (non-pointer->literal object)))

(define (store-non-pointer offset type datum)
  (store-non-pointer-literal offset (make-non-pointer-literal type datum)))

(define (store-non-pointer-literal offset literal)
  (store-unsigned-immediate offset literal))

(define (non-pointer->literal object)
  (make-non-pointer-literal (object-type object)
			    (careful-object-datum object)))

(define (load-signed-immediate target value)
  (cond ((zero? value)
	 (LAP (XOR Q ,target ,target)))
	((fits-in-signed-quad? value)
	 (LAP (MOV Q ,target (& ,value))))
	(else
	 (error "Signed immediate too large:" value))))

(define (load-unsigned-immediate target value)
  (cond ((zero? value)
	 (LAP (XOR Q ,target ,target)))
	((fits-in-unsigned-quad? value)
	 (LAP (MOV Q ,target (&U ,value))))
	(else
	 (error "Unsigned immediate too large:" value))))

(define (store-signed-immediate offset value)
  (with-signed-immediate-operand value
    (lambda (operand)
      (LAP (MOV Q ,(offset->reference! offset) ,operand)))))

(define (store-unsigned-immediate offset value)
  (with-unsigned-immediate-operand value
    (lambda (operand)
      (LAP (MOV Q ,(offset->reference! offset) ,operand)))))

(define (with-signed-immediate-operand value receiver)
  (receive (temp prefix operand)
      (signed-immediate-operand value temporary-register-reference)
    temp				;ignore
    (LAP ,@prefix
	 ,@(receiver operand))))

(define (with-unsigned-immediate-operand value receiver)
  (receive (temp prefix operand)
      (unsigned-immediate-operand value temporary-register-reference)
    temp				;ignore
    (LAP ,@prefix
	 ,@(receiver operand))))

;;; SIGNED-IMMEDIATE-OPERAND and UNSIGNED-IMMEDIATE-OPERAND abstract
;;; the pattern of performing an operation with an instruction that
;;; takes an immediate operand of 32 bits, but using a value that may
;;; exceed 32 bits and thus may require a temporary register (possibly
;;; reused from something else).  Some instructions take immediates
;;; differently, and cannot use this; e.g., IMUL.  These return the
;;; temporary register reference if a temporary was necessary, an
;;; instruction prefix to load the value into the temporary register,
;;; and the operand to pass to the desired instruction, either a
;;; 32-bit immediate operand or a register reference.  Except where
;;; reusing the temporary register is useful, it is generally enough
;;; to use WITH-(UN)SIGNED-IMMEDIATE-OPERAND above.

(define (signed-immediate-operand value temporary-reference)
  (let ((operand (INST-EA (& ,value))))
    (cond ((fits-in-signed-long? value)
	   (values #f (LAP) operand))
	  ((fits-in-signed-quad? value)
	   ;; (values #f
	   ;;         (LAP)
	   ;;         (INST-EA (@PCR ,(allocate-signed-quad-label value))))
	   (let ((temp (temporary-reference)))
	     (values temp (LAP (MOV Q ,temp ,operand)) temp)))
	  (else
	   (error "Signed immediate value too large:" value)))))

(define (unsigned-immediate-operand value temporary-reference)
  (let ((operand (INST-EA (&U ,value))))
    (cond ((fits-in-unsigned-long? value)
	   (values #f (LAP) operand))
	  ((fits-in-unsigned-quad? value)
	   ;; (values #f
	   ;;         (LAP)
	   ;;         (INST-EA (@PCR ,(allocate-unsigned-quad-label value))))
	   (let ((temp (temporary-reference)))
	     (values temp (LAP (MOV Q ,temp ,operand)) temp)))
	  (else
	   (error "Unsigned immediate value too large:" value)))))

(define (allocate-data-label datum block-name offset alignment data)
  (let* ((block
	  (or (find-extra-code-block block-name)
	      (let ((block
		     (declare-extra-code-block! block-name 'ANYWHERE '())))
		(add-extra-code!
		 block
		 (LAP (PADDING ,offset ,alignment ,padding-string)))
		block)))
	 (pairs (extra-code-block/xtra block))
	 (place (assoc datum pairs)))
    (if place
	(cdr place)
	(let ((label (generate-label block-name)))
	  (set-extra-code-block/xtra!
	   block
	   (cons (cons datum label) pairs))
	  (add-extra-code! block (LAP (LABEL ,label) ,@data))
	  label))))

(define (allocate-unsigned-quad-label quad)
  (allocate-data-label quad 'QUADS 0 8 (LAP (QUAD U ,quad))))

(define (allocate-signed-quad-label quad)
  (allocate-data-label quad 'QUADS 0 8 (LAP (QUAD S ,quad))))

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

(define (binary-register-operation operate commutative? type move
				   target source1 source2)
  (let* ((worst-case
	  (lambda (target source1 source2)
	    (LAP ,@(move target source1)
		 ,@(operate target source2))))
	 (new-target-alias!
	  (lambda ()
	    (let ((source1 (standard-register-reference source1 type #f))
		  (source2 (standard-register-reference source2 type #f)))
	      (delete-dead-registers!)
	      (worst-case
	       (register-reference
		(or (register-alias target type)
		    (allocate-alias-register! target type)))
	       source1
	       source2)))))
    (cond ((not (pseudo-register? target))
	   (if (not (eq? (register-type target) type))
	       (error "binary-register-operation: Wrong type register"
		      target
		      type)
	       (worst-case (register-reference target)
			   (standard-register-reference source1 type #f)
			   (standard-register-reference source2 type #f))))
	  ((register-copy-if-available source1 type target)
	   => (lambda (get-alias-ref)
		(if (= source2 source1)
		    (let ((ref (get-alias-ref)))
		      (operate ref ref))
		    (let ((source2
			   (standard-register-reference source2 type #f)))
		      (operate (get-alias-ref) source2)))))
	  ((not commutative?)
	   (new-target-alias!))
	  ((register-copy-if-available source2 type target)
	   => (lambda (get-alias-ref)
		(let ((source1 (standard-register-reference source1 type #f)))
		  (operate (get-alias-ref) source1))))
	  (else
	   (new-target-alias!)))))

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
  (if (zero? offset)
      (INST-EA (@RI ,base ,index ,scale))
      (INST-EA (@ROI ,base ,offset ,index ,scale))))

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
	 (if (rtl:register? base)
	     (or (rtl:machine-constant? offset)
		 (rtl:register? offset))
	     (and (rtl:float-offset-address? base)
		  (rtl:machine-constant? offset)
		  (rtl:register? (rtl:float-offset-address-base base))
		  (rtl:register? (rtl:float-offset-address-offset base)))))
       expression))

(define (float-offset->reference! offset)
  ;; OFFSET must be a simple float offset
  (let ((base (rtl:float-offset-base offset))
	(offset (rtl:float-offset-offset offset))
	(objects-per-float
	 (quotient address-units-per-float address-units-per-object)))
    (cond ((not (rtl:register? base))
	   (indexed-ea
	    (rtl:register-number (rtl:float-offset-address-base base))
	    (rtl:register-number (rtl:float-offset-address-offset base))
	    address-units-per-float
	    (* address-units-per-float (rtl:machine-constant-value offset))))
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
	    ,@(load-non-pointer
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

(define reg:lexpr-primitive-arity
  (offset-reference regnum:regs-pointer
		    register-block/lexpr-primitive-arity-offset))

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

(define-syntax define-entries
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
	,@(let loop ((names (cddr form)) (index (cadr form)))
	    (if (pair? names)
		(cons `(DEFINE-INTEGRABLE
			   ,(symbol-append 'ENTRY:COMPILER- (car names))
			 (BYTE-OFFSET-REFERENCE REGNUM:REGS-POINTER ,index))
		      (loop (cdr names) (+ index 8)))
		'()))))))

(define-entries #x80			; (* 16 8)
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
  fixnum-shift)

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