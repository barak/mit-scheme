#| -*-Scheme-*-

$Id: lapgen.scm,v 1.29 2001/12/20 21:45:24 cph Exp $

Copyright (c) 1992-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; RTL Rules utilities for i386 and family.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define available-machine-registers
  ;; esp holds the the stack pointer
  ;; ebp holds the pointer mask
  ;; esi holds the register array pointer
  ;; edi holds the free pointer
  ;; fr7 is not used so that we can always push on the stack once.
  (list eax ecx edx ebx fr0 fr1 fr2 fr3 fr4 fr5 fr6))

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
	     FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT FLOAT)
	  register))
	((register-value-class=word? register)
	 'GENERAL)
	((register-value-class=float? register)
	 'FLOAT)
	(else
	 (error "unable to determine register type" register))))

(define (register-types-compatible? type1 type2)
  (boolean=? (eq? type1 'FLOAT) (eq? type2 'FLOAT)))

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (let loop ((i 0))
      (cond ((>= i number-of-machine-registers)
	     (lambda (register)
	       (vector-ref references register)))
	    ((< i 8)
	     (vector-set! references i (INST-EA (R ,i)))
	     (loop (1+ i)))
	    (else
	     (vector-set! references i (INST-EA (ST ,(floreg->sti i))))
	     (loop (1+ i)))))))

(define (register->register-transfer source target)
  (machine->machine-register source target))

(define (reference->register-transfer source target)
  (cond ((equal? (register-reference target) source)
	 (LAP))
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

(define-integrable (float-register-reference? ea)
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
  (if (not (register-types-compatible? source target))
      (error "Moving between incompatible register types" source target))
  (if (not (float-register? source))
      (LAP (MOV W ,(register-reference target) ,(register-reference source)))
      (let ((ssti (floreg->sti source))
	    (tsti (floreg->sti target)))
	(if (zero? ssti)
	    (LAP (FST (ST ,tsti)))
	    (LAP (FLD (ST ,ssti))
		 (FSTP (ST ,(1+ tsti))))))))

(define (machine-register->memory source target)
  (if (not (float-register? source))
      (LAP (MOV W ,target ,(register-reference source)))
      (let ((ssti (floreg->sti source)))
	(if (zero? ssti)
	    (LAP (FST D ,target))
	    (LAP (FLD (ST ,ssti))
		 (FSTP D ,target))))))

(define (memory->machine-register source target)
  (if (not (float-register? target))
      (LAP (MOV W ,(register-reference target) ,source))
      (LAP (FLD D ,source)
	   (FSTP (ST ,(1+ (floreg->sti target)))))))

(define-integrable (offset-reference register offset)
  (byte-offset-reference register (* 4 offset)))

(define (byte-offset-reference register offset)
  (cond ((zero? offset)
	 (INST-EA (@R ,register)))
	((fits-in-signed-byte? offset)
	 (INST-EA (@RO B ,register ,offset)))
	(else
	 (INST-EA (@RO W ,register ,offset)))))

(define (byte-unsigned-offset-reference register offset)
  (cond ((zero? offset)
	 (INST-EA (@R ,register)))
	((fits-in-unsigned-byte? offset)
	 (INST-EA (@RO UB ,register ,offset)))
	(else
	 (INST-EA (@RO UW ,register ,offset)))))

(define-integrable (pseudo-register-offset register)
  (+ (+ (* 16 4) (* 80 4))
     (* 3 (register-renumber register))))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (floreg->sti reg)
  (- reg fr0))

(define-integrable (general-register? register)
  (< register fr0))

(define-integrable (float-register? register)
  (<= fr0 register fr7))

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
  (let ((code (load-constant (INST-EA (R ,mreg)) object)))
    (require-register! mreg)
    code))

(define (assign-register->register target source)
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define (convert-object/constant->register target constant conversion)
  (delete-dead-registers!)
  (let ((target (target-register-reference target)))
    (if (non-pointer-object? constant)
	;; Is this correct if conversion is object->address ?
	(load-non-pointer target 0 (careful-object-datum constant))
	(LAP ,@(load-constant target constant)
	     ,@(conversion target)))))

(define (non-pointer->literal object)
  (make-non-pointer-literal (object-type object)
			    (careful-object-datum object)))

(define (load-immediate target value)
  (if (zero? value)
      (LAP (XOR W ,target ,target))
      (LAP (MOV W ,target (& ,value)))))

(define (load-non-pointer target type datum)
  (let ((immediate-value (make-non-pointer-literal type datum)))
    (if (zero? immediate-value)
	(LAP (XOR W ,target ,target))
	(LAP (MOV W ,target (&U ,immediate-value))))))

(define (load-constant target obj)
  (if (non-pointer-object? obj)
      (load-non-pointer target (object-type obj) (careful-object-datum obj))
      (load-pc-relative target (constant->label obj))))

(define (load-pc-relative target label-expr)
  (with-pc
    (lambda (pc-label pc-register)
      (LAP (MOV W ,target (@RO W ,pc-register (- ,label-expr ,pc-label)))))))

(define (load-pc-relative-address target label-expr)
  (with-pc
    (lambda (pc-label pc-register)
      (LAP (LEA ,target (@RO W ,pc-register (- ,label-expr ,pc-label)))))))  

(define (with-pc recvr)
  (with-values (lambda () (get-cached-label))
    (lambda (label reg)
      (if label
	  (recvr label reg)
	  (let ((temporary (allocate-temporary-register! 'GENERAL)))
	    (pc->reg temporary
		     (lambda (label prefix)
		       (cache-label! label temporary)
		       (LAP ,@prefix
			    ,@(recvr label temporary)))))))))

(define (pc->reg reg recvr)
  (let ((label (generate-label 'GET-PC)))
    (recvr label
	   (LAP (CALL (@PCR ,label))
		(LABEL ,label)
		(POP ,(register-reference reg))))))

(define-integrable (get-cached-label)
  (register-map-label *register-map* 'GENERAL))

(define-integrable (cache-label! label temporary)
  (set! *register-map*
	(set-machine-register-label *register-map* temporary label))
  unspecific)

(define (compare/register*register reg1 reg2)
  (cond ((register-alias reg1 'GENERAL)
	 =>
	 (lambda (alias)
	   (LAP (CMP W ,(register-reference alias) ,(any-reference reg2)))))
	((register-alias reg2 'GENERAL)
	 =>
	 (lambda (alias)
	   (LAP (CMP W ,(any-reference reg1) ,(register-reference alias)))))
	(else
	 (LAP (CMP W ,(source-register-reference reg1)
		   ,(any-reference reg2))))))

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
	       (LAP (LEA ,tref ,ea)
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
	((<= -128 offset 127)
	 (INST-EA (@ROI B ,base ,offset ,index ,scale)))
	(else
	 (INST-EA (@ROI W ,base ,offset ,index ,scale)))))

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

(define (object->type target)
  (LAP (SHR W ,target (& ,scheme-datum-width))))

(define (object->datum target)
  (LAP (AND W ,target (R ,regnum:datum-mask))))

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
	    ,@(load-non-pointer (rtl:machine-constant-value
				 (rtl:cons-pointer-type expression))
				(rtl:machine-constant-value
				 (rtl:cons-pointer-datum expression))
				target)))
      ((OFFSET)
       (let ((source-reference (offset->reference! expression)))
	 (LAP ,@(clear-registers! register)
	      (MOV W ,target ,source-reference))))
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


(let-syntax ((define-codes
	       (lambda (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'CODE:COMPILER-
						(car names))
				,index)
			     (loop (cdr names) (1+ index)))))
		 `(BEGIN ,@(loop names start)))))
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

(define-integrable (invoke-hook entry)
  (LAP (JMP ,entry)))

(define-integrable (invoke-hook/call entry)
  (LAP (CALL ,entry)))

(define-integrable (invoke-interface code)
  (LAP (MOV B (R ,eax) (& ,code))
       ,@(invoke-hook entry:compiler-scheme-to-interface)))

(define-integrable (invoke-interface/call code)
  (LAP (MOV B (R ,eax) (& ,code))
       ,@(invoke-hook/call entry:compiler-scheme-to-interface/call)))

(let-syntax ((define-entries
	       (lambda (start high . names)
		 (define (loop names index high)
		   (cond ((null? names)
			  '())
			 ((>= index high)
			  (warn "define-entries: Too many for byte offsets.")
			  (loop names index (+ high 32000)))
			 (else
			  (cons `(DEFINE-INTEGRABLE
				   ,(symbol-append 'ENTRY:COMPILER-
						   (car names))
				   (byte-offset-reference regnum:regs-pointer
							  ,index))
				(loop (cdr names) (+ index 4) high)))))
		 `(BEGIN ,@(loop names start high)))))
  (define-entries #x40 #x80		; (* 16 4)
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

  (define-entries #x-80 0
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
    conditionally-serialize))

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
