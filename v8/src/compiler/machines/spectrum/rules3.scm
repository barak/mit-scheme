#| -*-Scheme-*-

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP Generation Rules: Invocations and Entries
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  (pop-return))

(define (pop-return)
  (LAP ,@(clear-map!)
       ;; This assumes that the return address is always longword aligned
       ;; (it better be, since instructions should be longword aligned).
       ;; Thus the bottom two bits of temp are 0, representing the
       ;; highest privilege level, and the privilege level will
       ;; not be changed by the BV instruction.
       (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) 1)
       ;; Originally was ,@(object->address 1) 
       ,@(entry->address 1)
       (BV (N) 0 1)))

(define (%invocation:apply frame-size)
  (case frame-size
    ((1) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-1 4
			      ,regnum:scheme-to-interface-ble))))
    ((2) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-2 4
			      ,regnum:scheme-to-interface-ble))))
    ((3) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-3 4
			      ,regnum:scheme-to-interface-ble))))
    ((4) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-4 4
			      ,regnum:scheme-to-interface-ble))))
    ((5) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-5 4
			      ,regnum:scheme-to-interface-ble))))
    ((6) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-6 4
			      ,regnum:scheme-to-interface-ble))))
    ((7) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-7 4
			      ,regnum:scheme-to-interface-ble))))
    ((8) (LAP (BLE () (OFFSET ,hook:compiler-shortcircuit-apply-8 4
			      ,regnum:scheme-to-interface-ble))))
    (else
     (LAP ,@(load-immediate frame-size regnum:second-arg)
	  (BLE () (OFFSET ,hook:compiler-shortcircuit-apply 4
			  ,regnum:scheme-to-interface-ble))))))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,@(%invocation:apply frame-size)
       (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) ,regnum:first-arg)))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		;ignore
  (LAP ,@(clear-map!)
       (B (N) (@PCR ,label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		;ignore
  ;; It expects the procedure at the top of the stack
  (pop-return))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,@(load-immediate number-pushed regnum:second-arg)
       ,@(load-pc-relative-address label regnum:first-arg 'CODE)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into first-arg
  (LAP ,@(clear-map!)
       (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) ,regnum:first-arg)
       ,@(load-immediate number-pushed regnum:second-arg)
       ,@(object->address regnum:first-arg)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  (invocation:some-uuo-link frame-size continuation name free-uuo-link-label))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  (invocation:some-uuo-link frame-size continuation name
			    global-uuo-link-label))

(define (invocation:some-uuo-link frame-size continuation name label-generator)
  (if continuation
      (if compiler:compile-by-procedures? ; i.e. small offsets
	  ;; Perhaps a better idea than this would be to generate the general
	  ;; code and peephole optimise
	  ;;  (BL () r (@pco 0))
	  ;;  (LDO/LDW () (offset d 0 r) t)
	  ;;  (B (N) (@pcr label))
	  ;; to
	  ;;  (BL () t (@pcr label)
	  ;;  (LDO/LDW () (offset d 0 t) t)
	  ;; where d' 

	  (let ((here  (generate-label)))
	    (let ((value `(+ ,here ,(+ 8 *privilege-level*))))
	      (LAP ,@(clear-map!)
		   (LABEL ,here)
		   (BL () 19 (@PCR ,(label-generator name frame-size)))
		   (LDO () (OFFSET (- ,continuation ,value) 0 19) 19))))
	  (LAP ,@(clear-map!)
	       ,@(load-pc-relative-address continuation 19 'CODE)
	       (B (N) (@PCR ,(label-generator name frame-size)))))
      (LAP ,@(clear-map!)
	   (B (N) (@PCR ,(label-generator name frame-size))))))
     

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (? extension register-expression))
  continuation				;ignore
  (LAP ,@(load-interface-args! extension false false false)
       ,@(load-immediate frame-size regnum:third-arg)
       ,@(load-pc-relative-address *block-label* regnum:second-arg 'CODE)
       ,@(invoke-interface code:compiler-cache-reference-apply)))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (? environment register-expression)
		     (? name))
  continuation				;ignore
  (LAP ,@(load-interface-args! environment false false false)
       ,(load-constant name regnum:second-arg)
       ,(load-immediate frame-size regnum:third-arg)
       ,@(invoke-interface code:compiler-lookup-apply)))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (if (eq? primitive compiled-error-procedure)
      (LAP ,@(clear-map!)
	   ,@(load-immediate frame-size regnum:first-arg)
	   ,@(invoke-interface code:compiler-error))
      (let ((arity (primitive-procedure-arity primitive)))
	(if (not (negative? arity))
	    (invoke-primitive primitive
			      hook:compiler-invoke-primitive)
	    (LAP ,@(clear-map!)
		 ,@(load-pc-relative (constant->label primitive)
				     regnum:first-arg
				     'CONSTANT)
		 ,@(cond ((= arity -1)
			  (LAP ,@(load-immediate (-1+ frame-size) 1)
			       (STW () 1 ,reg:lexpr-primitive-arity)
			       ,@(invoke-interface
				  code:compiler-primitive-lexpr-apply)))
			 #|
			 ((not (negative? arity))
			  (invoke-interface code:compiler-primitive-apply))
			 |#
			 (else
			  ;; Unknown primitive arity.  Go through apply.
			  (LAP ,@(load-immediate frame-size regnum:second-arg)
			       ,@(invoke-interface code:compiler-apply)))))))))

(define (invoke-primitive primitive hook)
  ;; Only for known, fixed-arity primitives
  (LAP ,@(clear-map!)
       ,@(invoke-hook hook)
       (WORD () (- ,(constant->label primitive) *PC*))))

(let-syntax
    ((define-old-optimized-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    frame-size
	    (old-optimized-primitive-invocation
	     ,(symbol-append 'HOOK:COMPILER- name)
	     continuation))))

     (define-optimized-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    frame-size
	    (optimized-primitive-invocation
	     ,(symbol-append 'HOOK:COMPILER- name)
	      continuation))))

     (define-allocation-primitive
       (macro (name)
	 (let ((prim (make-primitive-procedure name true)))
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,prim)
	    (open-code-block-allocation ',name ',prim
					,(symbol-append 'HOOK:COMPILER- name)
					frame-size continuation))))))

  (define-optimized-primitive-invocation &+)
  (define-optimized-primitive-invocation &-)
  (define-optimized-primitive-invocation &*)
  (define-optimized-primitive-invocation &/)
  (define-optimized-primitive-invocation &=)
  (define-optimized-primitive-invocation &<)
  (define-optimized-primitive-invocation &>)

  ;; Defunct.
  ;;(define-old-optimized-primitive-invocation 1+)
  ;;(define-old-optimized-primitive-invocation -1+)
  ;;(define-old-optimized-primitive-invocation zero?)
  ;;(define-old-optimized-primitive-invocation positive?)
  ;;(define-old-optimized-primitive-invocation negative?)

  (define-optimized-primitive-invocation quotient)
  (define-optimized-primitive-invocation remainder)

  (define-optimized-primitive-invocation set-interrupt-enables!)

  (define-allocation-primitive vector-cons)
  (define-allocation-primitive string-allocate)
  (define-allocation-primitive floating-vector-cons))

(define (preserving-regs clobbered-regs gen-suffix)
  ;; THIS IS ***NOT*** GENERAL PURPOSE CODE.
  ;; It assumes a bunch of things, like "the pseudo-registers
  ;; currently assigned to the clobbered registers aren't going to be
  ;; referenced before their contents are restored."
  ;; It is intended only for preserving registers around in-line calls
  ;; that may need to back in to the interpreter in rare cases.
  (define *comments* '())
  (define (delete-clobbered-aliases-for-recomputable-pseudo-registers preserved)
    (let* ((how (cadr preserved))
	   (reg (car preserved)))
      (if (eq? how 'RECOMPUTE)
	  (let ((entry (map-entries:find-home *register-map* reg)))
	    (if entry
		(let* ((aliases (map-entry-aliases entry))
		       (new-entry
			(make-map-entry
			 (map-entry-home entry)
			 false		; Not in home anymore
			 (list-transform-negative aliases
			   (lambda (alias) (memq alias clobbered-regs)))
					; No clobbered regs. for aliases
			 (map-entry-label entry))))
		  (set! *comments*
			(append
			 *comments*
			 `((COMMENT CLOBBERDATA: (,reg ,how ,entry ,new-entry)))))
		  (set! *register-map*
			(make-register-map
			 (map-entries:replace *register-map* entry new-entry)
			 (map-registers *register-map*)))))))))
  (for-each delete-clobbered-aliases-for-recomputable-pseudo-registers
    *preserved-registers*)
  (let ((clean (apply require-registers! clobbered-regs)))
    (LAP ,@clean
	 ,@*comments*
	 ,@(call-with-values
	    clear-map!/preserving
	    (lambda (machine-regs pseudo-regs)
	      (cond ((and (null? machine-regs) (null? pseudo-regs))
		     (gen-suffix false))
		    ((null? pseudo-regs)
		     (gen-suffix (->mask machine-regs false false)))
		    (else
		     (call-with-values
		      (lambda () (->bytes pseudo-regs))
		      (lambda (gen-int-regs gen-float-regs)
			(gen-suffix (->mask machine-regs
					    gen-int-regs
					    gen-float-regs)))))))))))

(define (->bytes pseudo-regs)
  ;; (values gen-int-regs gen-float-regs)
  (define (do-regs regs)
    (LAP (COMMENT (PSEUDO-REGISTERS . ,regs))
	 ,@(bytes->uwords
	    (let* ((l (length regs))
		   (bytes (reverse (cons l
					 (map register-renumber regs)))))
	      (append (let ((r (remainder (+ l 1) 4)))
			(if (zero? r)
			    '()
			    (make-list (- 4 r) 0)))
		      bytes)))))

  (call-with-values
   (lambda ()
     (list-split pseudo-regs
		 (lambda (reg)
		   (value-class=float? (pseudo-register-value-class reg)))))
   (lambda (float-regs int-regs)
     (values (and (not (null? int-regs))
		  (lambda () (do-regs int-regs)))
	     (and (not (null? float-regs))
		  (lambda () (do-regs float-regs)))))))

(define (->mask machine-regs gen-int-regs gen-float-regs)
  (let ((int-mask (make-bit-string 32 false))
	(flo-mask (make-bit-string 32 false)))
    (if gen-int-regs
	(bit-string-set! int-mask (- 31 0)))
    (if gen-float-regs
	(bit-string-set! int-mask (- 31 1)))
    (let loop ((regs machine-regs))
      (cond ((not (null? regs))
	     (let ((reg (car regs)))
	       (if (< reg 32)
		   (bit-string-set! int-mask (- 31 reg))
		   (bit-string-set! flo-mask (- 31 (- reg 32))))
	       (loop (cdr regs))))
	    ((bit-string-zero? flo-mask)
	     (lambda ()
	       (LAP ,@(if gen-float-regs (gen-float-regs) (LAP))
		    ,@(if gen-int-regs (gen-int-regs) (LAP))
		    (COMMENT (MACHINE-REGS . ,machine-regs))
		    (UWORD () ,(bit-string->unsigned-integer int-mask)))))
	    (else
	     (bit-string-set! int-mask (- 31 31))
	     (lambda ()
	       (LAP ,@(if gen-float-regs (gen-float-regs) (LAP))
		    (COMMENT (MACHINE-REGS . ,machine-regs))
		    (UWORD () ,(bit-string->unsigned-integer flo-mask))
		    ,@(if gen-int-regs (gen-int-regs) (LAP))
		    (COMMENT (MACHINE-REGS . ,machine-regs))
		    (UWORD () ,(bit-string->unsigned-integer int-mask)))))))))

;; *** optimized-primitive-invocation and open-code-block-allocation
;; skip the first instruction of the hook as a way of signalling
;; that there are registers to preserve.  Eventually the convention
;; can be changed, but this one is backwards compatible. ***

(define *optimized-clobbered-regs*
  (list g31 g2 g26 g25 g28 g29 fp4 fp5))

(define (optimized-primitive-invocation hook cont-label)
  (preserving-regs
   *optimized-clobbered-regs*
   (lambda (gen-preservation-info)
     (let ((load-continuation
	    (if cont-label
		(load-pc-relative-address cont-label 19 'CODE)
		'())))
       (if (not gen-preservation-info)
	   (LAP ,@load-continuation
		,@(invoke-hook/no-return hook))
	   (let ((label1 (generate-label))
		 (label2 (generate-label)))
	     (LAP ,@load-continuation
		  (BLE () (OFFSET ,hook 4 ,regnum:scheme-to-interface-ble))
		  (LDO () (OFFSET (- (- ,label2 ,label1) ,*privilege-level*)
				  0 31)
		       31)
		  (LABEL ,label1)
		  ,@(gen-preservation-info)
		  (LABEL ,label2))))))))

(define (old-optimized-primitive-invocation hook cont-label)
  (let ((load-continuation
	 (if cont-label
	     (load-pc-relative-address cont-label 19 'CODE)
	     '())))
    (LAP ,@(clear-map!)
	 ,@load-continuation
	 ,@(invoke-hook/no-return hook))))

(define *allocation-clobbered-regs*
  (list g31 g2 g26 g25 g28 g29))

(define (open-code-block-allocation name prim hook frame-size cont-label)
  name frame-size cont-label		; ignored
  (preserving-regs
   *allocation-clobbered-regs*
   (lambda (gen-preservation-info)
     (let ((load-continuation
	    (if cont-label
		(load-pc-relative-address cont-label 19 'CODE)
		'())))
       (if (not gen-preservation-info)
	   (LAP ,@(clear-map!)
		,@load-continuation
		,@(invoke-hook hook)
		(WORD () (- ,(constant->label prim) *PC*)))
	   (let ((label1 (generate-label))
		 (label2 (generate-label)))
	     (LAP ,@load-continuation
		  (BLE () (OFFSET ,hook 4 ,regnum:scheme-to-interface-ble))
		  (ADDI () (- (- ,label2 ,label1) ,*privilege-level*) 31 31)
		  (LABEL ,label1)
		  ,@(gen-preservation-info)
		  (LABEL ,label2)
		  (WORD () (- ,(constant->label prim) *PC*)))))))))

#|
(define (open-code-block-allocation name prim hook frame-size cont-label)
  ;; One argument (length in units) on top of the stack.
  ;; Note: The length checked is not necessarily the complete length
  ;; of the object, but is off by a constant number of words, which
  ;; is OK, since we can cons a finite number of words without
  ;; checking.
  (define (default)
    (LAP ,@(clear-map!)
	 ,@(load-pc-relative (constant->label prim)
			     regnum:first-arg
			     'CONSTANT)
	 ,@(invoke-interface code:compiler-primitive-apply)))

  hook					; ignored
  (cond ((not (= frame-size 2))
	 (error "open-code-allocate-block: Wrong number of arguments"
		prim frame-size))
	((not compiler:open-code-primitives?)
	 (default))
	(else
	 (let ((label (generate-label))
	       (rsp regnum:stack-pointer)
	       (rfp regnum:free-pointer)
	       (rmp regnum:memtop-pointer)
	       (ra1 regnum:first-arg)
	       (ra2 regnum:second-arg)
	       (ra3 regnum:third-arg)
	       (rrv regnum:return-value))

	   (define (end tag rl)
	     (LAP ,@(deposit-type (ucode-type manifest-nm-vector) rl)
		  (STW () ,rl (OFFSET 0 0 ,rrv))
		  ,@(deposit-type tag rrv)
		  (LDO () (OFFSET ,(* 4 frame-size) 0 ,rsp) ,rsp)
		  (B (N) (@PCR ,cont-label))
		  (LABEL ,label)
		  ,@(default)))
	     
	   (case name
	     ((STRING-ALLOCATE)
	      (LAP (LDW () (OFFSET 0 0 ,rsp) ,ra1)
		   (COPY () ,rfp ,rrv)
		   ,@(object->datum ra1 ra1)
		   (ADD () ,ra1 ,rfp ,ra2)
		   (COMB (>= N) ,ra2 ,rmp (@PCR ,label))
		   (STB () 0 (OFFSET 8 0 ,ra2))
		   (SHD () 0 ,ra1 2 ,ra3)
		   (LDO () (OFFSET 2 0 ,ra3) ,ra3)
		   (STWS (MB) ,ra1 (OFFSET 4 0 ,rfp))
		   (SH2ADD () ,ra3 ,rfp ,rfp)
		   ,@(end (ucode-type string) ra3)))
	     ((FLOATING-VECTOR-CONS)
	      (LAP (LDW () (OFFSET 0 0 ,rsp) ,ra1)
		   ;; (STW () 0 (OFFSET 0 0 ,rfp))
		   (DEPI () #b100 31 3 ,rfp)  ; 8-byte alignment for elements
		   (COPY () ,rfp ,rrv)
		   ,@(object->datum ra1 ra1)
		   (SH3ADD () ,ra1 ,rfp ,ra2)
		   (COMB (>= N) ,ra2 ,rmp (@PCR ,label))
		   (SHD () ,ra1 0 31 ,ra1)
		   (LDO () (OFFSET 4 0 ,ra2) ,rfp)
		   ,@(end (ucode-type flonum) ra1)))
	     (else
	      (error "open-code-block-allocation: Unknown primitive"
		     name)))))))
|#		    

;;;; Invocation Prefixes

;;; MOVE-FRAME-UP size address
;;;
;;; Moves up the last <size> words of the stack so that the first of
;;; these words is at location <address>, and resets the stack pointer
;;; to the last of these words.  That is, it pops off all the words
;;; between <address> and TOS+/-<size>.

(define-rule statement
  ;; Move up 0 words back to top of stack : a No-Op
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER (? reg)))
  (QUALIFIER (= reg regnum:stack-pointer))
  (LAP))

#|
(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? reg)))
  (QUALIFIER (= reg regnum:dynamic-link))
  (generate/move-frame-up frame-size
			  (lambda (reg)
			    (LAP (COPY () ,regnum:dynamic-link ,reg)))))
|#

(define-rule statement
  ;; Move <frame-size> words back to SP+offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size)
   (OFFSET-ADDRESS (REGISTER (? reg))
		   (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (= reg regnum:stack-pointer))
  (let ((how-far (* 4 (- offset frame-size))))
    (cond ((zero? how-far)
	   (LAP))
	  ((negative? how-far)
	   (error "invocation-prefix:move-frame-up: bad specs"
		  frame-size offset))
	  ((zero? frame-size)
	   (load-offset how-far regnum:stack-pointer regnum:stack-pointer))
	  ((= frame-size 1)
	   (let ((temp (standard-temporary!)))
	     (LAP (LDWM () (OFFSET ,how-far 0 ,regnum:stack-pointer) ,temp)
		  (STW () ,temp (OFFSET 0 0 ,regnum:stack-pointer)))))
	  ((= frame-size 2)
	   (let ((temp1 (standard-temporary!))
		 (temp2 (standard-temporary!)))
	     (LAP (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) ,temp1)
		  (LDWM () (OFFSET ,(- how-far 4) 0 ,regnum:stack-pointer)
			,temp2)
		  (STW () ,temp1 (OFFSET 0 0 ,regnum:stack-pointer))
		  (STW () ,temp2 (OFFSET 4 0 ,regnum:stack-pointer)))))
	  (else
	   (generate/move-frame-up frame-size
	     (lambda (reg)
	       (load-offset (* 4 offset) regnum:stack-pointer reg)))))))

(define-rule statement
  ;; Move <frame-size> words back to base virtual register + offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size)
   (OFFSET-ADDRESS (REGISTER (? base))
		   (MACHINE-CONSTANT (? offset))))
  (generate/move-frame-up frame-size
    (lambda (reg)
      (load-offset (* 4 offset) (standard-source! base) reg))))

;;; DYNAMIC-LINK instructions have a <frame-size>, <new frame end>,
;;; and <current dynamic link> as arguments.  They pop the stack by
;;; removing the lesser of the amount needed to move the stack pointer
;;; back to the <new frame end> or <current dynamic link>.  The last
;;; <frame-size> words on the stack (the stack frame for the procedure
;;; about to be called) are then put back onto the newly adjusted
;;; stack.

#|
(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? source))
				  (REGISTER (? reg)))
  (QUALIFIER (= reg regnum:dynamic-link))
  (if (and (zero? frame-size)
	   (= source regnum:stack-pointer))
      (LAP)
      (let ((env-reg (standard-move-to-temporary! source)))
	(LAP
	 ;; skip if env LS dyn link
	 (SUB (<<=) ,env-reg ,regnum:dynamic-link 0)
	 ;; env <- dyn link
	 (COPY () ,regnum:dynamic-link ,env-reg)
	 ,@(generate/move-frame-up* frame-size env-reg)))))
|#

(define (generate/move-frame-up frame-size destination-generator)
  (let ((temp (standard-temporary!)))
    (LAP ,@(destination-generator temp)
	 ,@(generate/move-frame-up* frame-size temp))))

(define (generate/move-frame-up* frame-size destination)
  ;; Destination is guaranteed to be a machine register number; that
  ;; register has the destination base address for the frame.  The stack
  ;; pointer is reset to the top end of the copied area.
  (LAP ,@(case frame-size
	   ((0)
	    (LAP))
	   ((1)
	    (let ((temp (standard-temporary!)))
	      (LAP (LDW () (OFFSET 0 0 ,regnum:stack-pointer) ,temp)
		   (STWM () ,temp (OFFSET -4 0 ,destination)))))
	   (else
	    (generate/move-frame-up** frame-size destination)))
       (COPY () ,destination ,regnum:stack-pointer)))

(define (generate/move-frame-up** frame-size dest)
  (let ((from (standard-temporary!))
	(temp1 (standard-temporary!))
	(temp2 (standard-temporary!)))
    (LAP ,@(load-offset (* 4 frame-size) regnum:stack-pointer from)
	 ,@(if (<= frame-size 3)
	       ;; This code can handle any number > 1 (handled above),
	       ;; but we restrict it to 3 for space reasons.
	       (let loop ((n frame-size))
		 (case n
		   ((0)
		    (LAP))
		   ((3)
		    (let ((temp3 (standard-temporary!)))
		      (LAP (LDWM () (OFFSET -4 0 ,from) ,temp1)
			   (LDWM () (OFFSET -4 0 ,from) ,temp2)
			   (LDWM () (OFFSET -4 0 ,from) ,temp3)
			   (STWM () ,temp1 (OFFSET -4 0 ,dest))
			   (STWM () ,temp2 (OFFSET -4 0 ,dest))
			   (STWM () ,temp3 (OFFSET -4 0 ,dest)))))
		   (else
		    (LAP (LDWM () (OFFSET -4 0 ,from) ,temp1)
			 (LDWM () (OFFSET -4 0 ,from) ,temp2)
			 (STWM () ,temp1 (OFFSET -4 0 ,dest))
			 (STWM () ,temp2 (OFFSET -4 0 ,dest))
			 ,@(loop (- n 2))))))
	       (LAP ,@(load-immediate frame-size temp2)
		    (LDWM () (OFFSET -4 0 ,from) ,temp1)
		    (ADDIBF (=) -1 ,temp2 (@PCO -12))
		    (STWM () ,temp1 (OFFSET -4 0 ,dest)))))))

;;;; External Labels

(define (make-external-label code label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (EXTERNAL-LABEL () ,code (@PCR ,label))
       (LABEL ,label)))

;;; Entry point types

(define-integrable (make-code-word min max)
  (+ (* #x100 min) max))

(define (make-procedure-code-word min max)
  ;; The "min" byte must be less than #x80; the "max" byte may not
  ;; equal #x80 but can take on any other value.
  (if (or (negative? min) (>= min #x80))
      (error "MAKE-PROCEDURE-CODE-WORD: minimum out of range" min))
  (if (>= (abs max) #x80)
      (error "MAKE-PROCEDURE-CODE-WORD: maximum out of range" max))
  (make-code-word min (if (negative? max) (+ #x100 max) max)))

(define expression-code-word
  (make-code-word #xff #xff))

(define internal-entry-code-word
  (make-code-word #xff #xfe))

(define internal-continuation-code-word
  (make-code-word #xff #xfc))

;; #xff #xfb taken up by return-to-interpreter and reflect-to-interface

(define internal-closure-code-word
  (make-code-word #xff #xfa))

(define (continuation-code-word label)
  (frame-size->code-word
   (if label
       (rtl-continuation/next-continuation-offset (label->object label))
       0)
   internal-continuation-code-word))

(define (internal-procedure-code-word rtl-proc)
  ;; represented as return addresses so the debugger will
  ;; not barf when it sees them (on the stack if interrupted).
  (frame-size->code-word
   (rtl-procedure/next-continuation-offset rtl-proc)
   internal-entry-code-word))

(define (frame-size->code-word offset default)
  (cond ((not offset)
	 default)
	((< offset #x2000)
	 ;; This uses up through (#xff #xdf).
	 (let ((qr (integer-divide offset #x80)))
	   (make-code-word (+ #x80 (integer-divide-remainder qr))
			   (+ #x80 (integer-divide-quotient qr)))))
	(else
	 (error "Unable to encode continuation offset" offset))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? size)))
  (cons-closure target procedure-label min max size))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? size) (? entries)))
  ;; entries is a vector of all the entry points
  (case nentries
    ((0)
     (let ((dest (standard-target! target)))
       (LAP ,@(load-non-pointer (ucode-type manifest-vector)
				size
				dest)
	    (STW () ,dest (OFFSET 0 0 ,regnum:free-pointer))
	    (COPY () ,regnum:free-pointer ,dest)
	    ,@(load-offset (* 4 (1+ size))
			   regnum:free-pointer
			   regnum:free-pointer))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (cons-closure
	target (car entry) (cadr entry) (caddr entry) size)))
    (else
     (cons-multiclosure target nentries size (vector->list entries)))))

#|
;;; Old style closure consing -- Out of line.

(define (%cons-closure target total-size size core)
  (let* ((flush-reg (require-registers! regnum:first-arg
					#| regnum:addil-result |#
				        regnum:ble-return))
	 (target (standard-target! target)))
    (LAP ,@flush-reg
	 ;; Vector header
	 ,@(load-non-pointer (ucode-type manifest-closure)
			     total-size
			     regnum:first-arg)
	 (STWS (MA C) ,regnum:first-arg (OFFSET 4 0 ,regnum:free-pointer))
	 ;; Make entries and store result
	 ,@(core target)
	 ;; Allocate space for closed-over variables
	 ,@(load-offset (* 4 size)
			regnum:free-pointer
			regnum:free-pointer))))

(define (cons-closure target entry min max size)
  (%cons-closure
   target
   (+ size closure-entry-size)
   size
   (lambda (target)
     (LAP ;; Entry point is result.
	 ,@(load-offset 4 regnum:free-pointer target)
	 ,@(cons-closure-entry entry min max 8)))))

(define (cons-multiclosure target nentries size entries)
  (define (generate-entries offset entries)
    (if (null? entries)
	(LAP)
	(let ((entry (car entries)))
	  (LAP ,@(cons-closure-entry (car entry) (cadr entry) (caddr entry)
				     offset)
	       ,@(generate-entries (+ offset (* 4 closure-entry-size))
				   (cdr entries))))))

  (%cons-closure
   target
   (+ 1 (* closure-entry-size nentries) size)
   size
   (lambda (target)
     (LAP ;; Number of closure entries
	 ,@(load-entry-format nentries 0 target)
	 (STWS (MA C) ,target (OFFSET 4 0 ,regnum:free-pointer))
	 ;; First entry point is result.
	 ,@(load-offset 4 regnum:free-pointer target)
	 ,@(generate-entries 12 entries)))))

;; Utilities for old-style closure consing.

(define (load-entry-format code-word gc-offset dest)
  (load-immediate (+ (* code-word #x10000)
		     (quotient gc-offset 2))
		  dest))

(define (cons-closure-entry entry min max offset)
  ;; Call an out-of-line hook to do this.
  ;; Making the instructions is a lot of work!
  ;; Perhaps there should be a closure hook invoked and the real
  ;; entry point could follow.  It would also be easier on the GC.
  (let ((entry-label (rtl-procedure/external-label (label->object entry))))
    (LAP ,@(load-entry-format (make-procedure-code-word min max)
			      offset
			      regnum:first-arg)
	 #|
	 ;; This does not work!!! The LDO may overflow.
	 ;; A new pseudo-op has been introduced for this purpose.
	 (BLE ()
	      (OFFSET ,hook:compiler-store-closure-entry
		      4
		      ,regnum:scheme-to-interface-ble))
	 (LDO ()
	      (OFFSET (- ,entry-label (+ *PC* 4))
		      0
		      ,regnum:ble-return)
	      ,regnum:addil-result)
	 |#
	 (PCR-HOOK ()
		   ,regnum:addil-result
		   (OFFSET ,hook:compiler-store-closure-entry
			   4
			   ,regnum:scheme-to-interface-ble)
		   (@PCR ,entry-label)))))
|#

;; Magic for compiled entries.

(define-integrable (address->entry register)
  (adjust-type quad-mask-value (ucode-type compiled-entry) register))

(define-integrable (entry->address register)
  (adjust-type (ucode-type compiled-entry) quad-mask-value register))

;;; New style closure consing using compiler-prepared and
;;; linker-maintained patterns

;; Compiled code blocks are aligned like floating-point numbers and vectors.
;; That is, the address of their header word is congruent 4 mod 8

(define *initial-dword-offset* 4)
(define *closure-padding-bitstring* (make-bit-string 32 false))

;; This agrees with hppa_extract_absolute_address in microcode/cmpintmd/hppa.h

(define *ldil/ble-split*
  ;; (expt 2 13) ***
  8192)

(define *ldil-factor*
  ;; (/ *ldil/ble-split* ldil-scale)
  4)

(define (declare-closure-pattern! pattern)
  (add-extra-code!
   (or (find-extra-code-block 'CLOSURE-PATTERNS)
       (let ((section-label (generate-label))
	     (ev-label (generate-label)))
	 (let ((block (declare-extra-code-block!
		       'CLOSURE-PATTERNS
		       'LAST
		       `(((/ (- ,ev-label ,section-label) 4)
			  . ,ev-label)))))
	   (add-extra-code! block
			    (LAP (LABEL ,section-label)))
	   block)))
   (LAP (PADDING ,(- 4 *initial-dword-offset*) 8 ,*closure-padding-bitstring*)
	,@pattern)))

(define (generate-closure-entry offset pattern label min max)
  (let ((entry-label (rtl-procedure/external-label (label->object label))))
    (LAP (USHORT ()
		 ,(make-procedure-code-word min max)
		 ,(quotient offset 2))
	 ;; This contains an offset -- the linker turns it to an abs. addr.
	 (LDIL () (* (QUOTIENT (- (+ ,pattern ,offset) ,entry-label)
			       ,*ldil/ble-split*)
		     ,*ldil-factor*)
	       26)
	 (BLE () (OFFSET (REMAINDER (- (+ ,pattern ,offset) ,entry-label)
				    ,*ldil/ble-split*)
			 5 26))
	 (ADDI () -15 31 25))))

(define (cons-closure target entry-label min max size)
  (let ((offset 8)
	(total-size (+ size closure-entry-size))
	(pattern (generate-label)))

    (declare-closure-pattern!
     (LAP ,@(lap:comment `(CLOSURE-PATTERN ,entry-label))
	  (LABEL ,pattern)
	  (UWORD () ,(make-non-pointer-literal (ucode-type manifest-closure)
					       total-size))
	  ,@(generate-closure-entry offset pattern entry-label min max)))
    #|
    ;; This version uses ordinary integer instructions

    (let* ((offset* (* 4 (1+ closure-entry-size)))
	   (target (standard-target! target))
	   (temp1 (standard-temporary!))
	   (temp2 (standard-temporary!))
	   (temp3 (standard-temporary!)))

      (LAP ,@(load-pc-relative-address pattern target 'CODE)
	   (LDWS (MA) (OFFSET 4 0 ,target) ,temp1)
	   (LDWS (MA) (OFFSET 4 0 ,target) ,temp2)
	   (LDWS (MA) (OFFSET 4 0 ,target) ,temp3)
	   (STWS (MA C) ,temp1 (OFFSET 4 0 ,regnum:free-pointer))
	   (STWS (MA C) ,temp2 (OFFSET 4 0 ,regnum:free-pointer))
	   (STWS (MA C) ,temp3 (OFFSET 4 0 ,regnum:free-pointer))

	   (LDWS (MA) (OFFSET 4 0 ,target) ,temp1)
	   (LDWS (MA) (OFFSET 4 0 ,target) ,temp2)
	   (STWS (MA C) ,temp1 (OFFSET 4 0 ,regnum:free-pointer))
	   (STWS (MA C) ,temp2 (OFFSET 4 0 ,regnum:free-pointer))
	   (LDO () (OFFSET ,(- offset offset*) 0 ,regnum:free-pointer) ,target)
	   (FDC () (INDEX 0 0 ,target))
	   (FDC () (INDEX 0 0 ,regnum:free-pointer))
	   (SYNC ())
	   (FIC () (INDEX 0 5 ,target))
	   (SYNC ())
	   (LDO () (OFFSET ,(* 4 size) 0 ,regnum:free-pointer)
		,regnum:free-pointer)))
    |#

    #|
    ;; This version is faster by using floating-point (doubleword) moves

    (let* ((offset* (* 4 (1+ closure-entry-size)))
	   (target (standard-target! target))
	   (dwtemp1 (flonum-temporary!))
	   (dwtemp2 (flonum-temporary!))
	   (swtemp (standard-temporary!)))

      (LAP ,@(load-pc-relative-address pattern target 'CODE)
	   (DEPI () #b100 31 3 ,regnum:free-pointer)		; quad align
	   (LDWS (MA) (OFFSET 4 0 ,target) ,swtemp)
    	   (FLDDS (MA) (OFFSET 8 0 ,target) ,dwtemp1)
	   (STWS (MA) ,swtemp (OFFSET 4 0 ,regnum:free-pointer))
	   (FLDDS (MA) (OFFSET 8 0 ,target) ,dwtemp2)
	   (FSTDS (MA) ,dwtemp1 (OFFSET 8 0 ,regnum:free-pointer))
	   (LDO () (OFFSET ,(- offset (- offset* 8)) 0 ,regnum:free-pointer)
		,target)
	   (FSTDS (MA) ,dwtemp2 (OFFSET 8 0 ,regnum:free-pointer))
	   (FDC () (INDEX 0 0 ,target))
	   (FDC () (INDEX 0 0 ,regnum:free-pointer))
	   (SYNC ())
	   (FIC () (INDEX 0 5 ,target))
	   (SYNC ())
	   (LDO () (OFFSET ,(* 4 size) 0 ,regnum:free-pointer)
		,regnum:free-pointer)))
    |#

    ;; This version does the copy out of line, using fp instructions.

    (let* ((hook-label (generate-label))
	   (flush-reg (require-registers! g29 g28 g26 g25 fp11 fp10
					  #| regnum:addil-result |#
					  regnum:ble-return)))
      (delete-register! target)
      (delete-dead-registers!)
      (add-pseudo-register-alias! target g25)
      (LAP ,@flush-reg
	   ,@(invoke-hook hook:compiler-copy-closure-pattern)
	   (LABEL ,hook-label)
	   (UWORD () (- (- ,pattern ,hook-label) ,*privilege-level*))
	   (LDO () (OFFSET ,(* 4 size) 0 ,regnum:free-pointer)
		,regnum:free-pointer)))))

(define (cons-multiclosure target nentries size entries)
  ;; nentries > 1
  (let ((offset 12)
	(total-size (+ (+ 1 (* closure-entry-size nentries)) size))
	(pattern (generate-label)))

    (declare-closure-pattern!
     (LAP ,@(lap:comment `(CLOSURE-PATTERN ,(caar entries)))
	  (LABEL ,pattern)
	  (UWORD () ,(make-non-pointer-literal (ucode-type manifest-closure)
					       total-size))
	  (USHORT () ,nentries 0)
	  ,@(let make-entries ((entries entries)
			       (offset offset))
	      (if (null? entries)
		  (LAP)
		  (let ((entry (car entries)))
		    (LAP ,@(generate-closure-entry offset
						   pattern
						   (car entry)
						   (cadr entry)
						   (caddr entry))
			 ,@(make-entries (cdr entries)
					 (+ offset
					    (* 4 closure-entry-size)))))))))
    #|
    ;; This version uses ordinary integer instructions

    (let ((target (standard-target! target)))
      (let ((temp1 (standard-temporary!))
	    (temp2 (standard-temporary!))
	    (ctr (standard-temporary!))
	    (srcptr (standard-temporary!))
	    (index (standard-temporary!))
	    (loop-label (generate-label)))

	(LAP ,@(load-pc-relative-address pattern srcptr 'CODE)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp1)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp2)
	     (STWS (MA C) ,temp1 (OFFSET 4 0 ,regnum:free-pointer))
	     (STWS (MA C) ,temp2 (OFFSET 4 0 ,regnum:free-pointer))
	     (LDO () (OFFSET 4 0 ,regnum:free-pointer) ,target)
	     (LDI () -16 ,index)
	     (LDI () ,nentries ,ctr)
	     ;; The loop copies 16 bytes, and the architecture specifies
	     ;; that a cache line must be a multiple of this value.
	     ;; Therefore we only need to flush once per loop,
	     ;; and once more (D only) to take care of phase.
	     (LABEL ,loop-label)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp1)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp2)
	     (STWS (MA C) ,temp1 (OFFSET 4 0 ,regnum:free-pointer))
	     (STWS (MA C) ,temp2 (OFFSET 4 0 ,regnum:free-pointer))
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp1)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp2)
	     (STWS (MA C) ,temp1 (OFFSET 4 0 ,regnum:free-pointer))
	     (STWS (MA C) ,temp2 (OFFSET 4 0 ,regnum:free-pointer))
	     (FDC () (INDEX ,index 0 ,regnum:free-pointer))
	     (SYNC ())
	     (ADDIB (>) -1 ,ctr ,ctr (@PCR ,loop-label))
	     (FIC () (INDEX ,index 5 ,regnum:free-pointer))
	     (FDC () (INDEX 0 0 ,regnum:free-pointer))
	     (SYNC ())
	     (FIC () (INDEX 0 5 ,regnum:free-pointer))
	     (SYNC ())
	     (LDO () (OFFSET ,(* 4 size) 0 ,regnum:free-pointer)
		  ,regnum:free-pointer))))
    |#

    #|
    ;; This version is faster by using floating-point (doubleword) moves

    (let ((target (standard-target! target)))
      (let ((dwtemp1 (flonum-temporary!))
	    (dwtemp2 (flonum-temporary!))
	    (temp (standard-temporary!))
	    (ctr (standard-temporary!))
	    (srcptr (standard-temporary!))
	    (index (standard-temporary!))
	    (loop-label (generate-label)))

	(LAP ,@(load-pc-relative-address pattern srcptr 'CODE)
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp)
	     (DEPI () #b100 31 3 ,regnum:free-pointer)		; quad align
	     (STWS (MA C) ,temp (OFFSET 4 0 ,regnum:free-pointer))
	     (LDO () (OFFSET 8 0 ,regnum:free-pointer) ,target)
	     (LDI () -16 ,index)
	     (LDI () ,nentries ,ctr)

	     ;; The loop copies 16 bytes, and the architecture specifies
	     ;; that a cache line must be a multiple of this value.
	     ;; Therefore we only need to flush (D) once per loop,
	     ;; and once more to take care of phase.
	     ;; We only need to flush the I cache once because it is
	     ;; newly allocated memory.

	     (LABEL ,loop-label)
	     (FLDDS (MA) (OFFSET 8 0 ,srcptr) ,dwtemp1)
	     (FLDDS (MA) (OFFSET 8 0 ,srcptr) ,dwtemp2)
	     (FSTDS (MA) ,dwtemp1 (OFFSET 8 0 ,regnum:free-pointer))
	     (FSTDS (MA) ,dwtemp2 (OFFSET 8 0 ,regnum:free-pointer))
	     (ADDIB (>) -1 ,ctr (@PCR ,loop-label))
	     (FDC () (INDEX ,index 0 ,regnum:free-pointer))
		
	     (LDWS (MA) (OFFSET 4 0 ,srcptr) ,temp)
	     (LDI () ,(* -4 (1+ size)) ,index)
	     (STWM () ,temp (OFFSET ,(* 4 (1+ size)) 0 ,regnum:free-pointer))
	     (FDC () (INDEX ,index 0 ,regnum:free-pointer))
	     (SYNC ())
	     (FIC () (INDEX 0 5 ,target))
	     (SYNC ()))))
    |#
    
    ;; This version does the copy out of line, using fp instructions.

    (let* ((hook-label (generate-label))
	   (flush-reg (require-registers! g29 g28 g26 g25 fp11 fp10
					  #| regnum:addil-result |#
					  regnum:ble-return)))
      (delete-register! target)
      (delete-dead-registers!)
      (add-pseudo-register-alias! target g25)
      (LAP ,@flush-reg
	   (LDI () ,nentries 1)
	   ,@(invoke-hook hook:compiler-copy-multiclosure-pattern)
	   (LABEL ,hook-label)
	   (UWORD () (- (- ,pattern ,hook-label) ,*privilege-level*))
	   (LDO () (OFFSET ,(* 4 size) 0 ,regnum:free-pointer)
		,regnum:free-pointer)))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  ;; Calls the linker
  (in-assembler-environment
   (empty-register-map)
   (list 2 19
	 regnum:first-arg regnum:second-arg
	 regnum:third-arg regnum:fourth-arg)
   (lambda ()
     (let* ((segment (load-pc-relative-address environment-label 1 'CONSTANT)))
       (LAP (STWM () 2 (OFFSET -4 0 ,regnum:stack-pointer))  ; Push Env
	    (STWM () 19 (OFFSET -4 0 ,regnum:stack-pointer)) ; Push continuation
	    ,@segment
	    (STW () 2 (OFFSET 0 0 1))
	    ,@(load-pc-relative-address *block-label* regnum:second-arg 'CODE)
	    ,@(load-pc-relative-address free-ref-label regnum:third-arg
					'CONSTANT)
	    ,@(load-immediate n-sections regnum:fourth-arg)
	    ,@(invoke-interface-ble code:compiler-link)
	    ,@(make-external-label (continuation-code-word false)
				   (generate-label))
	    ;; 19 popped by call to code:compiler-link
	    (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) 2) ; Env
	    )))))


(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  ;; Link all of the top level procedures within the file
  (in-assembler-environment
   (empty-register-map)
   (list 2 19
	 regnum:first-arg regnum:second-arg
	 regnum:third-arg regnum:fourth-arg)
   (lambda ()
     (let* ((segment (load-pc-relative code-block-label regnum:second-arg
				       'CONSTANT)))
       (LAP (STWM () 2 (OFFSET -4 0 ,regnum:stack-pointer)) ; Push Env
	    (STWM () 19 (OFFSET -4 0 ,regnum:stack-pointer)) ; Push continuation
	    ,@segment
	    ,@(object->address regnum:second-arg)
	    ,@(load-offset environment-offset regnum:second-arg 1)
	    (STW () 2 (OFFSET 0 0 1))
	    ,@(load-offset free-ref-offset regnum:second-arg regnum:third-arg)
	    ,@(load-immediate n-sections regnum:fourth-arg)
	    ,@(invoke-interface-ble code:compiler-link)
	    ,@(make-external-label (continuation-code-word false)
				   (generate-label))
	    ;; 19 popped by call to code:compiler-link
	    (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) 2) ; Env
	    )))))

(define (in-assembler-environment map needed-registers thunk)
  (fluid-let ((*register-map* map)
	      (*prefix-instructions* (LAP))
	      (*suffix-instructions* (LAP))
	      (*needed-registers* needed-registers))
    (let ((instructions (thunk)))
      (LAP ,@*prefix-instructions*
	   ,@instructions
	   ,@*suffix-instructions*))))

(define (generate/remote-links n-code-blocks code-blocks-label n-sections)
  (if (= n-code-blocks 0)
      (LAP)
      (let ((loop (generate-label))
	    (bytes (generate-label))
	    (after-bytes (generate-label)))
	(LAP (STWM () 0 (OFFSET -4 0 ,regnum:stack-pointer))
	     (COPY () 0 ,regnum:first-arg)
	     (LABEL ,loop)
	     (LDO () (OFFSET 1 0 ,regnum:first-arg) ,regnum:second-arg)
	     (STW () ,regnum:second-arg (OFFSET 0 0 ,regnum:stack-pointer))
	     (BL () ,regnum:third-arg (@PCR ,after-bytes))
	     (DEP () 0 31 2 ,regnum:third-arg)
	     (LABEL ,bytes)
	     ,@(sections->bytes n-code-blocks n-sections)
	     (LABEL ,after-bytes)
	     (LDBX () (INDEX ,regnum:first-arg 0 ,regnum:third-arg)
		   ,regnum:fourth-arg)
	     (LDW () (OFFSET (- ,code-blocks-label ,bytes) 0 ,regnum:third-arg)
		  ,regnum:third-arg)
	     ,@(object->address regnum:third-arg)
	     (LDWX (S) (INDEX ,regnum:second-arg 0 ,regnum:third-arg)
		   ,regnum:second-arg)
	     ,@(object->address regnum:second-arg)
	     (LDW () (OFFSET 4 0 ,regnum:second-arg) ,regnum:third-arg)
	     (LDW () (OFFSET 0 0 ,regnum:second-arg) ,regnum:first-arg)
	     ,@(object->datum regnum:third-arg regnum:third-arg)
	     ,@(object->datum regnum:first-arg regnum:first-arg)
	     (SH2ADD () ,regnum:third-arg ,regnum:second-arg ,regnum:third-arg)
	     (SH2ADD () ,regnum:first-arg ,regnum:second-arg
		     ,regnum:first-arg)
	     (LDO () (OFFSET 8 0 ,regnum:third-arg) ,regnum:third-arg)
	     (STW () 2 (OFFSET 0 0 ,regnum:first-arg))
	     (STWM () 2 (OFFSET -4 0 ,regnum:stack-pointer))  ;Push Env
	     (STWM () 19 (OFFSET -4 0 ,regnum:stack-pointer)) ;Push continuation
	     ,@(invoke-interface-ble code:compiler-link)
	     ,@(make-external-label (continuation-code-word false)
				    (generate-label))	 
	     ;; 19 popped by call to code:compiler-link
	     (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) 2) ; Env
	     (LDW () (OFFSET 0 0 ,regnum:stack-pointer) ,regnum:first-arg)
	     ,@(cond ((fits-in-5-bits-signed? n-code-blocks)
		      (LAP (COMIBF (<=) ,n-code-blocks ,regnum:first-arg
				   (@PCR ,loop))
			   (NOP ())))
		     ((fits-in-11-bits-signed? n-code-blocks)
		      (LAP (COMICLR (<=) ,n-code-blocks ,regnum:first-arg 0)
			   (B (N) (@PCR ,loop))))
		     (else
		      (LAP (LDI () ,n-code-blocks ,regnum:second-arg)
			   (COMBF (<=) ,regnum:second-arg ,regnum:first-arg
				  (@PCR ,loop))
			   (NOP ()))))
	     (LDO () (OFFSET 4 0 ,regnum:stack-pointer)
		  ,regnum:stack-pointer)))))

(define (sections->bytes n-code-blocks n-sections)
  (bytes->uwords (append (vector->list n-sections)
			 (let ((left (remainder n-code-blocks 4)))
			   (if (zero? left)
			       '()
			       (make-list (- 4 left) 0))))))

(define (bytes->uwords bytes)
  ;; There must be a multiple of 4 bytes
  (let walk ((bytes bytes))
    (if (null? bytes)
	(LAP)
	(let ((hi    (car bytes))
	      (midhi (cadr bytes))
	      (midlo (caddr bytes))
	      (lo    (cadddr bytes)))
	  (LAP (UWORD ()
		      ,(+ lo (* 256 (+ midlo (* 256 (+ midhi (* 256 hi)))))))
	       ,@(walk (cddddr bytes)))))))

(define (generate/constants-block constants references assignments
				  uuo-links global-links static-vars)
  (let ((constant-info
	 ;; Note: generate/remote-links depends on all the linkage sections
	 ;; (references & uuos) being first!
	 (declare-constants 0 (transmogrifly uuo-links)
	   (declare-constants 1 references
	     (declare-constants 2 assignments
	       (declare-constants 3 (transmogrifly global-links)
		 (declare-closure-patterns
		  (declare-constants false (map (lambda (pair)
						  (cons false (cdr pair)))
						static-vars)
		    (declare-constants false constants
		      (cons false (LAP)))))))))))
    (let ((free-ref-label (car constant-info))
	  (constants-code (cdr constant-info))
	  (profiling-info-label-1
	   (and compiler:generate-profiling-instructions?
		(allocate-constant-label)))
	  (debugging-information-label (allocate-constant-label))
	  (environment-label (allocate-constant-label))
	  (n-sections
	   (+ (if (null? uuo-links) 0 1)
	      (if (null? references) 0 1)
	      (if (null? assignments) 0 1)
	      (if (null? global-links) 0 1)
	      (if (not (find-extra-code-block 'CLOSURE-PATTERNS)) 0 1))))
      (values
       (LAP ,@constants-code
	    ,@(if profiling-info-label-1
		  `((SCHEME-OBJECT ,profiling-info-label-1    PROFILING-INFO-1)
		    (SCHEME-OBJECT ,(allocate-constant-label) PROFILING-INFO-2))
		  `())
	    ;; Place holder for the debugging info filename
	    (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	    ;; Place holder for the load time environment if needed
	    (SCHEME-OBJECT ,environment-label
			   ,(if (null? free-ref-label) 0 'ENVIRONMENT)))
       environment-label
       free-ref-label
       n-sections))))

(define (declare-constants/tagged tag header constants info)
  (define-integrable (wrap tag label value)
    (LAP (,tag ,label ,value)))

  (define (inner constants)
    (if (null? constants)
	(cdr info)
	(let ((entry (car constants)))
	  (LAP ,@(wrap tag (cdr entry) (car entry))
	       ,@(inner (cdr constants))))))

  (if (and header (not (null? constants)))
      (let ((label (allocate-constant-label)))
	(cons label
	      (LAP (SCHEME-OBJECT
		    ,label
		    ,(let ((datum (length constants)))
		       (if (> datum #xffff)
			   (error "datum too large" datum))
		       (+ (* header #x10000) datum)))
		   ,@(inner constants))))
      (cons (car info) (inner constants))))

(define (declare-constants header constants info)
  (declare-constants/tagged 'SCHEME-OBJECT header constants info))

(define (declare-closure-patterns info)
  (let ((block (find-extra-code-block 'CLOSURE-PATTERNS)))
    (if (not block)
	info
	(declare-constants/tagged 'SCHEME-EVALUATION
				  4
				  (extra-code-block/xtra block)
				  info))))

(define (declare-evaluations header evals info)
  (declare-constants/tagged 'SCHEME-EVALUATION header evals info))

(define (transmogrifly uuos)
  (define (inner name assoc)
    (if (null? assoc)
	(transmogrifly (cdr uuos))
	`((,name . ,(cdar assoc))		; uuo-label	LDIL
	  (0 . ,(allocate-constant-label))	; spare		BLE
	  (,(caar assoc) .			; frame-size
	   ,(allocate-constant-label))
	  ,@(inner name (cdr assoc)))))
  (if (null? uuos)
      '()
      (inner (caar uuos) (cdar uuos))))

;;;; New RTL

(define-rule statement
  (INVOCATION:REGISTER (? frame-size)
		       (? continuation)
		       (REGISTER (? dest))
		       #F (MACHINE-CONSTANT (? nregs)))
  frame-size				; ignored
  nregs					; ignored
  (profile-info/add 'INVOCATION:REGISTER)
  (if continuation
      (need-register! 19))
  (let ((load-continuation
	 (if continuation
	     (load-pc-relative-address continuation 19 'CODE)
	     '())))
    (let ((addr (standard-source! dest)))
      (LAP ,@(clear-map!)
	   ,@load-continuation
	   (BV (N) 0 ,addr)))))

;;(define-rule statement
;;  (INVOCATION:REGISTER 0 #F (REGISTER (? reg))
;;		       #F (MACHINE-CONSTANT (? nregs)))
;;  nregs					; ignored
;;  (profile-info/add 'INVOCATION:REGISTER)
;;  (let ((addr (standard-source! reg)))
;;    (LAP ,@(clear-map!)
;;	 (BV (N) 0 ,addr))))

(define-rule statement
  (INVOCATION:PROCEDURE 0 (? continuation) (? destination)
			(MACHINE-CONSTANT (? nregs)))
  nregs					; ignored
  (profile-info/add 'INVOCATION:PROCEDURE)
  (LAP ,@(clear-map!)
       ,@(if (not continuation)
	     (LAP (B (N) (@PCR ,destination)))
	     (LAP (BL () 19 (@PCR ,destination))
		  (LDO () (OFFSET ,(- 4 *privilege-level*) 0 19) 19)))))

(define-rule statement
  (INVOCATION:NEW-APPLY (? frame-size) (? continuation)
			(REGISTER (? dest)) (MACHINE-CONSTANT (? nregs)))
  ;; *** For now, ignore nregs and use frame-size ***
  nregs
  (profile-info/add 'INVOCATION:NEW-APPLY)
  (let* ((obj (register-alias dest (register-type dest)))
	 (prefix (if obj
		     (LAP)
		     (%load-machine-register! dest regnum:first-arg
					      delete-dead-registers!)))
	 (obj* (or obj regnum:first-arg)))
    (need-register! obj*)
    (if continuation
	(need-register! 19))
    (let ((addr (if untagged-entries? obj* (standard-temporary!)))
	  (temp (standard-temporary!))
	  (label (generate-label))
	  (load-continuation
	   (if continuation
	       (load-pc-relative-address continuation 19 'CODE)
	       '())))
      (LAP ,@prefix
	   ,@(clear-map!)
	   ,@load-continuation
	   ,@(object->type obj* temp)
	   ,@(let ((tag (ucode-type compiled-entry)))
	       (if (fits-in-5-bits-signed? tag)
		   (LAP (COMIBN (<>) ,tag ,temp (@PCR ,label)))
		   (LAP (COMICLR (=) ,tag ,temp 0)
			(B (N) (@PCR ,label)))))
	   ,@(if untagged-entries?
		 (LAP)
		 (LAP (COPY () ,obj* ,addr)
		      ,@(adjust-type (ucode-type compiled-entry)
				     quad-mask-value
				     addr)))
	   (LDB () (OFFSET -3 0 ,addr) ,temp)
	   (COMICLR (<>) ,frame-size ,temp 0)
	   (BV (N) 0 ,addr)
	   (LABEL ,label)
	   ,@(copy obj* regnum:first-arg)
	   ,@(%invocation:apply frame-size)
	   (NOP ())))))

(define-rule statement
  (RETURN-ADDRESS (? label)
		  (? dbg-info)
		  (MACHINE-CONSTANT (? frame-size))
		  (MACHINE-CONSTANT (? nregs)))
  dbg-info nregs			; ignored
  (begin
    (restore-registers!)
    (make-external-label
     (frame-size->code-word frame-size internal-continuation-code-word)
     label)))

(define-rule statement
  (PROCEDURE (? label) (? dbg-info) (MACHINE-CONSTANT (? frame-size)))
  dbg-info				; ignored
  (make-external-label (frame-size->code-word frame-size
					      internal-continuation-code-word)
		       label))

(define-rule statement
  (TRIVIAL-CLOSURE (? label)
		   (? dbg-info)
		   (MACHINE-CONSTANT (? min))
		   (MACHINE-CONSTANT (? max)))
  dbg-info				; ignored
  (make-external-label (make-procedure-code-word min max)
		       label))

(define-rule statement
  (CLOSURE (? label) (? dbg-info) (MACHINE-CONSTANT (? frame-size)))
  dbg-info frame-size			; ignored
  (LAP ,@(make-external-label internal-closure-code-word label)))

(define-rule statement
  (EXPRESSION (? label) (? dbg-info))
  #|
  ;; Prefix takes care of this
  (LAP ,@(make-external-label expression-code-word label))
  |#
  label dbg-info			; ignored
  (LAP))

#|
(define (interrupt-check:procedure/avoid-for-this-label? label)
  ;; A hack to test Bill's hypothesis that a lot of time is going into the
  ;; interrupt check at receiver-x, alt-x, and cons-x procedures.
  (define (like? pattern)
    (let ((s-pat  (symbol-name pattern))
	  (s-lab  (symbol-name label)))
      (and (> (string-length s-lab) (string-length s-pat))
	   (substring=? s-pat 0 (string-length s-pat)
			s-lab 0 (string-length s-pat)))))
  (or (like? 'alt-)
      (like? 'cons-)
      (like? 'next-)
      (like? 'receiver-)))
|#
(define (interrupt-check:procedure/avoid-for-this-label? label)
  label
  false)

(define-rule statement
  (INTERRUPT-CHECK:PROCEDURE (? intrpt) (? heap) (? stack) (? label)
			     (MACHINE-CONSTANT (? frame-size)))
  (if (interrupt-check:procedure/avoid-for-this-label? label)
      (begin
	(internal-warning "Eliding interrupt check at " label)
	(LAP))
      (generate-interrupt-check/new
       intrpt heap stack
       (lambda (interrupt-label)
	 (let ((ret-add-label (generate-label)))
	   (LAP (LABEL ,interrupt-label)
		(LDI () ,(- frame-size 1) 1)
		,@(invoke-hook hook:compiler-interrupt-procedure/new)
		(LABEL ,ret-add-label)
		(WORD () (- (- ,label ,ret-add-label) ,*privilege-level*))))))))

(define-rule statement
  (INTERRUPT-CHECK:CONTINUATION (? intrpt) (? heap) (? stack) (? label)
				(MACHINE-CONSTANT (? frame-size)))
  ;; Generated both for continuations and in some weird case of
  ;; top-level expressions.
  (generate-interrupt-check/new
   intrpt heap
   (and (= frame-size 1) stack)		; expressions only
   (lambda (interrupt-label)
     (let ((ret-add-label (generate-label)))
       (LAP (LABEL ,interrupt-label)
	    (LDI () ,(- frame-size 1) 1)
	    #| (LDI ()
		    ,(if (= nregs 0)	; **** probably wrong
			 code:compiler-interrupt-procedure
			 code:compiler-interrupt-continuation)
		    28) |#
	    ,@(invoke-hook hook:compiler-interrupt-continuation/new)
	    (LABEL ,ret-add-label)
	    (WORD () (- (- ,label ,ret-add-label) ,*privilege-level*)))))))

(define-rule statement
  (INTERRUPT-CHECK:CLOSURE (? intrpt) (? heap) (? stack)
			   (MACHINE-CONSTANT (? frame-size)))
  (generate-interrupt-check/new
   intrpt heap stack
   (lambda (interrupt-label)
     (LAP (LABEL ,interrupt-label)
	  (LDI () ,(- frame-size 2) 1)	; Continuation and self
					; register are saved by other
					; means.
	  ,@(invoke-hook hook:compiler-interrupt-closure/new)))))

(define-rule statement
  (INTERRUPT-CHECK:SIMPLE-LOOP (? intrpt) (? heap) (? stack)
			       (? loop-label) (? header-label)
			       (MACHINE-CONSTANT (? frame-size)))
  ;; Nothing generates this now -- JSM
  loop-label				; ignored
  (generate-interrupt-check/new
   intrpt heap stack
   (lambda (interrupt-label)
     (let ((ret-add-label (generate-label)))
       (LAP (LABEL ,interrupt-label)
	    (LDI () ,(- frame-size 1) 1)
	    ,@(invoke-hook hook:compiler-interrupt-procedure/new)
	    (LABEL ,ret-add-label)
	    (WORD () (- (- ,header-label ,ret-add-label)
			,*privilege-level*)))))))

(define (generate-interrupt-check/new intrpt heap stack generate-stub)
  ;; This does not check the heap because it is assumed that there is
  ;; a large buffer at the end of the heap.  As long as the code can't
  ;; loop without checking, which is what intrpt guarantees, there
  ;; is no need to check.

  (if (and (number? heap)
	   (> heap 1000))
      (internal-warning "Large allocation " heap 'words))
  (let* ((interrupt-label (generate-label))
	 (heap-check? intrpt)
	 (stack-check? (and stack compiler:generate-stack-checks?))
	 (need-interrupt-code (lambda ()
				(add-end-of-block-code!
				 (lambda ()
				   (generate-stub interrupt-label))))))
    (cond ((and heap-check? stack-check?)
	   (need-interrupt-code)
	   (profile-info/add 'HEAP-CHECK)
	   (profile-info/add 'STACK-CHECK)
	   (LAP (LDW () ,reg:stack-guard 1)
		(COMB (>=) ,regnum:free-pointer ,regnum:memtop-pointer
		      (@PCR ,interrupt-label))
		(COMB (<=) ,regnum:stack-pointer 1 (@PCR ,interrupt-label))
		(LDW () ,reg:memtop ,regnum:memtop-pointer)))
	  (heap-check?
	   (need-interrupt-code)
	   (profile-info/add 'HEAP-CHECK)
	   (LAP (COMB (>=) ,regnum:free-pointer ,regnum:memtop-pointer
		      (@PCR ,interrupt-label))
		(LDW () ,reg:memtop ,regnum:memtop-pointer)))
	  (stack-check?
	   (need-interrupt-code)
	   (profile-info/add 'STACK-CHECK)
	   (LAP (LDW () ,reg:stack-guard 1)
		(COMBN (<=) ,regnum:stack-pointer 1 (@PCR ,interrupt-label))))
	  (else
	   (LAP)))))

(define-rule statement
  (ASSIGN (REGISTER (? dst)) (ALIGN-FLOAT (REGISTER (? src))))
  (let ((dst (standard-move-to-target! src dst)))
    (LAP
     ;; The STW instruction would make the heap parsable forwards
     ;; (STW () 0 (OFFSET 0 0 ,dst))
     (DEPI () #b100 31 3 ,dst))))



(define-rule statement
  (PROFILE-COUNT)
  (let ((counter-label  (generate-label)))
    (profile-info/declare counter-label)
    (LAP (BLE (N) (OFFSET ,hook:compiler-profile-count
			 4
			 ,regnum:scheme-to-interface-ble))
	 (LABEL ,counter-label)
	 (UWORD () 0))))


;;(define-rule statement
;;  (PROFILE-DATA (CONSTANT (? data)))
;;  (profile-info/add data))

;; *** For now ***

(define-rule statement
  (ASSIGN (REGISTER (? target)) (STATIC-CELL (? name)))
  (***unimplemented-rtl***
   `(ASSIGN (REGISTER ,target) (STATIC-CELL ,name))))

(define (***unimplemented-rtl*** inst)
  (error "Unimplemented RTL statement" inst))   

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
