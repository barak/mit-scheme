#| -*-Scheme-*-

$Id: rules3.scm,v 4.35 1992/09/26 15:56:19 cph Exp $

Copyright (c) 1988-92 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; LAP Generation Rules: Invocations and Entries
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  (pop-return))

(define (pop-return)
  (let ((temp (standard-temporary!)))
    (LAP ,@(clear-map!)
	 ;; This assumes that the return address is always longword aligned
	 ;; (it better be, since instructions should be longword aligned).
	 ;; Thus the bottom two bits of temp are 0, representing the
	 ;; highest privilege level, and the privilege level will
	 ;; not be changed by the BV instruction.
	 (LDWM () (OFFSET 4 0 22) ,temp)
	 ,@(object->address temp)
	 (BV (N) 0 ,temp))))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,@(case frame-size
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
				 ,regnum:scheme-to-interface-ble)))))
       (LDWM () (OFFSET 4 0 22) ,regnum:first-arg)))

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
       ,@(load-pc-relative-address label regnum:first-arg)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into first-arg
  (LAP ,@(clear-map!)
       (LDWM () (OFFSET 4 0 22) ,regnum:first-arg)
       ,@(load-immediate number-pushed regnum:second-arg)
       ,@(object->address regnum:first-arg)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (B (N) (@PCR ,(free-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (B (N) (@PCR ,(global-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (? extension register-expression))
  continuation				;ignore
  (LAP ,@(load-interface-args! extension false false false)
       ,@(load-immediate frame-size regnum:third-arg)
       ,@(load-pc-relative-address *block-label* regnum:second-arg)
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
      (LAP ,@(clear-map!)
	   ,@(load-pc-relative (constant->label primitive)
			       regnum:first-arg)
	   ,@(let ((arity (primitive-procedure-arity primitive)))
	       (cond ((not (negative? arity))
		      (invoke-interface code:compiler-primitive-apply))
		     ((= arity -1)
		      (LAP ,@(load-immediate (-1+ frame-size) 1)
			   (STW () 1 ,reg:lexpr-primitive-arity)
			   ,@(invoke-interface
			      code:compiler-primitive-lexpr-apply)))
		     (else
		      ;; Unknown primitive arity.  Go through apply.
		      (LAP ,@(load-immediate frame-size regnum:second-arg)
			   ,@(invoke-interface code:compiler-apply))))))))

(let-syntax
    ((define-special-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    frame-size continuation
	    (special-primitive-invocation
	     ,(symbol-append 'CODE:COMPILER- name)))))

     (define-optimized-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    frame-size continuation
	    (optimized-primitive-invocation
	     ,(symbol-append 'HOOK:COMPILER- name))))))

  (define-optimized-primitive-invocation &+)
  (define-optimized-primitive-invocation &-)
  (define-optimized-primitive-invocation &*)
  (define-optimized-primitive-invocation &/)
  (define-optimized-primitive-invocation &=)
  (define-optimized-primitive-invocation &<)
  (define-optimized-primitive-invocation &>)
  (define-optimized-primitive-invocation 1+)
  (define-optimized-primitive-invocation -1+)
  (define-optimized-primitive-invocation zero?)
  (define-optimized-primitive-invocation positive?)
  (define-optimized-primitive-invocation negative?)
  (define-special-primitive-invocation quotient)
  (define-special-primitive-invocation remainder))

(define (special-primitive-invocation code)
  (LAP ,@(clear-map!)
       ,@(invoke-interface code)))

(define (optimized-primitive-invocation hook)
  (LAP ,@(clear-map!)
       ,@(invoke-hook hook)))

;;;; Invocation Prefixes

;;; MOVE-FRAME-UP size address
;;;
;;; Moves up the last <size> words of the stack so that the first of
;;; these words is at location <address>, and resets the stack pointer
;;; to the last of these words.  That is, it pops off all the words
;;; between <address> and TOS+/-<size>.

(define-rule statement
  ;; Move up 0 words back to top of stack : a No-Op
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 22))
  (LAP))

(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER 19))
  (generate/move-frame-up frame-size
			  (lambda (reg) (LAP (COPY () 19 ,reg)))))

(define-rule statement
  ;; Move <frame-size> words back to SP+offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 22) (? offset)))
  (let ((how-far (* 4 (- offset frame-size))))
    (cond ((zero? how-far)
	   (LAP))
	  ((negative? how-far)
	   (error "invocation-prefix:move-frame-up: bad specs"
		  frame-size offset))
	  ((zero? frame-size)
	   (load-offset how-far 22 22))
	  ((= frame-size 1)
	   (let ((temp (standard-temporary!)))
	     (LAP (LDWM () (OFFSET ,how-far 0 22) ,temp)
		  (STW () ,temp (OFFSET 0 0 22)))))
	  ((= frame-size 2)
	   (let ((temp1 (standard-temporary!))
		 (temp2 (standard-temporary!)))
	     (LAP (LDWM () (OFFSET 4 0 22) ,temp1)
		  (LDWM () (OFFSET ,(- how-far 4) 0 22) ,temp2)
		  (STW () ,temp1 (OFFSET 0 0 22))
		  (STW () ,temp2 (OFFSET 4 0 22)))))
	  (else
	   (generate/move-frame-up frame-size
	     (lambda (reg)
	       (load-offset (* 4 offset) 22 reg)))))))

(define-rule statement
  ;; Move <frame-size> words back to base virtual register + offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER (? base))
						   (? offset)))
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

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? source))
				  (REGISTER 19))
  (if (and (zero? frame-size)
	   (= source regnum:stack-pointer))
      (LAP)
      (let ((env-reg (standard-move-to-temporary! source)))
	(LAP (SUB (<<=) ,env-reg 19 0)	; skip if env LS dyn link
	     (COPY () 19 ,env-reg)	; env <- dyn link
	     ,@(generate/move-frame-up* frame-size env-reg)))))

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
	      (LAP (LDW () (OFFSET 0 0 22) ,temp)
		   (STWM () ,temp (OFFSET -4 0 ,destination)))))
	   (else
	    (generate/move-frame-up** frame-size destination)))
       (COPY () ,destination 22)))

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

;;;; Procedure headers

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.
;;;
;;; The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.
;;;
;;; The only exception is the dynamic link register, handled
;;; specially.  Procedures that require a dynamic link use a different
;;; interrupt handler that saves and restores the dynamic link
;;; register.

(define (simple-procedure-header code-word label code)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 ,@(invoke-interface-ble code)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check gc-label))))

(define (dlink-procedure-header code-word label)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 (COPY () ,regnum:dynamic-link ,regnum:second-arg)
	 ,@(invoke-interface-ble code:compiler-interrupt-dlink)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check gc-label))))

(define (interrupt-check gc-label)
  (case compiler:generate-stack-checks?
    ((#F)
     (LAP (COMB (>=) ,regnum:free-pointer ,regnum:memtop-pointer
		(@PCR ,gc-label))
	  (LDW () ,reg:memtop ,regnum:memtop-pointer)))
    ((OUT-OF-LINE)
     (let ((label (generate-label)))
       (LAP (BLE ()
		 (OFFSET ,hook:compiler-stack-and-interrupt-check
			 4
			 ,regnum:scheme-to-interface-ble))
	    ;; Assumes that (<= #x-2000 (- ,gc-label ,label) #x1fff)
	    ;; otherwise this assembles to two instructions, and it
	    ;; won't fit in the branch-delay slot.
	    (LDI () (- ,gc-label ,label) ,regnum:first-arg)
	    (LABEL ,label))))
    (else
     (LAP (LDW () ,reg:stack-guard ,regnum:first-arg)
	  (COMB (>=) ,regnum:free-pointer ,regnum:memtop-pointer
		(@PCR ,gc-label))
	  ;; I think the next two instructions could be interchanged,
	  ;; allowing the NOP to be eliminated, but since I don't have
	  ;; the PA-RISC programming manual here I can't be sure. -- CPH
	  (LDW () ,reg:memtop ,regnum:memtop-pointer)
	  (COMB (<=) ,regnum:stack-pointer ,regnum:first-arg (@PCR ,gc-label))
	  (NOP ())))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (make-external-label (continuation-code-word internal-label)
		       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   code:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label procedure)))
    (LAP (ENTRY-POINT ,external-label)
	 (EQUATE ,external-label ,internal-label)
	 ,@(simple-procedure-header expression-code-word
				    internal-label
				    code:compiler-interrupt-ic-procedure)))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (LAP (EQUATE ,(rtl-procedure/external-label rtl-proc) ,internal-label)
	 ,@((if (rtl-procedure/dynamic-link? rtl-proc)
		dlink-procedure-header 
		(lambda (code-word label)
		  (simple-procedure-header code-word label
					   code:compiler-interrupt-procedure)))
	    (internal-procedure-code-word rtl-proc)
	    internal-label))))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(rtl-procedure/external-label (label->object internal-label))
	       ,internal-label)
       ,@(simple-procedure-header (make-procedure-code-word min max)
				  internal-label
				  code:compiler-interrupt-procedure)))

;;;; Closures.  These two statements are intertwined:

(define-rule statement
  ;; This depends on the following facts:
  ;; 1- TC_COMPILED_ENTRY is a multiple of two.
  ;; 2- all the top 6 bits in a data address are 0 except the quad bit
  ;; 3- type codes are 6 bits long.
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry				; Used only if entries may not be word-aligned.
  (if (zero? nentries)
      (error "Closure header for closure with no entries!"
	     internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label rtl-proc)))
      (LAP (LABEL ,gc-label)
	   ,@(invoke-interface code:compiler-interrupt-closure)
	   ,@(make-external-label
	      (internal-procedure-code-word rtl-proc)
	      external-label)
	   ;; This code must match the code and count in microcode/cmpint2.h
	   (DEP () 0 31 2 ,regnum:ble-return)
	   ,@(address->entry regnum:ble-return)
	   (STWM () ,regnum:ble-return (OFFSET -4 0 22))
	   (LABEL ,internal-label)
	   ,@(interrupt-check gc-label)))))

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
	 (STWM () ,regnum:first-arg (OFFSET 4 0 ,regnum:free-pointer))
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
	 (STWM () ,target (offset 4 0 ,regnum:free-pointer))
	 ;; First entry point is result.
	 ,@(load-offset 4 regnum:free-pointer target)
	 ,@(generate-entries 12 entries)))))

;; Magic for compiled entries.

(define compiled-entry-type-im5
  (let* ((qr (integer-divide (ucode-type compiled-entry) 2))
	 (immed (integer-divide-quotient qr)))
    (if (or (not (= scheme-type-width 6))
	    (not (zero? (integer-divide-remainder qr)))
	    (not (<= 0 immed #x1F)))
	(error "HPPA RTL rules3: closure header rule assumptions violated!"))
    (if (<= immed #x0F)
	immed
	(- immed #x20))))

(define-integrable (address->entry register)
  (LAP (DEPI () ,compiled-entry-type-im5 4 5 ,register)))

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

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  ;; Calls the linker
  (LAP (LDW () ,reg:environment 2)
       ,@(load-pc-relative-address environment-label 1)
       (STW () 2 (OFFSET 0 0 1))
       ,@(load-pc-relative-address *block-label* regnum:second-arg)
       ,@(load-pc-relative-address free-ref-label regnum:third-arg)
       ,@(load-immediate n-sections regnum:fourth-arg)
       ,@(invoke-interface-ble code:compiler-link)
       ,@(make-external-label (continuation-code-word false)
			      (generate-label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  ;; Link all of the top level procedures within the file
  (LAP ,@(load-pc-relative code-block-label regnum:second-arg)
       ,@(object->address regnum:second-arg)
       (LDW () ,reg:environment 2)
       ,@(load-offset environment-offset regnum:second-arg 1)
       (STW () 2 (OFFSET 0 0 1))
       ,@(load-offset free-ref-offset regnum:second-arg regnum:third-arg)
       ,@(load-immediate n-sections regnum:fourth-arg)
       ,@(invoke-interface-ble code:compiler-link)
       ,@(make-external-label (continuation-code-word false)
			      (generate-label))))

(define (generate/constants-block constants references assignments
				  uuo-links global-links static-vars)
  (let ((constant-info
	 (declare-constants 0 (transmogrifly uuo-links)
	   (declare-constants 1 references
	     (declare-constants 2 assignments
	       (declare-constants 3 (transmogrifly global-links)
		 (declare-constants false
		     (map (lambda (pair)
			    (cons false (cdr pair)))
			  static-vars)
		   (declare-constants false constants
		     (cons false (LAP))))))))))
    (let ((free-ref-label (car constant-info))
	  (constants-code (cdr constant-info))
	  (debugging-information-label (allocate-constant-label))
	  (environment-label (allocate-constant-label))
	  (n-sections
	   (+ (if (null? uuo-links) 0 1)
	      (if (null? references) 0 1)
	      (if (null? assignments) 0 1)
	      (if (null? global-links) 0 1))))
      (values
       (LAP ,@constants-code
	    ;; Place holder for the debugging info filename
	    (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	    ;; Place holder for the load time environment if needed
	    (SCHEME-OBJECT ,environment-label
			   ,(if (null? free-ref-label) 0 'ENVIRONMENT)))
       environment-label
       free-ref-label
       n-sections))))

(define (declare-constants tag constants info)
  (define (inner constants)
    (if (null? constants)
	(cdr info)
	(let ((entry (car constants)))
	  (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
	       ,@(inner (cdr constants))))))
  (if (and tag (not (null? constants)))
      (let ((label (allocate-constant-label)))
	(cons label
	      (inner
	       `((,(let ((datum (length constants)))
		     (if (> datum #xffff)
			 (error "datum too large" datum))
		     (+ (* tag #x10000) datum))
		  . ,label)
		 ,@constants))))
      (cons (car info) (inner constants))))

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

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
