#| -*-Scheme-*-

$Id: rules3.scm,v 1.21 2002/11/20 19:45:53 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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
	 (LW ,temp (OFFSET 0 ,regnum:stack-pointer))
	 (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)
	 ,@(object->address temp temp)
	 (JR ,temp)
	 (NOP))))			; DELAY SLOT

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (ADDI ,regnum:second-arg ,regnum:scheme-to-interface -56)
       ,@(let ((regs (get-immediate-aliases frame-size)))
	   (cond ((not (null? regs))
		  (LAP (JR ,regnum:second-arg)
		       ,@(if (memv regnum:third-arg regs)
			     (LAP (NOP))
			     (LAP (ADD ,regnum:third-arg 0 ,(car regs))))))
		 ((fits-in-16-bits-signed? frame-size)
		  (LAP (JR ,regnum:second-arg)
		       (ADDIU ,regnum:third-arg 0 ,frame-size)))
		 ((fits-in-16-bits-unsigned? frame-size)
		  (LAP (JR ,regnum:second-arg)
		       (ORI ,regnum:third-arg 0 ,frame-size)))
		 ((top-16-bits-only? frame-size)
		  (LAP (JR ,regnum:second-arg)
		       (LUI ,regnum:third-arg ,(top-16-bits frame-size))))
		 (else
		  (LAP (LUI ,regnum:third-arg ,(top-16-bits frame-size))
		       (JR ,regnum:second-arg)
		       (ORI ,regnum:third-arg
			    ,regnum:third-arg
			    ,(bottom-16-bits frame-size))))))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		;ignore
  (LAP ,@(clear-map!)
       (BGEZ 0 (@PCR ,label))
       (NOP)))				; DELAY SLOT

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		;ignore
  ;; It expects the procedure at the top of the stack
  (pop-return))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				;ignore
  (let* ((clear-second-arg (clear-registers! regnum:second-arg))
	 (load-second-arg
	  (load-pc-relative-address regnum:second-arg 'CODE label)))
    (LAP ,@clear-second-arg
	 ,@load-second-arg
	 ,@(clear-map!)
	 ,@(load-immediate regnum:third-arg number-pushed #F)
	 ,@(invoke-interface code:compiler-lexpr-apply))))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into second-arg
  (LAP ,@(clear-map!)
       (LW ,regnum:second-arg (OFFSET 0 ,regnum:stack-pointer))
       (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)
       ,@(object->address regnum:second-arg regnum:second-arg)
       ,@(load-immediate regnum:third-arg number-pushed #F)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (BGEZ 0 (@PCR ,(free-uuo-link-label name frame-size)))
       (NOP)))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (BGEZ 0 (@PCR ,(global-uuo-link-label name frame-size)))
       (NOP)))				; DELAY SLOT

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (? extension register-expression))
  continuation				;ignore
  (let* ((clear-third-arg (clear-registers! regnum:third-arg))
	 (load-third-arg
	  (load-pc-relative-address regnum:third-arg 'CODE *block-label*)))
    (LAP ,@clear-third-arg
	 ,@load-third-arg
	 ,@(load-interface-args! extension false false false)
	 ,@(load-immediate regnum:fourth-arg frame-size #F)
	 ,@(invoke-interface code:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (? environment register-expression)
		     (? name))
  continuation				;ignore
  (LAP ,@(load-interface-args! environment false false false)
       ,@(load-constant regnum:third-arg name #F #F)
       ,@(load-immediate regnum:fourth-arg frame-size #F)
       ,@(invoke-interface code:compiler-lookup-apply)))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (cond ((eq? primitive compiled-error-procedure)
	 (LAP ,@(clear-map!)
	      ,@(load-immediate regnum:second-arg frame-size #F)
	      ,@(invoke-interface code:compiler-error)))
	((eq? primitive (ucode-primitive set-interrupt-enables!))
	 (LAP ,@(clear-map!)
	      (ADDI ,regnum:assembler-temp ,regnum:scheme-to-interface -48)
	      (JR ,regnum:assembler-temp)
	      (NOP)))
	(else
	 (let* ((clear-second-arg (clear-registers! regnum:second-arg))
		(load-second-arg
		 (load-pc-relative regnum:second-arg
				   'CONSTANT
				   (constant->label primitive)
				   false)))
	   (LAP ,@clear-second-arg
		,@load-second-arg
		,@(clear-map!)
		,@(let ((arity (primitive-procedure-arity primitive)))
		    (cond ((not (negative? arity))
			   (invoke-interface code:compiler-primitive-apply))
			  ((= arity -1)
			   (LAP ,@(load-immediate regnum:assembler-temp
						   (-1+ frame-size)
						   #F)
				(SW ,regnum:assembler-temp
				    ,reg:lexpr-primitive-arity)
				,@(invoke-interface
				   code:compiler-primitive-lexpr-apply)))
			  (else
			   ;; Unknown primitive arity.  Go through apply.
			   (LAP ,@(load-immediate regnum:third-arg
						  frame-size
						  #F)
				,@(invoke-interface
				   code:compiler-apply))))))))))

(let-syntax
    ((define-special-primitive-invocation
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE-RULE STATEMENT
	     (INVOCATION:SPECIAL-PRIMITIVE
	      (? FRAME-SIZE)
	      (? CONTINUATION)
	      ,(make-primitive-procedure (cadr form) #t))
	     FRAME-SIZE CONTINUATION
	     ,(list 'LAP
		    (list 'UNQUOTE-SPLICING '(CLEAR-MAP!))
		    (list 'UNQUOTE-SPLICING
			  `(INVOKE-INTERFACE
			    ,(close-syntax (symbol-append 'CODE:COMPILER-
							  (cadr form))
					   environment)))))))))
  (define-special-primitive-invocation &+)
  (define-special-primitive-invocation &-)
  (define-special-primitive-invocation &*)
  (define-special-primitive-invocation &/)
  (define-special-primitive-invocation &=)
  (define-special-primitive-invocation &<)
  (define-special-primitive-invocation &>)
  (define-special-primitive-invocation 1+)
  (define-special-primitive-invocation -1+)
  (define-special-primitive-invocation zero?)
  (define-special-primitive-invocation positive?)
  (define-special-primitive-invocation negative?))

;;;; Invocation Prefixes

;;; (INVOCATION-PREFIX:MOVE-FRAME-UP frame-size address)

;;; Move the topmost <frame-size> words of the stack downward so that
;;; the bottommost of these words is at location <address>, and set
;;; the stack pointer to the topmost of the moved words.  That is,
;;; discard the words between <address> and SP+<frame-size>, close the
;;; resulting gap by shifting down the words from above the gap, and
;;; adjust SP to point to the new topmost word.

(define-rule statement
  ;; Move up 0 words back to top of stack : a No-Op
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 3))
  (LAP))

(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER 11))
  (generate/move-frame-up frame-size
    (lambda (reg) (LAP (ADD ,reg 0 ,regnum:dynamic-link)))))

(define-rule statement
  ;; Move <frame-size> words back to SP+offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size)
   (OFFSET-ADDRESS (REGISTER 3)
		   (MACHINE-CONSTANT (? offset))))
  (let ((how-far (* 4 (- offset frame-size))))
    (cond ((zero? how-far)
	   (LAP))
	  ((negative? how-far)
	   (error "invocation-prefix:move-frame-up: bad specs"
		  frame-size offset))
	  ((zero? frame-size)
	   (add-immediate how-far regnum:stack-pointer regnum:stack-pointer))
	  ((= frame-size 1)
	   (let ((temp (standard-temporary!)))
	     (LAP (LW ,temp (OFFSET 0 ,regnum:stack-pointer))
		  (ADDI ,regnum:stack-pointer ,regnum:stack-pointer ,how-far)
		  (STW ,temp (OFFSET 0 ,regnum:stack-pointer)))))
	  ((= frame-size 2)
	   (let ((temp1 (standard-temporary!))
		 (temp2 (standard-temporary!)))
	     (LAP (LW ,temp1 (OFFSET 0 ,regnum:stack-pointer))
		  (LW ,temp2 (OFFSET 4 ,regnum:stack-pointer))
		  (ADDI ,regnum:stack-pointer ,regnum:stack-pointer ,how-far)
		  (SW ,temp1 (OFFSET 0 ,regnum:stack-pointer))
		  (SW ,temp2 (OFFSET 4 ,regnum:stack-pointer)))))
	  (else
	   (generate/move-frame-up frame-size
	     (lambda (reg)
	       (add-immediate (* 4 offset) regnum:stack-pointer reg)))))))

(define-rule statement
  ;; Move <frame-size> words back to base virtual register + offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size)
   (OFFSET-ADDRESS (REGISTER (? base))
		   (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (not (= base 3)))
  (generate/move-frame-up frame-size
    (lambda (reg)
      (add-immediate (* 4 offset) (standard-source! base) reg))))

(define (generate/move-frame-up frame-size destination-generator)
  (let ((temp (standard-temporary!)))
    (LAP ,@(destination-generator temp)
	 ,@(generate/move-frame-up* frame-size temp))))

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
				  (REGISTER 11))
  (if (and (zero? frame-size)
	   (= source regnum:stack-pointer))
      (LAP)
      (let ((env-reg (standard-move-to-temporary! source))
	    (label (generate-label)))
	(LAP (SLTU ,regnum:assembler-temp ,env-reg ,regnum:dynamic-link)
	     (BNE 0 ,regnum:assembler-temp (@PCR ,label))
	     (NOP)
	     (ADD ,env-reg 0 ,regnum:dynamic-link)
	     (LABEL ,label)
	     ,@(generate/move-frame-up* frame-size env-reg)))))

(define (generate/move-frame-up* frame-size destination)
  ;; Destination is guaranteed to be a machine register number; that
  ;; register has the destination base address for the frame.  The stack
  ;; pointer is reset to the top end of the copied area.
  (LAP ,@(case frame-size
	   ((0)
	    (LAP))
	   ((1)
	    (let ((temp (standard-temporary!)))
	      (LAP (LW ,temp (OFFSET 0 ,regnum:stack-pointer))
		   (ADDI ,destination ,destination -4)
		   (SW ,temp (OFFSET 0 ,destination)))))
	   (else
	    (let ((from (standard-temporary!))
		  (temp1 (standard-temporary!))
		  (temp2 (standard-temporary!)))
	      (LAP ,@(add-immediate (* 4 frame-size) regnum:stack-pointer from)
		   ,@(if (<= frame-size 3)
			 ;; This code can handle any number > 1
			 ;; (handled above), but we restrict it to 3
			 ;; for space reasons.
			 (let loop ((n frame-size))
			   (case n
			     ((0)
			      (LAP))
			     ((3)
			      (let ((temp3 (standard-temporary!)))
				(LAP (LW ,temp1 (OFFSET -4 ,from))
				     (LW ,temp2 (OFFSET -8 ,from))
				     (LW ,temp3 (OFFSET -12 ,from))
				     (ADDI ,from ,from -12)
				     (SW ,temp1 (OFFSET -4 ,destination))
				     (SW ,temp2 (OFFSET -8 ,destination))
				     (SW ,temp3 (OFFSET -12 ,destination))
				     (ADDI ,destination ,destination -12))))
			     (else
			      (LAP (LW ,temp1 (OFFSET -4 ,from))
				   (LW ,temp2 (OFFSET -8 ,from))
				   (ADDI ,from ,from -8)
				   (SW ,temp1 (OFFSET  -4 ,destination))
				   (SW ,temp2 (OFFSET -8 ,destination))
				   (ADDI ,destination ,destination -8)
				   ,@(loop (- n 2))))))
			 (let ((label (generate-label)))
			   (LAP ,@(load-immediate temp2 frame-size #F)
				(LABEL ,label)
				(LW ,temp1 (OFFSET -4 ,from))
				(ADDI ,from ,from -4)
				(ADDI ,temp2 ,temp2 -1)
				(ADDI ,destination ,destination -4)
				(BNE ,temp2 0 (@PCR ,label))
				(SW ,temp1 (OFFSET 0 ,destination)))))))))
       (ADD ,regnum:stack-pointer 0 ,destination)))

;;;; External Labels

(define (make-external-label code label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (EXTERNAL-LABEL ,code (@PCR ,label))
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
	 ,@(link-to-interface code)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define (dlink-procedure-header code-word label)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 (ADD ,regnum:third-arg 0 ,regnum:dynamic-link)
	 ,@(link-to-interface code:compiler-interrupt-dlink)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define (interrupt-check label gc-label)
  (if (not (let ((object (label->object label)))
	     (and (rtl-procedure? object)
		  (not (rtl-procedure/stack-leaf? object))
		  compiler:generate-stack-checks?)))
      (LAP (SLT ,regnum:assembler-temp ,regnum:memtop ,regnum:free)
	   (BNE ,regnum:assembler-temp 0 (@PCR ,gc-label))
	   (LW ,regnum:memtop ,reg:memtop))
      (LAP (LW ,regnum:first-arg ,reg:stack-guard)
	   (SLT ,regnum:assembler-temp ,regnum:memtop ,regnum:free)
	   (BNE ,regnum:assembler-temp 0 (@PCR ,gc-label))
	   (SLT ,regnum:assembler-temp ,regnum:stack-pointer ,regnum:first-arg)
	   (BNE ,regnum:assembler-temp 0 (@PCR ,gc-label))
	   (LW ,regnum:memtop ,reg:memtop))))

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

;;;; Closures.

;; Magic for compiled entries.

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry			; ignored -- non-RISCs only
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
	   ;; Code below here corresponds to code and count in cmpint2.h
	   ,@(fluid-let ((*register-map* *register-map*))
	       ;; Don't cache type constant here, because it won't be
	       ;; in the register if the closure is entered from the
	       ;; internal label.
	       (deposit-type-address (ucode-type compiled-entry)
				     regnum:linkage
				     regnum:linkage))
	   (ADDI ,regnum:stack-pointer ,regnum:stack-pointer -4)
	   (SW ,regnum:linkage (OFFSET 0 ,regnum:stack-pointer))
	   (LABEL ,internal-label)
	   ,@(interrupt-check internal-label gc-label)))))

(define (build-gc-offset-word offset code-word)
  (let ((encoded-offset (quotient offset 2)))
    (if (eq? endianness 'LITTLE)
	(+ (* encoded-offset #x10000) code-word)
	(+ (* code-word #x10000) encoded-offset))))

(define (closure-bump-size nentries nvars)
  (* (* 4 closure-entry-size)
     (1+ (quotient (+ (+ nvars (-1+ (* closure-entry-size nentries)))
		      (-1+ closure-entry-size))
		   closure-entry-size))))

(define (closure-test-size nentries nvars)
  (* 4
     (+ nvars
	(-1+ (* nentries closure-entry-size)))))

(define (cons-closure target label min max nvars)
  ;; Invoke an out-of-line handler to set up the closure's entry point.
  ;; Arguments:
  ;; - GR31: "Return address"
  ;;   GR31 points to a manifest closure header word, followed by a
  ;;    two-word closure descriptor, followed by the actual
  ;;    instructions to return to.
  ;;   The first word of the descriptor is the format+gc-offset word of
  ;;    the generated closure.
  ;;   The second word is the PC-relative JAL instruction.
  ;;    It is transformed into an absolute instruction by adding the shifted
  ;;    "return address".
  ;; - GR4: Value to compare to closure free.
  ;; - GR5: Increment for closure free.
  ;; Returns closure in regnum:first-arg (GR4)
  (rtl-target:=machine-register! target regnum:first-arg)
  (require-register! regnum:second-arg)
  (require-register! regnum:fourth-arg)
  (let ((label-arg (generate-label)))
    (LAP (ADDI ,regnum:second-arg ,regnum:scheme-to-interface -72)
	 (ADDI ,regnum:first-arg ,regnum:closure-free
	       ,(closure-test-size 1 nvars))
	 (JALR 31 ,regnum:second-arg)
	 (ADDI ,regnum:second-arg 0 ,(closure-bump-size 1 nvars))
       (LABEL ,label-arg)
         (LONG U ,(make-non-pointer-literal (ucode-type manifest-closure)
					    (+ closure-entry-size nvars)))
	 (LONG U ,(build-gc-offset-word 8 (make-procedure-code-word min max)))
	 (LONG U
	       (+ #x0c000000		; JAL opcode
		  (/ (- ,(rtl-procedure/external-label (label->object label))
			,label-arg)
		     4))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? nvars)))
  (cons-closure target procedure-label min max nvars))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? nvars) (? entries)))
  ;; entries is a vector of all the entry points
  (case nentries
    ((0)
     (let ((dest (standard-target! target))
	   (temp (standard-temporary!)))
       (LAP (ADD ,dest 0 ,regnum:free)
	    ,@(load-immediate
	       temp
	       (make-non-pointer-literal (ucode-type manifest-vector) nvars)
	       #T)
	    (SW ,temp (OFFSET 0 ,regnum:free))
	    (ADDI ,regnum:free ,regnum:free ,(* 4 (+ nvars 1))))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (cons-closure target (car entry) (cadr entry) (caddr entry) nvars)))
    (else
     (cons-multiclosure target nentries nvars (vector->list entries)))))

(define (cons-multiclosure target nentries nvars entries)
  ;; Invoke an out-of-line handler to set up the closure's entry points.
  ;; Arguments:
  ;; - GR31: "Return address"
  ;;   GR31 points to a manifest closure header word, followed by
  ;;   nentries two-word structures, followed by the actual
  ;;   instructions to return to.
  ;;   The first word of each descriptor is the format+gc-offset word of
  ;;    the corresponding entry point of the generated closure.
  ;;   The second word is the PC-relative JAL instruction.
  ;;    It is transformed into an absolute instruction by adding the shifted
  ;;    "return address".
  ;; - GR4: Value to compare to closure free.
  ;; - GR5: Increment for closure free.
  ;; - GR6: number of entries.
  ;; Returns closure in regnum:first-arg (GR4).
  (rtl-target:=machine-register! target regnum:first-arg)
  (require-register! regnum:second-arg)
  (require-register! regnum:third-arg)
  (require-register! regnum:fourth-arg)
  (let ((label-arg (generate-label)))
    (LAP (ADDI ,regnum:third-arg ,regnum:scheme-to-interface -64)
	 (ADDI ,regnum:first-arg ,regnum:closure-free
	       ,(closure-test-size nentries nvars))
	 (ADDI ,regnum:second-arg 0 ,(closure-bump-size nentries nvars))
	 (JALR 31 ,regnum:third-arg)
   	 (ADDI ,regnum:third-arg 0 ,nentries)
       (LABEL ,label-arg)
         (LONG U ,(make-non-pointer-literal (ucode-type manifest-closure)
					    (+ 1
					       (* nentries closure-entry-size)
					       nvars)))
         ,@(let expand ((offset 12) (entries entries))
	     (if (null? entries)
		 (LAP)
		 (let ((entry (car entries)))
		   (LAP 
		    (LONG U ,(build-gc-offset-word
			      offset
			      (make-procedure-code-word (cadr entry)
							(caddr entry))))
		    (LONG U
			  (+ #x0c000000	; JAL opcode
			     (/ (- ,(rtl-procedure/external-label
				     (label->object (car entry)))
				   ,label-arg)
				4)))
		    ,@(expand (+ offset (* 4 closure-entry-size))
			      (cdr entries)))))))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  ;; Calls the linker
  ;; On MIPS, regnum:first-arg is used as a temporary here since
  ;; load-pc-relative-address uses the assembler temporary.
  (in-assembler-environment (empty-register-map)
			    (list regnum:first-arg regnum:second-arg
				  regnum:third-arg regnum:fourth-arg)
    (lambda ()
      (let* ((i1
	      (load-pc-relative-address regnum:second-arg
					'CONSTANT environment-label))
	     (i2 (load-pc-relative-address regnum:third-arg
					   'CODE *block-label*))
	     (i3 (load-pc-relative-address regnum:fourth-arg
					   'CONSTANT free-ref-label)))
	(LAP
	 ;; Grab interp's env. and store in code block at environment-label
	 (LW ,regnum:first-arg ,reg:environment)
	 ,@i1
	 (SW ,regnum:first-arg (OFFSET 0 ,regnum:second-arg))
	 ;; Now invoke the linker
	 ;; (arg1 is return address, supplied by interface)
	 ,@i2
	 ,@i3
	 ,@(load-immediate regnum:first-arg n-sections #F)
	 (SW ,regnum:first-arg (OFFSET 16 ,regnum:C-stack-pointer))
	 ,@(link-to-interface code:compiler-link)
	 ,@(make-external-label (continuation-code-word false)
				(generate-label)))))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  ;; Link all of the top level procedures within the file
  (in-assembler-environment (empty-register-map)
			    (list regnum:first-arg regnum:second-arg
				  regnum:third-arg regnum:fourth-arg)
    (lambda ()
      (LAP ,@(load-pc-relative regnum:third-arg 'CODE code-block-label false)
	   (LW ,regnum:fourth-arg ,reg:environment)
	   ,@(object->address regnum:third-arg regnum:third-arg)
	   ,@(add-immediate environment-offset
			    regnum:third-arg
			    regnum:second-arg)
	   (SW ,regnum:fourth-arg (OFFSET 0 ,regnum:second-arg))
	   ,@(add-immediate free-ref-offset regnum:third-arg regnum:fourth-arg)
	   ,@(load-immediate regnum:first-arg n-sections #F)
	   (SW ,regnum:first-arg (OFFSET 16 ,regnum:C-stack-pointer))
	   ,@(link-to-interface code:compiler-link)
	   ,@(make-external-label (continuation-code-word false)
				  (generate-label))))))

(define (in-assembler-environment map needed-registers thunk)
  (fluid-let ((*register-map* map)
	      (*prefix-instructions* (LAP))
	      (*suffix-instructions* (LAP))
	      (*needed-registers* needed-registers))
    (let ((instructions (thunk)))
      (LAP ,@*prefix-instructions*
	   ,@instructions
	   ,@*suffix-instructions*))))

(define (generate/constants-block constants references assignments uuo-links
				  global-links static-vars)
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
	; produces ((name . label) (0 . label) ... (frame-size . label) ...)
        ; where the (0 . label) is repeated to fill out the size required
        ; as specified in machin.scm
	`((,name . ,(cdar assoc))		; uuo-label
	  ,@(let loop ((count (max 0 (- execute-cache-size 2))))
	      (if (= count 0)
		  '()
		  (cons `(0 . ,(allocate-constant-label))
			(loop (- count 1)))))
	  (,(caar assoc) .			; frame-size
	   ,(allocate-constant-label))
	  ,@(inner name (cdr assoc)))))
  (if (null? uuos)
      '()
      ;; caar is name, cdar is alist of frame sizes
      (inner (caar uuos) (cdar uuos))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
