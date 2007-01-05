#| -*-Scheme-*-

$Id: rules3.scm,v 1.13 2007/01/05 15:33:03 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Invocations and Entries (Alpha)
;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  (pop-return))

(define (pop-return)
  (let ((temp (standard-temporary!)))
    (LAP ,@(clear-map!)
	 (LDQ ,temp (OFFSET 0 ,regnum:stack-pointer))
	 (ADDQ ,regnum:stack-pointer (& 8) ,regnum:stack-pointer)
	 (XOR ,temp ,regnum:compiled-entry-type-bits ,temp)
	 ; XOR instead of ,@(object->address temp temp)
	 (RET ,temp))))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,@(load-immediate regnum:second-arg frame-size #F)
       (LDQ ,regnum:first-arg (OFFSET 0 ,regnum:stack-pointer))
       (ADDQ ,regnum:stack-pointer (& 8) ,regnum:stack-pointer)
       ,@(invoke-interface code:compiler-apply)))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		;ignore
  (LAP ,@(clear-map!)
       (BR ,regnum:came-from (@PCR ,label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		;ignore
  ;; It expects the procedure at the top of the stack
  (pop-return))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				;ignore
  (let* ((clear-first-arg (clear-registers! regnum:first-arg))
	 (load-first-arg
	  (load-pc-relative-address regnum:first-arg 'CODE label)))
    (LAP ,@clear-first-arg
	 ,@load-first-arg
	 ,@(clear-map!)
	 ,@(load-immediate regnum:second-arg number-pushed #F)
	 ,@(invoke-interface code:compiler-lexpr-apply))))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into first-arg
  (LAP ,@(clear-map!)
       (LDQ ,regnum:first-arg (OFFSET 0 ,regnum:stack-pointer))
       (ADDQ ,regnum:stack-pointer (& 8) ,regnum:stack-pointer)
       ,@(object->address regnum:first-arg regnum:first-arg)
       ,@(load-immediate regnum:second-arg number-pushed #F)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (BR ,regnum:came-from
	   (OFFSET 4 (@PCR ,(free-uuo-link-label name frame-size))))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (BR ,regnum:came-from
	   (OFFSET 4 (@PCR ,(global-uuo-link-label name frame-size))))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (? extension register-expression))
  continuation				;ignore
  (let* ((clear-second-arg (clear-registers! regnum:second-arg))
	 (load-second-arg
	  (load-pc-relative-address regnum:second-arg 'CODE *block-label*)))
    (LAP ,@clear-second-arg
	 ,@load-second-arg
	 ,@(load-interface-args! extension false false false)
	 ,@(load-immediate regnum:third-arg frame-size #F)
	 ,@(invoke-interface code:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (? environment register-expression)
		     (? name))
  continuation				;ignore
  (LAP ,@(load-interface-args! environment false false false)
       ,@(load-constant regnum:second-arg name #F)
       ,@(load-immediate regnum:third-arg frame-size #F)
       ,@(invoke-interface code:compiler-lookup-apply)))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (if (eq? primitive compiled-error-procedure)
      (LAP ,@(clear-map!)
	   ,@(load-immediate regnum:first-arg frame-size #F)
	   ,@(invoke-interface code:compiler-error))
      (let* ((clear-first-arg (clear-registers! regnum:first-arg))
	     (load-first-arg
	      (load-pc-relative regnum:first-arg
				'CONSTANT
				(constant->label primitive))))
	(LAP ,@clear-first-arg
	     ,@load-first-arg
	     ,@(clear-map!)
	     ,@(let ((arity (primitive-procedure-arity primitive)))
		 (cond ((not (negative? arity))
			(invoke-interface code:compiler-primitive-apply))
		       ((= arity -1)
			(LAP ,@(load-immediate regnum:assembler-temp
						(-1+ frame-size)
						#F)
			     (STQ ,regnum:assembler-temp
				  ,reg:lexpr-primitive-arity)
			     ,@(invoke-interface
				code:compiler-primitive-lexpr-apply)))
		       (else
			;; Unknown primitive arity.  Go through apply.
			(LAP ,@(load-immediate regnum:second-arg frame-size #F)
			     ,@(invoke-interface code:compiler-apply)))))))))

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
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER (? stack)))
  (QUALIFIER (= stack regnum:stack-pointer))
  (LAP))

(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? dlink)))
  (QUALIFIER (= dlink regnum:dynamic-link))
  (generate/move-frame-up frame-size
    (lambda (reg) (LAP (COPY ,regnum:dynamic-link ,reg)))))

(define-rule statement
  ;; Move <frame-size> words back to SP+offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size) (OFFSET-ADDRESS (REGISTER (? stack))
				  (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (= stack regnum:stack-pointer))
  (let ((how-far (* 8 (- offset frame-size))))
    (cond ((zero? how-far)
	   (LAP))
	  ((negative? how-far)
	   (error "invocation-prefix:move-frame-up: bad specs"
		  frame-size offset))
	  ((zero? frame-size)
	   (add-immediate how-far regnum:stack-pointer regnum:stack-pointer))
	  ((= frame-size 1)
	   (let ((temp (standard-temporary!)))
	     (LAP (LDQ ,temp (OFFSET 0 ,regnum:stack-pointer))
		  (ADDQ ,regnum:stack-pointer (& ,how-far)
			,regnum:stack-pointer)
		  (STQ ,temp (OFFSET 0 ,regnum:stack-pointer)))))
	  ((= frame-size 2)
	   (let ((temp1 (standard-temporary!))
		 (temp2 (standard-temporary!)))
	     (LAP (LDQ ,temp1 (OFFSET 0 ,regnum:stack-pointer))
		  (LDQ ,temp2 (OFFSET 8 ,regnum:stack-pointer))
		  (ADDQ ,regnum:stack-pointer (& ,how-far)
			,regnum:stack-pointer)
		  (STQ ,temp1 (OFFSET 0 ,regnum:stack-pointer))
		  (STQ ,temp2 (OFFSET 8 ,regnum:stack-pointer)))))
	  (else
	   (generate/move-frame-up frame-size
	     (lambda (reg)
	       (add-immediate (* 8 offset) regnum:stack-pointer reg)))))))

(define-rule statement
  ;; Move <frame-size> words back to base virtual register + offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
   (OFFSET-ADDRESS (REGISTER (? base)) (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (not (= base 20)))
  (generate/move-frame-up frame-size
    (lambda (reg)
      (add-immediate (* 8 offset) (standard-source! base) reg))))

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
				  (REGISTER (? dlink)))
  (QUALIFIER (= dlink regnum:dynamic-link))
  (if (and (zero? frame-size)
	   (= source regnum:stack-pointer))
      (LAP)
      (let ((env-reg (standard-move-to-temporary! source)))
	(LAP (CMPULT ,env-reg ,regnum:dynamic-link ,regnum:assembler-temp)
	     (CMOVEQ ,regnum:assembler-temp ,regnum:dynamic-link ,env-reg)
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
	      (LAP (LDQ ,temp (OFFSET 0 ,regnum:stack-pointer))
		   (SUBQ ,destination (& 8) ,destination)
		   (STQ ,temp (OFFSET 0 ,destination)))))
	   (else
	    (let ((from (standard-temporary!))
		  (temp1 (standard-temporary!))
		  (temp2 (standard-temporary!)))
	      (LAP ,@(add-immediate (* 8 frame-size) regnum:stack-pointer from)
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
				(LAP (LDQ ,temp1 (OFFSET -8 ,from))
				     (LDQ ,temp2 (OFFSET -16 ,from))
				     (LDQ ,temp3 (OFFSET -24 ,from))
				     (SUBQ ,from (& 24) ,from)
				     (STQ ,temp1 (OFFSET -8 ,destination))
				     (STQ ,temp2 (OFFSET -16 ,destination))
				     (STQ ,temp3 (OFFSET -24 ,destination))
				     (SUBQ ,destination (& 24) ,destination))))
			     (else
			      (LAP (LDQ ,temp1 (OFFSET -8 ,from))
				   (LDQ ,temp2 (OFFSET -16 ,from))
				   (SUBQ ,from (& 16) ,from)
				   (STQ ,temp1 (OFFSET  -8 ,destination))
				   (STQ ,temp2 (OFFSET -16 ,destination))
				   (SUBQ ,destination (& 16) ,destination)
				   ,@(loop (- n 2))))))
			 (let ((label (generate-label)))
			   (LAP ,@(load-immediate temp2 frame-size #F)
				(LABEL ,label)
				(LDQ ,temp1 (OFFSET -8 ,from))
				(SUBQ ,from (& 8) ,from)
				(SUBQ ,temp2 (& 1) ,temp2)
				(SUBQ ,destination (& 8) ,destination)
				(STQ ,temp1 (OFFSET 0 ,destination))
				(BNE ,temp2 (@PCR ,label)))))))))
       (COPY ,destination ,regnum:stack-pointer)))

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
    (LAP
      (LABEL ,gc-label)
	 ,@(link-to-interface code)
      ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define (dlink-procedure-header code-word label)
  (let ((gc-label (generate-label)))    
    (LAP
      (LABEL ,gc-label)
	 (COPY ,regnum:dynamic-link ,regnum:second-arg)
	 ,@(link-to-interface code:compiler-interrupt-dlink)
      ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define (interrupt-check procedure-label gc-label)
  ;; Code sequence 2 in cmpint-alpha.h
  ;; Interrupt/Stack checks always done in line.
  (let ((Interrupt (generate-label))
	(temp (standard-temporary!)))
    ;; The following trick makes branch prediction work.
    ;; The interrupt branch (taken very rarely) is guaranteed to
    ;; be a forward branch, so it is predicted NOT taken.
    (add-end-of-block-code!		
     (lambda ()
       (LAP (LABEL ,Interrupt)
	      (BR ,regnum:came-from (@PCR ,gc-label)))))
    (if (not (let ((object (label->object procedure-label)))
	       (and (rtl-procedure? object)
		    (not (rtl-procedure/stack-leaf? object))
		    compiler:generate-stack-checks?)))
	(LAP (CMPLT ,regnum:free ,regnum:memtop ,temp)
	     (LDQ ,regnum:memtop ,reg:memtop)
	     (BEQ ,temp (@PCR ,Interrupt)))
	(let ((temp2 (standard-temporary!)))
	  (LAP (LDQ ,temp2 ,reg:stack-guard)
	       (CMPLT ,regnum:free ,regnum:memtop ,temp)
	       (LDQ ,regnum:memtop ,reg:memtop)
	       (BEQ ,temp (@PCR ,Interrupt))
	       (CMPLE ,regnum:stack-pointer ,temp2 ,temp)
	       (BNE ,temp (@PCR ,Interrupt)))))))

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
  entry					; Ignored
  (if (zero? nentries)
      (error "Closure header for closure with no entries!"
	     internal-label))
  (let ((Interrupt (generate-label))
	(merge (generate-label))
	(interrupt-boolean (standard-temporary!))
	(stack-check?
	 (let ((object (label->object internal-label)))
	   (and (rtl-procedure? object)
		(not (rtl-procedure/stack-leaf? object))
		compiler:generate-stack-checks?))))
    (let ((stack-guard (and stack-check? (standard-temporary!))))
      ;; Interrupt/Stack checks always done in line.
      (add-end-of-block-code!
       (if (not stack-check?)
	   (lambda ()
	     (LAP
	      (LABEL ,internal-label)
	        ;; Code seq. 4 from cmpint-alpha.h
		(CMPLT ,regnum:free ,regnum:memtop ,interrupt-boolean)
		(LDQ   ,regnum:memtop ,reg:memtop)
		(BNE   ,interrupt-boolean (@PCR ,merge))
	      (LABEL ,Interrupt)
		;; Code seq. 5 from cmpint-alpha.h
		,@(invoke-interface code:compiler-interrupt-closure)))
	   (lambda ()
	     (LAP
	      (LABEL ,internal-label)
		;; Code seq. 4 from cmpint-alpha.h
	        (LDQ   ,stack-guard ,reg:stack-guard)
		(CMPLT ,regnum:free ,regnum:memtop ,interrupt-boolean)
		(LDQ   ,regnum:memtop ,reg:memtop)
		(BEQ   ,interrupt-boolean (@PCR ,Interrupt))
		(CMPLE ,regnum:stack-pointer ,stack-guard ,interrupt-boolean)
		(BEQ   ,interrupt-boolean (@PCR ,merge))
	      (LABEL ,Interrupt)
		;; Code seq. 5 from cmpint-alpha.h
		,@(invoke-interface code:compiler-interrupt-closure)))))

      (let ((rtl-proc (label->object internal-label)))
	(let ((label (rtl-procedure/external-label rtl-proc))
	      (reconstructed-closure (standard-temporary!)))
	  (if (not stack-check?)
	      (LAP
	         ;; Code seq. 3 from cmpint-alpha.h
	       ,@(make-external-label (internal-procedure-code-word rtl-proc)
				      label)
		 ;; (SUBQ ,regnum:stack-pointer (& 8) ,regnum:stack-pointer)
		 (SUBQ ,regnum:linkage (& 8) ,reconstructed-closure)
		 (CMPLT ,regnum:free ,regnum:memtop ,interrupt-boolean)
		 (LDQ ,regnum:memtop ,reg:memtop)
		 (BIS ,regnum:compiled-entry-type-bits
		      ,reconstructed-closure ,reconstructed-closure)
		 (STQ ,reconstructed-closure (OFFSET 0 ,regnum:stack-pointer))
		 (BEQ ,interrupt-boolean (@PCR ,Interrupt))
	       (LABEL ,merge))
	      (LAP
	         ;; Code seq. 3 from cmpint-alpha.h
	       ,@(make-external-label (internal-procedure-code-word rtl-proc)
				      label)
		 ;; (SUBQ ,regnum:stack-pointer (& 8) ,regnum:stack-pointer)
		 (SUBQ ,regnum:linkage (& 8) ,reconstructed-closure)
		 (LDQ ,stack-guard ,reg:stack-guard)
		 (CMPLT ,regnum:free ,regnum:memtop ,interrupt-boolean)
		 (LDQ ,regnum:memtop ,reg:memtop)
		 (BIS ,regnum:compiled-entry-type-bits
		      ,reconstructed-closure ,reconstructed-closure)
		 (STQ ,reconstructed-closure (OFFSET 0 ,regnum:stack-pointer))
		 (BEQ ,interrupt-boolean (@PCR ,Interrupt))
		 (CMPLE ,regnum:stack-pointer ,stack-guard ,interrupt-boolean)
		 (BNE ,interrupt-boolean (@PCR ,Interrupt))
	       (LABEL ,merge))))))))

(define (build-gc-offset-word offset code-word)
  (let ((encoded-offset (quotient offset 2)))
    (+ (* encoded-offset #x10000) code-word)))

(define (allocate-closure rtl-target nentries n-free-vars)
  (let ((target regnum:second-C-arg))
    (require-register! regnum:first-C-arg)
    (rtl-target:=machine-register! rtl-target target)
    (let ((total-size
	   (+ 1				; Closure header word
	      (* closure-entry-size nentries)
	      n-free-vars))
	  (limit (standard-temporary!))
	  (label (generate-label))
	  (forward-label (generate-label)))
      (add-end-of-block-code!
       (lambda ()
	 (LAP (LABEL ,forward-label)
    	      (MOVEI ,regnum:first-C-arg (& ,total-size))
	      ; second-C-arg was set up because target==second-C-arg!
	      ,@(invoke-assembly-hook assembly-hook:allocate-closure)
	      (BR ,regnum:came-from (@PCR ,label)))))
      (values
       target
       (LAP (LDA ,target (OFFSET 16 ,regnum:closure-free))
	    ;; Optional code (to reduce out-of-line calls):
	    (LDQ ,limit ,reg:closure-limit)
	    (LDA ,regnum:closure-free (OFFSET ,(* 8 total-size)
					      ,regnum:closure-free))
	    (CMPLT ,limit ,regnum:closure-free ,limit)
	    (BNE ,limit (@PCR ,forward-label))
	    ;; End of optional code -- convert BNE to BR to flush
	    (LABEL ,label)
	    ,@(with-values
		  (lambda ()
		    (immediate->register
		     (make-non-pointer-literal
		      (ucode-type manifest-closure) (- total-size 1))))
		(lambda (prefix header)
		  (LAP ,@prefix
		       (STQ ,header (OFFSET -16 ,target)))))
	    ,@(with-values
		  (lambda ()
		    (immediate->register
		     (build-gc-offset-word 0 nentries)))
		(lambda (prefix register)
		  (LAP ,@prefix
		       (STL ,register (OFFSET -8 ,target))))))))))

(define (cons-closure target label min max size)
  (with-values (lambda () (allocate-closure target 1 size))
    (lambda (target prefix)
      (let ((temp (standard-temporary!)))
	(LAP ,@prefix
	     ,@(with-values (lambda ()
			      (immediate->register
			       (build-gc-offset-word
				16 (make-procedure-code-word min max))))
		 (lambda (code reg)
		   (LAP ,@code
			(STL ,reg (OFFSET -4 ,target)))))
	     ,@(load-pc-relative-address
		temp 'CODE
		(rtl-procedure/external-label (label->object label)))
	     (STQ ,temp (OFFSET 8 ,target)))))))

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
     (let ((dest (standard-target! target))
	   (temp (standard-temporary!)))
       (LAP (COPY ,regnum:free ,dest)
	    ,@(load-immediate
	       temp
	       (make-non-pointer-literal (ucode-type manifest-vector) size)
	       #T)
	    (STQ ,temp (OFFSET 0 ,regnum:free))
	    (LDA ,regnum:free (OFFSET ,(* 8 (+ size 1))
				      ,regnum:free)))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (cons-closure target (car entry) (cadr entry) (caddr entry) size)))
    (else
     (cons-multiclosure target nentries size (vector->list entries)))))

(define (cons-multiclosure target nentries size entries)
  (with-values (lambda () (allocate-closure target nentries size))
    (lambda (target prefix)
      (let ((temp (standard-temporary!)))
	(LAP ,@prefix
	     ,@(let loop ((offset 16)
			  (entries entries))
		 (if (null? entries)
		     (LAP)
		     (let* ((entry (car entries))
			    (label (car entry))
			    (min (cadr entry))
			    (max (caddr entry)))
		       (let* ((this-value
			       (load-immediate
				temp
				(build-gc-offset-word
				 offset (make-procedure-code-word min max)) #F))
			      (this-entry
			       (load-pc-relative-address
				temp 'CODE
				(rtl-procedure/external-label
				 (label->object label)))))
			 (LAP
			  ,@this-value
			  (STL ,temp (OFFSET ,(- offset 20) ,target))
			  ,@this-entry
			  (STQ ,temp (OFFSET ,(- offset 8) ,target))
			  ,@(loop (+ offset 24)
				  (cdr entries))))))))))))

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
	      (load-pc-relative-address regnum:fourth-arg
					'CONSTANT environment-label))
	     (i2 (load-pc-relative-address regnum:second-arg
					   'CODE *block-label*))
	     (i3 (load-pc-relative-address regnum:third-arg
					   'CONSTANT free-ref-label)))
	(LAP
	 ;; Grab interp's env. and store in code block at environment-label
	 (LDQ ,regnum:first-arg ,reg:environment)
	 ,@i1
	 (STQ ,regnum:first-arg (OFFSET 0 ,regnum:fourth-arg))
	 ;; Now invoke the linker
	 ;; (arg1 is return address, supplied by interface)
	 ,@i2
	 ,@i3
	 (MOVEI ,regnum:fourth-arg (& ,n-sections))
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
      (LAP ,@(load-pc-relative regnum:second-arg 'CODE code-block-label)
	   (LDQ ,regnum:first-arg ,reg:environment) ; first-arg is a temp here
	   ,@(object->address regnum:second-arg regnum:second-arg)
	   ,@(add-immediate environment-offset
			    regnum:second-arg
			    regnum:fourth-arg) ; fourth-arg is a temp here...
	   (STQ ,regnum:first-arg (OFFSET 0 ,regnum:fourth-arg))
	   ,@(add-immediate free-ref-offset regnum:second-arg regnum:third-arg)
	   (MOVEI ,regnum:fourth-arg (& ,n-sections))
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


(define (generate/remote-links n-code-blocks code-blocks-label n-sections)
  (if (= n-code-blocks 0)
      (LAP)
      (let ((loop (generate-label))
	    (bytes (generate-label))
	    (after-bytes (generate-label)))
	(LAP 
	 ;; Push room for the block counter on the stack
	 (SUBQ ,regnum:stack-pointer (& ,address-units-per-object)
	       ,regnum:stack-pointer)
         (COPY ,regnum:zero ,regnum:first-arg)
(LABEL ,loop)
         ;; Increment block counter (into arg 2 and stack)
         (ADDQ ,regnum:first-arg (& 1) ,regnum:second-arg)
         (STQ ,regnum:second-arg (OFFSET 0 ,regnum:stack-pointer))
         ;; Load address of bytes into arg 3 and skip over them
         (BR ,regnum:third-arg (@PCR ,after-bytes))
(LABEL ,bytes)
         ;; Dump the vector of constant data here in the code stream
         ;; There is one byte per linkage block and the byte contains
         ;; the number of linkage sections in that block
         ,@(sections->bytes n-code-blocks n-sections)
(LABEL ,after-bytes)
         ;; Code to load the correct byte out of the vector at BYTES into arg 4
         (ADDQ ,regnum:first-arg ,regnum:third-arg ,regnum:volatile-scratch)
         (LDQ_U ,regnum:fourth-arg (OFFSET 0 ,regnum:volatile-scratch))
         (EXTBL ,regnum:fourth-arg ,regnum:volatile-scratch ,regnum:fourth-arg)
         ;; Load the vector of our compiled subblocks from our constant area
         (LDQ ,regnum:third-arg (OFFSET (- ,code-blocks-label ,bytes)
					,regnum:third-arg))
         ,@(object->address regnum:third-arg regnum:third-arg)
         ;; Load the subblock of interest
         (S8ADDQ ,regnum:second-arg ,regnum:third-arg ,regnum:second-arg)
         (LDQ ,regnum:second-arg (OFFSET 0 ,regnum:second-arg))
         ,@(object->address regnum:second-arg regnum:second-arg)
         ;; Get length of code area from subblock header
         (LDQ ,regnum:third-arg
	      (OFFSET ,address-units-per-object ,regnum:second-arg))
         ;; Get length of entire code [sub]block
         (LDQ ,regnum:first-arg (OFFSET 0 ,regnum:second-arg))
         (LDQ ,regnum:first-C-arg ,reg:environment)
         ,@(object->datum regnum:third-arg regnum:third-arg)
         ,@(object->datum regnum:first-arg regnum:first-arg)
         ;; Start calculating addr. of 1st linkage sect. of this [sub]block
         (S8ADDQ ,regnum:third-arg ,regnum:second-arg ,regnum:third-arg)
         ;; Calculate address of the end of the [sub]block to be linked
         (S8ADDQ ,regnum:first-arg ,regnum:second-arg ,regnum:first-arg)
         ;; Finish the address calculation for 1st linkage section
         (LDA ,regnum:third-arg (OFFSET ,(* 2 address-units-per-object)
					,regnum:third-arg))
         ;; Store environment at the end of the [sub]block
         (STQ ,regnum:first-C-arg (OFFSET 0 ,regnum:first-arg))
         ;; Call the linker!  Arguments are:
         ;; first-arg:  return address
         ;; second-arg: address of [sub]block to link
         ;; third-arg:  address of first linkage are in [sub]block
         ;; fourth-arg: number of linkage areas
         ,@(link-to-interface code:compiler-link)
,@(make-external-label (continuation-code-word false) (generate-label))
         ;; Reload the section counter and maybe loop back
         (LDQ ,regnum:first-arg (OFFSET 0 ,regnum:stack-pointer))
         ,@(add-immediate (- n-code-blocks)
			  regnum:first-arg regnum:second-arg)
         (BLT ,regnum:second-arg (@PCR ,loop))
         ;; Pop the section counter off the stack
	 (ADDQ ,regnum:stack-pointer (& ,address-units-per-object)
	       ,regnum:stack-pointer)))))

(define (sections->bytes n-code-blocks section-count-vector)
  ;; Generate a vector of bytes, padded to a multiple of 4.  The
  ;; vector holds the counts of the number of linkage sections in each
  ;; subblock.
  (let walk ((bytes			; Pad to multiple of 4
	      (append (vector->list section-count-vector)
		      (let ((left (remainder n-code-blocks 4)))
			(if (zero? left)
			    '()
			    (make-list (- 4 left) 0))))))
    (if (null? bytes)
	(LAP)
	(let ((lo (car bytes))
	      (midlo (cadr bytes))
	      (midhi (caddr bytes))
	      (hi (cadddr bytes)))
	  (LAP
	   (UWORD ,(+ lo (* 256 (+ midlo (* 256 (+ midhi (* 256 hi)))))))
	   ,@(walk (cddddr bytes)))))))

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
  ; uuos == list of
  ;           (name (frame-size-1 . label-1) (frame-size-2 . label-2) ...)
  ; produces ((frame-size-1 . label-1) (name . dummy-label)
  ;           (frame-size-2 . label-2) (name . dummy-label) ...)  
  (define (inner name assoc)
    (if (null? assoc)
	(transmogrifly (cdr uuos))
	`((,(caar assoc) . ,(cdar assoc)) ; uuo-label
	  (,name . ,(allocate-constant-label))
	  ,@(inner name (cdr assoc)))))
  (if (null? uuos)
      '()
      (inner (caar uuos) (cdar uuos))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
