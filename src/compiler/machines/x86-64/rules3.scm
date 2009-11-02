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

;;;; LAP Generation Rules: Invocations and Entries
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  ;; The continuation is on the stack.
  ;; The type code needs to be cleared first.
  (let ((checks (get-exit-interrupt-checks)))
    (cond ((null? checks)
	   (current-bblock-continue!
	    (make-new-sblock
	     (LAP (POP Q (R ,rax))				; continuation
		  (AND Q (R ,rax) (R ,regnum:datum-mask))	; clear type
		  (JMP (R ,rax))))))
	  ((block-association 'POP-RETURN)
	   => current-bblock-continue!)
	  (else
	   (let ((bblock
		  (make-new-sblock
		   (let ((interrupt-label (generate-label 'INTERRUPT)))
		     (LAP (CMP Q (R ,regnum:free-pointer) ,reg:compiled-memtop)
			  (JGE (@PCR ,interrupt-label))
			  (POP Q (R ,rax)) ; continuation
			  (AND Q (R ,rax) (R ,regnum:datum-mask)) ; clear type
			  (JMP (R ,rax))
			  (LABEL ,interrupt-label)
			  ,@(invoke-hook
			     entry:compiler-interrupt-continuation-2))))))
	     (block-associate! 'POP-RETURN bblock)
	     (current-bblock-continue! bblock))))
    (clear-map!)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (POP Q (R ,rcx))
       #|
       (MOV Q (R ,rdx) (&U ,frame-size))
       ,@(invoke-interface code:compiler-apply)
       |#
       ,@(case frame-size
	   ((1) (invoke-hook entry:compiler-shortcircuit-apply-size-1))
	   ((2) (invoke-hook entry:compiler-shortcircuit-apply-size-2))
	   ((3) (invoke-hook entry:compiler-shortcircuit-apply-size-3))
	   ((4) (invoke-hook entry:compiler-shortcircuit-apply-size-4))
	   ((5) (invoke-hook entry:compiler-shortcircuit-apply-size-5))
	   ((6) (invoke-hook entry:compiler-shortcircuit-apply-size-6))
	   ((7) (invoke-hook entry:compiler-shortcircuit-apply-size-7))
	   ((8) (invoke-hook entry:compiler-shortcircuit-apply-size-8))
	   (else
	    (LAP (MOV Q (R ,rdx) (&U ,frame-size))
		 ,@(invoke-hook entry:compiler-shortcircuit-apply))))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (JMP (@PCR ,label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation
  ;; It expects the procedure at the top of the stack
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (POP Q (R ,rax))
       (AND Q (R ,rax) (R ,regnum:datum-mask)) ;clear type code
       (JMP (R ,rax))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (LEA Q (R ,rcx) (@PCR ,label))
       (MOV Q (R ,rdx) (&U ,number-pushed))
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation
  ;; It expects the procedure at the top of the stack
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (POP Q (R ,rcx))
       (AND Q (R ,rcx) (R ,regnum:datum-mask)) ; clear type code
       (MOV Q (R ,rdx) (&U ,number-pushed))
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (JMP (@PCRO ,(free-uuo-link-label name frame-size) 8))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       (JMP (@PCRO ,(global-uuo-link-label name frame-size) 8))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  continuation
  (expect-no-exit-interrupt-checks)
  (let* ((set-extension
	  (interpreter-call-argument->machine-register! extension rcx))
	 (set-address
	  (begin (require-register! rdx)
		 (load-pc-relative-address (INST-EA (R ,rdx))
					   *block-label*))))
    (delete-dead-registers!)
    (LAP ,@set-extension
	 ,@set-address
	 ,@(clear-map!)
	 (MOV Q (R ,rbx) (&U ,frame-size))
	 ,@(invoke-interface code:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  continuation
  (expect-no-entry-interrupt-checks)
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment rcx))
	 (set-name (object->machine-register! name rdx)))
    (delete-dead-registers!)
    (LAP ,@set-environment
	 ,@set-name
	 ,@(clear-map!)
	 (MOV Q (R ,rbx) (&U ,frame-size))
	 ,@(invoke-interface code:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				; ignored
  (if (eq? primitive compiled-error-procedure)
      (LAP ,@(clear-map!)
	   (MOV Q (R ,rcx) (&U ,frame-size))
	   ,@(invoke-hook entry:compiler-error))
      (let ((arity (primitive-procedure-arity primitive)))
	(cond ((not (negative? arity))
	       (let ((get-code
		      (object->machine-register! primitive rcx)))
		 (LAP ,@get-code
		      ,@(clear-map!)
		      ,@(invoke-hook entry:compiler-primitive-apply))))
	      ((= arity -1)
	       (let ((get-code (object->machine-register! primitive rcx)))
		 (LAP ,@get-code
		      ,@(clear-map!)
		      (MOV Q ,reg:lexpr-primitive-arity (&U ,(-1+ frame-size)))
		      ,@(invoke-hook entry:compiler-primitive-lexpr-apply))))
	      (else
	       ;; Unknown primitive arity.  Go through apply.
	       (let ((get-code (object->machine-register! primitive rcx)))
		 (LAP ,@get-code
		      ,@(clear-map!)
		      (MOV Q (R ,rdx) (&U ,frame-size))
		      ,@(invoke-interface code:compiler-apply))))))))

(let-syntax
    ((define-primitive-invocation
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (cadr form)))
	    `(define-rule statement
	       (INVOCATION:SPECIAL-PRIMITIVE
		(? frame-size)
		(? continuation)
		,(make-primitive-procedure name #t))
	       frame-size continuation
	       (expect-no-exit-interrupt-checks)
	       #|
	       (special-primitive-invocation
		,(close-syntax (symbol-append 'CODE:COMPILER- name)
			       environment))
	       |#
	       (optimized-primitive-invocation
		,(close-syntax (symbol-append 'ENTRY:COMPILER- name)
			       environment))))))))

  (define-primitive-invocation &+)
  (define-primitive-invocation &-)
  (define-primitive-invocation &*)
  (define-primitive-invocation &/)
  (define-primitive-invocation &=)
  (define-primitive-invocation &<)
  (define-primitive-invocation &>)
  (define-primitive-invocation 1+)
  (define-primitive-invocation -1+)
  (define-primitive-invocation zero?)
  (define-primitive-invocation positive?)
  (define-primitive-invocation negative?)
  (define-primitive-invocation quotient)
  (define-primitive-invocation remainder))

(define (special-primitive-invocation code)
  (LAP ,@(clear-map!)
       ,@(invoke-interface code)))

(define (optimized-primitive-invocation entry)
  (LAP ,@(clear-map!)
       ,@(invoke-hook entry)))

;;; Invocation Prefixes

;;; rsp = 4, regnum:stack-pointer

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 4))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK 0 (REGISTER 4) (? any))
  any					; ignored
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP
   (? frame-size)
   (OFFSET-ADDRESS (REGISTER 4)
		   (MACHINE-CONSTANT (? offset))))
  (QUALIFIER (or (zero? (- offset frame-size)) (< frame-size 3)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (with-signed-immediate-operand (* address-units-per-object how-far)
	     (lambda (addend)
	       (LAP (ADD Q (R ,rsp) ,addend)))))
	  ((= frame-size 1)
	   (let ((temp (temporary-register-reference)))
	     (LAP (MOV Q ,temp (@R ,rsp))
		  ,@(with-signed-immediate-operand
			(* address-units-per-object offset)
		      (lambda (addend)
			(LAP (ADD Q (R ,rsp) ,addend))))
		  (PUSH Q ,temp))))
	  ((= frame-size 2)
	   (let ((temp1 (temporary-register-reference))
		 (temp2 (temporary-register-reference)))
	     (LAP (MOV Q ,temp2 (@RO B ,rsp ,address-units-per-object))
		  (MOV Q ,temp1 (@R ,rsp))
		  ,@(with-signed-immediate-operand
			(* address-units-per-object offset)
		      (lambda (addend)
			(LAP (ADD Q (R ,rsp) ,addend))))
		  (PUSH Q ,temp2)
		  (PUSH Q ,temp1))))
	  (else
	   (error "INVOCATION-PREFIX:MOVE-FRAME-UP: Incorrectly invoked!")))))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? reg)))
  (generate/move-frame-up* frame-size
			   (move-to-temporary-register! reg 'GENERAL)
			   temporary-register-reference))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? reg-1))
				  (REGISTER (? reg-2)))
  (QUALIFIER (not (= reg-1 rsp)))
  (let* ((label (generate-label 'DYN-CHOICE))
	 (temp1 (move-to-temporary-register! reg-1 'GENERAL))
	 (temp2 (standard-move-to-temporary! reg-2)))
    (LAP (CMP Q (R ,temp1) ,temp2)
	 (JLE (@PCR ,label))
	 (MOV Q (R ,temp1) ,temp2)
	 (LABEL ,label)
	 ,@(generate/move-frame-up* frame-size temp1 (lambda () temp2)))))

(define (generate/move-frame-up* frame-size reg get-temp)
  (if (zero? frame-size)
      (LAP (MOV Q (R ,rsp) (R ,reg)))
      (let ((temp (get-temp))
	    (ctr (allocate-temporary-register! 'GENERAL))
	    (label (generate-label 'MOVE-LOOP)))
	(LAP (LEA Q (R ,reg)
		  ,(byte-offset-reference
		    reg
		    (* -1 address-units-per-object frame-size)))
	     (MOV Q (R ,ctr) (&U ,(-1+ frame-size)))
	     (LABEL ,label)
	     (MOV Q ,temp (@RI ,rsp ,ctr ,address-units-per-object))
	     (MOV Q (@RI ,reg ,ctr ,address-units-per-object) ,temp)
	     (SUB Q (R ,ctr) (&U 1))
	     (JGE (@PCR ,label))
	     (MOV Q (R ,rsp) (R ,reg))))))

;;;; External Labels

;;; Entry point types

(define (make-procedure-code-word min max)
  ;; The "min" byte must be less than #x80; the "max" byte may not
  ;; equal #x80 but can take on any other value.
  (if (or (negative? min) (>= min #x80))
      (error "MAKE-PROCEDURE-CODE-WORD: minimum out of range" min))
  (if (>= (abs max) #x80)
      (error "MAKE-PROCEDURE-CODE-WORD: maximum out of range" max))
  (make-code-word min (if (negative? max) (+ #x100 max) max)))

(define internal-entry-code-word
  (make-code-word #xff #xfe))

(define internal-continuation-code-word
  (make-code-word #xff #xfc))

(define (frame-size->code-word offset default)
  (cond ((not offset)
	 default)
	((< offset #x2000)
	 ;; This uses up through (#xff #xdf).
	 (let ((qr (integer-divide offset #x80)))
	   (make-code-word (+ #x80 (integer-divide-remainder qr))
			   (+ #x80 (integer-divide-quotient qr)))))
	(else
	 (error "Unable to encode continuation offset"
		offset))))

(define (continuation-code-word label)
  (frame-size->code-word
   (if label
       (rtl-continuation/next-continuation-offset (label->object label))
       0)
   internal-continuation-code-word))

(define (internal-procedure-code-word rtl-proc)
  (frame-size->code-word
   (rtl-procedure/next-continuation-offset rtl-proc)
   internal-entry-code-word))

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

(define (interrupt-check interrupt-label checks)
  ;; This always does interrupt checks in line.
  (LAP ,@(if (or (memq 'INTERRUPT checks) (memq 'HEAP checks))
	     (LAP (CMP Q (R ,regnum:free-pointer) ,reg:compiled-memtop)
		  (JGE (@PCR ,interrupt-label)))
	     (LAP))
       ,@(if (memq 'STACK checks)
	     (LAP (CMP Q (R ,regnum:stack-pointer) ,reg:stack-guard)
		  (JL (@PCR ,interrupt-label)))
	     (LAP))))

(define (simple-procedure-header code-word label entry)
  (let ((checks (get-entry-interrupt-checks)))
    (if (null? checks)
	(LAP ,@(make-external-label code-word label))
	(let ((gc-label (generate-label)))
	  (LAP (LABEL ,gc-label)
	       ,@(invoke-hook/call entry)
	       ,@(make-external-label code-word label)
	       ,@(interrupt-check gc-label checks))))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (expect-no-entry-interrupt-checks)
  (make-external-label (continuation-code-word internal-label)
		       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  #|
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   entry:compiler-interrupt-continuation)
  |#
  (expect-no-entry-interrupt-checks)
  (make-external-label (continuation-code-word internal-label)
		       internal-label))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (get-entry-interrupt-checks)		; force search
  (let ((procedure (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label procedure))
	  (gc-label (generate-label)))
      (LAP (ENTRY-POINT ,external-label)
	   (EQUATE ,external-label ,internal-label)
	   (LABEL ,gc-label)
	   ,@(invoke-interface/call code:compiler-interrupt-ic-procedure)
	   ,@(make-external-label expression-code-word internal-label)
	   ,@(interrupt-check gc-label)))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (LAP (EQUATE ,(rtl-procedure/external-label rtl-proc) ,internal-label)
	 ,@(simple-procedure-header (internal-procedure-code-word rtl-proc)
				    internal-label
				    (if (rtl-procedure/dynamic-link? rtl-proc)
					entry:compiler-interrupt-dlink
					entry:compiler-interrupt-procedure)))))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(rtl-procedure/external-label
		 (label->object internal-label))
	       ,internal-label)
       ,@(simple-procedure-header (make-procedure-code-word min max)
				  internal-label
				  entry:compiler-interrupt-procedure)))

;;;; Closures:

(define (generate/cons-closure target procedure-label min max size)
  (let* ((mtarget (target-register target))
	 (target (register-reference mtarget))
	 (temp (temporary-register-reference))
	 (data-offset address-units-per-closure-manifest)
	 (format-offset (+ data-offset address-units-per-closure-entry-count))
	 (pc-offset (+ format-offset address-units-per-entry-format-code))
	 (slots-offset
	  (+ pc-offset
	     address-units-per-closure-entry-instructions
	     address-units-per-closure-padding))
	 (free-offset
	  (+ slots-offset (* size address-units-per-object))))
    (LAP (MOV Q ,temp (&U ,(make-closure-manifest size)))
	 (MOV Q (@R ,regnum:free-pointer) ,temp)
	 ;; There's only one entry point here.
	 (MOV L (@RO B ,regnum:free-pointer ,data-offset) (&U 1))
	 ,@(generate-closure-entry procedure-label min max format-offset temp)
	 ;; Load the address of the entry instruction into TARGET.
	 (LEA Q ,target (@RO B ,regnum:free-pointer ,pc-offset))
	 ;; Bump FREE.
	 ,@(with-signed-immediate-operand free-offset
	     (lambda (addend)
	       (LAP (ADD Q (R ,regnum:free-pointer) ,addend)))))))

(define (generate/cons-multiclosure target nentries size entries)
  (let* ((mtarget (target-register target))
	 (target (register-reference mtarget))
	 (temp (temporary-register-reference)))
    (define (generate-entries entries offset)
      (LAP ,@(let ((entry (car entries)))
	       (let ((label (car entry))
		     (min (cadr entry))
		     (max (caddr entry)))
		 (generate-closure-entry label min max offset temp)))
	   ,@(generate-entries (cdr entries)
			       (+ offset address-units-per-closure-entry))))
    (let* ((data-offset address-units-per-closure-manifest)
	   (first-format-offset
	    (+ data-offset address-units-per-closure-entry-count))
	   (first-pc-offset
	    (+ first-format-offset address-units-per-entry-format-code))
	   (free-offset
	    (+ first-format-offset
	       (* nentries address-units-per-closure-entry)
	       (* size address-units-per-object))))
      (LAP (MOV Q ,temp (&U ,(make-multiclosure-manifest nentries size)))
	   (MOV Q (@R ,regnum:free-pointer) ,temp)
	   (MOV L (@RO ,regnum:free-pointer ,data-offset) (&U ,nentries))
	   ,@(generate-entries entries first-format-offset)
	   (LEA Q ,target (@RO B ,regnum:free-pointer ,first-pc-offset))
	   ,@(with-signed-immediate-operand free-offset
	       (lambda (addend)
		 (LAP (ADD Q (R ,regnum:free-pointer) ,addend))))))))

(define (generate-closure-entry label min max offset temp)
  (let* ((procedure-label (rtl-procedure/external-label (label->object label)))
	 (MOV-offset (+ offset address-units-per-entry-format-code))
	 (imm64-offset (+ MOV-offset 2))
	 (CALL-offset (+ imm64-offset 8)))
    (LAP (MOV L (@RO B ,regnum:free-pointer ,offset)
	      (&U ,(make-closure-code-longword min max MOV-offset)))
	 (LEA Q ,temp (@PCR ,procedure-label))
	 ;; (MOV Q (R ,rax) (&U <procedure-label>))
	 ;; The instruction sequence is really `48 b8', but this is a
	 ;; stupid little-endian architecture.  I want my afternoon
	 ;; back.
	 (MOV W (@RO B ,regnum:free-pointer ,MOV-offset) (&U #xB848))
	 (MOV Q (@RO B ,regnum:free-pointer ,imm64-offset) ,temp)
	 ;; (CALL (R ,rax))
	 (MOV W (@RO B ,regnum:free-pointer ,CALL-offset) (&U #xD0FF)))))

(define (generate/closure-header internal-label nentries)
  (let* ((rtl-proc (label->object internal-label))
	 (external-label (rtl-procedure/external-label rtl-proc))
	 (checks (get-entry-interrupt-checks)))
    (define (label+adjustment)
      (LAP ,@(make-external-label internal-entry-code-word external-label)
	   ;; Assumption: RAX is not in use here.  (In fact, it is
	   ;; used to store the absolute address of this header.)
	   ;; See comment by CLOSURE-ENTRY-MAGIC to understand
	   ;; what's going on here.
	   (MOV Q (R ,rax) (&U ,(closure-entry-magic)))
	   (ADD Q (@R ,rsp) (R ,rax))
	   (LABEL ,internal-label)))
    (cond ((zero? nentries)
	   (LAP (EQUATE ,external-label ,internal-label)
		,@(simple-procedure-header
		   (internal-procedure-code-word rtl-proc)
		   internal-label
		   entry:compiler-interrupt-procedure)))
	  ((pair? checks)
	   (let ((gc-label (generate-label 'GC-LABEL)))
	     (LAP (LABEL ,gc-label)
		  ,@(invoke-hook entry:compiler-interrupt-closure)
		  ,@(label+adjustment)
		  ,@(interrupt-check gc-label checks))))
	  (else
	   (label+adjustment)))))

;;; On entry to a closure, the quadword at the top of the stack will
;;; be an untagged pointer to the byte following the CALL instruction
;;; that led the machine there.  CLOSURE-ENTRY-MAGIC returns a number
;;; that, when added to this quadword, yields the tagged compiled
;;; entry that was used to invoke the closure.  This is what the RTL
;;; deals with, and this is what interrupt handlers want, particularly
;;; for the garbage collector, which wants to find only nice tagged
;;; pointers on the stack.

(define-integrable (closure-entry-magic)
  (- (make-non-pointer-literal (ucode-type COMPILED-ENTRY) 0)
     address-units-per-closure-entry-instructions))

(define-integrable (make-closure-manifest size)
  (make-multiclosure-manifest 1 size))

(define-integrable (make-multiclosure-manifest nentries size)
  (make-non-pointer-literal
   (ucode-type MANIFEST-CLOSURE)
   (+ (quotient (+ address-units-per-closure-entry-count
		   (* nentries address-units-per-closure-entry)
		   address-units-per-closure-padding
		   7)
		8)
      size)))

(define-integrable (make-closure-longword code-word pc-offset)
  (+ code-word (* #x20000 pc-offset)))

(define-integrable (make-closure-code-longword frame/min frame/max pc-offset)
  (make-closure-longword (make-procedure-code-word frame/min frame/max)
			 pc-offset))

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry					;ignore
  (generate/closure-header internal-label nentries))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? size)))
  (generate/cons-closure target procedure-label min max size))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? size) (? entries)))
  (case nentries
    ((0)
     (let ((target (target-register-reference target)))
       (LAP (MOV Q ,target		;Use TARGET as a temporary.
		 (&U ,(make-non-pointer-literal (ucode-type manifest-vector)
						size)))
	    (MOV Q (@R ,regnum:free-pointer) ,target)
	    (MOV Q ,target (R ,regnum:free-pointer))
	    ,@(with-signed-immediate-operand
		  (* address-units-per-object (1+ size))
		(lambda (addend)
		  (LAP (ADD Q (R ,regnum:free-pointer) ,addend)))))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (generate/cons-closure target
			      (car entry) (cadr entry) (caddr entry)
			      size)))
    (else
     (generate/cons-multiclosure target nentries size
				 (vector->list entries)))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  (LAP (MOV Q (R ,rcx) ,reg:environment)
       (MOV Q (@PCR ,environment-label) (R ,rcx))
       (LEA Q (R ,rdx) (@PCR ,*block-label*))
       (LEA Q (R ,rbx) (@PCR ,free-ref-label))
       (MOV Q ,reg:utility-arg-4 (&U ,n-sections))
       #|
       ,@(invoke-interface/call code:compiler-link)
       |#
       ,@(invoke-hook/call entry:compiler-link)
       ,@(make-external-label (continuation-code-word #f)
			      (generate-label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (LAP (MOV Q (R ,rdx) (@PCR ,code-block-label))
       (AND Q (R ,rdx) (R ,regnum:datum-mask))
       (LEA Q (R ,rbx) (@RO L ,rdx ,free-ref-offset))
       (MOV Q (R ,rcx) ,reg:environment)
       (MOV Q (@RO L ,rdx ,environment-offset) (R ,rcx))
       (MOV Q ,reg:utility-arg-4 (&U ,n-sections))
       #|
       ,@(invoke-interface/call code:compiler-link)
       |#
       ,@(invoke-hook/call entry:compiler-link)
       ,@(make-external-label (continuation-code-word #f)
			      (generate-label))))

(define (generate/remote-links n-blocks vector-label nsects)
  (if (zero? n-blocks)
      (LAP)
      (let ((loop (generate-label))
	    (bytes (generate-label))
	    (end (generate-label)))
	(LAP
	 ;; Push counter
	 (PUSH Q (& 0))
	(LABEL ,loop)
	 ;; Get index
	 (MOV Q (R ,rcx) (@R ,rsp))
	 ;; Get vector
	 (MOV Q (R ,rdx) (@PCR ,vector-label))
	 ;; Get n-sections for this cc-block
	 (XOR Q (R ,rbx) (R ,rbx))
	 (LEA Q (R ,rax) (@PCR ,bytes))
	 (MOV B (R ,rbx) (@RI ,rax ,rcx 1))
	 ;; address of vector
	 (AND Q (R ,rdx) (R ,regnum:datum-mask))
	 ;; Store n-sections in arg
	 (MOV Q ,reg:utility-arg-4 (R ,rbx))
	 ;; vector-ref -> cc block
	 (MOV Q
	      (R ,rdx)
	      (@ROI B
		    ,rdx ,address-units-per-object
		    ,rcx ,address-units-per-object))
	 ;; address of cc-block
	 (AND Q (R ,rdx) (R ,regnum:datum-mask))
	 ;; cc-block length
	 (MOV Q (R ,rbx) (@R ,rdx))
	 ;; Get environment
	 (MOV Q (R ,rcx) ,reg:environment)
	 ;; Eliminate length tags
	 (AND Q (R ,rbx) (R ,regnum:datum-mask))
	 ;; Store environment
	 (MOV Q (@RI ,rdx ,rbx ,address-units-per-object) (R ,rcx))
	 ;; Get NMV header
	 (MOV Q (R ,rcx) (@RO B ,rdx ,address-units-per-object))
	 ;; Eliminate NMV tag
	 (AND Q (R ,rcx) (R ,regnum:datum-mask))
	 ;; Address of first free reference
	 (LEA Q
	      (R ,rbx)
	      (@ROI B
		    ,rdx ,(* 2 address-units-per-object)
		    ,rcx ,address-units-per-object))
	 ;; Invoke linker
	 ,@(invoke-hook/call entry:compiler-link)
	 ,@(make-external-label (continuation-code-word false)
				(generate-label))
	 ;; Increment counter and loop
	 (ADD Q (@R ,rsp) (&U 1))
	 ,@(receive (temp prefix comparand)
	       ;; Choose an arbitrary temporary register that is not
	       ;; in use in this sequence.
	       (unsigned-immediate-operand n-blocks (lambda () r11))
	     temp			;ignore
	     (LAP ,@prefix
		  (CMP Q (@R ,rsp) ,comparand)))
	 (JL (@PCR ,loop))

	 (JMP (@PCR ,end))
	(LABEL ,bytes)
	 ,@(let walk ((bytes (vector->list nsects)))
	     (if (null? bytes)
		 (LAP)
		 (LAP (BYTE U ,(car bytes))
		      ,@(walk (cdr bytes)))))
	(LABEL ,end)
	 ;; Pop counter
	 (POP Q (R ,rax))))))

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

;; IMPORTANT:
;; frame-size and uuo-label are switched (with respect to the 68k
;; version) in order to preserve the arity in a constant position (the
;; x86 is little-endian).  The invocation rule for uuo-links has been
;; changed to take the extra object into account.

(define (transmogrifly variable.caches-list)
  (append-map
   (lambda (variable.caches)
     (append-map (let ((variable (car variable.caches)))
		   (lambda (cache)
		     (let ((frame-size (car cache))
			   (label (cdr cache)))
		       `((,frame-size . ,label)
			 (,variable . ,(allocate-constant-label))
			 (#F . ,(allocate-constant-label))))))
		 (cdr variable.caches)))
   variable.caches-list))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
