#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/rules3.scm,v 1.1 1990/05/07 04:16:34 jinx Exp $
$MC68020-Header: rules3.scm,v 4.23 90/01/18 22:44:09 GMT cph Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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
	 ,@(object->address temp)
	 (JR ,temp)
	 (NOP))))			; DELAY SLOT

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,@(load-immediate frame-size regnum:third-arg)
       (LW ,regnum:second-arg (OFFSET 0 ,regnum:stack-pointer))
       (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)
       ,@(invoke-interface code:compiler-apply)))

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
  (LAP ,@(clear-map!)
       ,@(load-immediate number-pushed regnum:third-arg)
       ,@(load-pc-relative-address label regnum:second-arg)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into second-arg
  (LAP ,@(clear-map!)
       (LW ,regnum:second-arg (OFFSET 0 ,regnum:stack-pointer))
       (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)
       ,@(load-immediate number-pushed regnum:third-arg)
       ,@(object->address regnum:second-arg)
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       (BGEZ 0 (@PCR ,(free-uuo-link-label name frame-size)))
       (NOP)))				; DELAY SLOT

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (? extension register-expression))
  continuation				;ignore
  (LAP ,@(load-interface-args! extension false false false)
       ,@(load-immediate frame-size regnum:fourth-arg)
       ,@(load-pc-relative-address *block-label* regnum:third-arg)
       ,@(invoke-interface code:compiler-cache-reference-apply)))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (? environment register-expression)
		     (? name))
  continuation				;ignore
  (LAP ,@(load-interface-args! environment false false false)
       ,(load-constant name regnum:third-arg)
       ,(load-immediate frame-size regnum:fourth-arg)
       ,@(invoke-interface code:compiler-lookup-apply)))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (if (eq? primitive compiled-error-procedure)
      (LAP ,@(clear-map!)
	   ,@(load-immediate frame-size regnum:second-arg)
	   ,@(invoke-interface code:compiler-error))
      (LAP ,@(clear-map!)
	   ,@(load-pc-relative (constant->label primitive)
			       regnum:second-arg)
	   ,@(let ((arity (primitive-procedure-arity primitive)))
	       (cond ((not (negative? arity))
		      (invoke-interface code:compiler-primitive-apply))
		     ((= arity -1)
		      (LAP ,@(load-immediate (-1+ frame-size)
					     ,regnum:assembler-temp)

			   (SW ,regnum:assembler-temp
			       ,reg:lexpr-primitive-arity)
			   ,@(invoke-interface
			      code:compiler-primitive-lexpr-apply)))
		     (else
		      ;; Unknown primitive arity.  Go through apply.
		      (LAP ,@(load-immediate frame-size regnum:third-arg)
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
	    ,(list 'LAP
		   (list 'UNQUOTE-SPLICING '(clear-map!))
		   (list 'UNQUOTE-SPLICING
			 `(INVOKE-INTERFACE ,(symbol-append 'CODE:COMPILER-
							    name))))))))
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

;;; MOVE-FRAME-UP size address
;;;
;;; Moves up the last <size> words of the stack so that the first of
;;; these words is at location <address>, and resets the stack pointer
;;; to the last of these words.  That is, it pops off all the words
;;; between <address> and TOS+/-<size>.

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
   (? frame-size) (OFFSET-ADDRESS (REGISTER 3) (? offset)))
  (let ((how-far (* 4 (- offset frame-size))))
    (cond ((zero? how-far)
	   (LAP))
	  ((negative? how-far)
	   (error "invocation-prefix:move-frame-up: bad specs"
		  frame-size offset))
	  ((zero? frame-size)
	   (add-immediate how-far ,regnum:stack-pointer
			  ,regnum:stack-pointer))
	  ((= frame-size 1)
	   (let ((temp (standard-temporary!)))
	     (LAP (LW ,temp (OFFSET ,how-far ,regnum:stack-pointer))
		  (ADDI ,regnum:stack-pointer
			,regnum:stack-pointer ,how-far)
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
	       (add-immediate
		(* 4 offset) ,regnum:stack-pointer reg)))))))

(define-rule statement
  ;; Move <frame-size> words back to base virtual register + offset
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER (? base))
						   (? offset)))
  (generate/move-frame-up frame-size
    (lambda (reg)
      (add-immediate (* 4 offset) (standard-source! base) reg))))

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
      (let ((env-reg (standard-move-to-temporary! source)))
	(LAP (SLTU ,regnum:assembler-temp
		   ,env-reg ,regnum:dynamic-link)
	     (BNE 0 ,regnum:assembler-temp (@PCO 8))
	     (NOP)			; +0: DELAY SLOT
	     (ADD ,env-reg 0		; +4: Skipped instruction
		  ,regnum:dynamic-link) 
	     ,@(generate/move-frame-up*	; +8: here
		frame-size env-reg)))))

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
	      (LAP (LW ,temp (OFFSET 0 ,regnum:stack-pointer))
		   (ADDI ,destination ,destination -4)
		   (SW ,temp (OFFSET 0 ,destination)))))
	   (else
	    (generate/move-frame-up** frame-size destination)))
       (ADD ,regnum:stack-pointer 0 ,destination)))

(define (generate/move-frame-up** frame-size dest)
  (let ((from (standard-temporary!))
	(temp1 (standard-temporary!))
	(temp2 (standard-temporary!)))
    (LAP ,@(add-immediate (* 4 frame-size) regnum:stack-pointer from)
	 ,@(if (<= frame-size 3)
	       ;; This code can handle any number > 1 (handled above),
	       ;; but we restrict it to 3 for space reasons.
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
			   (SW ,temp1 (OFFSET -4 ,dest))
			   (SW ,temp2 (OFFSET -8 ,dest))
			   (SW ,temp3 (OFFSET -12 ,dest))
			   (ADDI ,dest ,dest -12))))
		   (else
		    (LAP (LW ,temp1 (OFFSET -4 ,from))
			 (LW ,temp2 (OFFSET -8 ,from))
			 (ADDI ,from ,from -8)
			 (SW ,temp1 (OFFSET  -4 ,dest))
			 (SW ,temp2 (OFFSET -8 ,dest))
			 (ADDI ,dest ,dest -8)
			 ,@(loop (- n 2))))))
	       (LAP ,@(load-immediate frame-size temp2)
		    (LW ,temp1 (OFFSET -4 ,from)) ; -20
		    (ADDI ,from ,from -4)	 ; -16
		    (ADDI ,temp2 ,temp2 -1)	 ; -12
		    (ADDI ,dest ,dest -4)	 ; -8
		    (BNE ,temp2 0 (@PCO -20))	 ; -4
		    (SW ,temp1 (OFFSET 0 ,dest)))))))

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

(define (continuation-code-word label)
  (let ((offset
	 (if label
	     (rtl-continuation/next-continuation-offset (label->object label))
	     0)))
    (cond ((not offset)
	   (make-code-word #xff #xfc))
	  ((< offset #x2000)
	   ;; This uses up through (#xff #xdf).
	   (let ((qr (integer-divide offset #x80)))
	     (make-code-word (+ #x80 (integer-divide-remainder qr))
			     (+ #x80 (integer-divide-quotient qr)))))
	  (else
	   (error "Unable to encode continuation offset" offset)))))

;;;; Procedure headers

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.
;;;
;;; **** This is not strictly true: the dynamic link register may
;;; contain a valid dynamic link, but the gc handler determines that
;;; and saves it as appropriate.

(define (simple-procedure-header code-word label code)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 ,@(link-to-interface code)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check gc-label))))

(define (dlink-procedure-header code-word label)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 (ADD ,regnum:third-arg 0 ,regnum:dynamic-link)
	 ,@(link-to-interface code:compiler-interrupt-dlink)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check gc-label))))

(define (interrupt-check gc-label)
  (LAP (SLT ,regnum:assembler-temp ,regnum:memtop ,regnum:free)
       (BNE ,regnum:assembler-temp 0 (@PCR ,gc-label))
       (LW ,regnum:memtop ,reg:memtop)))

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
	    internal-entry-code-word
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

(define-integrable (address->entry register)
  (deposit-type (ucode-type compiled-entry) register))

(define-rule statement
  (CLOSURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label procedure)))
      (LAP (LABEL ,gc-label)
	   ,@(invoke-interface code:compiler-interrupt-closure)
	   ,@(make-external-label internal-entry-code-word external-label)
	   ; Code below here corresponds to code and count in cmpint2.h
	   ,@(address->entry regnum:linkage)
	   (SW ,regnum:linkage (OFFSET -4 ,regnum:stack-pointer))
	   (ADDI ,regnum:stack-pointer ,regnum:stack-pointer -4)
	   (LABEL ,internal-label)
	   ,@(interrupt-check gc-label)))))

(define (cons-closure target label min max size ->entry?)
  (let ((flush-reg (clear-registers! regnum:interface-index)))
    (need-register! regnum:interface-index)
    (let ((dest (standard-target! target)))
      ;; Note: dest is used as a temporary before the JALR
      ;; instruction, and is written immediately afterwards.
      ;; The interface (scheme_to_interface-88) expects:
      ;;    1: size of closure = size+3
      ;;    4: offset to destination label
      ;;   25: GC offset and arity information
      (LAP ,@flush-reg
	   ,@(load-immediate (+ size 3) 1)
	   (LUI 25 4)
	   (PC-RELATIVE-OFFSET 4 16
	    ,(rtl-procedure/external-label (label->object label)))
	   (ADDI ,dest ,regnum:scheme-to-interface -88)	; + 4
	   (JALR ,regnum:linkage ,dest)		        ; + 8
	   (ORI 25 25 ,(make-procedure-code-word min max)) ; +12
	   ,@(add-immediate (* 4 (- (+ size 2))) ; +16
			    regnum:free dest)
	   ,@(if ->entry? (address->entry dest) (LAP))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
				      (? min) (? max) (? size))))
  (QUALIFIER (= type (ucode-type compiled-entry)))
  (cons-closure target procedure-label min max size true))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? size)))
  (QUALIFIER (= type (ucode-type compiled-entry)))
  (cons-closure target procedure-label min max size false))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  ;; Calls the linker
  ;; On MIPS, regnum:first-arg is used as a temporary here since
  ;; load-pc-relative-address uses the assembler temporary.
  (LAP
   ; Grab interp's env. and store in code block at environment-label
   (LW ,regnum:first-arg ,reg:environment)
   ,@(load-pc-relative-address environment-label regnum:second-arg)
   (SW ,regnum:first-arg (OFFSET 0 ,regnum:second-arg))
   ; Now invoke the linker (arg. 1 is return address, supplied by interface)
   ,@(load-pc-relative-address *block-label* regnum:third-arg)
   ,@(load-pc-relative-address free-ref-label regnum:fourth-arg)
   ,@(load-immediate n-sections regnum:first-arg)
   (SW ,regnum:first-arg (OFFSET 16 ,regnum:C-stack-pointer))
   ,@(link-to-interface code:compiler-link)
   ,@(make-external-label (continuation-code-word false)
			  (generate-label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  ;; Link all of the top level procedures within the file
  (LAP ,@(load-pc-relative code-block-label regnum:third-arg)
       (LW ,regnum:assembler-temp ,reg:environment)
       ,@(object->address regnum:third-arg)
       ,@(add-immediate environment-offset regnum:third-arg
			regnum:second-arg)
       (SW ,regnum:assembler-temp (OFFSET 0 ,regnum:second-arg))
       ,@(add-immediate free-ref-offset regnum:third-arg regnum:fourth-arg)
       ,@(load-immediate n-sections regnum:first-arg)
       (SW ,regnum:first-arg (OFFSET 16 ,regnum:C-stack-pointer))
       ,@(link-to-interface code:compiler-link)
       ,@(make-external-label (continuation-code-word false)
			      (generate-label))))

(define (generate/constants-block constants references assignments uuo-links)
  (let ((constant-info
	 (declare-constants 0 (transmogrifly uuo-links)
	   (declare-constants 1 references
	     (declare-constants 2 assignments
	       (declare-constants false constants
		 (cons false (LAP))))))))
    (let ((free-ref-label (car constant-info))
	  (constants-code (cdr constant-info))
	  (debugging-information-label (allocate-constant-label))
	  (environment-label (allocate-constant-label))
	  (n-sections
	   (+ (if (null? uuo-links) 0 1)
	      (if (null? references) 0 1)
	      (if (null? assignments) 0 1))))
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
      (inner (caar uuos) (cdar uuos))))         ; caar is name, cdar is alist of frame sizes

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
