#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rules3.scm,v 1.3 1992/01/30 06:32:33 jinx Exp $
$MC68020-Header: /scheme/compiler/bobcat/RCS/rules3.scm,v 4.31 1991/05/28 19:14:55 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

(define-integrable (clear-continuation-type-code)
  (LAP (AND W (@RO ,regnum:stack-pointer) (R ,regnum:pointer-mask))))

(define-rule statement
  (POP-RETURN)
  (LAP ,@(clear-map!)
       ,@(clear-continuation-type-code)
       (RET)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation
  (LAP ,@(clear-map!)
       ,@(case frame-size
	   ((1) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-1)))
	   ((2) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-2)))
	   ((3) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-3)))
	   ((4) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-4)))
	   ((5) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-5)))
	   ((6) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-6)))
	   ((7) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-7)))
	   ((8) (LAP (JMP ,entry:compiler-shortcircuit-apply-size-8)))
	   (else
	    (LAP (MOV W (R ,ecx) (& ,frame-size))
		 (JMP ,entry:compiler-shortcircuit-apply))))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation
  (LAP ,@(clear-map!)
       (JMP (@PCR ,label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation
  ;; It expects the procedure at the top of the stack
  (LAP ,@(clear-map!)
       ,@(clear-continuation-type-code)
       (RET)))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation
  (with-pc-relative-address
    (lambda (pc-label pc-register)
      (LAP ,@(clear-map!)
	   (LEA (R ,ecx) (@RO ,pc-register (- ,label ,pc-label)))
	   (MOV W (R ,edx) (& ,number-pushed))
	   ,@(invoke-interface code:compiler-lexpr-apply)))))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation
  ;; It expects the procedure at the top of the stack
  (LAP ,@(clear-map!)
       ,@(clear-continuation-type-code)
       (POP (R ,ecx))
       (MOV W (R ,edx) (& ,number-pushed))
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation
  (LAP ,@(clear-map!)
       (JMP (@PCR ,(free-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation
  (LAP ,@(clear-map!)
       (JMP (@PCR ,(global-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  continuation
  (let* ((set-extension
	  (interpreter-call-argument->machine-register! extension ecx))
	 (set-address
	  (begin (require-register! edx)
		 (load-pc-relative-address (INST-EA (R ,edx))
					   *block-label*))))
	  
    (delete-dead-registers!)
    (LAP ,@set-extension
	 ,@set-address
	 ,@(clear-map!)
	 (MOV W (R ,ebx) (& ,frame-size))
	 ,@(invoke-interface code:compiler-cache-reference-apply))))  

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  continuation
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment ecx))
	 (set-name (object->machine-register! name edx)))
    (delete-dead-registers!)
    (LAP ,@set-environment
	 ,@set-name
	 ,@(clear-map!)
	 (MOV W (R ,ebx) (& ,frame-size))
	 ,@(invoke-interface code:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation
  (if (eq? primitive compiled-error-procedure)
      (LAP ,@(clear-map!)
	   (MOV W (R ,ecx) (& ,frame-size))
	   (JMP ,entry:compiler-error))
      (let ((arity (primitive-procedure-arity primitive))
	    (get-code (object->machine-register! primitive ecx)))
	(cond ((not (negative? arity))
	       (LAP ,@get-code
		    ,@(clear-map!)
		    (JMP ,entry:compiler-primitive-apply)))
	      ((= arity -1)
	       (LAP ,@get-code
		    ,@(clear-map!)
		    (MOV W ,reg:lexpr-primitive-arity (& ,(-1+ frame-size)))
		    (JMP ,entry:compiler-primitive-lexpr-apply)))
	      (else
	       ;; Unknown primitive arity.  Go through apply.
	       (LAP ,@get-code
		    ,@(clear-map!)
		    (MOV W (R ,edx) (& ,frame-size))
		    ,@(invoke-interface code:compiler-apply)))))))

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
	     ,(symbol-append 'ENTRY:COMPILER- name))))))

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
  (define-optimized-primitive-invocation quotient)
  (define-optimized-primitive-invocation remainder))

(define (special-primitive-invocation code)
  (LAP ,@(clear-map!)
       ,@(invoke-interface code)))

(define (optimized-primitive-invocation hook)
  (LAP ,@(clear-map!)
       (JMP ,hook)))

;;; Invocation Prefixes

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 4))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK 0 (REGISTER 4) (? any))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 4) (? offset)))
  (QUALIFIER (or (zero? (- offset frame-size)) (< frame-size 3)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (LAP (ADD W (R 4) (& ,(* 4 how-far)))))
	  ((= frame-size 1)
	   (let ((temp (temporary-register-reference)))
	     (LAP (MOV W ,temp (@R 4))
		  (ADD W (R 4) (& ,(* 4 offset)))
		  (PUSH W ,temp))))
	  ((= frame-size 2)
	   (let ((temp1 (temporary-register-reference))
		 (temp2 (temporary-register-reference)))
	     (LAP (MOV W ,temp2 (@RO 4 4))
		  (MOV W ,temp1 (@R 4))
		  (ADD W (R 4) (& ,(* 4 offset)))
		  (PUSH W ,temp2)
		  (PUSH W ,temp1))))
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
  (QUALIFIER (not (= reg-1 4)))
  (let* ((label (generate-label 'DYN-CHOICE))
	 (temp1 (move-to-temporary-register! reg-1 'GENERAL))
	 (temp2 (standard-move-to-temporary! reg-2)))
    (LAP (CMP W (R ,temp1) ,temp2)
	 (JLE (@PCR ,label))
	 (MOV W (R ,temp1) ,temp2)
	 (LABEL ,label)
	 ,@(generate/move-frame-up* frame-size temp1 (lambda () temp2)))))

(define (generate/move-frame-up* frame-size reg get-temp)
  (if (zero? frame-size)
      (LAP (MOV W (R 4) (R ,reg)))
      (let ((temp (get-temp))
	    (ctr (allocate-temporary-register! 'GENERAL))
	    (label (generate-label 'MOVE-LOOP)))
	(LAP (LEA (R ,reg) (@RO ,reg ,(* -4 frame-size)))
	     (MOV W (R ,ctr) (& (-1+ frame-size)))
	     (LABEL ,label)
	     (MOV W ,temp (@RI 4 ,ctr 4))
	     (MOV W (@RI ,reg ,ctr 4) ,temp)
	     (DEC W ,ctr)
	     (JGE (PCR ,label))
	     (MOV W (R 4) (R ,reg))))))

;;;; External Labels

(define (make-external-label code label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (DC UW ,code)
       (BLOCK-OFFSET ,label)
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

(define-integrable (simple-procedure-header code-word label entry)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 (CALL ,entry)
	 ,@(make-external-label code-word label)
	 (CMP W (R ,regnum:free-pointer) ,reg:compiled-memtop)
	 (JGE (@PCR ,gc-label)))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (make-external-label (continuation-code-word internal-label)
		       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   entry:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label procedure))
	  (gc-label (generate-label)))
      (LAP (ENTRY-POINT ,external-label)
	   (EQUATE ,external-label ,internal-label)
	   (LABEL ,gc-label)
	   ,@(invoke-interface/call code:compiler-interrupt-ic-procedure)
	   ,@(make-external-label expression-code-word internal-label)
	   (CMP W (R ,regnum:free-pointer) ,reg:compiled-memtop)
	   (JGE (@PCR ,gc-label))))))

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

(define (generate/closure-header internal-label nentries entry)
  nentries				; ignored
  (let ((rtl-proc (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label rtl-proc)))
      (if (zero? nentries)
	  (LAP (EQUATE ,external-label ,internal-label)
	       ,@(simple-procedure-header
		  (internal-procedure-code-word rtl-proc)
		  internal-label
		  entry:compiler-interrupt-procedure))
	  (LAP (LABEL ,gc-label)
	       ,@(if (zero? entry)
		     (LAP)
		     (LAP (ADD W (@R ,esp) (& ,(* 10 entry)))))
	       (JMP ,entry:compiler-interrupt-closure)
	       ,@(make-external-label internal-entry-code-word
				      external-label)
	       (ADD W (@R ,esp)
		    (&U ,(generate/make-magic-closure-constant entry)))
	       (LABEL ,internal-label)
	       (CMP W (R ,regnum:free-pointer) ,reg:compiled-memtop)
	       (JGE (@PCR ,gc-label)))))))

(define (generate/make-magic-closure-constant entry)
  (- (make-non-pointer-literal (ucode-type compiled-entry) 0)
     (+ (* entry 10) 5)))

(define (make-closure-longword code-word pc-offset)
  (+ code-word (* #x20000 pc-offset)))

(define (make-closure-code-longword frame/min frame/max pc-offset)
  (make-closure-longword (make-procedure-code-word frame/min frame/max)
			 pc-offset))			 

(define (generate/cons-closure target procedure-label min max size)
  (let* ((target (target-register-reference))
	 (temporary (temporary-register-reference)))
    (LAP ,@(load-pc-relative-address
	    temporary
	    `(- ,(rtl-procedure/external-label (label->object procedure-label))
		5))
	 (MOV W (@R ,regnum:free-pointer)
	      (&U ,(make-non-pointer-literal (ucode-type manifest-closure)
					     (+ 3 size))))
	 (MOV W (@RO ,regnum:free-pointer 4)
	      (&U ,(make-closure-code-longword min max 8)))
	 (LEA ,target (@RO ,regnum:fre-pointer 8))
	 (MOV B (@RO ,regnum:free-pointer 8) (&U #xe8))	; (CALL (@PCR <entry>))
	 (SUB W ,temporary ,target)
	 (MOV L (@RO ,regnum:free-pointer 9) ,temporary) ; displacement
	 (ADD W (R ,regnum:free-pointer) (& ,(* 4 (+ 4 size)))))))

(define (generate/cons-multiclosure target nentries size entries)
  (let* ((target (target-register-reference))
	 (temp (temporary-register-reference)))
    (with-pc-relative-address
      (lambda (pc-label pc-reg)
	(define (generate-entries entries offset)
	  (let ((entry (car entries))
		(rest (cdr entries)))
	    (LAP (MOV W (@RO ,regnum:free-pointer -9)
		      (&U ,(make-closure-code-longword (cadr entry)
						       (caddr entry)
						       offset)))
		 (MOV B (@RO ,regnum:free-pointer -5) (&U #xe8))
		 (LEA ,temp (@RO ,pc-reg (- ,(rtl-procedure/external-label
					      (label->object (car entry)))
					    ,pc-label)))
		 (SUB W ,temp (R ,regnum:free-pointer))
		 (MOV W (@RO ,regnum:free-pointer -4) ,temp)
		 ,@(if (null? rest)
		       (LAP)
		       (LAP (ADD W (R ,regnum:free-pointer) 10)
			    ,@(generate-entries rest (+ 10 offset)))))))

	(LAP (MOV W (@R ,regnum:free-pointer)
		  (&U ,(make-non-pointer-literal
			(ucode-type manifest-closure)
			(+ size
			   (quotient (+ 3 (* 5 nentries))
				     2)))))
	     (MOV W (@RO ,regnum:free-pointer 4)
		  (&U ,(make-closure-longword nentries 0)))
	     (LEA ,target (@RO ,regnum:free-pointer 12))
	     (ADD W (R ,regnum:free-pointer) (& 17))
	     ,@(generate-entries entries 12)
	     (ADD W (R ,regnum:free-pointer)
		  (& ,(+ (* 4 size) (if (odd? nentries) 3 1)))))))))

;;;; The rules themselves.

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  (generate/closure-header internal-label nentries entry))

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
     (let ((target (target-register-reference)))
       (LAP (MOV W ,target (R ,regnum:free-pointer))
	    (MOV W (@R ,regnum:free-pointer)
		 (&U ,(make-non-pointer-literal (ucode-type manifest-vector)
						size)))
	    (ADD W (R ,regnum:free-pointer) (& (* 4 (1+ size)))))))
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

;; **** here ****

(define (generate/quotation-header environment-label free-ref-label n-sections)
  (LAP (LEA (@PCR ,environment-label) (A 0))
       (MOV L ,reg:environment (@A 0))
       (LEA (@PCR ,*block-label*) (A 0))
       (MOV L (A 0) (D 2))
       (LEA (@PCR ,free-ref-label) (A 0))
       (MOV L (A 0) (D 3))
       ,(load-dnl n-sections 4)
       (JSR ,entry:compiler-link)
       ,@(make-external-label (continuation-code-word false)
			      (generate-label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (let ((load-offset
	 (lambda (offset)
	   (if (<= -32768 offset 32767)
	       (INST (LEA (@AO 0 ,offset) (A 1)))
	       (INST (LEA (@AOF 0 E (,offset L) #F
				((D 0) L 1) Z
				(0 N))
			  (A 1)))))))
    (LAP (MOV L (@PCR ,code-block-label) (D 2))
	 (AND L ,mask-reference (D 2))
	 (MOV L (D 2) (A 0))
	 ,(load-offset environment-offset)
	 (MOV L ,reg:environment (@A 1))
	 ,(load-offset free-ref-offset)
	 (MOV L (A 1) (D 3))
	 ,(load-dnl n-sections 4)
	 (JSR ,entry:compiler-link)
	 ,@(make-external-label (continuation-code-word false)
				(generate-label)))))

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
	(cons (cons name (cdar assoc)) 		; uuo-label
	      (cons (cons (caar assoc)		; frame-size
			  (allocate-constant-label))
		    (inner name (cdr assoc))))))
  (if (null? uuos)
      '()
      (inner (caar uuos) (cdar uuos))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
