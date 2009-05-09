#| -*-Scheme-*-

$Id: rules3.scm,v 1.18 2008/01/30 20:01:46 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (pop-return)
  (LAP ,@(clear-map!)
       ,(c:pop-return)))

(define-rule statement
  (POP-RETURN)
  (pop-return))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,(c:brace-group (c:decl 'sobj 'procedure (c:pop))
		       (c:invoke-interface-2 code:compiler-apply
					     'procedure
					     frame-size))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		;ignore
  (LAP ,@(clear-map!)
       ,(c:goto label)))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		;ignore
  (pop-return))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,(c:brace-group (c:decl 'sobj*
			       'procedure_address
			       (c:cptr (label->offset label)))
		       (c:invoke-interface-2 code:compiler-lexpr-apply
					     'procedure_address
					     number-pushed))))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				;ignore
  ;; Destination address is at TOS; pop it into second-arg
  (LAP ,@(clear-map!)
       ,(c:brace-group (c:decl 'sobj 'procedure (c:pop))
		       (c:decl 'sobj*
			       'procedure_address
			       (c:object-address 'procedure))
		       (c:invoke-interface-2 code:compiler-lexpr-apply
					     'procedure_address
					     number-pushed))))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,(c:jump (c:cast 'sobj*
			(c:cref (free-uuo-link-label name frame-size))))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,(c:jump (c:cast 'sobj*
			(c:cref (global-uuo-link-label name frame-size))))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (REGISTER (? extension)))
  continuation				;ignore
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-3 code:compiler-cache-reference-apply
				extension
				'current_block
				frame-size))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (REGISTER (? environment))
		     (? name))
  continuation				;ignore
  (let ((environment (standard-source! environment 'SCHEME_OBJECT)))
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-3 code:compiler-lookup-apply
				environment
				(c:cref (object->offset name))
				frame-size))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				;ignore
  (LAP ,@(clear-map!)
       ,(if (eq? primitive compiled-error-procedure)
	    (c:invoke-interface-1 code:compiler-error frame-size)
	    (let ((prim (c:cref (object->offset primitive)))
		  (arity (primitive-procedure-arity primitive))
		  (nargs (- frame-size 1)))
	      (if (= arity nargs)
		  (c:invoke-primitive prim arity)
		  (begin
		    (if (not (= arity -1))
			(warn "Wrong number of arguments to primitive:"
			      primitive nargs arity))
		    (c:invoke-interface-2 code:compiler-apply
					  prim
					  frame-size)))))))

(define (invoke-special-primitive code)
  (LAP ,@(clear-map!)
       ,(c:invoke-interface-0 code)))

(let-syntax
    ((define-special-primitive-invocation
       (sc-macro-transformer
	(lambda (form environment)
	  (let ((name (cadr form)))
	    `(DEFINE-RULE STATEMENT
	       (INVOCATION:SPECIAL-PRIMITIVE
		(? FRAME-SIZE)
		(? CONTINUATION)
		,(make-primitive-procedure name #t))
	       FRAME-SIZE CONTINUATION
	       (INVOKE-SPECIAL-PRIMITIVE
		,(close-syntax (symbol-append 'CODE:COMPILER- name)
			       environment))))))))
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
  (define-special-primitive-invocation negative?)
  (define-special-primitive-invocation quotient)
  (define-special-primitive-invocation remainder))

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
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER ,regnum:stack-pointer))
  (LAP))

(define-rule statement
  ;; Move <frame-size> words back to dynamic link marker
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? new-frame)))
  (let ((new-frame (standard-source! new-frame 'SCHEME_OBJECT*)))
    (LAP ,(move-frame-up frame-size new-frame))))

(define (move-frame-up frame-size new-frame)
  (case frame-size
    ((0)
     (c:group (c:= (c:sp-reg) new-frame)))
    ((1)
     (c:group (c:= (c:* (c:predec new-frame)) (c:sref 0))
	      (c:= (c:sp-reg) new-frame)))
    ((2)
     (c:group (c:= (c:* (c:predec new-frame)) (c:sref 1))
	      (c:= (c:* (c:predec new-frame)) (c:sref 0))
	      (c:= (c:sp-reg) new-frame)))
    ((3)
     (c:group (c:= (c:* (c:predec new-frame)) (c:sref 2))
	      (c:= (c:* (c:predec new-frame)) (c:sref 1))
	      (c:= (c:* (c:predec new-frame)) (c:sref 0))
	      (c:= (c:sp-reg) new-frame)))
    (else
     (c:brace-group
      (c:decl 'sobj* "MFUp1" (c:sptr frame-size))
      (c:decl 'sobj* "MFUp2" new-frame)
      (c:while (c:> "MFUp1" (c:sp-reg))
	       (c:= (c:* (c:predec "MFUp2"))
		    (c:* (c:predec "MFUp1"))))
      (c:= (c:sp-reg) "MFUp2")))))

;;; DYNAMIC-LINK instructions have a <frame-size>, <new frame end>,
;;; and <current dynamic link> as arguments.  They pop the stack by
;;; removing the lesser of the amount needed to move the stack pointer
;;; back to the <new frame end> or <current dynamic link>.  The last
;;; <frame-size> words on the stack (the stack frame for the procedure
;;; about to be called) are then put back onto the newly adjusted
;;; stack.

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? choice-1))
				  (REGISTER (? choice-2)))
  (let ((choice-1 (standard-source! choice-1 'SCHEME_OBJECT*))
	(choice-2 (standard-source! choice-2 'SCHEME_OBJECT*)))
    (LAP ,(c:brace-group
	   (c:decl 'sobj* "IPDLp1"
		   (c:?: (c:<= choice-1 choice-2)
			 choice-1
			 choice-2))
	   (move-frame-up frame-size "IPDLp1")))))

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

(define (simple-procedure-header code-word label e-label code)
  (declare-block-label! code-word label e-label)
  (let ((block-label (label->offset label)))
    (LAP ,@(if (not e-label)
	       (LAP)
	       (label-statement e-label))
	 ,@(label-statement label)
	 ,(c:interrupt-check code block-label))))

(define (dlink-procedure-header code-word label e-label)
  (declare-block-label! code-word label e-label)
  (let ((block-label (label->offset label)))
    (LAP ,@(if (not e-label)
	       (LAP)
	       (label-statement e-label))
	 ,@(label-statement label)
	 ,(c:dlink-interrupt-check block-label))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (declare-block-label! (continuation-code-word internal-label)
			internal-label #f)
  (label-statement internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   #f
			   code:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (simple-procedure-header expression-code-word
			   internal-label
			   (rtl-procedure/external-label
			    (label->object internal-label))
			   code:compiler-interrupt-ic-procedure))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let* ((rtl-proc (label->object internal-label))
	 (external-label (rtl-procedure/external-label rtl-proc)))
    ((if (rtl-procedure/dynamic-link? rtl-proc)
	 dlink-procedure-header 
	 (lambda (code-word label external-label)
	   (simple-procedure-header code-word label external-label
				    code:compiler-interrupt-procedure)))
     (internal-procedure-code-word rtl-proc)
     internal-label external-label)))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (simple-procedure-header (make-procedure-code-word min max)
			   internal-label
			   (rtl-procedure/external-label
			    (label->object internal-label))
			   code:compiler-interrupt-procedure))

;;;; Closures.

;; Magic for compiled entries.

(define-integrable (label-statement label)
  (lap:make-label-statement label))

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  entry
  (if (zero? nentries)
      (error "Closure header for closure with no entries!"
	     internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label rtl-proc)))
      (declare-block-label! (internal-procedure-code-word rtl-proc)
			    #f external-label)
      (LAP ,@(label-statement external-label)
	   ,(c:scall "CLOSURE_HEADER" (label->offset external-label))
	   ,@(label-statement internal-label)
	   ,(c:closure-interrupt-check)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? nvars)))
  (cons-closure target procedure-label min max nvars))

(define (cons-closure target label min max nvars)
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= (c:* (c:postinc (c:free-reg)))
	       (c:make-object "TC_MANIFEST_CLOSURE"
			      (+ closure-entry-size nvars)))
	 ,(c:= target (c:+ (c:free-reg) 1))
	 ,@(write-closure-entry label min max 2)
	 ,(c:+= (c:free-reg) nvars))))

(define (write-closure-entry internal-label min max offset)
  (let ((external-label
	 (rtl-procedure/external-label (label->object internal-label))))
    (LAP ,(c:+= (c:free-reg) 1)
	 ,(c:scall "WRITE_LABEL_DESCRIPTOR"
		   (c:free-reg)
		   (c:hex (make-procedure-code-word min max))
		   offset)
	 ,(c:= (c:* (c:postinc (c:free-reg)))
	       (c:+ 'dispatch_base (label->dispatch-tag external-label)))
	 ,(c:= (c:* (c:postinc (c:free-reg)))
	       (c:cast 'sobj (c:cptr (label->offset external-label)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? nvars) (? entries)))
  ;; entries is a vector of all the entry points
  (case nentries
    ((0)
     (let ((dest (standard-target! target 'SCHEME_OBJECT*)))
       (LAP ,(c:= dest (c:free-reg))
	    ,(c:= (c:* (c:postinc (c:free-reg)))
		  (c:make-object "TC_MANIFEST_VECTOR" nvars))
	    ,(c:+= (c:free-reg) nvars))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (cons-closure target (car entry) (cadr entry) (caddr entry) nvars)))
    (else
     (cons-multiclosure target nentries nvars (vector->list entries)))))

(define (cons-multiclosure target nentries nvars entries)
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= (c:* (c:postinc (c:free-reg)))
	       (c:make-object "TC_MANIFEST_CLOSURE"
			      (+ 1 (* nentries closure-entry-size) nvars)))
	 ,(c:+= (c:free-reg) 1)
	 ,(c:scall "WRITE_LABEL_DESCRIPTOR" (c:free-reg) nentries 0)
	 ,(c:= target (c:+ (c:free-reg) 1))
	 ,@(reduce-right
	    (lambda (lap1 lap2)
	      (LAP ,@lap1 ,@lap2))
	    (LAP)
	    (map (lambda (entry offset)
		   (write-closure-entry (car entry)
					(cadr entry)
					(caddr entry)
					offset))
		 entries
		 (make-multiclosure-offsets nentries)))
	 ,(c:+= (c:free-reg) nvars))))
	 
(define (make-multiclosure-offsets nentries)
  (let generate ((n nentries) (offset 3))
    (if (> n 0)
	(cons offset (generate (- n 1) (+ offset closure-entry-size)))
	'())))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label
				   free-ref-offset n-sections)
  (let ((label (generate-label)))
    (declare-block-label! (continuation-code-word false) false label)
    (LAP ,(c:= (c:cref environment-label)
	       (c:env-reg))
	 ,(c:invoke-interface-4 code:compiler-link
				(c:cptr (label->offset label))
				'current_block
				(c:cptr free-ref-offset)
				n-sections)
	 ,@(label-statement label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (let ((label (generate-label)))
    (add-remote-link! code-block-label)
    (declare-block-label! (continuation-code-word false) false label)
    (LAP ,(c:brace-group
	   (c:decl 'sobj*
		   'sub_block
		   (c:object-address (c:cref code-block-label)))
	   (c:= (c:aref 'sub_block environment-offset)
		(c:env-reg))
	   (c:invoke-interface-4 code:compiler-link
				 (c:cptr (label->offset label))
				 'sub_block
				 (c:aptr 'sub_block free-ref-offset)
				 n-sections))
	 ,@(label-statement label))))

(define (add-remote-link! label)
  (if (not *purification-root-object*)
      (set! *purification-root-object*
	    (cons *purification-root-marker* '())))
  (set-cdr! *purification-root-object*
	    (cons (object-label-value label)
		  (cdr *purification-root-object*)))
  unspecific)

(define *purification-root-marker*
  (intern "#[PURIFICATION-ROOT]"))

(define (generate/remote-links n-code-blocks code-blocks-label n-sections)
  (let ((label (generate-label))
	(done (generate-label)))
    (set! *purification-root-object*
	  (cons *purification-root-marker*
		(object-label-value code-blocks-label)))
    (declare-block-label! (continuation-code-word false) false label)
    (LAP ,(c:push (c:ecall "ULONG_TO_FIXNUM" (c:cast 'ulong 1)))
	 ,@(label-statement label)
	 ,(c:brace-group
	   (c:array-decl "static const short"
			 'sections
			 ""
			 (cons 0 (vector->list n-sections)))
	   (c:decl 'ulong 'counter (c:object-datum (c:tos)))
	   (c:decl 'sobj 'blocks)
	   (c:decl 'sobj* 'sub_block)
	   (c:decl 'short 'section)
	   (c:if-goto (c:> 'counter n-code-blocks) done)
	   (c:= 'blocks (c:cref code-blocks-label))
	   (c:= 'sub_block
		(c:object-address (c:ecall "MEMORY_REF" 'blocks 'counter)))
	   (c:= (c:aref 'sub_block (c:object-datum (c:aref 'sub_block 0)))
		(c:env-reg))
	   (c:= 'section (c:aref 'sections 'counter))
	   (c:= (c:tos) (c:ecall "ULONG_TO_FIXNUM" (c:+ 'counter 1)))
	   (c:invoke-interface-4 code:compiler-link
				 (c:cptr (label->offset label))
				 'sub_block
				 (c:+ 'sub_block
				      (c:+ 2
					   (c:object-datum
					    (c:aref 'sub_block 1))))
				 'section))
	 ,@(label-statement done)
	 ,(c:+= (c:sp-reg) 1))))

#|
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
	;; produces ((name . label) (0 . label) ... (frame-size . label) ...)
        ;; where the (0 . label) is repeated to fill out the size required
        ;; as specified in machin.scm
	`((,name . ,(cdar assoc))		; uuo-label
	  (,(caar assoc) .			; frame-size
			 ,(allocate-constant-label))
	  ,@(inner name (cdr assoc)))))
  (if (null? uuos)
      '()
      ;; caar is name, cdar is alist of frame sizes
      (inner (caar uuos) (cdar uuos))))
|#

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
