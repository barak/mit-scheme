#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules3.scm,v 4.4 1988/02/19 20:58:21 jinx Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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
  (LAP ,@(clear-map!)
       (CLR B (@A 7))
       (RTS)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  (LAP ,@(clear-map!)
       ,(load-dnw frame-size 0)
       (JMP ,entry:compiler-apply)))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  (LAP ,@(clear-map!)
       (BRA (@PCR ,label))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  (LAP ,@(clear-map!)
       ,(load-dnw number-pushed 0)
       (BRA (@PCR ,label))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  (let ((set-extension (expression->machine-register! extension a3)))
    (delete-dead-registers!)
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,(load-dnw frame-size 0)
	 (LEA (@PCR ,*block-start-label*) (A 1))
	 (JMP ,entry:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  (let ((set-environment (expression->machine-register! environment d4)))
    (delete-dead-registers!)
    (LAP ,@set-environment
	 ,@(clear-map!)
	 ,(load-constant name (INST-EA (D 5)))
	 ,(load-dnw frame-size 0)
	 (JMP ,entry:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  (LAP ,@(clear-map!)
       ,(load-dnw frame-size 0)
       (MOV L (@PCR ,(free-uuo-link-label name)) (D 1))
       (MOV L (D 1) (@-A 7))
       (AND L (D 7) (D 1))
       (MOV L (D 1) (A 1))
       (MOV L (@A 1) (D 1))
       (AND L (D 7) (D 1))
       (MOV L (D 1) (A 0))
       (JMP (@A 0))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  (LAP ,@(clear-map!)
       ,@(if (eq? primitive compiled-error-procedure)
	     (LAP ,(load-dnw frame-size 0)
		  (JMP ,entry:compiler-error))
	     (let ((arity (primitive-procedure-arity primitive)))
	       (cond ((not (negative? arity))
		      (LAP (MOV L (@PCR ,(constant->label primitive)) (D 6))
			   (JMP ,entry:compiler-primitive-apply)))
		     ((= arity -1)
		      (LAP (MOV L (& ,(-1+ frame-size))
				,reg:lexpr-primitive-arity)
			   (MOV L (@PCR ,(constant->label primitive)) (D 6))
			   (JMP ,entry:compiler-primitive-lexpr-apply)))
		     (else
		      ;; Unknown primitive arity.  Go through apply.
		      (LAP ,(load-dnw frame-size 0)
			   (MOV L (@PCR ,(constant->label primitive)) (@-A 7))
			   (JMP ,entry:compiler-apply))))))))

(let-syntax
    ((define-special-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    ,(list 'LAP
		   (list 'UNQUOTE-SPLICING '(clear-map!))
		   (list 'JMP
			 (list 'UNQUOTE
			       (symbol-append 'ENTRY:COMPILER- name))))))))
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

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 15))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 15) (? offset)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (increment-anl 7 how-far))
	  ((= frame-size 1)
	   (LAP (MOV L (@A+ 7) ,(offset-reference a7 (-1+ how-far)))
		,@(increment-anl 7 (-1+ how-far))))
	  ((= frame-size 2)
	   (if (= how-far 1)
	       (LAP (MOV L (@AO 7 4) (@AO 7 8))
		    (MOV L (@A+ 7) (@A 7)))
	       (let ((i (lambda ()
			  (INST (MOV L (@A+ 7)
				     ,(offset-reference a7 (-1+ how-far)))))))
		 (LAP ,(i)
		      ,(i)
		      ,@(increment-anl 7 (- how-far 2))))))
	  (else
	   (generate/move-frame-up frame-size (offset-reference a7 offset))))))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER (? base))
						   (? offset)))
  (QUALIFIER (pseudo-register? base))
  (generate/move-frame-up frame-size (indirect-reference! base offset)))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK 0 (REGISTER 15) (REGISTER 12))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OFFSET-ADDRESS (REGISTER (? base))
						  (? offset))
				  (REGISTER 12))
  (let ((label (generate-label))
	(temp (allocate-temporary-register! 'ADDRESS)))
    (let ((temp-ref (register-reference temp)))
      (LAP (LEA ,(indirect-reference! base offset) ,temp-ref)
	   (CMP L ,temp-ref (A 4))
	   (B HS B (@PCR ,label))
	   (MOV L (A 4) ,temp-ref)
	   (LABEL ,label)
	   ,@(generate/move-frame-up* frame-size temp)))))

(define (generate/move-frame-up frame-size destination)
  (let ((temp (allocate-temporary-register! 'ADDRESS)))
    (LAP (LEA ,destination ,(register-reference temp))
	 ,@(generate/move-frame-up* frame-size temp))))

(define (generate/move-frame-up* frame-size destination)
  (let ((temp (allocate-temporary-register! 'ADDRESS)))
    (LAP (LEA ,(offset-reference a7 frame-size) ,(register-reference temp))
	 ,@(generate-n-times
	    frame-size 5
	    (lambda ()
	      (INST (MOV L
			 (@-A ,(- temp 8))
			 (@-A ,(- destination 8)))))
	    (lambda (generator)
	      (generator (allocate-temporary-register! 'DATA))))
	 (MOV L ,(register-reference destination) (A 7)))))

;;;; Entry Headers

(define generate/quotation-header
  ;; This is invoked by the top level of the LAP generator.
  (let ((declare-constants
	 (lambda (constants code)
	   (define (inner constants)
	     (if (null? constants)
		 code
		 (let ((entry (car constants)))
		   (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
			,@(inner (cdr constants))))))
	   (inner constants)))
	(declare-references
	 (lambda (references entry:single entry:multiple)
	   (if (null? references)
	       (LAP)
	       (LAP (LEA (@PCR ,(cdar references)) (A 1))
		    ,@(if (null? (cdr references))
			  (LAP (JSR ,entry:single))
			  (LAP ,(load-dnw (length references) 1)
			       (JSR ,entry:multiple)))
		    ,@(make-external-label (generate-label)))))))
    (lambda (block-label constants references assignments uuo-links)
      (declare-constants uuo-links
       (declare-constants references
	(declare-constants assignments
	 (declare-constants constants
	  (let ((debugging-information-label (allocate-constant-label))
		(environment-label (allocate-constant-label)))
	    (LAP
	     ;; Place holder for the debugging info filename
	     (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	     (SCHEME-OBJECT ,environment-label ENVIRONMENT)
	     (LEA (@PCR ,environment-label) (A 0))
	     ,@(if (and (null? references)
			(null? assignments)
			(null? uuo-links))
		   (LAP ,(load-constant 0 '(@A 0)))
		   (LAP (MOV L ,reg:environment (@A 0))
			(LEA (@PCR ,block-label) (A 0))
			,@(declare-references
			   references
			   entry:compiler-cache-variable
			   entry:compiler-cache-variable-multiple)
			,@(declare-references
			   assignments
			   entry:compiler-cache-assignment
			   entry:compiler-cache-assignment-multiple)
			,@(declare-references
			   uuo-links
			   entry:compiler-uuo-link
			   entry:compiler-uuo-link-multiple))))))))))))

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.

(define-rule statement
  (PROCEDURE-HEAP-CHECK (? label))
  (let ((gc-label (generate-label)))
    (LAP ,@(procedure-header (label->object label) gc-label)
	 (CMP L ,reg:compiled-memtop (A 5))
	 (B GE B (@PCR ,gc-label)))))

;;; Note: do not change the MOVE.W in the setup-lexpr call to a MOVEQ.
;;; The setup-lexpr code assumes a fixed calling sequence to compute
;;; the GC address if that is needed.  This could be changed so that
;;; the microcode determined how far to back up based on the argument,
;;; or by examining the calling sequence.

(define-rule statement
  (SETUP-LEXPR (? label))
  (let ((procedure (label->object label)))
    (LAP ,@(procedure-header procedure false)
	 (MOV W
	      (& ,(+ (rtl-procedure/n-required procedure)
		     (rtl-procedure/n-optional procedure)
		     (if (rtl-procedure/closure? procedure) 1 0)))
	      (D 1))
	 (MOVEQ (& ,(if (rtl-procedure/rest? procedure) 1 0)) (D 2))
	 (JSR ,entry:compiler-setup-lexpr))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? internal-label))
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSR ,entry:compiler-interrupt-continuation)
	 ,@(make-external-label internal-label)
	 (CMP L ,reg:compiled-memtop (A 5))
	 (B GE B (@PCR ,gc-label)))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (LAP ,@(make-external-label internal-label)))

(define (procedure-header procedure gc-label)
  (let ((internal-label (rtl-procedure/label procedure))
	(external-label (rtl-procedure/external-label procedure)))
    (LAP ,@(case (rtl-procedure/type procedure)
	     ((IC)
	      (LAP (ENTRY-POINT ,external-label)
		   (EQUATE ,external-label ,internal-label)))
	     ((CLOSURE)
	      (let ((required (1+ (rtl-procedure/n-required procedure)))
		    (optional (rtl-procedure/n-optional procedure)))
		(LAP (ENTRY-POINT ,external-label)
		     ,@(make-external-label external-label)
		     ,(test-dnw required 0)
		     ,@(cond ((rtl-procedure/rest? procedure)
			      (LAP (B GE B (@PCR ,internal-label))))
			     ((zero? optional)
			      (LAP (B EQ B (@PCR ,internal-label))))
			     (else
			      (let ((wna-label (generate-label)))
				(LAP (B LT B (@PCR ,wna-label))
				     ,(test-dnw (+ required optional) 0)
				     (B LE B (@PCR ,internal-label))
				     (LABEL ,wna-label)))))
		     (JMP ,entry:compiler-wrong-number-of-arguments))))
	     (else (LAP)))
	 ,@(if gc-label
	       (LAP (LABEL ,gc-label)
		    (JSR ,entry:compiler-interrupt-procedure))
	       (LAP))
	 ,@(make-external-label internal-label))))

(define (make-external-label label)
  (set! compiler:external-labels 
	(cons label compiler:external-labels))
  (LAP (BLOCK-OFFSET ,label)
       (LABEL ,label)))