#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules3.scm,v 1.15 1987/10/05 20:38:51 jinx Exp $

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
  (INVOCATION:APPLY (? frame-size) (? prefix) (? continuation))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-dnw frame-size 0)
	(JMP ,entry:compiler-apply))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-CLOSURE (? frame-size) (? receiver-offset))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(apply-closure-sequence frame-size receiver-offset label))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-STACK (? frame-size) (? receiver-offset)
				(? n-levels))
		   (? continuation) (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(apply-stack-sequence frame-size receiver-offset n-levels label))))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? prefix) (? continuation) (? label))
  (QUALIFIER (not (memq (car prefix) '(APPLY-CLOSURE APPLY-STACK))))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	(BRA (@PCR ,label)))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-dnw number-pushed 0)
	(BRA (@PCR ,label)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? prefix) (? continuation)
			      (? extension))
  (disable-frame-pointer-offset!
   (let ((set-extension (expression->machine-register! extension a3)))
     (delete-dead-registers!)
     (LAP ,@set-extension
	  ,@(generate-invocation-prefix prefix (list a3))
	  ,(load-dnw frame-size 0)
	  (LEA (@PCR ,*block-start-label*) (A 1))
	  (JMP ,entry:compiler-cache-reference-apply)))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? prefix) (? continuation)
		     (? environment) (? name))
  (disable-frame-pointer-offset!
   (let ((set-environment (expression->machine-register! environment d4)))
     (delete-dead-registers!)
     (LAP ,@set-environment
	  ,@(generate-invocation-prefix prefix (list d4))
	  ,(load-constant name (INST-EA (D 5)))
	  ,(load-dnw frame-size 0)
	  (JMP ,entry:compiler-lookup-apply)))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? prefix) (? continuation)
			(? primitive))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,@(if (eq? primitive compiled-error-procedure)
	      (LAP ,(load-dnw frame-size 0)
		   (JMP ,entry:compiler-error))
	      (LAP ,(load-dnw (primitive-datum primitive) 6)
		   (JMP ,entry:compiler-primitive-apply))))))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? prefix) (? continuation) (? name))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-dnw frame-size 0)
	(MOV L (@PCR ,(free-uuo-link-label name)) (D 1))
	(MOV L (D 1) (@-A 7))
	(AND L (D 7) (D 1))
	(MOV L (D 1) (A 1))
	(MOV L (@A 1) (D 1))
	(AND L (D 7) (D 1))
	(MOV L (D 1) (A 0))
	(JMP (@A 0)))))

(let-syntax
    ((define-special-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE ,name (? frame-size)
					  (? prefix) (? continuation))
	    (disable-frame-pointer-offset!
	     ,(list 'LAP
		    (list 'UNQUOTE-SPLICING
			  '(generate-invocation-prefix prefix '()))
		    (list 'JMP
			  (list 'UNQUOTE
				(symbol-append 'ENTRY:COMPILER- name)))))))))

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

(define-rule statement
  (RETURN)
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	(CLR B (@A 7))
	(RTS))))

(define (generate-invocation-prefix prefix needed-registers)
  (let ((clear-map (clear-map!)))
    (need-registers! needed-registers)
    (LAP ,@clear-map
	 ,@(case (car prefix)
	     ((NULL) '())
	     ((MOVE-FRAME-UP)
	      (apply generate-invocation-prefix:move-frame-up (cdr prefix)))
	     ((APPLY-CLOSURE)
	      (apply generate-invocation-prefix:apply-closure (cdr prefix)))
	     ((APPLY-STACK)
	      (apply generate-invocation-prefix:apply-stack (cdr prefix)))
	     (else
	      (error "bad prefix type" prefix))))))

(define (generate-invocation-prefix:apply-closure frame-size receiver-offset)
  (let ((label (generate-label)))
    (LAP ,@(apply-closure-sequence frame-size receiver-offset label)
	 (LABEL ,label))))

(define (generate-invocation-prefix:apply-stack frame-size receiver-offset
						n-levels)
  (let ((label (generate-label)))
    (LAP ,@(apply-stack-sequence frame-size receiver-offset n-levels label)
	 (LABEL ,label))))

(define (generate-invocation-prefix:move-frame-up frame-size how-far)
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
	 (let ((temp-0 (allocate-temporary-register! 'ADDRESS))
	       (temp-1 (allocate-temporary-register! 'ADDRESS)))
	   (LAP (LEA ,(offset-reference a7 frame-size)
		     ,(register-reference temp-0))
		(LEA ,(offset-reference a7 (+ frame-size how-far))
		     ,(register-reference temp-1))
		,@(generate-n-times
		   frame-size 5
		   (lambda ()
		     (INST (MOV L
				(@-A ,(- temp-0 8))
				(@-A ,(- temp-1 8)))))
		   (lambda (generator)
		     (generator (allocate-temporary-register! 'DATA))))
		(MOV L ,(register-reference temp-1) (A 7)))))))

;;; This is invoked by the top level of the LAP GENERATOR.

(define generate/quotation-header
  (let ()
    (define (declare-constants constants code)
      (define (inner constants)
	(if (null? constants)
	    code
	    (let ((entry (car constants)))
	      (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
		   ,@(inner (cdr constants))))))
      (inner constants))

    (define (declare-references references entry:single entry:multiple)
      (if (null? references)
	  (LAP)
	  (LAP (LEA (@PCR ,(cdar references)) (A 1))
	       ,@(if (null? (cdr references))
		     (LAP (JSR ,entry:single))
		     (LAP ,(load-dnw (length references) 1)
			  (JSR ,entry:multiple)))
	       ,@(make-external-label (generate-label)))))

    (lambda (block-label constants references assignments uuo-links)
      (declare-constants references
       (declare-constants assignments
	(declare-constants uuo-links
	 (declare-constants
	  constants
	  (let ((debugging-information-label (allocate-constant-label))
		(environment-label (allocate-constant-label)))
	    (LAP
	     ;; Place holder for the debugging info filename
	     (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	     (SCHEME-OBJECT ,environment-label ENVIRONMENT)
	     (LEA (@PCR ,environment-label) (A 0))
	     ,@(if (and (null? references) (null? assignments) (null? uuo-links))
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

;;;; Procedure/Continuation Entries

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.

(define-rule statement
  (PROCEDURE-HEAP-CHECK (? label))
  (disable-frame-pointer-offset!
   (let ((gc-label (generate-label)))
     (LAP ,@(procedure-header (label->procedure label) gc-label)
	  (CMP L ,reg:compiled-memtop (A 5))
	  (B GE B (@PCR ,gc-label))))))

;;; Note: do not change the MOVE.W in the setup-lexpr call to a MOVEQ.
;;; The setup-lexpr code assumes a fixed calling sequence to compute
;;; the GC address if that is needed.  This could be changed so that
;;; the microcode determined how far to back up based on the argument,
;;; or by examining the calling sequence.

(define-rule statement
  (SETUP-LEXPR (? label))
  (disable-frame-pointer-offset!
   (let ((procedure (label->procedure label)))
     (LAP ,@(procedure-header procedure false)
	  (MOV W
	       (& ,(+ (procedure-required procedure)
		      (procedure-optional procedure)
		      (if (procedure/closure? procedure) 1 0)))
	       (D 1))
	  (MOVEQ (& ,(if (procedure-rest procedure) 1 0)) (D 2))
	  (JSR ,entry:compiler-setup-lexpr)))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? internal-label))
  (enable-frame-pointer-offset!
   (continuation-frame-pointer-offset (label->continuation internal-label)))
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSR ,entry:compiler-interrupt-continuation)
	 ,@(make-external-label internal-label)
	 (CMP L ,reg:compiled-memtop (A 5))
	 (B GE B (@PCR ,gc-label)))))

(define (procedure-header procedure gc-label)
  (let ((internal-label (procedure-label procedure))
	(external-label (procedure-external-label procedure)))
    (LAP ,@(case (procedure-name procedure) ;really `procedure/type'.
	     ((IC)
	      (LAP (ENTRY-POINT ,external-label)
		   (EQUATE ,external-label ,internal-label)))
	     ((CLOSURE)
	      (let ((required (1+ (procedure-required procedure)))
		    (optional (procedure-optional procedure)))
		(LAP (ENTRY-POINT ,external-label)
		     ,@(make-external-label external-label)
		     ,(test-dnw required 0)
		     ,@(cond ((procedure-rest procedure)
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
