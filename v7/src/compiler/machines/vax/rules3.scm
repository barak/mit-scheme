#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rules3.scm,v 1.0 1988/01/05 16:07:13 bal Exp $

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

;;;; VAX LAP Generation Rules: Invocations and Entries
;;;  Matches MC68020 version 1.13

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? prefix) (? continuation))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-rnw frame-size 0)
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
	(BR (@PCR ,label)))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? label))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-rnw number-pushed 0)
	(BR (@PCR ,label)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? prefix) (? continuation)
			      (? extension))
  (disable-frame-pointer-offset!
   (let ((set-extension (expression->machine-register! extension r9)))
     (delete-dead-registers!)
     (LAP ,@set-extension
	  ,@(generate-invocation-prefix prefix (list r9))
	  ,(load-rnw frame-size 0)
	  (MOVA B (@PCR ,*block-start-label*) (R 8))
	  (JMP ,entry:compiler-cache-reference-apply)))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? prefix) (? continuation)
		     (? environment) (? name))
  (disable-frame-pointer-offset!
   (let ((set-environment (expression->machine-register! environment r8)))
     (delete-dead-registers!)
     (LAP ,@set-environment
	  ,@(generate-invocation-prefix prefix (list r8))
	  ,(load-constant name (INST-EA (R 9)))
	  ,(load-rnw frame-size 0)
	  (JMP ,entry:compiler-lookup-apply)))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? prefix) (? continuation)
			(? primitive))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,@(if (eq? primitive compiled-error-procedure)
	      (LAP ,(load-rnw frame-size 0)
		   (JMP ,entry:compiler-error))
	      (LAP ,(load-rnw (primitive-datum primitive) 8)
		   (JMP ,entry:compiler-primitive-apply))))))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? prefix) (? continuation) (? name))
  (disable-frame-pointer-offset!
   (LAP ,@(generate-invocation-prefix prefix '())
	,(load-rnw frame-size 0)
	(MOV L (@PCR ,(free-uuo-link-label name)) (R 1))
	(PUSHL (R 1))
	(BIC L (R 11) (R 1))
	(BIC L (R 11) (@R 1) (R 1))
	(JMP (@R 1)))))

(define-rule statement
  (RETURN)
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	(CLR B (@RO B 14 3))
	(RSB))))

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
	 (increment-rnl 14 how-far))
	((= frame-size 1)
	 (LAP (MOV L (@R+ 14) ,(offset-reference r14 (-1+ how-far)))
	      ,@(increment-rnl 14 (-1+ how-far))))
	((= frame-size 2)
	 (if (= how-far 1)
	     (LAP (MOV L (@RO B 14 4) (@RO B 14 8))
		  (MOV L (@R+ 14) (@R 14)))
	     (let ((i (lambda ()
			(INST (MOV L (@R+ 14)
				     ,(offset-reference r14 (-1+ how-far)))))))
	       (LAP ,(i)
		    ,(i)
		    ,@(increment-rnl 14 (- how-far 2))))))
	(else
	 (let ((temp-0 (allocate-temporary-register! 'GENERAL))
	       (temp-1 (allocate-temporary-register! 'GENERAL)))
	   (LAP (MOVA L ,(offset-reference r14 frame-size)
		        ,(register-reference temp-0))
		(MOVA L ,(offset-reference r14 (+ frame-size how-far))
		        ,(register-reference temp-1))
		,@(generate-n-times
		   frame-size 5
		   (lambda ()
		     (INST (MOV L
				(@-R ,temp-0)
				(@-R ,temp-1))))
		   (lambda (generator)
		     (generator (allocate-temporary-register! 'GENERAL))))
		(MOV L ,(register-reference temp-1) (R 14)))))))

;;; This is invoked by the top level of the LAP GENERATOR.

(define generate/quotation-header
  (let ()
    (define (initialize block-label environment-label references uuo-links)
      (define (initialize-references references entry:single entry:multiple)
	(if (null? references)
	    (LAP)
	    (LAP (MOVA L (@PCR ,(cdar references)) (R 9))
		 ,@(if (null? (cdr references))
		       (LAP (JSB ,entry:single))
		       (LAP ,(load-rnw (length references) 7)
			    (JSB ,entry:multiple)))
		 ,@(make-external-label (generate-label)))))

      (if (and (null? references) (null? uuo-links))
	  (LAP ,(load-constant 0 (INST-EA (@PCR ,environment-label))))
	  (LAP (MOV L ,reg:environment (@PCR ,environment-label))
	       (MOVA B (@PCR ,block-label) (R 8))
	       ,@(initialize-references references
					entry:compiler-cache-variable
					entry:compiler-cache-variable-multiple)
	       ,@(initialize-references uuo-links
					entry:compiler-uuo-link
					entry:compiler-uuo-link-multiple))))

    (define (declare-constants constants code)
      (define (inner constants)
	(if (null? constants)
	    code
	    (let ((entry (car constants)))
	      (LAP (SCHEME-OBJECT ,(cdr entry) ,(car entry))
		   ,@(inner (cdr constants))))))
      (inner constants))

    (lambda (block-label constants references uuo-links)
      (declare-constants references
       (declare-constants uuo-links
	(declare-constants constants
	 (LAP
	  ;; Place holder for the debugging info filename
	  ,@(let ((environment-label (allocate-constant-label))
		  (debugging-information-label (allocate-constant-label)))
	      (LAP (SCHEME-OBJECT ,debugging-information-label
				  DEBUGGING-INFO)
		   (SCHEME-OBJECT ,environment-label ENVIRONMENT)
		   ,@(initialize block-label
				 environment-label
				 references
				 uuo-links))))))))))

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
	  (CMP L ,reg:compiled-memtop (R 12))
	  ;; *** LEQU ? ***
	  (B B LEQ (@PCR ,gc-label))))))

;;; Note: do not change the (& ,mumble) in the setup-lexpr call to a
;;; (S ,mumble).  The setup-lexpr code assumes a fixed calling
;;; sequence to compute the GC address if that is needed.  This could
;;; be changed so that the microcode determined how far to back up
;;; based on the argument, or by examining the calling sequence.

(define-rule statement
  (SETUP-LEXPR (? label))
  (disable-frame-pointer-offset!
   (let ((procedure (label->procedure label)))
     (LAP ,@(procedure-header procedure false)
	  (MOV W
	       (& ,(+ (procedure-required procedure)
		      (procedure-optional procedure)
		      (if (procedure/closure? procedure) 1 0)))
	       (R 1))
	  (MOV L (S ,(if (procedure-rest procedure) 1 0)) (R 2))
	  (JSB ,entry:compiler-setup-lexpr)))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? internal-label))
  (enable-frame-pointer-offset!
   (continuation-frame-pointer-offset (label->continuation internal-label)))
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSB ,entry:compiler-interrupt-continuation)
	 ,@(make-external-label internal-label)
	 (CMP L ,reg:compiled-memtop (R 12))
	 ;; *** LEQU ? ***
	 (B B LEQ (@PCR ,gc-label)))))

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
		     ,(test-rnw required 0)
		     ,@(cond ((procedure-rest procedure)
			      (LAP (B B GEQ (@PCR ,internal-label))))
			     ((zero? optional)
			      (LAP (B B EQL (@PCR ,internal-label))))
			     (else
			      (let ((wna-label (generate-label)))
				(LAP (B B LSS (@PCR ,wna-label))
				     ,(test-rnw (+ required optional) 0)
				     (B B LEQ (@PCR ,internal-label))
				     (LABEL ,wna-label)))))
		     (JMP ,entry:compiler-wrong-number-of-arguments))))
	     (else (LAP)))
	 ,@(if gc-label
	       (LAP (LABEL ,gc-label)
		    (JSB ,entry:compiler-interrupt-procedure))
	       (LAP))
	 ,@(make-external-label internal-label))))

(define (make-external-label label)
  (set! compiler:external-labels 
	(cons label compiler:external-labels))
  (LAP (BLOCK-OFFSET ,label)
       (LABEL ,label)))
