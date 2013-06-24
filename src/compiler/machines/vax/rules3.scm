#| -*-Scheme-*-

$Id: rules3.scm,v 4.16 2003/02/14 18:28:07 cph Exp $

Copyright (c) 1987-1999, 2001, 2002 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; LAP Generation Rules: Invocations and Entries.
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Invocations

(define-integrable (clear-continuation-type-code)
  (LAP (BIC L ,mask-reference (@R 14))))

(define-rule statement
  (POP-RETURN)
  (LAP ,@(clear-map!)
       ,@(clear-continuation-type-code)
       (RSB)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ,@(load-rn frame-size 2)
       #|
       (JMP ,entry:compiler-shortcircuit-apply)
       |#
       (MOV L (@R+ 14) (R 1))
       ,@(invoke-interface code:compiler-apply)
       ;; 'Til here
       ))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation		; ignored
  (LAP ,@(clear-map!)
       (BR (@PCR ,label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation		; ignored
  ;; It expects the procedure at the top of the stack
  (LAP ,@(clear-map!)
       ,@(clear-continuation-type-code)
       (RSB)))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ,@(load-rn number-pushed 2)
       (MOVA B (@PCR ,label) (R 1))
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				; ignored
  ;; It expects the procedure at the top of the stack
  (LAP ,@(clear-map!)
       ,@(load-rn number-pushed 2)
       (BIC L ,mask-reference (@R+ 14) (R 1))
       ,@(invoke-interface code:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ;; The following assumes that at label there is
       ;;	(JMP (L <entry>))
       ;; The other possibility would be
       ;;       (JMP (@@PCR ,(free-uuo-link-label name frame-size)))
       ;; and to have <entry> at label, but it is longer and slower.
       ;; The 2 below accomodates the arrangement between the arity
       ;; and the instructions in an execute cache.
       (BR (@PCRO ,(free-uuo-link-label name frame-size) 2))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation				; ignored
  (LAP ,@(clear-map!)
       (BR (@PCRO ,(global-uuo-link-label name frame-size) 2))))

;;; The following two rules are obsolete.  They haven't been used in a while.
;;; They are provided in case the relevant switches are turned off, but there
;;; is no reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  continuation				; ignored
  (let* ((set-extension 
	  (interpreter-call-argument->machine-register! extension r1))
	 (clear-map (clear-map!)))
    (LAP ,@set-extension
	 ,@clear-map
	 ,@(load-rn frame-size 3)
	 (MOVA B (@PCR ,*block-label*) (R 2))
	 ,@(invoke-interface code:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  continuation				; ignored
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment r1))
	 (clear-map (clear-map!)))
    (LAP ,@set-environment
	 ,@clear-map
	 ,@(load-constant name (INST-EA (R 2)))
	 ,@(load-rn frame-size 3)
	 ,@(invoke-interface code:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ,@(if (eq? primitive compiled-error-procedure)
	     (LAP ,@(load-rn frame-size 1)
		  #|
		  (JMP ,entry:compiler-error)
		  |#
		  ,@(invoke-interface code:compiler-error))
	     (let ((arity (primitive-procedure-arity primitive)))
	       (cond ((not (negative? arity))
		      (LAP (MOV L (@PCR ,(constant->label primitive)) (R 1))
			   #|
			   (JMP ,entry:compiler-primitive-apply)
			   |#
			   ,@(invoke-interface code:compiler-primitive-apply)))
		     ((= arity -1)
		      (LAP (MOV L ,(make-immediate (-1+ frame-size))
				,reg:lexpr-primitive-arity)
			   (MOV L (@PCR ,(constant->label primitive)) (R 1))
			   #|
			   (JMP ,entry:compiler-primitive-lexpr-apply)
			   |#
			   ,@(invoke-interface
			      code:compiler-primitive-lexpr-apply)))
		     (else
		      ;; Unknown primitive arity.  Go through apply.
		      (LAP ,@(load-rn frame-size 2)
			   (MOV L (constant->ea primitive) (R 1))
			   #|
			   (JMP ,entry:compiler-apply)
			   |#
			   ,@(invoke-interface code:compiler-apply))))))))

(let-syntax
    ((define-special-primitive-invocation
       (sc-macro-transformer
	(lambda (form environment)
	  `(DEFINE-RULE STATEMENT
	     (INVOCATION:SPECIAL-PRIMITIVE
	      (? frame-size)
	      (? continuation)
	      ,(make-primitive-procedure (cadr form) #t))
	     FRAME-SIZE CONTINUATION	; ignored
	     ,(list 'LAP
		    (list 'UNQUOTE-SPLICING '(CLEAR-MAP!))
		    #|
		    (list 'JMP
			  (list 'UNQUOTE
				(close-syntax (symbol-append 'ENTRY:COMPILER-
							     (cadr form))
					      environment)))
		    |#
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

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 14))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER 13))
  (generate/move-frame-up frame-size (offset-reference 13 0)))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 14) (? offset)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (increment-rn 14 (* 4 how-far)))
	  ((= frame-size 1)
	   (LAP (MOV L (@R+ 14) ,(offset-reference r14 (-1+ how-far)))
		,@(increment-rn 14 (* 4 (-1+ how-far)))))
	  ((= frame-size 2)
	   (if (= how-far 1)
	       (LAP (MOV L (@RO B 14 4) (@RO B 14 8))
		    (MOV L (@R+ 14) (@R 14)))
	       (let ((i (lambda ()
			  (LAP (MOV L (@R+ 14)
				    ,(offset-reference r14 (-1+ how-far)))))))
		 (LAP ,@(i)
		      ,@(i)
		      ,@(increment-rn 14 (* 4 (- how-far 2)))))))
	  (else
	   (generate/move-frame-up frame-size
				   (offset-reference r14 offset))))))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER (? base))
						   (? offset)))
  (QUALIFIER (pseudo-register? base))
  (generate/move-frame-up frame-size (indirect-reference! base offset)))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK 0 (REGISTER 14) (REGISTER 13))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OFFSET-ADDRESS (REGISTER (? base))
						  (? offset))
				  (REGISTER 13))
  (let ((label (generate-label))
	(temp (allocate-temporary-register! 'GENERAL)))
    (let ((temp-ref (register-reference temp)))
      (LAP (MOVA L ,(indirect-reference! base offset) ,temp-ref)
	   (CMP L ,temp-ref (R 13))
	   (B B LEQU (@PCR ,label))
	   (MOV L (R 13) ,temp-ref)
	   (LABEL ,label)
	   ,@(generate/move-frame-up* frame-size temp)))))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OBJECT->ADDRESS (REGISTER (? source)))
				  (REGISTER 13))
  (QUALIFIER (pseudo-register? source))
  (let ((do-it
	 (lambda (reg-ref)
	   (let ((label (generate-label)))
	     (LAP (CMP L ,reg-ref (R 13))
		  (B B LEQU (@PCR ,label))
		  (MOV L (R 13) ,reg-ref)
		  (LABEL ,label)
		  ,@(generate/move-frame-up* frame-size
					     (lap:ea-R-register reg-ref)))))))
    (with-temporary-register-copy! source 'GENERAL
      (lambda (temp)
	(LAP (BIC L ,mask-reference ,temp)
	     ,@(do-it temp)))
      (lambda (source temp)
	(LAP (BIC L ,mask-reference ,source ,temp)
	     ,@(do-it temp))))))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? source))
				  (REGISTER 13))
  (QUALIFIER (pseudo-register? source))
  (let ((reg-ref (move-to-temporary-register! source 'GENERAL))
	(label (generate-label)))
    (LAP (CMP L ,reg-ref (R 13))
	 (B B LEQU (@PCR ,label))
	 (MOV L (R 13) ,reg-ref)
	 (LABEL ,label)
	 ,@(generate/move-frame-up* frame-size
				    (lap:ea-R-register reg-ref)))))

(define (generate/move-frame-up frame-size destination)
  (let ((temp (allocate-temporary-register! 'GENERAL)))
    (LAP (MOVA L ,destination ,(register-reference temp))
	 ,@(generate/move-frame-up* frame-size temp))))

(define (generate/move-frame-up* frame-size destination)
  (let ((temp (allocate-temporary-register! 'GENERAL)))
    (LAP (MOVA L ,(offset-reference r14 frame-size) ,(register-reference temp))
	 ,@(generate-n-times
	    frame-size 5
	    (lambda ()
	      (LAP (MOV L (@-R ,temp) (@-R ,destination))))
	    (lambda (generator)
	      (generator (allocate-temporary-register! 'GENERAL))))
	 (MOV L ,(register-reference destination) (R 14)))))

;;;; External Labels

(define (make-external-label code label)
  (set! *external-labels* (cons label *external-labels*))
  (LAP (WORD U ,code)
       (BLOCK-OFFSET ,label)
       (LABEL ,label)))

;;; Entry point types

(define-integrable (make-format-longword format-word gc-offset)
  (+ (* #x20000 gc-offset) format-word))

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
	 (error "Unable to encode continuation offset" offset))))

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

(define (interrupt-check procedure-label interrupt-label)
  ;; This always does interrupt/stack checks in line.
  (LAP (CMP L (R ,regnum:free-pointer) ,reg:compiled-memtop)
       (B B GEQ (@PCR ,interrupt-label))
       ,@(if (let ((object (label->object procedure-label)))
	       (and (rtl-procedure? object)
		    (not (rtl-procedure/stack-leaf? object))
		    compiler:generate-stack-checks?))
	     (LAP (CMP L (R ,regnum:stack-pointer) ,reg:stack-guard)
		  (B B LSS (@PCR ,interrupt-label)))
	     (LAP))))

(define (simple-procedure-header code-word label
				 ;; entry:compiler-interrupt
				 code:compiler-interrupt)
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 #|
	 (JSB ,entry:compiler-interrupt)
	 |#
	 ,@(invoke-interface-jsb code:compiler-interrupt)
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define (dlink-procedure-header code-word label)
  (let ((gc-label (generate-label)))    
    (LAP (LABEL ,gc-label)
	 #|
	 (JSB ,entry:compiler-interrupt-dlink)
	 |#
	 (MOV L (R 13) (R 2))		; move dlink to arg register.
	 ,@(invoke-interface-jsb code:compiler-interrupt-dlink)
	 ;; 'Til here
	 ,@(make-external-label code-word label)
	 ,@(interrupt-check label gc-label))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (make-external-label (continuation-code-word internal-label)
		       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header (continuation-code-word internal-label)
			   internal-label
			   ;; entry:compiler-interrupt-continuation
			   code:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (let* ((procedure (label->object internal-label))
	 (external-label (rtl-procedure/external-label procedure)))
    (LAP (ENTRY-POINT ,external-label)
	 (EQUATE ,external-label ,internal-label)
	 ,@(simple-procedure-header expression-code-word
				    internal-label
				    ;; entry:compiler-interrupt-ic-procedure
				    code:compiler-interrupt-ic-procedure))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (LAP
     (EQUATE ,(rtl-procedure/external-label rtl-proc) ,internal-label)
     ,@((if (rtl-procedure/dynamic-link? rtl-proc)
	    dlink-procedure-header 
	    (lambda (code-word label)
	      (simple-procedure-header code-word label
				       ;; entry:compiler-interrupt-procedure
				       code:compiler-interrupt-procedure)))
	(internal-procedure-code-word rtl-proc)
	internal-label))))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(rtl-procedure/external-label
		 (label->object internal-label))
	       ,internal-label)
       ,@(simple-procedure-header (make-procedure-code-word min max)
				  internal-label
				  ;; entry:compiler-interrupt-procedure
				  code:compiler-interrupt-procedure)))

;;;; Closures.  These two statements are intertwined:
;;; Note: If the closure is a multiclosure, the closure object on the
;;; stack corresponds to the first (official) entry point.
;;; Thus on entry and interrupt it must be bumped around.

(define (make-magic-closure-constant entry)
  (- (make-non-pointer-literal (ucode-type compiled-entry) 0)
     (+ (* entry 10) 6)))

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  nentries				; ignored
  (let ((rtl-proc (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label rtl-proc)))
      (if (zero? nentries)
	  (LAP (EQUATE ,external-label ,internal-label)
	       ,@(simple-procedure-header
		  (internal-procedure-code-word rtl-proc)
		  internal-label
		  ;; entry:compiler-interrupt-procedure
		  code:compiler-interrupt-procedure))
	  (LAP (LABEL ,gc-label)
	       ,@(increment/ea (INST-EA (@R 14)) (* 10 entry))
	       #|
	       (JMP ,entry:compiler-interrupt-closure)
	       |#
	       ,@(invoke-interface code:compiler-interrupt-closure)
	       ,@(make-external-label internal-entry-code-word
				      external-label)
	       (ADD L (&U ,(make-magic-closure-constant entry)) (@R 14))
	       (LABEL ,internal-label)
	       ,@(interrupt-check internal-label gc-label))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? size)))
  (let ((target (standard-target-reference target)))
    (generate/cons-closure target
			   false procedure-label min max size)))

(define (generate/cons-closure target type procedure-label min max size)
  (LAP ,@(load-non-pointer (ucode-type manifest-closure)
			   (+ 3 size)
			   (INST-EA (@R+ 12)))
       (MOV L (&U ,(make-format-longword (make-procedure-code-word min max) 8))
	    (@R+ 12))
       ,@(if type
	     (LAP (BIS L (&U ,(make-non-pointer-literal type 0)) (R 12)
		       ,target))
	     (LAP (MOV L (R 12) ,target)))
       (MOV W (&U #x9f16) (@R+ 12))	; (JSB (@& <entry>))
       (MOVA B (@PCR ,(rtl-procedure/external-label
		       (label->object procedure-label)))
	     (@R+ 12))
       (CLR W (@R+ 12))
       ,@(increment-rn 12 (* 4 size))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? size) (? entries)))
  (let ((target (standard-target-reference target)))
    (case nentries
      ((0)
       (LAP (MOV L (R 12) ,target)
	    ,@(load-non-pointer (ucode-type manifest-vector)
				size
				(INST-EA (@R+ 12)))
	    ,@(increment-rn 12 (* 4 size))))
      ((1)
       (let ((entry (vector-ref entries 0)))
	 (generate/cons-closure target false
				(car entry) (cadr entry) (caddr entry)
				size)))
      (else
       (generate/cons-multiclosure target nentries size
				   (vector->list entries))))))

(define (generate/cons-multiclosure target nentries size entries)
  (let ((total-size (+ size
		       (quotient (+ 3 (* 5 nentries))
				 2)))
	(temp (standard-temporary-reference)))

    (define (generate-entries entries offset first?)
      (if (null? entries)
	  (LAP)
	  (let ((entry (car entries)))
	    (LAP (MOV L (&U ,(make-format-longword
			      (make-procedure-code-word (cadr entry)
							(caddr entry))
			      offset))
		      (@R+ 12))
		 ,@(if first?
		       (LAP (MOV L (R 12) ,target))
		       (LAP))
		 (MOV W ,temp (@R+ 12))	; (JSB (@& <entry>))
		 (MOVA B (@PCR ,(rtl-procedure/external-label
				 (label->object (car entry))))
		       (@R+ 12))
		 ,@(generate-entries (cdr entries)
				     (+ 10 offset)
				     false)))))

    (LAP ,@(load-non-pointer (ucode-type manifest-closure)
			     total-size
			     (INST-EA (@R+ 12)))
	 (MOV L (&U ,(make-format-longword nentries 0)) (@R+ 12))
	 (MOV W (&U #x9f16) ,temp)
	 ,@(generate-entries entries 12 true)
	 ,@(if (odd? nentries)
	       (LAP (CLR W (@R+ 12)))
	       (LAP))
	 ,@(increment-rn 12 (* 4 size)))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP GENERATOR.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  (LAP (MOV L ,reg:environment (@PCR ,environment-label))
       (MOVA B (@PCR ,*block-label*) (R 2))
       (MOVA B (@PCR ,free-ref-label) (R 3))
       ,@(load-rn n-sections 4)
       #|
       (JSB ,entry:compiler-link)
       |#
       ,@(invoke-interface-jsb code:compiler-link)
       ,@(make-external-label (continuation-code-word false)
			      (generate-label))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (LAP (BIC L ,mask-reference (@PCR ,code-block-label) (R 2))
       (MOV L ,reg:environment
	    (@RO ,(datum-size environment-offset) 2 ,environment-offset))
       ,@(add-constant/ea (INST-EA (R 2)) free-ref-offset (INST-EA (R 3)))
       ,@(load-rn n-sections 4)
       #|
       (JSB ,entry:compiler-link)
       |#
       ,@(invoke-interface-jsb code:compiler-link)
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

;; IMPORTANT:
;; frame-size and uuo-label are switched (with respect to the 68k
;; version) in order to preserve the arity in a constant position (the
;; Vax is little-endian).  The invocation rule for uuo-links has been
;; changed to take the extra 2 bytes into account.
;; Alternatively we could
;; make execute caches 3 words long, with the third containing the
;; frame size and the middle the second part of the instruction.

(define (transmogrifly uuos)
  (define (inner name assoc)
    (if (null? assoc)
	(transmogrifly (cdr uuos))
	(cons (cons (caar assoc)			; frame-size
		    (cdar assoc))			; uuo-label
	      (cons (cons name				; variable name
			  (allocate-constant-label))	; dummy label
		    (inner name (cdr assoc))))))
  (if (null? uuos)
      '()
      (inner (caar uuos) (cdar uuos))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
