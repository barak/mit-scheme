#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules3.scm,v 4.10 1988/08/29 22:54:31 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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
  continuation
  (LAP ,@(clear-map!)
       ,(load-dnw frame-size 0)
       (JMP ,entry:compiler-apply)))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation
  (LAP ,@(clear-map!)
       (BRA (@PCR ,label))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation
  (LAP ,@(clear-map!)
       ,(load-dnw number-pushed 0)
       (LEA (@PCR ,label) (A 0))
       (JMP ,entry:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation
  (LAP ,@(clear-map!)
       ;; The following assumes that at label there is
       ;;	(JMP (L <entry>))
       ;; The other possibility would be
       ;;       (JMP (@@PCR ,(free-uuo-link-label name frame-size)))
       ;; and to have <entry> at label, but it is longer and slower.
       (BRA (@PCR ,(free-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  continuation
  (let ((set-extension (expression->machine-register! extension a3)))
    (delete-dead-registers!)
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,(load-dnw frame-size 0)
	 (LEA (@PCR ,*block-start-label*) (A 1))
	 (JMP ,entry:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  continuation
  (let ((set-environment (expression->machine-register! environment d4)))
    (delete-dead-registers!)
    (LAP ,@set-environment
	 ,@(clear-map!)
	 ,(load-constant name (INST-EA (D 5)))
	 ,(load-dnw frame-size 0)
	 (JMP ,entry:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation
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
	    frame-size continuation
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
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER 12))
  (generate/move-frame-up frame-size (offset-reference 12 0)))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 15) (? offset)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (increment-machine-register 15 how-far))
	  ((= frame-size 1)
	   (LAP (MOV L (@A+ 7) ,(offset-reference a7 (-1+ how-far)))
		,@(increment-machine-register 15 (-1+ how-far))))
	  ((= frame-size 2)
	   (if (= how-far 1)
	       (LAP (MOV L (@AO 7 4) (@AO 7 8))
		    (MOV L (@A+ 7) (@A 7)))
	       (let ((i (lambda ()
			  (INST (MOV L (@A+ 7)
				     ,(offset-reference a7 (-1+ how-far)))))))
		 (LAP ,(i)
		      ,(i)
		      ,@(increment-machine-register 15 (- how-far 2))))))
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

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OBJECT->ADDRESS (REGISTER (? source)))
				  (REGISTER 12))
  (QUALIFIER (pseudo-register? source))
  (let ((dreg (move-to-temporary-register! source 'DATA))
	(label (generate-label))
	(temp (allocate-temporary-register! 'ADDRESS)))
    (let ((areg (register-reference temp)))
      (LAP (AND L ,mask-reference ,dreg)
	   (MOV L ,dreg ,areg)
	   (CMP L ,areg (A 4))
	   (B HS B (@PCR ,label))
	   (MOV L (A 4) ,areg)
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

;;;; External Labels

(define (make-external-label code label)
  (set! compiler:external-labels 
	(cons label compiler:external-labels))
  (LAP (DC UW ,code)
       (BLOCK-OFFSET ,label)
       (LABEL ,label)))

;;; Entry point types

(define-integrable (make-code-word min max)
  (+ (* #x100 min) max))

(define (make-procedure-code-word min max)
  (define (coerce val)
    (cond ((and (not (negative? val))
		(< val 128))
	   val)
	  ((and (negative? val)
		(> val -128))
	   (+ 256 val))
	  (else
	   (error "make-procedure-code-word: Bad value" val))))
  (make-code-word (coerce min) (coerce max)))

(define expression-code-word
  (make-code-word #xff #xff))

(define internal-entry-code-word
  (make-code-word #xff #xfe))

;; This is the same until information is encoded in them

(define continuation-code-word
  (make-code-word #x80 #x80))

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

(define-integrable (simple-procedure-header code-word label
					    entry:compiler-interrupt)
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSR ,entry:compiler-interrupt)
	 ,@(make-external-label code-word label)
	 (CMP L ,reg:compiled-memtop (A 5))
	 (B GE B (@PCR ,gc-label)))))

(define-rule statement
  (CONTINUATION-ENTRY (? internal-label))
  (make-external-label continuation-code-word
		       internal-label))

(define-rule statement
  (CONTINUATION-HEADER (? internal-label))
  (simple-procedure-header continuation-code-word
			   internal-label
			   entry:compiler-interrupt-continuation))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((external-label (rtl-procedure/external-label procedure)))
    (LAP
     (ENTRY-POINT ,external-label)
     (EQUATE ,external-label ,internal-label)
     ,@(simple-procedure-header expression-code-word
				internal-label
				entry:compiler-interrupt-ic-procedure)))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (simple-procedure-header internal-entry-code-word
			   internal-label
			   entry:compiler-interrupt-procedure))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(rtl-procedure/external-label
		 (label->object internal-label))
	       ,internal-label)
       ,@(simple-procedure-header (make-procedure-code-word min max)
				  internal-label
				  entry:compiler-interrupt-procedure)))

;;;; Closures.  These two statements are intertwined:

(define magic-closure-constant
  (- (* #x1000000 (ucode-type compiled-entry)) 6))

(define-rule statement
  (CLOSURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label procedure)))
      (LAP (LABEL ,gc-label)
	   (JMP ,entry:compiler-interrupt-closure)
	   ,@(make-external-label internal-entry-code-word external-label)
	   (ADD L (& ,magic-closure-constant) (@A 7))
	   (LABEL ,internal-label)
	   (CMP L ,reg:compiled-memtop (A 5))
	   (B GE B (@PCR ,gc-label))))))

(define-rule statement
  (CONS-CLOSURE (ENTRY:PROCEDURE (? internal-label)) (? min) (? max) (? size))
  (let* ((temp (allocate-temporary-register! 'ADDRESS))
	 (temp-ref (register-reference temp)))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label
		      (label->object internal-label)))
	      ,temp-ref)
	 ,(load-non-pointer (ucode-type manifest-closure) (+ 3 size)
			    (INST-EA (@A+ 5)))
	 (MOVE L (& ,(+ (* (make-procedure-code-word min max) #x10000)
			#x8))
	       (@A+ 5))
	 (MOVE L (A 5) ,reg:enclose-result)
	 (MOVE B (& ,(ucode-type compiled-entry)) ,reg:enclose-result)
	 (MOVE W (& #x4eb9) (@A+ 5))			; (JSR (L <entry>))
	 (MOVE L ,temp-ref (@A+ 5))
	 (CLR W (@A+ 5))
	 ,@(increment-machine-register 13 size))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define generate/quotation-header
  (let ((uuo-link-tag 0)
	(reference-tag 1)
	(assignment-tag 2))

    (define (make-constant-block-tag tag datum)
      (if (> datum #xffff)
	  (error "make-constant-block-tag: datum too large" datum)
	  (+ (* tag #x10000) datum)))

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
		  (inner `((,(make-constant-block-tag tag (length constants))
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

    (lambda (block-label constants references assignments uuo-links)
      (let ((constant-info
	     (declare-constants uuo-link-tag (transmogrifly uuo-links)
	       (declare-constants reference-tag references
		 (declare-constants assignment-tag assignments
		   (declare-constants #f constants
		     (cons '() (LAP))))))))
	(let ((free-ref-label (car constant-info))
	      (constants-code (cdr constant-info))
	      (debugging-information-label (allocate-constant-label))
	      (environment-label (allocate-constant-label)))
	  (LAP ,@constants-code
	       ;; Place holder for the debugging info filename
	       (SCHEME-OBJECT ,debugging-information-label DEBUGGING-INFO)
	       ;; Place holder for the load time environment if needed
	       (SCHEME-OBJECT ,environment-label
			      ,(if (null? free-ref-label) 0 'ENVIRONMENT))
	       ,@(if (null? free-ref-label)
		     (LAP)
		     (LAP (LEA (@PCR ,environment-label) (A 0))
			  (MOV L ,reg:environment (@A 0))
			  (LEA (@PCR ,block-label) (A 0))
			  (LEA (@PCR ,free-ref-label) (A 1))
			  ,(load-dnw (+ (if (null? uuo-links) 0 1)
					(if (null? references) 0 1)
					(if (null? assignments) 0 1))
				     0)
			  (JSR ,entry:compiler-link)
			  ,@(make-external-label continuation-code-word
						 (generate-label))))))))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
