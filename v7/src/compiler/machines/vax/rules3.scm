#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/rules3.scm,v 4.7 1989/05/17 20:31:11 jinx Rel $
$MC68020-Header: rules3.scm,v 4.15 88/12/30 07:05:20 GMT cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Invocations and Entries.  DEC VAX version.

(declare (usual-integrations))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  (LAP ,@(clear-map!)
       (CLR B (@RO B 14 3))
       (RSB)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation				; ignored
  (LAP ,@(clear-map!)
	,(load-rn frame-size 0)
	(JMP ,entry:compiler-apply)))

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
       (CLR B (@RO B 14 3))
       (RSB)))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ,(load-rn number-pushed 0)
       (MOVA B (@PCR ,label) (R 3))
       (JMP ,entry:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation				; ignored
  ;; It expects the procedure at the top of the stack
  (LAP ,@(clear-map!)
       ,(load-rn number-pushed 0)
       (BIC L ,mask-reference (@R+ 14) (R 3))
       (JMP ,entry:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ;; The following assumes that at label there is
       ;;	(JMP (L <entry>))
       ;; The other possibility would be
       ;;       (JMP (@@PCR ,(free-uuo-link-label name frame-size)))
       ;; and to have <entry> at label, but it is longer and slower.
       (BR (@PCR ,(free-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size) (? continuation) (? extension))
  continuation				; ignored
  (let ((set-extension (expression->machine-register! extension r6)))
    (delete-dead-registers!)
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,(load-rn frame-size 0)
	 (MOVA B (@PCR ,*block-start-label*) (R 4))
	 (JMP ,entry:compiler-cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size) (? continuation) (? environment) (? name))
  continuation				; ignored
  (let ((set-environment (expression->machine-register! environment r7)))
    (delete-dead-registers!)
    (LAP ,@set-environment
	 ,@(clear-map!)
	 ,(load-constant name (INST-EA (R 8)))
	 ,(load-rn frame-size 0)
	 (JMP ,entry:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				; ignored
  (LAP ,@(clear-map!)
       ,@(if (eq? primitive compiled-error-procedure)
	     (LAP ,(load-rn frame-size 0)
		  (JMP ,entry:compiler-error))
	     (let ((arity (primitive-procedure-arity primitive)))
	       (cond ((not (negative? arity))
		      (LAP (MOV L (@PCR ,(constant->label primitive)) (R 9))
			   (JMP ,entry:compiler-primitive-apply)))
		     ((= arity -1)
		      (LAP (MOV L ,(make-immediate (-1+ frame-size))
				,reg:lexpr-primitive-arity)
			   (MOV L (@PCR ,(constant->label primitive)) (R 9))
			   (JMP ,entry:compiler-primitive-lexpr-apply)))
		     (else
		      ;; Unknown primitive arity.  Go through apply.
		      (LAP ,(load-rn frame-size 0)
			   (PUSHL (@PCR ,(constant->label primitive)))
			   (JMP ,entry:compiler-apply))))))))

(let-syntax
    ((define-special-primitive-invocation
       (macro (name)
	 `(define-rule statement
	    (INVOCATION:SPECIAL-PRIMITIVE
	     (? frame-size)
	     (? continuation)
	     ,(make-primitive-procedure name true))
	    frame-size continuation	; ignored
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
  (INVOCATION-PREFIX:MOVE-FRAME-UP 0 (REGISTER 14))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER 10))
  (generate/move-frame-up frame-size (offset-reference 10 0)))

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size)
				   (OFFSET-ADDRESS (REGISTER 14) (? offset)))
  (let ((how-far (- offset frame-size)))
    (cond ((zero? how-far)
	   (LAP))
	  ((zero? frame-size)
	   (increment-rn 14 how-far))
	  ((= frame-size 1)
	   (LAP (MOV L (@R+ 14) ,(offset-reference r14 (-1+ how-far)))
		,@(increment-rn 14 (-1+ how-far))))
	  ((= frame-size 2)
	   (if (= how-far 1)
	       (LAP (MOV L (@RO B 14 4) (@RO B 14 8))
		    (MOV L (@R+ 14) (@R 14)))
	       (let ((i (lambda ()
			  (INST (MOV L (@R+ 14)
				     ,(offset-reference r14 (-1+ how-far)))))))
		 (LAP ,(i)
		      ,(i)
		      ,@(increment-rn 14 (- how-far 2))))))
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
  (INVOCATION-PREFIX:DYNAMIC-LINK 0 (REGISTER 14) (REGISTER 10))
  (LAP))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OFFSET-ADDRESS (REGISTER (? base))
						  (? offset))
				  (REGISTER 10))
  (let ((label (generate-label))
	(temp (allocate-temporary-register! 'GENERAL)))
    (let ((temp-ref (register-reference temp)))
      (LAP (MOVA L ,(indirect-reference! base offset) ,temp-ref)
	   (CMP L ,temp-ref (R 10))
	   (B B LEQU (@PCR ,label))
	   (MOV L (R 10) ,temp-ref)
	   (LABEL ,label)
	   ,@(generate/move-frame-up* frame-size temp)))))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (OBJECT->ADDRESS (REGISTER (? source)))
				  (REGISTER 10))
  (QUALIFIER (pseudo-register? source))
  (let ((do-it
	 (lambda (reg-ref)
	   (let ((label (generate-label)))
	     (LAP (CMP L ,reg-ref (R 10))
		  (B B LEQU (@PCR ,label))
		  (MOV L (R 10) ,reg-ref)
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
				  (REGISTER 10))
  (QUALIFIER (pseudo-register? source))
  (let ((reg-ref (move-to-temporary-register! source 'GENERAL))
	(label (generate-label)))
    (LAP (CMP L ,reg-ref (R 10))
	 (B B LEQU (@PCR ,label))
	 (MOV L (R 10) ,reg-ref)
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
	      (INST (MOV L
			 (@-R ,temp)
			 (@-R ,destination))))
	    (lambda (generator)
	      (generator (allocate-temporary-register! 'GENERAL))))
	 (MOV L ,(register-reference destination) (R 14)))))

;;;; External Labels

(define (make-external-label code label)
  (set! compiler:external-labels 
	(cons label compiler:external-labels))
  (LAP (WORD U ,code)
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

(define-integrable (simple-procedure-header code-word label
					    entry:compiler-interrupt)
  (let ((gc-label (generate-label)))
    (LAP (LABEL ,gc-label)
	 (JSB ,entry:compiler-interrupt)
	 ,@(make-external-label code-word label)
	 (CMP L (R 12) ,reg:compiled-memtop)
	 (B B GEQ (@PCR ,gc-label)))))

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
    (let ((external-label (rtl-procedure/external-label procedure)))
    (LAP (ENTRY-POINT ,external-label)
	 (EQUATE ,external-label ,internal-label)
	 ,@(simple-procedure-header expression-code-word
				    internal-label
				    entry:compiler-interrupt-ic-procedure)))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (LAP (EQUATE ,(rtl-procedure/external-label
		 (label->object internal-label))
	       ,internal-label)
       ,@(simple-procedure-header internal-entry-code-word
				  internal-label
				  entry:compiler-interrupt-procedure)))

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
  (- (* (ucode-type compiled-entry) #x1000000) 6))

(define-rule statement
  (CLOSURE-HEADER (? internal-label))
  (let ((procedure (label->object internal-label)))
    (let ((gc-label (generate-label))
	  (external-label (rtl-procedure/external-label procedure)))
      (LAP (LABEL ,gc-label)
	   (JMP ,entry:compiler-interrupt-closure)
	   ,@(make-external-label internal-entry-code-word external-label)
	   (ADD L (& ,magic-closure-constant) (@R 14))
	   (LABEL ,internal-label)
	   (CMP L (R 12) ,reg:compiled-memtop)
	   (B B GEQ (@PCR ,gc-label))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type))
			(CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
				      (? min) (? max) (? size))))
  (QUALIFIER (pseudo-register? target))
  (generate/cons-closure (reference-target-alias! target 'GENERAL)
			 type procedure-label min max size))

(define-rule statement
  (ASSIGN (? target)
	  (CONS-POINTER (CONSTANT (? type))
			(CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
				      (? min) (? max) (? size))))
  (QUALIFIER (standard-target-expression? target))
  (generate/cons-closure
   (standard-target-expression->ea target)
   type procedure-label min max size))

(define (generate/cons-closure target type procedure-label min max size)
  (LAP ,(load-non-pointer (ucode-type manifest-closure)
			  (+ 3 size)
			  (INST-EA (@R+ 12)))
       (MOV L (&U ,(+ #x100000 (make-procedure-code-word min max)))
	    (@R+ 12))
       (BIS L (& ,(make-non-pointer-literal type 0)) (R 12) ,target)
       (MOV W (&U #x9f16) (@R+ 12))	; (JSB (@& <entry>))
       (MOVA B (@PCR ,(rtl-procedure/external-label
		       (label->object procedure-label)))
	     (@R+ 12))
       (CLR W (@R+ 12))
       ,@(increment-rn 12 size)))

;;;; Entry Header
;;; This is invoked by the top level of the LAP GENERATOR.

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
		     (LAP (MOV L ,reg:environment (@PCR ,environment-label))
			  (MOVA B (@PCR ,block-label) (R 3))
			  (MOVA B (@PCR ,free-ref-label) (R 4))
			  ,(load-rn (+ (if (null? uuo-links) 0 1)
				       (if (null? references) 0 1)
				       (if (null? assignments) 0 1))
				    0)
			  (JSB ,entry:compiler-link)
			  ,@(make-external-label (continuation-code-word false)
						 (generate-label))))))))))

;;; Local Variables: ***
;;; eval: (put 'declare-constants 'scheme-indent-hook 2) ***
;;; End: ***
