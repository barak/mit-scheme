#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/lapgen.scm,v 4.7 1989/05/21 03:55:03 jinx Rel $
$MC68020-Header: lapgen.scm,v 4.19 89/01/18 13:49:56 GMT cph Exp $

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

;;;; RTL Rules for DEC VAX.  Part 1

(declare (usual-integrations))

;;;; Basic machine instructions

(define (reference->register-transfer source target)
  (if (and (effective-address/register? source)
	   (= (lap:ea-R-register source) target))
      (LAP)
      (LAP (MOV L ,source ,(register-reference target)))))

(define (register->register-transfer source target)
  (LAP ,(machine->machine-register source target)))

(define (home->register-transfer source target)
  (LAP ,(pseudo->machine-register source target)))

(define (register->home-transfer source target)
  (LAP ,(machine->pseudo-register source target)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

;; Pseudo registers are at negative offsets from regs-pointer,
;; and each is two longwords long so it can hold a double float.

(define-integrable (pseudo-register-offset register)
  (* -2 (1+ (register-renumber register))))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (pseudo-register-offset register)))

(define-integrable (machine->machine-register source target)
  (INST (MOV L
	     ,(register-reference source)
	     ,(register-reference target))))

(define-integrable (machine-register->memory source target)
  (INST (MOV L
	     ,(register-reference source)
	     ,target)))

(define-integrable (memory->machine-register source target)
  (INST (MOV L
	     ,source
	     ,(register-reference target))))

(define (datum-size datum)
  (cond ((<= -128 datum 127) 'B)
	((<= -32768 datum 32767) 'W)
	(else 'L)))

(define (offset-reference register offset)
  (if (zero? offset)
      (INST-EA (@R ,register))
      (let ((real-offset (* 4 offset)))
	(INST-EA (@RO ,(datum-size real-offset) ,register ,real-offset)))))

(define (byte-offset-reference register offset)
  (if (zero? offset)
      (INST-EA (@R ,register))
      (INST-EA (@RO ,(datum-size offset) ,register ,offset))))	       

;; N is always unsigned.

(define (load-rn n r)
  (cond ((zero? n)
	 (INST (CLR L (R ,r))))
	((<= 0 n 63)
	 (INST (MOV L (S ,n) (R ,r))))
	((<= 0 n 127)
	 (INST (MOVZ B L (& ,n) (R ,r))))
	((<= 0 n 32767)
	 (INST (MOVZ W L (& ,n) (R ,r))))
	(else
	 (INST (MOV L (& ,n) (R ,r))))))

(define (test-rn n r)
  (cond ((zero? n)
	 (INST (TST L (R ,r))))
	((<= 0 n 63)
	 (INST (CMP L (R ,r) (S ,n))))
	(else
	 (INST (CMP L (R ,r) (& ,n))))))

(define (increment-rn rn n)
  (if (zero? n)
      (LAP)
      (let ((value (* 4 n)))
	(cond ((<= 0 value 63)
	       (LAP (ADD L (S ,value) (R ,rn))))
	      ((<= -63 value 0)
	       (LAP (SUB L (S ,value) (R ,rn))))
	      (else
	       (let ((size (datum-size value)))
		 (if (not (eq? size 'L))
		     (LAP (MOVA L (@RO ,size ,rn ,value)
				(R ,rn)))
		     (LAP (ADD L (& ,value) (R ,rn))))))))))

(define (constant->ea constant)
  (if (non-pointer-object? constant)
      (non-pointer->ea (object-type constant) (object-datum constant))
      (INST-EA (@PCR ,(constant->label constant)))))

(define (non-pointer->ea type datum)
  (cond ((not (zero? type))
	 (INST-EA (& ,(make-non-pointer-literal type datum))))
	((<= 0 datum 63)
	 (INST-EA (S ,datum)))
	(else
	 (INST-EA (& ,datum)))))

(define (push-constant constant)
  (if (non-pointer-object? constant)
      (push-non-pointer (object-type constant)
			(object-datum constant))
      (INST (PUSHL (@PCR ,(constant->label constant))))))

(define (push-non-pointer type datum)
  (cond ((not (zero? type))
	 (INST (PUSHL (& ,(make-non-pointer-literal type datum)))))
	((<= 0 datum 63)
	 (INST (PUSHL (S ,datum))))
	(else
	 (let ((size (datum-size datum)))
	   (if (not (eq? size 'L))
	       (INST (CVT ,size L (& ,datum) (@-R 14)))
	       (INST (PUSHL (& ,datum))))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (object-type constant)
			(object-datum constant)
			target)
      (INST (MOV L
		 (@PCR ,(constant->label constant))
		 ,target))))

(define (load-non-pointer type datum target)
  (if (not (zero? type))
      (INST (MOV L
		 (& ,(make-non-pointer-literal type datum))
		 ,target))
      (load-immediate datum target)))

(define (load-immediate datum target)
  (cond ((zero? datum)
	 (INST (CLR L ,target)))
	((<= 0 datum 63)
	 (INST (MOV L (S ,datum) ,target)))
	(else
	 (let ((size (datum-size datum)))
	   (if (not (eq? size 'L))
	       (INST (CVT ,size L (& ,datum) ,target))
	       (INST (MOV L (& ,datum) ,target)))))))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (test-byte n effective-address)
  (cond ((zero? n)
	 (INST (TST B ,effective-address)))
	((<= 0 n 63)
	 (INST (CMP B ,effective-address (S ,n))))
	(else
	 (INST (CMP B ,effective-address (& ,n))))))

(define (test-non-pointer type datum effective-address)
  (cond ((not (zero? type))
	 (INST (CMP L
		    ,effective-address
		    (& ,(make-non-pointer-literal type datum)))))
	((zero? datum)
	 (INST (TST L ,effective-address)))
	((<= 0 datum 63)
	 (INST (CMP L ,effective-address (S ,datum))))
	(else
	 (INST (CMP L
		    ,effective-address
		    (& ,(make-non-pointer-literal type datum)))))))

(define (set-standard-branches! condition-code)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,condition-code (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc condition-code) (@PCR ,label))))))

(define (invert-cc condition-code)
  (cdr (or (assq condition-code
		 '((NEQU . EQLU) (EQLU . NEQU)
		   (NEQ . EQL) (EQL . NEQ)
		   (GTR . LEQ) (LEQ . GTR)
		   (GEQ . LSS) (LSS . GEQ)
		   (VC . VS) (VS . VC)
		   (CC . CS) (CS . CC)
		   (GTRU . LEQU) (LEQU . GTRU)
		   (GEQU . LSSU) (LSSU . GEQU)))
	   (error "INVERT-CC: Not a known CC" condition-code))))

(define (invert-cc-noncommutative condition-code)
  ;; Despite the fact that the name of this procedure is similar to
  ;; that of `invert-cc', it is quite different.  `invert-cc' is used
  ;; when the branches of a conditional are being exchanged, while
  ;; this is used when the arguments are being exchanged.
  (cdr (or (assq condition-code
		 '((NEQU . NEQU) (EQLU . EQLU)
     		   (NEQ . NEQ) (EQL . EQL)
		   (GTR . LSS) (LSS . GTR)
		   (GEQ . LEQ) (LEQ . GEQ)
		   ;; *** Are these two really correct? ***
		   (VC . VC) (VS . VS)
		   (CC . CC) (CS . CS)
		   (GTRU . LSSU) (LSSU . GTRU)
		   (GEQU . LEQU) (LEQU . GEQU)))
	   (error "INVERT-CC-NONCOMMUTATIVE: Not a known CC" condition-code))))

(define-integrable (cc-commutative? condition-code)
  (memq condition-code '(NEQ EQL NEQU EQLU VC VS CC CS)))

(define-integrable (effective-address/register? ea)
  (eq? (lap:ea-keyword ea) 'R))

(define-integrable (effective-address/register-indirect? ea)
  (eq? (lap:ea-keyword ea) '@R))

(define-integrable (effective-address/register-offset? ea)
  (eq? (lap:ea-keyword ea) '@RO))

(define (standard-target-reference target)
  (delete-dead-registers!)
  (register-reference
   (or (register-alias target 'GENERAL)
       (allocate-alias-register! target 'GENERAL))))

(define-integrable (preferred-register-reference register)
  (register-reference (preferred-register register)))

(define (preferred-register register)
  (or (register-alias register 'GENERAL)
      (load-alias-register! register 'GENERAL)))

(define (offset->indirect-reference! offset)
  (indirect-reference! (rtl:register-number (rtl:offset-register offset))
		       (rtl:offset-number offset)))

(define-integrable (indirect-reference! register offset)
  (offset-reference (allocate-indirection-register! register) offset))

(define-integrable (indirect-byte-reference! register offset)
  (byte-offset-reference (allocate-indirection-register! register) offset))

(define (allocate-indirection-register! register)
  (if (machine-register? register)
      register
      (preferred-register register)))

(define (code-object-label-initialize code-object)
  ;; *** What is this for? ***
  code-object				; ignored
  false)

(define (generate-n-times n limit instruction-gen with-counter)
  (if (> n limit)
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,(load-rn (-1+ n) counter)
		(LABEL ,loop)
		,(instruction-gen)
		(SOB GEQ (R ,counter) (@PCR ,loop))))))
      (let loop ((n n))
	(if (zero? n)
	    (LAP)
	    (LAP ,(instruction-gen)
		 ,@(loop (-1+ n)))))))

;;;; Expression-Generic Operations

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER)
	      (load-machine-register! (rtl:register-number expression)
				      register))
	     ((OFFSET)
	      (LAP (MOV L ,(offset->indirect-reference! expression) ,target)))
	     ((CONSTANT)
	      (LAP ,(load-constant (rtl:constant-value expression) target)))
	     ((UNASSIGNED)
	      (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define (make-immediate value)
  (if (<= 0 value 63)
      (INST-EA (S ,value))
      (INST-EA (& ,value))))

(define (bump-type ea)
  (cond ((effective-address/register-indirect? ea)
	 (INST-EA (@RO B ,(lap:ea-@R-register ea) 3)))
	((effective-address/register-offset? ea)
	 (let ((offset (+ 3 (lap:ea-@RO-offset ea))))
	   (INST-EA (@RO ,(datum-size offset)
			 ,(lap:ea-@RO-register ea)
			 ,offset))))
	(else #F)))

(define (put-type-in-ea type-code ea)
  (cond ((not (effective-address/register? ea))
	 (let ((target (bump-type ea)))
	   (if target
	       (LAP (MOV B ,(make-immediate type-code) ,target))
	       (error "PUT-TYPE-IN-EA: Illegal effective address" ea))))
	((zero? type-code)
	 (LAP (BIC L ,mask-reference ,ea)))
	(else
	 (LAP (BIC L ,mask-reference ,ea)
	      (BIS L (& ,(make-non-pointer-literal type-code 0)) ,ea)))))

(define (standard-target-expression? target)
  (or (rtl:offset? target)
      (rtl:free-push? target)
      (rtl:stack-push? target)))

(define (rtl:free-push? expression)
  (and (rtl:post-increment? expression)
       (interpreter-free-pointer? (rtl:post-increment-register expression))
       (= 1 (rtl:post-increment-number expression))))

(define (rtl:stack-push? expression)
  (and (rtl:pre-increment? expression)
       (interpreter-stack-pointer? (rtl:pre-increment-register expression))
       (= -1 (rtl:pre-increment-number expression))))

(define (standard-target-expression->ea target)
  (cond ((rtl:offset? target) (offset->indirect-reference! target))
	((rtl:free-push? target) (INST-EA (@R+ 12)))
	((rtl:stack-push? target) (INST-EA (@-R 14)))
	(else (error "STANDARD-TARGET->EA: Not a standard target" target))))

;; Fixnum stuff moved to rulfix.scm

;;;; Datum and character utilities

#|
;;; OBJECT->DATUM rules - Mhwu

;; These seem unused.

(define (load-constant-datum constant register-ref)
  (if (non-pointer-object? constant)
      (load-non-pointer 0 (object-datum constant) ,register-ref)
      (LAP (MOV L
		(@PCR ,(constant->label constant))
		,register-ref)
	   ,@(object->address register-ref))))

(define (byte-offset->register source source-reg target)
  source-reg				; ignored
  (delete-dead-registers!)
  (let ((target (allocate-alias-register! target 'GENERAL)))
    (LAP (MOVZ B L ,source ,(register-reference target)))))
|#

;;; CHAR->ASCII rules

(define (coerce->any/byte-reference register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (indirect-char/ascii-reference!
	     regnum:regs-pointer
	     (pseudo-register-offset register))))))

(define-integrable (indirect-char/ascii-reference! register offset)
  (indirect-byte-reference! register (+ 3 (* offset 4))))
(define (char->signed-8-bit-immediate character)
  (let ((ascii (char->ascii character)))
    (if (< ascii 128)
	ascii
	(- ascii 256))))

(define (indirect-register register)
  (if (machine-register? register)
      register
      (register-alias register false)))

(define-integrable (lap:ea-keyword expression)
  (car expression))

(define-integrable (lap:ea-R-register expression)
  (cadr expression))

(define-integrable (lap:ea-@R-register expression)
  (cadr expression))

(define-integrable (lap:ea-@RO-register expression)
  (caddr expression))

(define-integrable (lap:ea-@RO-offset expression)
  (cadddr expression))

(define-export (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define-export (lap:make-unconditional-branch label)
  (INST (BR (@PCR ,label))))		; Unsized

(define-export (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

(let-syntax ((define-entries
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE-INTEGRABLE
				,(symbol-append 'ENTRY:COMPILER-
						(car names))
				(INST-EA (@RO W 13 ,index)))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names start)))))
  (define-entries #x0280
    link error apply
    lexpr-apply primitive-apply primitive-lexpr-apply
    cache-reference-apply lookup-apply
    interrupt-continuation interrupt-ic-procedure
    interrupt-procedure interrupt-closure
    lookup safe-lookup set! access unassigned? unbound? define
    reference-trap safe-reference-trap assignment-trap unassigned?-trap
    &+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?))

(define-integrable reg:compiled-memtop (INST-EA (@R 13)))
(define-integrable reg:environment (INST-EA (@RO B 13 #x0C)))
(define-integrable reg:temp (INST-EA (@RO B 13 #x10)))
(define-integrable reg:lexpr-primitive-arity (INST-EA (@RO B 13 #x1C)))

;;;; 2/3 Operand register allocation

;; These should probably live in back/lapgn2.scm

(define (with-copy-if-available source type if-win if-lose use-register!)
  (reuse-pseudo-register-alias
   source type
   (lambda (reusable-alias)
     (if-win (lambda ()
	       (delete-machine-register! reusable-alias)
	       (delete-dead-registers!)
	       (use-register! reusable-alias)
	       (register-reference reusable-alias))))
   if-lose))

(define-integrable (with-register-copy-if-available
		     source type target if-win if-lose)
  (with-copy-if-available source type if-win if-lose
    (lambda (reusable-alias)
      (add-pseudo-register-alias! target reusable-alias))))

(define-integrable (with-temporary-copy-if-available
		     source type if-win if-lose)
  (with-copy-if-available source type if-win if-lose need-register!))

;;;; Higher level rules - assignment

(define-integrable (convert-object/constant->register target constant
					   rtconversion
					   ctconversion)
  (let ((target (standard-target-reference target)))
    (if (non-pointer-object? constant)
	(ctconversion constant target)
	(rtconversion (constant->ea constant) target))))

(define-integrable (convert-object/register->register target source conversion)
  ;; `conversion' often expands into multiple references to `target'.
  (with-register-copy-alias! source 'GENERAL target
    (lambda (target)
      (conversion target target))
    conversion))

(define-integrable (convert-object/offset->register target address
						    offset conversion)
  (let ((source (indirect-reference! address offset)))
    (conversion source 
		(standard-target-reference target))))

;;;; Higher level rules - predicates

(define (predicate/memory-operand? expression)
  (or (rtl:offset? expression)
      (and (rtl:post-increment? expression)
	   (interpreter-stack-pointer?
	    (rtl:post-increment-register expression)))))

(define (predicate/memory-operand-reference expression)
  (case (rtl:expression-type expression)
    ((OFFSET) (offset->indirect-reference! expression))
    ((POST-INCREMENT) (INST-EA (@R+ 14)))
    (else (error "Illegal memory operand" expression))))

(define (compare/register*register register-1 register-2 cc)
  (set-standard-branches! cc)
  (LAP (CMP L ,(standard-register-reference register-1 false)
	    ,(standard-register-reference register-2 false))))

(define (compare/register*memory register memory cc)
  (set-standard-branches! cc)
  (LAP (CMP L ,(standard-register-reference register false) ,memory)))

(define (compare/memory*memory memory-1 memory-2 cc)
  (set-standard-branches! cc)
  (LAP (CMP L ,memory-1 ,memory-2)))