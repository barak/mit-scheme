#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/lapgen.scm,v 4.3 1988/01/13 19:29:29 bal Exp $

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

;;;; RTL Rules for DEC VAX.  Part 1
;;;  Matches MC68020 version 1.188

;;;
;;; Popper code has been removed, since poppers are
;;; no longer being used 
;;;
(declare (usual-integrations))

;;;; Basic machine instructions

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

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

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

(define (offset-type offset)
  (cond ((<= -128 offset 127) 'B)
	((<= -32768 offset 32767) 'W)
	(else 'L)))

(define (offset-reference register offset)
  (if (zero? offset)
      (INST-EA (@R ,register))
      (let ((real-offset (* 4 offset)))
	(INST-EA (@RO ,(offset-type real-offset) ,register ,real-offset)))))

;; N is always unsigned.
;; Actually loaded as long (the popper code depends on this).

(define (load-rnw n r)
  (cond ((zero? n)
	 (INST (CLR L (R ,r))))
	((<= 0 n 63)
	 (INST (MOVZ B L (S ,n) (R ,r))))
	((<= 0 n 127)
	 (INST (MOVZ B L (& ,n) (R ,r))))
	(else
	 (INST (MOVZ W L (& ,n) (R ,r))))))

(define (test-rnw n r)
  (cond ((zero? n)
	 (INST (TST W (R ,r))))
	((<= 0 n 63)
	 (INST (CMP W (R ,r) (S ,n))))
	(else
	 (INST (CMP W (R ,r) (& ,n))))))

(define (increment-rnl rn n)
  (if (zero? n)
      (LAP)
      (let ((offset (* 4 n)))
	(cond ((<= 0 offset 63)
	       (LAP (ADD L (S ,offset) (R ,rn))))
	      ((<= -63 offset 0)
	       (LAP (SUB L (S ,offset) (R ,rn))))
	      (else
	       (LAP (MOVA L (@RO ,(offset-type offset) ,rn ,offset)
			    (R ,rn))))))))

(define (push-constant constant)
  (if (non-pointer-object? constant)
      (push-non-pointer (primitive-type constant)
			(primitive-datum constant))
      (INST (PUSHL (@PCR ,(constant->label constant))))))

(define (push-non-pointer type datum)
  (cond ((not (zero? type))
	 (INST (PUSHL (& ,(make-non-pointer-literal type datum)))))
	((zero? datum)
	 (INST (CLR L (@-R 14))))
	((<= 0 datum 63)
	 (INST (PUSHL (S ,datum))))
	(else
	 (INST (CVT ,(offset-type datum) L (& ,datum) (@-R 14))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      (INST (MOV L
		 (@PCR ,(constant->label constant))
		 ,target))))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 (INST (MOV L
		    (& ,(make-non-pointer-literal type datum))
		    ,target)))
	((zero? datum)
	 (INST (CLR L ,target)))
	((<= 0 datum 63)
	 (INST (MOV L (S ,datum) ,target)))
	(else
	 (INST (CVT ,(offset-type datum) L (& ,datum) ,target)))))

(define (test-non-pointer type datum effective-address)
  ;; *** These may be backwards ***
  (cond ((not (zero? type))
	 (INST (CMP L
		    (& ,(make-non-pointer-literal type datum))
		    ,effective-address)))
	((zero? datum)
	 (INST (TST L ,effective-address)))
	((<= 0 datum 63)
	 (INST (CMP L (S ,datum) ,effective-address)))
	(else
	 (INST (CMP L
		    (& ,(make-non-pointer-literal type datum))
		    ,effective-address)))))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (test-byte n effective-address)
  (cond ((zero? n)
	 (INST (TST B ,effective-address)))
	;; These may be backwards
	((<= 0 n 63)
	 (INST (CMP B (S ,n) ,effective-address)))
	(else
	 (INST (CMP B (& ,n) ,effective-address)))))  

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc cc) (@PCR ,label))))))

(define (invert-cc cc)
  (cdr (or (assq cc
		 '((NEQU . EQLU) (EQLU . NEQU)
		   (NEQ . EQL) (EQL . NEQ)
		   (GTR . LEQ) (LEQ . GTR)
		   (GEQ . LSS) (LSS . GEQ)
		   (VC . VS) (VS . VC)
		   (CC . CS) (CS . CC)
		   (GTRU . LEQU) (LEQU . GTRU)
		   (GEQU . LSSU) (LSSU . GEQU)))
	   (error "INVERT-CC: Not a known CC" cc))))

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER)
	      (LAP (MOV L ,(coerce->any (cadr expression)) ,target)))
	     ((OFFSET)
	      (LAP
	       (MOV L
		    ,(indirect-reference! (cadadr expression)
					  (caddr expression))
		    ,target)))
	     ((CONSTANT)
	      (LAP ,(load-constant (cadr expression) target)))
	     ((UNASSIGNED)
	      (LAP ,(load-non-pointer type-code:unassigned 0 target)))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define (indirect-reference! register offset)
  (if (= register regnum:frame-pointer)
      (offset-reference regnum:stack-pointer (+ offset (frame-pointer-offset)))
      (offset-reference
       (if (machine-register? register)
	   register
	   (or (register-alias register false)
	       ;; This means that someone has written an address out
	       ;; to memory, something that should happen only when the
	       ;; register block spills something.
	       (begin (warn "Needed to load indirect register!" register)
		      (load-alias-register! register 'GENERAL))))
       offset)))

(define (coerce->any register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (pseudo-register-home register)))))

(define (coerce->machine-register register)
  (if (machine-register? register)
      (register-reference register)
      (reference-alias-register! register false)))

;; *** What is this? ***

(define (code-object-label-initialize code-object)
  false)

(define (generate-n-times n limit instruction-gen with-counter)
  (if (> n limit)
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,(load-rnw (-1+ n) counter)
		(LABEL ,loop)
		,(instruction-gen)
		(SOB GEQ (R ,counter) (@PCR ,loop))))))
      (let loop ((n n))
	(if (zero? n)
	    (LAP)
	    (LAP ,(instruction-gen)
		 ,@(loop (-1+ n)))))))

(define-integrable (lap:ea-keyword expression)
  (car expression))

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
  (set! compiler:external-labels
	(cons label compiler:external-labels))
  (LAP (ENTRY-POINT ,label)
       (BLOCK-OFFSET ,label)
       (LABEL ,label)))

;;;; Registers/Entries

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
  (define-entries #x00F0
    return-to-interpreter uuo-link-trap apply error
    wrong-number-of-arguments interrupt-procedure
    interrupt-continuation lookup-apply lookup access unassigned?
    unbound?  set!  define primitive-apply setup-lexpr
    safe-lookup cache-variable reference-trap assignment-trap uuo-link
    cache-reference-apply safe-reference-trap unassigned?-trap
    cache-variable-multiple uuo-link-multiple))

(define-integrable reg:compiled-memtop (INST-EA (@R 13)))
(define-integrable reg:environment (INST-EA (@RO B 13 #x0C)))
(define-integrable reg:temp (INST-EA (@RO B 13 #x10)))
(define-integrable reg:enclose-result (INST-EA (@RO B 13 #x14)))

;; These are the results of using bump-type on the corresponding values.
(define-integrable reg:temp-type (INST-EA (@RO B 13 #x13)))
(define-integrable reg:enclose-result-type (INST-EA (@RO B 13 #x17)))

(define (bump-type effective-address)
  (cond ((eq? (lap:ea-keyword effective-address) '@R)
	 (INST-EA (@RO B ,(lap:ea-@R-register effective-address) 3)))
	((eq? (lap:ea-keyword effective-address) '@RO)
	 (let ((offset (+ 3 (lap:ea-@RO-offset effective-address))))
	   (INST-EA (@RO ,(offset-type offset)
			 ,(lap:ea-@RO-register effective-address)
			 ,offset))))
	(else #F)))

(define (immediate-type type-code)
  (if (<= 0 type-code 63)
      (INST-EA (S ,type-code))
      (INST-EA (& ,type-code))))
