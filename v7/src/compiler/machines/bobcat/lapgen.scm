#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 1.183 1987/06/15 22:03:23 cph Exp $

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

;;;; RTL Rules for 68020

(declare (usual-integrations))

;;;; Basic machine instructions

(define (register->register-transfer source target)
  `(,(machine->machine-register source target)))

(define (home->register-transfer source target)
  `(,(pseudo->machine-register source target)))

(define (register->home-transfer source target)
  `(,(machine->pseudo-register source target)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (+ #x000A (register-renumber register))))

(define-integrable (machine->machine-register source target)
  `(MOVE L ,(register-reference source) ,(register-reference target)))

(define-integrable (machine-register->memory source target)
  `(MOVE L ,(register-reference source) ,target))

(define-integrable (memory->machine-register source target)
  `(MOVE L ,source ,(register-reference target)))

(define (offset-reference register offset)
  (if (zero? offset)
      (if (< register 8)
	  `(@D ,register)
	  `(@A ,(- register 8)))
      (if (< register 8)
	  `(@DO ,register ,(* 4 offset))
	  `(@AO ,(- register 8) ,(* 4 offset)))))

(define (load-dnw n d)
  (cond ((zero? n) `(CLR W (D ,d)))
	((<= -128 n 127) `(MOVEQ (& ,n) (D ,d)))
	(else `(MOVE W (& ,n) (D ,d)))))

(define (test-dnw n d)
  (if (zero? n)
      `(TST W (D ,d))
      `(CMP W (& ,n) (D ,d))))

(define (increment-anl an n)
  (case n
    ((0) '())
    ((1 2) `((ADDQ L (& ,(* 4 n)) (A ,an))))
    ((-1 -2) `((SUBQ L (& ,(* -4 n)) (A ,an))))
    (else `((LEA (@AO ,an ,(* 4 n)) (A ,an))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (primitive-type constant)
			(primitive-datum constant)
			target)
      `(MOVE L (@PCR ,(constant->label constant)) ,target)))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 `(MOVE L (& ,(make-non-pointer-literal type datum)) ,target))
	((and (zero? datum)
	      (memq (car target) '(D @D @A @A+ @-A @AO @DO @AOX W L)))
	 `(CLR L ,target))
	((and (<= -128 datum 127) (eq? (car target) 'D))
	 `(MOVEQ (& ,datum) ,target))
	(else
	 `(MOVE L (& ,datum) ,target))))

(define (test-byte n expression)
  (if (and (zero? n) (TSTable-expression? expression))
      `(TST B ,expression)
      `(CMP B (& ,n) ,expression)))

(define (test-non-pointer type datum expression)
  (if (and (zero? type) (zero? datum) (TSTable-expression? expression))
      `(TST L ,expression)
      `(CMP L (& ,(make-non-pointer-literal type datum)) ,expression)))

(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (set-standard-branches! cc)
  (set-current-branches! (lambda (label)
			   `((B ,cc L (@PCR ,label))))
			 (lambda (label)
			   `((B ,(invert-cc cc) L (@PCR ,label))))))

(define (invert-cc cc)
  (cdr (or (assq cc
		 '((T . F) (F . T)
		   (HI . LS) (LS . HI)
		   (HS . LO) (LO . HS)
		   (CC . CS) (CS . CC)
		   (NE . EQ) (EQ . NE)
		   (VC . VS) (VS . VC)
		   (PL . MI) (MI . PL)
		   (GE . LT) (LT . GE)
		   (GT . LE) (LE . GT)
		   ))
	   (error "INVERT-CC: Not a known CC" cc))))

(define (expression->machine-register! expression register)
  (let ((target (register-reference register)))
    (let ((result
	   (case (car expression)
	     ((REGISTER)
	      `((MOVE L ,(coerce->any (cadr expression)) ,target)))
	     ((OFFSET)
	      `((MOVE L
		      ,(indirect-reference! (cadadr expression)
					    (caddr expression))
		      ,target)))
	     ((CONSTANT)
	      `(,(load-constant (cadr expression) target)))
	     ((UNASSIGNED)
	      `(,(load-non-pointer type-code:unassigned 0 target)))
	     (else
	      (error "Unknown expression type" (car expression))))))
      (delete-machine-register! register)
      result)))

(define-integrable (TSTable-expression? expression)
  (memq (car expression) '(D @D @A @A+ @-A @DO @AO @AOX W L)))

(define-integrable (register-expression? expression)
  (memq (car expression) '(A D)))

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
		      ;; Should specify preference for ADDRESS but will
		      ;; accept DATA if no ADDRESS registers available.
		      (load-alias-register! register 'ADDRESS))))
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

(define (code-object-label-initialize code-object)
  false)

(define (generate-n-times n limit instruction with-counter)
  (if (<= n limit)
      (let loop ((n n))
	(if (zero? n)
	    '()
	    `(,instruction
	      ,@(loop (-1+ n)))))
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   `(,(load-dnw (-1+ n) counter)
	     (LABEL ,loop)
	     ,instruction
	     (DB F (D ,counter) (@PCR ,loop))))))))

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro (start . names)
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE ,(symbol-append 'ENTRY:COMPILER-
						      (car names))
				'(@AO 6 ,index))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names start)))))
  (define-entries #x00F0 apply error wrong-number-of-arguments
    interrupt-procedure interrupt-continuation lookup-apply lookup access
    unassigned? unbound? set! define primitive-apply enclose setup-lexpr
    return-to-interpreter safe-lookup cache-variable reference-trap
    assignment-trap)
  (define-entries #x0228 uuo-link uuo-link-trap cache-reference-apply
    safe-reference-trap unassigned?-trap cache-variable-multiple
    uuo-link-multiple))

(define reg:compiled-memtop '(@A 6))
(define reg:environment '(@AO 6 #x000C))
(define reg:temp '(@AO 6 #x0010))
(define reg:enclose-result '(@AO 6 #x0014))

(define popper:apply-closure '(@AO 6 #x0168))
(define popper:apply-stack '(@AO 6 #x01A8))
(define popper:value '(@AO 6 #x01E8))