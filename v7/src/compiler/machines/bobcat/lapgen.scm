#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/lapgen.scm,v 4.20 1989/07/25 12:40:04 arthur Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; RTL Rules for 68020.  Part 1

(declare (usual-integrations))

;;;; Basic machine instructions

(define (reference->register-transfer source target)
  (if (or (and (effective-address/data-register? source)
	       (= (lap:ea-operand-1 source) target))
	  (and (effective-address/address-register? source)
	       (= (+ 8 (lap:ea-operand-1 source)) target)))
      (LAP)
      (memory->machine-register source target)))

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

(define-integrable (pseudo-register-offset register)
  (+ 180 (* 3 (register-renumber register))))

(define-integrable (pseudo-register-home register)
  (offset-reference regnum:regs-pointer
		    (pseudo-register-offset register)))

(define-integrable (machine->machine-register source target)
  (cond ((float-register? source)
	 (if (float-register? target)
	     (INST (FMOVE ,source ,target))
	     (error "Moving from floating point register to non-fp register")))
	((float-register? target)
	 (error "Moving from non-floating point register to fp register"))
	(else (INST (MOV L
			 ,(register-reference source)
			 ,(register-reference target))))))

(define-integrable (machine-register->memory source target)
  (if (float-register? source)
      (INST (FMOVE X ,(register-reference source) ,target))
      (INST (MOV L ,(register-reference source) ,target))))

(define-integrable (memory->machine-register source target)
  (if (float-register? target)
      (INST (FMOVE X ,source ,(register-reference target)))
      (INST (MOV L ,source ,(register-reference target)))))

(package (offset-reference byte-offset-reference)

(define ((make-offset-reference grain-size) register offset)
    (if (zero? offset)
	(if (< register 8)
	    (INST-EA (@D ,register))
	    (INST-EA (@A ,(- register 8))))
	(if (< register 8)
	    (INST-EA (@DO ,register ,(* grain-size offset)))
	    (INST-EA (@AO ,(- register 8) ,(* grain-size offset))))))

(define-export offset-reference
  (make-offset-reference
   (quotient scheme-object-width addressing-granularity)))

(define-export byte-offset-reference
  (make-offset-reference
   (quotient 8 addressing-granularity)))

)

(define (load-dnw n d)
  (cond ((zero? n)
	 (INST (CLR W (D ,d))))
	((<= -128 n 127)
	 (INST (MOVEQ (& ,n) (D ,d))))
	(else
	 (INST (MOV W (& ,n) (D ,d))))))

(define (test-dnw n d)
  (if (zero? n)
      (INST (TST W (D ,d)))
      (INST (CMPI W (& ,n) (D ,d)))))

(define (increment-machine-register register n)
  (let ((target (register-reference register)))
    (case n
      ((0) (LAP))
      ((1 2) (LAP (ADDQ L (& ,(* 4 n)) ,target)))
      ((-1 -2) (LAP (SUBQ L (& ,(* -4 n)) ,target)))
      (else
       (if (< register 8)
	   (LAP (ADD L (& ,(* 4 n)) ,target))
	   (LAP (LEA (@AO ,(- register 8) ,(* 4 n)) ,target)))))))

(define (load-constant constant target)
  (if (non-pointer-object? constant)
      (load-non-pointer (object-type constant)
			(object-datum constant)
			target)
      (INST (MOV L
		 (@PCR ,(constant->label constant))
		 ,target))))

(define (load-non-pointer type datum target)
  (cond ((not (zero? type))
	 (INST (MOV L
		    (& ,(make-non-pointer-literal type datum))
		    ,target)))
	((and (zero? datum)
	      (effective-address/data&alterable? target))
	 (INST (CLR L ,target)))
	((and (<= -128 datum 127)
	      (effective-address/data-register? target))
	 (INST (MOVEQ (& ,datum) ,target)))
	(else
	 (INST (MOV L (& ,datum) ,target)))))

(define (test-byte n effective-address)
  (if (and (zero? n) (effective-address/data&alterable? effective-address))
      (INST (TST B ,effective-address))
      (INST (CMPI B (& ,n) ,effective-address))))

(define (test-non-pointer type datum effective-address)
  (if (and (zero? type) (zero? datum)
	   (effective-address/data&alterable? effective-address))
      (INST (TST L ,effective-address))
      (INST (CMPI L
		  (& ,(make-non-pointer-literal type datum))
		  ,effective-address))))
 
(define make-non-pointer-literal
  (let ((type-scale-factor (expt 2 24)))
    (lambda (type datum)
      (+ (* (if (negative? datum) (1+ type) type)
	    type-scale-factor)
	 datum))))

(define (set-standard-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (B ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (B ,(invert-cc cc) (@PCR ,label))))))

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

(define (invert-cc-noncommutative cc)
  ;; Despite the fact that the name of this procedure is similar to
  ;; that of `invert-cc', it is quite different.  `invert-cc' is used
  ;; when the branches of a conditional are being exchanged, while
  ;; this is used when the arguments are being exchanged.
  (cdr (or (assq cc
		 '((HI . LO) (LO . HI)
		   (HS . LS) (LS . HS)
		   (CC . LS) (CS . HI)
		   (PL . MI) (MI . PL)
		   (GE . LE) (LE . GE)
		   (GT . LT) (LT . GT)
		   (T . T) (F . F)
		   (NE . NE) (EQ . EQ)
		   (VC . VC) (VS . VS)
		   ))
	   (error "INVERT-CC-NONCOMMUTATIVE: Not a known CC" cc))))

(define-integrable (cc-commutative? cc)
  (memq cc '(T F NE EQ)))

(define-integrable (effective-address/data&alterable? ea)
  (memq (lap:ea-keyword ea) '(D @D @A @A+ @-A @AO @DO @AOX W L)))

(define-integrable (effective-address/register? ea)
  (memq (lap:ea-keyword ea) '(A D)))

(define-integrable (effective-address/data-register? ea)
  (eq? (lap:ea-keyword ea) 'D))

(define-integrable (effective-address/address-register? ea)
  (eq? (lap:ea-keyword ea) 'A))

(define (standard-target-reference target)
  ;; Our preference for data registers here is a heuristic that works
  ;; reasonably well since if the value is a pointer, we will probably
  ;; want to dereference it, which requires that we first mask it.
  (delete-dead-registers!)
  (register-reference
   (or (register-alias target 'DATA)
       (register-alias target 'ADDRESS)
       (allocate-alias-register! target 'DATA))))

(define-integrable (preferred-data-register-reference register)
  (register-reference (preferred-data-register register)))

(define (preferred-data-register register)
  (or (register-alias register 'DATA)
      (register-alias register 'ADDRESS)
      (load-alias-register! register 'DATA)))

(define-integrable (preferred-address-register-reference register)
  (register-reference (preferred-address-register register)))

(define (preferred-address-register register)
  (or (register-alias register 'ADDRESS)
      (register-alias register 'DATA)
      (load-alias-register! register 'ADDRESS)))

(define (offset->indirect-reference! offset)
  (indirect-reference! (rtl:register-number (rtl:offset-register offset))
		       (rtl:offset-number offset)))

(define (indirect-reference! register offset)
  (offset-reference (allocate-indirection-register! register) offset))

(define (indirect-byte-reference! register offset)
  (byte-offset-reference (allocate-indirection-register! register) offset))

(define (allocate-indirection-register! register)
  (if (machine-register? register)
      register
      (preferred-address-register register)))

(define (code-object-label-initialize code-object)
  code-object
  false)

(define (generate-n-times n limit instruction-gen with-counter)
  (if (> n limit)
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   (LAP ,(load-dnw (-1+ n) counter)
		(LABEL ,loop)
		,(instruction-gen)
		(DB F (D ,counter) (@PCR ,loop))))))
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

(define (put-type-in-ea type-code ea)
  (cond ((effective-address/data-register? ea)
	 (LAP (AND L ,mask-reference ,ea)
	      (OR L (& ,(make-non-pointer-literal type-code 0)) ,ea)))
	((effective-address/data&alterable? ea)
	 (LAP (MOV B (& ,type-code) ,ea)))
	(else
	 (error "PUT-TYPE-IN-EA: Illegal effective-address" ea))))

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
	((rtl:free-push? target) (INST-EA (@A+ 5)))
	((rtl:stack-push? target) (INST-EA (@-A 7)))
	(else (error "STANDARD-TARGET->EA: Not a standard target" target))))

;;;; Fixnum Operators

(define (signed-fixnum? n)
  (and (integer? n)
       (>= n signed-fixnum/lower-limit)
       (< n signed-fixnum/upper-limit)))

(define (unsigned-fixnum? n)
  (and (integer? n)
       (not (negative? n))
       (< n unsigned-fixnum/upper-limit)))

(define (guarantee-signed-fixnum n)
  (if (not (signed-fixnum? n)) (error "Not a signed fixnum" n))
  n)

(define (guarantee-unsigned-fixnum n)
  (if (not (unsigned-fixnum? n)) (error "Not a unsigned fixnum" n))
  n)

(define-integrable (load-fixnum-constant constant register-reference)
  (LAP (MOV L (& ,(* #x100 constant)) ,register-reference)))

(define-integrable (object->fixnum reg-ref)
  (LAP (LS L L (& 8) ,reg-ref)))

(define-integrable (address->fixnum reg-ref)
  (LAP (LS L L (& 8) ,reg-ref)))

(define (fixnum->object reg-ref)
  (LAP
   (MOV B (& ,(ucode-type fixnum)) ,reg-ref)
   (RO R L (& 8) ,reg-ref)))

(define-integrable (fixnum->address reg-ref)
  (LAP
   (LS R L (& 8) ,reg-ref)))

(define (test-fixnum effective-address)
  (if (effective-address/data&alterable? effective-address)
      (INST (TST L ,effective-address))
      (INST (CMPI L (& 0) ,effective-address))))

(define (fixnum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FIXNUM? ZERO-FIXNUM?) 'EQ)
    ((LESS-THAN-FIXNUM? NEGATIVE-FIXNUM?) 'LT)
    ((GREATER-THAN-FIXNUM? POSITIVE-FIXNUM?) 'GT)
    (else (error "FIXNUM-PREDICATE->CC: Unknown predicate" predicate))))

(define-integrable (fixnum-2-args/commutative? operator)
  (memq operator '(PLUS-FIXNUM MULTIPLY-FIXNUM)))

(define (reuse-and-load-fixnum-target! target source operate-on-target)
  (reuse-fixnum-target! target
    (lambda (target)
      (operate-on-target (move-to-alias-register! source 'DATA target)))
    (lambda (target)
      (LAP (MOV L ,(standard-register-reference source 'DATA) ,target)
	   ,@(operate-on-target target)))))

(define (reuse-fixnum-target! target
			      operate-on-pseudo-target
			      operate-on-machine-target)
  (let ((use-temporary
	 (lambda (target)
	   (let ((temp (reference-temporary-register! 'DATA)))
	     (LAP ,@(operate-on-machine-target temp)
		  (MOV L ,temp ,target))))))
    (case (rtl:expression-type target)
      ((REGISTER)
       (let ((register (rtl:register-number target)))
	 (if (pseudo-register? register)
	     (operate-on-pseudo-target register)
	     (let ((target (register-reference register)))
	       (if (data-register? register)
		   (operate-on-machine-target target)
		   (use-temporary target))))))
       ((OFFSET)
	(use-temporary (offset->indirect-reference! target)))
       (else
	(error "REUSE-FIXNUM-TARGET!: Unknown fixnum target" target)))))

(define (fixnum-operation-target? target)
  (or (rtl:register? target)
      (rtl:offset? target)))

(define (define-fixnum-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-fixnum-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define fixnum-methods/1-arg
  (list 'FIXNUM-METHODS/1-ARG))

(define-integrable (fixnum-1-arg/operate operator)
  (lookup-fixnum-method operator fixnum-methods/1-arg))

(define fixnum-methods/2-args
  (list 'FIXNUM-METHODS/2-ARGS))

(define-integrable (fixnum-2-args/operate operator)
  (lookup-fixnum-method operator fixnum-methods/2-args))

(define fixnum-methods/2-args-constant
  (list 'FIXNUM-METHODS/2-ARGS-CONSTANT))

(define-integrable (fixnum-2-args/operate-constant operator)
  (lookup-fixnum-method operator fixnum-methods/2-args-constant))

(define-fixnum-method 'ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (reference)
    (LAP (ADD L (& #x100) ,reference))))

(define-fixnum-method 'MINUS-ONE-PLUS-FIXNUM fixnum-methods/1-arg
  (lambda (reference)
    (LAP (SUB L (& #x100) ,reference))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args
  (lambda (target source)
    (LAP (ADD L ,source ,target))))

(define-fixnum-method 'PLUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n) (LAP))
	  (else (LAP (ADD L (& ,(* n #x100)) ,target))))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args
  (lambda (target source)
    (if (equal? target source)
	(let ((new-source (reference-temporary-register! 'DATA)))
	  ;;; I should add new-source as an alias for source, but I
	  ;;; don't have a handle on the actual register here (I just
	  ;;; have the register-reference).  Maybe this should be
	  ;;; moved into the rules.
	  (LAP
	   (MOV L ,source ,new-source)
	   (AS R L (& 8) ,target)
	   (MUL S L ,new-source ,target)))
	(LAP
	 (AS R L (& 8) ,target)
	 (MUL S L ,source ,target)))))

(define-fixnum-method 'MULTIPLY-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n) (LAP (CLR L ,target)))
	  ((= n 1) (LAP))
	  ((= n -1) (LAP (NEG L ,target)))
	  (else
	   (let ((power-of-2 (integer-log-base-2? n)))
	     (if power-of-2
		 (if (> power-of-2 8)
		     (let ((temp (reference-temporary-register! 'DATA)))
		       (LAP (MOV L (& ,power-of-2) ,temp)
			    (AS L L ,temp ,target)))
		     (LAP (AS L L (& ,power-of-2) ,target)))
		 (LAP (MUL S L (& ,n) ,target))))))))

(define (integer-log-base-2? n)
  (let loop ((power 1) (exponent 0))
    (cond ((< n power) false)
	  ((= n power) exponent)
	  (else (loop (* 2 power) (1+ exponent))))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args
  (lambda (target source)
    (LAP (SUB L ,source ,target))))

(define-fixnum-method 'MINUS-FIXNUM fixnum-methods/2-args-constant
  (lambda (target n)
    (cond ((zero? n) (LAP))
	  (else (LAP (SUB L (& ,(* n #x100)) ,target))))))

;;;; Flonum Operators

(define (float-target-reference target)
  (delete-dead-registers!)
  (register-reference
   (or (register-alias target 'FLOAT)
       (allocate-alias-register! target 'FLOAT))))

(define (define-flonum-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-flonum-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))


(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

(define-integrable (flonum-1-arg/operate operator)
  (lookup-flonum-method operator flonum-methods/1-arg))

;;; Notice the weird ,', syntax here.  If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name instruction-name)
	 `(define-flonum-method ',primitive-name flonum-methods/1-arg
	    (lambda (source target)
	      (LAP (,instruction-name ,',source ,',target)))))))
  (define-flonum-operation SINE-FLONUM FSIN)
  (define-flonum-operation COSINE-FLONUM FCOS)
  (define-flonum-operation ARCTAN-FLONUM FATAN)
  (define-flonum-operation EXP-FLONUM FETOX)
  (define-flonum-operation LN-FLONUM FLOGN)
  (define-flonum-operation SQRT-FLONUM FSQRT)
  (define-flonum-operation TRUNCATE-FLONUM FINT))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(define-integrable (flonum-2-args/operate operator)
  (lookup-flonum-method operator flonum-methods/2-args))

(let-syntax
    ((define-flonum-operation
       (macro (primitive-name instruction-name)
	 `(define-flonum-method ',primitive-name flonum-methods/2-args
	   (lambda (source target)
	     (LAP (,instruction-name ,',source ,',target)))))))
  (define-flonum-operation PLUS-FLONUM FADD)
  (define-flonum-operation MINUS-FLONUM FSUB)
  (define-flonum-operation MULTIPLY-FLONUM FMUL)
  (define-flonum-operation DIVIDE-FLONUM FDIV))

(define (invert-float-cc cc)
  (cdr (or (assq cc
		'((EQ . NE) (NE . EQ)
		  (GT . NGT) (NGT . GT)
		  (GE . NGE) (NGE . GE)
		  (LT . NLT) (NLT . LT)
		  (LE . NLE) (NLE . LE)
		  (GL . NGL) (NGL . GL)
		  (MI . PL) (PL . MI)))
	   (error "INVERT-FLOAT-CC: Not a known CC" cc))))


(define (set-flonum-branches! cc)
  (set-current-branches!
   (lambda (label)
     (LAP (FB ,cc (@PCR ,label))))
   (lambda (label)
     (LAP (FB ,(invert-float-cc cc) (@PCR ,label))))))

(define (flonum-predicate->cc predicate)
  (case predicate
    ((EQUAL-FLONUM? ZERO-FLONUM?) 'EQ)
    ((LESS-THAN-FLONUM? NEGATIVE-FLONUM?) 'LT)
    ((GREATER-THAN-FLONUM? POSITIVE-FLONUM?) 'GT)
    (else (error "FLONUM-PREDICATE->CC: Unknown predicate" predicate))))
;;;; OBJECT->DATUM rules - Mhwu
;;;  Similar to fixnum rules, but no sign extension

(define (load-constant-datum constant register-ref)
  (if (non-pointer-object? constant)
      (LAP (MOV L (& ,(object-datum constant)) ,register-ref))
      (LAP (MOV L
		(@PCR ,(constant->label constant))
		,register-ref)
	   ,@(object->address register-ref))))

(define-integrable (object->address register-reference)
  (LAP (AND L ,mask-reference ,register-reference)))

(define-integrable (object->datum register-reference)
  (LAP (AND L ,mask-reference ,register-reference)))

(define-integrable (object->type register-reference)
  (LAP (RO L L (& 8) ,register-reference)))
;;;; CHAR->ASCII rules

(define (coerce->any/byte-reference register)
  (if (machine-register? register)
      (register-reference register)
      (let ((alias (register-alias register false)))
	(if alias
	    (register-reference alias)
	    (indirect-char/ascii-reference!
	     regnum:regs-pointer
	     (pseudo-register-offset register))))))

(define (indirect-char/ascii-reference! register offset)
  (indirect-byte-reference! register (+ (* offset 4) 3)))

(define (char->signed-8-bit-immediate character)
  (let ((ascii (char->ascii character)))
    (if (< ascii 128) ascii (- ascii 256))))

(define (byte-offset->register source source-reg target)
  ;; This code uses a temporary register because right now the register
  ;; allocator thinks that it could use the same register for the target
  ;; and source, while what we want to happen is to first clear the target
  ;; and then move from source to target.
  ;; Optimal Code: (CLR L ,target-ref)
  ;;               (MOV B ,source ,target)
  ;; source-register is passed in to check for this. Yuck.
  (delete-dead-registers!)
  (let* ((temp-ref (register-reference (allocate-temporary-register! 'DATA)))
	 (target (allocate-alias-register! target 'DATA)))
    (if (= target source-reg)
	(LAP (CLR L ,temp-ref)
	     (MOV B ,source ,temp-ref)
	     (MOV L ,temp-ref ,(register-reference target)))
	(LAP (CLR L ,(register-reference target))
	     (MOV B ,source ,(register-reference target))))))

(define (indirect-register register)
  (if (machine-register? register)
      register
      (register-alias register false)))

;;;; Registers/Entries

(define-integrable (data-register? register)
  (< register 8))

(define (address-register? register)
  (and (< register 16)
       (>= register 8)))

(define (float-register? register)
  (and (< register 24)
       (>= register 16)))

(define-integrable (lap:ea-keyword expression)
  (car expression))

(define-integrable (lap:ea-operand-1 expression)
  (cadr expression))

(define-integrable (lap:ea-operand-2 expression)
  (caddr expression))

(define (lap:make-label-statement label)
  (INST (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (INST (BRA (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
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
				(INST-EA (@AO 6 ,index)))
			     (loop (cdr names) (+ index 6)))))
		 `(BEGIN ,@(loop names start)))))
  (define-entries #x012c
    link error apply
    lexpr-apply primitive-apply primitive-lexpr-apply
    cache-reference-apply lookup-apply
    interrupt-continuation interrupt-ic-procedure
    interrupt-procedure interrupt-closure
    lookup safe-lookup set! access unassigned? unbound? define
    reference-trap safe-reference-trap assignment-trap unassigned?-trap
    &+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?))

(define-integrable reg:compiled-memtop (INST-EA (@A 6)))
(define-integrable reg:environment (INST-EA (@AO 6 #x000C)))
(define-integrable reg:temp (INST-EA (@AO 6 #x0010)))
(define-integrable reg:enclose-result (INST-EA (@AO 6 #x0014)))
(define-integrable reg:lexpr-primitive-arity (INST-EA (@AO 6 #x001C)))

(define-integrable popper:apply-closure (INST-EA (@AO 6 #x0168)))
(define-integrable popper:apply-stack (INST-EA (@AO 6 #x01A8)))
(define-integrable popper:value (INST-EA (@AO 6 #x01E8)))