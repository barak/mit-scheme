#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; RTL Rules for Alpha.  Shared utilities.
;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-Allocator Interface

(define (register->register-transfer source target)
  (guarantee-registers-compatible source target)
  (case (register-type source)
    ((GENERAL) (copy source target))
    ((FLOAT) (fp-copy source target))
    (else (error "unknown register type" source))))

(define (home->register-transfer source target)
  (memory->register-transfer (pseudo-register-displacement source)
			     regnum:regs-pointer
			     target))

(define (register->home-transfer source target)
  (register->memory-transfer source
			     (pseudo-register-displacement target)
			     regnum:regs-pointer))

(define (reference->register-transfer source target)
  (case (ea/mode source)
    ((GR)
     (copy (register-ea/register source) target))
    ((FPR)
     (fp-copy (fpr->float-register (register-ea/register source)) target))
    ((OFFSET)
     (memory->register-transfer (offset-ea/offset source)
				(offset-ea/register source)
				target))
    (else
     (error "unknown effective-address mode" source))))

(define (pseudo-register-home register)
  ;; Register block consists of 16 8-byte registers followed by 256
  ;; 8-byte temporaries.
  (INST-EA (OFFSET ,(pseudo-register-displacement register)
		   ,regnum:regs-pointer)))

(define-integrable (sort-machine-registers registers)
  registers)

(define available-machine-registers
  (list
   ;; r0 -- return value
   r1 ;; -- utility index
   ;; r2 -- stack pointer
   ;; r3 -- memtop
   ;; r4 -- free
   ;; r5 -- dynamic link
   r6 r7 r8
   ;; r9 -- register pointer
   ;; r10 -- scheme-to-interface
   ;; r11 -- closure hook
   ;; r12 -- scheme-to-interface-jsr
   ;; r13 -- compiled-entry type bits
   ;; r14 -- closure free
   r15 r16 r17 r18 r19 r20 r21 r22 r23 r24 r25 r26 r27
   ;; r28 -- assembler temp / came from
   r29
   ;; r30 -- C stack pointer
   ;; r31 -- ZERO
   f0 f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15
   f16 f17 f18 f19 f20 f21 f22 f23 f24 f25 f26 f27 f28
   f29 f30
   ;; f31 -- ZERO.
   ))

(define-integrable (float-register? register)
  (eq? (register-type register) 'FLOAT))

(define-integrable (general-register? register)
  (eq? (register-type register) 'GENERAL))

(define-integrable (word-register? register)
  (eq? (register-type register) 'GENERAL))

(define (register-type register)
  (cond ((machine-register? register)
	 (vector-ref
	  '#(; 0       1       2       3       4       5       6       7
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
	     GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT
              FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT
              FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT
              FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT   FLOAT)
	  register))
	((register-value-class=word? register) 'GENERAL)
	((register-value-class=float? register) 'FLOAT)
	(else (error "unable to determine register type" register))))

(define register-reference
  ; Needed by standard-register-reference in lapgn2
  (let ((references (make-vector number-of-machine-registers)))
    (let loop ((register 0))
      (if (< register 32)
	  (begin
	    (vector-set! references register (INST-EA (GR ,register)))
	    (loop (1+ register)))))
    (let loop ((register 32) (fpr 0))
      (if (< register 64)
	  (begin
	    (vector-set! references register (INST-EA (FPR ,fpr)))
	    (loop (1+ register) (1+ fpr)))))
    (lambda (register)
      (vector-ref references register))))

;;;; Utilities for the rules

(define (require-register! machine-reg)
  (flush-register! machine-reg)
  (need-register! machine-reg))

(define-integrable (flush-register! machine-reg)
  (prefix-instructions! (clear-registers! machine-reg)))

(define (rtl-target:=machine-register! rtl-reg machine-reg)
  (if (machine-register? rtl-reg)
      (begin
	(require-register! machine-reg)
	(if (not (= rtl-reg machine-reg))
	    (suffix-instructions!
	     (register->register-transfer machine-reg rtl-reg))))
      (begin
	(delete-register! rtl-reg)
	(flush-register! machine-reg)
	(add-pseudo-register-alias! rtl-reg machine-reg))))

;;;; Useful Cliches

(define (memory->register-transfer offset base target)
  (case (register-type target)
    ((GENERAL) (LAP (LDQ ,target (OFFSET ,offset ,base))))
    ((FLOAT) (fp-load-doubleword offset base target))
    (else (error "unknown register type" target))))

(define (register->memory-transfer source offset base)
  (case (register-type source)
    ((GENERAL) (LAP (STQ ,source (OFFSET ,offset ,base))))
    ((FLOAT) (fp-store-doubleword offset base source))
    (else (error "unknown register type" source))))

(define (load-constant target constant record?)
  ;; Load a Scheme constant into a machine register.
  (if (non-pointer-object? constant)
      (load-immediate target (non-pointer->literal constant) record?)
      (load-pc-relative target
			'CONSTANT
			(constant->label constant))))

(define (deposit-type-address type source target)
  (if (= type (ucode-type compiled-entry))
      (LAP (BIS ,regnum:compiled-entry-type-bits ,source ,target))
      (deposit-type-datum type source target)))

(define (deposit-type-datum type source target)
  (with-values
      (lambda ()
	(immediate->register (make-non-pointer-literal type 0)))
    (lambda (prefix alias)
      (LAP ,@prefix
	   (BIS ,alias ,source ,target)))))

(define (non-pointer->literal constant)
  (make-non-pointer-literal (object-type constant)
			    (careful-object-datum constant)))

(define-integrable (make-non-pointer-literal type datum)
  (+ (* type (expt 2 scheme-datum-width)) datum))

;;;; Regularized Machine Instructions

(define-integrable (fits-in-8-bits-unsigned? value)
  (<= #x0 value #xff))

(define-integrable (fits-in-16-bits-signed? value)
  (<= #x-8000 value #x7fff))

(define-integrable (fits-in-16-bits-unsigned? value)
  (<= #x0 value #xffff))

(define-integrable (fits-in-32-bits-signed? value)
  (fits-in-16-bits-signed? (quotient value #x10000)))

(define (top-16-of-32-bits-only? value)
  (let ((result (integer-divide value #x10000)))
    (and (zero? (integer-divide-remainder result))
	 (fits-in-16-bits-signed? (integer-divide-quotient result)))))

; The adjustments are only good when n is 32 bits long.

(define (adjusted:high n)
  (let ((n (->unsigned n 32)))
    (if (< (remainder n #x10000) #x8000)
	(->signed (quotient n #x10000) 16)
	(->signed (+ (quotient n #x10000) 1) 16))))

(define (adjusted:low n)
  (let ((remainder (remainder (->unsigned n 32) #x10000)))
    (if (< remainder #x8000)
	remainder
	(- remainder #x10000))))

(define (split-64-bits n)
  (let* ((n (->unsigned n 64))
	 (split (integer-divide n #x100000000)))
    (let ((rem (integer-divide-remainder split))
	  (quo (integer-divide-quotient split)))
      (if (or (>= rem #x80000000)
	      (negative? (adjusted:high rem)))
	  (values (->signed (1+ quo) 32)
		  (->signed (- rem #x100000000) 32))
	  (values (->signed quo 32)
		  (->signed rem 32))))))

(define (->unsigned n nbits)
  (if (negative? n)
      (+ (expt 2 nbits) n)
      n))

(define (->signed n nbits)
  (if (>= n (expt 2 (- nbits 1)))
      (- n (expt 2 nbits))
      n))

(define (copy r t)
  (if (= r t)
      (LAP)
      (LAP (COPY ,r ,t))))

(define (fp-copy from to)
  (if (= to from)
      (LAP)
      (LAP (CPYS ,(float-register->fpr from)
		 ,(float-register->fpr from)
		 ,(float-register->fpr to)))))

(define (fp-load-doubleword offset base target)
  (LAP (LDT ,(float-register->fpr target)
	    (OFFSET ,offset ,base))))

(define (fp-store-doubleword offset base source)
  (LAP (STT ,(float-register->fpr source)
	    (OFFSET ,offset ,base))))

;;;; PC-relative addresses

(define (load-pc-relative target type label)
  ;; Load a pc-relative location's contents into a machine register.
  ;; Optimization: if there is a register that contains the value of
  ;; another label, use that register as the base register.
  ;; Otherwise, allocate a temporary and load it with the value of the
  ;; label, then use the temporary as the base register.  This
  ;; strategy of loading a temporary wins if the temporary is used
  ;; again, but loses if it isn't, since loading the temporary takes
  ;; one instruction in addition to the LDQ instruction, while doing a
  ;; pc-relative LDQ instruction takes only two instructions total.
  ;; But pc-relative loads of various kinds are quite common, so this
  ;; should almost always be advantageous.
  (with-values (lambda () (get-typed-label type))
    (lambda (label* alias type-of-label*)
      (cond ((not label*)		; No labels of any kind
	     (let ((temporary (standard-temporary!))
		   (here (generate-label)))
	       (set-typed-label! 'CODE here temporary)
	       (LAP (BR ,temporary (@PCO 0))
		    (LABEL ,here)
		    ,@(if (eq? type 'CODE)
			  (LAP (LDQ ,target
				    (OFFSET (- ,label ,here) ,temporary)))
			  (let ((temp2 (standard-temporary!)))
			    (set-typed-label! type label temp2)
			    (LAP (LDA ,temp2
				      (OFFSET (- ,label ,here) ,temporary))
				 (LDQ ,target (OFFSET 0 ,temp2))))))))
	    ((eq? type type-of-label*)	; We got what we wanted
	     (LAP (LDQ ,target (OFFSET (- ,label ,label*) ,alias))))
	    ((eq? type 'CODE)		; Cheap to generate
	     (let ((temporary (standard-temporary!))
		   (here (generate-label)))
	       (set-typed-label! 'CODE here temporary)
	       (LAP (BR ,temporary (@PCO 0))
		    (LABEL ,here)
		    (LDQ ,target (OFFSET (- ,label ,here) ,temporary)))))
	    (else			; Wrong type of label, and what
					; we need may be expensive
	     (let ((temporary (standard-temporary!)))
	       (set-typed-label! type label temporary)
	       (LAP (LDA ,temporary (OFFSET (- ,label ,label*) ,alias))
		    (LDQ ,target (OFFSET 0 ,temporary)))))))))

(define (load-pc-relative-address target type label)
  ;; Load address of a pc-relative location into a machine register.
  ;; Optimization: if there is another register that contains the
  ;; value of another label, add the difference between the labels to
  ;; that register's contents instead.  The ADDI takes one
  ;; instruction, while the %LOAD-PC-RELATIVE-ADDRESS takes two, so
  ;; this is always advantageous.
  ;;
  ;; IMPORTANT: the target can't be clobbered by the current RTL rule
  ;; (except by this code) since we are remembering its contents in
  ;; the register map.  This implies that the rule better not be
  ;; matching target with a machine register (use pseudo-register? to
  ;; test it).
  (with-values (lambda () (get-typed-label type))
    (lambda (label* alias type-of-label*)
      (cond ((not label*)		; No labels of any kind
	     (let ((temporary (standard-temporary!))
		   (here (generate-label)))
	       (set-typed-label! 'CODE here temporary)
	       (if (not (eq? type 'CODE))
		   (set-typed-label! type label target))
	       (LAP (BR ,temporary (@PCO 0))
		    (LABEL ,here)
		    (LDA ,target
			 (OFFSET (- ,label ,here) ,temporary)))))
	    ((eq? type type-of-label*)	; We got what we wanted
	     (LAP (LDA ,target (OFFSET (- ,label ,label*) ,alias))))
	    ((eq? type 'CODE)		; Cheap to generate
	     (let ((temporary (standard-temporary!))
		   (here (generate-label)))
	       (set-typed-label! 'CODE here temporary)
	       (LAP (BR ,temporary (@PCO 0))
		    (LABEL ,here)
		    (LDA ,target (OFFSET (- ,label ,here) ,temporary)))))
	    (else			; Wrong type of label, and what
					; we need may be expensive
	     (set-typed-label! type label target)
	     (LAP (LDA ,target (OFFSET (- ,label ,label*) ,alias))))))))

;;; Typed labels provide further optimization.  There are two types,
;;; CODE and CONSTANT, that say whether the label is located in the
;;; code block or the constants block of the output.  Statistically,
;;; a label is likely to be closer to another label of the same type
;;; than to a label of the other type.

(define (get-typed-label type)
  (let ((entries (register-map-labels *register-map* 'GENERAL)))
    (let loop ((entries* entries))
      (cond ((null? entries*)
	     ;; If no entries of the given type, use any entry that is
	     ;; available.
	     (let loop ((entries entries))
	       (cond ((null? entries)
		      (values false false false))
		     ((pair? (caar entries))
		      (values (cdaar entries) (cadar entries) (caaar entries)))
		     (else
		      (loop (cdr entries))))))
	    ((and (pair? (caar entries*))
		  (eq? type (caaar entries*)))
	     (values (cdaar entries*) (cadar entries*) type))
	    (else
	     (loop (cdr entries*)))))))

(define (set-typed-label! type label alias)
  (set! *register-map*
	(set-machine-register-label *register-map* alias (cons type label)))
  unspecific)

(define (immediate->register immediate)
  (with-values (lambda () (get-immediate-alias immediate))
    (lambda (register bumper)		; Bumper = #T -> exact hit
      (cond ((not register)
	     (let* ((temporary (standard-temporary!))
		    (code (%load-immediate temporary immediate)))
	       (set! *register-map*
		     (set-machine-register-label *register-map*
						 temporary
						 immediate))
	       (values code temporary)))
	    ((eq? bumper #T) (values (LAP) register))
	    (else
	     (let* ((temporary (standard-temporary!))
		    (code (bumper register temporary)))
	       (set! *register-map*
		     (set-machine-register-label *register-map*
						 temporary
						 immediate))
	       (values code temporary)))))))

(define (bump old-value desired-value)
  (define (zappable? old new)
    (do ((i    8
	       (- i 1))
	 (old  (->unsigned old 64)
	       (quotient old 256))
	 (new  (->unsigned new 64)
	       (quotient new 256))
	 (bit  1
	       (* bit 2))
	 (mask 0
	       (let ((old (remainder old 256))
		     (new (remainder new 256)))
		 (cond ((= old new) mask)
		       ((zero? new) (+ mask bit))
		       (else #F)))))
	((or (not mask) (= i 0)) mask)))

  (define (differs-in-contiguous-bits? old-value desired-value)
    ; 16 bits at the top end, 15 bits elsewhere
    (let ((difference-bits
	   (bit-string-xor
	    (signed-integer->bit-string 64 old-value)
	    (signed-integer->bit-string 64 desired-value))))
      (let ((low-differing-bit
	     (bit-substring-find-next-set-bit
	      difference-bits 0 64)))
	(cond ((not low-differing-bit) (values #F #F))
	      ((>= low-differing-bit 48)
	       (values (bit-string->signed-integer
			(bit-substring difference-bits 48 64))
		       48))
	      ((bit-substring-find-next-set-bit
		difference-bits (+ low-differing-bit 15)
		64)
	       (values #F #F))
	      (else
	       (values (bit-string->unsigned-integer
			(bit-substring difference-bits
			  low-differing-bit
			  (+ low-differing-bit 15)))
		       low-differing-bit))))))

  (define (try-high-and-low value)
    (let ((bits (signed-integer->bit-string 64 value)))
      (let ((low-16 (bit-string->signed-integer
		     (bit-substring bits 0 16))))
	(if (not (= low-16 (bit-string->signed-integer
			    (bit-substring bits 0 48))))
	    (values false false)
	    (let* ((high-16 (bit-string->signed-integer
			     (bit-substring bits 48 64)))
		   (adjusted (cond ((not (negative? low-16)) high-16)
				   ((= high-16 #x7FFF) #x-8000)
				   (else (+ high-16 1)))))
	      (values 3
		      (lambda (source target)
			source		; ignored
			(LAP (MOVEI ,target (& ,adjusted))
			     (SLL ,target (& 48) ,target)
			     (LDA ,target (OFFSET ,low-16 ,target))))))))))

  (let ((desired-value (->signed desired-value 64))
	(old-value (->signed old-value 64)))
    (let ((delta (- desired-value old-value)))
      (cond ((fits-in-16-bits-signed? delta)
	     (values 1
		     (lambda (source target)
		       (LAP (LDA ,target (OFFSET ,delta ,source))))))
	    ((top-16-of-32-bits-only? delta)
	     (values 1
		     (lambda (source target)
		       (LAP (LDAH ,target (OFFSET ,(quotient delta #x10000)
						  ,source))))))
	    ((eqv? old-value (- desired-value))
	     (values 1
		     (lambda (source target)
		       (LAP (SUBQ ,regnum:zero ,source ,target)))))
	    ((eqv? desired-value (- (+ 1 old-value)))
	     (values 1
		     (lambda (source target)
		       (LAP (EQV ,regnum:zero ,source ,target)))))
	    ((zappable? old-value desired-value) 
	     => (lambda (mask)
		  (values 1
			  (lambda (source target)
			    (LAP (ZAP ,source (& ,mask) ,target))))))
	    ((fits-in-32-bits-signed? delta)
	     (values 2
		     (lambda (source target)
		       (LAP (LDA ,target (OFFSET ,(adjusted:low delta) ,source))
			    (LDAH ,target (OFFSET ,(adjusted:high delta)
						  ,target))))))
	    (else
	     (with-values
		 (lambda ()
		   (differs-in-contiguous-bits? old-value desired-value))
	       (lambda (constant shift)
		 (cond ((and (not constant) (eqv? old-value 0))
			(try-high-and-low desired-value))
		       ((not constant) (values #F #F))
		       ((eqv? old-value 0)
			(values 2
				(lambda (source target)
				  source ; Unused
				  (LAP (MOVEI ,target (& ,constant))
				       (SLL ,target (& ,shift) ,target)))))
		       (else
			(values 3
				(lambda (source target)
				  source ; Unused
				  (LAP
				   (MOVEI ,target (& ,constant))
				   (SLL ,target (& ,shift) ,target)
				   (XOR ,target ,source ,target)))))))))))))

(define (get-immediate-alias immediate)
  (let loop ((entries
	      (cons (list 0 regnum:zero)
		    (register-map-labels *register-map* 'GENERAL)))
	     (best-bumper #T)
	     (least-cost #F)
	     (best-register #F))
    (cond ((null? entries)
	   (values best-register best-bumper))
	  ((eqv? (caar entries) immediate)
	   (values (cadar entries) #T))	; Exact match
	  ((not (number? (caar entries)))
	   (loop (cdr entries) best-bumper least-cost best-register))
	  (else
	   (with-values (lambda () (bump (caar entries) immediate))
	     (lambda (cost bumper)
	       (cond ((not cost)
		      (loop (cdr entries) best-bumper
			    least-cost best-register))
		     ((or (not least-cost) (< cost least-cost))
		      (loop (cdr entries) bumper
			    cost (cadar entries)))
		     (else (loop (cdr entries) best-bumper
				 least-cost best-register)))))))))

(define (load-immediate target immediate record?)
  (let ((registers (get-immediate-aliases immediate)))
    (cond ((memv target registers)
	   (LAP))
	  ((not (null? registers))
	   (if record?
	       (set! *register-map*
		     (set-machine-register-label *register-map*
						 target
						 immediate)))
	   (LAP (COPY ,(car registers) ,target)))
	  (else
	   (with-values (lambda () (get-immediate-alias immediate))
	     (lambda (register bumper)
	       (let ((result
		      (if register
			  (bumper register target)
			  (%load-immediate target immediate))))
		 (if record?
		     (set! *register-map*
			   (set-machine-register-label *register-map*
						       target
						       immediate)))
		 result)))))))

(define (get-immediate-aliases immediate)
  (let loop ((entries
	      (cons (list 0 regnum:zero)
		    (register-map-labels *register-map* 'GENERAL))))
    (cond ((null? entries)
	   '())
	  ((eqv? (caar entries) immediate)
	   (append (cdar entries) (loop (cdr entries))))
	  (else
	   (loop (cdr entries))))))

(define (%load-immediate target immediate)
  ; All simple cases are handled above this level.
  #|
  (let ((label (immediate->label immediate)))
    (load-pc-relative target 'IMMEDIATE label))
  |#
  #|
  (warn "%load-immediate: generating 64-bit constant"
	(number->string immediate 16))
  |#
  (with-values (lambda () (split-64-bits immediate))
    (lambda (high low)
      (let ((left-half (load-immediate target high false)))
	(LAP ,@left-half
	     (SLL ,target (& 32) ,target)
	     ,@(add-immediate low target target))))))

(define (add-immediate immediate source target)
  (cond ((fits-in-16-bits-signed? immediate)
	 (LAP (LDA ,target (OFFSET ,immediate ,source))))
	((top-16-of-32-bits-only? immediate)
	 (LAP (LDAH ,target (OFFSET ,(->signed (quotient immediate #x10000) 16)
				    ,source))))
	((fits-in-32-bits-signed? immediate)
	 (LAP (LDA ,target (OFFSET ,(adjusted:low immediate) ,source))
	      (LDAH ,target (OFFSET ,(adjusted:high immediate) ,target))))
	(else (with-values (lambda () (immediate->register immediate))
		(lambda (prefix alias)
		  (LAP ,@prefix
		       (ADDQ ,source ,alias ,target)))))))

;;;; Comparisons

(define (compare-immediate condition immediate source)
  ; Branch if immediate <condition> source
  (let ((cc (invert-condition-noncommutative condition)))
    ;; This machine does register <op> immediate; you can
    ;; now think of cc in this way
    (cond ((zero? immediate)
	   (branch-generator! cc
	    `(BEQ ,source) `(BLT ,source) `(BGT ,source)
	    `(BNE ,source) `(BGE ,source) `(BLE ,source))
	   (LAP))
	  ((fits-in-8-bits-unsigned? immediate)
	   (let ((temp (standard-temporary!)))
	     (branch-generator! condition
	       `(BNE ,temp) `(BNE ,temp) `(BEQ ,temp)
	       `(BEQ ,temp) `(BEQ ,temp) `(BNE ,temp))
	     (case condition
	       ((= <>) (LAP (CMPEQ ,source (& ,immediate) ,temp)))
	       ((< >=) (LAP (CMPLT ,source (& ,immediate) ,temp)))
	       ((> <=) (LAP (CMPLE ,source (& ,immediate) ,temp))))))
	  (else (with-values (lambda () (immediate->register immediate))
		  (lambda (prefix alias)
		    (LAP ,@prefix
			 ,@(compare condition alias source))))))))

(define (compare condition r1 r2)
  ; Branch if r1 <cc> r2
  (if (= r1 r2)
      (let ((branch
	     (lambda (label) (LAP (BR ,regnum:came-from (@PCR ,label)))))
	    (dont-branch
	     (lambda (label) label (LAP))))
	(if (memq condition '(< > <>))
	    (set-current-branches! dont-branch branch)
	    (set-current-branches! branch dont-branch))
	(LAP))
      (let ((temp (standard-temporary!)))
	(branch-generator! condition
	  `(BNE ,temp) `(BNE ,temp) `(BNE ,temp)
	  `(BEQ ,temp) `(BEQ ,temp) `(BEQ ,temp))
	(case condition
	  ((= <>) (LAP (CMPEQ ,r1 ,r2 ,temp)))
	  ((< >=) (LAP (CMPLT ,r1 ,r2 ,temp)))
	  ((> <=) (LAP (CMPLT ,r2 ,r1 ,temp)))))))

(define (branch-generator! cc = < > <> >= <=)
  (let ((forward
	 (case cc
	   ((=)   =) ((<)  <)  ((>)  >)
	   ((<>) <>) ((>=) >=) ((<=) <=)))
	(inverse
	 (case cc
	   ((=)  <>) ((<)  >=) ((>)  <=)
	   ((<>) =)  ((>=) <)  ((<=) >))))
    (set-current-branches!
     (lambda (label)
       (LAP (,@forward (@PCR ,label))))
     (lambda (label)
       (LAP (,@inverse (@PCR ,label)))))))

(define (invert-condition condition)
  (let ((place (assq condition condition-inversion-table)))
    (if (not place)
	(error "unknown condition" condition))
    (cadr place)))

(define (invert-condition-noncommutative condition)
  (let ((place (assq condition condition-inversion-table)))
    (if (not place)
	(error "unknown condition" condition))
    (caddr place)))

(define condition-inversion-table
  ; A OP B  NOT (A OP B)      B OP A
  ;           invert      invert non-comm.
  '((=		<>		=)
    (<		>=		>)
    (>		<=		<)
    (<>		=		<>)
    (<=		>		>=)
    (>=		<		<=)))

;;;; Miscellaneous

(define-integrable (object->type source target)
  ; Type extraction
  (LAP (EXTBL ,source (& 7) ,target)))

(define-integrable (object->datum source target)
  ; Zero out the type field
  (LAP (ZAP ,source (& 128) ,target)))

(define-integrable (object->address source target)
  (object->datum source target))

(define (standard-unary-conversion source target conversion)
  ;; `source' is any register, `target' a pseudo register.
  (let ((source (standard-source! source)))
    (conversion source (standard-target! target))))

(define (standard-binary-conversion source1 source2 target conversion)
  (let ((source1 (standard-source! source1))
	(source2 (standard-source! source2)))
    (conversion source1 source2 (standard-target! target))))

(define (standard-source! register)
  (load-alias-register! register (register-type register)))

(define (standard-target! register)
  (delete-dead-registers!)
  (allocate-alias-register! register (register-type register)))

(define-integrable (standard-temporary!)
  (allocate-temporary-register! 'GENERAL))

(define (new-temporary! . avoid)
  (let loop ()
    (let ((result (allocate-temporary-register! 'GENERAL)))
      (if (memq result avoid)
	  (loop)
	  result))))

(define (standard-move-to-target! source target)
  (move-to-alias-register! source (register-type source) target))

(define (standard-move-to-temporary! source)
  (move-to-temporary-register! source (register-type source)))

(Define (register-expression expression)
  (case (rtl:expression-type expression)
    ((REGISTER)
     (rtl:register-number expression))
    ((CONSTANT)
     (let ((object (rtl:constant-value expression)))
       (and (zero? (object-type object))
	    (zero? (object-datum object))
	    regnum:zero)))
    ((CONS-NON-POINTER)
     (and (let ((type (rtl:cons-non-pointer-type expression)))
	    (and (rtl:machine-constant? type)
		 (zero? (rtl:machine-constant-value type))))
	  (let ((datum (rtl:cons-non-pointer-datum expression)))
	    (and (rtl:machine-constant? datum)
		 (zero? (rtl:machine-constant-value datum))))
	  regnum:zero))
    ((MACHINE-CONSTANT)
     (and (zero? (rtl:machine-constant-value expression))
	  regnum:zero))
    (else false)))

(define (define-arithmetic-method operator methods method)
  (let ((entry (assq operator (cdr methods))))
    (if entry
	(set-cdr! entry method)
	(set-cdr! methods (cons (cons operator method) (cdr methods)))))
  operator)

(define (lookup-arithmetic-method operator methods)
  (cdr (or (assq operator (cdr methods))
	   (error "Unknown operator" operator))))

(define-integrable (ea/mode ea) (car ea))
(define-integrable (register-ea/register ea) (cadr ea))
(define-integrable (offset-ea/offset ea) (cadr ea))
(define-integrable (offset-ea/register ea) (caddr ea))

(define (pseudo-register-displacement register)
  ;; Register block consists of 16 8-byte registers followed by 256
  ;; 8-byte temporaries.
  (+ (* 8 16)				; 16 machine independent, microcode
     (* 8 8)				;  8 Alpha, compiled code interface
     (* 8 (register-renumber register))))

(define-integrable (float-register->fpr register)
  ;; Float registers are represented by 32 through 63 in the RTL,
  ;; corresponding to floating point registers 0 through 31 in the machine.
  (- register 32))

(define-integrable (fpr->float-register register)
  (+ register 32))

(define-integrable reg:memtop
  (INST-EA (OFFSET #x0000 ,regnum:regs-pointer)))

(define-integrable reg:environment
  (INST-EA (OFFSET #x0018 ,regnum:regs-pointer)))

(define-integrable reg:lexpr-primitive-arity
  (INST-EA (OFFSET #x0038 ,regnum:regs-pointer)))

(define-integrable reg:closure-limit
  (INST-EA (OFFSET #x0050 ,regnum:regs-pointer)))

(define-integrable reg:stack-guard
  (INST-EA (OFFSET #x0058 ,regnum:regs-pointer)))

(define-integrable reg:divq
  (INST-EA (OFFSET #x00A0 ,regnum:regs-pointer)))

(define-integrable reg:remq
  (INST-EA (OFFSET #x00A8 ,regnum:regs-pointer)))

(define (lap:make-label-statement label)
  (LAP (LABEL ,label)))

(define (lap:make-unconditional-branch label)
  (LAP (BR ,regnum:came-from (@PCR ,label))))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP (ENTRY-POINT ,label)
       ,@(make-external-label expression-code-word label)))

;;;; Codes and Hooks

(let-syntax ((define-codes
	       (sc-macro-transformer
		(lambda (form environment)
		  environment
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (index (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'CODE:COMPILER-
						      (car names))
				      ,index)
				   (loop (cdr names) (+ index 1)))
			     '())))))))
  (define-codes #x012
    primitive-apply primitive-lexpr-apply
    apply error lexpr-apply link
    interrupt-closure interrupt-dlink interrupt-procedure 
    interrupt-continuation interrupt-ic-procedure
    assignment-trap cache-reference-apply
    reference-trap safe-reference-trap unassigned?-trap
    -1+ &/ &= &> 1+ &< &- &* negative? &+ positive? zero?
    access lookup safe-lookup unassigned? unbound?
    set! define lookup-apply))

(let-syntax ((define-codes
	       (sc-macro-transformer
		(lambda (start . names)
		  `(BEGIN
		     ,@(let loop ((names (cddr form)) (offset (cadr form)))
			 (if (pair? names)
			     (cons `(DEFINE-INTEGRABLE
				      ,(symbol-append 'ASSEMBLY-HOOK:
						      (car names))
				      ,offset)
				   (loop (cdr names) (+ offset 16)))
			     '())))))))
  (define-codes #x0
    long-jump
    allocate-closure))

(define (invoke-assembly-hook which-hook)
  (LAP (LDA ,regnum:assembler-temp (OFFSET ,which-hook ,regnum:closure-hook))
       (JSR ,regnum:assembler-temp ,regnum:assembler-temp (@PCO ,which-hook))))

(define-integrable (link-to-interface code)
  ;; Jump, with link in regnum:first-arg, to link_to_interface
  (LAP (MOVEI ,regnum:interface-index (& ,code))
       (JMP ,regnum:first-arg ,regnum:scheme-to-interface-jsr)))

(define-integrable (invoke-interface code)
  ;; Jump to scheme-to-interface
  (LAP (MOVEI ,regnum:interface-index (& ,code))
       (JMP ,regnum:linkage ,regnum:scheme-to-interface)))

(define (load-interface-args! first second third fourth)
  (let ((clear-regs
	 (apply clear-registers!
		(append (if first (list regnum:first-arg) '())
			(if second (list regnum:second-arg) '())
			(if third (list regnum:third-arg) '())
			(if fourth (list regnum:fourth-arg) '()))))
	(load-reg
	 (lambda (reg arg)
	   (if reg (load-machine-register! reg arg) (LAP)))))
    (let ((load-regs
	   (LAP ,@(load-reg first regnum:first-arg)
		,@(load-reg second regnum:second-arg)
		,@(load-reg third regnum:third-arg)
		,@(load-reg fourth regnum:fourth-arg))))
      (LAP ,@clear-regs
	   ,@load-regs
	   ,@(clear-map!)))))

(define (pre-lapgen-analysis rgraphs)
  rgraphs
  unspecific)