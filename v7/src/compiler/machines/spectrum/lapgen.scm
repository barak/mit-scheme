#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/lapgen.scm,v 1.136 1987/03/19 00:55:32 cph Exp $

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

;;;; RTL Rules for Spectrum

(declare (usual-integrations))

;;;; Interface to Allocator

(define (register->register-transfer source destination)
  `(,(machine->machine-register source destination)))

(define (home->register-transfer source destination)
  `(,(pseudo->machine-register source destination)))

(define (register->home-transfer source destination)
  `(,(machine->pseudo-register source destination)))

(define-integrable (pseudo->machine-register source target)
  (memory->machine-register (pseudo-register-home source) target))

(define-integrable (machine->pseudo-register source target)
  (machine-register->memory source (pseudo-register-home target)))

(define-integrable (pseudo-register-home register)
  (index-reference regnum:regs-pointer
		   (+ #x000A (register-renumber register))))

;;;; Basic machine instructions

(define-integrable (machine->machine-register source target)
  `(OR () ,source 0 ,target))

(define-integrable (machine-register->memory source target)
  `(STW () ,source ,target))

(define-integrable (machine-register->memory-post-increment source target)
  ;; Used for heap allocation
  `(STWM () ,source ,(index-reference target 1)))

(define-integrable (machine-register->memory-pre-decrement source target)
  ;; Used for stack push
  `(STWM () ,source ,(index-reference target -1)))

(define-integrable (memory->machine-register source target)
  `(LDW () ,source ,target))

(define-integrable (memory-post-increment->machine-register source target)
  ;; Used for stack pop
  `(LDWM () ,(index-reference source 1) ,target))

(define-integrable (invoke-entry entry)
  `(BE (N) ,entry))

(define (assign&invoke-entry number target entry)
  (if (<= -8192 number 8191)
      `((BE () ,entry)
	(LDI () ,number ,target))
      `((LDIL () (LEFT ,number) ,target)
	(BE () ,entry)
	(LDO () (OFFSET (RIGHT ,number) ,target) ,target))))

(define (branch->label label)
  `(BL (N) ,(label-relative-expression label) 0))

(define-integrable (index-reference register offset)
  `(INDEX ,(* 4 offset) 0 ,(register-reference register)))

(define-integrable (offset-reference register offset)
  `(OFFSET ,(* 4 offset) ,(register-reference register)))

(define-integrable (short-offset? offset)
  (< offset 2048))

(define (load-memory source offset target)
  `(LDW () ,(index-reference source offset) ,target))

(define (store-memory source target offset)
  `(STW () ,source ,(index-reference target offset)))

(define (load-memory-increment source offset target)
  `(LDWM () ,(index-reference source offset) ,target))

(define (store-memory-increment source target offset)
  `(STWM () ,source ,(index-reference target offset)))

;;;; Instruction Sequence Generators

(define (indirect-reference! register offset)
  (index-reference
   (if (machine-register? register)
       register
       (or (register-alias register false)
	   ;; This means that someone has written an address out
	   ;; to memory, something that should never happen.
	   (error "Needed to load indirect register!" register)))
   offset))

(define (object->address source #!optional target)
  (if (unassigned? target) (set! target source))
  `((EXTRU () ,source 31 24 ,target)
    (OR () ,regnum:address-offset ,target ,target)))

(define (register->machine-register register target)
  (if (machine-register? register)
      (machine->machine-register register target)
      (let ((alias (register-alias register false)))
	(if alias
	    (machine->machine-register alias target)
	    (pseudo->machine-register register target)))))

(define (expression->machine-register! expression register)
  (let ((result
	 (case (car expression)
	   ((REGISTER)
	    `(,(register->machine-register (cadr expression) register)))
	   ((OFFSET)
	    `(,(memory->machine-register
		(indirect-reference! (cadadr expression) (caddr expression))
		register)))
	   ((CONSTANT)
	    (scheme-constant->machine-register (cadr expression) register))
	   (else (error "Bad expression type" (car expression))))))
    (delete-machine-register! register)
    result))

(package (register->memory
	  register->memory-post-increment
	  register->memory-pre-decrement)
  (define ((->memory machine-register->memory) register target)
    `(,(machine-register->memory (guarantee-machine-register! register false)
				 target)))
  (define-export register->memory
    (->memory machine-register->memory))
  (define-export register->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export register->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(package (memory->memory
	  memory->memory-post-increment
	  memory->memory-pre-decrement)
  (define ((->memory machine-register->memory) source target)
    `(,(memory->machine-register source r1)
      ,(machine-register->memory r1 target)))
  (define-export memory->memory
    (->memory machine-register->memory))
  (define-export memory->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export memory->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(package (memory-post-increment->memory
	  memory-post-increment->memory-post-increment
	  memory-post-increment->memory-pre-decrement)
  (define ((->memory machine-register->memory) source target)
    `(,(memory-post-increment->machine-register source r1)
      ,(machine-register->memory r1 target)))
  (define-export memory-post-increment->memory
    (->memory machine-register->memory))
  (define-export memory-post-increment->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export memory-post-increment->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(package (scheme-constant->memory
	  scheme-constant->memory-post-increment
	  scheme-constant->memory-pre-decrement)
  (define ((->memory machine-register->memory) constant target)
    `(,@(scheme-constant->machine-register constant r1)
      ,(machine-register->memory r1 target)))
  (define-export scheme-constant->memory
    (->memory machine-register->memory))
  (define-export scheme-constant->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export scheme-constant->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (scheme-constant->machine-register constant target)
  (if (non-pointer-object? constant)
      (non-pointer->machine-register (primitive-type constant)
				     (primitive-datum constant)
				     target)
      `(,(memory->machine-register (scheme-constant-reference constant)
				   target))))

(define-integrable (scheme-constant-reference constant)
  `(INDEX ,(label->machine-constant (constant->label constant))
	  0
	  ,regnum:code-object-base))

(define (non-pointer->machine-register type datum target)
  (if (and (zero? datum)
	   (deposit-type-constant? type))
      (if (zero? type)
	  `((OR () 0 0 ,target))
	  (with-type-deposit-parameters type
	    (lambda (const end)
	      `((ZDEPI () ,const ,end 5 ,target)))))
      (let ((number (make-non-pointer type datum)))
	(if (<= -8192 number 8191)
	    `((LDI () ,number ,target))
	    `((LDIL () (LEFT ,number) ,target)
	      (LDO () (OFFSET (RIGHT ,number) ,target) ,target))))))

(package (non-pointer->memory
	  non-pointer->memory-post-increment
	  non-pointer->memory-pre-decrement)
  (define ((->memory machine-register->memory) constant target)
    `(,@(non-pointer->machine-register constant r1)
      ,(machine-register->memory r1 target)))
  (define-export non-pointer->memory
    (->memory machine-register->memory))
  (define-export non-pointer->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export non-pointer->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (machine-constant->machine-register constant target)
  (non-pointer->machine-register (machine-constant->type constant)
				 (machine-constant->datum constant)
				 target))

(package (machine-constant->memory
	  machine-constant->memory-post-increment
	  machine-constant->memory-pre-decrement)
  (define ((->memory machine-register->memory) constant target)
    `(,@(machine-constant->machine-register constant r1)
      ,(machine-register->memory r1 target)))
  (define-export machine-constant->memory
    (->memory machine-register->memory))
  (define-export machine-constant->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export machine-constant->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (label->machine-register label target)
  (let ((constant (label->machine-constant label)))
    `((ADDIL () (LEFT ,constant) ,regnum:code-object-base)
      (LDO () (OFFSET (RIGHT ,constant) ,r1) ,target))))

(define-integrable (label->machine-constant label)
  `(- ,label ,(code-object-base)))

(package (label->memory
	  label->memory-post-increment
	  label->memory-pre-decrement)
  (define ((->memory machine-register->memory) type label target)
    (let ((temp (allocate-temporary-register! false)))
      `(,@(label->machine-register type label temp)
	,(machine-register->memory temp target))))
  (define-export label->memory
    (->memory machine-register->memory))
  (define-export label->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export label->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (typed-label->machine-register type label target)
  `(,@(label->machine-register label target)
    ,@(cons-pointer->machine-register type target target)))

(package (typed-label->memory
	  typed-label->memory-post-increment
	  typed-label->memory-pre-decrement)
  (define ((->memory machine-register->memory) type label target)
    (let ((temp (allocate-temporary-register! false)))
      `(,@(typed-label->machine-register type label temp)
	,(machine-register->memory temp target))))
  (define-export typed-label->memory
    (->memory machine-register->memory))
  (define-export typed-label->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define-export typed-label->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (cons-pointer->machine-register type source target)
  (let ((source (guarantee-machine-register! source false)))
    (if (eqv? source target)
	(let ((temp (allocate-temporary-register! false)))
	  `(,@(cons-pointer->machine-register type source temp)
	    ,(machine->machine-register temp source)))
	`(,@(if (deposit-type-constant? type)
		(with-type-deposit-parameters type
		  (lambda (type end)
		    `((ZDEPI () ,type ,end 5 ,target))))
		`((LDI () ,type ,target)
		  (ZDEP () ,target 7 8 ,target)))
	  (DEP () ,source 31 24 ,target)))))

(package (cons-pointer->memory
	  cons-pointer->memory-post-increment
	  cons-pointer->memory-pre-decrement)
  (define ((->memory machine-register->memory) type source target)
    (let ((temp (allocate-temporary-register! false)))
      `(,@(cons-pointer->machine-register type source temp)
	,(machine-register->memory temp target))))
  (define cons-pointer->memory
    (->memory machine-register->memory))
  (define cons-pointer->memory-post-increment
    (->memory machine-register->memory-post-increment))
  (define cons-pointer->memory-pre-decrement
    (->memory machine-register->memory-pre-decrement)))

(define (test:machine/machine-register condition source0 source1 receiver)
  (let ((make-branch
	 (lambda (completer)
	   (lambda (label)
	     `((COMB (,completer N) ,source0 ,source1
		     ,(label-relative-expression label)))))))
    (receiver '()
	      (make-branch condition)
	      (make-branch (invert-test-completer condition)))))

(define (test:short-machine-constant/machine-register condition constant source
						      receiver)
  (let ((make-branch
	 (lambda (completer)
	   (lambda (label)
	     `((COMIB (,completer N) ,constant ,source
		      ,(label-relative-expression label)))))))
    (receiver '()
	      (make-branch condition)
	      (make-branch (invert-test-completer condition)))))

(define (invert-test-completer completer)
  (cdr (or (assq completer
		 '((EQ . LTGT) (LTGT . EQ)
		   (LT . GTEQ) (GTEQ . LT)
		   (GT . LTEQ) (GT . LTEQ)
		   (LTLT . GTGTEQ) (GTGTEQ . LTLT)
		   (GTGT . LTLTEQ) (GTGT . LTLTEQ)
		   ))
	   (error "Unknown test completer" completer))))

(define (test:machine-constant/machine-register condition constant source
						receiver)
  (cond ((zero? constant)
	 (test:machine/machine-register condition 0 source receiver))
	((test-short-constant? constant)
	 (test:short-machine-constant/machine-register condition constant
						       source receiver))
	(else
	 `(,@(non-pointer->machine-register 0 constant r1)
	   ,@(test:machine/machine-register condition r1 source receiver)))))

(define (test:machine-constant/register condition constant source receiver)
  (test:machine-constant/machine-register
   condition constant (guarantee-machine-register! source false) receiver))

(define (test:machine-constant/memory condition constant source receiver)
  (let ((temp (allocate-temporary-register! false)))
    `(,(memory->machine-register source temp)
      ,@(test:machine-constant/machine-register condition constant temp
						receiver))))

(define (test:type/machine-register condition type source receiver)
  (let ((temp (allocate-temporary-register! false)))
    `(,(extract-type-machine->machine-register source temp)
      ,@(test:machine-constant/machine-register condition type temp
						receiver))))

(define (test:type/register condition type source receiver)
  (test:type/machine-register condition type
			      (guarantee-machine-register! source false)
			      receiver))

(define (test:type/memory condition type source receiver)
  (let ((temp (allocate-temporary-register! false)))
    `(,(memory->machine-register source temp)
      ,@(cond ((zero? type)
	       (test:machine/machine-register condition 0 temp receiver))
	      ((test-short-constant? type)
	       `(,(extract-type-machine->machine-register temp temp)
		 ,@(test:short-machine-constant/machine-register condition
								 type
								 temp
								 receiver)))
	      (else
	       `(,@(non-pointer->machine-register 0 type r1)
		 ,(extract-type-machine->machine-register temp temp)
		 ,@(test:machine/machine-register condition r1 temp
						  receiver)))))))

(define (standard-predicate-receiver prefix consequent alternative)
  (set-current-branches! consequent alternative)
  prefix)

(define ((inline-predicate-receiver label) prefix consequent alternative)
  `(,@prefix ,@(consequent label)))

(define-integrable (extract-type-machine->machine-register source target)
  `(EXTRU () ,source 7 8 ,target))

(define-integrable (test-short-constant? constant)
  (<= -16 constant 15))

(define (deposit-type-constant? n)
  ;; Assume that (<= 0 n 127).
  (or (< n 16)
      (zero? (remainder n
			(cond ((< n 32) 2)
			      ((< n 64) 4)
			      (else 8))))))

(define (with-type-deposit-parameters type receiver)
  ;; This one is for type codes, assume that (<= 0 n 127).
  ;; Also assume that `(deposit-type-constant? type)' is true.
  (cond ((< type 16) (receiver type 7))
	((< type 32) (receiver (quotient type 2) 6))
	((< type 64) (receiver (quotient type 4) 5))
	(else (receiver (quotient type 8) 4))))

(define (code-object-label-initialize code-object)
  (cond ((procedure? code-object) false)
	((continuation? code-object) (continuation-label code-object))
	((quotation? code-object) (quotation-label code-object))
	(else
	 (error "CODE-OBJECT-LABEL-INITIALIZE: Unknown code object type"
		code-object))))

(define (code-object-base)
  ;; This will fail if the difference between the beginning of the
  ;; code-object and LABEL is greater than 11 bits (signed).
  (or *code-object-label*
      (let ((label (generate-label)))
	(prefix-instructions!
	 `((BL () 0 ,regnum:code-object-base)
	   (LABEL ,label)))
	(let ((label `(+ ,label 4)))
	  (set! *code-object-label* label)
	  label))))

(define (generate-n-times n limit prefix suffix with-counter)
  (if (<= n limit)
      (let loop ((n n))
	(if (zero? n)
	    '()
	    `(,@prefix
	      ,suffix
	      ,@(loop (-1+ n)))))
      (let ((loop (generate-label 'LOOP)))
	(with-counter
	 (lambda (counter)
	   `(,@(machine-constant->machine-register (-1+ n) counter)
	     (LABEL ,loop)
	     ,@prefix
	     (ADDIBF (EQ) -1 ,counter ,(label-relative-expression loop))
	     ,suffix))))))

(define-integrable (label-relative-expression label)
  `(- (- ,label *PC*) 8))

;;;; Registers/Entries

(let-syntax ((define-entries
	       (macro names
		 (define (loop names index)
		   (if (null? names)
		       '()
		       (cons `(DEFINE ,(symbol-append 'ENTRY:COMPILER-
						      (car names))
				`(INDEX ,,index 5 ,regnum:regs-pointer))
			     (loop (cdr names) (+ index 8)))))
		 `(BEGIN ,@(loop names #x00F0)))))
  (define-entries apply error wrong-number-of-arguments interrupt-procedure
    interrupt-continuation lookup-apply lookup access unassigned? unbound?
    set! define primitive-apply enclose setup-lexpr setup-ic-procedure))

(define reg:temp `(INDEX #x0010 0 ,regnum:regs-pointer))
(define reg:compiled-memtop `(INDEX 0 0 ,regnum:regs-pointer))

(define popper:apply-closure `(INDEX 400 5 ,regnum:regs-pointer))
(define popper:apply-stack `(INDEX 528 5 ,regnum:regs-pointer))
(define popper:value `(INDEX 656 5 ,regnum:regs-pointer))

(package (type->machine-constant
	  make-non-pointer
	  machine-constant->type
	  machine-constant->datum)
  (define type-scale-factor
    (expt 2 24))
  (define-export (type->machine-constant type)
    (* type type-scale-factor))
  (define-export (make-non-pointer type datum)
    (+ (* type type-scale-factor) datum))
  (define-export (machine-constant->type constant)
    (quotient constant type-scale-factor))
  (define-export (machine-constant->datum constant)
    (remainder constant type-scale-factor)))

(define constant:compiled-expression
  (type->machine-constant (ucode-type compiled-expression)))

(define constant:return-address
  (type->machine-constant (ucode-type return-address)))

(define constant:unassigned
  (make-non-pointer (ucode-type unassigned) 0))

(define constant:false
  (make-non-pointer (ucode-type false) 0))

;;;; Transfers to Registers

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER 30) (OFFSET-ADDRESS (REGISTER 30) (? n)))
  `((LDO () ,(offset-reference regnum:stack-pointer n) ,r30)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (scheme-constant->machine-register source
				     (allocate-assignment-alias! target
								 false)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source false target)
  '())

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (object->address (move-to-alias-register! source false target)))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source false target)))
    `(,(extract-type-machine->machine-register target target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (and (pseudo-register? target) (short-offset? offset)))
  (let ((source (indirect-reference! address offset))) ;force eval order.
    `(,(memory->machine-register source
				 (allocate-assignment-alias! target false)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? source)) 1))
  (QUALIFIER (pseudo-register? target))
  (memory-post-increment->machine-register
   source
   (allocate-assignment-alias! target false)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (pseudo-register? target))
  (cons-pointer->machine-register type datum
				  (allocate-assignment-alias! target false)))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (QUALIFIER (short-offset? n))
  (scheme-constant->memory object (indirect-reference! a n)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (QUALIFIER (short-offset? n))
  (register->memory r (indirect-reference! a n)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POINTER-INCREMENT (REGISTER (? source)) 1))
  (QUALIFIER (short-offset? n))
  (memory-post-increment->memory source (indirect-reference! a n)))

(define-rule statement
  ;; The code assumes r cannot be trashed
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (QUALIFIER (short-offset? n))
  (cons-pointer->memory type r (indirect-reference! a n)))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? r-target)) (? n-target))
	  (OFFSET (REGISTER (? r-source)) (? n-source)))
  (QUALIFIER (and (short-offset? n-target) (short-offset? n-source)))
  (memory->memory (indirect-reference! r-source n-source)
		  (indirect-reference! r-target n-target)))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 25) 1) (CONSTANT (? object)))
  (scheme-constant->memory-post-increment object r25))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 25) 1) (REGISTER (? r)))
  (register->memory-post-increment r r25))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 25) 1) (OFFSET (REGISTER (? r)) (? n)))
  (memory->memory-post-increment (indirect-reference! r n) r25))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 25) 1) (ENTRY:PROCEDURE (? procedure)))
  (typed-label->memory-post-increment (ucode-type compiled-expression)
				      (procedure-external-label procedure)
				      r25))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1) (CONSTANT (? object)))
  (scheme-constant->memory-pre-decrement object r30))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1) (UNASSIGNED))
  (scheme-constant->memory-pre-decrement constant:unassigned r30))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1) (REGISTER (? r)))
  (register->memory-pre-decrement r r30))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (cons-pointer->memory-pre-decrement type r r30))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1) (OFFSET (REGISTER (? r)) (? n)))
  (QUALIFIER (short-offset? n))
  (memory->memory-pre-decrement (indirect-reference! r n) r30))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1)
	  (OFFSET-ADDRESS (REGISTER 30) (? n)))
  (QUALIFIER (short-offset? n))
  (let ((temp (allocate-temporary-register! false)))
    `((LDI () ,(ucode-type stack-environment) ,temp)
      (LDO () ,(offset-reference r30 n) ,r1)
      (DEP () ,temp 7 8 ,r1)
      ,@(register->memory-pre-decrement r1 r30))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 30) -1)
	  (ENTRY:CONTINUATION (? continuation)))
  (typed-label->memory-pre-decrement (ucode-type return-address)
				     (continuation-label continuation)
				     r30))

;;;; Predicates

(define-rule predicate
  (TRUE-TEST (REGISTER (? register)))
  (test:machine-constant/register 'LTGT constant:false register
				  standard-predicate-receiver))

(define-rule predicate
  (TRUE-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (test:machine-constant/memory 'LTGT constant:false
				(indirect-reference! register offset)
				standard-predicate-receiver))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (test:machine-constant/machine-register 'LTGT type register
					  standard-predicate-receiver))

(define-rule predicate
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) (? type))
  (test:type/register 'LTGT type register standard-predicate-receiver))

(define-rule predicate
  (UNASSIGNED-TEST (REGISTER (? register)))
  (test:machine-constant/register 'LTGT constant:unassigned register
				  standard-predicate-receiver))

(define-rule predicate
  (UNASSIGNED-TEST (OFFSET (REGISTER (? register)) (? offset)))
  (test:machine-constant/memory 'LTGT constant:unassigned
				(indirect-reference! register offset)
				standard-predicate-receiver))

;;;; Invocations

(define-rule statement
  (INVOCATION:APPLY (? number-pushed) (? prefix) (? continuation))
  `(,@(generate-invocation-prefix prefix)
    ,@(assign&invoke-entry number-pushed regnum:frame-size
			   entry:compiler-apply)))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-CLOSURE (? frame-size) (? receiver-offset))
		   (? continuation) (? procedure))
  `(,@(clear-map!)
    ,@(apply-closure-sequence frame-size receiver-offset
			      (procedure-label procedure))))

(define-rule statement
  (INVOCATION:JUMP (? n)
		   (APPLY-STACK (? frame-size) (? receiver-offset)
				(? n-levels))
		   (? continuation) (? procedure))
  `(,@(clear-map!)
    ,@(apply-stack-sequence frame-size receiver-offset n-levels
			    (procedure-label procedure))))

(define-rule statement
  (INVOCATION:JUMP (? number-pushed) (? prefix) (? continuation) (? procedure))
  (QUALIFIER (not (memq (car prefix) '(APPLY-CLOSURE APPLY-STACK))))
  `(,@(generate-invocation-prefix prefix)
    ,(branch->label (procedure-label procedure))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? prefix) (? continuation)
		    (? procedure))
  `(,@(generate-invocation-prefix prefix)
    ,@(machine-constant->machine-register number-pushed regnum:frame-size)
    ,(branch->label (procedure-label procedure))))

(define-rule statement
  (INVOCATION:LOOKUP (? number-pushed) (? prefix) (? continuation)
		     (? environment) (? name))
  (let ((set-environment
	 (expression->machine-register! environment regnum:call-argument-0)))
    (delete-dead-registers!)
    `(,@set-environment
      ,@(generate-invocation-prefix prefix)
      ,@(scheme-constant->machine-register name regnum:call-argument-1)
      ,@(assign&invoke-entry (1+ number-pushed) regnum:frame-size
			     entry:compiler-lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? number-pushed) (? prefix) (? continuation)
			(? primitive))
  `(,@(generate-invocation-prefix prefix)
    ,@(if (eq? primitive compiled-error-procedure)
	  (assign&invoke-entry number-pushed regnum:frame-size
			       entry:compiler-error)
	  ;; Simple thing for now.
	  (assign&invoke-entry (primitive-datum primitive)
			       regnum:call-argument-0
			       entry:compiler-primitive-apply))))

(define-rule statement
  (RETURN)
  `(,@(clear-map!)
    ,(memory-post-increment->machine-register regnum:stack-pointer
					      regnum:code-object-base)
    ,@(object->address regnum:code-object-base)
    (BE (N) (INDEX 0 1 ,regnum:code-object-base))))

(define (generate-invocation-prefix prefix)
  `(,@(clear-map!)
    ,@(case (car prefix)
	((NULL) '())
	((MOVE-FRAME-UP)
	 (apply generate-invocation-prefix:move-frame-up (cdr prefix)))
	((APPLY-CLOSURE)
	 (apply generate-invocation-prefix:apply-closure (cdr prefix)))
	((APPLY-STACK)
	 (apply generate-invocation-prefix:apply-stack (cdr prefix)))
	(else (error "GENERATE-INVOCATION-PREFIX: bad prefix type" prefix)))))

(define (generate-invocation-prefix:move-frame-up frame-size how-far)
  (cond ((or (zero? frame-size) (zero? how-far)) '())
	((= frame-size 1)
	 `(,(load-memory-increment regnum:stack-pointer (+ frame-size how-far)
				   r1)
	   ,(store-memory r1 regnum:stack-pointer 0)))
	((= frame-size 2)
	 (let ((temp (allocate-temporary-register! false)))
	   `(,(load-memory-increment regnum:stack-pointer 1 r1)
	     ,(load-memory-increment regnum:stack-pointer (-1+ how-far) temp)
	     ,(store-memory r1 regnum:stack-pointer 0)
	     ,(store-memory temp regnum:stack-pointer 1))))
	(else
	 (let ((temp0 (allocate-temporary-register! false))
	       (temp1 (allocate-temporary-register! false)))
	   `((LDO ()
		  ,(offset-reference regnum:stack-pointer frame-size)
		  ,temp0)
	     (LDO ()
		  ,(offset-reference regnum:stack-pointer
				     (+ frame-size how-far))
		  ,temp1)
	     ,@(generate-n-times
		frame-size 5
		`(,(load-memory-increment temp0 -1 r1))
		(store-memory-increment r1 temp1 -1)
		(lambda (generator)
		  (generator (allocate-temporary-register! false))))
	     ,(machine->machine-register temp1 regnum:stack-pointer))))))

(define (generate-invocation-prefix:apply-closure frame-size receiver-offset)
  (let ((label (generate-label)))
    `(,@(apply-closure-sequence frame-size receiver-offset label)
      (LABEL ,label))))

(define (generate-invocation-prefix:apply-stack frame-size receiver-offset
						n-levels)
  (let ((label (generate-label)))
    `(,@(apply-stack-sequence frame-size receiver-offset n-levels label)
      (LABEL ,label))))

;;;; Environment Calls

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? environment) (? name))
  (lookup-call entry:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? environment) (? name))
  (lookup-call entry:compiler-lookup environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? environment) (? name))
  (lookup-call entry:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? environment) (? name))
  (lookup-call entry:compiler-unbound? environment name))

(define (lookup-call entry environment name)
  (let ((set-environment
	 (expression->machine-register! environment regnum:call-argument-0)))
    (let ((clear-map (clear-map!)))
      `(,@set-environment
	,@clear-map
	,(scheme-constant->machine-register name regnum:argument-1)
	(BLE (N) ,entry)
	,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:ENCLOSE (? number-pushed))
  `(,@(cons-pointer->machine-register (ucode-type vector) regnum:free-pointer
				      regnum:call-value)
    ,@(non-pointer->memory-post-increment (ucode-type manifest-vector)
					  number-pushed
					  regnum:free-pointer)
    ,@(generate-n-times number-pushed 5
			`(,(load-memory-increment regnum:stack-pointer 1 r1))
			(store-memory-increment r1 regnum:free-pointer 1)
	(lambda (generator)
	  (generator (allocate-temporary-register! false))))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-set! environment name value))

(define (assignment-call:default entry environment name value)
  (let ((set-environment
	 (expression->machine-register! environment regnum:call-argument-0)))
    (let ((set-value
	   (expression->machine-register! value regnum:call-argument-2)))
      (let ((clear-map (clear-map!)))
	`(,@set-environment
	  ,@set-value
	  ,@clear-map
	  ,@(scheme-constant->machine-register name regnum:call-argument-1)
	  (BLE (N) ,entry)
	  ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name)
			   (CONS-POINTER (CONSTANT (? type))
					 (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-define environment name type
				datum))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name)
			 (CONS-POINTER (CONSTANT (? type))
				       (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-set! environment name type
				datum))

(define (assignment-call:cons-pointer entry environment name type datum)
  (let ((set-environment
	 (expression->machine-register! environment regnum:call-argument-0)))
    (let ((set-value
	   (cons-pointer->machine-register type datum regnum:call-argument-2)))
      (let ((clear-map (clear-map!)))
	`(,@set-environment
	  ,@set-value
	  ,@clear-map
	  ,@(scheme-constant->machine-register name regnum:call-argument-1)
	  (BLE (N) ,entry)
	  ,@(make-external-label (generate-label)))))))

;;;; Procedure/Continuation Entries

;;; The following calls MUST appear as the first thing at the entry
;;; point of a procedure.  They assume that the register map is clear
;;; and that no register contains anything of value.

;;; **** The only reason that this is true is that no register is live
;;; across calls.  If that were not true, then we would have to save
;;; any such registers on the stack so that they would be GC'ed
;;; appropriately.

(define-rule statement
  (PROCEDURE-HEAP-CHECK (? procedure))
  (let ((label (generate-label)))
    `(,@(procedure-header procedure)
      (COMBT (LT N) ,regnum:free-pointer ,regnum:memtop-pointer
	     ,(label-relative-expression label))
      (BLE (N) ,entry:compiler-interrupt-procedure)
      (LABEL ,label))))

(define-rule statement
  (CONTINUATION-HEAP-CHECK (? continuation))
  (let ((label (generate-label)))
    `(,@(make-external-label (continuation-label continuation))
      (COMBT (LT N) ,regnum:free-pointer ,regnum:memtop-pointer
	     ,(label-relative-expression label))
      (BLE (N) ,entry:compiler-interrupt-procedure)
      (LABEL ,label))))

(define (procedure-header procedure)
  (let ((internal-label (procedure-label procedure)))
    (append! (if (closure-procedure? procedure)
		 (let ((required (1+ (length (procedure-required procedure))))
		       (optional (length (procedure-optional procedure)))
		       (label (procedure-external-label procedure)))
		   (if (and (procedure-rest procedure)
			    (zero? required))
		       (begin (set-procedure-external-label! procedure
							     internal-label)
			      `((ENTRY-POINT ,internal-label)))
		       `((ENTRY-POINT ,label)
    			 ,@(make-external-label label)
			 ,@(cond ((procedure-rest procedure)
				  (test:machine-constant/machine-register
				   'GTEQ required regnum:frame-size
				   (inline-predicate-receiver internal-label)))
				 ((zero? optional)
				  (test:machine-constant/machine-register
				   'EQ required regnum:frame-size
				   (inline-predicate-receiver internal-label)))
				 (else
				  (let ((wna-label (generate-label)))
				    `(,@(test:machine-constant/machine-register
					 'LT required regnum:frame-size
					 (inline-predicate-receiver wna-label))
				      ,@(test:machine-constant/machine-register
					 'LTEQ (+ required optional)
					 regnum:frame-size
					 (inline-predicate-receiver
					  internal-label))
				      (LABEL ,wna-label)))))
			 ,(invoke-entry
			   entry:compiler-wrong-number-of-arguments))))
		 '())
	     `(,@(make-external-label internal-label)))))

(define *block-start-label*)

(define (make-external-label label)
  `((WORD (- ,label ,*block-start-label*))
    (LABEL ,label)))

;;;; Poppers

(define-rule statement
  (MESSAGE-RECEIVER:CLOSURE (? frame-size))
  (machine-constant->memory-pre-decrement (* frame-size 4) r30))

(define-rule statement
  (MESSAGE-RECEIVER:STACK (? frame-size))
  (machine-constant->memory-pre-decrement (+ #x00200000 (* frame-size 4))
					       r30))

(define-rule statement
  (MESSAGE-RECEIVER:SUBPROBLEM (? continuation))
  `(,@(typed-label->memory-pre-decrement (ucode-type return-address)
					 (continuation-label continuation)
					 r30)
    ,@(machine-constant->memory-pre-decrement #x00400000 r30)))

(define (apply-closure-sequence frame-size receiver-offset label)
  `(,@(machine-constant->machine-register (* frame-size 4) r19)
    (LDO () ,(offset-reference r30 (* receiver-offset 4)) ,r20)
    ,@(label->machine-register label r21)
    (BLE (N) ,popper:apply-closure)))

(define (apply-stack-sequence frame-size receiver-offset n-levels label)
  `(,@(machine-constant->machine-register (* frame-size 4) r19)
    (LDO () ,(offset-reference r30 (* receiver-offset 4)) ,r20)
    ,@(label->machine-register label r21)
    ,@(machine-constant->machine-register n-levels r22)
    (BLE (N) ,popper:apply-stack)))

(define-rule statement
  (MESSAGE-SENDER:VALUE (? receiver-offset))
  `(,@(clear-map!)
    (LDO () ,(offset-reference r30 (* receiver-offset 4)) ,r30)
    (BLE (N) ,popper:value)))