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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; LAP Generation Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (REGISTER (? source)))
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? thunk parse-memory-ref))
  (receive (scale source) (thunk)
    (inst:load scale (word-target target) source)))

(define-rule statement
  (ASSIGN (? thunk parse-memory-ref)
	  (REGISTER (? target)))
  (receive (scale target) (thunk)
    (inst:store scale (word-source source) target)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (? thunk parse-memory-address))
  (let ((source (thunk)))
    (inst:load-address (word-target target) source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONSTANT (? object)))
  (load-constant (word-target target) object))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (MACHINE-CONSTANT (? n)))
  (inst:load-immediate (word-target target) n))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ENTRY:PROCEDURE (? label)))
  (inst:load-address (word-target target)
		     (ea:address (internal->external-label label))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ENTRY:CONTINUATION (? label)))
  (inst:load-address (word-target target) (ea:address label)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (VARIABLE-CACHE (? name)))
  (inst:load 'WORD
	     (word-target target)
	     (ea:address (free-reference-label name))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ASSIGNMENT-CACHE (? name)))
  (inst:load 'WORD
	     (word-target target)
	     (ea:address (free-assignment-label name))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (REGISTER (? type))
			    (REGISTER (? datum))))
  (let ((type (word-source type))
	(datum (word-source datum)))
    (inst:load-non-pointer (word-target target)
			   type
			   datum)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			    (REGISTER (? datum))))
  (let ((datum (word-source datum)))
    (inst:load-non-pointer (word-target target)
			   type
			   datum)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			    (MACHINE-CONSTANT (? datum))))
  (inst:load-non-pointer (word-target target)
			 type
			 datum))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type))
			(REGISTER (? datum))))
  (let ((type (word-source type))
	(datum (word-source datum)))
    (inst:load-pointer (word-target target)
		       type
		       datum)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(REGISTER (? datum))))
  (let ((datum (word-source datum)))
    (inst:load-pointer (word-target target)
		       type
		       datum)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->TYPE (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:object-type (word-target target)
		      source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->TYPE (CONSTANT (? object))))
  (inst:load-immediate (word-target target)
		       (object-type object)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:object-datum (word-target target)
		       source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->DATUM (CONSTANT (? object))))
  (QUALIFIER (and (object-non-pointer? object)
		  (load-immediate-operand? (object-datum object))))
  (inst:load-immediate (word-target target)
		       (object-datum object)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->ADDRESS (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:object-address (word-target target)
			 source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:object-datum (word-target target)
		       source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (CONSTANT (? char))))
  (QUALIFIER (and (char? char) (char-ascii? char)))
  (inst:load-immediate (word-target target)
		       (object-datum char)))

(define-rule predicate
  (TYPE-TEST (REGISTER (? source)) (? type))
  (let ((temp (word-temporary)))
    (simple-branches! 'EQ (word-source source) temp)
    (inst:load-immediate temp type)))

(define-rule predicate
  (EQ-TEST (REGISTER (? source1))
	   (REGISTER (? source2)))
  (simple-branches! 'EQ
		    (word-source source1)
		    (word-source source2))
  (LAP))

(define-rule predicate
  (PRED-1-ARG INDEX-FIXNUM?
	      (REGISTER (? source)))
  (simple-branches! 'IFIX (word-source source))
  (LAP))

;;;; Fixnums

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:fixnum->integer (word-target target)
			  source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->OBJECT (REGISTER (? source))))
  (let ((source (word-source source)))
    (inst:integer->fixnum (word-target target)
			  source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FIXNUM (CONSTANT (? value))))
  (QUALIFIER (and (fix:fixnum? value) (load-immediate-operand? value)))
  (inst:load-immediate (word-target target)
		       value))

;; The next two are no-ops on this architecture.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (ADDRESS->FIXNUM (REGISTER (? source))))
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM->ADDRESS (REGISTER (? source))))
  (move-to-alias-register! source (register-type target) target)
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-1-ARG (? predicate)
		     (REGISTER (? source)))
  (simple-branches! (case predicate
		      ((ZERO-FIXNUM?) 'EQ)
		      ((NEGATIVE-FIXNUM?) 'LT)
		      ((POSITIVE-FIXNUM?) 'GT)
		      (else (error "Unknown fixnum predicate:" predicate)))
		    (word-source source))
  (LAP))

(define-rule predicate
  (FIXNUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (simple-branches! (case predicate
		      ((EQUAL-FIXNUM?) 'EQ)
		      ((LESS-THAN-FIXNUM?) 'SLT)
		      ((GREATER-THAN-FIXNUM?) 'SGT)
		      ((UNSIGNED-LESS-THAN-FIXNUM?) 'LT)
		      ((UNSIGNED-GREATER-THAN-FIXNUM?) 'GT)
		      (else (error "Unknown fixnum predicate:" predicate)))
		    (word-source source1)
		    (word-source source2))
  (LAP))

(define-rule predicate
  (OVERFLOW-TEST (REGISTER (? source)))
  (simple-branches! 'NFIX source)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (let ((source (word-source source)))
    ((or (1d-table/get fixnum-1-arg-methods operation #f)
	 (error "Unknown fixnum operation:" operation))
     (word-target target)
     source
     overflow?)))

(define fixnum-1-arg-methods
  (make-1d-table))

(define (define-fixnum-1-arg-method name method)
  (1d-table/put! fixnum-1-arg-methods name method))

(let ((standard
       (lambda (name inst)
	 (define-fixnum-1-arg-method name
	   (lambda (target source overflow?)
	     overflow?
	     (inst target source))))))
  (standard 'ONE-PLUS-FIXNUM inst:increment)
  (standard 'MINUS-ONE-PLUS-FIXNUM inst:decrement)
  (standard 'FIXNUM-NEGATE inst:negate)
  (standard 'FIXNUM-NOT inst:not))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FIXNUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (let ((source1 (word-source source1))
	(source2 (word-source source2)))
    ((or (1d-table/get fixnum-2-args-methods operation #f)
	 (error "Unknown fixnum operation:" operation))
     (word-target target)
     source1
     source2
     overflow?)))

(define fixnum-2-args-methods
  (make-1d-table))

(define (define-fixnum-2-args-method name method)
  (1d-table/put! fixnum-2-args-methods name method))

(let ((standard
       (lambda (name inst)
	 (define-fixnum-2-args-method name
	   (lambda (target source1 source2 overflow?)
	     overflow?
	     (inst target source1 source2))))))
  (standard 'PLUS-FIXNUM inst:+)
  (standard 'MINUS-FIXNUM inst:-)
  (standard 'MULTIPLY-FIXNUM inst:*)
  (standard 'FIXNUM-QUOTIENT inst:quotient)
  (standard 'FIXNUM-REMAINDER inst:remainder)
  (standard 'FIXNUM-LSH inst:lsh)
  (standard 'FIXNUM-AND inst:and)
  (standard 'FIXNUM-ANDC inst:andc)
  (standard 'FIXNUM-OR inst:or)
  (standard 'FIXNUM-XOR inst:xor))

;;;; Flonums

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (float-source source))
	(temp (word-temporary)))
    (LAP ,@(inst:flonum-align rref:free-pointer rref:free-pointer)
	 ,@(inst:load-pointer (word-target target)
			      (ucode-type flonum)
			      rref:free-pointer)
	 ,@(inst:flonum-header temp 1)
	 ,@(inst:store 'WORD temp (ea:alloc-word))
	 ,@(inst:store 'FLOAT source (ea:alloc-float)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (word-source source))
	(temp (word-temporary)))
    (LAP ,@(inst:object-address temp source)
	 ,@(inst:load 'FLOAT
		      (float-target target)
		      (ea:offset temp 1 'WORD)))))

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate)
		     (REGISTER (? source)))
  (simple-branches! (case predicate
		      ((FLONUM-ZERO?) 'EQ)
		      ((FLONUM-NEGATIVE?) 'LT)
		      ((FLONUM-POSITIVE?) 'GT)
		      (else (error "Unknown flonum predicate:" predicate)))
		    (float-source source))
  (LAP))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (simple-branches! (case predicate
		      ((FLONUM-EQUAL?) 'EQ)
		      ((FLONUM-LESS?) 'LT)
		      ((FLONUM-GREATER?) 'GT)
		      (else (error "Unknown flonum predicate:" predicate)))
		    (float-source source1)
		    (float-source source2))
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation)
			(REGISTER (? source))
			(? overflow?)))
  (let ((source (word-source source)))
    ((or (1d-table/get flonum-1-arg-methods operation #f)
	 (error "Unknown flonum operation:" operation))
     (word-target target)
     source
     overflow?)))

(define flonum-1-arg-methods
  (make-1d-table))

(define (define-flonum-1-arg-method name method)
  (1d-table/put! flonum-1-arg-methods name method))

(let ((standard
       (lambda (name inst)
	 (define-flonum-1-arg-method name
	   (lambda (target source overflow?)
	     overflow?
	     (inst target target source))))))
  (standard 'FLONUM-NEGATE inst:negate)
  (standard 'FLONUM-ABS inst:abs)
  (standard 'FLONUM-SQRT inst:sqrt)
  (standard 'FLONUM-ROUND inst:round)
  (standard 'FLONUM-CEILING inst:ceiling)
  (standard 'FLONUM-FLOOR inst:floor)
  (standard 'FLONUM-TRUNCATE inst:truncate)
  (standard 'FLONUM-LOG inst:log)
  (standard 'FLONUM-EXP inst:exp)
  (standard 'FLONUM-COS inst:cos)
  (standard 'FLONUM-SIN inst:sin)
  (standard 'FLONUM-TAN inst:tan)
  (standard 'FLONUM-ACOS inst:acos)
  (standard 'FLONUM-ASIN inst:asin)
  (standard 'FLONUM-ATAN inst:atan))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  (let ((source1 (word-source source1))
	(source2 (word-source source2)))
    ((or (1d-table/get flonum-2-args-methods operation #f)
	 (error "Unknown flonum operation:" operation))
     (word-target target)
     source1
     source2
     overflow?)))

(define flonum-2-args-methods
  (make-1d-table))

(define (define-flonum-2-args-method name method)
  (1d-table/put! flonum-2-args-methods name method))

(let ((standard
       (lambda (name inst)
	 (define-flonum-2-args-method name
	   (lambda (target source1 source2 overflow?)
	     overflow?
	     (inst target source1 source2))))))
  (standard 'FLONUM-ADD inst:+)
  (standard 'FLONUM-SUBTRACT inst:-)
  (standard 'FLONUM-MULTIPLY inst:*)
  (standard 'FLONUM-DIVIDE inst:/)
  (standard 'FLONUM-ATAN2 inst:atan2))

;;;; Invocations

(define-rule statement
  (POP-RETURN)
  ;; The continuation is on the stack.
  ;; The type code needs to be cleared first.
  (current-bblock-continue!
   (let ((pop-return
	  (lambda ()
	    (let ((temp (word-temporary)))
	      (LAP ,@(inst:load 'WORD temp (ea:stack-pop))
		   ,@(inst:object-address temp temp)
		   ,@(inst:jump (ea:indirect temp)))))))
     (let ((checks (get-exit-interrupt-checks)))
       (if (null? checks)
	   (make-new-sblock (pop-return))
	   (memoize-associated-bblock 'POP-RETURN
	     (lambda ()
	       (make-new-sblock
		(let ((label (generate-label 'INTERRUPT)))
		  (LAP ,@(interrupt-check label checks)
		       ,@(pop-return)
		       ,@(inst:label label)
		       ,@(trap:interrupt-continuation)))))))))))

(define (memoize-associated-bblock name generator)
  (or (block-association name)
      (let ((bblock (generator)))
	(block-associate! name bblock)
	bblock)))

(define-rule statement
  (INVOCATION:APPLY (? frame-size) (? continuation))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:load 'WORD rref:word-0 (ea:stack-pop))
       ,@(inst:load-immediate rref:word-1 frame-size)
       ,@(trap:apply)))

(define-rule statement
  (INVOCATION:JUMP (? frame-size) (? continuation) (? label))
  frame-size continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:jump (ea:address label))))

(define-rule statement
  (INVOCATION:COMPUTED-JUMP (? frame-size) (? continuation))
  frame-size continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:load 'WORD rref:word-0 (ea:stack-pop))
       ,@(inst:object-address rref:word-0 rref:word-0)
       ,@(inst:jump (ea:indirect rref:word-0))))

(define-rule statement
  (INVOCATION:LEXPR (? number-pushed) (? continuation) (? label))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:load-address rref:word-0 (ea:address label))
       ,@(inst:load-immediate rref:word-1 number-pushed)
       ,@(trap:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:COMPUTED-LEXPR (? number-pushed) (? continuation))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:load 'WORD rref:word-0 (ea:stack-pop))
       ,@(inst:object-address rref:word-0 rref:word-0)
       ,@(inst:load-immediate rref:word-1 number-pushed)
       ,@(trap:compiler-lexpr-apply)))

(define-rule statement
  (INVOCATION:UUO-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:jump (ea:address (free-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:GLOBAL-LINK (? frame-size) (? continuation) (? name))
  continuation
  (expect-no-exit-interrupt-checks)
  (LAP ,@(clear-map!)
       ,@(inst:jump (ea:address (global-uuo-link-label name frame-size)))))

(define-rule statement
  (INVOCATION:CACHE-REFERENCE (? frame-size)
			      (? continuation)
			      (REGISTER (? extension)))
  continuation
  (expect-no-exit-interrupt-checks)
  (let ((set-extension (load-machine-register! extension regnum:word-2)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,@(inst:load-immediate rref:word-0 frame-size)
	 ,@(inst:load-address rref:word-2 (ea:address *block-label*))
	 ,@(trap:cache-reference-apply))))

(define-rule statement
  (INVOCATION:LOOKUP (? frame-size)
		     (? continuation)
		     (REGISTER (? environment))
		     (? name))
  continuation
  (expect-no-entry-interrupt-checks)
  (let ((set-environment (load-machine-register! environment regnum:word-2)))
    (LAP ,@set-environment
	 ,@(clear-map!)
	 ,@(inst:load-immediate rref:word-0 frame-size)
	 ,@(load-constant rref:word-1 name)
	 ,@(trap:lookup-apply))))

(define-rule statement
  (INVOCATION:PRIMITIVE (? frame-size) (? continuation) (? primitive))
  continuation				; ignored
  (LAP ,@(clear-map!)
       (if (eq? primitive compiled-error-procedure)
	   (LAP ,@(inst:load-immediate rref:word-0 frame-size)
		,@(trap:error))
	   (LAP ,@(load-constant rref:word-0 primitive)
		,@(let ((arity (primitive-procedure-arity primitive)))
		    (if (>= arity 0)
			(trap:primitive-apply)
			(LAP ,@(inst:load-immediate rref:word-1 frame-size)
			     ,@(if (= arity -1)
				   (trap:primitive-lexpr-apply)
				   (trap:apply)))))))))

(define-syntax define-primitive-invocation
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form)))
       `(define-rule statement
	  (INVOCATION:SPECIAL-PRIMITIVE (? frame-size)
					(? continuation)
					,(make-primitive-procedure name #t))
	  frame-size continuation
	  (expect-no-exit-interrupt-checks)
	  (,(close-syntax (symbol-append 'TRAP: name) environment)))))))

(define-primitive-invocation &+)
(define-primitive-invocation &-)
(define-primitive-invocation &*)
(define-primitive-invocation &/)
(define-primitive-invocation &=)
(define-primitive-invocation &<)
(define-primitive-invocation &>)
(define-primitive-invocation 1+)
(define-primitive-invocation -1+)
(define-primitive-invocation zero?)
(define-primitive-invocation positive?)
(define-primitive-invocation negative?)
(define-primitive-invocation quotient)
(define-primitive-invocation remainder)

;;; Invocation Prefixes

(define-rule statement
  (INVOCATION-PREFIX:MOVE-FRAME-UP (? frame-size) (REGISTER (? register)))
  (move-frame-up frame-size (word-source register)))

(define-rule statement
  (INVOCATION-PREFIX:DYNAMIC-LINK (? frame-size)
				  (REGISTER (? r1))
				  (REGISTER (? r2)))
  (if (and (= frame-size 0)
	   (= r1 regnum:stack-pointer))
      (LAP)
      (let ((temp (word-temporary)))
	(LAP ,@(inst:min-unsigned temp (word-source r1) (word-source r2))
	     ,@(move-frame-up frame-size temp)))))

(define (move-frame-up frame-size register)
  (if (= frame-size 0)
      (if (= register rref:stack-pointer)
	  (LAP)
	  (inst:copy rref:stack-pointer register))
      (let ((temp (word-temporary)))
	(LAP ,@(inst:load-address temp
				  (ea:offset register (- frame-size) 'WORD))
	     ,@(inst:copy-block frame-size 'WORD rref:stack-pointer temp)
	     ,@(inst:copy rref:stack-pointer temp)))))

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

(define (interrupt-check label checks)
  ;; This always does interrupt checks in line.
  (LAP ,@(if (or (memq 'INTERRUPT checks) (memq 'HEAP checks))
	     (LAP ,@(inst:compare 'WORD
				  rref:free-pointer
				  rref:memtop-pointer)
		  ,@(inst:conditional-jump 'UGE (ea:address label)))
	     (LAP))
       ,@(if (memq 'STACK checks)
	     (LAP ,@(inst:compare 'WORD
				  rref:stack-pointer
				  rref:stack-guard)
		  ,@(inst:conditional-jump 'ULT (ea:address label)))
	     (LAP))))

(define (simple-procedure-header label trap)
  (let ((checks (get-entry-interrupt-checks)))
    (if (null? checks)
	label
	(let ((gc-label (generate-label)))
	  (LAP (LABEL ,gc-label)
	       ,@(trap)
	       ,@label
	       ,@(interrupt-check gc-label checks))))))

(define-rule statement
  (CONTINUATION-ENTRY (? label))
  (expect-no-entry-interrupt-checks)
  (make-continuation-label label label))

(define-rule statement
  (CONTINUATION-HEADER (? label))
  (expect-no-entry-interrupt-checks)
  (make-continuation-label label label))

(define-rule statement
  (IC-PROCEDURE-HEADER (? internal-label))
  (get-entry-interrupt-checks)		; force search
  (let ((external-label (internal->external-label internal-label))
	(gc-label (generate-label)))
    (LAP (ENTRY-POINT ,external-label)
	 (EQUATE ,external-label ,internal-label)
	 (LABEL ,gc-label)
	 ,@(trap:interrupt-ic-procedure)
	 ,@(make-expression-label internal-label)
	 ,@(interrupt-check gc-label))))

(define-rule statement
  (OPEN-PROCEDURE-HEADER (? internal-label))
  (let ((rtl-proc (label->object internal-label)))
    (LAP (EQUATE ,(internal->external-label internal-label) ,internal-label)
	 ,@(simple-procedure-header
	    (make-internal-procedure-label internal-label)
	    (if (rtl-procedure/dynamic-link? rtl-proc)
		trap:interrupt-dlink
		trap:interrupt-procedure)))))

(define-rule statement
  (PROCEDURE-HEADER (? internal-label) (? min) (? max))
  (LAP (EQUATE ,(internal->external-label internal-label) ,internal-label)
       ,@(simple-procedure-header
	  (make-procedure-label min (- (abs max) min) (< max 0) internal-label)
	  trap:interrupt-procedure)))

;; Interrupt check placement
;;
;; The first two procedures are the interface.
;; GET-EXIT-INTERRUPT-CHECKS and GET-ENTRY-INTERRUPT-CHECKS get a list
;; of kinds interrupt check.  An empty list implies no check is
;; required.  The list can contain these symbols:
;;
;;    STACK      stack check required here
;;    HEAP       heap check required here
;;    INTERRUPT  check required here to avoid loops without checks.
;;
;; The traversal and decision making is done immediately prior to LAP
;; generation (from PRE-LAPGEN-ANALYSIS.)

(define (get-entry-interrupt-checks)
  (get-interrupt-checks 'ENTRY-INTERRUPT-CHECKS))

(define (get-exit-interrupt-checks)
  (get-interrupt-checks 'EXIT-INTERRUPT-CHECKS))

(define (expect-no-entry-interrupt-checks)
  (if (not (null? (get-entry-interrupt-checks)))
      (error "No entry interrupt checks expected here:" *current-bblock*)))

(define (expect-no-exit-interrupt-checks)
  (if (not (null? (get-exit-interrupt-checks)))
      (error "No exit interrupt checks expected here:" *current-bblock*)))

(define (get-interrupt-checks kind)
  (cdr (or (cfg-node-get *current-bblock* kind)
	   (error "DETERMINE-INTERRUPT-CHECKS failed:" kind))))

;; This algorithm finds leaf-procedure-like paths in the rtl control
;; flow graph.  If a procedure entry point can only reach a return, it
;; is leaf-like.  If a return can only be reached from a procedure
;; entry, it too is leaf-like.
;;
;; If a procedure reaches a procedure call, that could be a loop, so
;; it is not leaf-like.  Similarly, if a continuation entry reaches
;; return, that could be a long unwinding of recursion, so a check is
;; needed in case the unwinding does allocation.
;;
;; Typically, true leaf procedures avoid both checks, and trivial
;; cases (like MAP returning '()) avoid the exit check.
;;
;; This could be a lot smarter.  For example, a procedure entry does
;; not need to check for interrupts if it reaches call sites of
;; strictly lesser arity; or it could analyze the cycles in the CFG
;; and select good places to break them
;;
;; The algorithm has three phases: (1) explore the CFG to find all
;; entry and exit points, (2) propagate entry (exit) information so
;; that each potential interrupt check point knows what kinds of exits
;; (entrys) it reaches (is reached from), and (3) decide on the kinds
;; of interrupt check that are required at each entry and exit.
;;
;; [TOFU is just a header node for the list of interrupt checks, to
;; distingish () and #F]

(define (determine-interrupt-checks bblock)
  (let ((entries '())
	(exits '()))

    (define (explore bblock)
      (or (cfg-node-get bblock 'INTERRUPT-CHECK-EXPLORE)
	  (begin
	    (cfg-node-put! bblock 'INTERRUPT-CHECK-EXPLORE #T)
	    (if (node-previous=0? bblock)
		(set! entries (cons bblock entries))
		(if (rtl:continuation-entry?
		     (rinst-rtl (bblock-instructions bblock)))
		    ;; previous block is invocation:special-primitive
		    ;; so it is just an out of line instruction
		    (cfg-node-put! bblock 'ENTRY-INTERRUPT-CHECKS '(TOFU))))

	    (for-each-previous-node bblock explore)
	    (for-each-subsequent-node bblock explore)
	    (if (and (snode? bblock)
		     (or (not (snode-next bblock))
			 (let ((last (last-insn bblock)))
			   (or (rtl:invocation:special-primitive? last)
			       (rtl:invocation:primitive? last)))))
		(set! exits (cons bblock exits))))))

    (define (for-each-subsequent-node node procedure)
      (if (snode? node)
	  (if (snode-next node)
	      (procedure (snode-next node)))
	  (begin
	    (procedure (pnode-consequent node))
	    (procedure (pnode-alternative node)))))

    (define (propagator for-each-link)
      (lambda (node update place)
	(let propagate ((node node))
	  (let ((old (cfg-node-get node place)))
	    (let ((new (update old)))
	      (if (not (equal? old new))
		  (begin
		    (cfg-node-put! node place new)
		    (for-each-link node propagate))))))))

    (define upward   (propagator for-each-previous-node))
    (define downward (propagator for-each-subsequent-node))

    (define (setting-flag old) old #T)

    (define (propagate-entry-info bblock)
      (let ((insn (rinst-rtl (bblock-instructions bblock))))
	(cond ((or (rtl:continuation-entry? insn)
		   (rtl:continuation-header? insn))
	       (downward bblock setting-flag 'REACHED-FROM-CONTINUATION))
	      ((or (rtl:closure-header? insn)
		   (rtl:ic-procedure-header? insn)
		   (rtl:open-procedure-header? insn)
		   (rtl:procedure-header? insn))
	       (downward bblock setting-flag 'REACHED-FROM-PROCEDURE))
	      (else unspecific))))

    (define (propagate-exit-info exit-bblock)
      (let ((insn (last-insn exit-bblock)))
	(cond ((rtl:pop-return? insn)
	       (upward exit-bblock setting-flag 'REACHES-POP-RETURN))
	      (else
	       (upward exit-bblock setting-flag 'REACHES-INVOCATION)))))

    (define (decide-entry-checks bblock)
      (define (checks! types)
	(cfg-node-put! bblock 'ENTRY-INTERRUPT-CHECKS (cons 'TOFU types)))
      (define (decide-label internal-label)
	(let ((object (label->object internal-label)))
	  (let ((stack?
		 (if (and (rtl-procedure? object)
			  (not (rtl-procedure/stack-leaf? object))
			  compiler:generate-stack-checks?)
		     '(STACK)
		     '())))
	    (if (or (cfg-node-get bblock 'REACHES-INVOCATION)
		    (pair? stack?))
		(checks! (cons* 'HEAP 'INTERRUPT stack?))
		(checks! '())))))

      (let ((insn (rinst-rtl (bblock-instructions bblock))))
	(cond ((rtl:continuation-entry? insn)  (checks! '()))
	      ((rtl:continuation-header? insn) (checks! '()))
	      ((rtl:closure-header? insn)
	       (decide-label (rtl:closure-header-procedure insn)))
	      ((rtl:ic-procedure-header? insn)
	       (decide-label (rtl:ic-procedure-header-procedure insn)))
	      ((rtl:open-procedure-header? insn)
	       (decide-label (rtl:open-procedure-header-procedure insn)))
	      ((rtl:procedure-header? insn)
	       (decide-label (rtl:procedure-header-procedure insn)))
	      (else
	       (checks! '(INTERRUPT))))))

    (define (last-insn bblock)
      (rinst-rtl (rinst-last (bblock-instructions bblock))))

    (define (decide-exit-checks bblock)
      (define (checks! types)
	(cfg-node-put! bblock 'EXIT-INTERRUPT-CHECKS (cons 'TOFU types)))
      (if (rtl:pop-return? (last-insn bblock))
	  (if (cfg-node-get bblock 'REACHED-FROM-CONTINUATION)
	      (checks! '(INTERRUPT))
	      (checks! '()))
	  (checks! '())))

    (explore bblock)

    (for-each propagate-entry-info entries)
    (for-each propagate-exit-info exits)
    (for-each decide-entry-checks entries)
    (for-each decide-exit-checks exits)

    ))

;;;; Closures:

;; Since i386 instructions are pc-relative, the GC can't relocate them unless
;; there is a way to find where the closure was in old space before being
;; transported.  The first entry point (tagged as an object) is always
;; the last component of closures with any entry points.

(define (generate/cons-closure target procedure-label min max size)
  (let ((target (word-target target))
	(temp (word-temporary)))
    (LAP ,@(inst:load-address
	    temp
	    (ea:address `(- ,(internal->external-label procedure-label) 5)))
	 (MOV W (@R ,regnum:free-pointer)
	      (&U ,(make-non-pointer-literal (ucode-type manifest-closure)
					     (+ 4 size))))
	 (MOV W (@RO B ,regnum:free-pointer 4)
	      (&U ,(make-closure-code-longword min max 8)))
	 (LEA ,target (@RO B ,regnum:free-pointer 8))
	 ;; (CALL (@PCR <entry>))
	 (MOV B (@RO B ,regnum:free-pointer 8) (&U #xe8))
	 (SUB W ,temp ,target)
	 (MOV W (@RO B ,regnum:free-pointer 9) ,temp) ; displacement
	 (ADD W (R ,regnum:free-pointer) (& ,(* 4 (+ 5 size))))
	 (LEA ,temp (@RO UW
			 ,mtarget
			 ,(make-non-pointer-literal (ucode-type compiled-entry)
						    0)))
	 (MOV W (@RO B ,regnum:free-pointer -4) ,temp)
	 ,@(trap:conditionally-serialize))))

(define (generate/cons-multiclosure target nentries size entries)
  (let ((target (word-target target))
	(temp (word-temporary)))
    (with-pc
      (lambda (pc-label pc-reg)
	(define (generate-entries entries offset)
	  (let ((entry (car entries))
		(rest (cdr entries)))
	    (LAP (MOV W (@RO B ,regnum:free-pointer -9)
		      (&U ,(make-closure-code-longword (cadr entry)
						       (caddr entry)
						       offset)))
		 (MOV B (@RO B ,regnum:free-pointer -5) (&U #xe8))
		 (LEA ,temp (@RO W
				 ,pc-reg
				 (- ,(internal->external-label (car entry))
				    ,pc-label)))
		 (SUB W ,temp (R ,regnum:free-pointer))
		 (MOV W (@RO B ,regnum:free-pointer -4) ,temp)
		 ,@(if (null? rest)
		       (LAP)
		       (LAP (ADD W (R ,regnum:free-pointer) (& 10))
			    ,@(generate-entries rest (+ 10 offset)))))))

	(LAP (MOV W (@R ,regnum:free-pointer)
		  (&U ,(make-non-pointer-literal
			(ucode-type manifest-closure)
			(+ size (quotient (* 5 (1+ nentries)) 2)))))
	     (MOV W (@RO B ,regnum:free-pointer 4)
		  (&U ,(make-closure-longword nentries 0)))
	     (LEA ,target (@RO B ,regnum:free-pointer 12))
	     (ADD W (R ,regnum:free-pointer) (& 17))
	     ,@(generate-entries entries 12)
	     (ADD W (R ,regnum:free-pointer)
		  (& ,(+ (* 4 size) (if (odd? nentries) 7 5))))
	     (LEA ,temp
		  (@RO UW
		       ,mtarget
		       ,(make-non-pointer-literal (ucode-type compiled-entry)
						  0)))
	     (MOV W (@RO B ,regnum:free-pointer -4) ,temp)
	     ,@(trap:conditionally-serialize))))))

(define closure-share-names
  '#(closure-0-interrupt closure-1-interrupt closure-2-interrupt
     closure-3-interrupt closure-4-interrupt closure-5-interrupt
     closure-6-interrupt closure-7-interrupt))

(define (generate/closure-header internal-label nentries entry)
  nentries				; ignored
  (let ((external-label (internal->external-label internal-label))
	(checks (get-entry-interrupt-checks)))
    (if (zero? nentries)
	(LAP (EQUATE ,external-label ,internal-label)
	     ,@(simple-procedure-header
		(make-internal-procedure-label internal-label)
		trap:interrupt-procedure))
	(let* ((prefix
		(lambda (gc-label)
		  (LAP (LABEL ,gc-label)
		       ,@(if (zero? entry)
			     (LAP)
			     (LAP (ADD W (@R ,esp) (& ,(* 10 entry)))))
		       ,@(trap:interrupt-closure))))
	       (label+adjustment
		(lambda ()
		  (LAP ,@(make-internal-entry-label external-label)
		       (ADD W (@R ,esp)
			    (&U ,(generate/make-magic-closure-constant entry)))
		       (LABEL ,internal-label))))
	       (suffix
		(lambda (gc-label)
		  (LAP ,@(label+adjustment)
		       ,@(interrupt-check gc-label checks)))))
	  (if (null? checks)
	      (LAP ,@(label+adjustment))
	      (if (>= entry (vector-length closure-share-names))
		  (let ((gc-label (generate-label)))
		    (LAP ,@(prefix gc-label)
			 ,@(suffix gc-label)))
		  (share-instruction-sequence!
		   (vector-ref closure-share-names entry)
		   suffix
		   (lambda (gc-label)
		     (LAP ,@(prefix gc-label)
			  ,@(suffix gc-label))))))))))

(define (generate/make-magic-closure-constant entry)
  (- (make-non-pointer-literal (ucode-type compiled-entry) 0)
     (+ (* entry 10) 5)))

(define (make-closure-longword code-word pc-offset)
  (+ code-word (* #x20000 pc-offset)))

(define (make-closure-code-longword frame/min frame/max pc-offset)
  (make-closure-longword (make-procedure-code-word frame/min frame/max)
			 pc-offset))

(define-rule statement
  (CLOSURE-HEADER (? internal-label) (? nentries) (? entry))
  (generate/closure-header internal-label nentries entry))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-CLOSURE (ENTRY:PROCEDURE (? procedure-label))
			(? min) (? max) (? size)))
  (generate/cons-closure target procedure-label min max size))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-MULTICLOSURE (? nentries) (? size) (? entries)))
  (case nentries
    ((0)
     (let ((target (word-target target)))
       (LAP (MOV W ,target (R ,regnum:free-pointer))
	    (MOV W (@R ,regnum:free-pointer)
		 (&U ,(make-non-pointer-literal (ucode-type manifest-vector)
						size)))
	    (ADD W (R ,regnum:free-pointer) (& ,(* 4 (1+ size)))))))
    ((1)
     (let ((entry (vector-ref entries 0)))
       (generate/cons-closure target
			      (car entry) (cadr entry) (caddr entry)
			      size)))
    (else
     (generate/cons-multiclosure target nentries size
				 (vector->list entries)))))

;;;; Entry Header
;;; This is invoked by the top level of the LAP generator.

(define (generate/quotation-header environment-label free-ref-label n-sections)
  (let ((t1 (word-temporary))
	(t2 (word-temporary))
	(t3 (word-temporary)))
    (LAP ,@(inst:store 'WORD regnum:environment (ea:address environment-label))
	 ,@(inst:load-address t1 (ea:address *block-label*))
	 ,@(inst:load-address t2 (ea:address free-ref-label))
	 ,@(inst:load-immediate t3 n-sections)
	 ,@(trap:link t1 t2 t3)
	 ,@(make-internal-continuation-label (generate-label)))))

(define (generate/remote-link code-block-label
			      environment-offset
			      free-ref-offset
			      n-sections)
  (let ((t1 (word-temporary))
	(t2 (word-temporary))
	(t3 (word-temporary)))
    (LAP ,@(inst:load-address t1 (ea:address code-block-label))
	 ,@(inst:load-address t2 (ea:offset t1 environment-offset 'WORD))
	 ,@(inst:store 'WORD regnum:environment (ea:indirect t2))
	 ,@(inst:load-address t2 (ea:offset t1 free-ref-offset 'WORD))
	 ,@(inst:load-immediate t3 n-sections)
	 ,@(trap:link t1 t2 t3)
	 ,@(make-internal-continuation-label (generate-label)))))

(define (generate/remote-links n-blocks vector-label nsects)
  (if (> n-blocks 0)
      (let ((loop (generate-label))
	    (bytes  (generate-label))
	    (end (generate-label)))
	(LAP ,@(inst:load-immediate regnum:word-0 0)
	     ,@(inst:store 'WORD regnum:word-0 (ea:stack-push))
	     ,@(inst:label loop)
	     ;; Get index
	     ,@(inst:load 'WORD regnum:word-0 (ea:stack-ref 0))
	     ;; Get vector
	     ,@(inst:load 'WORD regnum:word-1 (ea:address vector-label))
	     ;; Get n-sections for this cc-block
	     ,@(inst:load-immediate regnum:word-2 0)
	     ,@(inst:load-address regnum:word-3 (ea:address bytes))
	     ,@(inst:load 'BYTE regnum:word-3
			  (ea:indexed regnum:word-3
				      1 'BYTE
				      regnum:word-0 'BYTE))
	     ;; address of vector
	     ,@(object-address regnum:word-1 regnum:word-1)



	     ;; Store n-sections in arg
	     (MOV W ,regnum:utility-arg-4 (R ,ebx))
	     ;; vector-ref -> cc block
	     (MOV W (R ,edx) (@ROI B ,edx 4 ,ecx 4))
	     ;; address of cc-block
	     (AND W (R ,edx) (R ,regnum:datum-mask))
	     ;; cc-block length
	     (MOV W (R ,ebx) (@R ,edx))
	     ;; Get environment
	     (MOV W (R ,ecx) ,regnum:environment)
	     ;; Eliminate length tags
	     (AND W (R ,ebx) (R ,regnum:datum-mask))
	     ;; Store environment
	     (MOV W (@RI ,edx ,ebx 4) (R ,ecx))
	     ;; Get NMV header
	     (MOV W (R ,ecx) (@RO B ,edx 4))
	     ;; Eliminate NMV tag
	     (AND W (R ,ecx) (R ,regnum:datum-mask))
	     ;; Address of first free reference
	     (LEA (R ,ebx) (@ROI B ,edx 8 ,ecx 4))
	     ;; Invoke linker
	     ,@(trap:link)
	     ,@(make-internal-continuation-label (generate-label))
	     ;; Increment counter and loop
	     (INC W (@R ,esp))
	     (CMP W (@R ,esp) (& ,n-blocks))
	     (JL (@PCR ,loop))
	     (JMP (@PCR ,end))
	     (LABEL ,bytes)
	     ,@(let walk ((bytes (vector->list nsects)))
		 (if (null? bytes)
		     (LAP)
		     (LAP (BYTE U ,(car bytes))
			  ,@(walk (cdr bytes)))))
	     (LABEL ,end)
	     ;; Pop counter
	     (POP (R ,eax))))
      (LAP)))

(define (generate/constants-block constants references assignments
				  uuo-links global-links static-vars)
  (receive (labels code)
      (???3 linkage-type:operator (???4 uuo-links)
	    linkage-type:reference references
	    linkage-type:assignment assignments
	    linkage-type:global-operator (???4 global-links))
    (let ((environment-label (allocate-constant-label)))
      (values (LAP ,@code
		   ,@(???2 (map (lambda (pair)
				  (cons #f (cdr pair)))
				static-vars))
		   ,@(???2 constants)
		   ;; Placeholder for the debugging info filename
		   (SCHEME-OBJECT ,(allocate-constant-label) DEBUGGING-INFO)
		   ;; Placeholder for the load time environment if needed
		   (SCHEME-OBJECT ,environment-label
				  ,(if (pair? labels)
				       'ENVIRONMENT
				       0)))
	      environment-label
	      (if (pair? labels) (car labels) #f)
	      (length labels)))))

(define (???3 . groups)
  (let loop ((groups groups))
    (if (pair? groups)
	(let ((linkage-type (car groups))
	      (entries (cadr groups)))
	  (if (pair? entries)
	      (receive (labels code) (loop (cddr groups))
		(receive (label code*) (???1 linkage-type entries)
		  (values (cons label labels)
			  (LAP ,@code* ,@code))))
	      (loop (cddr groups))))
	(values '() (LAP)))))

(define (???1 linkage-type entries)
  (if (pair? entries)
      (let ((label (allocate-constant-label)))
	(values label
		(LAP (SCHEME-OBJECT
		      ,label
		      ,(make-linkage-type-marker linkage-type
						 (length entries)))
		     ,@(???2 entries))))
      (values #f (LAP))))

(define (???2 entries)
  (let loop ((entries entries))
    (if (pair? entries)
	(LAP (SCHEME-OBJECT ,(cdar entries) ,(caar entries))
	     ,@(loop (cdr entries)))
	(LAP))))

(define (???4 links)
  (append-map (lambda (entry)
		(append-map (let ((name (car entry)))
			      (lambda (p)
				(list p
				      (cons name (allocate-constant-label)))))
			    (cdr entry)))
	      links))

(define (make-linkage-type-marker linkage-type n-entries)
  (let ((type-offset #x10000))
    (if (not (< n-entries type-offset))
	(error "Linkage section too large:" n-entries))
    (+ (* linkage-type type-offset) n-entries)))

;;;; Variable cache trap handling.

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont) (? extension) (? safe?))
  (QUALIFIER (interpreter-call-argument? extension))
  cont					; ignored
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension edx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,@(if safe?
	       (trap:safe-reference-trap)
	       (trap:reference-trap)))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont) (? extension) (? value))
  (QUALIFIER (and (interpreter-call-argument? extension)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (let* ((set-extension
	  (interpreter-call-argument->machine-register! extension edx))
	 (set-value (interpreter-call-argument->machine-register! value ebx)))
    (LAP ,@set-extension
	 ,@set-value
	 ,@(clear-map!)
	 ,@(trap:assignment-trap))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  cont					; ignored
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension edx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,@(trap:unassigned?-trap))))

;;;; Interpreter Calls

;;; All the code that follows is obsolete.  It hasn't been used in a while.
;;; It is provided in case the relevant switches are turned off, but there
;;; is no real reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call trap:access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont) (? environment) (? name) (? safe?))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call (if safe? trap:safe-lookup trap:lookup) environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call trap:unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call trap:unbound? environment name))

(define (lookup-call trap environment name)
  (let ((set-environment
	  (interpreter-call-argument->machine-register! environment edx)))
    (LAP ,@set-environment
	 ,@(clear-map (clear-map!))
	 ,@(load-constant regnum:word-1 name)
	 ,@(trap))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (assignment-call trap:define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (assignment-call trap:set! environment name value))

(define (assignment-call trap environment name value)
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment edx))
	 (set-value (interpreter-call-argument->machine-register! value eax)))
    (LAP ,@set-environment
	 ,@set-value
	 ,@(clear-map!)
	 (MOV W ,regnum:utility-arg-4 (R ,eax))
	 ,@(load-constant (INST-EA (R ,ebx)) name)
	 ,@(trap))))

;;;; Synthesized Data

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER (rtl:machine-constant? type))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER
   (and (rtl:object->type? type)
	(rtl:constant? (rtl:object->type-expression type))))
  (rtl:make-cons-pointer
   (rtl:make-machine-constant
    (object-type (rtl:constant-value (rtl:object->type-expression datum))))
   datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER (rtl:machine-constant? datum))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER
   (and (rtl:object->datum? datum)
	(rtl:constant-non-pointer? (rtl:object->datum-expression datum))))
  (rtl:make-cons-pointer
   type
   (rtl:make-machine-constant
    (object-datum (rtl:constant-value (rtl:object->datum-expression datum))))))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant (object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant (object-datum (rtl:constant-value source))))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (object-non-pointer? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

(define-rule rewriting
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target comparand))

(define-rule rewriting
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source register-known-value)))
  (QUALIFIER
   (and (rtl:byte-offset-address? source)
	(rtl:machine-constant? (rtl:byte-offset-address-offset source))
	(let ((base (let ((base (rtl:byte-offset-address-base source)))
		      (if (rtl:register? base)
			  (register-known-value (rtl:register-number base))
			  base))))
	  (and base
	       (rtl:offset? base)
	       (let ((base* (rtl:offset-base base))
		     (offset* (rtl:offset-offset base)))
		 (and (rtl:machine-constant? offset*)
		      (= (rtl:register-number base*) address)
		      (= (rtl:machine-constant-value offset*) offset)))))))
  (let ((target (let ((base (rtl:byte-offset-address-base source)))
		  (if (rtl:register? base)
		      (register-known-value (rtl:register-number base))
		      base))))
    (list 'ASSIGN
	  target
	  (rtl:make-byte-offset-address
	   target
	   (rtl:byte-offset-address-offset source)))))

(define-rule rewriting
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define-rule rewriting
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define (rtl:immediate-zero-constant? expression)
  (cond ((rtl:constant? expression)
	 (let ((value (rtl:constant-value expression)))
	   (and (object-non-pointer? value)
		(zero? (object-type value))
		(zero? (object-datum value)))))
	((rtl:cons-pointer? expression)
	 (and (let ((expression (rtl:cons-pointer-type expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))
	      (let ((expression (rtl:cons-pointer-datum expression)))
		(and (rtl:machine-constant? expression)
		     (zero? (rtl:machine-constant-value expression))))))
	(else #f)))

;;;; Fixnums

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-fixnum? source))
  (rtl:make-object->fixnum source))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER (rtl:constant-fixnum-test operand-1 (lambda (n) n #t)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (rtl:constant-fixnum-test operand-2 (lambda (n) n #t))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(PLUS-FIXNUM MINUS-FIXNUM))
	(rtl:register? operand-1)
	(rtl:constant-fixnum-test operand-2 zero?)))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operator '(FIXNUM-QUOTIENT FIXNUM-REMAINDER))
	(rtl:register? operand-1)
	(rtl:constant-fixnum-test operand-2
	  (lambda (n)
	    (integer-power-of-2? (abs n))))))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS FIXNUM-LSH
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 #F)
  (QUALIFIER (and (rtl:register? operand-1)
		  (rtl:constant-fixnum-test operand-2 (lambda (n) n #t))))
  (rtl:make-fixnum-2-args 'FIXNUM-LSH operand-1 operand-2 #F))

(define (rtl:constant-fixnum? expression)
  (and (rtl:constant? expression)
       (fix:fixnum? (rtl:constant-value expression))
       (rtl:constant-value expression)))

(define (rtl:constant-fixnum-test expression predicate)
  (and (rtl:object->fixnum? expression)
       (let ((expression (rtl:object->fixnum-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (fix:fixnum? n)
		     (predicate n)))))))

(define-rule rewriting
  (OBJECT->FLOAT (REGISTER (? operand register-known-value)))
  (QUALIFIER
   (rtl:constant-flonum-test operand (lambda (v) v #T)))
  (rtl:make-object->float operand))

(define-rule rewriting
  (FLONUM-2-ARGS FLONUM-SUBTRACT
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER (rtl:constant-flonum-test operand-1 flo:zero?))
  (rtl:make-flonum-2-args 'FLONUM-SUBTRACT operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-2-ARGS (? operation)
		 (REGISTER (? operand-1 register-known-value))
		 (? operand-2)
		 (? overflow?))
  (QUALIFIER
   (and (memq operation
	      '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE))
	(rtl:constant-flonum-test operand-1 flo:one?)))
  (rtl:make-flonum-2-args operation operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-2-ARGS (? operation)
		 (? operand-1)
		 (REGISTER (? operand-2 register-known-value))
		 (? overflow?))
  (QUALIFIER
   (and (memq operation
	      '(FLONUM-ADD FLONUM-SUBTRACT FLONUM-MULTIPLY FLONUM-DIVIDE))
	(rtl:constant-flonum-test operand-2 flo:one?)))
  (rtl:make-flonum-2-args operation operand-1 operand-2 overflow?))

(define-rule rewriting
  (FLONUM-PRED-2-ARGS (? predicate)
		      (? operand-1)
		      (REGISTER (? operand-2 register-known-value)))
  (QUALIFIER (rtl:constant-flonum-test operand-2 flo:zero?))
  (list 'FLONUM-PRED-2-ARGS predicate operand-1 operand-2))

(define-rule rewriting
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? operand-1 register-known-value))
		      (? operand-2))
  (QUALIFIER (rtl:constant-flonum-test operand-1 flo:zero?))
  (list 'FLONUM-PRED-2-ARGS predicate operand-1 operand-2))

#|
;; These don't work as written.  They are not simplified and are
;; therefore passed whole to the back end, and there is no way to
;; construct the graph at this level.

;; acos (x) = atan ((sqrt (1 - x^2)) / x)

(define-rule pre-cse-rewriting
  (FLONUM-1-ARG FLONUM-ACOS (? operand) #f)
  (rtl:make-flonum-2-args
   'FLONUM-ATAN2
   (rtl:make-flonum-1-arg
    'FLONUM-SQRT
    (rtl:make-flonum-2-args
     'FLONUM-SUBTRACT
     (rtl:make-object->float (rtl:make-constant 1.))
     (rtl:make-flonum-2-args 'FLONUM-MULTIPLY operand operand #f)
     #f)
    #f)
   operand
   #f))

;; asin (x) = atan (x / (sqrt (1 - x^2)))

(define-rule pre-cse-rewriting
  (FLONUM-1-ARG FLONUM-ASIN (? operand) #f)
  (rtl:make-flonum-2-args
   'FLONUM-ATAN2
   operand
   (rtl:make-flonum-1-arg
    'FLONUM-SQRT
    (rtl:make-flonum-2-args
     'FLONUM-SUBTRACT
     (rtl:make-object->float (rtl:make-constant 1.))
     (rtl:make-flonum-2-args 'FLONUM-MULTIPLY operand operand #f)
     #f)
    #f)
   #f))

|#

(define (rtl:constant-flonum-test expression predicate)
  (and (rtl:object->float? expression)
       (let ((expression (rtl:object->float-expression expression)))
	 (and (rtl:constant? expression)
	      (let ((n (rtl:constant-value expression)))
		(and (flo:flonum? n)
		     (predicate n)))))))

(define (flo:one? value)
  (flo:= value 1.))

;;;; Indexed addressing modes

(define-rule rewriting
  (OFFSET (REGISTER (? base register-known-value))
	  (MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (if (= value 0)
      (rtl:make-offset (rtl:offset-address-base base)
		       (rtl:offset-address-offset base))
      (rtl:make-offset base (rtl:make-machine-constant value))))

(define-rule rewriting
  (BYTE-OFFSET (REGISTER (? base register-known-value))
	       (MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:byte-offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (if (= value 0)
      (rtl:make-byte-offset (rtl:byte-offset-address-base base)
			    (rtl:byte-offset-address-offset base))
      (rtl:make-byte-offset base (rtl:make-machine-constant value))))

(define-rule rewriting
  (FLOAT-OFFSET (REGISTER (? base register-known-value))
		(MACHINE-CONSTANT (? value)))
  (QUALIFIER (and (rtl:float-offset-address? base)
		  (rtl:simple-subexpressions? base)))
  (if (= value 0)
      (rtl:make-float-offset (rtl:float-offset-address-base base)
			     (rtl:float-offset-address-offset base))
      (rtl:make-float-offset base (rtl:make-machine-constant value))))

;; This is here to avoid generating things like
;;
;; (offset (offset-address (object->address (constant #(foo bar baz gack)))
;;                         (register 29))
;;         (machine-constant 1))
;;
;; since the offset-address subexpression is constant, and therefore
;; known!

(define (rtl:simple-subexpressions? expr)
  (for-all? (cdr expr)
    (lambda (sub)
      (or (rtl:machine-constant? sub)
	  (rtl:register? sub)))))