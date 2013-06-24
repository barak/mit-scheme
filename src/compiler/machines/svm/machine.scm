#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Machine Model for SVM
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? #t)
(define-integrable endianness 'LITTLE)
(define-integrable addressing-granularity 8)
(define-integrable scheme-type-width 6)
(define-integrable scheme-type-limit #x40)
(define-integrable scheme-object-width 32) ;could be 64 too

(define-integrable scheme-datum-width
  ;; See "***" below.
  (- scheme-object-width scheme-type-width))

(define-integrable float-width 64)
(define-integrable float-alignment scheme-object-width)

(define-integrable address-units-per-float
  (quotient float-width addressing-granularity))

(define-integrable address-units-per-object
  (quotient scheme-object-width addressing-granularity))

(define-integrable signed-fixnum/upper-limit
  ;; *** This is (expt 2 (-1+ scheme-datum-width)), manually constant-folded.
  #x02000000)

(define-integrable signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable unsigned-fixnum/upper-limit
  (* 2 signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)

;;;; Instructions

(define-syntax define-inst
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL * SYMBOL) (cdr form))
	 (let ((tag (cadr form))
	       (params (cddr form)))
	   (let ((name (symbol-append 'INST: tag)))
	     `(BEGIN
		(DEFINE-INTEGRABLE (,name ,@params)
		  (LIST (LIST ',tag ,@params)))
		(DEFINE-INTEGRABLE (,(symbol-append name '?) INST)
		  (EQ? (CAR INST) ',tag)))))
	 (ill-formed-syntax form)))))

(define-syntax define-unary-operations
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TARGET SOURCE)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-syntax define-binary-operations
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 `(BEGIN
	    ,@(let loop ((names (cdr form)))
		(if (pair? names)
		    (cons `(DEFINE-INST ,(car names) TARGET SOURCE1 SOURCE2)
			  (loop (cdr names)))
		    '())))
	 (ill-formed-syntax form)))))

(define-inst store size source address)
(define-inst load size target address)
(define-inst load-address target address)
(define-inst load-immediate target value)
(define-inst copy-block size size-type from to)

(define (load-immediate-operand? n)
  (or (and (exact-integer? n)
	   (<= #x-80000000 n) (<= n #x7FFFFFFF))
      (flo:flonum? n)))

;; TYPE and DATUM can be constants or registers; address is a register.
(define-inst load-pointer target type address)
(define-inst load-non-pointer target type datum)

(define-inst label label)
(define-inst entry-point label)

(define-inst jump address)

(define (inst:trap n . args)
  (list (cons* 'TRAP n args)))

(define (inst:conditional-jump condition source arg3 #!optional arg4)
  (list (cons* 'CONDITIONAL-JUMP
	       condition
	       source
	       arg3
	       (if (default-object? arg4) '() (list arg4)))))

(define (inst:conditional-jump? inst)
  (eq? (car inst) 'CONDITIONAL-JUMP))

;; N-ELTS is a constant or a register.
(define-inst flonum-header target n-elts)

(define-inst datum-u8 expression)
(define-inst datum-u16 expression)
(define-inst datum-u32 expression)
(define-inst datum-s8 expression)
(define-inst datum-s16 expression)
(define-inst datum-s32 expression)

(define-unary-operations
  copy negate increment decrement abs
  object-type object-datum object-address
  fixnum->integer integer->fixnum address->integer integer->address
  not
  sqrt round ceiling floor truncate
  log exp cos sin tan acos asin atan
  flonum-align flonum-length)

(define-binary-operations
  + - *
  quotient remainder
  lsh and andc or xor
  max-unsigned min-unsigned
  / atan2)

;;;; Memory addressing

(define-syntax define-ea
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL * SYMBOL) (cdr form))
	 (let ((tag (cadr form))
	       (params (cddr form)))
	   (let ((name (symbol-append 'EA: tag)))
	     `(BEGIN
		(DEFINE-INTEGRABLE (,name ,@params)
		  (INST-EA (,tag ,@(map (lambda (p) (list 'UNQUOTE p))
					params))))
		(DEFINE-INTEGRABLE (,(symbol-append name '?) EA)
		  (AND (PAIR? EA)
		       (EQ? (CAR EA) ',tag))))))
	 (ill-formed-syntax form)))))

(define-ea indirect base)
(define-ea offset base offset scale)
(define-ea indexed base offset oscale index iscale)
(define-ea pre-decrement base scale)
(define-ea pre-increment base scale)
(define-ea post-decrement base scale)
(define-ea post-increment base scale)
(define-ea pc-relative offset)

(define (memory-reference? ea)
  (or (ea:indirect? ea)
      (ea:offset? ea)
      (ea:indexed? ea)
      (ea:pre-decrement? ea)
      (ea:pre-increment? ea)
      (ea:post-decrement? ea)
      (ea:post-increment? ea)
      (ea:pc-relative? ea)))

(define (ea:address label)
  (ea:pc-relative `(- ,label *PC*)))

(define (ea:uuo-entry-address label)
  ;; LABEL is the uuo-link-label, but the PC to jump to is AFTER the u16
  ;; frame-size.
  (ea:pc-relative `(- (+ ,label 2) *PC*)))

(define (ea:stack-pop)
  (ea:post-increment rref:stack-pointer 'WORD))

(define (ea:stack-push)
  (ea:pre-decrement rref:stack-pointer 'WORD))

(define (ea:stack-ref index)
  (ea:offset rref:stack-pointer index 'WORD))

(define (ea:alloc-word)
  (ea:post-increment rref:free-pointer 'WORD))

(define (ea:alloc-byte)
  (ea:post-increment rref:free-pointer 'BYTE))

(define (ea:alloc-float)
  (ea:post-increment rref:free-pointer 'FLOAT))

(define (ea:environment)
  (ea:offset rref:interpreter-register-block
	     register-block/environment-offset 'WORD))

(define (ea:lexpr-actuals)
  (ea:offset rref:interpreter-register-block
	     register-block/lexpr-actuals-offset 'WORD))

;;;; Traps

(define-syntax define-traps
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
	,@(map (lambda (name)
		 (let ((code (if (pair? name) (cadr name) name))
		       (prim (if (pair? name) (car name) name)))
		   `(DEFINE (,(symbol-append 'TRAP: prim) . ARGS)
		      (APPLY INST:TRAP ',code ARGS))))
	       (cdr form))))))

(define-traps
  ;; This group doesn't return; don't push return address.
  apply lexpr-apply cache-reference-apply
  primitive-apply primitive-lexpr-apply
  error primitive-error
  (&+ add) (&- subtract) (&* multiply) (&/ divide) (1+ increment)
  (-1+ decrement) quotient remainder modulo
  (&= equal?) (&< less?) (&> greater?) zero? positive? negative?

  ;; This group returns; push return address.
  link assignment
  ;; set! define unbound? access
  lookup safe-lookup unassigned?)

(define-syntax define-interrupt-tests
  (sc-macro-transformer
   (lambda (form environment)
     environment
     `(BEGIN
       ,@(map (lambda (name)
		`(DEFINE-INST ,(symbol-append 'INTERRUPT-TEST- name)))
	      (cdr form))))))

(define-interrupt-tests dynamic-link procedure continuation ic-procedure)

;;;; Machine registers, register references.

(define-integrable number-of-machine-registers 512)
(define-integrable number-of-temporary-registers 512)

(define register-reference
  (let ((references (make-vector number-of-machine-registers)))
    (do ((i 0 (+ i 1)))
	((>= i number-of-machine-registers))
      (vector-set! references i `(R ,i)))
    (lambda (register)
      (guarantee-limited-index-fixnum register
				      number-of-machine-registers
				      'REGISTER-REFERENCE)
      (vector-ref references register))))

(define (register-reference? object)
  (and (pair? object)
       (eq? (car object) 'R)
       (pair? (cdr object))
       (index-fixnum? (cadr object))
       (fix:< (cadr object) number-of-machine-registers)
       (null? (cddr object))))

(define-guarantee register-reference "register reference")

(define (reference->register reference)
  (guarantee-register-reference reference 'REFERENCE->REGISTER)
  (cadr reference))

(define-syntax define-fixed-registers
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(* SYMBOL) (cdr form))
	 (let ((alist
		(let loop ((names (cdr form)) (index 0))
		  (if (pair? names)
		      (cons (cons (car names) index)
			    (loop (cdr names) (+ index 1)))
		      '()))))
	   `(BEGIN
	      ,@(map (lambda (p)
		       `(DEFINE-INTEGRABLE ,(symbol-append 'REGNUM: (car p))
			  ,(cdr p)))
		     alist)
	      ,@(map (lambda (p)
		       `(DEFINE-INTEGRABLE ,(symbol-append 'RREF: (car p))
			  (REGISTER-REFERENCE ,(cdr p))))
		     alist)
	      (DEFINE FIXED-REGISTERS ',alist)))
	 (ill-formed-syntax form)))))

(define-fixed-registers
  interpreter-register-block
  stack-pointer
  free-pointer
  value
  dynamic-link)

(define-integrable regnum:float-0 256)

(define-integrable regnum:word-0 (1+ regnum:dynamic-link))

(define-integrable rref:word-0 (register-reference regnum:word-0))
(define-integrable rref:word-1 (register-reference (+ 1 regnum:word-0)))
(define-integrable rref:word-2 (register-reference (+ 2 regnum:word-0)))
(define-integrable rref:word-3 (register-reference (+ 3 regnum:word-0)))
(define-integrable rref:word-4 (register-reference (+ 4 regnum:word-0)))
(define-integrable rref:word-5 (register-reference (+ 5 regnum:word-0)))
(define-integrable rref:word-6 (register-reference (+ 6 regnum:word-0)))
(define-integrable rref:word-7 (register-reference (+ 7 regnum:word-0)))

(define-integrable (machine-register-known-value register)
  register
  #f)

(define (machine-register-value-class register)
  (guarantee-limited-index-fixnum register
				  number-of-machine-registers
				  'MACHINE-REGISTER-VALUE-CLASS)
  (cond ((or (fix:= register regnum:interpreter-register-block)
	     (fix:= register regnum:stack-pointer)
	     (fix:= register regnum:dynamic-link)
	     (fix:= register regnum:free-pointer))
	 value-class=address)
	((fix:< register regnum:float-0) value-class=object)
	(else value-class=float)))

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/lexpr-actuals-offset 7)
(define-integrable register-block/stack-guard-offset 11)

;;;; RTL Generator Interface

(define (interpreter-register:access)
  (rtl:make-machine-register regnum:word-0))

(define (interpreter-register:cache-reference)
  (rtl:make-machine-register regnum:word-0))

(define (interpreter-register:cache-unassigned?)
  (rtl:make-machine-register regnum:word-0))

(define (interpreter-register:lookup)
  (rtl:make-machine-register regnum:word-0))

(define (interpreter-register:unassigned?)
  (rtl:make-machine-register regnum:word-0))

(define (interpreter-register:unbound?)
  (rtl:make-machine-register regnum:word-0))
  
(define-syntax define-machine-register
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(symbol identifier) (cdr form))
	 (let ((name (symbol-append 'INTERPRETER- (cadr form)))
	       (regnum (close-syntax (caddr form) environment)))
	   `(BEGIN
	      (DEFINE (,name)
		(RTL:MAKE-MACHINE-REGISTER ,regnum))
	      (DEFINE (,(symbol-append name '?) EXPRESSION)
		(AND (RTL:REGISTER? EXPRESSION)
		     (FIX:= (RTL:REGISTER-NUMBER EXPRESSION) ,regnum)))))
	 (ill-formed-syntax form)))))

(define-machine-register stack-pointer regnum:stack-pointer)
(define-machine-register dynamic-link regnum:dynamic-link)
(define-machine-register free-pointer regnum:free-pointer)
(define-machine-register value-register regnum:value)

(define (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:interpreter-register-block))

(define (interpreter-regs-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:interpreter-register-block)))

(define-integrable (interpreter-block-register offset-value)
  (rtl:make-offset (interpreter-regs-pointer)
		   (rtl:make-machine-constant offset-value)))

(define-integrable (interpreter-block-register? expression offset-value)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= (rtl:machine-constant-value offset)
		 offset-value)))))

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER) (interpreter-stack-pointer))
    ((FREE) (interpreter-free-pointer))
    ((DYNAMIC-LINK) (interpreter-dynamic-link))
    ((VALUE) (interpreter-value-register))
    ((INTERPRETER-CALL-RESULT:ACCESS)
     (interpreter-register:access))
    ((INTERPRETER-CALL-RESULT:CACHE-REFERENCE)
     (interpreter-register:cache-reference))
    ((INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?)
     (interpreter-register:cache-unassigned?))
    ((INTERPRETER-CALL-RESULT:LOOKUP)
     (interpreter-register:lookup))
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?)
     (interpreter-register:unassigned?))
    ((INTERPRETER-CALL-RESULT:UNBOUND?)
     (interpreter-register:unbound?))
    (else
     false)))

(define (rtl:interpreter-register->offset locative)
  (case locative
    ((MEMORY-TOP)
     register-block/memtop-offset)
    ((INT-MASK)
     register-block/int-mask-offset)
    ((STACK-GUARD)
     register-block/stack-guard-offset)
    ((ENVIRONMENT)
     register-block/environment-offset)
    (else
     (error "No such interpreter register" locative))))

(define (rtl:constant-cost expression)
  (let ((if-integer
	 (lambda (value)
	   value
	   ;; Can this be done in fewer bytes for suitably small values?
	   1))				; MOV immediate
	(get-pc-cost
	 (+ 3				; CALL
	    4))				; POP
	(based-reference-cost
	 1)				; MOV r/m
	(address-offset-cost
	 1))				; LEA instruction

    (define (if-synthesized-constant type datum)
      (if-integer (make-non-pointer-literal type datum)))

    (case (rtl:expression-type expression)
      ((CONSTANT)
       (let ((value (rtl:constant-value expression)))
	 (if (object-non-pointer? value)
	     (if-synthesized-constant (object-type value) (object-datum value))
	     (+ get-pc-cost based-reference-cost))))
      ((MACHINE-CONSTANT)
       (if-integer (rtl:machine-constant-value expression)))
      ((ENTRY:PROCEDURE ENTRY:CONTINUATION)
       (+ get-pc-cost address-offset-cost))
      ((ASSIGNMENT-CACHE VARIABLE-CACHE)
       (+ get-pc-cost based-reference-cost))
      ((OFFSET-ADDRESS BYTE-OFFSET-ADDRESS FLOAT-OFFSET-ADDRESS)
       address-offset-cost)
      ((CONS-POINTER)
       (and (rtl:machine-constant? (rtl:cons-pointer-type expression))
	    (rtl:machine-constant? (rtl:cons-pointer-datum expression))
	    (if-synthesized-constant
	     (rtl:machine-constant-value (rtl:cons-pointer-type expression))
	     (rtl:machine-constant-value
	      (rtl:cons-pointer-datum expression)))))
      (else
       #f))))

(define compiler:open-code-floating-point-arithmetic?
  #t)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM &/ FLONUM-EXPM1 FLONUM-LOG1P
		  VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS))

;;;; Closure format

;; See microcode/cmpintmd/svm1.c for a description of the layout.

(define-integrable closure-entry-size 5)
(define-integrable entry-type-size 2)

;; Offset of the first object in the closure from the address of the
;; first closure entry point, in words.

;; The first entry point for a closure with no entry points is the
;; head of the vector of value cells.

(define (closure-first-offset count entry)
  entry
  (if (= count 0)
      1
      (+ (integer-ceiling (- (* count closure-entry-size) entry-type-size)
			  address-units-per-object)
	 ;; Targets.
	 count)))

;; Offset of the first object in the closure from the address of the
;; manifest-closure header word, in words.

(define (closure-object-first-offset count)
  (if (= count 0)
      1
      (+ 1 ;; Header
	 1 ;; Count
	 (closure-first-offset count 0)	;; Entries and targets.
	 )))

;; Increment from one closure entry address to another, in bytes.

(define (closure-entry-distance count entry entry*)
  count
  (* closure-entry-size (- entry* entry)))

;; Increment from a given closure address to the first closure
;; address, in bytes.  Usually negative.

(define (closure-environment-adjustment count entry)
  (closure-entry-distance count entry 0))