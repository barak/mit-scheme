#| -*-Scheme-*-

$Id: machin.scm,v 1.2 1993/06/10 01:06:33 jawilson Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; Machine Model for C
;;; package: (compiler)

(declare (usual-integrations))

;;;; Architecture Parameters

(define use-pre/post-increment? true)
(define endianness 'DONT-KNOW)
(define scheme-object-width "OBJECT_LENGTH")
(define scheme-type-width "TYPE_CODE_LENGTH")

(define scheme-datum-width "DATUM_LENGTH")

;;; It is currently required that both packed characters and objects
;;; be integrable numbers of address units.  Furthermore, the number
;;; of address units per object must be an integral multiple of the
;;; number of address units per character.  This will cause problems
;;; on a machine that is word addressed, in which case we will have to
;;; rethink the character addressing strategy.

(define address-units-per-object "ADDRESS_UNITS_PER_OBJECT")

(define-integrable address-units-per-packed-char 1)

;; We expect a C long to be at least 32 bits wide,
;; but not necessarily two's complement.

(define-integrable min-long-width 32)
(define-integrable max-tag-width 8)

(define-integrable guaranteed-long/upper-limit
  (expt 2 min-long-width))
(define-integrable guaranteed-long/lower-limit
  (- (-1+ guaranteed-long/upper-limit)))

(define signed-fixnum/upper-limit
  (expt 2 (- min-long-width (1+ max-tag-width))))
(define signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable (stack->memory-offset offset) offset)
(define-integrable ic-block-first-parameter-offset 2)
(define-integrable execute-cache-size 2) ; Long words per UUO link slot
(define-integrable closure-entry-size
  ;; Long words in a single closure entry:
  ;;   Format + GC offset word
  ;;   C procedure descriptor + switch tag
  ;;   pointer to code block
  3)

;; Given: the number of entry points in a closure, and a particular
;; entry point number. Return: the distance from that entry point to
;; the first variable slot in the closure (in words).

(define (closure-first-offset nentries entry)
  (if (zero? nentries)
      1					; Strange boundary case
      (- (* closure-entry-size (- nentries entry)) 1)))

;; Like the above, but from the start of the complete closure object,
;; viewed as a vector, and including the header word.

(define (closure-object-first-offset nentries)
  (case nentries
    ((0)
     ;; Vector header only
     1)
    ((1)
     ;; Manifest closure header followed by single entry point
     (+ 1 closure-entry-size))
    (else
     ;; Manifest closure header, number of entries, then entries.
     (+ 1 1 (* closure-entry-size nentries)))))

;; Bump from one entry point to another -- distance in BYTES

(define (closure-entry-distance nentries entry entry*) ; for now
  nentries				; ignored
  (let ((entry-delta (- entry* entry)))
    (if (zero? entry-delta)
	0
	(string-append "((sizeof (SCHEME_OBJECT)) * "
		       (number->string
			(* closure-entry-size entry-delta))
		       ")"))))

;; Bump to the canonical entry point.  On a RISC (which forces
;; longword alignment for entry points anyway) there is no need to
;; canonicalize.

(define (closure-environment-adjustment nentries entry)
  nentries entry			; ignored
  0)

;;;; Machine Registers

(define-integrable number-of-machine-registers 5) 		; for now
(define-integrable number-of-temporary-registers 1000000)	; enough?

;;; Fixed-use registers for Scheme compiled code.
(define-integrable regnum:regs 0)
(define-integrable regnum:stack-pointer 1)
(define-integrable regnum:free 2)
(define-integrable regnum:dynamic-link 3)
(define-integrable regnum:value 4)

;;; Fixed-use registers due to architecture or OS calling conventions.

(define machine-register-value-class
  (let ((special-registers
	 `((,regnum:stack-pointer . ,value-class=address)
	   (,regnum:regs . ,value-class=unboxed)
	   (,regnum:free . ,value-class=address)
	   (,regnum:dynamic-link . ,value-class=address)
	   (,regnum:value . ,value-class=object))))

    (lambda (register)
      (let ((lookup (assv register special-registers)))
	(cond
	 ((not (null? lookup)) (cdr lookup))
	 (else (error "illegal machine register" register)))))))

(define-integrable (machine-register-known-value register)
  register				;ignore
  false)

;;;; Interpreter Registers

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/value-offset 2)
(define-integrable register-block/environment-offset 3)
(define-integrable register-block/dynamic-link-offset 4) ; compiler temp
(define-integrable register-block/lexpr-primitive-arity-offset 7)
(define-integrable register-block/utility-arg4-offset 9) ; closure free
(define-integrable register-block/stack-guard-offset 11)

(define-integrable (interpreter-free-pointer)
  (rtl:make-machine-register regnum:free))

(define (interpreter-free-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:free)))

(define-integrable (interpreter-regs-pointer)
  (rtl:make-machine-register regnum:regs))

(define (interpreter-regs-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:regs)))

(define-integrable (interpreter-value-register)
  #|
  (rtl:make-offset (interpreter-regs-pointer)
		   register-block/value-offset)
  |#
  (rtl:make-machine-register regnum:value))

(define (interpreter-value-register? expression)
  #|
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (= (rtl:offset-number expression) register-block/value-offset))
  |#
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:value)))

(define-integrable (interpreter-stack-pointer)
  (rtl:make-machine-register regnum:stack-pointer))

(define (interpreter-stack-pointer? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:stack-pointer)))

(define-integrable (interpreter-dynamic-link)
  (rtl:make-machine-register regnum:dynamic-link))

(define (interpreter-dynamic-link? expression)
  (and (rtl:register? expression)
       (= (rtl:register-number expression) regnum:dynamic-link)))

(define-integrable (interpreter-environment-register)
  (rtl:make-offset (interpreter-regs-pointer)
		   register-block/environment-offset))

(define (interpreter-environment-register? expression)
  (and (rtl:offset? expression)
       (interpreter-regs-pointer? (rtl:offset-base expression))
       (= register-block/environment-offset (rtl:offset-number expression))))

(define-integrable (interpreter-register:access)
  (interpreter-value-register))

(define-integrable (interpreter-register:cache-reference)
  (interpreter-value-register))

(define-integrable (interpreter-register:cache-unassigned?)
  (interpreter-value-register))

(define-integrable (interpreter-register:lookup)
  (interpreter-value-register))

(define-integrable (interpreter-register:unassigned?)
  (interpreter-value-register))

(define-integrable (interpreter-register:unbound?)
  (interpreter-value-register))

;;;; RTL Registers, Constants, and Primitives

(define (rtl:machine-register? rtl-register)
  (case rtl-register
    ((STACK-POINTER)
     (interpreter-stack-pointer))
    ((DYNAMIC-LINK)
     (interpreter-dynamic-link))
    ((VALUE)
     (interpreter-value-register))
    ((FREE)
     (interpreter-free-pointer))
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

(define (rtl:interpreter-register? rtl-register)
  (case rtl-register
    ((MEMORY-TOP)
     register-block/memtop-offset)
    ((STACK-GUARD)
     register-block/stack-guard-offset)
    ((ENVIRONMENT)
     register-block/environment-offset)
    #|
    ((VALUE)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:ACCESS)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:CACHE-REFERENCE)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:CACHE-UNASSIGNED?)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:LOOKUP)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:UNASSIGNED?)
     register-block/value-offset)
    ((INTERPRETER-CALL-RESULT:UNBOUND?)
     register-block/value-offset)
    |#
    (else
     false)))

(define (rtl:interpreter-register->offset locative)
  (or (rtl:interpreter-register? locative)
      (error "Unknown register type" locative)))

(define (rtl:constant-cost expression)
  expression				; ignored
  1)

(define compiler:open-code-floating-point-arithmetic?
  true)

(define compiler:primitives-with-no-open-coding
  '(DIVIDE-FIXNUM GCD-FIXNUM  &/ FLONUM-SIN FLONUM-COS FLONUM-TAN FLONUM-ASIN FLONUM-ACOS
    FLONUM-ATAN FLONUM-EXP FLONUM-LOG FLONUM-TRUNCATE FLONUM-ROUND
    FLONUM-REMAINDER FLONUM-SQRT))

