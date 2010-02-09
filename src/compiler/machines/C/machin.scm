#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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
(define-integrable address-units-per-float "ADDRESS_UNITS_PER_FLOAT")
(define-integrable address-units-per-packed-char 1)

;; We expect a C long to be at least 32 bits wide,
;; but not necessarily two's complement.
;; Tags won't be wider than 6 bits

(define-integrable min-long-width 32)
(define-integrable max-tag-width 6)

(define-integrable guaranteed-long/upper-limit
  (expt 2 (-1+ min-long-width)))
(define-integrable guaranteed-long/lower-limit
  (- (-1+ guaranteed-long/upper-limit)))

(define signed-fixnum/upper-limit
  (expt 2 (- min-long-width (1+ max-tag-width))))
(define signed-fixnum/lower-limit
  (- signed-fixnum/upper-limit))

(define-integrable unsigned-fixnum/upper-limit
  (* 2 signed-fixnum/upper-limit))

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

;; Bump from one entry point to another -- distance in addressing units.

(define (closure-entry-distance nentries entry entry*) ; for now
  nentries				; ignored
  (let ((entry-delta (- entry* entry)))
    (if (zero? entry-delta)
	0
	(c:* "CLOSURE_ENTRY_DELTA" (* closure-entry-size entry-delta)))))

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

(define (machine-register-value-class register)
  (cond ((or (= register regnum:regs)
	     (= register regnum:stack-pointer)
	     (= register regnum:free)
	     (= register regnum:dynamic-link))
	 value-class=address)
	((= register regnum:value)
	 value-class=object)
	(else
	 (error "illegal machine register" register))))

(define-integrable (machine-register-known-value register)
  register				;ignore
  false)

;;;; Interpreter Registers

(define-integrable register-block/memtop-offset 0)
(define-integrable register-block/int-mask-offset 1)
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
       (let ((offset (rtl:offset-offset expression)))
	 (and (rtl:machine-constant? offset)
	      (= 3 (rtl:machine-constant-value offset))))))

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
    ((INT-MASK)
     register-block/int-mask-offset)
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
  '(DIVIDE-FIXNUM GCD-FIXNUM &/
    VECTOR-CONS STRING-ALLOCATE FLOATING-VECTOR-CONS))