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

;;;; 68000 Disassembler: Top Level
;;; package: (compiler disassembler)

(declare (usual-integrations))

(define (disassembler/read-variable-cache block index)
  (let-syntax ((ucode-type
		(sc-macro-transformer
		 (lambda (form environment)
		   environment
		   (apply microcode-type (cdr form)))))
	       (ucode-primitive
		(sc-macro-transformer
		 (lambda (form environment)
		   environment
		   (apply make-primitive-procedure (cdr form))))))
    ((ucode-primitive primitive-object-set-type 2)
     (ucode-type quad)
     (system-vector-ref block index))))

(define (disassembler/read-procedure-cache block index)
  (fluid-let ((*block block))
    (let* ((offset (compiled-code-block/index->offset index)))
      (let ((opcode (read-unsigned-integer offset 16))
	    (arity (read-unsigned-integer (+ offset 6) 16)))
	(case opcode
	  ((#x4ef9)			; JMP <value>.L
	   ;; This should learn how to decode the new trampolines.
	   (vector 'COMPILED
		   (read-procedure (+ offset 2))
		   arity))
	  (else
	   (error "disassembler/read-procedure-cache: Unknown opcode"
		  opcode block index)))))))

(define (disassembler/instructions block start-offset end-offset symbol-table)
  (let loop ((offset start-offset) (state (disassembler/initial-state)))
    (if (and end-offset (< offset end-offset))
	(disassemble-one-instruction
	 block offset symbol-table state
	 (lambda (offset* instruction state)
	   (make-instruction offset
			     instruction
			     (lambda () (loop offset* state)))))
	'())))

(define (disassembler/instructions/null? obj)
  (null? obj))

(define (disassembler/instructions/read instruction-stream receiver)
  (receiver (instruction-offset instruction-stream)
	    (instruction-instruction instruction-stream)
	    (instruction-next instruction-stream)))

(define-structure (instruction (type vector))
  (offset false read-only true)
  (instruction false read-only true)
  (next false read-only true))

(define *block)
(define *current-offset)
(define *symbol-table)
(define *ir)
(define *valid?)

(define (disassemble-one-instruction block offset symbol-table state receiver)
  (fluid-let ((*block block)
	      (*current-offset offset)
	      (*symbol-table symbol-table)
	      (*ir)
	      (*valid? true))
    (set! *ir (get-word))
    (let ((start-offset *current-offset))
      ;; External label markers come in two parts:
      ;; An entry type descriptor, and a gc offset.
      (cond ((eq? state 'EXTERNAL-LABEL-OFFSET)
	     (receiver *current-offset
		       (make-dc 'W *ir)
		       'INSTRUCTION))
	    ((external-label-marker? symbol-table offset state)
	     (receiver *current-offset
		       (make-dc 'W *ir)
		       'EXTERNAL-LABEL-OFFSET))
	    (else
	     (let ((instruction
		    (((vector-ref opcode-dispatch (extract *ir 12 16))))))
	       (if *valid?
		   (receiver *current-offset
			     instruction
			     (disassembler/next-state instruction state))
		   (let ((inst (make-dc 'W *ir)))
		     (receiver start-offset
			       inst
			       (disassembler/next-state inst state))))))))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  state					; ignored
  (if (and disassembler/compiled-code-heuristics?
	   (or (memq (car instruction) '(BRA JMP RTS))
	       (and (eq? (car instruction) 'JSR)
		    (let ((entry
			   (interpreter-register? (cadr instruction))))
		      (and entry
			   (eq? (car entry) 'ENTRY))))))
      'EXTERNAL-LABEL
      'INSTRUCTION))

(define (disassembler/lookup-symbol symbol-table offset)
  (and symbol-table
       (let ((label (dbg-labels/find-offset symbol-table offset)))
	 (and label 
	      (dbg-label/name label)))))

(define (external-label-marker? symbol-table offset state)
  (if symbol-table
      (let ((label (dbg-labels/find-offset symbol-table (+ offset 4))))
	(and label
	     (dbg-label/external? label)))
      (and *block
	   (not (eq? state 'INSTRUCTION))
	   (let loop ((offset (+ offset 4)))
	     (let ((contents (read-bits (- offset 2) 16)))
	       (if (bit-string-clear! contents 0)
		   (let ((offset
			  (- offset (bit-string->unsigned-integer contents))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset (bit-string->unsigned-integer contents))))))))

(define (make-dc wl bit-string)
  `(DC ,wl ,(bit-string->unsigned-integer bit-string)))

(define (read-procedure offset)
  (with-absolutely-no-interrupts
   (lambda ()
     (let-syntax ((ucode-type
		   (sc-macro-transformer
		    (lambda (form environment)
		      environment
		      (apply microcode-type (cdr form)))))
		  (ucode-primitive
		   (sc-macro-transformer
		    (lambda (form environment)
		      environment
		      (apply make-primitive-procedure (cdr form))))))
       ((ucode-primitive primitive-object-set-type 2)
	(ucode-type compiled-entry)
	((ucode-primitive make-non-pointer-object 1)
	 (read-unsigned-integer offset 32)))))))

(define (read-unsigned-integer offset size)
  (bit-string->unsigned-integer (read-bits offset size)))

(define (read-bits offset size-in-bits)
  (let ((word (bit-string-allocate size-in-bits))
	(bit-offset (* offset addressing-granularity)))
    (with-absolutely-no-interrupts
     (lambda ()
       (if *block
	   (read-bits! *block bit-offset word)
	   (read-bits! offset 0 word))))
    word))

;;;; Compiler specific information

(define make-data-register)
(define make-address-register)
(define make-address-offset)
(define interpreter-register?)

(let ()

#|
(define (register-maker assignments)
  (lambda (mode register)
    (list mode
	  (if disassembler/symbolize-output?
	      (cdr (assq register assignments))
	      register))))
|#

(set! make-data-register
  (lambda (mode register)
    (list mode
	  (if disassembler/symbolize-output?
	      (cdr (assq register data-register-assignments))
	      register))))

(set! make-address-register
  (lambda (mode register)
    (if disassembler/symbolize-output?
	(or (and (eq? mode '@A)
		 (= register interpreter-register-pointer)
		 (let ((entry (assq 0 interpreter-register-assignments)))
		   (and entry
			(cdr entry))))
	    (list mode (cdr (assq register address-register-assignments))))
	(list mode register))))

(define data-register-assignments
  '((0 . 0)	;serves multiple functions, not handled now
    (1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . REFERENCE-MASK)))

(define address-register-assignments
  '((0 . 0)
    (1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . DYNAMIC-LINK)
    (5 . FREE-POINTER)
    (6 . REGS-POINTER)
    (7 . STACK-POINTER)))

(set! make-address-offset
  (lambda (register offset)
    (if disassembler/symbolize-output?
	(or (interpreter-register register offset)
	    `(@AO ,(cdr (assq register address-register-assignments))
		  ,offset))
	`(@AO ,register ,offset))))

(set! interpreter-register?
  (lambda (effective-address)
    (case (car effective-address)
      ((@AO)
       (and (or (eq? (cadr effective-address) 'REGS-POINTER)
		(eqv? (cadr effective-address) interpreter-register-pointer))
	    (interpreter-register interpreter-register-pointer
				  (caddr effective-address))))
      ((REGISTER TEMPORARY ENTRY) effective-address)
      (else false))))

(define (interpreter-register register offset)
  (and (= register interpreter-register-pointer)
       (let ((entry (assq offset interpreter-register-assignments)))
	 (if entry
	     (cdr entry)
	     (let ((qr (integer-divide offset 2)))
	       (let ((entry
		      (assq (integer-divide-quotient qr)
			    interpreter-register-assignments)))
		 (and entry
		      (if (= (integer-divide-quotient qr) 0)
			  (cdr entry)
			  `(,@(cdr entry)
			    (,(integer-divide-quotient qr)))))))))))

(define interpreter-register-pointer
  6)

(define interpreter-register-assignments
  (let* ((first-entry (* 4 16))
	 (first-temp (+ first-entry (* 8 80))))
    (define (make-entries index names)
      (if (null? names)
	  '()
	  (cons `(,index . (ENTRY ,(car names)))
		(make-entries (+ index 8) (cdr names)))))
    `(;; Interpreter registers
      (0  . (REGISTER MEMORY-TOP))
      (4  . (REGISTER INT-MASK))
      (8  . (REGISTER VALUE))
      (12 . (REGISTER ENVIRONMENT))
      (16 . (REGISTER TEMPORARY))
      (44 . (REGISTER STACK-GUARD))
      ;; Interpreter entry points
      ,@(make-entries
	 first-entry
	 '(scheme-to-interface
	   scheme-to-interface-jsr
	   trampoline-to-interface
	   shortcircuit-apply
	   shortcircuit-apply-size-1
	   shortcircuit-apply-size-2
	   shortcircuit-apply-size-3
	   shortcircuit-apply-size-4
	   shortcircuit-apply-size-5
	   shortcircuit-apply-size-6
	   shortcircuit-apply-size-7
	   shortcircuit-apply-size-8
	   primitive-apply
	   primitive-lexpr-apply
	   error
	   link
	   interrupt-closure
	   interrupt-dlink
	   interrupt-procedure 
	   interrupt-continuation
	   assignment-trap
	   reference-trap
	   safe-reference-trap
	   &+
	   &-
	   &*
	   &/
	   &=
	   &<
	   &>
	   1+
	   -1+
	   zero?
	   positive?
	   negative?
	   primitive-error
	   allocate-closure
	   closure-hook
	   quotient
	   remainder
	   modulo
	   stack-and-interrupt-check-12
	   stack-and-interrupt-check-14
	   stack-and-interrupt-check-18
	   stack-and-interrupt-check-22
	   stack-and-interrupt-check-24
	   set-interrupt-enables
	   ))
      ;; Compiled code temporaries
      ,@(let loop ((i 0) (index first-temp))
	  (if (= i 256)
	      '()
	      (cons `(,index . (TEMPORARY ,i))
		    (loop (1+ i) (+ index 12))))))))
)

(define (make-pc-relative thunk)
  (let ((reference-offset *current-offset))
    (let ((pco (thunk)))
      (offset->pc-relative pco reference-offset))))

(define (offset->pc-relative pco reference-offset)
  (if disassembler/symbolize-output?
      `(@PCR ,(let ((absolute (+ pco reference-offset)))
		(or (disassembler/lookup-symbol *symbol-table absolute)
		    absolute)))
      `(@PCO ,pco)))

(define (undefined-instruction)
  ;; This losing assignment removes a 'cwcc'. Too bad.
  (set! *valid? false)
  '())

(define (undefined)
  undefined-instruction)

;; These are used by dassm1.scm

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)