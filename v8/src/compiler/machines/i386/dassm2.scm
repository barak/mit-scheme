#| -*-Scheme-*-

$Id: cae5a3d2cfd5d1160253af17345a00613aabbbbf $
$MC68020-Header: /scheme/compiler/bobcat/RCS/dassm2.scm,v 4.18 1991/05/07 13:46:04 jinx Exp $

Copyright (c) 1992, 1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Intel i386 Disassembler: Top Level
;;; package: (compiler disassembler)

(declare (usual-integrations))

(define (disassembler/read-variable-cache block index)
  (let-syntax ((ucode-type
		(macro (name) (microcode-type name)))
	       (ucode-primitive
		(macro (name arity)
		  (make-primitive-procedure name arity))))
    ((ucode-primitive primitive-object-set-type 2)
     (ucode-type quad)
     (system-vector-ref block index))))

(define (disassembler/read-procedure-cache block index)
  (fluid-let ((*block block))
    (let* ((offset (compiled-code-block/index->offset index)))
      (let ((opcode (read-unsigned-integer (+ offset 3) 8))
	    (arity (read-unsigned-integer offset 16)))
	(case opcode
	  ((#xe9)			; (JMP (@PCR label))
	   ;; This should learn how to decode the new trampolines.
	   (vector 'COMPILED
		   (read-procedure (+ offset 4))
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

(define-integrable (disassembler/instructions/null? obj)
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
(define *valid?)

(define (disassemble-one-instruction block offset symbol-table state receiver)
  (fluid-let ((*block block)
	      (*current-offset offset)
	      (*symbol-table symbol-table)
	      (*valid? true))
    (let ((start-offset *current-offset))
      ;; External label markers come in two parts:
      ;; An entry type descriptor, and a gc offset.
      (cond ((eq? state 'EXTERNAL-LABEL-OFFSET)
	     (let* ((word (next-unsigned-16-bit-word))
		    (label (find-label *current-offset)))
	       (receiver *current-offset
			 (if label
			     `(BLOCK-OFFSET ,label)
			     `(WORD U ,word))
			 'INSTRUCTION)))
	    ((external-label-marker? symbol-table offset state)
	     (let ((word (next-unsigned-16-bit-word)))
	       (receiver *current-offset
			 `(WORD U ,word)
			 'EXTERNAL-LABEL-OFFSET)))
	    (else
	     (let ((instruction (disassemble-next-instruction)))
	       (if (or *valid? (not (eq? 'BYTE (car instruction))))
		   (receiver *current-offset
			     instruction
			     (disassembler/next-state instruction state))
		   (let ((inst `(BYTE U ,(caddr instruction))))
		     (receiver (1+ start-offset)
			       inst
			       (disassembler/next-state inst state))))))))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  state					; ignored
  (if (and disassembler/compiled-code-heuristics?
	   (or (memq (car instruction) '(JMP RET))
	       (and (eq? (car instruction) 'CALL)
		    (let ((operand (cadr instruction)))
		      (or (and (pair? operand)
			       (eq? (car operand) 'ENTRY))
			  (let ((entry
				 (interpreter-register? operand)))
			    (and entry
				 (eq? (car entry) 'ENTRY))))))))
      'EXTERNAL-LABEL
      'INSTRUCTION))

(define (disassembler/lookup-symbol symbol-table offset)
  (and symbol-table
       (let ((label (dbg-labels/find-offset symbol-table offset)))
	 (and label 
	      (dbg-label/name label)))))

(define (external-label-marker? symbol-table offset state)
  (define-integrable (offset-word->offset word)
    (fix:quotient (bit-string->unsigned-integer word) 2))

  (if symbol-table
      (let ((label (dbg-labels/find-offset symbol-table (+ offset 4))))
	(and label
	     (dbg-label/external? label)))
      (and *block
	   (not (eq? state 'INSTRUCTION))
	   (let loop ((offset (+ offset 4)))
	     (let ((contents (read-bits (- offset 2) 16)))
	       (if (bit-string-clear! contents 0)
		   (let ((offset (- offset (offset-word->offset contents))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset (offset-word->offset contents))))))))

(define (read-procedure offset)
  (with-absolutely-no-interrupts
   (lambda ()
     (let-syntax ((ucode-type
		   (macro (name) (microcode-type name)))
		  (ucode-primitive
		   (macro (name arity)
		     (make-primitive-procedure name arity))))
       ((ucode-primitive primitive-object-set-type 2)
	(ucode-type compiled-entry)
	((ucode-primitive make-non-pointer-object 1)
	 (+ (read-signed-integer offset 32)
	    (+ (if *block
		   (object-datum *block)
		   0)
	       (+ offset 4)))))))))

(define (read-unsigned-integer offset size)
  (bit-string->unsigned-integer (read-bits offset size)))

(define (read-signed-integer offset size)
  (bit-string->signed-integer (read-bits offset size)))

(define (read-bits offset size-in-bits)
  (let ((word (bit-string-allocate size-in-bits))
	(bit-offset (* offset addressing-granularity)))
    (with-absolutely-no-interrupts
     (lambda ()
       (if *block
	   (read-bits! *block bit-offset word)
	   (read-bits! offset 0 word))))
    word))

(define-integrable (make-unsigned-reader nbits)
  (let ((nbytes (fix:quotient nbits 8)))
    (lambda ()
      (let ((offset *current-offset))
	(let ((word (read-bits offset nbits)))
	  (set! *current-offset (+ offset nbytes))
	  (bit-string->unsigned-integer word))))))

(define-integrable (make-signed-reader nbits)
  (let ((nbytes (fix:quotient nbits 8)))
    (lambda ()
      (let ((offset *current-offset))
	(let ((word (read-bits offset nbits)))
	  (set! *current-offset (+ offset nbytes))
	  (bit-string->signed-integer word))))))

(define next-byte (make-signed-reader 8))
(define next-unsigned-byte (make-unsigned-reader 8))
(define next-16-bit-word (make-signed-reader 16))
(define next-unsigned-16-bit-word (make-unsigned-reader 16))
(define next-32-bit-word (make-signed-reader 32))
(define next-unsigned-32-bit-word (make-unsigned-reader 32))

(define (find-label offset)
  (and disassembler/symbolize-output?
       (disassembler/lookup-symbol *symbol-table offset)))

(define (interpreter-register? operand)
  (define (regs-pointer? reg)
    (if (symbol? reg)
	(eq? reg 'ESI)
	(= reg 6)))
  
  (define (offset->register offset)
    (let ((place (assq offset interpreter-register-offsets)))
      (and place
	   (cdr place))))

  (and (pair? operand)
       (or (and (eq? (car operand) '@R)
		(regs-pointer? (cadr operand))
		(offset->register 0))
	   (and (eq? (car operand) '@RO)
		(regs-pointer? (caddr operand))
		(offset->register (cadddr operand))))))

(define interpreter-register-offsets
  (letrec ((make-entries
	    (lambda (kind offset names)
	      (if (null? names)
		  '()
		  (cons (cons offset `(,kind ,(car names)))
			(make-entries kind
				      (+ offset 4)
				      (cdr names)))))))
    (append
     (make-entries
      'REGISTER 0
      '(memtop
	stack-guard
	val
	env
	compiler-temp
	expr
	return-code
	lexpr-actuals
	primitive
	closure-free
	closure-space))

     (make-entries
      'ENTRY #x40			; 16 * 4
      '(scheme-to-interface
	scheme-to-interface/call
	trampoline-to-interface
	interrupt-procedure
	interrupt-continuation
	interrupt-closure
	interrupt-dlink
	primitive-apply
	primitive-lexpr-apply
	assignment-trap
	reference-trap
	safe-reference-trap
	link
	error
	primitive-error
	short-primitive-apply))

     (make-entries
      'ENTRY #x-80
      '(&+
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
	quotient
	remainder
	modulo
	shortcircuit-apply		; Used by rules3, for speed.
	shortcircuit-apply-size-1	; Small frames, save time and space.
	shortcircuit-apply-size-2
	shortcircuit-apply-size-3
	shortcircuit-apply-size-4
	shortcircuit-apply-size-5
	shortcircuit-apply-size-6
	shortcircuit-apply-size-7
	shortcircuit-apply-size-8)))))

;; These are used by dassm1.scm

(define compiled-code-block/procedure-cache-offset 1)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)