#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; MIPS Disassembler: Top Level
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
      (let ((JAL (read-bits offset 32))
	    (ADDI (read-bits (+ offset 4) 32)))
	(let ((opcode
	       (bit-string->unsigned-integer (bit-substring JAL 26 32))))
	  (case opcode
	    ((#x3)			; JAL
	     ;; This should learn how to decode trampolines.
	     (vector 'COMPILED
		     (read-procedure offset)
		     (bit-string->unsigned-integer
		      (bit-substring ADDI 0 16))))
	    (else
	     (error "disassembler/read-procedure-cache: Unknown opcode"
		    opcode block index))))))))

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
    (set! *ir (get-longword))
    (let ((start-offset *current-offset))
      (if (external-label-marker? symbol-table offset state)
	  (receiver *current-offset
		    (make-external-label *ir)
		    'INSTRUCTION)
	  (let ((instruction (disassemble-word *ir)))
	    (if (not *valid?)
		(let ((inst (make-word *ir)))
		  (receiver start-offset
			    inst
			    (disassembler/next-state inst state)))
		(let ((next-state (disassembler/next-state instruction state)))
		  (receiver
		   *current-offset
		   (cond ((and (pair? state)
			       (eq? (car state) 'PC-REL-LOW-OFFSET))
			  (pc-relative-inst offset instruction (cadr state)))
			((and (eq? 'PC-REL-OFFSET state)
			      (not (pair? next-state)))
			 (pc-relative-inst offset instruction false))
			(else
			 instruction))
		   next-state))))))))

(define (pc-relative-inst start-address instruction left-side)
  (let ((opcode (car instruction)))
    (if (not (memq opcode '(LDO LDW)))
	instruction
	(let ((offset-exp (caddr instruction))
	      (target (cadddr instruction)))
	  (let ((offset (cadr offset-exp))
		(space-reg (caddr offset-exp))
		(base-reg (cadddr offset-exp)))
	    (let* ((real-address
		    (+ start-address
		       offset
		       (if (not left-side)
			   0
			   (- (let ((val (* left-side #x800)))
				(if (>= val #x80000000)
				    (- val #x100000000)
				    val))
			      4))))
		   (label
		    (disassembler/lookup-symbol *symbol-table real-address)))
	      (if (not label)
		  instruction
		  `(,opcode () (OFFSET ,(if left-side
					    `(RIGHT (- ,label (- *PC* 4)))
					    `(- ,label *PC*))
				       ,space-reg
				       ,base-reg)
			    ,target))))))))	    

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  instruction state
  'INSTRUCTION)

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
			  (- offset
			     (* 2 (bit-string->unsigned-integer contents)))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset
		      (* 2 (bit-string->unsigned-integer contents)))))))))

(define (make-word bit-string)
  `(UWORD ,(bit-string->unsigned-integer bit-string)))

(define (make-external-label bit-string)
  (let ((do-it
	 (lambda (format-word offset)
	   `(EXTERNAL-LABEL (FORMAT ,format-word)
			    ,(offset->@pcr (* 2 offset))))))
    (if (eq? endianness 'LITTLE)
	(do-it (extract bit-string 0 16)
	       (extract bit-string 16 32))
	(do-it (extract bit-string 16 32)
	       (extract bit-string 0 16)))))

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
	 (bit-string->unsigned-integer
	  (bit-substring (read-bits offset 32) 0 26))))))))

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

(define (invalid-instruction)
  (set! *valid? false)
  false)

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)