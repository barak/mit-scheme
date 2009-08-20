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

;;;; Spectrum Disassembler: Top Level
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
    (let* ((offset (compiled-code-block/index->offset index))
	   (opcode (fix:lsh (read-unsigned-integer offset 8) -2)))
      (case opcode
	((#x08)				; LDIL
	 ;; This should learn how to decode trampolines.
	 (vector 'COMPILED
		 (read-procedure offset)
		 (read-unsigned-integer (+ offset 10) 16)))
	(else
	 (error "disassembler/read-procedure-cache: Unknown opcode"
		opcode block index))))))

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
	  (receiver start-offset
		    (make-external-label *ir start-offset)
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
		   (if (and (pair? state)
			    (eq? (car state) 'PC-REL-OFFSET))
		       (pc-relative-inst offset instruction (cdr state))
		       instruction)
		   next-state))))))))

(define-integrable *privilege-level* 3)

(define (pc-relative-inst start-address instruction base-reg)
  (let ((opcode (car instruction)))
    (if (not (memq opcode '(LDO LDW)))
	instruction
	(let ((offset-exp (caddr instruction))
	      (target (cadddr instruction)))
	  (let ((offset (cadr offset-exp))
		(space-reg (caddr offset-exp))
		(base-reg* (cadddr offset-exp)))
	    (if (not (= base-reg* base-reg))
		instruction
		(let* ((real-address
			(+ start-address
			   (- offset *privilege-level*)
			   #|
			   (if (not left-side)
			       0
			       (- (let ((val (* left-side #x800)))
				    (if (>= val #x80000000)
					(- val #x100000000)
					val))
				  4))
			   |#
			   ))
		       (label
			(disassembler/lookup-symbol *symbol-table real-address)))
		  (if (not label)
		      instruction
		      `(,opcode () (OFFSET `(- ,label *PC*)
					   #|
					   ,(if left-side
						`(RIGHT (- ,label (- *PC* 4)))
						`(- ,label *PC*))
					   |#
					   ,space-reg
					   ,base-reg)
				,target)))))))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  (cond ((not disassembler/compiled-code-heuristics?)
	 'INSTRUCTION)
	((and (eq? state 'INSTRUCTION)
	      (eq? (list-ref instruction 0) 'BL)
	      (equal? (list-ref instruction 3) '(@PCO 0)))
	 (cons 'PC-REL-OFFSET (list-ref instruction 2)))
	((memq (car instruction) '(B BV BLE))
	 (if (memq 'N (cadr instruction))
	     'EXTERNAL-LABEL
	     'DELAY-SLOT))
	((eq? state 'DELAY-SLOT)
	 'EXTERNAL-LABEL)
	(else
	 'INSTRUCTION)))

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
	   (eq? state 'EXTERNAL-LABEL)
	   (let loop ((offset (+ offset 4)))
	     (let* ((contents (read-bits (- offset 2) 16))
		    (odd? (bit-string-clear! contents 0))
		    (delta (* 2 (bit-string->unsigned-integer contents))))
	       (if odd?
		   (let ((offset (- offset delta)))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset delta)))))))

(define (make-word bit-string)
  `(UWORD () ,(bit-string->unsigned-integer bit-string)))

(define (make-external-label bit-string offset)
  `(EXTERNAL-LABEL ()
		   ,(extract bit-string 16 32)
		   ,(offset->pc-relative (* 4 (extract bit-string 1 16))
					 offset)))

(define (read-procedure offset)
  (define (bit-string-andc-bang x y)
    (bit-string-andc! x y)
    x)

  (define-integrable (low-21-bits offset)
    #|
    (bit-string->unsigned-integer
     (bit-string-andc-bang (read-bits offset 32)
			   #*11111111111000000000000000000000))
    |#
    (fix:and (read-unsigned-integer (1+ offset) 24) #x1FFFFF))

  (define (assemble-21 val)
    (fix:or (fix:or (fix:lsh (fix:and val 1) 20)
		    (fix:lsh (fix:and val #xffe) 8))
	    (fix:or (fix:or (fix:lsh (fix:and val #xc000) -7)
			    (fix:lsh (fix:and val #x1f0000) -14))
		    (fix:lsh (fix:and val #x3000) -12))))
    

  (define (assemble-17 val)
    (fix:or (fix:or (fix:lsh (fix:and val 1) 16)
		    (fix:lsh (fix:and val #x1f0000) -5))
	    (fix:or (fix:lsh (fix:and val #x4) 8)
		    (fix:lsh (fix:and val #x1ff8) -3))))

  (with-absolutely-no-interrupts
    (lambda ()
      (let* ((address
	      (+ (* (assemble-21 (low-21-bits offset)) #x800)
		 (fix:lsh (assemble-17 (low-21-bits (+ offset 4))) 2)))
	     (bitstr (bit-string-andc-bang
		      (unsigned-integer->bit-string 32 address)
		      #*11111100000000000000000000000000)))
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
	    (bit-string->unsigned-integer bitstr))))))))

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

(define (offset->pc-relative pco reference-offset)
  (if (not disassembler/symbolize-output?)
      `(@PCO ,pco)
      ;; Only add 4 because it has already been bumped to the
      ;; next instruction.
      (let* ((absolute (+ pco (+ 4 reference-offset)))
	     (label (disassembler/lookup-symbol *symbol-table absolute)))
	(if label
	    `(@PCR ,label)
	    `(@PCO ,pco)))))

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 3)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)