#| -*-Scheme-*-

$Id: dassm2.scm,v 1.1 1992/08/29 13:51:19 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; Alpha Disassembler: Top Level
;;; Package: (compiler disassembler)

(declare (usual-integrations))

(set! compiled-code-block/bytes-per-object 4)
(set! compiled-code-block/objects-per-procedure-cache 2)
(set! compiled-code-block/objects-per-variable-cache 1)

(set! disassembler/read-variable-cache
      (lambda (block index)
	(let-syntax ((ucode-type
		      (macro (name) (microcode-type name)))
		     (ucode-primitive
		      (macro (name arity)
			(make-primitive-procedure name arity))))
	  ((ucode-primitive primitive-object-set-type 2)
	   (ucode-type quad)
	   (system-vector-ref block index)))))

(set! disassembler/read-procedure-cache
      (lambda (block index)
	(fluid-let ((*block block))
	  (let* ((offset (compiled-code-block/index->offset index)))
	    offset
	    ;; For now
	    (error "disassembler/read-procedure-cache: Not written")))))

(set! disassembler/instructions
  (lambda (block start-offset end-offset symbol-table)
    (let loop ((offset start-offset) (state (disassembler/initial-state)))
      (if (and end-offset (< offset end-offset))
	  (disassemble-one-instruction block offset symbol-table state
	    (lambda (offset* instruction state)
	      (make-instruction offset
				instruction
				(lambda () (loop offset* state)))))
	  '()))))

(set! disassembler/instructions/null?
  null?)

(set! disassembler/instructions/read
  (lambda (instruction-stream receiver)
    (receiver (instruction-offset instruction-stream)
	      (instruction-instruction instruction-stream)
	      (instruction-next instruction-stream))))

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
  (if (not (eq? state 'INSTRUCTION))
      (error "Unexpected disassembler state" state))
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
		   instruction
		   next-state))))))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  instruction state
  'INSTRUCTION)

(set! disassembler/lookup-symbol
  (lambda (symbol-table offset)
    (and symbol-table
	 (let ((label (dbg-labels/find-offset symbol-table offset)))
	   (and label 
		(dbg-label/name label))))))

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