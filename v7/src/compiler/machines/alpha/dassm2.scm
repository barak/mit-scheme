#| -*-Scheme-*-

$Id: dassm2.scm,v 1.5 2002/11/20 19:45:50 cph Exp $

Copyright (c) 1992-1999, 2001, 2002 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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