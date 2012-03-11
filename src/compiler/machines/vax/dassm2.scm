#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; VAX Disassembler: Top Level
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
	   (opcode (read-unsigned-integer (+ offset 2) 16)))
      (case opcode
	((#x9f17)			; JMP @&<value>
	 ;; This should learn to decode trampolines.
	 (vector 'COMPILED
		 (read-procedure (+ offset 4))
		 (read-unsigned-integer offset 16)))
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
(define *valid?)

(define (disassemble-one-instruction block offset symbol-table state receiver)
  (define (instruction-end instruction state)
    (let ((next-state (disassembler/next-state instruction state)))
      (receiver *current-offset instruction next-state)))

  (fluid-let ((*block block)
	      (*current-offset offset)
	      (*symbol-table symbol-table)
	      (*valid? true))
    (let* ((byte (get-byte))
	   (start-offset *current-offset))
      ;; External label markers come in two parts:
      ;; An entry type descriptor, and a gc offset.
      (if (or (eq? state 'EXTERNAL-LABEL)
	      (eq? state 'EXTERNAL-LABEL-OFFSET)
	      (external-label-marker? symbol-table offset state))
	  (instruction-end (make-data-deposit byte 'W)
			   (if (eq? state 'EXTERNAL-LABEL-OFFSET)
			       state
			       'EXTERNAL-LABEL))
	  (let ((instruction
		 ((vector-ref
		   opcode-dispatch
		   (bit-string->unsigned-integer byte)))))
	    (if *valid?
		(instruction-end instruction state)
		(begin
		  (set! *current-offset start-offset)
		  (instruction-end
		   (make-data-deposit
		    byte
		    (if disassembler/compiled-code-heuristics?
			'W
			'B))
		   'UNKNOWN))))))))

(define (disassembler/initial-state)
  'INSTRUCTION)

(define (disassembler/next-state instruction state)
  (define (check delta state get-word)
    (let ((offset *current-offset))
      (let* ((next (bit-string->unsigned-integer (get-word)))
	     (result
	      (if (= (+ offset delta) (/ next 2))
		  state
		  'INSTRUCTION)))
	(set! *current-offset offset)
	result)))

  (cond ((or (not disassembler/compiled-code-heuristics?)
	     (eq? state 'EXTERNAL-LABEL-OFFSET))
	 'INSTRUCTION)
	((and (eq? state 'INSTRUCTION)
	      (or (memq (car instruction) '(BR JMP RSB))
		  (and (eq? (car instruction) 'JSB)
		       (let ((entry
			      (interpreter-register?
			       (cadr instruction))))
			 (and entry
			      (eq? (car entry) 'ENTRY))))))
	 (check 4 'EXTERNAL-LABEL (lambda () (get-word) (get-word))))
	((eq? state 'EXTERNAL-LABEL)
	 'EXTERNAL-LABEL-OFFSET)
	((eq? state 'UNKNOWN)
	 (check 2 'EXTERNAL-LABEL-OFFSET get-word))
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
	   (not (eq? state 'INSTRUCTION))
	   (let loop ((offset (+ offset 4)))
	     (let ((contents (read-bits (- offset 2) 16)))
	       (if (bit-string-clear! contents 0)
		   (let ((offset
			  (- offset
			     (/ (bit-string->unsigned-integer contents) 2))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset
		      (/ (bit-string->unsigned-integer contents) 2))))))))

(define (make-data-deposit *ir size)
  (case size
    ((B)
     `(BYTE U ,(bit-string->unsigned-integer *ir)))
    ((W)
     `(WORD U ,(bit-string->unsigned-integer
		(bit-string-append *ir (get-byte)))))
    ((L)
     `(LONG U ,(bit-string->unsigned-integer
		(bit-string-append (bit-string-append *ir (get-byte))
				   (get-word)))))))
  
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

(define-integrable (lookup-special-register reg table)
  (assq reg table))

(define-integrable (special-register reg-pair)
  (cdr reg-pair))

(define (make-register register)
  (let ((special (and disassembler/symbolize-output?
		      (assq register register-assignments))))
    (if special
	(cdr special)
	register)))

(define register-assignments
  '((0 . 0)	;serves multiple functions, not handled now
    (1 . 1)
    (2 . 2)
    (3 . 3)
    (4 . 4)
    (5 . 5)
    (6 . 6)
    (7 . 7)
    (8 . 8)
    (9 . 9)
    (10 . DYNAMIC-LINK)
    (11 . REFERENCE-MASK)
    (12 . FREE-POINTER)
    (13 . REGS-POINTER)
    (14 . STACK-POINTER)
    (15 . PROGRAM-COUNTER)))

(define (make-offset deferred? register size offset)
  (let ((key (if deferred? '@@RO '@RO)))
    (if (not disassembler/symbolize-output?)
	`(,key ,size ,register ,offset)
	(let ((special
	       (lookup-special-register register register-assignments)))
	  (if special
	      (if (eq? (special-register special) 'REGS-POINTER)
		  (let ((interpreter-register
			 (lookup-special-register offset 
						  interpreter-register-assignments)))
		    (cond ((not interpreter-register)
			   `(,key ,size REGS-POINTER ,offset))
			  ((not deferred?)
			   (special-register interpreter-register))
			  (else
			   `(@ ,(special-register interpreter-register)))))
		  `(,key ,size ,(special-register special) ,offset))
	      `(,key ,size ,register ,offset))))))

(define interpreter-register?
  (lambda (effective-address)
    (case (car effective-address)
      ((@RO)
       (and (eq? (caddr effective-address) 'REGS-POINTER)
	    (let ((entry
		   (assq (cadddr effective-address)
			 interpreter-register-assignments)))
	      (and entry
		   (cdr entry)))))
      ((REGISTER TEMPORARY ENTRY) effective-address)
      (else false))))

(define interpreter-register-assignments
  (let ()
    (define (make-entries index names)
      (if (null? names)
	  '()
	  (cons `(,index . (ENTRY ,(car names)))
		(make-entries (+ index 6) (cdr names)))))
    `(;; Interpreter registers
      (0  . (REGISTER MEMORY-TOP))
      (4  . (REGISTER INT-MASK))
      (8  . (REGISTER VALUE))
      (12 . (REGISTER ENVIRONMENT))
      (16 . (REGISTER TEMPORARY))
      (20 . (REGISTER INTERPRETER-CALL-RESULT:ENCLOSE))
      (24 . (REGISTER RETURN-CODE))
      (28 . (REGISTER LEXPR-PRIMITIVE-ACTUALS))
      (32 . (REGISTER MINIMUM-LENGTH))
      (36 . (REGISTER PRIMITIVE))
      (44 . (REGISTER STACK-GUARD))
      ;; Interface entry points
      ,@(make-entries
	 #x0280
	 '(link error apply
		lexpr-apply primitive-apply primitive-lexpr-apply
		cache-reference-apply lookup-apply
		interrupt-continuation interrupt-ic-procedure
		interrupt-procedure interrupt-closure
		lookup safe-lookup set! access unassigned? unbound? define
		reference-trap safe-reference-trap assignment-trap
		unassigned?-trap
		&+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?))
      ;; Compiler temporaries
      ,@(let loop ((index -4) (i 0))
	  (if (>= i 512)
	      '()
	      (cons `(,index . (TEMPORARY ,i))
		    (loop (- index 4) (1+ i))))))))


(define (make-pc-relative deferred? size pco)
  ;; This assumes that pco was just extracted.
  ;; VAX PC relative modes are defined with respect to the pc
  ;; immediately after the PC relative field.

  (define (default)
    `(,(if deferred? '@@PCO '@PCO) ,size ,pco))

  (define (test address)
    (disassembler/lookup-symbol *symbol-table address))

  (define (object-offset? relative)
    (let* ((unsigned (if (negative? relative)
			 (+ (expt 2 32) relative)
			 relative))
	   (tc (quotient unsigned (expt 2 scheme-datum-width))))

      (define (try tc)
	(let* ((object-base (* tc (expt 2 scheme-datum-width)))
	       (offset (- unsigned object-base)))
	  (cond ((test (+ *current-offset offset))
		 =>
		 (lambda (label)
		   (list label object-base)))
		(else
		 false))))

      (or (try tc)
	  (try (1+ tc)))))

  (let ((absolute (+ pco *current-offset)))
    (cond ((not disassembler/symbolize-output?)
	   (default))
	  ((test absolute)
	   =>
	   (lambda (answ)
	     `(,(if deferred? '@@PCR '@PCR) ,answ)))
	  ((test (- absolute 2))
	   ;; Kludge to get branches to execute caches correctly.
	   =>
	   (lambda (answ)
	     `(,(if deferred? '@@PCRO '@PCRO) ,answ 2)))
	  ((object-offset? pco)
	   =>
	   (lambda (answ)
	     `(,(if deferred? '@@PCRO '@PCRO) ,@answ)))
	  (else
	   (default)))))

(define (undefined-instruction)
  ;; This losing assignment removes a 'cwcc'. Too bad.
  (set! *valid? false)
  '())

(define (undefined)
  undefined-instruction)

(define compiled-code-block/procedure-cache-offset 0)
(define compiled-code-block/objects-per-procedure-cache 2)
(define compiled-code-block/objects-per-variable-cache 1)

;; global variable used by runtime/udata.scm -- Moby yuck!

(set! compiled-code-block/bytes-per-object 4)
