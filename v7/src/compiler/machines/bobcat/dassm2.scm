#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/dassm2.scm,v 4.12 1988/12/30 07:05:13 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; 68000 Disassembler: Top Level

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
	    (let ((opcode (read-unsigned-integer offset 16))
		  (arity (read-unsigned-integer (+ offset 6) 16)))
	      (case opcode
		((#x4ef9)		; JMP <value>.L
		 (vector 'COMPILED
			 (read-procedure (+ offset 2))
			 arity))
		((#x4eb9)		; JSR <value>.L
		 (let* ((new-block
			 (compiled-code-address->block
			  (read-procedure (+ offset 2))))
			(offset
			 (fluid-let ((*block new-block))
			   (read-unsigned-integer 14 16))))
		   (case offset
		     ((#xf6)		; lookup
		      (vector 'VARIABLE
			      (variable-cache-name
			       (system-vector-ref new-block 3))
			      arity))
		     ((#xfc)		; interpreted
		      (vector 'INTERPRETED
			      (system-vector-ref new-block 3)
			      arity))
		     ((#x102		; arity
		       #x10e		; entity
		       #x258 #x25e #x264 #x26a #x270 ; specialized arity
		       #x276 #x27c #x282 #x288 #x28e)
		      (vector 'COMPILED
			      (system-vector-ref new-block 3)
			      arity))
		     (else		; including #x108, APPLY
		      (error
		       "disassembler/read-procedure-cache: Unknown offset"
		       offset block index)))))
		(else
		 (error "disassembler/read-procedure-cache: Unknown opcode"
			opcode block index))))))))

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
		   (macro (name) (microcode-type name)))
		  (ucode-primitive
		   (macro (name arity)
		     (make-primitive-procedure name arity))))
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
		(and (number? (cadr effective-address))
		     (= (cadr effective-address)
			interpreter-register-pointer)))	    (interpreter-register interpreter-register-pointer
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
  (let ()
    (define (make-entries index names)
      (if (null? names)
	  '()
	  (cons `(,index . (ENTRY ,(car names)))
		(make-entries (+ index 6) (cdr names)))))
    `(;; Interpreter registers
      (0  . (REGISTER MEMORY-TOP))
      (4  . (REGISTER STACK-GUARD))
      (8  . (REGISTER VALUE))
      (12 . (REGISTER ENVIRONMENT))
      (16 . (REGISTER TEMPORARY))
      ;; Compiler temporaries
      ,@(let loop ((index 40) (i 0))
	  (if (= i 50)
	      '()
	      (cons `(,index . (TEMPORARY ,i))
		    (loop (+ index 4) (1+ i)))))
      ;; Interpreter entry points
      ,@(make-entries
	 #x012c
	 '(link error apply
		lexpr-apply primitive-apply primitive-lexpr-apply
		cache-reference-apply lookup-apply
		interrupt-continuation interrupt-ic-procedure
		interrupt-procedure interrupt-closure
		lookup safe-lookup set! access unassigned? unbound? define
		reference-trap safe-reference-trap assignment-trap
		unassigned?-trap
		&+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?))))))

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