#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/dassm2.scm,v 4.6 1989/05/17 20:28:17 jinx Exp $
$MC68020-Header: dassm2.scm,v 4.12 88/12/30 07:05:13 GMT cph Exp $

Copyright (c) 1987, 1989 Massachusetts Institute of Technology

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

;;;; VAX Disassembler: Top Level

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
		((#x9f17)		; JMP @#<value>
		 (vector 'COMPILED
			 (read-procedure (+ offset 2))
			 arity))
		((#x9f16)		; JSB @#<value>
		 (let* ((new-block
			 (compiled-code-address->block
			  (read-procedure (+ offset 2))))
			(offset
			 (fluid-let ((*block new-block))
			   (read-unsigned-integer 14 16))))
		   (case offset
		     ((#x106)		; lookup
		      (vector 'VARIABLE
			      (variable-cache-name
			       (system-vector-ref new-block 3))
			      arity))
		     ((#x10c)		; interpreted
		      (vector 'INTERPRETED
			      (system-vector-ref new-block 3)
			      arity))
		     ((#x112		; arity
		       #x11e		; entity
		       #x124 #x12a #x130 #x136 #x13c ; specialized arity
		       #x142 #x148 #x14e #x154 #x15e)
		      (vector 'COMPILED
			      (system-vector-ref new-block 3)
			      arity))
		     (else		; including #x118, APPLY
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
(define *valid?)

(define (disassemble-one-instruction block offset symbol-table state receiver)
  (fluid-let ((*block block)
	      (*current-offset offset)
	      (*symbol-table symbol-table)
	      (*valid? true))
    (let ((instruction
	   (let ((byte (get-byte)))
	     (if (external-label-marker? symbol-table offset state)
		 (make-data-deposit byte 'W)
		 (let ((instruction
			((vector-ref
			  opcode-dispatch
			  (bit-string->unsigned-integer byte)))))
		   (if *valid?
		       instruction
		       (make-data-deposit byte 'B)))))))
      (receiver *current-offset
		instruction
		(disassembler/next-state instruction state)))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  state					; ignored
  (if (and disassembler/compiled-code-heuristics?
	   (or (memq (car instruction) '(BR JMP RSB))
	       (and (eq? (car instruction) 'JSB)
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
			  (- offset
			     (/ (bit-string->unsigned-integer contents) 2))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset
		      (/ (bit-string->unsigned-integer contents) 2))))))))

(define (make-data-deposit *ir size)
  (case size
    ((B)
     `(BYTE ,(bit-string->unsigned-integer *ir)))
    ((W)
     `(WORD ,(bit-string->unsigned-integer
	      (bit-string-append *ir (get-byte)))))
    ((L)
     `(LONG ,(bit-string->unsigned-integer
	      (bit-string-append (bit-string-append *ir (get-byte))
				 (get-word)))))))
  
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
      (4  . (REGISTER STACK-GUARD))
      (8  . (REGISTER VALUE))
      (12 . (REGISTER ENVIRONMENT))
      (16 . (REGISTER TEMPORARY))
      (20 . (REGISTER INTERPRETER-CALL-RESULT:ENCLOSE))
      (24 . (REGISTER RETURN-CODE))
      (28 . (REGISTER LEXPR-PRIMITIVE-ACTUALS))
      (32 . (REGISTER MINIMUM-LENGTH))
      (36 . (REGISTER PRIMITIVE))
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
  (let ((absolute (+ pco *current-offset)))
    (if disassembler/symbolize-output?
	(let ((answ (disassembler/lookup-symbol *symbol-table absolute)))
	  (if answ
	      `(,(if deferred? '@@PCR '@PCR) ,answ)
	      `(,(if deferred? '@@PCO '@PCO) ,size ,pco)))
	`(,(if deferred? '@@PCO '@PCO) ,size ,pco))))

(define (undefined-instruction)
  ;; This losing assignment removes a 'cwcc'. Too bad.
  (set! *valid? false)
  '())

(define (undefined)
  undefined-instruction)
