#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/dassm2.scm,v 4.3 1988/02/11 21:12:27 bal Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; VMS Disassembler: Top Level

(declare (usual-integrations))

(set! compiled-code-block/bytes-per-object 4)

(set! disassembler/instructions
  (lambda (block start-offset end-offset symbol-table)
    (let loop ((offset start-offset) (state (disassembler/initial-state)))
      (if (and end-offset
	       (< offset end-offset))
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
    (let ((instruction
	   (if (external-label-marker? symbol-table offset state)
	       (make-dc 'W *ir)
	       (let ((instruction
		      (((vector-ref opcode-dispatch (extract *ir 12 16))))))
		 (if *valid?
		     instruction
		     (make-dc 'W *ir))))))
      (receiver *current-offset
		instruction
		(disassembler/next-state instruction state)))))

(define (disassembler/initial-state)
  'INSTRUCTION-NEXT)

(define (disassembler/next-state instruction state)
  (if (and disassembler/compiled-code-heuristics?
	   (or (memq (car instruction) '(BR JMP RSB))
	       (and (eq? (car instruction) 'JSB)
		    (let ((entry
			   (interpreter-register? (cadr instruction))))
		      (and entry
			   (eq? (car entry) 'ENTRY)
			   (not (eq? (cadr entry) 'SETUP-LEXPR)))))))
      'EXTERNAL-LABEL
      'INSTRUCTION))

(set! disassembler/lookup-symbol
  (lambda (symbol-table offset)
    (and symbol-table
	 (let ((label (sorted-vector/find-element symbol-table offset)))
	   (and label 
		(label-info-name label))))))

(define (external-label-marker? symbol-table offset state)
  (if symbol-table
      (sorted-vector/there-exists? symbol-table
				   (+ offset 2)
				   label-info-external?)
      (and *block
	   (not (eq? state 'INSTRUCTION))
	   (let loop ((offset (+ offset 2)))
	     (let ((contents (read-bits (- offset 2) 16)))
	       (if (bit-string-clear! contents 0)
		   (let ((offset
			  (- offset (bit-string->unsigned-integer contents))))
		     (and (positive? offset)
			  (loop offset)))
		   (= offset (bit-string->unsigned-integer contents))))))))

(define (make-dc wl bit-string)
  `(DC ,wl ,(bit-string->unsigned-integer bit-string)))

(define (read-bits offset size-in-bits)
  (let ((word (bit-string-allocate size-in-bits)))
    (with-interrupt-mask interrupt-mask-none
      (lambda (old)
	(read-bits! (if *block
			(+ (primitive-datum *block) offset)
			offset)
		    0
		    word)))
    word))

;;;; Compiler specific information

(define make-register-offset)
(define interpreter-register?)

(let ()

(define (register-maker assignments)
  (lambda (mode register)
    (list mode
	  (if disassembler/symbolize-output?
	      (cdr (assq register assignments))
	      register))))

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
    (15 . PC)))

(set! make-register-offset
  (lambda (register offset)
    (if disassembler/symbolize-output?
	(or (and (= register interpreter-register-pointer)
		 (let ((entry (assq offset interpreter-register-assignments)))
		   (and entry
			(cdr entry))))
	    `(@RO ,(cdr (assq register register-assignments))
		  ,offset))
	`(@RO ,register ,offset))))

(set! interpreter-register?
  (lambda (effective-address)
    (case (car effective-address)
      ((@RO)
       (and (= (cadr effective-address) interpreter-register-pointer)
	    (let ((entry
		   (assq (caddr effective-address)
			 interpreter-register-assignments)))
	      (and entry
		   (cdr entry)))))
      ((REGISTER TEMPORARY ENTRY) effective-address)
      (else false))))

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
      (20 . (REGISTER INTERPRETER-CALL-RESULT:ENCLOSE))
      ;; Compiler temporaries
      ,@(let loop ((index 40) (i 0))
	  (if (= i 50)
	      '()
	      (cons `(,index . (TEMPORARY ,i))
		    (loop (+ index 4) (1+ i)))))
      ;; Interpreter entry points
      ,@(make-entries
	 #x00F0
	 '(return-to-interpreter 
	   uuo-link-trap operator-trap
	   apply error wrong-number-of-arguments
	   interrupt-procedure interrupt-continuation lookup-apply 
	   lookup access unassigned? unbound? set! define primitive-apply enclose
	   setup-lexpr safe-lookup cache-variable reference-trap
	   assignment-trap uuo-link cache-reference-apply
	   safe-reference-trap unassigned?-trap cache-variable-multiple
	   uuo-link-multiple &+ &- &* &/ &= &< &> 1+ -1+ zero? positive? negative?
	   cache-assignment cache-assignment-multiple primitive-lexpr-apply)))))

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
