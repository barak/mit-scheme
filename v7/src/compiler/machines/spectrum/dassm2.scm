#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/dassm2.scm,v 4.18 1992/08/11 02:37:45 jinx Exp $
$MC68020-Header: dassm2.scm,v 4.17 90/05/03 15:17:04 GMT jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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

;;;; Spectrum Disassembler: Top Level
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
      ;; For now
      (error "disassembler/read-procedure-cache: Not written"))))

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
  (cond ((not disassembler/compiled-code-heuristics?)
	 'INSTRUCTION)
	((and (eq? state 'INSTRUCTION)
	      (equal? instruction '(BL () 1 (@PCO 0))))
	 'PC-REL-DEP)
	((and (eq? state 'PC-REL-DEP)
	      (equal? instruction '(DEP () 0 31 2 1)))
	 'PC-REL-OFFSET)
	((and (eq? state 'PC-REL-OFFSET)
	      (= (length instruction) 4)
	      (equal? (list (car instruction)
			    (cadr instruction)
			    (cadddr instruction))
		      '(ADDIL () 1)))
	 (list 'PC-REL-LOW-OFFSET (caddr instruction)))
	((memq (car instruction) '(B BV BLE))
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
	   (not (eq? state 'INSTRUCTION))
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
#|
;;; 68k version

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
|#

(define (read-procedure offset)
  (error "read-procedure: Called" offset))

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