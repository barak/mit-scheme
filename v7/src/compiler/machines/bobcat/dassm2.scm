#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/dassm2.scm,v 1.1 1987/08/07 17:12:40 cph Exp $

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

;;;; 68000 Disassembler

(declare (usual-integrations))

(define ((with-info-to-file type receiver) filename)
  (let ((filename (->pathname filename)))
    (let ((block (file->block (pathname-new-type filename "com"))))
      (fluid-let ((*symbol-table))
	(setup-table! (pathname-new-type filename "binf"))
	(call-with-output-file (pathname-new-type filename type)
	  (lambda (port) (receiver block port)))))))

(define (block-code->port! block port)
  (define (instruction-output-string label? instruction)
    (let ((string (with-output-to-string
		    (lambda ()
		      (if label? (format "~%~s:" label?))
		      (format "~%  ")
		      (display instruction)))))
      (string-downcase! string)
      string))

  (let ((last-valid-offset (block-code-ending-offset block)))
    (let loop ((offset (block-code-starting-offset block)))
      (disassemble-one-instruction block offset
	(lambda (new-offset label? instruction)
	  (write-string (instruction-output-string label? instruction) port)
	  (and (<= new-offset last-valid-offset)
	       (loop new-offset)))))))

(define (block-constants->port! block port)
  (define (constant-output-string label? constant)
    (with-output-to-string
      (lambda ()
	(if label?
	    (format "~%~s:" (string-downcase label?)))
	(format "~%  ~o" constant))))

  (let ((last-valid-index (block-constants-ending-index block)))
    (let loop ((index (block-constants-starting-index block)))
      (and (<= index last-valid-index)
	   (let ((offset (block-index->offset index)))
	     (write-string 
	      (constant-output-string (lookup-label block offset)
				      (system-vector-ref block index))
	      port)
	     (loop (1+ index)))))))

(set! compiler:write-lap-file
  (with-info-to-file "lap"
    (lambda (block port)
      (newline port)
      (write-string "Executable Code:" port)
      (newline port)
      (block-code->port! block port)
      (newline port)
      (newline port)
      (write-string "Constants:" port)
      (newline port)
      (block-constants->port! block port))))

(set! compiler:write-constants-file
  (with-info-to-file "con" block-constants->port!))

(set! disassembly-stream
  (named-lambda (disassembly-stream start)
    (disassemble-anything start
      (lambda (base block offset)
	(let ((last-valid-offset (block-code-ending-offset block)))
	  (let loop ((offset offset))
	    (disassemble-one-instruction block offset
	      (lambda (new-offset label? instruction)
		(if (> new-offset last-valid-offset)
		    '()
		    ;; INSTRUCTION-STREAM-CONS
		    (cons (make-instruction offset label? instruction)
			  (delay (loop new-offset))))))))))))

(define (disassemble-anything thing continuation)
  (cond ((compiled-code-address? thing)
	 (let ((block (compiled-code-address->block thing)))
	   (continuation (primitive-datum block) 
			 block
			 (compiled-code-address->offset thing))))
	((integer? thing)
	 (continuation 0 0 thing))
	(else
	 (error "Unknown entry to disassemble" thing))))

(define (make-address base offset label?)
  (or label? offset))

(define *block)
(define *initial-offset)
(define *current-offset)
(define *valid?)
(define *ir)

(define (disassemble-one-instruction block offset receiver)
  (define (make-losing-instruction size)
    (if (eq? size 'W)
	`(DC W ,(bit-string->unsigned-integer *ir))
	`(DC L ,(bit-string->unsigned-integer (bit-string-append (get-word)
								 *ir)))))

  (fluid-let ((*block block)
	      (*initial-offset offset)
	      (*current-offset offset)
	      (*valid? true)
	      (*ir))
    (set! *ir (get-word))
    (receiver *current-offset
	      (lookup-label block offset)
	      (let ((size (dcw? block offset)))
		(if size
		    (make-losing-instruction size)
		    (let ((instruction
			   (((vector-ref opcode-dispatch (extract *ir 12 16))))))
		      (if *valid?
			  instruction
			  (make-losing-instruction 'W))))))))

(define (undefined-instruction)
  ;; This losing assignment removes a 'call/cc'. Too bad.
  (set! *valid? false)
  '())

(define (undefined)
  undefined-instruction)

;;;; Compiler specific information

(define data-register-assignments
  ;; D0 serves multiple functions, not handled now
  '((7 . REFERENCE-MASK)))

(define address-register-assignments
  '((4 . FRAME-POINTER)
    (5 . FREE-POINTER)
    (6 . REGS-POINTER)
    (7 . STACK-POINTER)))

(define interpreter-register-assignments
  (let-syntax ()
    (define-macro (make-table)
      (define (make-entries index names)
	(if (null? names)
	    '()
	    (cons `(,index . (ENTRY ,(car names)))
		  (make-entries (+ index 6) (cdr names)))))
      `'(;; Interpreter registers
         (0  . (REG MEMORY-TOP))
	 (4  . (REG STACK-GUARD))
	 (8  . (REG VALUE))
	 (12 . (REG ENVIRONMENT))
	 (16 . (REG TEMPORARY))
	 (20 . (REG INTERPRETER-CALL-RESULT:ENCLOSE))
	 ;; Interpreter entry points
	 ,@(make-entries 
	    #x00F0 
	    '(apply error wrong-number-of-arguments interrupt-procedure
		    interrupt-continuation lookup-apply lookup access
		    unassigned? unbound? set! define primitive-apply
		    enclose setup-lexpr return-to-interpreter safe-lookup
		    cache-variable reference-trap assignment-trap))
	 ,@(make-entries 
	    #x0228
	    '(uuo-link uuo-link-trap cache-reference-apply
		       safe-reference-trap unassigned?-trap
		       cache-variable-multiple uuo-link-multiple))))
    (make-table)))

(define-integrable (lookup-special-register reg table)
  (assq reg table))

(define-integrable (special-register reg-pair)
  (cdr reg-pair))

(define ((register-maker table) mode register)
  (let ((special (and disassembler:symbolize-output?
		      (lookup-special-register register table))))
    (list mode
	  (if special
	      (special-register special)
	      register))))

(define make-data-register
  (register-maker data-register-assignments))

(define make-address-register
  (register-maker address-register-assignments))

(define (make-address-offset register offset)
  (if (not disassembler:symbolize-output?)
      `(@AO ,register ,offset)
      (let ((special
	     (lookup-special-register register address-register-assignments)))
	(if special
	    (if (eq? (special-register special) 'REGS-POINTER)
		(let ((interpreter-register
		       (lookup-special-register offset 
						interpreter-register-assignments)))
		  (if interpreter-register
		      (special-register interpreter-register)
		      `(@AO REGS-POINTER ,offset)))
		`(@AO ,(special-register special) ,offset))
	    `(@AO ,register ,offset)))))

(define (make-pc-relative thunk)
  ;; Done this way to force order of evaluation
  (let* ((reference-offset *current-offset)
	 (pco (thunk)))
    (offset->pc-relative pco reference-offset)))

(define-integrable (offset->pc-relative pco reference-offset)
  (let ((absolute (+ pco reference-offset)))
    (if disassembler:symbolize-output?
	(let ((answ (lookup-label *block absolute)))
	  (if answ
	      `(@PCR ,answ)
	      `(@PCO ,(- pco (- reference-offset *initial-offset)))))
	`(@PCO ,(- pco (- reference-offset *initial-offset))))))

(define *symbol-table)

;; Temporary Kludge

(set! setup-table!
  (named-lambda (setup-table! filename)
    (set! *symbol-table
	  (make-binary-searcher (compiler-info-labels (fasload filename))
				offset/label-info=?
				offset/label-info<?))
    *symbol-table))

(define (lookup-label block offset)
  (and (not (unassigned? *symbol-table))
       (let ((label (*symbol-table offset)))
	 (and label 
	      (label-info-name label)))))

(define (dcw? block offset)
  (and (not (unassigned? *symbol-table))
       (let ((label (*symbol-table (+ offset 2))))
	 (and label
	      (label-info-external? label)
	      'W))))