#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/vax/dsyn.scm,v 1.5 1987/08/21 02:49:28 jinx Exp $

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

;;;; VAX Disassembler instruction definition syntax

(declare (usual-integrations))

;;;; Instruction decoding

(define instructions-handled-specially '(DC BUG B BR BSB))

(define disassembler-syntax-table
  (make-syntax-table system-global-syntax-table))

(syntax-table-define disassembler-syntax-table
    'DEFINE-INSTRUCTION
  (macro (name . cases)
    (if (memq name instructions-handled-specially)
	''()
	`(begin ,@(map (lambda (case)
			 (process-instruction-definition name case))
		       cases)))))

(define (process-instruction-definition name case)
  (let ((prefix (cons name (find-pattern-prefix (car case))))
	(opcode-field (cadr case))
	(operands (cddr case)))
    (if (not (eq? (car opcode-field) 'BYTE))
	(error "process-instruciton-definition: unhandled opcode kind"
	       opcode-field))
    (let ((opcode (cadadr opcode-field)))
      (case (caadr opcode-field)		;size in bits
	((8)
	 `(define-standard-instruction ,opcode
	    ,(make-instruction-parser prefix operands)))
	((16)
	 (let ((low (remainder opcode 256))
	       (high (quotient opcode 256)))
	   (if (not (= low #xFD))
	       (error "process-instruction-definition: unhandled extension"
		      opcode))
	   `(define-extended-instruction ,high
	      ,(make-instruction-parser prefix operands))))
	(else
	 (error "process-instruction-definition: bad opcode size"
		(caadr opcode-field)))))))

(define (find-pattern-prefix pattern)	; KLUDGE
  (if (or (null? pattern)
	  (and (pair? (car pattern)) (eq? (caar pattern) '?)))
      '()
      (cons (car pattern) (find-pattern-prefix (cdr pattern)))))

(define (make-instruction-parser prefix operands)
  `(lambda ()
     (append ',prefix
	     ,(process-operands operands))))

;; A let* is used below to force the order of evaluation.

(define (process-operands operands)
  (if (null? operands)
      ''()
      `(let* ((this ,(let ((operand (car operands)))
		       (case (car operand)
			 ((OPERAND)
			  `(decode-operand ',(cadr operand)))
			 ((DISPLACEMENT)
			  `(decode-displacement ,(caadr operand)))
			 (else
			  (error "process-operand: Unknown operand kind"
				 operand)))))
	      (rest ,(process-operands (cdr operands))))
	 (cons this rest))))