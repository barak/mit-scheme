#| -*-Scheme-*-

$Id: dsyn.scm,v 1.10 2001/12/21 18:28:31 cph Exp $

Copyright (c) 1987, 1989, 1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; VAX Disassembler instruction definition syntax

(declare (usual-integrations))

;;;; Instruction decoding
#|
(define (initialize-package!)
  (environment-define-macro (->environment '(COMPILER DISASSEMBLER))
			    'DEFINE-INSTRUCTION
			    transform/define-instruction))
|#

(define instructions-disassembled-specially
  '(BYTE WORD LONG BUG B BR BSB))

(define-syntax define-instruction
  (lambda (name . patterns)
    (if (memq name instructions-disassembled-specially)
	''()
	`(begin ,@(map (lambda (pattern)
			 (process-instruction-definition name pattern))
		       patterns)))))

(define (process-instruction-definition name pattern)
  (let ((prefix (cons name (find-pattern-prefix (car pattern))))
	(opcode-field (cadr pattern))
	(operands (cddr pattern)))
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