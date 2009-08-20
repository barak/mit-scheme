#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL * DATUM) (cdr form))
	 (if (memq (cadr form) instructions-disassembled-specially)
	     `'()
	     `(,(close-syntax 'BEGIN environment)
		,@(map (lambda (pattern)
			 (process-instruction-definition (cadr form)
							 pattern
							 environment))
		       (cddr form))))
	 (ill-formed-syntax form)))))

(define (process-instruction-definition name pattern environment)
  (let ((prefix (cons name (find-pattern-prefix (car pattern))))
	(opcode-field (cadr pattern))
	(operands (cddr pattern)))
    (if (not (eq? (car opcode-field) 'BYTE))
	(error "Unhandled opcode kind:" opcode-field))
    (let ((opcode (cadadr opcode-field)))
      (case (caadr opcode-field)		;size in bits
	((8)
	 `(,(close-syntax 'DEFINE-STANDARD-INSTRUCTION environment)
	   ,opcode
	   ,(make-instruction-parser prefix operands environment)))
	((16)
	 (let ((low (remainder opcode 256))
	       (high (quotient opcode 256)))
	   (if (not (= low #xFD))
	       (error "Unhandled extension:" opcode))
	   `(,(close-syntax 'DEFINE-EXTENDED-INSTRUCTION environment)
	     ,high
	     ,(make-instruction-parser prefix operands environment))))
	(else
	 (error "Bad opcode size:" (caadr opcode-field)))))))

(define (find-pattern-prefix pattern)	; KLUDGE
  (if (and (pair? pattern)
	   (not (and (pair? (car pattern))
		     (eq? (caar pattern) '?))))
      (cons (car pattern) (find-pattern-prefix (cdr pattern)))
      '()))

(define (make-instruction-parser prefix operands environment)
  `(,(close-syntax 'LAMBDA environment)
    ()
    (,(close-syntax 'APPEND environment)
     ',prefix
     ,(process-operands operands environment))))

;; A let is used below to force the order of evaluation.

(define (process-operands operands environment)
  (if (pair? operands)
      (let ((temp (make-synthetic-identifier 'TEMP)))
	`(,(close-syntax 'LET environment)
	  ((,temp
	    ,(let ((operand (car operands)))
	       (case (car operand)
		 ((OPERAND)
		  `(,(close-syntax 'DECODE-OPERAND environment)
		    ',(cadr operand)))
		 ((DISPLACEMENT)
		  `(,(close-syntax 'DECODE-DISPLACEMENT environment)
		    ,(caadr operand)))
		 (else
		  (error "Unknown operand kind:" operand))))))
	   (,(close-syntax 'CONS environment)
	    ,temp
	    ,(process-operands (cdr operands) environment))))
      `'()))