#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/inerly.scm,v 1.1 1992/02/13 02:57:04 jinx Exp $
$MC68020-Header: inerly.scm,v 1.6 88/08/31 06:00:59 GMT cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;; i386 Instruction Set Macros.  Early version
;;; NOPs for now.

(declare (usual-integrations))

;;;; Transformers and utilities

(define early-instructions '())
(define early-transformers '())

(define (define-early-transformer name transformer)
  (set! early-transformers
	(cons (cons name transformer)
	      early-transformers)))

(define (eq-subset? s1 s2)
  (or (null? s1)
      (and (memq (car s1) s2)
	   (eq-subset? (cdr s1) s2))))

;;; Instruction and addressing mode macros

(syntax-table-define early-syntax-table 'DEFINE-INSTRUCTION
  (macro (opcode . patterns)
    `(SET! EARLY-INSTRUCTIONS
	   (CONS
	    (LIST ',opcode
		  ,@(map (lambda (pattern)
			   `(early-parse-rule
			     ',(car pattern)
			     (lambda (pat vars)
			       (early-make-rule
				pat
				vars
				(scode-quote
				 (instruction->instruction-sequence
				  ,(parse-instruction (cadr pattern)
						      (cddr pattern)
						      true)))))))
			 patterns))
		 EARLY-INSTRUCTIONS))))