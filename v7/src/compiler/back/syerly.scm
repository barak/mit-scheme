#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/syerly.scm,v 1.1 1987/06/25 10:56:09 jinx Exp $

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

;;;; Syntax time instruction expansion

(declare (usual-integrations))

(define ->lap-instructions-expander
  ((access scode->scode-expander package/expansion package/scode-optimizer)
   (lambda (operands if-expanded if-not-expanded)
     (define (wrap expression)
       (if-expanded
	(scode/make-combination
	 (scode/make-variable '->INSTRUCTION-SEQUENCE)
	 (list expression))))

     (define (kernel instruction rules)
       (early-pattern-lookup
	rules
	instruction
	(lambda (mode result)
	  (cond ((false? mode)
		 (error "->lap-instruction-expander: unknown instruction"
			instruction))
		((eq? mode 'TOO-MANY)
		 (if-not-expanded))
		(else (wrap result))))
	1))

     (let ((instruction (scode/unquasiquote (car operands))))
       (cond ((not (pair? instruction))
	      (error "->lap-instruction-expander: bad instruction" instruction))
	     ((eq? (car instruction) 'EVALUATE)
	      (if-not-expanded))
	     ((memq (car instruction)
		    '(EQUATE SCHEME-OBJECT ENTRY-POINT LABEL))
	      (wrap (scode/make-absolute-combination 'LIST operands)))
	     (else
	      (let ((place (assq (car instruction) early-instructions)))
		(if (null? place)
		    (error "->lap-instruction-expander: unknown opcode"
			   (car instruction))
		    (kernel (cdr instruction) (cdr place))))))))))

(define (scode/unquasiquote exp)
  (cond ((scode/combination? exp)
	 (scode/combination-components
	  exp
	  (lambda (operator operands)
	    (define (kernel operator-name)
	      (case operator-name
		((CONS)
		 (cons (scode/unquasiquote (car operands))
		       (scode/unquasiquote (cadr operands))))
		((LIST)
		 (apply list (map scode/unquasiquote operands)))
		((CONS*)
		 (apply cons* (map scode/unquasiquote operands)))
		((APPEND)
		 (mapcan (lambda (component)
			   (if (scode/constant? component)
			       (scode/constant-value component)
			       (list (list 'EVALUATE-SPLICE component))))
			 operands))
		(else (list 'EVALUATE exp))))
	    (cond ((eq? operator cons)
		   ;; integrations
		   (kernel 'CONS))
		  ((scode/absolute-reference? operator)
		   (kernel (scode/absolute-reference-name operator)))
		  (else (list 'EVALUATE exp))))))
	((scode/constant? exp)
	 (scode/constant-value exp))
	(else (list 'EVALUATE exp))))
      
