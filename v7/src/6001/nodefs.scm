#| -*-Scheme-*-

$Id: nodefs.scm,v 1.10 1995/04/13 22:26:11 cph Exp $

Copyright (c) 1991-95 Massachusetts Institute of Technology

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

;;;; SCode rewriting for 6.001
;;; package: (student scode-rewriting)

(declare (usual-integrations))

(define (initialize-package!)
  (set! hook/repl-eval student/repl-eval)
  unspecific)

(define (student/repl-eval repl s-expression environment syntax-table)
  (repl-scode-eval
   repl
   (rewrite-scode (syntax s-expression syntax-table)
		  (and repl
		       (let ((port (cmdl/port repl)))
			 (let ((operation
				(port/operation
				 port
				 'CURRENT-EXPRESSION-CONTEXT)))
			   (and operation
				(operation port s-expression))))))
   environment))

(define (rewrite-scode expression context)
  (let ((expression
	 (if (open-block? expression)
	     (open-block-components expression unscan-defines)
	     expression)))
    (if (eq? context 'REPL-BUFFER)
	(make-sequence
	 (map (lambda (expression)
		(if (definition? expression)
		    (let ((name (definition-name expression))
			  (value (definition-value expression)))
		      (make-sequence
		       (list expression
			     (make-combination
			      write-definition-value
			      (cons name
				    (if (unassigned-reference-trap? value)
					'()
					(list (make-variable name))))))))
		    expression))
	      (sequence-actions expression)))
	expression)))

(define (write-definition-value name #!optional value)
  (with-string-output-port
   (lambda (port)
     (write name port)
     (if (not (default-object? value))
	 (begin
	   (write-string " --> " port)
	   (fluid-let ((*unparser-list-depth-limit* 2)
		       (*unparser-list-breadth-limit* 10)
		       (*unparser-string-length-limit* 30))
	     (write value port)))))))