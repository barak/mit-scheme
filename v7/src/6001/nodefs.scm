#| -*-Scheme-*-

$Id: nodefs.scm,v 1.16 2005/04/01 05:09:26 cph Exp $

Copyright 1991,1992,1993,1995,2001,2003 Massachusetts Institute of Technology
Copyright 2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; SCode rewriting for 6.001
;;; package: (student scode-rewriting)

(declare (usual-integrations))

(define (initialize-package!)
  (set! hook/repl-eval student/repl-eval)
  unspecific)

(define (student/repl-eval s-expression environment repl)
  (repl-scode-eval
   (rewrite-scode (syntax s-expression environment)
		  (and repl
		       (let ((port (cmdl/port repl)))
			 (let ((operation
				(port/operation
				 port
				 'CURRENT-EXPRESSION-CONTEXT)))
			   (and operation
				(operation port s-expression))))))
   environment
   repl))

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
  (call-with-output-string
   (lambda (port)
     (write name port)
     (if (not (default-object? value))
	 (begin
	   (write-string " --> " port)
	   (fluid-let ((*unparser-list-depth-limit* 2)
		       (*unparser-list-breadth-limit* 10)
		       (*unparser-string-length-limit* 30))
	     (write value port)))))))