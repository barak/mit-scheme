#| -*-Scheme-*-

$Id: nodefs.scm,v 1.13 2002/11/20 19:45:46 cph Exp $

Copyright (c) 1991-1999, 2001 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; SCode rewriting for 6.001
;;; package: (student scode-rewriting)

(declare (usual-integrations))

(define (initialize-package!)
  (set! hook/repl-eval student/repl-eval)
  unspecific)

(define (student/repl-eval repl s-expression environment)
  (repl-scode-eval
   repl
   (rewrite-scode (syntax s-expression environment)
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