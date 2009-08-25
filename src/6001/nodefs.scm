#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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