#| -*-Scheme-*-

$Id: strmac.scm,v 1.2 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1987, 1988, 1989, 1990, 1999 Massachusetts Institute of Technology

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

;;;; Stream Macros

(declare (usual-integrations))

(syntax-table-define system-global-syntax-table 'COLLECT
  (let ()
    (define (collect-macro-kernel result bindings filter)
      (if (null? bindings)
	  (error "COLLECT: No bindings"))
      (parse-bindings bindings
	(lambda (names sets)
	  (define (make-tuple-generator names* sets)
	    (if (null? (cdr names*))
		`(MAP-STREAM (LAMBDA (,(car names*))
			       (LIST ,@names))
			     ,(car sets))
		`(FLATMAP (LAMBDA (,(car names*))
			    ,(make-tuple-generator (cdr names*)
						   (cdr sets)))
			  ,(car sets))))

	  `(MAP-STREAM (SPREAD-TUPLE (LAMBDA ,names ,result))
		       ,(let ((tuple-generator
			       (make-tuple-generator names sets)))
			  (if (null? filter)
			      tuple-generator
			      `(FILTER (SPREAD-TUPLE (LAMBDA ,names ,@filter))
				       ,tuple-generator)))))))

    (define (parse-bindings bindings receiver)
      (if (null? bindings)
	  (receiver '() '())
	  (begin
	   (if (not (pair? bindings))
	       (error "COLLECT: Bindings must be a list" bindings))
	   (parse-bindings (cdr bindings)
	     (lambda (names sets)
	       (if (not (and (list? (car bindings))
			     (= (length (car bindings)) 2)
			     (symbol? (caar bindings))))
		   (error "COLLECT: Badly formed binding" (car bindings)))
	       (receiver (cons (caar bindings) names)
			 (cons (cadar bindings) sets)))))))

    (macro (result bindings . filter)
      (collect-macro-kernel result bindings filter))))
