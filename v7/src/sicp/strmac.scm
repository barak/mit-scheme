#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sicp/strmac.scm,v 1.1 1990/09/10 18:12:49 jinx Exp $

Copyright (c) 1987, 1988, 1989, 1990 Massachusetts Institute of Technology

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
