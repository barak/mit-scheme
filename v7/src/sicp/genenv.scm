#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sicp/genenv.scm,v 1.2 1990/11/14 14:57:50 cph Exp $

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

;;;; Environment hacking for 6.001

(declare (usual-integrations))

(define build-environment)

(define make-unassigned-object
  microcode-object/unassigned)

(let ()
  (define (get-values descriptors frame receiver)
    (define (inner descriptors names values unref)
      (define (do-next name-here name-there)
	(if (or (not (symbol? name-there))
		(lexical-unreferenceable? frame name-there))
	    (inner (cdr descriptors)
		   (cons name-here names)
		   (cons (make-unassigned-object)
			 values)
		   (if (not (symbol? name-there))
		       unref
		       (cons name-here unref)))
	    (inner (cdr descriptors)
		   (cons name-here names)
		   (cons (lexical-reference frame name-there)
			 values)
		   unref)))

      (if (null? descriptors)
	  (receiver (reverse! names)
		    (reverse! values)
		    (reverse! unref))
	  (let ((this (car descriptors)))
	    (cond ((not (pair? this))
		   (do-next this this))
		  ((null? (cdr this))
		   (do-next (car this) (car this)))
		  (else
		   (do-next (car this) (cdr this)))))))
    (inner descriptors '() '() '()))

  (set! build-environment
	(named-lambda (build-environment names source-frame
					 #!optional parent-frame
					 process receiver)
	  (get-values names source-frame
	    (lambda (names values unreferenceable)
	      (if (default-object? receiver)
		  unreferenceable
		  (receiver
		   (apply (scode-eval (make-lambda lambda-tag:make-environment
						   names
						   '()
						   '()
						   '()
						   '()
						   (make-the-environment))
				      (if (default-object? parent-frame)
					  source-frame
					  parent-frame))
			  (map (if (default-object? process)
				   unmap-reference-trap
				   (lambda (x)
				     (unmap-reference-trap (process x))))
			       values))
		   unreferenceable))))))
  42)