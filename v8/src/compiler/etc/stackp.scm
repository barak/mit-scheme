#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/compiler/etc/stackp.scm,v 1.2 1988/10/26 04:14:53 cph Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

;;;; Primitive Stack Parser

(declare (usual-integrations))

(define (rcd #!optional filename continuation)
  (let ((do-it
	 (lambda ()
	   (write-continuation
	    (if (default-object? continuation)
		(error-continuation)
		continuation)))))
    (if (default-object? filename)
	(do-it)
	(with-output-to-file filename do-it))))

(define (rcr n #!optional continuation)
  (continuation-ref (if (default-object? continuation)
			(error-continuation)
			continuation)
		    n))

(define (write-continuation continuation)
  (let write-stack-stream
      ((stream (continuation->stream continuation)) (n 0))
    (if (not (stream-null? stream))
	(begin (if (return-address? (stream-car stream))
		   (newline))
	       (newline)
	       (write n)
	       (write-string "\t")
	       (let ((string (write-to-string (stream-car stream) 60)))
		 (write-string (cdr string))
		 (if (car string)
		     (write-string "...")))
	       (write-stack-stream (tail stream) (1+ n)))))
  unspecific)

(define (continuation-ref continuation n)
  (stream-ref (continuation->stream continuation) n))

(define (continuation->stream continuation)
  (let stack-frame->stream ((frame (continuation->stack-frame continuation)))
    (let ((length (stack-frame/length frame)))
      (let loop ((n 0))
	(if (< n length)
	    (cons-stream (stack-frame/ref frame n) (loop (1+ n)))
	    (let ((next (stack-frame/next frame)))
	      (if next
		  (stack-frame->stream next)
		  (stream))))))))