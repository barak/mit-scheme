#| -*-Scheme-*-

$Id: stackp.scm,v 1.9 2003/02/14 18:28:01 cph Exp $

Copyright (c) 1987-8, 1991, 1999 Massachusetts Institute of Technology

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

;;;; Primitive Stack Parser

(declare (usual-integrations))

(define (rcd #!optional filename continuation)
  (let ((do-it
	 (lambda ()
	   (write-continuation
	    (if (default-object? continuation)
		(error-continuation)
		continuation)))))
    (if (or (default-object? filename) (not filename))
	(do-it)
	(with-output-to-file filename do-it))))

(define (rcr n #!optional continuation)
  (continuation-ref (if (default-object? continuation)
			(error-continuation)
			continuation)
		    n))

(define (error-continuation)
  (let ((condition (nearest-repl/condition)))
    (if (not condition)
	(error "no error continuation"))
    (condition/continuation condition)))

(define (write-continuation continuation)
  (let write-stack-stream
      ((stream (continuation->stream continuation)) (n 0))
    (if (not (stream-null? stream))
	(begin (if (let ((object (stream-car stream)))
		     (or (return-address? object)
			 (compiled-return-address? object)))
		   (newline))
	       (newline)
	       (write n)
	       (write-string "\t")
	       (let ((string (write-to-string (stream-car stream) 68)))
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