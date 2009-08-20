#| -*-Scheme-*-

$Id$

Copyright (c) 1987-8, 1991, 1999 Massachusetts Institute of Technology

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