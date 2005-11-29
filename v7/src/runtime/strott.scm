#| -*-Scheme-*-

$Id: strott.scm,v 14.13 2005/11/29 06:52:28 cph Exp $

Copyright 1988,1993,1999,2004,2005 Massachusetts Institute of Technology

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

;;;; String Output Ports (Truncated)
;;; package: (runtime truncated-string-output)

(declare (usual-integrations))

(define (with-output-to-truncated-string max thunk)
  (call-with-current-continuation
   (lambda (k)
     (let ((state
	    (make-gstate #f #f 'TEXT k max (make-string (fix:min max 128)) 0)))
       (with-output-to-port (make-port output-string-port-type state)
	 thunk)
       (cons #f
	     (without-interrupts
	      (lambda ()
		(string-head (astate-chars state)
			     (astate-index state)))))))))

(define output-string-port-type)
(define (initialize-package!)
  (set! output-string-port-type
	(make-port-type
	 `((WRITE-CHAR
	    ,(lambda (port char)
	       (guarantee-8-bit-char char)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (let* ((n (astate-index state)))
		      (if (fix:< n (astate-max-length state))
			  (let ((n* (fix:+ n 1)))
			    (if (fix:= n (string-length (astate-chars state)))
				(grow-accumulator! state n*))
			    (string-set! (astate-chars state) n char)
			    (set-astate-index! state n*))
			  ((astate-return state)
			   (cons #t (string-copy (astate-chars state)))))))))
	       1))
	   (WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " to string (truncating)" output-port))))
	 generic-no-i/o-type))
  unspecific)

(define-structure (astate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  (return #f read-only #t)
  (max-length #f read-only #t)
  chars
  index)

(define (grow-accumulator! state min-size)
  (let* ((old (astate-chars state))
	 (n (string-length old))
	 (new
	  (make-string
	   (let loop ((n (fix:+ n n)))
	     (if (fix:>= n min-size)
		 (fix:min n (astate-max-length state))
		 (loop (fix:+ n n)))))))
    (substring-move! old 0 n new 0)
    (set-astate-chars! state new)))