#| -*-Scheme-*-

$Id: strout.scm,v 14.19 2004/02/16 05:38:49 cph Exp $

Copyright 1988,1990,1993,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2003,2004 Massachusetts Institute of Technology

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

;;;; String Output Ports (SRFI-6)
;;; package: (runtime string-output)

(declare (usual-integrations))

(define (open-output-string)
  (make-port accumulator-output-port-type (make-astate (make-string 128) 0)))

(define (get-output-string port)
  ((port/operation port 'EXTRACT-OUTPUT!) port))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (port)
      (with-output-to-port port thunk))))

(define accumulator-output-port-type)
(define (initialize-package!)
  (set! accumulator-output-port-type
	(make-port-type
	 `((EXTRACT-OUTPUT!
	    ,(lambda (port)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (let ((s (astate-chars state))
			  (n (astate-index state)))
		      (set-astate-chars! state (make-string 128))
		      (set-astate-index! state 0)
		      (set-string-maximum-length! s n)
		      s))))))
	   (WRITE-CHAR
	    ,(lambda (port char)
	       (guarantee-8-bit-char char)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (let* ((n (astate-index state))
			   (n* (fix:+ n 1)))
		      (if (fix:> n* (string-length (astate-chars state)))
			  (grow-accumulator! state n*))
		      (string-set! (astate-chars state) n char)
		      (set-astate-index! state n*)))))
	       1))
	   (WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " to string" output-port)))
	   (WRITE-SUBSTRING
	    ,(lambda (port string start end)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (let* ((n (astate-index state))
			   (n* (fix:+ n (fix:- end start))))
		      (if (fix:> n* (string-length (astate-chars state)))
			  (grow-accumulator! state n*))
		      (substring-move! string start end (astate-chars state) n)
		      (set-astate-index! state n*)))))
	       (fix:- end start))))
	 #f))
  unspecific)

(define-structure (astate (type vector))
  chars
  index)

(define (grow-accumulator! state min-size)
  (let* ((old (astate-chars state))
	 (n (string-length old))
	 (new
	  (make-string
	   (let loop ((n (fix:+ n n)))
	     (if (fix:>= n min-size)
		 n
		 (loop (fix:+ n n)))))))
    (substring-move! old 0 n new 0)
    (set-astate-chars! state new)))