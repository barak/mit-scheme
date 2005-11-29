#| -*-Scheme-*-

$Id: strout.scm,v 14.21 2005/11/29 06:54:11 cph Exp $

Copyright 1988,1990,1993,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2003,2004,2005 Massachusetts Institute of Technology

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
  (make-port accumulator-output-port-type
	     (make-gstate #f #f 'TEXT #f #f)))

(define (get-output-string port)
  ((port/operation port 'EXTRACT-OUTPUT) port))

(define (get-output-string! port)
  ((port/operation port 'EXTRACT-OUTPUT!) port))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define (with-output-to-string thunk)
  (call-with-output-string
    (lambda (port)
      (with-output-to-port port thunk))))

(define-structure (astate (type vector)
			  (initial-offset 4) ;must match "genio.scm"
			  (constructor #f))
  chars
  index)

(define (maybe-reset-astate state)
  (if (not (astate-chars state))
      (begin
	(set-astate-chars! state (make-string 128))
	(set-astate-index! state 0))))

(define (maybe-grow-accumulator! state min-size)
  (if (fix:> min-size (string-length (astate-chars state)))
      (let* ((old (astate-chars state))
	     (n (string-length old))
	     (new
	      (make-string
	       (let loop ((n (fix:+ n n)))
		 (if (fix:>= n min-size)
		     n
		     (loop (fix:+ n n)))))))
	(substring-move! old 0 n new 0)
	(set-astate-chars! state new))))

(define accumulator-output-port-type)
(define (initialize-package!)
  (set! accumulator-output-port-type
	(make-port-type
	 `((EXTRACT-OUTPUT
	    ,(lambda (port)
	       (let ((state (port/state port)))
		 (if (astate-chars state)
		     (string-head (astate-chars state)
				  (astate-index state))
		     (make-string 0)))))
	   (EXTRACT-OUTPUT!
	    ,(lambda (port)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (let ((s (astate-chars state)))
		      (if s
			  (begin
			    (set-astate-chars! state #f)
			    (set-string-maximum-length! s (astate-index state))
			    s)
			  (make-string 0))))))))
	   (WRITE-CHAR
	    ,(lambda (port char)
	       (guarantee-8-bit-char char)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (maybe-reset-astate state)
		    (let* ((n (astate-index state))
			   (n* (fix:+ n 1)))
		      (maybe-grow-accumulator! state n*)
		      (string-set! (astate-chars state) n char)
		      (set-astate-index! state n*)))))
	       1))
	   (WRITE-SUBSTRING
	    ,(lambda (port string start end)
	       (let ((state (port/state port)))
		 (without-interrupts
		  (lambda ()
		    (maybe-reset-astate state)
		    (let* ((n (astate-index state))
			   (n* (fix:+ n (fix:- end start))))
		      (maybe-grow-accumulator! state n*)
		      (substring-move! string start end (astate-chars state) n)
		      (set-astate-index! state n*)))))
	       (fix:- end start)))
	   (WRITE-SELF
	    ,(lambda (port output-port)
	       port
	       (write-string " to string" output-port))))
	 generic-no-i/o-type))
  unspecific)