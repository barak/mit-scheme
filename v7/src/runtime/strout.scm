#| -*-Scheme-*-

$Id: strout.scm,v 14.18 2003/02/14 18:28:34 cph Exp $

Copyright 1988,1990,1993,1999,2000,2001 Massachusetts Institute of Technology
Copyright 2003 Massachusetts Institute of Technology

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
	     (make-accumulator-state (make-string 16) 0)))

(define (get-output-string port)
  ((port/operation port 'EXTRACT-OUTPUT!) port))

(define (with-output-to-string thunk)
  (call-with-output-string (lambda (port) (with-output-to-port port thunk))))

(define (call-with-output-string generator)
  (let ((port (open-output-string)))
    (generator port)
    (get-output-string port)))

(define accumulator-output-port-type)
(define (initialize-package!)
  (set! accumulator-output-port-type
	(make-port-type `((WRITE-SELF ,operation/write-self)
			  (WRITE-CHAR ,operation/write-char)
			  (WRITE-SUBSTRING ,operation/write-substring)
			  (EXTRACT-OUTPUT! ,operation/extract-output!))
			#f))
  unspecific)

(define (operation/write-self port output-port)
  port
  (write-string " to string" output-port))

(define (operation/write-char port char)
  (without-interrupts
   (lambda ()
     (let* ((state (port/state port))
	    (n (accumulator-state-counter state))
	    (n* (fix:+ n 1)))
       (if (fix:= n (string-length (accumulator-state-accumulator state)))
	   (grow-accumulator! state n*))
       (string-set! (accumulator-state-accumulator state) n char)
       (set-accumulator-state-counter! state n*)))))

(define (operation/write-substring port string start end)
  (without-interrupts
   (lambda ()
     (let* ((state (port/state port))
	    (n (accumulator-state-counter state))
	    (n* (fix:+ n (fix:- end start))))
       (if (fix:> n* (string-length (accumulator-state-accumulator state)))
	   (grow-accumulator! state n*))
       (substring-move! string start end
			(accumulator-state-accumulator state) n)
       (set-accumulator-state-counter! state n*)))))

(define (operation/extract-output! port)
  (without-interrupts
   (lambda ()
     (let ((state (port/state port)))
       (let ((s (accumulator-state-accumulator state))
	     (n (accumulator-state-counter state)))
	 (set-accumulator-state-accumulator! state (make-string 16))
	 (set-accumulator-state-counter! state 0)
	 (set-string-maximum-length! s n)
	 s)))))

(define-structure (accumulator-state (type vector))
  accumulator
  counter)

(define (grow-accumulator! state min-size)
  (let* ((old (accumulator-state-accumulator state))
	 (n (string-length old))
	 (new
	  (make-string
	   (let loop ((n (fix:+ n n)))
	     (if (fix:>= n min-size)
		 n
		 (loop (fix:+ n n)))))))
    (substring-move! old 0 n new 0)
    (set-accumulator-state-accumulator! state new)))