#| -*-Scheme-*-

$Id: strout.scm,v 14.9 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; String Output Ports
;;; package: (runtime string-output)

(declare (usual-integrations))

(define (initialize-package!)
  (set! output-string-template
	(make-output-port `((WRITE-SELF ,operation/write-self)
			    (WRITE-CHAR ,operation/write-char)
			    (WRITE-SUBSTRING ,operation/write-substring))
			  false))
  unspecific)

(define (with-output-to-string thunk)
  (with-string-output-port
   (lambda (port)
     (with-output-to-port port thunk))))

(define (with-string-output-port generator)
  (let ((state (make-output-string-state (make-string 16) 0)))
    (let ((port (output-port/copy output-string-template state)))
      (generator port)
      (without-interrupts
       (lambda ()
	 (string-head (output-string-state/accumulator state)
		      (output-string-state/counter state)))))))

(define output-string-template)

(define-structure (output-string-state (type vector)
				       (conc-name output-string-state/))
  accumulator
  counter)

(define (grow-accumulator! state min-size)
  (let* ((old (output-string-state/accumulator state))
	 (n (string-length old))
	 (new
	  (make-string
	   (let loop ((n (fix:+ n n)))
	     (if (fix:>= n min-size)
		 n
		 (loop (fix:+ n n)))))))
    (substring-move-left! old 0 n new 0)
    (set-output-string-state/accumulator! state new)))

(define (operation/write-char port char)
  (without-interrupts
   (lambda ()
     (let* ((state (output-port/state port))
	    (n (output-string-state/counter state))
	    (n* (fix:+ n 1)))
       (if (fix:= (string-length (output-string-state/accumulator state)) n)
	   (grow-accumulator! state n*))
       (string-set! (output-string-state/accumulator state) n char)
       (set-output-string-state/counter! state n*)))))

(define (operation/write-substring port string start end)
  (without-interrupts
   (lambda ()
     (let* ((state (output-port/state port))
	    (n (output-string-state/counter state))
	    (n* (fix:+ n (fix:- end start))))
       (if (fix:> n* (string-length (output-string-state/accumulator state)))
	   (grow-accumulator! state n*))
       (substring-move-left! string start end
			     (output-string-state/accumulator state) n)
       (set-output-string-state/counter! state n*)))))

(define (operation/write-self port output-port)
  port
  (write-string " to string" output-port))