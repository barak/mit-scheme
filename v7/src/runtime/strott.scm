#| -*-Scheme-*-

$Id: strott.scm,v 14.5 1999/01/02 06:19:10 cph Exp $

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

;;;; String Output Ports (Truncated)
;;; package: (runtime truncated-string-output)

(declare (usual-integrations))

(define (initialize-package!)
  (set! output-string-template
	(make-output-port `((WRITE-SELF ,operation/write-self)
			    (WRITE-CHAR ,operation/write-char)
			    (WRITE-STRING ,operation/write-string))
			  false)))

(define (with-output-to-truncated-string max thunk)
  (call-with-current-continuation
   (lambda (return)
     (cons false
	   (apply string-append
		  (reverse!
		   (let ((state
			  (make-output-string-state return max '() max)))
		     (with-output-to-port
			 (output-port/copy output-string-template state)
		       thunk)
		     (output-string-state/accumulator state))))))))

(define output-string-template)

(define-structure (output-string-state (type vector)
				       (conc-name output-string-state/))
  (return false read-only true)
  (max-length false read-only true)
  accumulator
  counter)

(define (operation/write-char port char)
  (let ((state (output-port/state port)))
    (let ((accumulator (output-string-state/accumulator state))
	  (counter (output-string-state/counter state)))
      (if (zero? counter)
	  ((output-string-state/return state)
	   (cons true (apply string-append (reverse! accumulator))))
	  (begin
	    (set-output-string-state/accumulator!
	     state
	     (cons (string char) accumulator))
	    (set-output-string-state/counter! state (-1+ counter)))))))

(define (operation/write-string port string)
  (let ((state (output-port/state port)))
    (let ((accumulator (cons string (output-string-state/accumulator state)))
	  (counter
	   (- (output-string-state/counter state) (string-length string))))
      (if (negative? counter)
	  ((output-string-state/return state)
	   (cons true
		 (substring (apply string-append (reverse! accumulator))
			    0
			    (output-string-state/max-length state))))
	  (begin
	    (set-output-string-state/accumulator! state accumulator)
	    (set-output-string-state/counter! state counter))))))

(define (operation/write-self port output-port)
  port
  (write-string " to string (truncating)" output-port))