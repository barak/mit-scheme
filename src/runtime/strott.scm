#| -*-Scheme-*-

$Id: strott.scm,v 14.11 2003/02/14 18:28:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

(define (initialize-package!)
  (set! output-string-port-type
	(make-port-type `((WRITE-SELF ,operation/write-self)
			  (WRITE-CHAR ,operation/write-char)
			  (WRITE-SUBSTRING ,operation/write-substring))
			#f)))

(define (with-output-to-truncated-string max thunk)
  (call-with-current-continuation
   (lambda (return)
     (cons #f
	   (apply string-append
		  (reverse!
		   (let ((state
			  (make-output-string-state return max '() max)))
		     (with-output-to-port
			 (make-port output-string-port-type state)
		       thunk)
		     (output-string-state/accumulator state))))))))

(define output-string-port-type)

(define-structure (output-string-state (type vector)
				       (conc-name output-string-state/))
  (return #f read-only #t)
  (max-length #f read-only #t)
  accumulator
  counter)

(define (operation/write-char port char)
  (let ((state (port/state port)))
    (let ((accumulator (output-string-state/accumulator state))
	  (counter (output-string-state/counter state)))
      (if (zero? counter)
	  ((output-string-state/return state)
	   (cons #t (apply string-append (reverse! accumulator))))
	  (begin
	    (set-output-string-state/accumulator!
	     state
	     (cons (string char) accumulator))
	    (set-output-string-state/counter! state (-1+ counter)))))))

(define (operation/write-substring port string start end)
  (let ((state (port/state port)))
    (let ((accumulator
	   (cons (substring string start end)
		 (output-string-state/accumulator state)))
	  (counter (- (output-string-state/counter state) (- end start))))
      (if (negative? counter)
	  ((output-string-state/return state)
	   (cons #t
		 (substring (apply string-append (reverse! accumulator))
			    0
			    (output-string-state/max-length state))))
	  (begin
	    (set-output-string-state/accumulator! state accumulator)
	    (set-output-string-state/counter! state counter))))))

(define (operation/write-self port output-port)
  port
  (write-string " to string (truncating)" output-port))