#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/strott.scm,v 14.3 1988/10/15 17:19:21 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; String Output Ports (Truncated)
;;; package: (runtime truncated-string-output)

(declare (usual-integrations))

(define (initialize-package!)
  (set! output-string-template
	(make-output-port `((PRINT-SELF ,operation/print-self)
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

(define (operation/print-self state port)
  port
  (unparse-string state "to string (truncating)"))