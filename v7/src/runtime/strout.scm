#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/strout.scm,v 14.4 1990/09/13 22:31:59 cph Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; String Output Ports
;;; package: (runtime string-output)

(declare (usual-integrations))

(define (initialize-package!)
  (set! output-string-template
	(make-output-port `((PRINT-SELF ,operation/print-self)
			    (WRITE-CHAR ,operation/write-char)
			    (WRITE-STRING ,operation/write-string))
			  false)))

(define (with-output-to-string thunk)
  (with-string-output-port
   (lambda (port)
     (with-output-to-port port thunk))))

(define (with-string-output-port generator)
  (apply string-append
	 (reverse!
	  (let ((port (output-port/copy output-string-template '())))
	    (generator port)
	    (output-port/state port)))))

(define output-string-template)

(define (operation/write-char port char)
  (set-output-port/state! port (cons (string char) (output-port/state port))))

(define (operation/write-string port string)
  (set-output-port/state! port (cons string (output-port/state port))))

(define (operation/print-self state port)
  port
  (unparse-string state "to string"))