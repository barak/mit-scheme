#| -*-Scheme-*-

$Id: strout.scm,v 14.6 1993/01/18 16:50:09 gjr Exp $

Copyright (c) 1988-1993 Massachusetts Institute of Technology

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

(define (grow-accumulator! state min-n*)
  (let* ((old (output-string-state/accumulator state))
	 (n (string-length old))
	 (n* (+ n n))
	 (new (make-string
	       (if (< n* min-n*)
		   min-n*
		   n*))))
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

(define (operation/print-self state port)
  port
  (unparse-string state "to string"))