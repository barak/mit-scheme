#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dbgcmd.scm,v 14.2 1988/06/13 11:43:06 cph Exp $

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

;;;; Debugger Command Loop Support
;;; package: (runtime debugger-command-loop)

(declare (usual-integrations))

(define (make-command-set name definitions)
  (let ((command-set (list name)))
    (for-each (lambda (entry)
		(define-letter-command command-set
		  (car entry)
		  (if (eq? standard-help-command (cadr entry))
		      (standard-help-command command-set)
		      (cadr entry))
		  (caddr entry)))
	      definitions)
    command-set))

(define (define-letter-command command-set new-command function help-text)
  (let ((entry (assv new-command (cdr command-set))))
    (if entry
	(set-cdr! entry (list function help-text))
	(let loop ((command-set command-set))
	  (if (or (null? (cdr command-set))
		  (char<? new-command (caadr command-set)))
	      (set-cdr! command-set
			(cons (list new-command function help-text)
			      (cdr command-set)))
	      (loop (cdr command-set)))))))

(define (letter-commands command-set message prompt)
  (with-standard-proceed-point
   (lambda ()
     (push-cmdl letter-commands/driver
		(cons command-set prompt)
		message))))

(define (letter-commands/driver cmdl)
  (let ((command-set (car (cmdl/state cmdl)))
	(prompt (cdr (cmdl/state cmdl))))
    (let loop ()
      (let ((char (char-upcase (prompt-for-command-char prompt cmdl))))
	(with-output-to-port (cmdl/output-port cmdl)
	  (lambda ()
	    (let ((entry (assv char (cdr command-set))))
	      (if entry
		  ((cadr entry))
		  (begin
		    (beep)
		    (newline)
		    (write-string "Unknown command char: ")
		    (write char)
		    (loop)))))))))
  (cmdl-message/null))

(define ((standard-help-command command-set))
  (for-each (lambda (entry)
	      (newline)
	      (write-string "   ")
	      (write-char (car entry))
	      (write-string "   ")
	      (write-string (caddr entry)))
	    (cdr command-set))
  *the-non-printing-object*)

(define (standard-exit-command)  (proceed))

(define (initialize-package!)
  (set! hook/leaving-command-loop default/leaving-command-loop))

(define hook/leaving-command-loop)

(define (leaving-command-loop thunk)
  (hook/leaving-command-loop thunk))

(define (default/leaving-command-loop thunk)
  (thunk))

(define (debug/read-eval-print environment message prompt)
  (leaving-command-loop
   (lambda ()
     (read-eval-print environment (cmdl-message/standard message) prompt))))

(define (debug/eval expression environment)
  (hook/repl-environment (nearest-cmdl) environment)
  (leaving-command-loop
   (lambda ()
     (eval expression environment))))

(define (debug/where environment)
  (leaving-command-loop
   (lambda ()
     (where environment))))