#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/dbgcmd.scm,v 14.13 1991/11/26 07:05:04 cph Exp $

Copyright (c) 1988-91 Massachusetts Institute of Technology

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

(define (letter-commands command-set message prompt state)
  (cmdl/start (push-cmdl letter-commands/driver
			 (vector command-set prompt state)
			 '())
	      message))

(define (letter-commands/driver cmdl)
  (call-with-current-continuation
   (lambda (continuation)
     (let ((port (cmdl/port cmdl)))
       (bind-condition-handler (list condition-type:error)
	   (lambda (condition)
	     (beep port)
	     (fresh-line port)
	     (write-string ";Ignoring error:\n;" port)
	     (write-condition-report condition port)
	     (continuation unspecific))
	 (lambda ()
	   (let ((command-set (vector-ref (cmdl/state cmdl) 0))
		 (prompt
		  (string-append (number->string (cmdl/level cmdl))
				 " "
				 (vector-ref (cmdl/state cmdl) 1)))
		 (state (vector-ref (cmdl/state cmdl) 2)))
	     (let loop ()
	       (let ((entry
		      (assv (char-upcase (prompt-for-command-char prompt port))
			    (cdr command-set))))
		 (if entry
		     ((cadr entry) state port)
		     (begin
		       (beep port)
		       (newline port)
		       (write-string "Unknown command character" port)
		       (loop)))))))))))
  (cmdl-message/null))

(define ((standard-help-command command-set) state port)
  state					;ignore
  (for-each (lambda (entry)
	      (newline port)
	      (write-string "   " port)
	      (write-char (car entry) port)
	      (write-string "   " port)
	      (write-string (caddr entry) port))
	    (cdr command-set))
  unspecific)

(define (standard-exit-command state port)
  state					;ignore
  (continue)
  (debugger-failure port "Can't exit; use a restart command instead."))

(define (initialize-package!)
  (set! hook/leaving-command-loop default/leaving-command-loop)
  unspecific)

(define (leaving-command-loop thunk)
  (hook/leaving-command-loop thunk))

(define hook/leaving-command-loop)
(define (default/leaving-command-loop thunk)
  (thunk))

(define (debug/read-eval-print environment from to)
  (leaving-command-loop
   (lambda ()
     (with-simple-restart 'CONTINUE
	 (lambda (port)
	   (write-string "Return to " port)
	   (write-string from port)
	   (write-string "." port))
       (lambda ()
	 (read-eval-print
	  environment
	  (cmdl-message/strings
	   (string-append "You are now in " to ".")
	   (string-append "Type C-c C-u to return to " from "."))
	  user-initial-prompt))))))

(define (debug/eval expression environment)
  (leaving-command-loop
   (lambda ()
     (eval expression environment))))

(define (debug/scode-eval expression environment)
  (leaving-command-loop
   (lambda ()
     (extended-scode-eval expression environment))))

(define (debug/where environment)
  (leaving-command-loop
   (lambda ()
     (where environment))))