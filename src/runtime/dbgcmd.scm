#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

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
	   (let ((state (cmdl/state cmdl)))
	     (let ((command-set (vector-ref state 0))
		   (prompt (vector-ref state 1))
		   (state (vector-ref state 2)))
	       (let loop ()
		 (let ((entry
			(assv (char-upcase
			       (prompt-for-command-char (cons 'STANDARD prompt)
							port))
			      (cdr command-set))))
		   (if entry
		       ((cadr entry) state port)
		       (begin
			 (beep port)
			 (newline port)
			 (write-string "Unknown command character" port)
			 (loop))))))))))))
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