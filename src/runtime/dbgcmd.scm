#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(define-record-type <command-set>
    %make-command-set
    command-set?
  (name command-set-name)
  (commands command-set-commands)
  (props command-set-props))

(define (make-command-set name definitions . props)
  (let ((command-set (%make-command-set name (alist-table char=?) props)))
    (for-each (lambda (entry)
		(define-letter-command command-set
		  (car entry)
		  (if (eq? standard-help-command (cadr entry))
		      (standard-help-command command-set)
		      (cadr entry))
		  (caddr entry)))
	      definitions)
    command-set))

(define (define-letter-command command-set letter proc help-text)
  (alist-table-set! (command-set-commands command-set)
		    letter
		    (cons proc help-text)))

(define (command-set-proc command-set letter)
  (alist-table-ref (command-set-commands command-set)
		   letter
		   (lambda () #f)
		   car))

(define (command-set-help-text command-set letter)
  (alist-table-ref (command-set-commands command-set)
		   letter
		   (lambda () #f)
		   cdr))

(define (command-set-letters command-set)
  (sort (alist-table-keys (command-set-commands command-set)) char<?))

(define (command-set-prop command-set keyword default-value)
  (get-keyword-value (command-set-props command-set) keyword default-value))

(define (command-set-immutable-state? command-set)
  (command-set-prop command-set 'immutable-state? #f))

(define (letter-commands command-set message prompt state)
  (cmdl/start (push-cmdl letter-commands/driver
			 (vector command-set prompt state)
			 '())
	      message))

(define debugger:catch-internal-errors? #t)

(define (letter-commands/driver cmdl)
  (if debugger:catch-internal-errors?
      (call-with-current-continuation
       (lambda (continuation)
	 (bind-condition-handler (list condition-type:error)
	     (lambda (condition)
	       (let ((port (cmdl/port cmdl)))
		 (beep port)
		 (fresh-line port)
		 (write-string ";Ignoring error:\n;" port)
		 (write-condition-report condition port))
	       (continuation unspecific))
	   (lambda () (%driver cmdl)))))
      (%driver cmdl))
  (cmdl-message/null))

(define (%driver cmdl)
  (let ((port (cmdl/port cmdl))
	(state (cmdl/state cmdl)))
    (let ((command-set (vector-ref state 0)))
      (let loop ()
	(let ((proc
	       (command-set-proc command-set
				 (char-upcase
				  (prompt-for-command-char
				   (cons 'standard (vector-ref state 1))
				   port)))))
	  (if proc
	      (let ((result (proc (vector-ref state 2) port)))
		(if (command-set-immutable-state? command-set)
		    (vector-set! state 2 result)
		    result))
	      (begin
		(beep port)
		(newline port)
		(write-string "Unknown command character" port)
		(loop))))))))


(define ((standard-help-command command-set) state port)
  (for-each (lambda (letter)
	      (newline port)
	      (write-string "   " port)
	      (write-char letter port)
	      (write-string "   " port)
	      (write-string (command-set-help-text command-set letter) port))
	    (command-set-letters command-set))
  state)

(define (standard-exit-command state port)
  (continue)
  (debugger-failure port "Can't exit; use a restart command instead.")
  state)

(define (leaving-command-loop thunk)
  (hook/leaving-command-loop thunk))

(define (default/leaving-command-loop thunk)
  (thunk))

(define hook/leaving-command-loop
  default/leaving-command-loop)

(define (debug/read-eval-print environment from to)
  (leaving-command-loop
   (lambda ()
     (with-simple-restart 'continue
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