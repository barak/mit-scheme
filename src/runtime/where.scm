#| -*-Scheme-*-

$Id: where.scm,v 14.16 2007/01/05 21:19:28 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Environment Inspector
;;; package: (runtime environment-inspector)

(declare (usual-integrations))

(define (where #!optional environment)
  (with-simple-restart 'CONTINUE "Return from WHERE."
    (lambda ()
      (let ((wstate
	     (make-wstate
	      (list
	       (if (default-object? environment)
		   (nearest-repl/environment)
		   (->environment environment))))))
	(letter-commands
	 command-set
	 (cmdl-message/active
	  (lambda (port)
	    (show-current-frame wstate #t port)
	    (debugger-message
	     port
	     "You are now in the environment inspector.  Type q to quit, ? for commands.")))
	 "where>"
	 wstate)))))

(define-structure (wstate
		   (conc-name wstate/))
  frame-list)

(define (initialize-package!)
  (set!
   command-set
   (make-command-set
    'WHERE-COMMANDS
    `((#\? ,standard-help-command
	   "help, list command letters")
      (#\A ,show-all
	   "show All bindings in current environment and its ancestors")
      (#\C ,show
	   "show bindings of identifiers in the Current environment")
      (#\E ,enter
	   "Enter a read-eval-print loop in the current environment")
      (#\O ,command/print-environment-procedure
	   "pretty print the procedure that created the current environment")
      (#\P ,parent
	   "move to environment that is Parent of current environment")
      (#\Q ,standard-exit-command
	   "Quit (exit environment inspector)")
      (#\S ,son
	   "move to child of current environment (in current chain)")
      (#\V ,show-object
	   "eValuate expression in current environment")
      (#\W ,recursive-where
	   "enter environment inspector (Where) on the current environment")
      )))
  unspecific)

(define command-set)

(define (show wstate port)
  (show-current-frame wstate #f port))

(define (show-current-frame wstate brief? port)
  (debugger-presentation port
    (lambda ()
      (let ((frame-list (wstate/frame-list wstate)))
	(show-frame (car frame-list)
		    (length (cdr frame-list))
		    brief?
		    port)))))

(define (show-all wstate port)
  (show-frames (car (last-pair (wstate/frame-list wstate))) 0 port))

(define (parent wstate port)
  (let ((frame-list (wstate/frame-list wstate)))
    (if (eq? #t (environment-has-parent? (car frame-list)))
	(begin
	  (set-wstate/frame-list! wstate
				  (cons (environment-parent (car frame-list))
					frame-list))
	  (show-current-frame wstate #t port))
	(debugger-failure port "The current frame has no parent"))))

(define (son wstate port)
  (let ((frames (wstate/frame-list wstate)))
    (if (pair? (cdr frames))
	(begin
	  (set-wstate/frame-list! wstate (cdr frames))
	  (show-current-frame wstate #t port))
	(debugger-failure
	 port
	 "This is the original frame; its children cannot be found"))))

(define (command/print-environment-procedure wstate port)
  (show-environment-procedure (car (wstate/frame-list wstate)) port))

(define (recursive-where wstate port)
  (let ((environment (car (wstate/frame-list wstate))))
    (let ((inp
	   (prompt-for-expression "Object to evaluate and examine"
				  port
				  environment)))
      (debugger-message port "New where!")
      (debug/where (debug/eval inp environment)))))

(define (enter wstate port)
  port
  (debug/read-eval-print (car (wstate/frame-list wstate))
			 "the environment inspector"
			 "the environment for this frame"))

(define (show-object wstate port)
  (debug/read-eval-print-1 (car (wstate/frame-list wstate)) port))