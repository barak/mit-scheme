#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/where.scm,v 14.7 1989/08/07 07:37:09 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Environment Inspector
;;; package: (runtime environment-inspector)

(declare (usual-integrations))

(define (where #!optional environment)
  (let ((environment
	 (if (default-object? environment)
	     (nearest-repl/environment)
	     (->environment environment))))
    (hook/repl-environment (nearest-repl) environment)
    (letter-commands command-set
		     (cmdl-message/standard "Environment Inspector")
		     "Where-->"
		     (make-wstate (list environment)))))

(define-structure (wstate
		   (conc-name wstate/))
  frame-list)

(define (initialize-package!)
  (set! command-set
	(make-command-set
	 'WHERE-COMMANDS
	 `((#\? ,standard-help-command
		"Help, list command letters")
	   (#\Q ,standard-exit-command
		"Quit (exit from Where)")
	   (#\C ,show
		"Display the bindings in the current frame")
	   (#\A ,show-all
		"Display the bindings of all the frames in the current chain")
	   (#\P ,parent
		"Find the parent frame of the current one")
	   (#\S ,son
		"Find the son of the current environment in the current chain")
	   (#\W ,recursive-where
		"Eval an expression in the current frame and do WHERE on it")
	   (#\V ,show-object
		"Eval expression in current frame")
	   (#\E ,enter
		"Create a read-eval-print loop in the current environment")
	   (#\N ,name
		"Name of procedure which created current environment")
	   )))
  unspecific)

(define command-set)

(define (show wstate)
  (show-current-frame wstate false))

(define (show-current-frame wstate brief?)
  (presentation
   (lambda ()
     (let ((frame-list (wstate/frame-list wstate)))
       (show-frame (car frame-list)
		   (length (cdr frame-list))
		   brief?)))))

(define (show-all wstate)
  (show-frames (car (last-pair (wstate/frame-list wstate))) 0))

(define (parent wstate)
  (let ((frame-list (wstate/frame-list wstate)))
    (if (environment-has-parent? (car frame-list))
	(begin
	  (set-wstate/frame-list! wstate
				  (cons (environment-parent (car frame-list))
					frame-list))
	  (show-current-frame wstate true))
	(debugger-failure "The current frame has no parent"))))

(define (son wstate)
  (let ((frames (wstate/frame-list wstate)))
    (if (null? (cdr frames))
	(debugger-failure
	 "This is the original frame; its children cannot be found")
	(begin
	  (set-wstate/frame-list! wstate (cdr frames))
	  (show-current-frame wstate true)))))

(define (name wstate)
  (presentation
   (lambda ()
     (write-string "This frame was created by ")
     (print-user-friendly-name (car (wstate/frame-list wstate))))))

(define (recursive-where wstate)
  (let ((inp (prompt-for-expression "Object to evaluate and examine")))
    (debugger-message "New where!")
    (debug/where (debug/eval inp (car (wstate/frame-list wstate))))))

(define (enter wstate)
  (debug/read-eval-print (car (wstate/frame-list wstate))
			 "You are now in the desired environment"
			 "Eval-in-env-->"))

(define (show-object wstate)
  (debug/read-eval-print-1 (car (wstate/frame-list wstate))))