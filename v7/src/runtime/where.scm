#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/where.scm,v 14.6 1989/08/03 23:02:37 cph Exp $

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
(define frame-list)

(define (where #!optional environment)
  (let ((environment
	 (if (default-object? environment)
	     (nearest-repl/environment)
	     (->environment environment))))
    (hook/repl-environment (nearest-repl) environment)
    (fluid-let ((frame-list (list environment)))
      (letter-commands command-set
		       (cmdl-message/standard "Environment Inspector")
		       "Where-->"))))

(define (show)
  (show-current-frame false))

(define (show-current-frame brief?)
  (show-frame (car frame-list) (length (cdr frame-list)) brief?))

(define (show-all)
  (show-frames (car (last-pair frame-list)) 0))

(define (parent)
  (if (environment-has-parent? (car frame-list))
      (begin
	(set! frame-list
	      (cons (environment-parent (car frame-list)) frame-list))
	(show-current-frame true))
      (begin
	(newline)
	(write-string "The current frame has no parent."))))

(define (son)
  (let ((frames frame-list))
    (if (null? (cdr frames))
	(begin
	  (newline)
	  (write-string
	   "This is the original frame.  Its children cannot be found."))
	(begin
	  (set! frame-list (cdr frames))
	  (show-current-frame true)))))

(define (name)
  (newline)
  (write-string "This frame was created by ")
  (print-user-friendly-name (car frame-list)))

(define (recursive-where)
  (let ((inp (prompt-for-expression "Object to eval and examine-> ")))
    (write-string "New where!")
    (debug/where (debug/eval inp (car frame-list)))))

(define (enter)
  (debug/read-eval-print (car frame-list)
			 "You are now in the desired environment"
			 "Eval-in-env-->"))

(define (show-object)
  (debug/read-eval-print-1 (car frame-list)))