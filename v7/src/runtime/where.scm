#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/where.scm,v 14.3 1988/08/01 23:09:58 cph Exp $

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
	   ))))

(define command-set)

(define env)
(define current-frame)
(define current-frame-depth)

(define (where #!optional environment)
  (let ((environment
	 (if (default-object? environment)
	     (nearest-repl/environment)
	     (->environment environment))))
    (hook/repl-environment (nearest-repl) environment)
    (fluid-let ((env environment)
		(current-frame environment)
		(current-frame-depth 0))
      (letter-commands command-set
		       (cmdl-message/standard "Environment Inspector")
		       "Where-->"))))

;;;; Display Commands

(define (show)
  (show-frame current-frame current-frame-depth))

(define (show-all)
  (let s1 ((env env) (depth 0))
    (if (not (system-global-environment? env))
	(begin (show-frame env depth)
	       (if (environment-has-parent? env)
		   (s1 (environment-parent env) (1+ depth))))))
  *the-non-printing-object*)

;;;; Motion Commands

(define (parent)
  (cond ((environment-has-parent? current-frame)
	 (set! current-frame (environment-parent current-frame))
	 (set! current-frame-depth (1+ current-frame-depth))
	 (show))
	(else
	 (newline)
	 (write-string "The current frame has no parent."))))


(define (son)
  (cond ((eq? current-frame env)
	 (newline)
	 (write-string
	  "This is the original frame.  Its children cannot be found."))
	(else
	 (let son-1 ((prev env)
		     (prev-depth 0)
		     (next (environment-parent env)))
	   (if (eq? next current-frame)
	       (begin (set! current-frame prev)
		      (set! current-frame-depth prev-depth))
	       (son-1 next
		      (1+ prev-depth)
		      (environment-parent next))))
	 (show))))

(define (recursive-where)
  (let ((inp (prompt-for-expression "Object to eval and examine-> ")))
    (write-string "New where!")
    (debug/where (debug/eval inp current-frame))))

;;;; Relative Evaluation Commands

(define (enter)
  (debug/read-eval-print current-frame
			 "You are now in the desired environment"
			 "Eval-in-env-->"))

(define (show-object)
  (debug/read-eval-print-1 current-frame))

;;;; Miscellaneous Commands

(define (name)
  (newline)
  (write-string "This frame was created by ")
  (print-user-friendly-name current-frame))