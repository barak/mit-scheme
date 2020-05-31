#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Read-Eval-Print UI
;;; package: (runtime rep ui)

(declare (usual-integrations))

(add-boot-deps! '(runtime rep)
                '(runtime dynamic)
                '(runtime comparator)
                '(runtime string))

(define (rep-escape? form)
  (unquote? form))

(define (handle-rep-escape form env)
  (let ((expr (cadr form)))
    (if (unquote? expr)
	(repl-eval (cadr expr) (->environment '(user)))
	(catch-and-report-errors
	 (lambda ()
	   (cond ((interned-symbol? expr)
		  ((get-handler expr)))
		 ((and (pair? expr)
		       (interned-symbol? (car expr))
		       (list? (cdr expr)))
		  (apply (get-handler (car expr))
			 (map (lambda (arg)
				(if (unquote? arg)
				    (eval (cadr arg) env)
				    arg))
			      (cdr expr))))
		 (else
		  (failure "Unrecognized REPL escape:" form))))))))

(define (unquote? form)
  (and (pair? form)
       (eq? 'unquote (car form))
       (pair? (cdr form))
       (null? (cddr form))))

(define (get-handler name)
  (command-handler (command-ref (choose-command-name name))))

(define (catch-and-report-errors thunk)
  (call-with-current-continuation
    (lambda (k)
      (parameterize ((rep-continuation k))
	(if debug-internal-errors?
	    (thunk)
	    (bind-condition-handler (list condition-type:error)
		(lambda (condition)
		  (with-notification
		      (lambda (port)
			(write-string "Error: " port)
			(write-condition-report condition port)))
		  (k unspecific))
	      thunk))))))

(define debug-internal-errors? #f)
(define-deferred rep-continuation (make-parameter #f))

(define (failure . objects)
  (apply message objects)
  ((rep-continuation) unspecific))

(define (message . objects)
  (with-notification
      (lambda (port)
        (for-each (lambda (object)
                    (display object port))
                  objects))))

(define-record-type <command>
    (make-command name params doc handler)
    command?
  (name command-name)
  (params command-params)
  (doc command-doc)
  (handler command-handler))

(define (define-command name params doc handler)
  (add-boot-init!
   (lambda ()
     (guarantee interned-symbol? name 'define-command)
     (guarantee mit-lambda-list? params 'define-command)
     (guarantee string? doc 'define-command)
     (alist-table-set! all-commands name
		       (make-command name params doc handler)))))

(define (all-command-names)
  (sort (alist-table-keys all-commands) symbol<?))

(define (command-ref name)
  (alist-table-ref all-commands name))

(define-deferred all-commands
  (alist-table eq?))

(define-deferred choose-command-name
  (apropos-chooser "command" all-command-names))

(define ((apropos-chooser noun get-names) candidate)
  (let ((matches (apropos-matches candidate (get-names))))
    (cond ((not (pair? matches))
	   (failure "No matching " noun " for " candidate))
	  ((pair? (cdr matches))
	   (failure "Multiple matching " noun "s for " candidate ": " matches))
	  (else
	   (car matches)))))

(define (apropos-matches to-match candidates)
  (let ((to-match (string to-match)))
    (let ((match
           (find (lambda (candidate)
                   (string=? to-match (string candidate)))
                 candidates)))
      (if match
          (list match)
          (let ((matches
                 (filter (lambda (candidate)
                           (string-prefix? to-match (string candidate)))
                         candidates)))
            (if (null? matches)
                (filter (lambda (candidate)
                          (substring? to-match (string candidate)))
                        candidates)
                matches))))))

(define-command 'help '(#!optional name)
  "Shows all of the commands and their documentation.
If NAME is supplied, only commands matching NAME are shown."
  (lambda (#!optional name)
    (let ((names
           (if (default-object? name)
               (all-command-names)
               (apropos-matches name (all-command-names)))))
      (if (pair? names)
          (show-commands names)
          (message "No commands match " name)))))

(define (show-commands names)
  (with-notification
      (lambda (port)
	(for-each (lambda (name)
		    (let* ((command (command-ref name))
			   (params (command-params command)))
		      (write-char #\, port)
		      (cond ((null? params)
			     (write name port))
			    ((and (pair? params)
				  (eq? #!optional (car params))
				  (pair? (cdr params))
				  (null? (cddr params)))
			     (write name port)
			     (newline port)
			     (write-char #\, port)
			     (write (list name (cadr params)) port))
			    (else
			     (write (cons name params) port)))
		      (newline port)
		      (for-each (lambda (line)
				  (write-indent port)
				  (write-string line port)
				  (newline port))
				(line-splitter (command-doc command)))))
		  names))))

(define-deferred line-splitter
  (string-splitter 'delimiter #\newline
		   'allow-runs? #f))

(define (write-indent port)
  (write-string "    " port))

(define-command 'debug '()
  "Toggles debugging of internal errors on and off."
  (lambda ()
    (set! debug-internal-errors? (not debug-internal-errors?))
    unspecific))

(define (env-mgr)
  (repl/env-mgr (nearest-repl)))

(define (all-env-names)
  (sort ((env-mgr) 'env-names) symbol<?))

(define-deferred choose-env-name
  (apropos-chooser "environment name" all-env-names))

(define (convert-env env)
  (if (default-object? env)
      (make-top-level-environment)
      (->environment env)))

(define-command 'envs '(#!optional name)
  "Shows the current set of environments.
If NAME is given, shows only those environments with matching names.
Otherwise, shows all REPL environments."
  (lambda (#!optional name)
    (with-notification
	(lambda (port)
	  (if (default-object? name)
	      (begin
		(print-stack port)
		(newline port)))
	  (let ((names
		 (if (default-object? name)
		     (all-env-names)
		     (apropos-matches name (all-env-names)))))
	    (if (pair? names)
		(begin
		  (if (default-object? name)
		      (write-string "named envs" port)
		      (begin
			(write-string "named envs matching " port)
			(write name)))
		  (for-each (lambda (name)
			      (newline port)
			      (write-indent port)
			      (print-env ((env-mgr) 'get-named name) port))
			    names))
		(if (default-object? name)
		    (write-string "no named envs" port)
		    (begin
		      (write-string "no env names match " port)
		      (write name)))))))))

(define (show-stack)
  (with-notification print-stack))

(define (print-stack port)
  (write-string "here: " port)
  (print-env ((env-mgr) 'current) port)
  (newline port)
  (let ((stack ((env-mgr) 'get-stack)))
    (if (pair? stack)
	(begin
	  (write-string "stack:" port)
	  (for-each (lambda (env index)
		      (newline port)
		      (write-indent port)
		      (write index port)
		      (write-string ": " port)
		      (print-env env port))
		    stack
		    (iota (length stack))))
	(write-string "The env stack is empty" port))))

(define (print-env env port)
  (cond (((env-mgr) 'name-of env)
	 => (lambda (name)
	      (write name port)
	      (write-string " " port))))
  (cond ((environment->package env)
	 => (lambda (package)
	      (write (package/name package) port)
	      (write-string " " port))))
  (write env port))

(define-command 'name '(name)
  "Assigns NAME to the current REPL environment."
  (lambda (name)
    (if ((env-mgr) 'known-name? name)
	(failure "The name " name " is already assigned."
		 "  Please choose a different name."))
    (let ((name* ((env-mgr) 'name-of ((env-mgr) 'current))))
      (if name*
	  (failure "The REPL environment is already named " name* ".")))
    ((env-mgr) 'name-current! name)))

(define-command 'unname '(#!optional name)
  "Unassigns one or more envnames.
If NAME is provided, that name is unassigned.
Otherwise, all env names are unassigned."
  (lambda (#!optional name)
    (if (default-object? name)
	(begin
	  ((env-mgr) 'clear-names!)
	  (message "all env names have been unassigned"))
	(let ((name (choose-env-name name)))
	  ((env-mgr) 'delete-name! name)
	  (message "env named " name " has been unassigned")))))

(define-command 'push '(#!optional env)
  "Push the REPL env on the env stack and move the REPL to a new env.

If ENV is provided, it is converted to an environment in the usual
way.  The the current REPL env is pushed on the env stack and the REPL
is moved to ENV.

If ENV is not provided, the current REPL env is exchanged with the top
of the env stack."
  (lambda (#!optional env)
    (if (default-object? env)
        (if (not ((env-mgr) 'swap-tos!))
            (failure "The environment stack is empty."))
        ((env-mgr) 'push! (convert-env env)))
    (show-stack)))

(define-command 'pop '()
  "Pops an environment off the stack and moves the REPL there."
  (lambda ()
    (if (not ((env-mgr) 'pop!))
	(failure "The environment stack is empty."))
    (show-stack)))

(define-command 'bury '()
  "Pops an environment off the stack and moves the REPL there.
The current REPL env is saved at the bottom of the stack."
  (lambda ()
    (if (not ((env-mgr) 'bury!))
	(failure "The environment stack is empty."))
    (show-stack)))

(define-command 'ge '(#!optional env)
  "Moves the REPL to ENV without changing the stack.
If ENV is not provided, generates a new top-level environment."
  (lambda (#!optional env)
    ((env-mgr) 'set-current! (convert-env env))))

(define-command 've '(#!optional env)
  "Creates a new child REPL in ENV.
If ENV is not provided, generates a new top-level environment."
  (lambda (#!optional env)
    (read-eval-print (convert-env env) #f 'inherit)))

(define-command 'down '()
  "Creates a new child REPL in the current REPL environment."
  (lambda ()
    (read-eval-print 'inherit #f 'inherit)))

(define-command 'up '()
  "Returns to the parent of this REPL."
  (lambda ()
    (cmdl-interrupt/abort-previous)))

(define-command 'top-level '()
  "Returns to the top-level REPL."
  (lambda ()
    (cmdl-interrupt/abort-top-level)))