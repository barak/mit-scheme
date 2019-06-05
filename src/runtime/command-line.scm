#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Command-line processing
;;; package: (runtime command-line)

(declare (usual-integrations))

(add-boot-init!
 (lambda ()
   (add-event-receiver! event:after-restart process-command-line)))

(define (scheme-program-name)
  (string-from-primitive ((ucode-primitive scheme-program-name 0))))

(define (command-line)
  (map string-from-primitive
       (vector->list ((ucode-primitive get-command-line 0)))))

(define-deferred param:load-init-file?
  (make-settable-parameter #t))

(define *command-line-arguments*)

(define (command-line-arguments)
  *command-line-arguments*)

(define (process-command-line)
  (let ((after-parsing-actions '()))

    (define (process-keyword command-line unused)
      (if (pair? command-line)
	  (let ((keyword (car command-line)))
	    (if (option-keyword? keyword)
		(let ((parser (find-keyword-parser keyword)))
		  (if parser
		      (receive (next tail-action) (parser command-line)
			(if tail-action
			    (set! after-parsing-actions
				  (cons tail-action after-parsing-actions)))
			(process-keyword next unused))
		      (find-next-keyword command-line unused)))
		(begin
		  (warn "Invalid keyword:" keyword)
		  (find-next-keyword command-line unused))))
	  (done unused)))

    (define (find-next-keyword command-line unused)
      (let ((unused (cons (car command-line) unused))
	    (command-line (cdr command-line)))
	(if (pair? command-line)
	    (if (option-keyword? (car command-line))
		(process-keyword command-line unused)
		(find-next-keyword command-line unused))
	    (done unused))))

    (define (done unused)
      (if (pair? unused)
	  (warn "Unhandled command line options:" (reverse unused))))

    (set! *command-line-arguments* '())
    (let ((unused (or ((ucode-primitive get-unused-command-line 0)) '#())))
      (parameterize ((param:load-init-file? #t))
	(process-keyword (map string-from-primitive (vector->list unused)) '())
	(for-each (lambda (act) (act))
		  (reverse after-parsing-actions))
	(if (and (param:load-init-file?)
		 (not (nearest-cmdl/batch-mode?)))
	    (load-init-file))))))

(define (find-keyword-parser keyword)
  (let ((entry (assoc (strip-leading-hyphens keyword) *command-line-parsers*)))
    (and entry
	 (cddr entry))))

(define (option-keyword? argument)
  (and (fix:> (string-length argument) 1)
       (char=? #\- (string-ref argument 0))))

(define (load-init-file)
  (let ((pathname (init-file-pathname)))
    (if pathname
	(load pathname user-initial-environment)))
  unspecific)

(define *command-line-parsers* '())

(define (set-command-line-parser! keyword proc #!optional description)
  (guarantee string? keyword 'set-command-line-parser!)
  (let ((keyword (strip-leading-hyphens keyword))
	(desc (if (default-object? description)
		  ""
		  (begin
		    (guarantee string? description
			       'set-command-line-parser!)
		    description))))

    (let ((place (assoc keyword *command-line-parsers*)))
      (if place
	  (begin
	    (set-car! (cdr place) desc)
	    (set-cdr! (cdr place) proc))
	  (begin
	    (set! *command-line-parsers*
		  (cons (cons* keyword desc proc)
			*command-line-parsers*))
	    unspecific)))))

(define (strip-leading-hyphens keyword)
  (let ((end (string-length keyword)))
    (let loop ((start 0))
      (cond ((and (fix:< start end)
		  (char=? #\- (string-ref keyword start)))
	     (loop (fix:+ start 1)))
	    ((fix:= start 0)
	     keyword)
	    (else
	     (string-slice keyword start end))))))

(define (command-line-option-description keyword-line description-lines caller)
  (if (pair? description-lines)
      (if (and (null? (cdr description-lines))
	       (not (car description-lines)))
	  ""
	  (begin
	    (for-each (lambda (description-line)
			(guarantee string? description-line caller))
		      description-lines)
	    (decorated-string-append "" "\n  " ""
				     (cons keyword-line description-lines))))
      (string-append keyword-line "\n  (No description.)")))

(define (simple-command-line-parser keyword thunk . description-lines)
  (guarantee string? keyword 'simple-command-line-parser)
  (set-command-line-parser! keyword
    (lambda (command-line)
      (values (cdr command-line) thunk))
    (command-line-option-description
     (string-append "--" keyword)
     description-lines
     'simple-command-line-parser)))

(define (argument-command-line-parser keyword multiple? procedure
				      . description-lines)
  (set-command-line-parser! keyword
    (if multiple?
	(lambda (command-line)
	  (for-each-non-keyword (cdr command-line) procedure))
	(lambda (command-line)
	  (if (pair? (cdr command-line))
	      (values (cddr command-line)
		      (lambda () (procedure (cadr command-line))))
	      (values '()
		      (lambda ()
			(warn "Missing argument to command-line option:"
			      (string-append "--" keyword)))))))
    (command-line-option-description
     (string-append "--" keyword " ARG" (if multiple? " ..." ""))
     description-lines
     'argument-command-line-parser)))

(define (for-each-non-keyword command-line processor)
  (let ((end
	 (lambda (command-line accum)
	   (if (pair? accum)
	       (let ((objects (reverse! accum)))
		 (values command-line
			 (lambda () (for-each processor objects))))
	       (values command-line #f)))))
    (let loop ((command-line command-line) (accum '()))
      (if (pair? command-line)
	  (let ((next (car command-line)))
	    (if (option-keyword? next)
		(end command-line accum)
		(loop (cdr command-line) (cons next accum))))
	  (end '() accum)))))

(define (collect-args command-line)

  (define-integrable (end unused args)
    (set! *command-line-arguments*
	  (append! *command-line-arguments* (reverse! args)))
    (values unused #f))

  (let loop ((unused (cdr command-line)) (args '()))
    (if (pair? unused)
	(let ((next (car unused)))
	  (if (option-keyword? next)
	      (end unused args)
	      (loop (cdr unused) (cons next args))))
	(end unused args))))

(define (collect-remaining-args command-line)
  (set! *command-line-arguments*
	(append! *command-line-arguments* (cdr command-line)))
  (values '() #f))

(define (show-command-line-options)
  (write-string "

ADDITIONAL OPTIONS supported by this band:\n")
  (do ((parsers (sort *command-line-parsers*
		      (lambda (a b) (string<? (car a) (car b))))
		(cdr parsers)))
      ((null? parsers))
    (let ((description (cadar parsers)))
      (if (not (fix:= 0 (string-length description)))
	  (begin
	    (newline)
	    (write-string description)
	    (newline)))))
  (exit))

(add-boot-init!
 (lambda ()

   (simple-command-line-parser "no-init-file"
     (lambda ()
       (param:load-init-file? #f))
     "Inhibits automatic loading of the ~/.scheme.init file.")

   (simple-command-line-parser "suspend-file"
     (lambda ()
       (set! generate-suspend-file? #t)
       unspecific)
     "If specified, Scheme saves a band to ~/scheme_suspend on reception"
     "of some signals.  This is unavailable on some operating systems."
     "Under Unix, this is triggered by SIGUSR1 and SIGPWR, and also, if"
     "Scheme is not running under Emacs, SIGHUP.")

   (simple-command-line-parser "no-suspend-file"
     (lambda ()
       (set! generate-suspend-file? #f)
       unspecific)
     "Inhibits automatic saving of bands to ~/scheme_suspend.")

   (argument-command-line-parser "load" #t
     (lambda (arg)
       (run-in-nearest-repl
	(lambda (repl)
	  (parameterize ((param:suppress-loading-message?
			  (cmdl/batch-mode? repl)))
	    (load arg (repl/environment repl))))))
     "Loads the argument files as if in the REPL."
     "In batch mode, loading messages are suppressed.")

   (argument-command-line-parser "eval" #t
     (lambda (arg)
       (run-in-nearest-repl
	(lambda (repl)
	  (let ((environment (repl/environment repl)))
	    (repl-eval/write (read (open-input-string arg))
			     environment
			     repl)))))
     "Evaluates the argument expressions as if in the REPL.")

   (simple-command-line-parser "edit" edit
     "Causes Edwin to start immediately after Scheme.")

   (simple-command-line-parser "help" show-command-line-options #f)

   (simple-command-line-parser "version" (lambda () (exit)) #f)

   (set-command-line-parser! "args" collect-args
     (command-line-option-description
      "--args ARG ..."
      '("Appends ARGs (until the next keyword)"
	"to the list (command-line-arguments).")
      (default-object)))

   (set-command-line-parser! "" collect-remaining-args
     (command-line-option-description
      "-- ARG ..."
      '("Appends all ARGs (until the end of the command-line)"
	"to the list (command-line-arguments).")
      (default-object)))))