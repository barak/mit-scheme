;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/autold.scm,v 1.41 1989/04/05 18:11:32 cph Exp $
;;;
;;;	Copyright (c) 1986, 1989 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Autoloads for Edwin

(declare (usual-integrations))

;;;; Definitions

(define (define-autoload-procedure package name library-name)
  (let ((environment (->environment package)))
    (local-assignment environment
		      name
		      (make-autoloading-procedure
		       library-name
		       (lambda () (lexical-reference environment name))))))

(define (make-autoloading-procedure library-name get-procedure)
  (define entity
    (make-entity (lambda arguments
		   (load-library library-name)
		   (let ((procedure (get-procedure)))
		     (set-entity-procedure! entity procedure)
		     (apply procedure (cdr arguments))))
		 (cons autoloading-procedure-tag library-name)))
  entity)

(define autoloading-procedure-tag
  "autoloading-procedure-tag")

(define (autoloading-procedure? object)
  (and (entity? object)
       (eq? autoloading-procedure-tag (car (entity-extra object)))))

(define (define-autoload-major-mode name super-mode-name library-name
	  description)
  (define mode
    (make-mode name
	       true
	       (if super-mode-name
		   (mode-comtabs (name->mode super-mode-name))
		   '())
	       description
	       (make-autoloading-procedure library-name
					   (lambda ()
					     (mode-initialization mode)))))
  mode)

(define (define-autoload-minor-mode name library-name description)
  (define mode
    (make-mode name
	       false
	       '()
	       description
	       (make-autoloading-procedure library-name
					   (lambda ()
					     (mode-initialization mode)))))
  mode)

(define (autoloading-mode? mode)
  (autoloading-procedure? (mode-initialization mode)))

(define (define-autoload-command name library-name description)
  (define command
    (make-command name
		  description
		  (make-autoloading-procedure library-name
					      (lambda ()
						(command-procedure command)))))
  command)

(define (autoloading-command? command)
  (autoloading-procedure? (command-procedure command)))

;;;; Libraries

(define known-libraries
  '())

(define (define-library name . entries)
  (let ((entry (assq name known-libraries)))
    (if entry
	(set-cdr! entry entries)
	(set! known-libraries
	      (cons (cons name entries)
		    known-libraries))))
  name)

(define loaded-libraries
  '())

(define (library-loaded? name)
  (memq name loaded-libraries))

(define library-load-hooks
  '())

(define (add-library-load-hook! name hook)
  (if (library-loaded? name)
      (hook)
      (let ((entry (assq name library-load-hooks)))
	(if entry
	    (append! entry (list hook))
	    (set! library-load-hooks
		  (cons (list name hook)
			library-load-hooks))))))

(define (run-library-load-hooks! name)
  (let ((entry (assq name library-load-hooks)))
    (define (loop)
      (if (null? (cdr entry))
	  (set! library-load-hooks (delq! entry library-load-hooks))
	  (let ((hook (cadr entry)))
	    (set-cdr! entry (cddr entry))
	    (hook)
	    (loop))))
    (if entry (loop))))

(define (load-library name)
  (if (not (library-loaded? name))
      (let ((entry (assq name known-libraries)))
	(if entry
	    (%load-library entry)
	    (error "LOAD-LIBRARY: Unknown library name" name)))))

(define (%load-library library)
  (for-each (lambda (entry)
	      (apply load-edwin-file entry))
	    (cdr library))
  (if (not (memq (car library) loaded-libraries))
      (set! loaded-libraries (cons (car library) loaded-libraries)))
  (run-library-load-hooks! (car library)))

;;;; Loading

(define (load-edwin-file filename package #!optional purify?)
  (let ((pathname
	 (merge-pathnames (->pathname filename) edwin-binary-directory)))    (temporary-message "Loading file \"" (pathname->string pathname) "\"")
    (let ((scode (fasload pathname true)))
      (if (or (default-object? purify?) purify?) (purify scode))
      (scode-eval scode (->environment package))))  (append-message " -- done"))

(define-variable "Load File Default"
  "Pathname given as default for \\[Load File]."
  edwin-binary-directory)

(define-command ("Load File" argument)
  "Load an Edwin binary file.
An argument, if given, means purify the file too."
  (let ((pathname
	 (prompt-for-pathname "Load File" (ref-variable "Load File Default"))))
    (set-variable! "Load File Default" pathname)
    (load-edwin-file pathname '(EDWIN) argument)))

(define-command ("Load Library")
  "Load an Edwin library."
  (%load-library
   (prompt-for-alist-value "Load Library"
			   (map (lambda (library)
				  (cons (symbol->string (car library))
					library))
				known-libraries))))
