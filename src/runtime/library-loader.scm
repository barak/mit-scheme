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

;;;; R7RS libraries: loader
;;; package: (runtime library loader)

(declare (usual-integrations))

;;;; Syntax

(define (syntax-r7rs-source source db)
  (register-r7rs-source! source (copy-library-db db))
  (r7rs-source->scode-file source))

(define-automatic-property '(contents bound-names imports-used)
    '(parsed-contents imports exports imports-environment)
  #f
  (lambda (contents imports exports env)
    (receive (body bound free)
	(syntax-library-forms (expand-contents contents) env)
      (let ((exports-from (map library-export-from exports)))
	(if (not (lset<= eq? exports-from (lset-union eq? bound free)))
	    (warn "Library export refers to unbound identifiers:"
		  (lset-difference eq?
				   exports-from
				   (lset-union eq? bound free)))))
      (let ((imports-to (map library-import-to imports)))
	(if (not (lset<= eq? free imports-to))
	    (warn "Library has free references not provided by imports:"
		  (lset-difference eq? free imports-to))))
      (values body
	      bound
	      (filter (lambda (import)
			(memq (library-import-to import) free))
		      imports)))))

(define (expand-contents contents)
  (append-map (lambda (directive)
		(case (car directive)
		  ((include)
		   (parameterize ((param:reader-fold-case? #f))
		     (append-map read-file
				 (cdr directive))))
		  ((include-ci)
		   (parameterize ((param:reader-fold-case? #t))
		     (append-map read-file
				 (cdr directive))))
		  ((begin)
		   (cdr directive))
		  (else
		   (error "Unknown content directive:" directive))))
	      contents))

;;;; Imports environment

(define (import-environment-available? import db)
  (let ((name (library-import-from-library import)))
    (and (registered-library? name db)
	 ((registered-library name db) 'has? 'environment))))

(define (make-environment-from-imports imports db)
  (let ((env
	 (make-root-top-level-environment
	  (delete-duplicates (map library-import-to imports) eq?))))
    (for-each (lambda (import)
		(let ((value
		       ((library-exporter
			 (registered-library
			  (library-import-from-library import)
			  db))
			(library-import-from import)))
		      (name (library-import-to import)))
		  (if (or (not (environment-bound? env name))
			  (let ((value* (environment-safe-lookup env name)))
			    (and (not (values-equivalent? value value*))
				 (or (unassigned-reference-trap? value*)
				     (error "Conflicting imports:"
					    name value* value)))))
		      (cond ((macro-reference-trap? value)
			     (environment-define-macro
			      env name
			      (macro-reference-trap-transformer value)))
			    ((unassigned-reference-trap? value)
			     ;; nothing to do
			     )
			    (else
			     (environment-define env name value))))))
	      imports)
    env))

(define (values-equivalent? v1 v2)
  (cond ((unassigned-reference-trap? v1)
	 (unassigned-reference-trap? v2))
	((macro-reference-trap? v1)
	 (and (macro-reference-trap? v2)
	      (eqv? (macro-reference-trap-transformer v1)
		    (macro-reference-trap-transformer v2))))
	(else (eqv? v1 v2))))

(define-automatic-property 'imports-environment '(imports db)
  (lambda (imports db)
    (every (lambda (import)
	     (import-environment-available? import db))
	   imports))
  make-environment-from-imports)

(define (environment . import-sets)
  (let ((parsed (map parse-import-set import-sets))
	(db host-library-db))
    (let ((imports (expand-parsed-imports parsed db)))
      (let ((unavailable
	     (remove (lambda (import)
		       (import-environment-available? import db))
		     imports)))
	(if (pair? unavailable)
	    (error "Imported libraries unavailable:"
		   (library-imports-from unavailable))))
      (make-environment-from-imports imports db))))

(define (scheme-report-environment version)
  (if (not (eqv? version 5))
      (error "Unsupported version:" version))
  (environment '(scheme r5rs)))

(define (null-environment version)
  (if (not (eqv? version 5))
      (error "Unsupported version:" version))
  (environment '(only (scheme r5rs)
		      ... => _ and begin case cond define define-syntax delay do
		      else if lambda let let* let-syntax letrec letrec-syntax or
		      quasiquote quote set! syntax-rules)))

;;;; Evaluation

(define (eval-r7rs-source source db)
  (let ((program (register-r7rs-source! source db)))
    (if program
	(library-eval-result program))))

(define (eval-r7rs-scode-file scode pathname db)
  (let ((libraries
	 (let ((filename (->namestring pathname)))
	   (map (lambda (library)
		  (scode-library->library library filename))
		(r7rs-scode-file-elements scode)))))
    (register-libraries! libraries db)
    (let loop ((libraries libraries) (result unspecific))
      (if (pair? libraries)
	  (loop (cdr libraries)
		(library-eval-result (car libraries)))
	  result))))

(define-automatic-property '(eval-result environment)
    '(contents imports-environment name)
  #f
  (lambda (contents env name)
    (let ((result (scode-eval contents env)))
      (values (or name result)
	      env))))

(define-automatic-property 'exporter '(exports environment)
  #f
  (lambda (exports environment)
    (let ((export-alist
	   (map (lambda (export)
		  (cons (library-export-to export)
			(environment-safe-lookup environment
						 (library-export-from export))))
		exports)))
      (lambda (name)
	(let ((p (assq name export-alist)))
	  (if (not p)
	      (error "Not an exported name:" name))
	  (cdr p))))))

(define (find-scheme-libraries! pathname)
  (preregister-libraries! pathname (current-library-db)))

(define (preregister-libraries! pathname db)
  (let ((pattern (get-directory-read-pattern pathname)))
    (if pattern
	(let ((root (directory-pathname pattern)))
	  (let loop ((pattern pattern))
	    (receive (files subdirs)
		(find-matching-files scheme-pathname? pattern)
	      (for-each (lambda (group)
			  (preregister-scheme-file! group root db))
			(group-scheme-files files))
	      (for-each (lambda (subdir)
			  (loop (pathname-as-directory subdir)))
			subdirs)))))))

(define (get-directory-read-pattern pathname)
  (case (file-type-direct pathname)
    ((regular)
     (and (or (scheme-pathname? pathname)
	      (not (pathname-type pathname)))
	  (pathname-new-type pathname 'wild)))
    ((directory) (pathname-as-directory pathname))
    (else #f)))

(define (find-matching-files predicate pattern)
  (let loop
      ((pathnames (directory-read pattern #f))
       (files '())
       (directories '()))
    (if (pair? pathnames)
	(let ((pathname (car pathnames))
	      (pathnames (cdr pathnames)))
	  (case (file-type-direct pathname)
	    ((regular)
	     (loop pathnames
		   (if (predicate pathname)
		       (cons pathname files)
		       files)
		   directories))
	    ((directory)
	     (loop pathnames
		   files
		   (if (member (file-namestring pathname) '("." ".."))
		       directories
		       (cons pathname directories))))
	    (else
	     (loop pathnames files directories))))
	(values files directories))))

(define (scheme-pathname? pathname)
  (member (pathname-type pathname) '("scm" "bin" "com")))

(define (group-scheme-files files)
  (let ((table (make-string-hash-table)))
    (for-each (lambda (file)
		(hash-table-update! table
				    (pathname-name file)
				    (lambda (files) (cons file files))
				    (lambda () '())))
	      files)
    (hash-table-values table)))

(define (preregister-scheme-file! file-group root db)

  (define (find-file type)
    (find (lambda (file)
	    (string=? type (pathname-type file)))
	  file-group))

  (define (handle-compiled compiled)
    (let ((scode (fasload compiled)))
      (if (r7rs-scode-file? scode)
	  (finish (map (lambda (scode-library)
			 (scode-library->library scode-library
						 (->namestring compiled)))
		       (r7rs-scode-file-libraries scode))
		  compiled))))

  (define (handle-source source)
    (let ((parsed (read-r7rs-source source)))
      (if parsed
	  (finish (r7rs-source-libraries parsed) source))))

  (define (finish libraries filename)
    (let ((display-filename (enough-namestring filename root)))
      ;; Remove any previously registed libraries from this file.
      (for-each (let ((no-type (pathname-new-type filename #f)))
		  (lambda (library)
		    (if (let ((filename* (library-filename library)))
			  (and filename*
			       (pathname=? (pathname-new-type filename* #f)
					   no-type)))
			(with-notification
			    (lambda (port)
			      (write-string "Deregistering library " port)
			      (write (library-name library) port)
			      (write-string " from \"" port)
			      (write-string
			       (enough-namestring (library-filename library)
						  root)
			       port)
			      (write-string "\"" port))
			  (lambda ()
			    (db 'delete! (library-name library)))))))
		(db 'get-all))
      ;; Now register the current file contents.
      (for-each (lambda (library)
		  (with-notification
		      (lambda (port)
			(write-string "Registering library " port)
			(write (library-name library) port)
			(write-string " from \"" port)
			(write-string display-filename port)
			(write-string "\"" port))
		    (lambda ()
		      (preregister-library! library db))))
		libraries)))

  (let ((compiled (or (find-file "com") (find-file "bin")))
	(source (find-file "scm")))
    (if compiled
	(handle-compiled compiled)
	(begin
	  (if (not source)
	      (error "No scheme files:" file-group))
	  (handle-source source)))))