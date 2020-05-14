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

(add-boot-deps! '(runtime string))

;;;; Syntax

(define (syntax-r7rs-source source db)
  (register-r7rs-source! source (copy-library-db db))
  (r7rs-source->scode-file source))

(define-automatic-property '(contents bound-names free-names)
    '(parsed-contents imports-environment)
  #f
  (lambda (parsed-contents env)
    (syntax-library-forms (expand-contents parsed-contents) env)))

(define-automatic-property 'imports-used
    '(imports exports free-names bound-names)
  #f
  (lambda (imports exports free-names bound-names)
    (let ((imports-to
	   (lset-difference eq?
			    (map library-ixport-to imports)
			    bound-names)))
      (let ((missing (lset-difference eq? free-names imports-to)))
	(if (pair? missing)
	    (warn "Library has free references not provided by imports:"
		  missing)))
      (let ((used
	     (lset-intersection eq?
				imports-to
				(lset-union eq?
					    free-names
					    (map library-ixport-from
						 exports)))))
	(filter (lambda (import)
		  (memq (library-ixport-to import) used))
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

(define (environment-available? import db)
  (let ((name (library-ixport-from-library import)))
    (and (registered-library? name db)
	 (library-has? 'environment (registered-library name db)))))

(define (make-environment-from-imports imports db)
  (let ((env
	 (make-root-top-level-environment
	  (delete-duplicates (map library-ixport-to imports) eq?))))
    (add-imports-to-env! imports env db)
    env))

(define (add-imports-to-env! imports env db)
  (let ((grouped
	 (let ((table (make-strong-eq-hash-table)))
	   (for-each (lambda (import)
		       (let-values (((senv sname)
				     (library-import-source import db)))
			 (hash-table-update! table (library-ixport-to import)
			   (lambda (sources) (cons (cons senv sname) sources))
			   (lambda () '()))))
		     imports)
	   (hash-table->alist table))))
    (let ((conflicts
	   (remove (lambda (p)
		     (let ((source (cadr p)))
		       (every (lambda (source*)
				(and (eq? (car source) (car source*))
				     (eq? (cdr source) (cdr source*))))
			      (cddr p))))
		   grouped)))
      (if (pair? conflicts)
	  (error "Conflicting imports:" conflicts)))
    (let-values (((overrides other)
		  (partition (let ((existing (environment-bindings env)))
			       (lambda (p)
				 (let ((binding (assq (car p) existing)))
				   (and binding
					(not (null? (cdr binding)))))))
			     grouped)))
      (if (pair? overrides)
	  (warn "Not overriding names with imports:" (map car overrides)))
      (for-each (lambda (p)
		  (let ((tname (car p))
			(source (cadr p)))
		    (let ((value
			   (environment-safe-lookup (car source) (cdr source))))
		      (cond ((macro-reference-trap? value)
			     (environment-define-macro
			      env tname
			      (macro-reference-trap-transformer value)))
			    ((unassigned-reference-trap? value)
			     ;; nothing to do
			     )
			    (else
			     (environment-define env tname value))))))
		other))))

(define (library-import-source import db)
  (let ((name (library-ixport-from import))
	(library (registered-library (library-ixport-from-library import) db)))
    (let ((export
	   (find (lambda (export)
		   (eq? name (library-ixport-to export)))
		 (library-exports library))))
      (if (not export)
	  (error "Not an exported name:" name))
      (values (library-environment library)
	      (library-ixport-from export)))))

(define-automatic-property 'imports-environment '(imports db)
  (lambda (imports db)
    (every (lambda (import)
	     (environment-available? import db))
	   imports))
  make-environment-from-imports)

(define (environment . import-sets)
  (let ((db (current-library-db)))
    (make-environment-from-imports (import-sets->imports import-sets db)
				   db)))

(define (import! import-set #!optional env)
  (let ((db (current-library-db)))
    (add-imports-to-env! (import-sets->imports (list import-set) db)
			 (if (default-object? env)
			     (nearest-repl/environment)
			     (guarantee environment? env 'import!))
			 db)))

(define (import-sets->imports import-sets db)
  (parsed-imports->imports (map parse-import-set import-sets) db))

(define (make-environment-from-parsed-imports parsed-imports)
  (let ((db (current-library-db)))
    (make-environment-from-imports (parsed-imports->imports parsed-imports db)
				   db)))

(define (parsed-imports->imports parsed-imports db)
  (let ((imports (expand-parsed-imports parsed-imports db)))
    (maybe-load-libraries! imports db)
    (let ((unavailable
	   (remove (lambda (import)
		     (environment-available? import db))
		   imports)))
      (if (pair? unavailable)
	  (error "Imported libraries unavailable:"
		 (library-ixports->library-names unavailable))))
    imports))

(define (maybe-load-libraries! imports db)
  (let ((libraries (library-ixports->libraries imports db)))
    (if (any library-preregistered? libraries)
	(for-each load-preregistered-library!
		  (reverse
		   (filter library-preregistered?
			   ((compute-dependency-graph libraries db)
			    'topological-sort)))))))

(define (compute-dependency-graph libraries db)
  (let ((table (make-key-weak-eq-hash-table)))

    (define (trace library)
      (if (not (hash-table-exists? table library))
	  (let ((deps
		 (library-ixports->libraries (library-imports library) db)))
	    (hash-table-set! table library deps)
	    (for-each trace deps))))

    (for-each trace libraries)
    (make-digraph (hash-table-keys table)
		  (lambda (library) (hash-table-ref table library)))))

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

(define (eval-r7rs-source source)
  (let ((program (register-r7rs-source! source (current-library-db))))
    (if program
	(library-eval-result program))))

(define (eval-r7rs-scode-file scode pathname)
  (fold (lambda (library result)
	  (declare (ignore result))
	  (library-eval-result library))
	unspecific
	(let ((filename (->namestring pathname))
	      (db (current-library-db)))
	  (map (lambda (library)
		 (register-library! (scode-library->library library filename)
				    db))
	       (r7rs-scode-file-elements scode)))))

(define-automatic-property '(eval-result environment)
    '(contents imports-environment name)
  #f
  (lambda (contents env name)
    (let ((result (scode-eval contents env)))
      (values (or name result)
	      env))))

;;;; Preregistration

(define (preregister-standard-libraries!)
  (parameterize ((param:hide-notifications? #t))
    (let ((pn (system-library-directory-pathname "libraries")))
      (if pn
	  (find-scheme-libraries! pn)))
    (let ((pn (init-file-specifier->pathname '("libraries"))))
      (if (file-directory? pn)
	  (find-scheme-libraries! (pathname-as-directory pn))))))
(add-event-receiver! event:after-restore preregister-standard-libraries!)

(define (find-scheme-libraries! pathname)
    (let ((pattern (get-directory-read-pattern pathname)))
    (if pattern
	(let ((root (directory-pathname pattern)))
	  (let loop ((pattern pattern))
	    (receive (files subdirs)
		(find-matching-files scheme-pathname? pattern)
	      (let ((groups (group-scheme-files files))
		    (db (current-library-db)))
		(for-each (lambda (name)
			    (preregister-scheme-file! (groups name) root db))
			  (groups)))
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
  (let ((type (pathname-type pathname)))
    (or (equal? (file-type-src file-types:library) type)
	(equal? (file-type-bin file-types:library) type)
	(equal? (file-type-com file-types:library) type)
	(equal? (file-type-src file-types:program) type)
	(equal? (file-type-bin file-types:program) type)
	(equal? (file-type-com file-types:program) type))))

(define-deferred group-scheme-files
  (partition-generator pathname-name string=? cons '()))

(define (preregister-scheme-file! file-group root db)

  (define (try-types types fail)
    (let ((com (find-file (file-type-com types)))
	  (bin (find-file (file-type-bin types)))
	  (src (find-file (file-type-src types))))
      (if (or com bin src)
	  (try-scode com
	    (lambda ()
	      (try-scode bin
		(lambda ()
		  (try-source src
		    (lambda () #f))))))
	  (fail))))

  (define (find-file type)
    (find (lambda (file)
	    (string=? type (pathname-type file)))
	  file-group))

  (define (try-scode file fail)
    (if file
	(let ((scode (fasload file)))
	  (if (r7rs-scode-file? scode)
	      (let ((libs (r7rs-scode-file-libraries scode)))
		(if (every scode-library-version-current? libs)
		    (succeed (let ((ns (->namestring file)))
			       (map (lambda (lib)
				      (scode-library->library lib ns))
				    libs))
			     file)
		    (begin
		      (warn "Compiled library has old version:"
			    (scode-library-version (car libs))
			    file)
		      (fail))))
	      (fail)))
	(fail)))

  (define (try-source file fail)
    (if file
	(let ((parsed (read-r7rs-source file)))
	  (if parsed
	      (succeed (r7rs-source-libraries parsed) file)
	      (fail)))
	(fail)))

  (define (succeed libs file)
    (preregister-libraries! libs file root db))

  (try-types file-types:library
    (lambda ()
      (try-types file-types:program
	(lambda () #f)))))

(define (preregister-libraries! libraries filename root db)
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
			  (deregister-library! library db))))))
	      (registered-libraries db))
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