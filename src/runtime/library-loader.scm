#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;; Returns one of the following:
;; * Zero or more libraries, one or more imports, and a body.
;; * Zero or more libraries, no imports, and no body.
;; * #F, meaning this isn't R7RS source.
(define (read-r7rs-source pathname)
  (parameterize ((param:reader-fold-case? #f))
    (call-with-input-file pathname
      (lambda (port)

	(define (read-libs libs)
	  (let ((form (read port)))
	    (cond ((eof-object? form)
		   (make-r7rs-source (reverse libs) '() #f))
		  ((r7rs-library? form)
		   (read-libs
		    (cons (parse-define-library-form form pathname)
			  libs)))
		  ((r7rs-import? form)
		   (read-imports (list (parse-import-form form))
				 (reverse libs)))
		  ;; Not a valid R7RS file.
		  (else #f))))

	(define (read-imports imports libs)
	  (let ((form (read port)))
	    (if (eof-object? form)
		(error "EOF while reading imports"))
	    (if (r7rs-library? form)
		(error "Can't mix libraries and imports:" form))
	    (if (r7rs-import? form)
		(read-imports (cons (parse-import-form form) imports) libs)
		(make-r7rs-source libs
				  (append-map cdr (reverse imports))
				  (read-body (list form))))))

	(define (read-body forms)
	  (let ((form (read port)))
	    (if (eof-object? form)
		(reverse forms)
		(read-body (cons form forms)))))

	(read-libs '())))))

(define (r7rs-library? object)
  (and (pair? object)
       (eq? 'define-library (car object))))

(define (r7rs-import? object)
  (and (pair? object)
       (eq? 'import (car object))))

(define (make-r7rs-source libraries imports body)

  (define (save-metadata! library-db)
    ;; TODO: adjust expansion order due to dependencies.
    (for-each
     (lambda (library)
       (library-db 'save-metadata!
		   (parsed-library->metadata library library-db)))
     libraries))

  (define (load library-db)
    (for-each (lambda (library)
		(load-library (compile-library library library-db)
			      library-db))
	      libraries)
    (if (pair? imports)
	(let ((environment*
	       (expanded-imports->environment
		(expand-import-sets imports library-db))))
	  (let loop ((exprs body) (value unspecific))
	    (if (pair? exprs)
		(loop (cdr exprs)
		      (eval (car exprs) environment*))
		value)))))

  (bundle r7rs-source? save-metadata! load))

(define r7rs-source?
  (make-bundle-predicate 'r7rs-source))

;;;; Compile

(define (compile-library library db)
  (let ((name (parsed-library-name library))
	(imports
	 (expand-import-sets (parsed-library-imports library)
			     db))
	(exports (parsed-library-exports library))
	(contents (expand-parsed-contents (parsed-library-contents library))))
    (db 'save-compiled!
	(make-compiled-library name
			       imports
			       exports
			       (compile-contents contents
						 imports
						 (map library-export-from
						      exports)
						 db)
			       db))
    name))

(define (compile-contents contents imports exports-from library-db)
  (receive (body bound free)
      (syntax-library-forms contents
			    (expanded-imports->environment imports
							   library-db))
    (if (not (lset<= eq? exports-from (lset-union eq? bound free)))
	(warn "Library export refers to unbound identifiers:"
	      (lset-difference eq?
			       exports-from
			       (lset-union eq? bound free))))
    (let ((imports-to (map library-import-to imports)))
      (if (not (lset<= eq? free imports-to))
	  (warn "Library has free references not provided by imports:"
		(lset-difference eq? free imports-to))))
    body))

;;;; Load

(define (load-library library-name library-db)
  (or (library-db 'get-loaded library-name #f)
      (let ((compiled (library-db 'get-compiled library-name)))
	(let ((environment
	       (expanded-imports->environment
		(compiled-library-imports compiled)
		library-db)))
	  (scode-eval (compiled-library-body compiled)
		      environment)
	  (let ((loaded
		 (make-loaded-library (compiled-library-name compiled)
				      (compiled-library-exports compiled)
				      environment)))
	    (library-db 'save-loaded! loaded)
	    loaded)))))

(define (library-exporter library-name library-db)
  (loaded-library-exporter (load-library library-name library-db)))

(define (environment . import-sets)
  (expanded-imports->environment
   (expand-import-sets (map parse-import-set import-sets))))

(define (expanded-imports->environment imports library-db)
  (let ((env
	 (make-root-top-level-environment (map library-import-to imports))))
    (for-each (lambda (import)
		(let ((value
		       (library-exporter
			(library-import-from-library import)
			library-db))
		      (name (library-import-to import)))
		  (cond ((macro-reference-trap? value)
			 (environment-define-macro
			  env name
			  (macro-reference-trap-transformer value)))
			((unassigned-reference-trap? value)
			 ;; nothing to do
			 )
			(else
			 (environment-define env name value)))))
	      imports)
    env))