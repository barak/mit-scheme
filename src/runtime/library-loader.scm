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

;;;; Compile

(define (compile-library form library-db)
  (let ((library (parse-define-library-form form)))
    (let ((imports
	   (convert-import-sets (parsed-library-imports library)
				library-db)))
      (make-compiled-library (parsed-library-name library)
			     imports
			     (parsed-library-exports library)
			     (compile-contents library library-db)))))

(define (compile-contents library library-db)
  (let ((imports (parsed-library-imports library))
	(exports (parsed-library-exports library)))
    (receive (body bound free)
	(syntax-library-forms
	 (append-map (lambda (directive)
		       (case (car directive)
			 ((include)
			  (fluid-let ((param:reader-fold-case? #f))
			    (append-map (lambda (pathname)
					  (call-with-input-file pathname
					    read-file))
					(cdr directive))))
			 ((include-ci)
			  (fluid-let ((param:reader-fold-case? #t))
			    (append-map (lambda (pathname)
					  (call-with-input-file pathname
					    read-file))
					(cdr directive))))
			 ((begin)
			  (cdr directive))
			 (else
			  (error "Unknown content directive:" directive))))
		     (parsed-library-contents library))
	 (converted-imports->environment imports library-db))
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
      body)))

(define-record-type <compiled-library>
    (make-compiled-library name imports exports body)
    compiled-library?
  (name compiled-library-name)
  (imports compiled-library-imports)
  (exports compiled-library-exports)
  (body compiled-library-body))

(define (compiled-library->scode library)
  (make-scode-declaration
   `(target-metadata
     (library (name ,(compiled-library-name library))
	      (imports ,(compiled-library-imports library))
	      (exports ,(compiled-library-exports library))))
   (make-scode-quotation (compiled-library-body library))))

;;;; Load

(define (load-library library-name library-db)
  (or (library-db 'get-loaded library-name #f)
      (let ((compiled (library-db 'get-compiled library-name)))
	(let ((environment
	       (converted-imports->environment
		(compiled-library-imports compiled)
		library-db)))
	  (scode-eval (compiled-library-body compiled)
		      environment)
	  (make-loaded-library (compiled-library-name compiled)
			       (compiled-library-exports compiled)
			       environment
			       library-db)))))

(define (make-loaded-library name exports environment library-db)
  (let ((library
	 (%make-loaded-library name
			       (map library-export-to exports)
			       (make-exporter exports environment)
			       environment)))
    (library-db 'save-loaded! library)
    library))

(define (make-exporter exports environment)
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
	(cdr p)))))

(define-record-type <loaded-library>
    (%make-loaded-library name environment exporter)
    loaded-library?
  (name loaded-library-name)
  (exports loaded-library-exports)
  (exporter loaded-library-exporter)
  (environment loaded-library-environment))

(define (library-exporter library-name library-db)
  (loaded-library-exporter (load-library library-name library-db)))

(define (environment . import-sets)
  (converted-imports->environment
   (convert-import-sets (map parse-import-set import-sets))))

(define (converted-imports->environment imports library-db)
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