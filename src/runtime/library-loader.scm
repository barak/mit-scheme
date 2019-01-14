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
  (make-r7rs-scode-file
   (map library->scode-library
	(append (r7rs-source-libraries source)
		(let ((program (r7rs-source-program source)))
		  (if program
		      (list program)
		      '()))))))

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
    (let ((unusable
	   (remove (lambda (import)
		     (parsed-import-expandable? import db))
		   parsed)))
      (if (pair? unusable)
	  (error "Imports not usable:" unusable)))
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
		(r7rs-scode-file-libraries scode)))))
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