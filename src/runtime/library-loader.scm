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

;;;; Syntax

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

(define (imports->environment imports db)
  (if (not (import-environments-available? imports db))
      (error "Imported libraries unavailable:"
	     (library-imports-from
	      (remove import-environment-available? imports))))
  (make-environment-from-imports imports db))

(define (import-environments-available? imports db)
  (every (lambda (import)
	   (import-environment-available? import db))
	 imports))

(define (import-environment-available? import db)
  (let ((name (library-import-from-library import)))
    (and (registered-library? name db)
	 ((registered-library name db) 'has? 'environment))))

(define (make-environment-from-imports imports db)
  (let ((env
	 (make-root-top-level-environment (map library-import-to imports))))
    (for-each (lambda (import)
		(let ((value
		       ((library-exporter
			 (registered-library
			  (library-import-from-library import)
			  db))
			(library-import-from import)))
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

(define-automatic-property 'imports-environment '(imports db)
  import-environments-available?
  make-environment-from-imports)

(define (environment . import-sets)
  (let ((parsed (map parse-import-set import-sets)))
    (let ((unusable (remove parsed-import-expandable? parsed)))
      (if (pair? unusable)
	  (error "Imports not usable:" unusable)))
    (imports->environment
     (expand-parsed-imports parsed host-library-db)
     host-library-db)))

;;;; SCode representation

(define-automatic-property 'scode
  '(name imports-used exports bound-names contents)
  #f
  (lambda (name imports exports bound-names contents)
    (make-scode-library (make-scode-library-metadata name imports exports
						     bound-names)
			contents)))

(define (make-scode-library metadata contents)
  (make-scode-declaration `((target-metadata ,metadata))
			  (make-scode-quotation contents)))

(define (scode-library? object)
  (and (scode-declaration? object)
       (let ((text (scode-declaration-text object)))
	 (and (singleton-list? text)
	      (target-metadata? (car text))
	      (let ((metadata-values (metadata-elt-values (car text))))
		(and (singleton-list? metadata-values)
		     (scode-library-metadata? (car metadata-values))))))
       (scode-quotation? (scode-declaration-expression object))))

(define (scode-library-metadata library)
  (car (metadata-elt-values (car (scode-declaration-text library)))))

(define (scode-library-contents library)
  (scode-quotation-expression (scode-declaration-expression library)))

(define (make-scode-library-metadata name imports exports bound-names)
  `(scode-library (name ,name)
		  (imports ,@(map library-import->list imports))
		  (exports ,@(map library-export->list exports))
		  (bound-names ,@bound-names)))

(define (scode-library-property keyword library)
  (metadata-elt-values
   (find (lambda (metadata)
	   (eq? (metadata-elt-keyword metadata) keyword))
	 (metadata-elt-values (scode-library-metadata library)))))

(define (scode-library-name library)
  (car (scode-library-property 'name library)))

(define (scode-library-imports library)
  (map list->library-import (scode-library-property 'imports library)))

(define (scode-library-exports library)
  (map list->library-export (scode-library-property 'exports library)))

(define (singleton-list? object)
  (and (pair? object)
       (null? (cdr object))))

(define (specific-metadata-predicate keyword)
  (lambda (object)
    (and (metadata-elt? object)
	 (eq? (metadata-elt-keyword object) keyword)
	 (every metadata-elt? (metadata-elt-values object)))))

(define target-metadata? (specific-metadata-predicate 'target-metadata))
(define scode-library-metadata? (specific-metadata-predicate 'scode-library))

(define (metadata-elt? object)
  (and (pair? object)
       (symbol? (car object))
       (list? (cdr object))))
(register-predicate! metadata-elt? 'metadata-elt)

(define (metadata-elt-keyword elt)
  (guarantee metadata-elt? elt 'metadata-elt-keyword)
  (car elt))

(define (metadata-elt-values elt)
  (guarantee metadata-elt? elt 'metadata-elt-values)
  (cdr elt))

;;;; Evaluation

(define (eval-r7rs-source source db)
  (let ((program (register-r7rs-source! source db)))
    (if program
	(library-eval-result program))))

(define (r7rs-scode-file? scode)
  (let ((scode (strip-comments scode)))
    (or (scode-library? scode)
	(and (scode-sequence? scode)
	     (every scode-library? (scode-sequence-actions scode))))))

(define (r7rs-scode-file-libraries scode)
  (let ((scode (strip-comments scode)))
    (if (scode-library? scode)
	(list scode)
	(scode-sequence-actions scode))))

(define (strip-comments object)
  (if (and (scode-comment? object)
	   (not (scode-declaration? object)))
      (strip-comments (scode-comment-expression object))
      object))

(define (eval-r7rs-scode-file scode pathname db)
  (let ((libraries
	 (let ((filename (->namestring pathname)))
	   (map (lambda (library)
		  (scode-library->library library filename))
		(r7rs-scode-file-libraries scode)))))
    (register-libraries! libraries db)
    (for-each library-eval-result libraries)))

(define (scode-library->library library filename)
  (make-library (scode-library-name library)
		'imports (scode-library-imports library)
		'exports (scode-library-exports library)
		'contents (scode-library-contents library)
		'filename filename))

(define-automatic-property '(eval-result environment)
    '(contents imports-environment)
  #f
  (lambda (contents env)
    (values (scode-eval contents env)
	    env)))

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

;;;; Load

#|
(define (load db)
  (for-each (lambda (parsed)
	      (load-library (syntax-library parsed db)
			    db))
	    parsed-libraries)
  (if (pair? imports)
      (let ((environment*
	     (imports->environment
	      (expand-import-sets imports db))))
	(let loop ((exprs body) (value unspecific))
	  (if (pair? exprs)
	      (loop (cdr exprs)
		    (eval (car exprs) environment*))
	      value)))))

(define (load-library library-name db)
  (or (db 'get-loaded library-name #f)
      (let ((syntaxed (db 'get-syntaxed library-name)))
	(let ((environment
	       (imports->environment
		(syntaxed-library-imports syntaxed)
		db)))
	  (scode-eval (syntaxed-library-body syntaxed)
		      environment)
	  (let ((loaded
		 (make-loaded-library (syntaxed-library-name syntaxed)
				      (syntaxed-library-exports syntaxed)
				      environment)))
	    (db 'save-loaded! loaded)
	    loaded)))))
|#