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

;;;; R7RS libraries: database abstraction
;;; package: (runtime library database)

(declare (usual-integrations))

(define (make-library-db name)
  (let loop ((table (make-equal-hash-table)))

    (define (has? name)
      (hash-table-exists? table name))

    (define (get name)
      (hash-table-ref table name))

    (define (put! library)
      (if (and (library 'has? 'db)
	       (not (eq? (library 'get 'db) this)))
	  (error "Can't use library in multiple databases:" library))
      (library 'put! 'db this)
      (let ((name (library 'get 'name)))
	(if name
	    (begin
	      (if (has? name)
		  (warn "Overwriting library:" name))
	      (hash-table-set! table name library)))))

    (define (get-names)
      (hash-table-keys table))

    (define (get-all)
      (hash-table-values table))

    (define (get-copy)
      (loop (hash-table-copy table)))

    (define (summarize-self)
      (list name))

    (define (describe-self)
      (map (lambda (library)
	     (list 'library library))
	   (get-all)))

    (define this
      (bundle library-db?
	      has? get put! get-names get-all get-copy
	      summarize-self describe-self))
    this))

(define library-db?
  (make-bundle-predicate 'library-database))

(define (copy-library-db db)
  (guarantee library-db? db 'copy-library-db)
  (db 'get-copy))

(define (make-library name . keylist)
  (if name
      (guarantee library-name? name 'make-library))
  (let ((alist
	 (cons* 'library
		(cons 'name name)
		(keyword-list->alist keylist))))

    (define (has? key)
      (if (assq key (cdr alist))
	  #t
	  (let ((auto (automatic-property key)))
	    (and auto
		 (auto-runnable? auto this)))))

    (define (get key)
      (let ((p (assq key (cdr alist))))
	(if p
	    (cdr p)
	    (let ((auto (automatic-property key)))
	      (if (not auto)
		  (error "Unknown property:" key))
	      (if (not (auto-runnable? auto this))
		  (error "Auto property not ready:" auto))
	      (let ((bindings (run-auto auto this)))
		(set-cdr! alist (append bindings (cdr alist)))
		(cdr (assq key bindings)))))))

    (define (put! key value)
      (if (automatic-property? key)
	  (warn "Overwriting automatic property:" key))
      (let ((p (assq key (cdr alist))))
	(if p
	    (begin
	      (warn "Overwriting property:" key)
	      (set-cdr! p value))
	    (set-cdr! alist (cons (cons key value) (cdr alist))))))

    (define (intern! key get-value)
      (let ((p (assq key (cdr alist))))
	(if p
	    (cdr p)
	    (let ((value (get-value)))
	      (set-cdr! alist (cons (cons key value) (cdr alist)))
	      value))))

    (define (delete! key)
      (set-cdr! alist (del-assq! key (cdr alist))))

    (define (summarize-self)
      (if name
	  (list name)
	  '()))

    (define (describe-self)
      (map (lambda (p)
	     (list (car p) (cdr p)))
	   (cdr alist)))

    (define this
      (bundle library?
	      has? get put! intern! delete! summarize-self describe-self))
    this))

(define library?
  (make-bundle-predicate 'library))

;;;; Automatic properties

(define (define-automatic-property prop deps guard generator)
  (guarantee automatic-property-key? prop 'define-automatic-property)
  (guarantee-list-of symbol? deps 'define-automatic-property)
  (set! automatic-properties
	(cons (make-auto (if (symbol? prop) (list prop) prop)
			 generator guard deps)
	      automatic-properties))
  unspecific)

(define (automatic-property-key? object)
  (or (symbol? object)
      (and (non-empty-list? object)
	   (every symbol? object))))
(register-predicate! automatic-property-key? 'automatic-property-key)

(define-integrable make-auto cons*)
(define-integrable auto-keys car)
(define-integrable auto-generator cadr)
(define-integrable auto-guard caddr)
(define-integrable auto-deps cdddr)

(define-integrable (auto-key auto)
  (car (auto-keys auto)))

(define-integrable (auto-multi-valued? auto)
  (pair? (cdr (auto-keys auto))))

(define (automatic-property? key)
  (any (lambda (auto)
	 (memq key (auto-keys auto)))
       automatic-properties))

(define (automatic-property key)
  (find (lambda (auto)
	  (memq key (auto-keys auto)))
	automatic-properties))

(define automatic-properties '())

(define (auto-runnable? auto library)
  (and (every (lambda (key)
		(library 'has? key))
	      (auto-deps auto))
       (or (not (auto-guard auto))
	   (apply (auto-guard auto)
		  (map (lambda (key)
			 (library 'get key))
		       (auto-deps auto))))))

(define (run-auto auto library)
  (let ((runner
	 (lambda ()
	   (apply (auto-generator auto)
		  (map (lambda (key)
			 (library 'get key))
		       (auto-deps auto))))))
    (if (auto-multi-valued? auto)
	(receive all-values (runner)
	  (if (not (= (length (auto-keys auto))
		      (length all-values)))
	      (error "Wrong number of values returned:"
		     (auto-keys auto)
		     all-values))
	  (map cons (auto-keys auto) all-values))
	(list (cons (auto-key auto) (runner))))))

;;;; Imports and exports

(define (make-library-import from-library from #!optional to)
  (guarantee library-name? from-library 'make-library-import)
  (guarantee symbol? from 'make-library-import)
  (%make-library-import from-library from
			(if (default-object? to)
			    from
			    (begin
			      (guarantee symbol? to 'make-library-import)
			      to))))

(define-record-type <library-import>
    (%make-library-import from-library from to)
    library-import?
  (from-library library-import-from-library)
  (from library-import-from)
  (to library-import-to))

(define (library-import=? e1 e2)
  (and (library-name=? (library-import-from-library e1)
		       (library-import-from-library e2))
       (eq? (library-import-from e1)
	    (library-import-from e2))
       (eq? (library-import-to e1)
	    (library-import-to e2))))

(define (library-import->list import)
  (cons* (library-import-from-library import)
	 (library-import-from import)
	 (if (eq? (library-import-from import) (library-import-to import))
	     '()
	     (list (library-import-to import)))))

(define (list->library-import list)
  (apply make-library-import list))

(define (library-imports-from imports)
  (delete-duplicates (map library-import-from-library imports)
		     library-name=?))

(define-print-method library-import?
  (standard-print-method 'library-import
    library-import->list))

(define (make-library-export from #!optional to)
  (guarantee symbol? from 'make-library-export)
  (if (default-object? to)
      (%make-library-export from from)
      (begin
	(guarantee symbol? to 'make-library-export)
	(%make-library-export from to))))

(define-record-type <library-export>
    (%make-library-export from to)
    library-export?
  (from library-export-from)
  (to library-export-to))

(define (library-export=? e1 e2)
  (and (eq? (library-export-from e1)
	    (library-export-from e2))
       (eq? (library-export-to e1)
	    (library-export-to e2))))

(define (library-export->list export)
  (cons (library-export-from export)
	(if (eq? (library-export-from export) (library-export-to export))
	    '()
	    (list (library-export-to export)))))

(define (list->library-export list)
  (apply make-library-export list))

(define-print-method library-export?
  (standard-print-method 'library-export
    library-export->list))

;;;; Library accessors

(define (registered-library? name db)
  (db 'has? name))

(define (registered-library name db)
  (db 'get name))

(define (registered-libraries db)
  (db 'get-all))

(define (register-library! library db)
  (guarantee library? library 'register-library!)
  (guarantee library-db? db 'register-library!)
  (db 'put! library))

(define (register-libraries! libraries db)
  (for-each (lambda (library)
	      (register-library! library db))
	    libraries))

(define (library-accessor key)
  (lambda (library)
    (library 'get key)))

(define library-bound-names (library-accessor 'bound-names))
(define library-contents (library-accessor 'contents))
(define library-environment (library-accessor 'environment))
(define library-eval-result (library-accessor 'eval-result))
(define library-exporter (library-accessor 'exporter))
(define library-exports (library-accessor 'exports))
(define library-filename (library-accessor 'filename))
(define library-imports (library-accessor 'imports))
(define library-imports-environment (library-accessor 'imports-environment))
(define library-imports-used (library-accessor 'imports-used))
(define library-name (library-accessor 'name))
(define library-parsed-contents (library-accessor 'parsed-contents))
(define library-parsed-imports (library-accessor 'parsed-imports))
(define library-syntaxed-contents (library-accessor 'syntaxed-contents))

(define (library->environment-helper name)
  (if (library? name)
      (and (name 'has? 'environment)
	   name)
      (and (library-name? name)
	   (registered-library? name host-library-db)
	   (let ((library (registered-library name host-library-db)))
	     (and (library 'has? 'environment)
		  library)))))