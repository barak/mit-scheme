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

;;;; R7RS libraries: database abstraction
;;; package: (runtime library database)

(declare (usual-integrations))

(define (make-library-db name)
  (let ((table (make-equal-hash-table)))

    (define (has? name)
      (hash-table-exists? table name))

    (define (get name)
      (hash-table-ref table name))

    (define (put! library)
      (if (and (library 'has? 'db)
	       (not (eq? (library 'get 'db) this)))
	  (error "Can't use library in multiple databases:" library))
      (let ((name (library 'get 'name)))
	(if (has? name)
	    (warn "Overwriting library:" name))
	(library 'put! 'db this)
	(hash-table-set! table name library)))

    (define (get-names)
      (hash-table-keys table))

    (define (get-all)
      (hash-table-values table))

    (define (summarize-self)
      (list name))

    (define (describe-self)
      (map (lambda (library)
	     (list 'library library))
	   (get-all)))

    (define this
      (bundle library-db?
	      has? get put! get-names get-all
	      summarize-self describe-self))
    this))

(define library-db?
  (make-bundle-predicate 'library-database))

(define-deferred host-library-db
  (make-library-db 'host))

(define (make-library name . keylist)
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
		  (error "Unknown library property:" key))
	      (if (not (auto-runnable? auto this))
		  (error "Auto property not ready:" auto))
	      (let ((value (run-auto auto this)))
		(set-cdr! alist (cons (cons key value) (cdr alist)))
		value)))))

    (define (put! key value)
      (if (automatic-property? key)
	  (error "Can't overwrite automatic property:" key))
      (let ((p (assq key (cdr alist))))
	(if p
	    (begin
	      (warn "Overwriting library property:" key name)
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
      (list name))

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
  (guarantee symbol? prop 'define-automatic-property)
  (guarantee-list-of symbol? deps 'define-automatic-property)
  (let ((p (assq prop automatic-properties))
	(e (cons* generator guard deps)))
    (if p
	(set-cdr! p e)
	(begin
	  (set! automatic-properties
		(cons (cons prop e)
		      automatic-properties))
	  unspecific))))

(define auto-key car)
(define auto-generator cadr)
(define auto-guard caddr)
(define auto-deps cdddr)

(define (automatic-property? prop)
  (and (assq prop automatic-properties) #t))

(define (automatic-property prop)
  (assq prop automatic-properties))

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
  (apply (auto-generator auto)
	 (map (lambda (key)
		(library 'get key))
	      (auto-deps auto))))

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
  (list (library-import-from-library import)
	(library-import-from import)
	(library-import-to import)))

(define (list->library-import list)
  (make-library-import (car list)
		       (cadr list)
		       (caddr list)))

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
  (list (library-export-from export)
	(library-export-to export)))

(define (list->library-export list)
  (make-library-export (car list) (cadr list)))

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

(define library-environment (library-accessor 'environment))
(define library-exporter (library-accessor 'exporter))
(define library-exports (library-accessor 'exports))
(define library-filename (library-accessor 'filename))
(define library-imports (library-accessor 'imports))
(define library-name (library-accessor 'name))
(define library-parsed-contents (library-accessor 'parsed-contents))
(define library-parsed-imports (library-accessor 'parsed-imports))
(define library-syntaxed-contents (library-accessor 'syntaxed-contents))