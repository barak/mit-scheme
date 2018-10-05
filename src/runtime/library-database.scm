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

(define (make-library-db)
  (let ((metadata (make-library-table))
	(compiled (make-library-table))
	(loaded (make-library-table)))

    (define (metadata? name)
      (metadata 'has? name))

    (define (get-metadata name #!optional default-value)
      (metadata 'get name default-value))

    (define (save-metadata! library)
      (metadata 'put! (library-metadata-name library) library))

    (define (require-metadata names)
      (let ((unknown (remove metadata? names)))
	(if (pair? unknown)
	    (error "Can't resolve libraries:" unknown))))

    (define (compiled? name)
      (compiled 'has? name))

    (define (get-compiled name #!optional default-value)
      (compiled 'get name default-value))

    (define (save-compiled! library)
      (compiled 'put! (compiled-library-name library) library))

    (define (require-compiled names)
      (let ((unknown (remove compiled? names)))
	(if (pair? unknown)
	    (error "Can't resolve libraries:" unknown))))

    (define (loaded? name)
      (loaded 'has? name))

    (define (get-loaded name #!optional default-value)
      (loaded 'get name default-value))

    (define (save-loaded! library)
      (loaded 'put! (loaded-library-name library) library))

    (bundle library-db?
	    metadata? get-metadata save-metadata! require-metadata
	    compiled? get-compiled save-compiled! require-compiled
	    loaded? get-loaded save-loaded!)))

(define library-db?
  (make-bundle-predicate 'library-database))

(define (make-library-table)
  (let ((table (make-equal-hash-table)))

    (define (has? name)
      (hash-table-exists? table name))

    (define (get name #!optional default-value)
      (if (default-object? default-value)
	  (hash-table-ref table name)
	  (hash-table-ref/default table name default-value)))

    (define (put! name value)
      (hash-table-set! table name value))

    (define (delete! key)
      (hash-table-delete! table key))

    (define (get-alist)
      (hash-table->alist table))

    (define (put-alist! alist*)
      (for-each (lambda (p)
		  (put! (car p) (cdr p)))
		alist*))

    (bundle library-table? has? get put! delete! get-alist put-alist!)))

(define library-table?
  (make-bundle-predicate 'library-table))

(define-record-type <library-metadata>
    (make-library-metadata name imports exports pathname)
    library-metadata?
  (name library-metadata-name)
  ;; Parsed unexpanded import sets.
  (imports library-metadata-imports)
  ;; List of external symbols.
  (exports library-metadata-exports)
  ;; Pathname to file where library is defined.
  ;; May be #f in special cases.
  (pathname library-metadata-pathname))

(define (parsed-library->metadata parsed db)
  (make-library-metadata
   (parsed-library-name parsed)
   (expand-import-sets (parsed-library-imports parsed) db)
   (map library-export-to (parsed-library-exports parsed))
   (parsed-library-pathname parsed)))

(define (make-loaded-library name exports environment)
  (%make-loaded-library name
			(map library-export-to exports)
			(make-exporter exports environment)
			environment))

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
    (%make-loaded-library name exports exporter environment)
    loaded-library?
  (name loaded-library-name)
  (exports loaded-library-exports)
  (exporter loaded-library-exporter)
  (environment loaded-library-environment))

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