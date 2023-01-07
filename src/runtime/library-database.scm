#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(add-boot-deps! '(runtime hash-table))

(define-record-type <library-db>
    (%make-library-db name table)
    library-db?
  (name %db-name)
  (table %db-table))

(define (make-library-db #!optional name)
  (%make-library-db (if (or (default-object? name) (not name))
			#f
			(guarantee symbol? name 'make-library-db))
		    (make-equal-hash-table)))

(define (registered-library? key db)
  (hash-table-exists? (%db-table db) key))

(define (registered-library key db)
  (let ((library (hash-table-ref/default (%db-table db) key #f)))
    (if (not library)
	(error "No library with this key in database:" key db))
    library))

(define (registered-libraries db)
  (hash-table-values (%db-table db)))

(define (register-library! library db)
  (if (library-registered? library)
      (error "Library already registered:" library))
  (let ((key (library-key library))
	(table (%db-table db)))
    (let ((library* (hash-table-ref/default table key #f)))
      (cond ((not library*)
	     (%set-library-db! library db)
	     (hash-table-set! table key library)
	     library)
	    ((library-preregistered? library*)
	     (%set-library-alist! library* library)
	     library*)
	    (else
	     (warn "Replacing library:" library* db)
	     (%set-library-db! library db)
	     (hash-table-set! table key library)
	     (%set-library-db! library* #f)
	     library)))))

(define (register-libraries! libraries db)
  (for-each (lambda (library)
	      (register-library! library db))
	    libraries))

(define (deregister-library! library db)
  (let ((key (library-key library)))
    (if (not (registered-library? key db))
	(error "Library not registered here:" library db))
    (if (not (library-preregistered? library))
	(warn "Removing library:" library db))
    (hash-table-delete! (%db-table db) key)
    (%set-library-db! library #f)))

(define (copy-library-db db #!optional new-name)
  (let ((db* (make-library-db new-name)))
    (for-each (lambda (library)
		(register-library! (copy-library library) db*))
	      (registered-libraries db))
    db*))

(define-print-method library-db?
  (standard-print-method 'library-db
    (lambda (db)
      (if (%db-name db) (list (%db-name db)) '()))))

(define-record-type <library>
    (%make-library name db alist original-alist)
    library?
  (name library-name)
  (db %library-db %set-library-db!)
  (alist %library-alist)
  (original-alist %library-original-alist))

(define-print-method library?
  (standard-print-method 'library
    (lambda (library)
      (let ((name (library-name library)))
	(if name
	    (list name)
	    '())))))

(define-pp-describer library?
  (lambda (library)
    (cons (list 'db (%library-db library))
	  (map (lambda (p)
		 (list (car p) (cdr p)))
	       (cdr (%library-alist library))))))

(define (library-key library)
  (or (library-name library)
      (list '|#[program]| (hash-object library))))

(define (alist->library name alist)
  (%make-library name
		 #f
		 (cons 'library alist)
		 (cons 'library alist)))

(define (%set-library-alist! library library*)
  (set-cdr! (%library-alist library)
	    (alist-copy (cdr (%library-alist library*))))
  (set-cdr! (%library-original-alist library)
	    (alist-copy (cdr (%library-original-alist library*)))))

(define (make-library name . keylist)
  (if name (guarantee library-name? name 'make-library))
  (alist->library name (convert-library-keylist keylist)))

(define (convert-library-keylist keylist)
  (fold-right (lambda (key value alist)
		(if (assq key alist)
		    alist
		    (cons (cons key value) alist)))
	      (keyword-list->alist keylist)
	      (map car library-defaults)
	      (map cadr library-defaults)))

(define library-defaults
  '((parsed-imports ())
    (parsed-exports ())
    (parsed-defines ())
    (parsed-contents ())
    (filename #f)))

(define (copy-library library)
  (alist->library (library-name library)
		  (alist-copy (cdr (%library-original-alist library)))))

(define (library-registered? library)
  (and (%library-db library) #t))

(define (library-db library)
  (let ((db (%library-db library)))
    (if (not db) (error "Library not registered:" library))
    db))

(define (library-has? key library)
  (if (and (memq key properties-requiring-load)
	   (library-preregistered? library))
      (load-preregistered-library! library))
  (cond ((assq key (cdr (%library-alist library))) #t)
	((auto-property key)
	 => (lambda (auto) (auto-runnable? auto library)))
	(else #f)))

(define (library-get key library)
  (if (and (memq key properties-requiring-load)
	   (library-preregistered? library))
      (load-preregistered-library! library))
  (let ((alist (%library-alist library)))
    (cond ((assq key (cdr alist)) => cdr)
	  ((auto-property key)
	   => (lambda (auto)
		(if (not (auto-runnable? auto library))
		    (error "Automatic property not ready:"
			   key
			   (error-irritant/noise " because of")
			   (auto-unready-deps auto library)))
		(set-cdr! alist (append (run-auto auto library) (cdr alist)))
		(cdr (assq key (cdr alist)))))
	  (else (error "Unknown property:" key)))))

(define properties-requiring-load
  '(contents))

(define (library-accessor key)
  (lambda (library)
    (library-get key library)))

(define library-bound-names (library-accessor 'bound-names))
(define library-contents (library-accessor 'contents))
(define library-environment (library-accessor 'environment))
(define library-eval-result (library-accessor 'eval-result))
(define library-export-groups (library-accessor 'export-groups))
(define library-filename (library-accessor 'filename))
(define library-free-names (library-accessor 'free-names))
(define library-imports (library-accessor 'imports))
(define library-imports-environment (library-accessor 'imports-environment))
(define library-imports-used (library-accessor 'imports-used))
(define library-parsed-contents (library-accessor 'parsed-contents))
(define library-parsed-imports (library-accessor 'parsed-imports))
(define library-parsed-exports (library-accessor 'parsed-exports))

(define (library->environment-helper name)
  (if (library? name)
      (and (library-has? 'environment name)
	   name)
      (and (library-name? name)
	   (registered-library? name host-library-db)
	   (let ((library (registered-library name host-library-db)))
	     (and (library-has? 'environment library)
		  library)))))

(define (preregister-library! library db)
  (register-library! (prepare-for-preregistration library) db))

(define (prepare-for-preregistration library)
  (alist->library (library-name library)
    (alist-fold-right (lambda (key value acc)
			(if (eq? key 'contents)
			    acc
			    (alist-cons key value acc)))
		      (alist-cons 'preregistration? #t '())
		      (cdr (%library-alist library)))))

(define (library-preregistered? library)
  (library-has? 'preregistration? library))

(define (load-preregistered-library! library)
  (parameterize ((current-library-db (library-db library)))
    (load (library-filename library))))

;;;; Export groups

;;; An export group can be private, meaning that only the library specified in
;;; library-name may import the group's exports.  It can also be public, in
;;; which case library-name is #f.

(define-record-type <export-group>
    (make-export-group library-name exports)
    export-group?
  (library-name export-group-library-name)
  (exports export-group-exports))

(define-print-method export-group?
  (standard-print-method 'export-group
    (lambda (group)
      (list (export-group-library-name group)))))

(define (library-exports library #!optional importing-library)
  (let ((groups (library-export-groups library)))
    (if (or (not importing-library)
	    (default-object? importing-library))
	(let ((public
	       (find (lambda (group)
		       (not (export-group-library-name group)))
		     groups)))
	  (if public
	      (export-group-exports public)
	      '()))
	(fold (lambda (group exports)
		(let ((export-to (export-group-library-name group))
		      (importing-name (library-name importing-library)))
		  (if (or (not export-to)
			  (library-name=? export-to importing-name))
		      (append (export-group-exports group) exports)
		      exports)))
	      '()
	      groups))))

(define (export-group->list group)
  (cons (export-group-library-name group)
	(map library-ixport->list (export-group-exports group))))

(define (list->export-group list)
  (make-export-group (car list)
		     (map list->library-ixport (cdr list))))

;;;; Automatic properties

(define (define-automatic-property prop deps guard generator)
  (guarantee auto-property-key? prop 'define-automatic-property)
  (guarantee-list-of symbol? deps 'define-automatic-property)
  (set! automatic-properties
	(cons (make-auto (if (symbol? prop) (list prop) prop)
			 generator guard deps)
	      automatic-properties))
  unspecific)

(define (auto-property-key? object)
  (or (symbol? object)
      (and (non-empty-list? object)
	   (every symbol? object))))
(register-predicate! auto-property-key? 'automatic-property-key)

(define-record-type <auto>
    (make-auto keys generator guard deps)
    auto?
  (keys auto-keys)
  (generator auto-generator)
  (guard auto-guard)
  (deps auto-deps))

(define (auto-property key)
  (find (lambda (auto) (memq key (auto-keys auto)))
	automatic-properties))

(define automatic-properties '())

(define (auto-runnable? auto library)
  (and (auto-deps-available? auto library)
       (or (not (auto-guard auto))
	   (apply-auto (auto-guard auto) auto library))))

(define (auto-unready-deps auto library)
  (if (auto-deps-available? auto library)
      (list "guard expr")
      (filter-map (lambda (key)
		    (and (not (auto-dep-available? key library))
			 (let ((auto* (auto-property key)))
			   (if auto*
			       (cons key (auto-unready-deps auto* library))
			       key))))
		  (auto-deps auto))))

(define (run-auto auto library)
  (let-values ((all-values (apply-auto (auto-generator auto) auto library)))
    (if (not (= (length (auto-keys auto)) (length all-values)))
	(error "Wrong number of values returned:" (auto-keys auto) all-values))
    (map cons (auto-keys auto) all-values)))

(define (auto-deps-available? auto library)
  (every (lambda (key) (auto-dep-available? key library))
	 (auto-deps auto)))

(define (auto-dep-available? key library)
  (case key
    ((library name) #t)
    ((db) (library-registered? library))
    (else (library-has? key library))))

(define (apply-auto proc auto library)
  (apply proc
	 (map (lambda (key)
		(case key
		  ((library) library)
		  ((name) (library-name library))
		  ((db) (library-db library))
		  (else (library-get key library))))
	      (auto-deps auto))))