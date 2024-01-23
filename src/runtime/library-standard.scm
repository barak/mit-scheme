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

;;;; R7RS libraries: standard libraries
;;; package: (runtime library standard)

(declare (usual-integrations))

(add-boot-deps! '(runtime library database))

(define-deferred host-library-db
  (make-library-db 'host))

(define-deferred current-library-db
  (make-settable-parameter host-library-db
    (lambda (value)
      (guarantee library-db? value))))

(define (finish-host-library-db!)
  (register-libraries! (make-standard-libraries) host-library-db))

(define (make-standard-libraries)
  (map (lambda (p)
	 (let ((library (car p))
	       (exports (cdr p)))
	   (make-library library
			 'export-groups
			 (list
			  (make-export-group #f (convert-exports exports)))
			 'environment system-global-environment)))
       standard-libraries))

(define (check-standard-libraries!)
  (for-each (lambda (p)
	      (check-standard-library! (car p) (cdr p)))
	    standard-libraries))

(define (check-standard-library! library exports)
  (let ((missing
	 (remove (lambda (name)
		   (memq (environment-reference-type system-global-environment
						     name)
			 '(normal macro)))
		 exports)))
    (if (pair? missing)
	(warn "Missing definitions for library:" library missing))))

;; Filters the given imports to find those that are equivalent to global
;; variables, and for each one returns a pair of the "to" identifier and the
;; corresponding global identifier.  For now this is greatly simplified by
;; knowing that all standard libraries use global variables, but this will need
;; to be adapted when there are libraries that don't.
(define (standard-library-globals imports)
  (filter-map (lambda (import)
		(let ((p
		       (assoc (library-ixport-from-library import)
			      standard-libraries
			      library-name=?)))
		  (and p
		       (memq (library-ixport-from import)
			     (cdr p))
		       (cons (library-ixport-to import)
			     (library-ixport-from import)))))
	      imports))

(define (define-standard-library library exports)
  (let ((p (assoc library standard-libraries library-name=?)))
    (if p
	(set-cdr! p exports)
	(begin
	  (set! standard-libraries
		(cons (cons library exports)
		      standard-libraries))
	  unspecific)))
  library)

(define standard-libraries '())

(include "generated-standard-libs")

;; Make sure that the names introduced by macro expansions are also included.
;; This is a kludge to work around our inability to address names globally.
(define (convert-exports names)
  (map (lambda (name)
	 (make-library-ixport '(mit legacy runtime) name))
       (lset-union eq?
		   names
		   (fold (lambda (name extra)
			   (let ((deps (macro-deps name)))
			     (if deps
				 (lset-union eq? deps extra)
				 extra)))
			 '()
			 names))))

(define-deferred macro-deps
  (flatten-macro-deps
   '((and if)
     (and-let* and begin let)
     (assert error if not)
     (begin0 let)
     (case begin eq? eqv? if let or quote)
     (case-lambda apply default-object? error fix:= fix:>= if lambda length let
		  make-arity-dispatched-procedure)
     (circular-stream cons delay letrec)
     (cond begin if let)
     (cond-expand begin)
     (cons-stream cons delay)
     (cons-stream* cons delay)
     (define lambda named-lambda)
     (define-record-type %record? %record-ref %record-set! define eq?
			 guarantee make-record-type quote record-accessor
			 record-constructor record-modifier record-predicate)
     (define-values begin call-with-values define lambda set!)
     (delay delay-force make-promise)
     (delay-force lambda make-unforced-promise)
     (do begin if let)
     (guard begin call-with-current-continuation if lambda let
	    raise-continuable with-exception-handler)
     (include begin)
     (include-ci begin)
     (let declare lambda letrec letrec* named-lambda)
     (let* let)
     (let-syntax* let-syntax)
     (letrec lambda let set!)
     (letrec* begin lambda let)
     (local-declare declare let)
     (parameterize cons lambda list parameterize*)
     (quasiquote append cons list list->vector quote vector)
     (receive call-with-values lambda)
     (syntax-rules declare er-macro-transformer ill-formed-syntax lambda
		   syntax-rules:expand-template syntax-rules:match-datum)
     (unless begin if not)
     (when begin if))))

(define (flatten-macro-deps alist)

  (define (expand-deps deps expanded)
    (fold (lambda (dep expanded)
	    (if (memq dep expanded)
		expanded
		(let ((expanded* (cons dep expanded))
		      (p (assq dep alist)))
		  (if p
		      (expand-deps (cdr p) expanded*)
		      expanded*))))
	  expanded
	  deps))

  (let ((expanded
	 (map (lambda (p)
		(cons (car p) (expand-deps (cdr p) '())))
	      alist)))
    (lambda (name)
      (let ((p (assq name expanded)))
	(and p
	     (cdr p))))))

;;;; Synthetic libraries

;;; A synthetic library is one that's derived from legacy packages, much like a
;;; standard library, with a little more flexibility in where the exports come
;;; from.

(define (define-synthetic-library name source-package package-pred)
  (set! synthetic-libraries
	(cons (list name source-package package-pred)
	      synthetic-libraries))
  unspecific)

(define synthetic-libraries '())

(define (package-predicate:name-prefix prefix)
  (lambda (pd)
    (let ((name (package-description/name pd)))
      (and (>= (length name) (length prefix))
	   (equal? (take name (length prefix)) prefix)))))

(define-synthetic-library '(mit legacy runtime) '()
  (package-predicate:name-prefix '(runtime)))

(define-synthetic-library '(mit library) '(runtime)
  (package-predicate:name-prefix '(runtime library)))

(define-synthetic-library '(mit runtime-internal) '(runtime)
  (package-predicate:name-prefix '(runtime)))

(define initial-host-library-db)
(define (initialize-synthetic-libraries! package-file)
  (for-each (lambda (p)
	      (let ((library (car p))
		    (source-package (cadr p))
		    (package-pred (caddr p)))
		(make-synthetic-library library
		  (get-exports package-file library source-package
			       package-pred)
		  (->environment source-package))))
	    synthetic-libraries)
  (set! initial-host-library-db
	(copy-library-db host-library-db 'initial-host))
  (check-standard-libraries!))

(define (new-library-db #!optional name)
  (copy-library-db initial-host-library-db name))

(define (make-synthetic-library library exports environment)
  (register-library! (make-library library
				   'export-groups
				   (list (make-export-group #f exports))
				   'environment environment)
		     host-library-db))

(define (get-exports package-file library source-package package-pred)
  (append-map
   (lambda (pd)
     (package-exports pd library source-package))
   (filter package-pred
	   (vector->list (package-file/descriptions package-file)))))

(define (package-exports pd library source-package)
  (filter-map (lambda (link)
		(and (equal? source-package (link-description/package link))
		     (not (link-description/status link))
		     (make-library-ixport library
					  (link-description/outer-name link))))
	      (vector->list (package-description/exports pd))))