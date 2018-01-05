#| -*- Mode: Scheme; keyword-style: none -*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Bundles

;;; A bundle is a set of named objects implemented as a procedure.  When called,
;;; the first argument to the bundle is a symbol identifying the named object to
;;; call, and the rest of the bundle's arguments are passed to the selected
;;; procedure.  If the specified named object isn't a procedure, an error is
;;; signaled.

(declare (usual-integrations))

(define (make-bundle-interface name clauses)
  (guarantee symbol? name 'make-bundle-interface)
  (guarantee clauses? clauses 'make-bundle-interface)
  (letrec*
      ((predicate
	(lambda (datum)
	  (and (bundle? datum)
	       (tag<= (bundle-tag datum) tag))))
       (tag
	(make-tag name
		  predicate
		  predicate-tagging-strategy:never
		  'make-bundle-interface
		  (make-bim name (copy-clauses clauses)))))
    (set-tag<=! tag the-bundle-tag)
    predicate))

(define (bundle-interface? object)
  (and (predicate? object)
       (bim? (tag-extra (predicate->tag object)))))

(define (bundle-interface-name interface)
  (bim-name (tag-extra (predicate->tag interface))))

(define (bundle-interface-clauses interface)
  (copy-clauses (bim-clauses (tag-extra (predicate->tag interface)))))

(define-record-type <bim>
    (make-bim name clauses)
    bim?
  (name bim-name)
  (clauses bim-clauses))

(define (clauses? object)
  (and (list? object)
       (every (lambda (p)
		(or (symbol? p)
		    (and (pair? p)
			 (symbol? (car p))
			 (list? (cdr p))
			 (every (lambda (r)
				  (and (pair? r)
				       (symbol? (car r))
				       (list? (cdr r))))
				(cdr p)))))
	      object)
       (let ((clause-name
	      (lambda (clause)
		(if (symbol? clause)
		    clause
		    (car clause)))))
	 (let loop ((clauses object))
	   (if (pair? clauses)
	       (and (not (any (let ((name (clause-name (car clauses))))
				(lambda (clause)
				  (eq? name (clause-name clause))))
			      (cdr clauses)))
		    (loop (cdr clauses)))
	       #t)))))

(define (copy-clauses clauses)
  (map (lambda (clause)
	 (if (symbol? clause)
	     (list clause)
	     (cons (car clause)
		   (map list-copy (cdr clause)))))
       clauses))

(define (make-bundle interface alist)
  (guarantee bundle-interface? interface 'make-bundle)
  (guarantee bundle-alist? alist 'make-bundle)
  (let ((tag (predicate->tag interface)))
    (check-bundle-alist alist tag)
    (make-entity (lambda (self operator . args)
		   (apply (bundle-ref self operator) args))
		 (make-bundle-metadata tag (alist-copy alist)))))

(define (bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
		(symbol? (car p)))
	      object)))

(define (check-bundle-alist alist tag)
  (let ((clauses (bim-clauses (tag-extra tag))))
    (if (not (lset= (lambda (a c)
		      (eq? (car a) (car c)))
		    alist
		    clauses))
	(error "Bundle alist doesn't match its clauses:" alist clauses))))

(define-record-type <bundle-metadata>
    (make-bundle-metadata tag alist)
    bundle-metadata?
  (tag bundle-metadata-tag)
  (alist bundle-metadata-alist))

(define (define-bundle-printer interface printer)
  (hash-table-set! bundle-printers (predicate->tag interface) printer))

(set-record-type-entity-unparser-method! <bundle-metadata>
  (standard-unparser-method
   (lambda (bundle)
     (bim-name (tag-extra (bundle-tag bundle))))
   (lambda (bundle port)
     (let ((printer
	    (hash-table-ref/default bundle-printers (bundle-tag bundle) #f)))
       (if printer
	   (printer bundle port))))))

(define (bundle? object)
  (and (entity? object)
       (bundle-metadata? (entity-extra object))))

(define (bundle-tag bundle)
  (bundle-metadata-tag (entity-extra bundle)))

(define (bundle-interface bundle)
  (tag->predicate (bundle-tag bundle)))

(define (%bundle-alist bundle)
  (bundle-metadata-alist (entity-extra bundle)))

(define (bundle-alist bundle)
  (alist-copy (%bundle-alist bundle)))

(define (bundle-names bundle)
  (map car (%bundle-alist bundle)))

(define (bundle-ref bundle operator #!optional default)
  (let ((p (assq operator (%bundle-alist bundle))))
    (if p
        (cdr p)
        (begin
          (if (default-object? default)
              (error "Unknown bundle operator:" operator))
          default))))

(define the-bundle-tag)
(define bundle-printers)
(add-boot-init!
 (lambda ()
   (register-predicate! bundle? 'bundle '<= entity?)
   (set! the-bundle-tag (predicate->tag bundle?))
   (set! bundle-printers (make-key-weak-eqv-hash-table))
   (register-predicate! bundle-interface? 'bundle-interface '<= predicate?)
   (register-predicate! clauses? 'interface-clauses)))