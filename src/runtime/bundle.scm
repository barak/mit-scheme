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

;;; A bundle is a set of named elements.  The name and metadata properties of
;;; each element are specified by an interface.  Each metadata property consists
;;; of a symbol identifying the property and some objects that are the property
;;; values.  While some metadata properties will be defined and used by the
;;; bundle implementation, any property can be specified and will be carried
;;; along in the interface.
;;;
;;; It is anticipated that most bundle elements will be procedures.  For
;;; convenience, the bundle is itself implemented as a procedure.  The first
;;; argument to the bundle is a symbol identifying the named object to call, and
;;; the rest of the bundle's arguments are passed to the selected procedure.

(declare (usual-integrations))

(define (make-bundle-interface name elements)
  (guarantee symbol? name 'make-bundle-interface)
  (guarantee elements? elements 'make-bundle-interface)
  (let ((elements (sort-alist elements)))
    (%make-bundle-interface (make-bundle-tag name)
			    name
			    (list->vector (map car elements))
			    (list->vector (map (lambda (element)
						 (map list-copy
						      (cdr element)))
					       elements)))))

(define (make-bundle-tag name)
  (letrec*
      ((predicate
	(lambda (datum)
	  (and (bundle? datum)
	       (tag<= (bundle-interface-tag (bundle-interface datum)) tag))))
       (tag
	(begin
	  (register-predicate! predicate name '<= bundle?)
	  (predicate->tag predicate))))
    tag))

(define (elements? object)
  (and (list? object)
       (every (lambda (p)
		(and (pair? p)
		     (symbol? (car p))
		     (list? (cdr p))
		     (every (lambda (r)
			      (and (pair? r)
				   (symbol? (car r))
				   (list? (cdr r))))
			    (cdr p))))
	      object)
       (alist-has-unique-keys? object)))
(register-predicate! elements? 'interface-elements)

(define-record-type <bundle-interface>
    (%make-bundle-interface tag name element-names element-properties)
    bundle-interface?
  (tag bundle-interface-tag)
  (name bundle-interface-name)
  (element-names %bundle-interface-element-names)
  (element-properties %bundle-interface-element-properties))

(define (bundle-interface-predicate interface)
  (tag->predicate (bundle-interface-tag interface)))

(define (bundle-interface-element-names interface)
  (vector->list (%bundle-interface-element-names interface)))

(define (bundle-interface-element-properties interface name)
  (map list-copy
       (vector-ref (%bundle-interface-element-properties interface)
		   (element-index interface name #t))))

(define (element-index interface name required?)
  (let ((index
	 (let ((v (%bundle-interface-element-names interface)))
	   (let loop ((start 0) (end (vector-length v)))
	     (and (fix:< start end)
		  (let* ((midpoint (fix:quotient (fix:+ start end) 2))
			 (name* (vector-ref v midpoint)))
		    (cond ((symbol<? name name*) (loop start midpoint))
			  ((symbol<? name* name) (loop (fix:+ midpoint 1) end))
			  (else midpoint))))))))
    (if (not (or index (not required?)))
	(error "Unknown element name:" name interface))
    index))

(define (make-bundle interface alist)
  (guarantee bundle-alist? alist 'make-bundle)
  (make-entity (lambda (self operator . args)
		 (apply (bundle-ref self operator) args))
	       (make-bundle-metadata interface
				     (bundle-alist->values interface alist))))

(define (bundle-alist->values interface alist)
  (let ((n (vector-length (%bundle-interface-element-names interface))))
    (if (not (fix:= (length alist) n))
	(error "Bundle alist doesn't match its elements:" alist interface))
    (let ((values (make-vector n)))
      (for-each (lambda (p)
		  (vector-set! values
			       (element-index interface (car p) #t)
			       (cdr p)))
		alist)
      values)))

(define (bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
		(symbol? (car p)))
	      object)
       (alist-has-unique-keys? object)))
(register-predicate! bundle-alist? 'bundle-alist '<= alist?)

(define-record-type <bundle-metadata>
    (make-bundle-metadata interface values)
    bundle-metadata?
  (interface bundle-metadata-interface)
  (values bundle-metadata-values))

(define (bundle? object)
  (and (entity? object)
       (bundle-metadata? (entity-extra object))))

(defer-boot-action 'predicate-registrations
  (lambda ()
    (register-predicate! bundle? 'bundle '<= entity?)))

(define (bundle-interface bundle)
  (bundle-metadata-interface (entity-extra bundle)))

(define (%bundle-values bundle)
  (bundle-metadata-values (entity-extra bundle)))

(define (bundle-names bundle)
  (bundle-interface-element-names (bundle-interface bundle)))

(define (bundle-alist bundle)
  (map cons
       (bundle-names bundle)
       (vector->list (%bundle-values bundle))))

(define (bundle-ref bundle operator #!optional default)
  (let ((index
	 (element-index (bundle-interface bundle)
			operator
			(default-object? default))))
    (if index
	(vector-ref (%bundle-values bundle) index)
        default)))

(define (alist-has-unique-keys? alist)
  (or (null? alist)
      (and (not (any (let ((name (caar alist)))
		       (lambda (p)
			 (eq? name (car p))))
		     (cdr alist)))
	   (alist-has-unique-keys? (cdr alist)))))

(define (sort-alist alist)
  (sort alist
	(lambda (a b)
	  (symbol<? (car a) (car b)))))

(define (define-bundle-printer interface printer)
  (hash-table-set! bundle-printers interface printer))

(define-unparser-method bundle?
  (standard-unparser-method
   (lambda (bundle)
     (bundle-interface-name (bundle-interface bundle)))
   (lambda (bundle port)
     (let ((printer
	    (hash-table-ref/default bundle-printers
				    (bundle-interface bundle)
				    #f)))
       (if printer
	   (printer bundle port))))))

(define-pp-describer bundle?
  (lambda (bundle)
    (map (lambda (name)
	   (list name (bundle-ref bundle name)))
	 (bundle-names bundle))))

(define-deferred bundle-printers
  (make-key-weak-eqv-hash-table))