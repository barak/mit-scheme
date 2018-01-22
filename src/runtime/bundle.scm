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
  (letrec*
      ((predicate
	(lambda (object)
	  (and (bundle? object)
	       (eq? tag (%bundle-tag object)))))
       (tag
	(make-bundle-interface-tag name
				   predicate
				   (list->vector (map element-name elements))
				   (list->vector
				    (map (lambda (element)
					   (map list-copy
						(element-properties element)))
					 elements)))))
    predicate))

(define (elements? object)
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
       (no-duplicate-keys? object element-name)))
(register-predicate! elements? 'interface-elements)

(define (element-name element)
  (if (symbol? element)
      element
      (car element)))

(define (element-properties element)
  (if (symbol? element)
      '()
      (cdr element)))

(define bundle-interface-tag?)
(define make-bundle-interface-tag)
(add-boot-init!
 (lambda ()
   (let ((metatag (make-dispatch-metatag 'bundle-interface)))
     (set! bundle-interface-tag? (dispatch-tag->predicate metatag))
     (set! make-bundle-interface-tag
	   (dispatch-metatag-constructor metatag 'make-bundle-interface))
     unspecific)))

(define (bundle-interface? object)
  (and (predicate? object)
       (bundle-interface-tag? (predicate->dispatch-tag object))))

(define-integrable (tag-element-names tag)
  (dispatch-tag-extra tag 0))

(define-integrable (tag-element-properties tag)
  (dispatch-tag-extra tag 1))

(define (bundle-interface-name interface)
  (guarantee bundle-interface? interface 'bundle-interface-name)
  (dispatch-tag-name (predicate->dispatch-tag interface)))

(define (bundle-interface-element-names interface)
  (guarantee bundle-interface? interface 'bundle-interface-element-names)
  (vector->list (tag-element-names (predicate->dispatch-tag interface))))

(define (bundle-interface-element-properties interface name)
  (guarantee bundle-interface? interface 'bundle-interface-element-properties)
  (let ((tag (predicate->dispatch-tag interface)))
    (map list-copy
	 (vector-ref (tag-element-properties tag)
		     (element-index tag name #t)))))

(define (element-index tag name required?)
  (let ((index (vector-find-next-element (tag-element-names tag) name)))
    (if (not (or index (not required?)))
	(error "Unknown element name:" name (dispatch-tag->predicate tag)))
    index))

(define (bundle? object)
  (and (entity? object)
       (let ((extra (entity-extra object)))
	 (and (vector? extra)
	      (fix:= 2 (vector-length extra))
	      (bundle-interface-tag? (vector-ref extra 0))))))

(define (%make-bundle tag values)
  (make-entity (lambda (self operator . args)
		 (apply (bundle-ref self operator) args))
	       (vector tag values)))

(define-integrable (%bundle-tag bundle)
  (vector-ref (entity-extra bundle) 0))

(define-integrable (%bundle-values bundle)
  (vector-ref (entity-extra bundle) 1))

(define (bundle-interface bundle)
  (guarantee bundle? bundle 'bundle-interface)
  (dispatch-tag->predicate (%bundle-tag bundle)))

(define (bundle-names bundle)
  (guarantee bundle? bundle 'bundle-names)
  (vector->list (tag-element-names (%bundle-tag bundle))))

(define (bundle->alist bundle)
  (guarantee bundle? bundle 'bundle->alist)
  (map cons
       (vector->list (tag-element-names (%bundle-tag bundle)))
       (vector->list (%bundle-values bundle))))

(define (bundle-ref bundle operator #!optional default)
  (guarantee bundle? bundle 'bundle-ref)
  (let ((index
	 (element-index (%bundle-tag bundle)
			operator
			(default-object? default))))
    (if index
	(vector-ref (%bundle-values bundle) index)
        default)))

(define-unparser-method bundle?
  (standard-unparser-method
   (lambda (bundle)
     (dispatch-tag-name (%bundle-tag bundle)))
   #f))

(define-pp-describer bundle?
  (lambda (bundle)
    (map (lambda (name)
	   (list name (bundle-ref bundle name)))
	 (bundle-names bundle))))

(define (bundle-constructor interface)
  (guarantee bundle-interface? interface 'bundle-constructor)
  (let* ((tag (predicate->dispatch-tag interface))
	 (n (vector-length (tag-element-names tag))))
    (let-syntax
	((expand-cases
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((limit (cadr form))
		   (default (caddr form))
		   (make-name
		    (lambda (i)
		      (intern (string-append "v" (number->string i))))))
	       (let loop ((i 0) (names '()))
		 (if (fix:< i limit)
		     `(if (fix:= n ,i)
			  (lambda (,@names) (%make-bundle tag (vector ,@names)))
			  ,(loop (fix:+ i 1)
				 (append names (list (make-name i)))))
		     default)))))))
      (expand-cases 16
	(letrec
	    ((constructor
	      (lambda args
		(if (not (fix:= n (length args)))
		    (error:wrong-number-of-arguments constructor n args))
		(%make-bundle interface (list->vector args)))))
	  constructor)))))

(define (bundle-accessor interface name)
  (guarantee bundle-interface? interface 'bundle-accessor)
  (let ((index (element-index (predicate->dispatch-tag interface) name #t)))
    (lambda (bundle)
      (guarantee interface bundle)
      (vector-ref (%bundle-values bundle) index))))

(define (alist->bundle interface alist)
  (guarantee bundle-interface? interface 'alist->bundle)
  (guarantee bundle-alist? alist 'alist->bundle)
  (let* ((tag (predicate->dispatch-tag interface))
	 (n (vector-length (tag-element-names tag))))
    (if (not (fix:= (length alist) n))
	(error "Bundle alist doesn't match its elements:" alist interface))
    (let ((values (make-vector n)))
      (for-each (lambda (p)
		  (vector-set! values
			       (element-index tag (car p) #t)
			       (cdr p)))
		alist)
      (%make-bundle tag values))))

(define (bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
		(symbol? (car p)))
	      object)
       (no-duplicate-keys? object car)))
(register-predicate! bundle-alist? 'bundle-alist '<= alist?)

(define (alist-has-unique-keys? alist)
  (no-duplicate-keys? alist car))

(define (no-duplicate-keys? items get-key)
  (or (null? items)
      (and (not (any (let ((key (get-key (car items))))
		       (lambda (item)
			 (eq? key (get-key item))))
		     (cdr items)))
	   (no-duplicate-keys? (cdr items) get-key))))