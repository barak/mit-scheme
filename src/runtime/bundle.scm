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

;;; A bundle is a set of named procedures implemented as a procedure.  When
;;; called, the first argument to the bundle is a symbol identifying the named
;;; procedure to call, and the rest of the bundle's arguments are passed to the
;;; selected procedure.

;;; Each bundle also carries a type that can be used to identify it.  Normally
;;; the type is shared between bundles with the same general structure.

(declare (usual-integrations))

(define (make-bundle-type name #!optional parent-type)
  (let ((type
	 (new-make-record-type name
			       '()
			       (if (default-object? parent-type)
				   <bundle>
				   (guarantee bundle-type? parent-type
					      'make-bundle-type)))))
    (set-record-type-applicator! type %bundle-applicator)
    type))

(define (%bundle-applicator bundle operator . args)
  (apply (bundle-ref bundle operator) args))

(define (bundle-type? object)
  (and (record-type? object)
       (predicate<= (record-predicate object) bundle?)))
(register-predicate! bundle-type? 'bundle-type '<= record-type?)

(define-unparser-method bundle-type?
  (simple-unparser-method 'bundle-type
    (lambda (type)
      (list (dispatch-tag-name type)))))

(define (bundle-predicate type)
  (guarantee bundle-type? type 'bundle-predicate)
  (record-predicate type))

(define (alist->bundle type alist)
  (guarantee bundle-type? type 'alist->bundle)
  (guarantee %bundle-alist? alist 'alist->bundle)
  ((record-constructor type) (alist-copy alist)))

(define (%bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
                (symbol? (car p)))
              object)))

(define-record-type <bundle>
    (%unused% alist) ;change to #f after 9.3 release
    bundle?
  (alist bundle-alist))

(define-unparser-method bundle?
  (standard-unparser-method
   (lambda (bundle)
     (dispatch-tag-name (bundle-type bundle)))
   (lambda (bundle port)
     (let ((handler (bundle-ref bundle 'write-self #f)))
       (if handler
	   (handler port))))))

(define-pp-describer bundle?
  (lambda (bundle)
    (let ((handler (bundle-ref bundle 'describe-self #f)))
      (if handler
	  (handler)
	  (map (lambda (p) `(,(car p) ,(cdr p)))
	       (bundle-alist bundle))))))

(define (bundle-type bundle)
  (guarantee bundle? bundle 'bundle-type)
  (record-type-descriptor bundle))

(define (bundle->alist bundle)
  (alist-copy (bundle-alist bundle)))

(define (bundle-names bundle)
  (map car (bundle-alist bundle)))

(define (bundle-ref bundle operator #!optional default)
  (let ((p (assq operator (bundle-alist bundle))))
    (if p
        (cdr p)
        (begin
          (if (default-object? default)
              (error "Unknown bundle operator:" operator))
          default))))