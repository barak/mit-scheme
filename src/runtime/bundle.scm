#| -*- Mode: Scheme; keyword-style: none -*-

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

;;;; Bundles

;;; A bundle is a set of named procedures implemented as a procedure.  When
;;; called, the first argument to the bundle is a symbol identifying the named
;;; procedure to call, and the rest of the bundle's arguments are passed to the
;;; selected procedure.

;;; Each bundle also carries a type that can be used to identify it.  Normally
;;; the type is shared between bundles with the same general structure.

;;; Old-style bundles are created by a combination of the make-bundle-predicate
;;; procedure and the bundle macro.  They store their bindings in an alist and
;;; access is done by calling the bundle as above or using the bundle-ref
;;; procedure.

;;; New-style bundles are created by the define-bundle macro.  They store their
;;; bindings as record fields.  The macro defines a procedure for each field,
;;; which accepts the bundle as the first argument and calls the field's
;;; procedure with the remaining arguments.

(declare (usual-integrations))

;;; New style

(define <bundle>
  (make-record-type 'bundle '()
    'applicator
    (lambda (b name . args)
      (apply ((record-accessor (record-type-descriptor b) name) b)
	     args))))

(define %new-bundle?
  (record-predicate <bundle>))

(define-integrable (%new-bundle->alist bundle)
  (let ((rtd (record-type-descriptor bundle)))
    (map (lambda (name)
	   (cons name ((record-accessor rtd name) bundle)))
	 (record-type-field-names rtd))))

(define-integrable (%new-bundle-names bundle)
  (record-type-field-names (record-type-descriptor bundle)))

;;;; Old style

(define <old-bundle>
  (make-record-type 'old-bundle '(alist)
		    'applicator
		    (lambda (b name . args)
		      (apply (bundle-ref b name) args))))

(define %old-bundle?
  (record-predicate <old-bundle>))

(define %old-bundle-alist
  (record-accessor <old-bundle> 'alist))

(define-print-method %old-bundle?
  (standard-print-method
      (lambda (bundle)
	(record-type-name (record-type-descriptor bundle)))
    (lambda (bundle)
      (let ((handler (bundle-ref bundle 'summarize-self #f)))
	(if handler
	    (handler)
	    '())))))

(define-pp-describer %old-bundle?
  (lambda (bundle)
    (let ((handler (bundle-ref bundle 'describe-self #f)))
      (if handler
	  (handler)
	  (map (lambda (p) `(,(car p) ,(cdr p)))
	       (%old-bundle-alist bundle))))))

(define-integrable (make-bundle-predicate name)
  (record-predicate (make-record-type name '() <old-bundle>)))

(define (bundle-predicate bundle)
  (guarantee %old-bundle? bundle 'bundle-predicate)
  (record-predicate (record-type-descriptor bundle)))

(define (bundle-predicate? object)
  (and (predicate? object)
       (predicate<= object %old-bundle?)))

(seq:after-predicate 'add-action!
  (lambda ()
    (register-predicate! bundle-predicate? 'bundle-predicate '<= predicate?)))

(define-integrable (%predicate->record-type predicate)
  (predicate->dispatch-tag predicate))

(define %bundle-predicate->record-type
  %predicate->record-type)

(seq:after-predicate 'add-action!
  (lambda ()
    (set! %bundle-predicate->record-type
	  (named-lambda (%bundle-predicate->record-type predicate)
	    (guarantee bundle-predicate? predicate 'alist->bundle)
	    (%predicate->record-type predicate)))
    unspecific))

(define (alist->bundle predicate alist)
  (guarantee %bundle-alist? alist 'alist->bundle)
  ((record-constructor
    (if predicate
	(%bundle-predicate->record-type predicate)
	<old-bundle>))
   (alist-copy alist)))

(define (%bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
                (symbol? (car p)))
              object)))

;;; Either style, mostly for backward compatibility

(define (bundle? object)
  (or (%old-bundle? object)
      (%new-bundle? object)))

(define (bundle->alist bundle)
  (cond ((%old-bundle? bundle)
	 (alist-copy (%old-bundle-alist bundle)))
	((%new-bundle? bundle)
	 (%new-bundle->alist bundle))
	(else
	 (error:not-a bundle? bundle 'bundle->alist))))

(define (bundle-names bundle)
  (cond ((%old-bundle? bundle)
	 (map car (%old-bundle-alist bundle)))
	((%new-bundle? bundle)
	 (%new-bundle-names bundle))
	(else
	 (error:not-a bundle? bundle 'bundle-name))))

(define (bundle-ref bundle name #!optional get-default)
  (guarantee symbol? name 'bundle-ref)
  (let ((get-default
	 (cond ((not get-default)
		(lambda () #f))
	       ((default-object? get-default)
		(lambda ()
		  (error "Unknown bundle name:" name)))
	       (else
		get-default))))
    (let ((p (assq name (bundle->alist bundle))))
      (if p
	  (cdr p)
	  (get-default)))))

(define (bundle-ref/default bundle name #!optional default)
  (bundle-ref bundle
	      name
	      (if (default-object? default)
		  default
		  (lambda () default))))

(define (bundle-map predicate procedure . bundles)
  (bundle-map* predicate procedure bundles))

(define (bundle-map* predicate procedure bundles)
  (alist->bundle
   predicate
   (filter-map (lambda (name)
                 (let ((value
                        (apply procedure
                               name
                               (map (lambda (bundle)
                                      (bundle-ref bundle name default-object))
                                    bundles))))
                   (and (not (default-object? value))
			(cons name value))))
	       (apply lset-union eq? (map bundle-names bundles)))))

(define (bundle-combine predicate combiner . bundles)
  (bundle-combine* predicate combiner bundles))

(define (bundle-combine* predicate combiner bundles)
  (bundle-map* predicate
	       (lambda (name . vals)
		 (let ((vals (remove default-object? vals)))
		   (if (pair? (cdr vals))
		       (combiner name vals)
		       (car vals))))
	       bundles))

(define (bundle-combiner:first name vals)
  (declare (ignore name))
  (car vals))