#| -*- Mode: Scheme; keyword-style: none -*-

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

;;;; Bundles

;;; A bundle is a set of named procedures implemented as a procedure.  When
;;; called, the first argument to the bundle is a symbol identifying the named
;;; procedure to call, and the rest of the bundle's arguments are passed to the
;;; selected procedure.

;;; Each bundle also carries a type that can be used to identify it.  Normally
;;; the type is shared between bundles with the same general structure.

(declare (usual-integrations))

(define (make-bundle-predicate name)
  (let ((type (new-make-record-type name '() <bundle>)))
    (set-record-type-applicator! type %bundle-applicator)
    (record-predicate type)))

(define (%bundle-applicator bundle name . args)
  (apply (bundle-ref bundle name) args))

(define-integrable (%predicate->record-type predicate)
  (predicate->dispatch-tag predicate))

(define (bundle-predicate? object)
  (and (predicate? object)
       (predicate<= object bundle?)))

(defer-boot-action 'predicate-relations
  (lambda ()
    (register-predicate! bundle-predicate? 'bundle-predicate '<= predicate?)))

(define (alist->bundle predicate alist)
  (guarantee %bundle-alist? alist 'alist->bundle)
  ((record-constructor
    (if predicate
	(%bundle-predicate->record-type predicate)
	<bundle>))
   (alist-copy alist)))

(define %bundle-predicate->record-type
  %predicate->record-type)

(defer-boot-action 'predicate-relations
  (lambda ()
    (set! %bundle-predicate->record-type
	  (named-lambda (%bundle-predicate->record-type predicate)
	    (guarantee bundle-predicate? predicate 'alist->bundle)
	    (%predicate->record-type predicate)))
    unspecific))

(define (%bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
                (symbol? (car p)))
              object)))

(define <bundle>
  (new-make-record-type '<bundle> '(alist)))

(defer-boot-action 'record-procedures
  (lambda ()
    (set-record-type-applicator! <bundle> %bundle-applicator)))

(define bundle?
  (record-predicate <bundle>))

(define bundle-alist
  (record-accessor <bundle> 'alist))

(define-print-method bundle?
  (standard-print-method
      (lambda (bundle)
	(record-type-name (record-type-descriptor bundle)))
    (lambda (bundle)
      (let ((handler (bundle-ref bundle 'summarize-self #f)))
	(if handler
	    (handler)
	    '())))))

(define-pp-describer bundle?
  (lambda (bundle)
    (let ((handler (bundle-ref bundle 'describe-self #f)))
      (if handler
	  (handler)
	  (map (lambda (p) `(,(car p) ,(cdr p)))
	       (bundle-alist bundle))))))

(define (bundle-predicate bundle)
  (guarantee bundle? bundle 'bundle-type)
  (record-predicate (record-type-descriptor bundle)))

(define (bundle->alist bundle)
  (alist-copy (bundle-alist bundle)))

(define (bundle-names bundle)
  (map car (bundle-alist bundle)))

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
    (let ((p (assq name (bundle-alist bundle))))
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