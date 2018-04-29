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

;;; Each bundle also carries a predicate that can be used to identify it.
;;; Normally the predicate is shared between bundles with the same general
;;; structure.

(declare (usual-integrations))

(define (make-bundle-predicate name)
  (letrec ((predicate
            (lambda (object)
              (and (bundle? object)
                   (eq? predicate (bundle-predicate object))))))
    (register-predicate! predicate name '<= bundle?)
    predicate))

(define (bundle-predicate? object)
  (and (predicate? object)
       (predicate<= object bundle?)))
(register-predicate! bundle-predicate? 'bundle-predicate)

;; Defer this because predicate? will change later in the cold load.
(defer-boot-action 'predicate-relations
  (lambda ()
    (set-predicate<=! bundle-predicate? predicate?)))

(define (alist->bundle predicate alist)
  (guarantee bundle-predicate? predicate 'alist->bundle)
  (guarantee bundle-alist? alist 'alist->bundle)
  (%make-bundle predicate (alist-copy alist)))

(define (bundle-alist? object)
  (and (alist? object)
       (every (lambda (p)
                (symbol? (car p)))
              object)))

(define-record-type <bundle>
    (%make-bundle predicate alist)
    bundle?
  (predicate bundle-predicate)
  (alist bundle-alist))

(set-record-type-applicator! <bundle>
  (lambda (bundle operator . args)
    (apply (bundle-ref bundle operator) args)))

(define-unparser-method bundle?
  (standard-unparser-method
   (lambda (bundle)
     (predicate-name (bundle-predicate bundle)))
   (lambda (bundle port)
     (let ((handler (bundle-ref bundle 'write-self #f)))
       (if handler
	   (handler port))))))

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