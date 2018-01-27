#| -*-Scheme-*-

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

;;;; Syntactic Environments

(declare (usual-integrations))

(define syntactic-environment?
  (make-bundle-interface 'syntactic-environment
			 '(get-type get-runtime lookup store rename)))
(define make-senv (bundle-constructor syntactic-environment?))
(define senv-get-type (bundle-accessor syntactic-environment? 'get-type))
(define senv-get-runtime (bundle-accessor syntactic-environment? 'get-runtime))
(define senv-lookup (bundle-accessor syntactic-environment? 'lookup))
(define senv-store (bundle-accessor syntactic-environment? 'store))
(define senv-rename (bundle-accessor syntactic-environment? 'rename))

(define (senv-type senv)
  ((senv-get-type senv)))

(define (syntactic-environment/top-level? senv)
  (memq (senv-type senv) '(top-level runtime-top-level)))

(define (syntactic-environment->environment senv)
  ((senv-get-runtime senv)))

(define (syntactic-environment/lookup senv identifier)
  (guarantee raw-identifier? identifier 'syntactic-environment/lookup)
  ((senv-lookup senv) identifier))

(define (syntactic-environment/reserve senv identifier)
  (guarantee raw-identifier? identifier 'syntactic-environment/reserve)
  ((senv-store senv) identifier (make-reserved-name-item)))

(define (syntactic-environment/bind-keyword senv identifier item)
  (guarantee raw-identifier? identifier 'syntactic-environment/bind-keyword)
  (guarantee keyword-item? item 'syntactic-environment/bind-keyword)
  ((senv-store senv) identifier item))

(define (syntactic-environment/bind-variable senv identifier)
  (guarantee raw-identifier? identifier 'syntactic-environment/bind-variable)
  (let ((rename ((senv-rename senv) identifier)))
    ((senv-store senv) identifier (make-variable-item rename))
    rename))

(define (->syntactic-environment object #!optional caller)
  (declare (ignore caller))
  (cond ((syntactic-environment? object) object)
	((environment? object) (%make-runtime-syntactic-environment object))
	(else (error "Unable to convert to a syntactic environment:" object))))

;;; Runtime syntactic environments are wrappers around runtime environments.
;;; They maintain their own bindings, but can defer lookups of syntactic
;;; keywords to the given runtime environment.

(define (%make-runtime-syntactic-environment env)

  (define (get-type)
    (if (interpreter-environment? env) 'runtime-top-level 'runtime))

  (define (get-runtime)
    env)

  (define (lookup identifier)
    (and (symbol? identifier)
	 (environment-lookup-macro env identifier)))

  (define (store identifier item)
    (environment-define-macro env identifier item))

  (define (rename identifier)
    identifier)

  (make-senv get-type get-runtime lookup store rename))

;;; Keyword environments are used to make keywords that represent items.

(define (make-keyword-syntactic-environment name item)

  (define (get-type)
    'keyword)

  (define (get-runtime)
    (error "Can't evaluate in keyword environment."))

  (define (lookup identifier)
    (and (eq? name identifier)
	 item))

  (define (store identifier item)
    (error "Can't bind in keyword environment:" identifier item))

  (define (rename identifier)
    (error "Can't rename in keyword environment:" identifier))

  (guarantee raw-identifier? name 'make-keyword-environment)
  (guarantee keyword-item? item 'make-keyword-environment)
  (make-senv get-type get-runtime lookup store rename))

;;; Top-level syntactic environments represent top-level environments.
;;; They are always layered over a runtime syntactic environment.

(define (make-top-level-syntactic-environment parent)
  (guarantee syntactic-environment? parent
	     'make-top-level-syntactic-environment)
  (if (not (memq (senv-type parent) '(runtime-top-level top-level)))
      (error:bad-range-argument parent 'make-top-level-syntactic-environment))
  (let ((bound '())
	(get-runtime (senv-get-runtime parent)))

    (define (get-type)
      'top-level)

    (define (lookup identifier)
      (let ((binding (assq identifier bound)))
	(if binding
	    (cdr binding)
	    ((senv-lookup parent) identifier))))

    (define (store identifier item)
      (let ((binding (assq identifier bound)))
	(if binding
	    (set-cdr! binding item)
	    (begin
	      (set! bound (cons (cons identifier item) bound))
	      unspecific))))

    (define (rename identifier)
      identifier)

    (make-senv get-type get-runtime lookup store rename)))

;;; Internal syntactic environments represent environments created by
;;; procedure application.

(define (make-internal-syntactic-environment parent)
  (guarantee syntactic-environment? parent 'make-internal-syntactic-environment)
  (let ((bound '())
	(free '())
	(get-runtime (senv-get-runtime parent))
	(rename (make-local-identifier-renamer)))

    (define (get-type)
      'internal)

    (define (lookup identifier)
      (let ((binding
	     (or (assq identifier bound)
		 (assq identifier free))))
	(if binding
	    (cdr binding)
	    (let ((item ((senv-lookup parent) identifier)))
	      (set! free (cons (cons identifier item) free))
	      item))))

    (define (store identifier item)
      (cond ((assq identifier bound)
	     => (lambda (binding)
		  (set-cdr! binding item)))
	    ((assq identifier free)
	     (if (reserved-name-item? item)
		 (syntax-error "Premature reference to reserved name:"
			       identifier)
		 (error "Can't define name; already free:" identifier)))
	    (else
	     (set! bound (cons (cons identifier item) bound))
	     unspecific)))

    (make-senv get-type get-runtime lookup store rename)))

;;; Partial syntactic environments are used to implement syntactic
;;; closures that have free names.

(define (make-partial-syntactic-environment free-ids free-senv bound-senv)
  (let ((caller 'make-partial-syntactic-environment))
    (guarantee list-of-unique-symbols? free-ids caller)
    (guarantee syntactic-environment? free-senv caller)
    (guarantee syntactic-environment? bound-senv caller))
  (if (or (null? free-ids)
	  (eq? free-senv bound-senv))
      bound-senv
      (let ()
	(define (get-type)
	  'partial)

	(define (get-runtime)
	  ;; **** Shouldn't this be a syntax error?  It can happen as the
	  ;; result of a partially-closed transformer.  ****
	  (error "Can't evaluate in partial syntactic environment"))

	(define (lookup identifier)
	  ((senv-lookup (select-env identifier)) identifier))

	(define (store identifier item)
	  ;; **** Shouldn't this be a syntax error?  It can happen as the
	  ;; result of a misplaced definition.  ****
	  (error "Can't bind identifier in partial syntactic environment:"
		 identifier item))

	(define (rename identifier)
	  ((senv-rename (select-env identifier)) identifier))

	(define (select-env identifier)
	  (if (memq identifier free-ids) free-senv bound-senv))

	(make-senv get-type get-runtime lookup store rename))))