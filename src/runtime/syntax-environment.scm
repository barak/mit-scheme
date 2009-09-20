#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

(define (syntactic-environment? object)
  (or (internal-syntactic-environment? object)
      (top-level-syntactic-environment? object)
      (environment? object)
      (partial-syntactic-environment? object)
      (null-syntactic-environment? object)))

(define-guarantee syntactic-environment "syntactic environment")

(define (syntactic-environment/top-level? object)
  (or (top-level-syntactic-environment? object)
      (interpreter-environment? object)))

(define (syntactic-environment/lookup environment name)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/lookup environment name))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/lookup environment name))
	((environment? environment)
	 (and (symbol? name)
	      (environment/lookup environment name)))
	((partial-syntactic-environment? environment)
	 (partial-syntactic-environment/lookup environment name))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/lookup environment name))
	(else
	 (error:not-syntactic-environment environment
					  'SYNTACTIC-ENVIRONMENT/LOOKUP))))

(define (syntactic-environment/define environment name item)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/define environment name item))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/define environment name item))
	((environment? environment)
	 (environment/define environment name item))
	((partial-syntactic-environment? environment)
	 (partial-syntactic-environment/define environment name item))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/define environment name item))
	(else
	 (error:not-syntactic-environment environment
					  'SYNTACTIC-ENVIRONMENT/DEFINE))))

(define (syntactic-environment/rename environment name)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment/rename environment name))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment/rename environment name))
	((environment? environment)
	 (environment/rename environment name))
	((partial-syntactic-environment? environment)
	 (partial-syntactic-environment/rename environment name))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment/rename environment name))
	(else
	 (error:not-syntactic-environment environment
					  'SYNTACTIC-ENVIRONMENT/RENAME))))

(define (syntactic-environment->environment environment)
  (cond ((internal-syntactic-environment? environment)
	 (internal-syntactic-environment->environment environment))
	((top-level-syntactic-environment? environment)
	 (top-level-syntactic-environment->environment environment))
	((environment? environment)
	 environment)
	((partial-syntactic-environment? environment)
	 (partial-syntactic-environment->environment environment))
	((null-syntactic-environment? environment)
	 (null-syntactic-environment->environment environment))
	(else
	 (error:not-syntactic-environment
	  environment
	  'SYNTACTIC-ENVIRONMENT->ENVIRONMENT))))

(define (bind-variable! environment name)
  (let ((rename (syntactic-environment/rename environment name)))
    (syntactic-environment/define environment
				  name
				  (make-variable-item rename))
    rename))

;;; Null syntactic environments signal an error for any operation.
;;; They are used as the definition environment for expressions (to
;;; prevent illegal use of definitions) and to seal off environments
;;; used in magic keywords.

(define-record-type <null-syntactic-environment>
    (%make-null-syntactic-environment)
    null-syntactic-environment?)

(define null-syntactic-environment
  (%make-null-syntactic-environment))

(define (null-syntactic-environment/lookup environment name)
  environment
  (error "Can't lookup name in null syntactic environment:" name))

(define (null-syntactic-environment/define environment name item)
  environment
  (error "Can't bind name in null syntactic environment:" name item))

(define (null-syntactic-environment/rename environment name)
  environment
  (error "Can't rename name in null syntactic environment:" name))

(define (null-syntactic-environment->environment environment)
  environment
  (error "Can't evaluate in null syntactic environment."))

;;; Runtime environments can be used to look up keywords, but can't be
;;; modified.

(define (environment/lookup environment name)
  (let ((item (environment-lookup-macro environment name)))
    (if (procedure? item)
	;; **** Kludge to support bootstrapping.
	(non-hygienic-macro-transformer->expander item environment)
	item)))

(define (environment/define environment name item)
  (environment-define-macro environment name item))

(define (environment/rename environment name)
  environment
  (rename-top-level-identifier name))

;;; Top-level syntactic environments represent top-level environments.
;;; They are always layered over a real syntactic environment.

(define-record-type <top-level-syntactic-environment>
    (%make-top-level-syntactic-environment parent bound)
    top-level-syntactic-environment?
  (parent top-level-syntactic-environment/parent)
  (bound top-level-syntactic-environment/bound
	 set-top-level-syntactic-environment/bound!))

(define (make-top-level-syntactic-environment parent)
  (guarantee-syntactic-environment parent
				   'MAKE-TOP-LEVEL-SYNTACTIC-ENVIRONMENT)
  (if (not (or (syntactic-environment/top-level? parent)
	       (null-syntactic-environment? parent)))
      (error:bad-range-argument parent "top-level syntactic environment"
				'MAKE-TOP-LEVEL-SYNTACTIC-ENVIRONMENT))
  (%make-top-level-syntactic-environment parent '()))

(define (top-level-syntactic-environment/lookup environment name)
  (let ((binding
	 (assq name (top-level-syntactic-environment/bound environment))))
    (if binding
	(cdr binding)
	(syntactic-environment/lookup
	 (top-level-syntactic-environment/parent environment)
	 name))))

(define (top-level-syntactic-environment/define environment name item)
  (let ((bound (top-level-syntactic-environment/bound environment)))
    (let ((binding (assq name bound)))
      (if binding
	  (set-cdr! binding item)
	  (set-top-level-syntactic-environment/bound!
	   environment
	   (cons (cons name item) bound))))))

(define (top-level-syntactic-environment/rename environment name)
  environment
  (rename-top-level-identifier name))

(define (top-level-syntactic-environment->environment environment)
  (syntactic-environment->environment
   (top-level-syntactic-environment/parent environment)))

;;; Internal syntactic environments represent environments created by
;;; procedure application.

(define-record-type <internal-syntactic-environment>
    (%make-internal-syntactic-environment parent bound free rename-state)
    internal-syntactic-environment?
  (parent internal-syntactic-environment/parent)
  (bound internal-syntactic-environment/bound
	 set-internal-syntactic-environment/bound!)
  (free internal-syntactic-environment/free
	set-internal-syntactic-environment/free!)
  (rename-state internal-syntactic-environment/rename-state))

(define (make-internal-syntactic-environment parent)
  (guarantee-syntactic-environment parent 'MAKE-INTERNAL-SYNTACTIC-ENVIRONMENT)
  (%make-internal-syntactic-environment parent '() '() (make-rename-id)))

(define (internal-syntactic-environment/lookup environment name)
  (let ((binding
	 (or (assq name (internal-syntactic-environment/bound environment))
	     (assq name (internal-syntactic-environment/free environment)))))
    (if binding
	(cdr binding)
	(let ((item
	       (syntactic-environment/lookup
		(internal-syntactic-environment/parent environment)
		name)))
	  (set-internal-syntactic-environment/free!
	   environment
	   (cons (cons name item)
		 (internal-syntactic-environment/free environment)))
	  item))))

(define (internal-syntactic-environment/define environment name item)
  (cond ((assq name (internal-syntactic-environment/bound environment))
	 => (lambda (binding)
	      (set-cdr! binding item)))
	((assq name (internal-syntactic-environment/free environment))
	 (if (reserved-name-item? item)
	     (syntax-error "Premature reference to reserved name:" name)
	     (error "Can't define name; already free:" name)))
	(else
	 (set-internal-syntactic-environment/bound!
	  environment
	  (cons (cons name item)
		(internal-syntactic-environment/bound environment))))))

(define (internal-syntactic-environment/rename environment name)
  (rename-identifier
   name
   (internal-syntactic-environment/rename-state environment)))

(define (internal-syntactic-environment->environment environment)
  (syntactic-environment->environment
   (internal-syntactic-environment/parent environment)))

;;; Partial syntactic environments are used to implement syntactic
;;; closures that have free names.

(define-record-type <partial-syntactic-environment>
    (%make-partial-syntactic-environment names
					 names-environment
					 else-environment)
    partial-syntactic-environment?
  (names partial-syntactic-environment/names)
  (names-environment partial-syntactic-environment/names-environment)
  (else-environment partial-syntactic-environment/else-environment))

(define (make-partial-syntactic-environment names
					    names-environment
					    else-environment)
  (if (or (null? names)
	  (eq? names-environment else-environment))
      else-environment
      (%make-partial-syntactic-environment names
					   names-environment
					   else-environment)))

(define (partial-syntactic-environment/lookup environment name)
  (syntactic-environment/lookup
   (if (memq name (partial-syntactic-environment/names environment))
       (partial-syntactic-environment/names-environment environment)
       (partial-syntactic-environment/else-environment environment))
   name))

(define (partial-syntactic-environment/define environment name item)
  ;; **** Shouldn't this be a syntax error?  It can happen as the
  ;; result of a misplaced definition.  ****
  (error "Can't bind name in partial syntactic environment:"
	 environment name item))

(define (partial-syntactic-environment/rename environment name)
  (syntactic-environment/rename
   (if (memq name (partial-syntactic-environment/names environment))
       (partial-syntactic-environment/names-environment environment)
       (partial-syntactic-environment/else-environment environment))
   name))

(define (partial-syntactic-environment->environment environment)
  ;; **** Shouldn't this be a syntax error?  It can happen as the
  ;; result of a partially-closed transformer.  ****
  (error "Can't evaluate in partial syntactic environment:" environment))