#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

(define-record-type <syntactic-environment>
    (make-senv ops state)
    syntactic-environment?
  (ops senv-ops)
  (state senv-state))

(define-guarantee syntactic-environment "syntactic environment")

(define-record-type <senv-ops>
    (make-senv-ops type lookup define rename ->environment)
    senv-ops?
  (type senv-ops:type)
  (lookup senv-ops:lookup)
  (define senv-ops:define)
  (rename senv-ops:rename)
  (->environment senv-ops:->environment))

(define (->syntactic-environment object #!optional caller)
  (cond ((environment? object)
	 (runtime-environment->syntactic-environment object))
	((syntactic-environment? object)
	 object)
	(else
	 (error:not-syntactic-environment object caller))))

(define (senv-type senv)
  ((senv-ops:type (senv-ops senv)) (senv-state senv)))

(define (syntactic-environment/top-level? senv)
  (let ((type (senv-type senv)))
    (or (eq? type 'top-level)
	(eq? type 'runtime-top-level))))

(define (syntactic-environment/lookup senv name)
  ((senv-ops:lookup (senv-ops senv)) (senv-state senv) name))

(define (syntactic-environment/define senv name item)
  ((senv-ops:define (senv-ops senv)) (senv-state senv) name item))

(define (syntactic-environment/rename senv name)
  ((senv-ops:rename (senv-ops senv)) (senv-state senv) name))

(define (syntactic-environment->environment senv)
  ((senv-ops:->environment (senv-ops senv)) (senv-state senv)))

(define (bind-variable! senv name)
  (let ((rename (syntactic-environment/rename senv name)))
    (syntactic-environment/define senv name (make-variable-item rename))
    rename))

;;; Null syntactic environments signal an error for any operation.
;;; They are used as the definition environment for expressions (to
;;; prevent illegal use of definitions) and to seal off environments
;;; used in magic keywords.

(define null-senv-ops
  (make-senv-ops
   (lambda (state)
     state
     'null)
   (lambda (state name)
     state
     (error "Can't lookup name in null syntactic environment:" name))
   (lambda (state name item)
     state
     (error "Can't bind name in null syntactic environment:" name item))
   (lambda (state name)
     state
     (error "Can't rename name in null syntactic environment:" name))
   (lambda (state)
     state
     (error "Can't evaluate in null syntactic environment."))))

(define null-syntactic-environment
  (make-senv null-senv-ops unspecific))

;;; Runtime environments can be used to look up keywords, but can't be
;;; modified.

(define (runtime-environment->syntactic-environment env)
  (guarantee-environment env 'environment->syntactic-environment)
  (make-senv runtime-senv-ops env))

(define runtime-senv-ops
  (make-senv-ops
   (lambda (env)
     (if (interpreter-environment? env) 'runtime-top-level 'runtime))
   (lambda (env name)
     (and (symbol? name)
	  (let ((item (environment-lookup-macro env name)))
	    (if (procedure? item)
		;; **** Kludge to support bootstrapping.
		(non-hygienic-macro-transformer->expander item env)
		item))))
   (lambda (env name item)
     (environment-define-macro env name item))
   (lambda (env name)
     env
     (rename-top-level-identifier name))
   (lambda (env)
     env)))

;;; Top-level syntactic environments represent top-level environments.
;;; They are always layered over a real syntactic environment.

(define (make-top-level-syntactic-environment parent)
  (guarantee-syntactic-environment parent 'make-top-level-syntactic-environment)
  (if (not (let ((type (senv-type parent)))
	     (or (eq? type 'top-level)
		 (eq? type 'runtime-top-level)
		 (eq? type 'null))))
      (error:bad-range-argument parent "top-level syntactic environment"
				'make-top-level-syntactic-environment))
  (make-senv tl-senv-ops (make-tl-state parent '())))

(define-record-type <tl-state>
    (make-tl-state parent bound)
    tl-state?
  (parent tl-state-parent)
  (bound tl-state-bound set-tl-state-bound!))

(define tl-senv-ops
  (make-senv-ops
   (lambda (state)
     state
     'top-level)
   (lambda (state name)
     (let ((binding (assq name (tl-state-bound state))))
       (if binding
	   (cdr binding)
	   (syntactic-environment/lookup (tl-state-parent state) name))))
   (lambda (state name item)
     (let ((bound (tl-state-bound state)))
       (let ((binding (assq name bound)))
	 (if binding
	     (set-cdr! binding item)
	     (set-tl-state-bound! state (cons (cons name item) bound))))))
   (lambda (state name)
     state
     (rename-top-level-identifier name))
   (lambda (state)
     (syntactic-environment->environment (tl-state-parent state)))))

;;; Internal syntactic environments represent environments created by
;;; procedure application.

(define (make-internal-syntactic-environment parent)
  (guarantee-syntactic-environment parent 'make-internal-syntactic-environment)
  (make-senv internal-senv-ops
	     (make-internal-state parent '() '() (make-rename-id))))

(define-record-type <internal-state>
    (make-internal-state parent bound free rename-state)
    internal-state?
  (parent internal-state-parent)
  (bound internal-state-bound set-internal-state-bound!)
  (free internal-state-free set-internal-state-free!)
  (rename-state internal-state-rename-state))

(define internal-senv-ops
  (make-senv-ops
   (lambda (state)
     state
     'internal)
   (lambda (state name)
     (let ((binding
	    (or (assq name (internal-state-bound state))
		(assq name (internal-state-free state)))))
       (if binding
	   (cdr binding)
	   (let ((item
		  (syntactic-environment/lookup (internal-state-parent state)
						name)))
	     (set-internal-state-free! state
				       (cons (cons name item)
					     (internal-state-free state)))
	     item))))
   (lambda (state name item)
     (cond ((assq name (internal-state-bound state))
	    => (lambda (binding)
		 (set-cdr! binding item)))
	   ((assq name (internal-state-free state))
	    (if (reserved-name-item? item)
		(syntax-error "Premature reference to reserved name:" name)
		(error "Can't define name; already free:" name)))
	   (else
	    (set-internal-state-bound! state
				       (cons (cons name item)
					     (internal-state-bound state))))))
   (lambda (state name)
     (rename-identifier name (internal-state-rename-state state)))
   (lambda (state)
     (syntactic-environment->environment (internal-state-parent state)))))

;;; Partial syntactic environments are used to implement syntactic
;;; closures that have free names.

(define (make-partial-syntactic-environment names names-senv else-senv)
  (guarantee-list-of-unique-symbols names 'make-partial-syntactic-environment)
  (guarantee-syntactic-environment names-senv
				   'make-partial-syntactic-environment)
  (guarantee-syntactic-environment else-senv
				   'make-partial-syntactic-environment)
  (if (or (null? names)
	  (eq? names-senv else-senv))
      else-senv
      (make-senv partial-senv-ops
		 (%make-partial-state names names-senv else-senv))))

(define-record-type <partial-state>
    (%make-partial-state names names-senv else-senv)
    partial-state?
  (names partial-state-names)
  (names-senv partial-state-names-senv)
  (else-senv partial-state-else-senv))

(define partial-senv-ops
  (make-senv-ops
   (lambda (state)
     state
     'partial)
   (lambda (state name)
     (syntactic-environment/lookup (if (memq name (partial-state-names state))
				       (partial-state-names-senv state)
				       (partial-state-else-senv state))
				   name))
   (lambda (state name item)
     ;; **** Shouldn't this be a syntax error?  It can happen as the
     ;; result of a misplaced definition.  ****
     (error "Can't bind name in partial syntactic environment:"
	    state name item))
   (lambda (state name)
     (syntactic-environment/rename (if (memq name (partial-state-names state))
				       (partial-state-names-senv state)
				       (partial-state-else-senv state))
				   name))
   (lambda (state)
     ;; **** Shouldn't this be a syntax error?  It can happen as the
     ;; result of a partially-closed transformer.  ****
     (error "Can't evaluate in partial syntactic environment:" state))))