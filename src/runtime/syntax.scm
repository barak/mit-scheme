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

;;;; Syntactic Closures
;;;  Based on a design by Alan Bawden.

;;; This is a two-stage program: the first stage classifies input
;;; expressions into types, e.g. "definition", "lambda body",
;;; "expression", etc., and the second stage compiles those classified
;;; expressions ("items") into output code.  The classification stage
;;; permits discovery of internal definitions prior to code
;;; generation.  It also identifies keywords and variables, which
;;; allows a powerful form of syntactic binding to be implemented.

;;; See "Syntactic Closures", by Alan Bawden and Jonathan Rees, in
;;; Proceedings of the 1988 ACM Conference on Lisp and Functional
;;; Programming, page 86.

(declare (usual-integrations))

;;;; Top level

(define (syntax form environment)
  (syntax* (list form) environment))

(define (syntax* forms environment)
  (guarantee list? forms 'syntax*)
  (let ((senv (->syntactic-environment environment 'syntax*)))
    (with-identifier-renaming
     (lambda ()
       (if (syntactic-environment/top-level? senv)
	   (compile-body-item/top-level
	    (classify/body forms (make-top-level-syntactic-environment senv)))
	   (output/sequence (compile/expressions forms senv)))))))

(define (compile/expression expression environment)
  (compile-item/expression (classify/expression expression environment)))

(define (compile/expressions expressions environment)
  (map (lambda (expression)
	 (compile/expression expression environment))
       expressions))

;;;; Syntactic closures

(define (close-syntax form senv)
  (make-syntactic-closure senv '() form))

(define (make-syntactic-closure senv free form)
  (guarantee syntactic-environment? senv 'make-syntactic-closure)
  (guarantee-list-of identifier? free 'make-syntactic-closure)
  (if (or (memq form free)		;LOOKUP-IDENTIFIER assumes this.
	  (constant-form? form)
	  (and (syntactic-closure? form)
	       (null? (syntactic-closure-free form))))
      form
      (%make-syntactic-closure senv free form)))

(define (constant-form? form)
  (not (or (syntactic-closure? form)
	   (pair? form)
	   (identifier? form))))

(define-record-type <syntactic-closure>
    (%make-syntactic-closure senv free form)
    syntactic-closure?
  (senv syntactic-closure-senv)
  (free syntactic-closure-free)
  (form syntactic-closure-form))

(define (strip-syntactic-closures object)
  (if (let loop ((object object))
	(if (pair? object)
	    (or (loop (car object))
		(loop (cdr object)))
	    (syntactic-closure? object)))
      (let loop ((object object))
	(if (pair? object)
	    (cons (loop (car object))
		  (loop (cdr object)))
	    (if (syntactic-closure? object)
		(loop (syntactic-closure-form object))
		object)))
      object))

;;;; Identifiers

(define (identifier? object)
  (or (raw-identifier? object)
      (closed-identifier? object)))

(define (raw-identifier? object)
  (and (symbol? object)
       ;; This makes `:keyword' objects be self-evaluating.
       (not (keyword? object))))

(define (closed-identifier? object)
  (and (syntactic-closure? object)
       (null? (syntactic-closure-free object))
       (raw-identifier? (syntactic-closure-form object))))

(register-predicate! identifier? 'identifier)
(register-predicate! raw-identifier? 'raw-identifier '<= identifier?)
(register-predicate! closed-identifier? 'closed-identifier '<= identifier?)

(define (new-identifier identifier)
  (string->uninterned-symbol (symbol->string (identifier->symbol identifier))))

(define (identifier->symbol identifier)
  (cond ((raw-identifier? identifier) identifier)
	((closed-identifier? identifier) (syntactic-closure-form identifier))
	(else (error:not-a identifier? identifier 'identifier->symbol))))

(define (lookup-identifier identifier senv)
  (cond ((raw-identifier? identifier)
	 (%lookup-raw-identifier identifier senv))
	((closed-identifier? identifier)
	 (%lookup-raw-identifier (syntactic-closure-form identifier)
				 (syntactic-closure-senv identifier)))
	(else
	 (error:not-a identifier? identifier 'lookup-identifier))))

(define (%lookup-raw-identifier identifier senv)
  (let ((item (syntactic-environment/lookup senv identifier)))
    (if (reserved-name-item? item)
	(syntax-error "Premature reference to reserved name:" identifier))
    (or item
	(make-variable-item identifier))))

(define (identifier=? environment-1 identifier-1 environment-2 identifier-2)
  (let ((item-1 (lookup-identifier identifier-1 environment-1))
	(item-2 (lookup-identifier identifier-2 environment-2)))
    (or (eq? item-1 item-2)
	;; This is necessary because an identifier that is not
	;; explicitly bound by an environment is mapped to a variable
	;; item, and the variable items are not cached.  Therefore
	;; two references to the same variable result in two
	;; different variable items.
	(and (variable-item? item-1)
	     (variable-item? item-2)
	     (eq? (variable-item/name item-1)
		  (variable-item/name item-2))))))

(define (reserve-identifier senv identifier)
  (cond ((raw-identifier? identifier)
	 (syntactic-environment/reserve senv identifier))
	((closed-identifier? identifier)
	 (syntactic-environment/reserve (syntactic-closure-senv identifier)
					(syntactic-closure-form identifier)))
	(else
	 (error:not-a identifier? identifier 'reserve-identifier))))

(define (bind-keyword senv identifier item)
  (cond ((raw-identifier? identifier)
	 (syntactic-environment/bind-keyword senv identifier item))
	((closed-identifier? identifier)
	 (syntactic-environment/bind-keyword
	  (syntactic-closure-senv identifier)
	  (syntactic-closure-form identifier)
	  item))
	(else
	 (error:not-a identifier? identifier 'bind-keyword))))

(define (bind-variable senv identifier)
  (cond ((raw-identifier? identifier)
	 (syntactic-environment/bind-variable senv identifier))
	((closed-identifier? identifier)
	 (syntactic-environment/bind-variable
	  (syntactic-closure-senv identifier)
	  (syntactic-closure-form identifier)))
	(else
	 (error:not-a identifier? identifier 'bind-variable))))

;;;; Utilities

(define (syntax-error . rest)
  (apply error rest))

(define (classifier->keyword classifier)
  (item->keyword (make-classifier-item classifier)))

(define (compiler->keyword compiler)
  (item->keyword (make-compiler-item compiler)))

(define (item->keyword item)
  (close-syntax 'keyword (make-keyword-syntactic-environment 'keyword item)))

(define (capture-syntactic-environment expander)
  `(,(classifier->keyword
      (lambda (form environment)
	form				;ignore
	(classify/form (expander environment)
		       environment)))))

(define (reverse-syntactic-environments environment procedure)
  (capture-syntactic-environment
   (lambda (closing-environment)
     (close-syntax (procedure closing-environment) environment))))