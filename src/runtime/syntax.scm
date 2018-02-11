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
  (let ((senv
	 (if (syntactic-environment? environment)
	     environment
	     (runtime-environment->syntactic environment))))
    (with-identifier-renaming
     (lambda ()
       (if (senv-top-level? senv)
	   (%compile-top-level-body (%classify-body-top-level forms senv))
	   (output/sequence
	    (map (lambda (form)
		   (compile-expr-item
		    (%classify-form-top-level form senv)))
		 forms)))))))

(define (%classify-form-top-level form senv)
  (classify-form form senv (initial-hist form)))

(define (%classify-body-top-level forms senv)
  (seq-item
   (map-in-order (lambda (form)
		   (%classify-form-top-level form senv))
		 forms)))

(define (%compile-top-level-body item)
  (output/top-level-sequence
   (map (lambda (item)
	  (if (defn-item? item)
	      (let ((name (defn-item-id item))
		    (value (compile-expr-item (defn-item-value item))))
		(if (defn-item-syntax? item)
		    (output/top-level-syntax-definition name value)
		    (output/top-level-definition name value)))
	      (compile-expr-item item)))
	(item->list item))))

;;;; Classifier

(define (classify-form form senv hist)
  (cond ((identifier? form)
	 (lookup-identifier form senv))
	((syntactic-closure? form)
	 (reclassify (syntactic-closure-form form)
		     (make-partial-senv (syntactic-closure-free form)
					senv
					(syntactic-closure-senv form))
		     hist))
	((pair? form)
	 (let ((item (classify-form-car form senv hist)))
	   (cond ((classifier-item? item)
		  ((classifier-item-impl item) form senv hist))
		 ((compiler-item? item)
		  (expr-item
		   (let ((compiler (compiler-item-impl item)))
		     (lambda ()
		       (compiler form senv hist)))))
		 ((expander-item? item)
		  (reclassify ((expander-item-impl item) form senv)
			      senv
			      hist))
		 (else
		  (if (not (list? (cdr form)))
		      (syntax-error "Combination must be a proper list:" form))
		  (expr-item
		   (let ((items
			  (classify-forms (cdr form)
					  senv
					  (hist-cdr hist))))
		     (lambda ()
		       (output/combination
			(compile-expr-item item)
			(map compile-expr-item items)))))))))
	(else
	 (expr-item (lambda () (output/constant form))))))

(define (classify-form-car form senv hist)
  (classify-form (car form) senv (hist-car hist)))

(define (classify-form-cadr form senv hist)
  (classify-form (cadr form) senv (hist-cadr hist)))

(define (classify-form-caddr form senv hist)
  (classify-form (caddr form) senv (hist-caddr hist)))

(define (classify-form-cadddr form senv hist)
  (classify-form (cadddr form) senv (hist-cadddr hist)))

(define (classify-forms forms senv hist)
  (map (lambda (expr hist)
	 (classify-form expr senv hist))
       forms
       (subform-hists forms hist)))

(define (reclassify form env hist)
  (classify-form form env (hist-reduce form hist)))

(define (classify-body forms senv hist)
  ;; Syntactic definitions affect all forms that appear after them, so classify
  ;; FORMS in order.
  (seq-item
   (map-in-order (lambda (form hist)
		   (classify-form form senv hist))
		 forms
		 (subform-hists forms hist))))

(define (classify-body-cdr form senv hist)
  (classify-body (cdr form) senv (hist-cdr hist)))

(define (classify-body-cddr form senv hist)
  (classify-body (cddr form) senv (hist-cddr hist)))

;;;; Compiler

(define (compile-body-items items)
  (let ((items (flatten-items items)))
    (if (not (pair? items))
	(syntax-error "Empty body"))
    (output/sequence
     (append-map
      (lambda (item)
	(if (defn-item? item)
	    (if (defn-item-syntax? item)
		'()
		(list (output/definition
		       (defn-item-id item)
		       (compile-expr-item (defn-item-value item)))))
	    (list (compile-expr-item item))))
      items))))

(define compile-expr-item)
(add-boot-init!
 (lambda ()
   (set! compile-expr-item
	 (standard-predicate-dispatcher 'compile-expr-item 1))
   (run-deferred-boot-actions 'define-item-compiler)))

(define (define-item-compiler predicate compiler)
  (defer-boot-action 'define-item-compiler
    (lambda ()
      (define-predicate-dispatch-handler compile-expr-item
	(list predicate)
	compiler))))

(define-item-compiler var-item?
  (lambda (item)
    (output/variable (var-item-id item))))

(define-item-compiler expr-item?
  (lambda (item)
    ((expr-item-compiler item))))

(define-item-compiler seq-item?
  (lambda (item)
    (compile-body-items (seq-item-elements item))))

(define-item-compiler decl-item?
  (lambda (item)
    (output/declaration (decl-item-text item))))

(define (illegal-expression-compiler description)
  (let ((message (string description " may not be used as an expression:")))
    (lambda (item)
      (syntax-error message item))))

(define-item-compiler reserved-name-item?
  (illegal-expression-compiler "Reserved name"))

(define-item-compiler keyword-item?
  (illegal-expression-compiler "Syntactic keyword"))

(define-item-compiler defn-item?
  (illegal-expression-compiler "Definition"))

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

(define (identifier=? senv-1 identifier-1 senv-2 identifier-2)
  (let ((item-1 (lookup-identifier identifier-1 senv-1))
	(item-2 (lookup-identifier identifier-2 senv-2)))
    (or (eq? item-1 item-2)
	;; This is necessary because an identifier that is not explicitly bound
	;; by an environment is mapped to a variable item, and the variable
	;; items are not memoized by the runtime syntactic environments.  Fixing
	;; this would require doing that memoizing, and also ensuring that
	;; runtime-environment->syntactic memoized its result as well.  Avoiding
	;; that complexity requires this small tweak.
	(and (var-item? item-1)
	     (var-item? item-2)
	     (eq? (var-item-id item-1)
		  (var-item-id item-2))))))

;;;; History

(define-record-type <history>
    (%history records)
    history?
  (records %history-records))

(define (initial-hist form)
  (%history (list form)))

(define (hist-select selector hist)
  (%history
   (let ((records (%history-records hist)))
     (if (and (pair? records)
	      (eq? 'select (caar records)))
	 (cons (cons 'select (biselect-append selector (cdar records)))
	       (cdr records))
	 (cons (cons 'select selector)
	       records)))))

(define (hist-reduce form hist)
  (%history (cons (cons 'reduce form) (%history-records hist))))

(define (hist-car hist)
  (hist-select biselector:car hist))

(define (hist-cdr hist)
  (hist-select biselector:cdr hist))

(define (hist-cadr hist)
  (hist-select biselector:cadr hist))

(define (hist-cddr hist)
  (hist-select biselector:cddr hist))

(define (hist-caddr hist)
  (hist-select biselector:caddr hist))

(define (hist-cdddr hist)
  (hist-select biselector:cdddr hist))

(define (hist-cadddr hist)
  (hist-select biselector:cadddr hist))

(define (subform-hists forms hist)
  (let loop ((forms forms) (hist hist))
    (if (pair? forms)
	(cons (hist-car hist)
	      (loop (cdr forms) (hist-cdr hist)))
	'())))

;;;; Binary selectors

(define (biselect-car selector)
  (let ((n (integer-length selector)))
    (+ (shift-left 1 n)
       (- selector (shift-left 1 (- n 1))))))

(define (biselect-cdr selector)
  (+ (shift-left 1 (integer-length selector))
     selector))

(define (biselect-subform selector form)
  (if (> selector 1)
      (biselect-subform (quotient selector 2)
			(if (even? selector) (car form) (cdr form)))
      form))

;; Selector order is:
;; (= biselector:cadr (biselect-append biselector:car biselector:cdr))
(define (biselect-append . selectors)
  (reduce (lambda (s1 s2)
	    (let ((n (- (integer-length s1) 1)))
	      (+ (shift-left s2 n)
		 (- s1 (shift-left 1 n)))))
	  biselector:cr
	  selectors))

(define-integrable biselector:cr     #b00001)
(define-integrable biselector:car    #b00010)
(define-integrable biselector:cdr    #b00011)
(define-integrable biselector:cadr   #b00101)
(define-integrable biselector:cddr   #b00111)
(define-integrable biselector:caddr  #b01011)
(define-integrable biselector:cdddr  #b01111)
(define-integrable biselector:cadddr #b10111)
(define-integrable biselector:cddddr #b11111)

;;;; Utilities

(define (syntax-error . rest)
  (apply error rest))

(define (classifier->keyword classifier)
  (item->keyword (classifier-item classifier)))

(define (compiler->keyword compiler)
  (item->keyword (compiler-item compiler)))

(define (item->keyword item)
  (close-syntax 'keyword (make-keyword-senv 'keyword item)))

(define (capture-syntactic-environment expander)
  `(,(classifier->keyword
      (lambda (form senv hist)
	(declare (ignore form))
	(classify-form (expander senv) senv hist)))))

(define (reverse-syntactic-environments senv procedure)
  (capture-syntactic-environment
   (lambda (closing-senv)
     (close-syntax (procedure closing-senv) senv))))

(define (map-in-order procedure . lists)
  (let loop ((lists lists) (values '()))
    (if (pair? (car lists))
	(loop (map cdr lists)
	      (cons (apply procedure (map car lists)) values))
	(reverse! values))))