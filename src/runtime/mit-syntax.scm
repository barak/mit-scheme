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

;;;; MIT/GNU Scheme Syntax

(declare (usual-integrations))

;;;; Macro transformers

(define (transformer-keyword name transformer->expander)
  (lambda (form environment definition-environment)
    definition-environment		;ignore
    (syntax-check '(KEYWORD EXPRESSION) form)
    (let ((item (classify/expression (cadr form) environment)))
      (make-keyword-value-item
       (transformer->expander (transformer-eval (compile-item/expression item)
						environment)
			      environment)
       (make-expression-item
	(lambda ()
	  (output/combination (output/runtime-reference name)
			      (list (compile-item/expression item)
				    (output/the-environment)))))))))

(define classifier:sc-macro-transformer
  ;; "Syntactic Closures" transformer
  (transformer-keyword 'SC-MACRO-TRANSFORMER->EXPANDER
		       sc-macro-transformer->expander))

(define classifier:rsc-macro-transformer
  ;; "Reversed Syntactic Closures" transformer
  (transformer-keyword 'RSC-MACRO-TRANSFORMER->EXPANDER
		       rsc-macro-transformer->expander))

(define classifier:er-macro-transformer
  ;; "Explicit Renaming" transformer
  (transformer-keyword 'ER-MACRO-TRANSFORMER->EXPANDER
		       er-macro-transformer->expander))

(define classifier:non-hygienic-macro-transformer
  (transformer-keyword 'NON-HYGIENIC-MACRO-TRANSFORMER->EXPANDER
		       non-hygienic-macro-transformer->expander))

;;;; Core primitives

(define (compiler:lambda form environment)
  (syntax-check '(KEYWORD MIT-BVL + FORM) form)
  (receive (bvl body)
      (compile/lambda (cadr form) (cddr form) environment)
    (output/lambda bvl body)))

(define (compiler:named-lambda form environment)
  (syntax-check '(KEYWORD (IDENTIFIER . MIT-BVL) + FORM) form)
  (receive (bvl body)
      (compile/lambda (cdadr form) (cddr form) environment)
    (output/named-lambda (identifier->symbol (caadr form)) bvl body)))

(define (compile/lambda bvl body environment)
  (let ((environment (make-internal-syntactic-environment environment)))
    ;; Force order -- bind names before classifying body.
    (let ((bvl
	   (map-mit-lambda-list (lambda (identifier)
				  (bind-variable! environment identifier))
				bvl)))
      (values bvl
	      (compile-body-item
	       (classify/body body
			      environment
			      environment))))))

(define (classifier:begin form environment definition-environment)
  (syntax-check '(KEYWORD * FORM) form)
  (classify/body (cdr form) environment definition-environment))

(define (compiler:if form environment)
  (syntax-check '(KEYWORD EXPRESSION EXPRESSION ? EXPRESSION) form)
  (output/conditional
   (compile/expression (cadr form) environment)
   (compile/expression (caddr form) environment)
   (if (pair? (cdddr form))
       (compile/expression (cadddr form) environment)
       (output/unspecific))))

(define (compiler:quote form environment)
  environment				;ignore
  (syntax-check '(KEYWORD DATUM) form)
  (output/constant (strip-syntactic-closures (cadr form))))

(define (compiler:set! form environment)
  (syntax-check '(KEYWORD FORM ? EXPRESSION) form)
  (receive (name environment-item)
      (classify/location (cadr form) environment)
    (let ((value
	   (if (pair? (cddr form))
	       (compile/expression (caddr form) environment)
	       (output/unassigned))))
      (if environment-item
	  (output/access-assignment
	   name
	   (compile-item/expression environment-item)
	   value)
	  (output/assignment name value)))))

(define (classify/location form environment)
  (let ((item (classify/expression form environment)))
    (cond ((variable-item? item)
	   (values (variable-item/name item) #f))
	  ((access-item? item)
	   (values (access-item/name item) (access-item/environment item)))
	  (else
	   (syntax-error "Variable required in this context:" form)))))

(define (compiler:delay form environment)
  (syntax-check '(KEYWORD EXPRESSION) form)
  (output/delay (compile/expression (cadr form) environment)))

;;;; Definitions

(define keyword:define
  (classifier->keyword
   (lambda (form environment definition-environment)
     (classify/define form environment definition-environment
		      variable-binding-theory))))

(define (classifier:define-syntax form environment definition-environment)
  (syntax-check '(KEYWORD IDENTIFIER EXPRESSION) form)
  (classify/define form environment definition-environment
		   syntactic-binding-theory))

(define (classify/define form environment definition-environment
			 binding-theory)
  (if (not (syntactic-environment/top-level? definition-environment))
      (syntactic-environment/define definition-environment
				    (cadr form)
				    (make-reserved-name-item)))
  (binding-theory definition-environment
		  (cadr form)
		  (classify/expression (caddr form) environment)))

(define (syntactic-binding-theory environment name item)
  (if (not (keyword-item? item))
      (syntax-error "Syntactic binding value must be a keyword:" name))
  (syntactic-environment/define environment name item)
  ;; User-defined macros at top level are preserved in the output.
  (if (and (keyword-value-item? item)
           (syntactic-environment/top-level? environment))
      (make-binding-item (rename-top-level-identifier name) item)
      (make-null-binding-item)))

(define (variable-binding-theory environment name item)
  (if (keyword-item? item)
      (syntax-error "Binding value may not be a keyword:" name))
  (make-binding-item (bind-variable! environment name) item))

;;;; LET-like

(define keyword:let
  (classifier->keyword
   (lambda (form environment definition-environment)
     definition-environment
     (let* ((binding-environment
	     (make-internal-syntactic-environment environment))
	    (body-environment
	     (make-internal-syntactic-environment binding-environment)))
       (classify/let-like form
			  environment
			  binding-environment
			  body-environment
			  variable-binding-theory
			  output/let)))))

(define (classifier:letrec form environment definition-environment)
  definition-environment
  (syntax-check '(KEYWORD (* (IDENTIFIER ? EXPRESSION)) + FORM) form)
  (let* ((binding-environment
	  (make-internal-syntactic-environment environment))
	 (body-environment
	  (make-internal-syntactic-environment binding-environment)))
    (for-each (let ((item (make-reserved-name-item)))
		(lambda (binding)
		  (syntactic-environment/define binding-environment
						(car binding)
						item)))
	      (cadr form))
    (classify/let-like form
		       binding-environment
		       binding-environment
		       body-environment
		       variable-binding-theory
		       output/letrec)))

(define (classifier:let-syntax form environment definition-environment)
  definition-environment
  (syntax-check '(KEYWORD (* (IDENTIFIER EXPRESSION)) + FORM) form)
  (let* ((binding-environment
	  (make-internal-syntactic-environment environment))
	 (body-environment
	  (make-internal-syntactic-environment binding-environment)))
    (classify/let-like form
		       environment
		       binding-environment
		       body-environment
		       syntactic-binding-theory
		       output/let)))

(define keyword:let-syntax
  (classifier->keyword classifier:let-syntax))

(define (classifier:letrec-syntax form environment definition-environment)
  definition-environment
  (syntax-check '(KEYWORD (* (IDENTIFIER EXPRESSION)) + FORM) form)
  (let* ((binding-environment
	  (make-internal-syntactic-environment environment))
	 (body-environment
	  (make-internal-syntactic-environment binding-environment)))
    (for-each (let ((item (make-reserved-name-item)))
		(lambda (binding)
		  (syntactic-environment/define binding-environment
						(car binding)
						item)))
	      (cadr form))
    (classify/let-like form
		       binding-environment
		       binding-environment
		       body-environment
		       syntactic-binding-theory
		       output/letrec)))

(define (classify/let-like form
			   value-environment
			   binding-environment
			   body-environment
			   binding-theory
			   output/let)
  ;; Classify right-hand sides first, in order to catch references to
  ;; reserved names.  Then bind names prior to classifying body.
  (let* ((bindings
	  (remove! null-binding-item?
		   (map (lambda (binding item)
			  (binding-theory binding-environment
					  (car binding)
					  item))
			(cadr form)
			(map (lambda (binding)
			       (classify/expression (cadr binding)
						    value-environment))
			     (cadr form)))))
	 (body
	  (classify/body (cddr form)
			 body-environment
			 body-environment)))
    (if (eq? binding-theory syntactic-binding-theory)
	body
	(make-expression-item
	 (let ((names (map binding-item/name bindings))
	       (values (map binding-item/value bindings)))
	   (lambda ()
	     (output/let names
			 (map compile-item/expression values)
			 (compile-body-item body))))))))

(define (compile-body-item item)
  (receive (declaration-items items)
      (extract-declarations-from-body (body-item/components item))
    (output/body (map declaration-item/text declaration-items)
		 (compile-body-items items))))

;; TODO: this is a compiler rather than a macro because it uses the
;; special OUTPUT/DISJUNCTION.  Unfortunately something downstream in
;; the compiler wants this, but it would be nice to eliminate this
;; hack.
(define (compiler:or form environment)
  (syntax-check '(KEYWORD * EXPRESSION) form)
  (if (pair? (cdr form))
      (let loop ((expressions (cdr form)))
	(let ((compiled (compile/expression (car expressions) environment)))
	  (if (pair? (cdr expressions))
	      (output/disjunction compiled (loop (cdr expressions)))
	      compiled)))
      `#F))

;;;; MIT-specific syntax

(define-record-type <access-item>
    (make-access-item name environment)
    access-item?
  (name access-item/name)
  (environment access-item/environment))

(define keyword:access
  (classifier->keyword
   (lambda (form environment definition-environment)
     definition-environment
     (make-access-item (cadr form)
		       (classify/expression (caddr form) environment)))))

(define-item-compiler <access-item>
  (lambda (item)
    (output/access-reference
     (access-item/name item)
     (compile-item/expression (access-item/environment item)))))

(define (compiler:the-environment form environment)
  environment
  (syntax-check '(KEYWORD) form)
  (if (not (syntactic-environment/top-level? environment))
      (syntax-error "This form allowed only at top level:" form))
  (output/the-environment))

(define keyword:unspecific
  (compiler->keyword
   (lambda (form environment)
     form environment			;ignore
     (output/unspecific))))

(define keyword:unassigned
  (compiler->keyword
   (lambda (form environment)
     form environment			;ignore
     (output/unassigned))))

;;;; Declarations

(define (classifier:declare form environment definition-environment)
  definition-environment
  (syntax-check '(KEYWORD * (SYMBOL * DATUM)) form)
  (make-declaration-item
   (lambda ()
     (classify/declarations (cdr form) environment))))

(define (classifier:local-declare form environment definition-environment)
  (syntax-check '(KEYWORD (* (SYMBOL * DATUM)) + FORM) form)
  (let ((body
	 (classify/body (cddr form)
			environment
			definition-environment)))
    (make-expression-item
     (lambda ()
       (output/local-declare (classify/declarations (cadr form) environment)
			     (compile-body-item body))))))

(define (classify/declarations declarations environment)
  (map (lambda (declaration)
	 (classify/declaration declaration environment))
       declarations))

(define (classify/declaration declaration environment)
  (map-declaration-identifiers (lambda (identifier)
				 (variable-item/name
				  (classify/variable-reference identifier
							       environment)))
			       declaration))

(define (classify/variable-reference identifier environment)
  (let ((item (classify/expression identifier environment)))
    (if (not (variable-item? item))
	(syntax-error "Variable required in this context:" identifier))
    item))