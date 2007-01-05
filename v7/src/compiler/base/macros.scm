#| -*-Scheme-*-

$Id: macros.scm,v 4.33 2007/01/05 15:33:03 cph Exp $

Copyright 1986,1987,1988,1989,1990,1992 Massachusetts Institute of Technology
Copyright 1993,1995,2001,2002,2003,2004 Massachusetts Institute of Technology

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

;;;; Compiler Macros
;;; package: (compiler macros)

(declare (usual-integrations))

(define-syntax last-reference
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER) (cdr form))
	 (let ((name (close-syntax (cadr form) environment)))
	   `(IF COMPILER:PRESERVE-DATA-STRUCTURES?
		,name
		(LET ((TEMP ,name))
		  (SET! ,name)
		  TEMP)))
	 (ill-formed-syntax form)))))

(define-syntax package
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '((* IDENTIFIER) * EXPRESSION) (cdr form))
	 (let ((names (cadr form))
	       (body (cddr form)))
	   `(,(close-syntax 'BEGIN environment)
	     ,@(map (let ((r-define
			   (close-syntax 'DEFINE environment)))
		      (lambda (name)
			`(,r-define ,name)))
		    names)
	     (,(close-syntax 'LET environment) () ,@body)))
	 (ill-formed-syntax form)))))

(define-syntax define-export
  (rsc-macro-transformer
   (lambda (form environment)
     (cond ((syntax-match? '(IDENTIFIER EXPRESSION) (cdr form))
	    `(,(close-syntax 'SET! environment)
	      ,@(cdr form)))
	   ((syntax-match? '((IDENTIFIER . MIT-BVL) + EXPRESSION) (cdr form))
	    `(,(close-syntax 'SET! environment)
	      ,(caadr form)
	      (,(close-syntax 'NAMED-LAMBDA environment)
	       ,@(cdr form))))
	   (else
	    (ill-formed-syntax form))))))

(define-syntax define-vector-slots
  (sc-macro-transformer
   (let ((pattern
	  `(SYMBOL ,exact-nonnegative-integer?
		   * ,(lambda (x)
			(or (symbol? x)
			    (and (pair? x)
				 (list-of-type? x symbol?)))))))
     (lambda (form environment)
       environment
       (if (syntax-match? pattern (cdr form))
	   (let ((class (cadr form))
		 (index (caddr form))
		 (slots (cdddr form)))
	     (let ((make-defs
		    (lambda (slot index)
		      (let ((ref-name (symbol-append class '- slot)))
			`((DEFINE-INTEGRABLE (,ref-name V)
			    (VECTOR-REF V ,index))
			  (DEFINE-INTEGRABLE
			    (,(symbol-append 'SET- ref-name '!) V OBJECT)
			    (VECTOR-SET! V ,index OBJECT)))))))
	       (if (pair? slots)
		   `(BEGIN
		      ,@(let loop ((slots slots) (index index))
			  (if (pair? slots)
			      (append (if (pair? (car slots))
					  (append-map (lambda (slot)
							(make-defs slot index))
						      (car slots))
					  (make-defs (car slots) index))
				      (loop (cdr slots) (+ index 1)))
			      '())))
		   'UNSPECIFIC)))
	   (ill-formed-syntax form))))))

(define-syntax define-root-type
  (sc-macro-transformer
   (let ((pattern
	  `(SYMBOL * ,(lambda (x)
			(or (symbol? x)
			    (and (pair? x)
				 (list-of-type? x symbol?)))))))
     (lambda (form environment)
       (if (syntax-match? pattern (cdr form))
	   (let ((type (cadr form))
		 (slots (cddr form)))
	     (let ((tag-name (symbol-append type '-TAG)))
	       (let ((tag-ref (close-syntax tag-name environment)))
		 `(BEGIN
		    (DEFINE ,tag-name
		      (MAKE-VECTOR-TAG #F ',type #F))
		    (DEFINE ,(symbol-append type '?)
		      (TAGGED-VECTOR/SUBCLASS-PREDICATE ,tag-ref))
		    (DEFINE-VECTOR-SLOTS ,type 1 ,@slots)
		    (SET-VECTOR-TAG-DESCRIPTION! ,tag-ref
		      (LAMBDA (OBJECT)
			(DESCRIPTOR-LIST OBJECT ,type ,@slots)))))))
	   (ill-formed-syntax form))))))

(define-syntax define-type-definition
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (cadr form))
	   (reserved (caddr form))
	   (enumeration (close-syntax (cadddr form) environment)))
       (let ((parent
	      (close-syntax (symbol-append name '-TAG) environment)))
	 `(define-syntax ,(symbol-append 'DEFINE- name)
	    (sc-macro-transformer
	     (let ((pattern
		    `(SYMBOL * ,(lambda (x)
				  (or (symbol? x)
				      (and (pair? x)
					   (list-of-type? x symbol?)))))))
	       (lambda (form environment)
		 (if (syntax-match? pattern (cdr form))
		     (let ((type (cadr form))
			   (slots (cddr form)))
		       (let ((tag-name (symbol-append type '-TAG)))
			 (let ((tag-ref
				(close-syntax tag-name environment)))
			   `(BEGIN
			      (DEFINE ,tag-name
				(MAKE-VECTOR-TAG ,',parent ',type
						 ,',enumeration))
			      (DEFINE ,(symbol-append type '?)
				(TAGGED-VECTOR/PREDICATE ,tag-ref))
			      (DEFINE-VECTOR-SLOTS ,type ,,reserved
				,@slots)
			      (SET-VECTOR-TAG-DESCRIPTION!
			       ,tag-name
			       (LAMBDA (OBJECT)
				 (APPEND!
				  ((VECTOR-TAG-DESCRIPTION ,',parent)
				   OBJECT)
				  (DESCRIPTOR-LIST OBJECT
						   ,type
						   ,@slots))))))))
		     (ill-formed-syntax form)))))))))))

(define-type-definition snode 5 #f)
(define-type-definition pnode 6 #f)
(define-type-definition rvalue 2 rvalue-types)
(define-type-definition lvalue 14 #f)

(define-syntax descriptor-list
  (sc-macro-transformer
   (let ((pattern
	  `(IDENTIFIER SYMBOL
		       * ,(lambda (x)
			    (or (symbol? x)
				(and (pair? x)
				     (list-of-type? x symbol?)))))))
     (lambda (form environment)
       (if (syntax-match? pattern (cdr form))
	   (let ((object (close-syntax (cadr form) environment))
		 (type (caddr form))
		 (slots (cdddr form)))
	     (let ((ref-name
		    (lambda (slot)
		      (close-syntax (symbol-append type '- slot)
				    environment))))
	       `(LIST
		 ,@(map (lambda (slot)
			  (if (pair? slot)
			      (let ((names (map ref-name slot)))
				``(,',names ,(,(car names) ,object)))
			      (let ((name (ref-name slot)))
				``(,',name ,(,name ,object)))))
			slots))))
	   (ill-formed-syntax form))))))

;;; Kludge to make these compile efficiently.

(define-syntax make-snode
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(+ EXPRESSION) (cdr form))
	 (let ((tag (close-syntax (cadr form) environment))
	       (extra
		(map (lambda (form) (close-syntax form environment))
		     (cddr form))))
	   `((ACCESS VECTOR ,system-global-environment)
	     ,tag #F '() '() #F ,@extra))
	 (ill-formed-syntax form)))))

(define-syntax make-pnode
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(+ EXPRESSION) (cdr form))
	 (let ((tag (close-syntax (cadr form) environment))
	       (extra
		(map (lambda (form) (close-syntax form environment))
		     (cddr form))))
	   `((ACCESS VECTOR ,system-global-environment)
	     ,tag #F '() '() #F #F ,@extra))
	 (ill-formed-syntax form)))))

(define-syntax make-rvalue
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(+ EXPRESSION) (cdr form))
	 (let ((tag (close-syntax (cadr form) environment))
	       (extra
		(map (lambda (form) (close-syntax form environment))
		     (cddr form))))
	   `((ACCESS VECTOR ,system-global-environment)
	     ,tag #F ,@extra))
	 (ill-formed-syntax form)))))

(define-syntax make-lvalue
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(+ EXPRESSION) (cdr form))
	 (let ((tag (close-syntax (cadr form) environment))
	       (extra
		(map (lambda (form) (close-syntax form environment))
		     (cddr form))))
	   `(LET ((LVALUE
		   ((ACCESS VECTOR ,system-global-environment)
		    ,tag #F '() '() '() '() '() '() 'NOT-CACHED
		    #F '() #F #F '() ,@extra)))
	      (SET! *LVALUES* (CONS LVALUE *LVALUES*))
	      LVALUE))
	 (ill-formed-syntax form)))))

(define-syntax define-rtl-expression
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (define-rtl-common form
       (lambda (expression) expression)
       'RTL:EXPRESSION-TYPES))))

(define-syntax define-rtl-statement
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (define-rtl-common form
       (lambda (expression) `(STATEMENT->SRTL ,expression))
       'RTL:STATEMENT-TYPES))))

(define-syntax define-rtl-predicate
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (define-rtl-common form
       (lambda (expression) `(PREDICATE->PRTL ,expression))
       'RTL:PREDICATE-TYPES))))

(define (define-rtl-common form wrap-constructor types)
  (if (syntax-match? '(SYMBOL SYMBOL * SYMBOL) (cdr form))
      (let ((type (cadr form))
	    (prefix (caddr form))
	    (components (cdddr form)))
	`(BEGIN
	   (SET! ,types (CONS ',type ,types))
	   ,(let ((parameters (map make-synthetic-identifier components)))
	      `(DEFINE-INTEGRABLE
		 (,(symbol-append prefix 'MAKE- type) ,@parameters)
		 ,(wrap-constructor `(LIST ',type ,@parameters))))
	   (DEFINE-INTEGRABLE (,(symbol-append 'RTL: type '?) EXPRESSION)
	     (EQ? (CAR EXPRESSION) ',type))
	   ,@(let loop ((components components) (ref-index 6) (set-index 2))
	       (if (pair? components)
		   (let ((name (symbol-append type '- (car components))))
		     `((DEFINE-INTEGRABLE
			 (,(symbol-append 'RTL: name) OBJECT)
			 (GENERAL-CAR-CDR OBJECT ,ref-index))
		       (DEFINE-INTEGRABLE
			 (,(symbol-append 'RTL:SET- name '!) OBJECT V)
			 (SET-CAR! (GENERAL-CAR-CDR OBJECT ,set-index) V))
		       ,@(loop (cdr components)
			       (* ref-index 2)
			       (* set-index 2))))
		   '()))))
      (ill-formed-syntax form)))

(define-syntax define-rule
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER DATUM + DATUM) (cdr form))
	 (receive (pattern matcher)
	     (rule->matcher (caddr form) (cdddr form) environment)
	   `(,(case (cadr form)
		((STATEMENT PREDICATE)
		 (close-syntax 'ADD-STATEMENT-RULE! environment))
		((REWRITING)
		 (close-syntax 'ADD-REWRITING-RULE! environment))
		((PRE-CSE-REWRITING)
		 (close-syntax 'ADD-PRE-CSE-REWRITING-RULE! environment))
		(else
		 (error "Unknown rule type:" (cadr form))))
	     ',pattern
	     ,matcher))
	 (ill-formed-syntax form)))))

(define-syntax rule-matcher
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(DATUM + DATUM) (cdr form))
	 (receive (pattern matcher)
	     (rule->matcher (cadr form) (cddr form) environment)
	   pattern
	   matcher)
	 (ill-formed-syntax form)))))

(define-syntax lap
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(* DATUM) (cdr form))
	 `(,(close-syntax 'QUASIQUOTE environment) ,(cdr form))
	 (ill-formed-syntax form)))))

(define-syntax inst-ea
  (rsc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(DATUM) (cdr form))
	 `(,(close-syntax 'QUASIQUOTE environment) ,(cadr form))
	 (ill-formed-syntax form)))))

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL (* SYMBOL)) (cdr form))
	 (let ((name (cadr form))
	       (elements (caddr form)))
	   (let ((enumeration (symbol-append name 'S)))
	     (let ((enum-ref (close-syntax enumeration environment)))
	       `(BEGIN
		  (DEFINE ,enumeration
		    (MAKE-ENUMERATION ',elements))
		  ,@(map (lambda (element)
			   `(DEFINE ,(symbol-append name '/ element)
			      (ENUMERATION/NAME->INDEX ,enum-ref ',element)))
			 elements)))))
	 (ill-formed-syntax form)))))

(define-syntax enumeration-case
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL EXPRESSION * (DATUM * EXPRESSION)) (cdr form))
	 (enumeration-case-1 (caddr form) (cdddr form) environment
			     (lambda (element)
			       (symbol-append (cadr form) '/ element))
			     (lambda (expression) expression '()))
	 (ill-formed-syntax form)))))

(define-syntax cfg-node-case
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(EXPRESSION * (DATUM * EXPRESSION)) (cdr form))
	 (enumeration-case-1 (cadr form) (cddr form) environment
			     (lambda (element) (symbol-append element '-TAG))
			     (lambda (expression)
			       `((ELSE
				  (ERROR "Unknown node type:" ,expression)))))
	 (ill-formed-syntax form)))))

(define (enumeration-case-1 expression clauses environment map-element default)
  (capture-syntactic-environment
   (lambda (closing-environment)
     (let ((expression (close-syntax expression environment))
	   (generate-body
	    (lambda (expression)
	      `(COND
		,@(let loop ((clauses clauses))
		    (if (pair? clauses)
			(if (and (identifier? (caar clauses))
				 (identifier=? environment (caar clauses)
					       closing-environment 'ELSE))
			    (begin
			      (if (pair? (cdr clauses))
				  (error "ELSE clause not last:" clauses))
			      `((ELSE
				 ,@(map (lambda (expression)
					  (close-syntax expression
							environment))
					(cdar clauses)))))
			    `(((OR ,@(map (lambda (element)
					    `(EQ? ,expression
						  ,(close-syntax
						    (map-element element)
						    environment)))
					  (caar clauses)))
			       ,@(map (lambda (expression)
					(close-syntax expression environment))
				      (cdar clauses)))
			      ,@(loop (cdr clauses))))
			(default expression)))))))
       (if (identifier? expression)
	   (generate-body expression)
	   `(LET ((TEMP ,expression))
	      ,(generate-body 'TEMP)))))))