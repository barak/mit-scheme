;;; -*-Scheme-*-
;;;
;;; $Id: syntax-output.scm,v 14.5 2002/03/02 04:31:41 cph Exp $
;;;
;;; Copyright (c) 1989-1991, 2001, 2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Syntaxer Output Interface

(declare (usual-integrations))

(define (syntax-error history . rest)
  history				;ignore
  (apply error rest))

(define (transformer-eval expression environment)
  (eval expression environment))

(define (output/variable name)
  (make-variable name))

(define (output/constant datum)
  datum)

(define (output/assignment name value)
  (make-assignment name value))

(define (output/top-level-definition name value)
  (make-definition name
		   (if (lambda? value)
		       (lambda-components* value
			 (lambda (name* required optional rest body)
			   (if (eq? name* lambda-tag:unnamed)
			       (make-lambda* name required optional rest body)
			       value)))
		       value)))

(define (output/top-level-syntax-definition name value)
  (make-definition name (make-macro-reference-trap-expression value)))

(define (output/conditional predicate consequent alternative)
  (make-conditional predicate consequent alternative))

(define (output/sequence expressions)
  (make-sequence expressions))

(define (output/combination operator operands)
  (make-combination operator operands))

(define (output/lambda lambda-list body)
  (output/named-lambda lambda-tag:unnamed lambda-list body))

(define (output/named-lambda name lambda-list body)
  (output/lambda-internal name lambda-list '() body))

(define (output/lambda-internal name lambda-list declarations body)
  (call-with-values (lambda () (parse-mit-lambda-list lambda-list))
    (lambda (required optional rest)
      (make-lambda* name required optional rest
		    (let ((declarations (apply append declarations)))
		      (if (pair? declarations)
			  (make-sequence (make-block-declaration declarations)
					 body)
			  body))))))

(define (output/delay expression)
  (make-delay expression))

(define (output/unassigned-test name)
  (make-unassigned? name))

(define (output/unassigned)
  (make-unassigned-reference-trap))

(define (output/unspecific)
  unspecific)

(define (output/let names values body)
  (output/combination (output/named-lambda lambda-tag:let names body) values))

(define (output/letrec names values body)
  (output/let '() '()
	      (output/body '()
			   (make-sequence
			    (append! (map make-definition names values)
				     (list body))))))

(define (output/body declarations body)
  (scan-defines (let ((declarations (apply append declarations)))
		  (if (pair? declarations)
		      (make-sequence
		       (list (make-block-declaration declarations)
			     body))
		      body))
		make-open-block))

(define (output/definition name value)
  (make-definition name value))

(define (output/top-level-sequence declarations expressions)
  (let ((declarations (apply append declarations))
	(make-open-block
	 (lambda (expressions)
	   (scan-defines (make-sequence expressions) make-open-block))))
    (if (pair? declarations)
	(make-open-block
	 (cons (make-block-declaration declarations)
	       (if (pair? expressions)
		   expressions
		   (list (output/unspecific)))))
	(if (pair? expressions)
	    (if (pair? (cdr expressions))
		(make-open-block expressions)
		(car expressions))
	    (output/unspecific)))))

(define (output/the-environment)
  (make-the-environment))

(define (output/access-reference name environment)
  (make-access environment name))

(define (output/access-assignment name environment value)
  (make-combination lexical-assignment (list environment name value)))

(define (output/local-declare declarations body)
  (make-declaration declarations body))

(define lambda-tag:unnamed
  ((ucode-primitive string->symbol) "#[unnamed-procedure]"))

(define lambda-tag:let
  ((ucode-primitive string->symbol) "#[let-procedure]"))

(define lambda-tag:fluid-let
  ((ucode-primitive string->symbol) "#[fluid-let-procedure]"))

;;;; Declarations

(define (define-declaration name pattern mapper)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (cons pattern mapper))
	(begin
	  (set! known-declarations
		(cons (cons name (cons pattern mapper))
		      known-declarations))
	  unspecific))))

(define (process-declaration declaration
			     selector
			     map-identifier
			     ill-formed-declaration)
  (if (pair? declaration)
      (let ((entry (assq (car declaration) known-declarations)))
	(if (and entry (syntax-match? (cadr entry) (cdr declaration)))
	    ((cddr entry) declaration selector map-identifier)
	    (begin
	      (warn "Unknown declaration:" declaration)
	      declaration)))
      (ill-formed-declaration declaration selector)))

(define known-declarations '())

(for-each (lambda (keyword)
	    (define-declaration keyword '()
	      (lambda (declaration selector map-identifier)
		selector map-identifier
		declaration)))
	  '(AUTOMAGIC-INTEGRATIONS
	    NO-AUTOMAGIC-INTEGRATIONS
	    ETA-SUBSTITUTION
	    NO-ETA-SUBSTITUTION
	    OPEN-BLOCK-OPTIMIZATIONS
	    NO-OPEN-BLOCK-OPTIMIZATIONS))

(for-each (lambda (keyword)
	    (define-declaration keyword '(* IDENTIFIER)
	      (lambda (declaration selector map-identifier)
		(cons (car declaration)
		      (select-map map-identifier
				  (cdr declaration)
				  (selector/add-cdr selector))))))
	  ;; The names in USUAL-INTEGRATIONS are always global.
	  '(USUAL-INTEGRATIONS
	    INTEGRATE
	    INTEGRATE-OPERATOR
	    INTEGRATE-SAFELY
	    IGNORE))

(define-declaration 'INTEGRATE-EXTERNAL
  `(* ,(lambda (object)
	 (or (string? object)
	     (pathname? object))))
  (lambda (declaration selector map-identifier)
    selector map-identifier
    declaration))

(for-each
 (lambda (keyword)
   (define-declaration keyword '(DATUM)
     (lambda (declaration selector map-identifier)
       (list (car declaration)
	     (let loop
		 ((varset (cadr declaration))
		  (selector (selector/add-cadr selector)))
	       (cond ((syntax-match? '('SET * IDENTIFIER) varset)
		      (cons (car varset)
			    (select-map map-identifier
					(cdr varset)
					(selector/add-cdr selector))))
		     ((or (syntax-match? '('UNION * DATUM) varset)
			  (syntax-match? '('INTERSECTION * DATUM) varset)
			  (syntax-match? '('DIFFERENCE DATUM DATUM) varset))
		      (cons (car varset)
			    (select-map loop
					(cdr varset)
					(selector/add-cdr selector))))
		     (else varset)))))))
 '(CONSTANT
   IGNORE-ASSIGNMENT-TRAPS
   IGNORE-REFERENCE-TRAPS
   PURE-FUNCTION
   SIDE-EFFECT-FREE
   USUAL-DEFINITION
   UUO-LINK))

(define-declaration 'REPLACE-OPERATOR '(* (IDENTIFIER * (DATUM DATUM)))
  (lambda (declaration selector map-identifier)
    (cons (car declaration)
	  (select-map
	   (lambda (rule selector)
	     (cons (map-identifier (car rule) (selector/add-car selector))
		   (select-map
		    (lambda (clause selector)
		      (list (car clause)
			    (if (identifier? (cadr clause))
				(map-identifier (cadr clause)
						(selector/add-cadr selector))
				(cadr clause))))
		    (cdr rule))))
	   (cdr declaration)
	   (selector/add-cdr selector)))))

(define-declaration 'REDUCE-OPERATOR '(* (IDENTIFIER DATUM * DATUM))
  (lambda (declaration selector map-identifier)
    (cons (car declaration)
	  (select-map
	   (lambda (rule selector)
	     (cons* (map-identifier (car rule) (selector/add-car selector))
		    (if (identifier? (cadr rule))
			(map-identifier (cadr rule)
					(selector/add-cadr selector))
			(cadr rule))
		    (select-map
		     (lambda (clause selector)
		       (if (or (syntax-match? '('NULL-VALUE IDENTIFIER DATUM)
					      clause)
			       (syntax-match? '('SINGLETON IDENTIFIER)
					      clause)
			       (syntax-match? '('WRAPPER IDENTIFIER ? DATUM)
					      clause))
			   (cons* (car clause)
				  (map-identifier (cadr clause)
						  (selector/add-cadr selector))
				  (cddr clause))
			   clause))
		     (cddr rule)
		     (selector/add-cddr selector))))
	   (cdr declaration)
	   (selector/add-cdr selector)))))

;;;; Identifiers

(define *rename-database*)

(define-structure (rename-database (constructor initial-rename-database ())
				   (conc-name rename-database/))
  (frame-number 0)
  (mapping-table (make-equal-hash-table) read-only #t)
  (unmapping-table (make-eq-hash-table) read-only #t)
  (id-table (make-eq-hash-table) read-only #t))

(define (make-rename-id)
  (delay
    (let ((n (+ (rename-database/frame-number *rename-database*) 1)))
      (set-rename-database/frame-number! *rename-database* n)
      n)))

(define (rename-identifier identifier rename-id)
  (let ((key (cons identifier rename-id))
	(mapping-table (rename-database/mapping-table *rename-database*)))
    (or (hash-table/get mapping-table key #f)
	(let ((mapped-identifier
	       (string->uninterned-symbol
		(symbol-name (identifier->symbol identifier)))))
	  (hash-table/put! mapping-table key mapped-identifier)
	  (hash-table/put! (rename-database/unmapping-table *rename-database*)
			   mapped-identifier
			   key)
	  mapped-identifier))))

(define (rename-top-level-identifier identifier)
  (if (symbol? identifier)
      identifier
      (rename-identifier identifier (delay 0))))

(define (make-name-generator)
  (let ((id (make-rename-id)))
    (lambda (identifier)
      (rename-identifier identifier id))))

;;; Post processing

(define (output/post-process-expression expression)
  (let ((unmapping (empty-unmapping)))
    (compute-substitution expression unmapping)
    (alpha-substitute (unmapping->substitution unmapping) expression)))

(define (empty-unmapping)
  (make-eq-hash-table))

(define (store-unmapping-entry! identifier unmapped-identifier unmapping)
  (hash-table/put! unmapping identifier unmapped-identifier))

(define ((unmapping->substitution unmapping) identifier)
  (or (hash-table/get unmapping identifier #f)
      (finalize-mapped-identifier identifier)))

(define (unmap-identifier identifier)
  (let ((entry
	 (hash-table/get (rename-database/unmapping-table *rename-database*)
			 identifier
			 #f)))
    (if entry
	(identifier->symbol (car entry))
	(begin
	  (if (not (symbol? identifier))
	      (error:bad-range-argument identifier 'UNMAP-IDENTIFIER))
	  identifier))))

(define (finalize-mapped-identifier identifier)
  (let ((entry
	 (hash-table/get (rename-database/unmapping-table *rename-database*)
			 identifier
			 #f)))
    (if entry
	(let ((identifier (car entry))
	      (frame-number (force (cdr entry))))
	  (if (interned-symbol? identifier)
	      (map-interned-symbol identifier frame-number)
	      (map-uninterned-identifier identifier frame-number)))
	(begin
	  (if (not (symbol? identifier))
	      (error:bad-range-argument identifier
					'FINALIZE-MAPPED-IDENTIFIER))
	  identifier))))

(define (map-interned-symbol symbol frame-number)
  (string->symbol
   (string-append "."
		  (symbol-name symbol)
		  "."
		  (number->string frame-number))))

(define (map-uninterned-identifier identifier frame-number)
  (let ((table (rename-database/id-table *rename-database*))
	(symbol (identifier->symbol identifier)))
    (let ((alist (hash-table/get table symbol '())))
      (let ((entry (assv frame-number alist)))
	(if entry
	    (let ((entry* (assq identifier (cdr entry))))
	      (if entry*
		  (cdr entry*)
		  (let ((mapped-symbol
			 (map-indexed-symbol symbol
					     frame-number
					     (length (cdr entry)))))
		    (set-cdr! entry
			      (cons (cons identifier mapped-symbol)
				    (cdr entry)))
		    mapped-symbol)))
	    (let ((mapped-symbol (map-indexed-symbol symbol frame-number 0)))
	      (hash-table/put! table
			       symbol
			       (cons (list frame-number
					   (cons identifier mapped-symbol))
				     alist))
	      mapped-symbol))))))

(define (map-indexed-symbol symbol frame-number index-number)
  (string->symbol
   (string-append "."
		  (symbol-name symbol)
		  "."
		  (number->string frame-number)
		  "-"
		  (number->string index-number))))

;;;; Compute substitution

(define (compute-substitution expression unmapping)
  ((scode-walk compute-substitution-walker expression) expression unmapping))

(define (compute-substitution/variable expression unmapping)
  unmapping
  (singleton-reference-set (variable-name expression)))

(define (compute-substitution/assignment expression unmapping)
  (add-to-reference-set (assignment-name expression)
			(compute-substitution (assignment-value expression)
					      unmapping)))

(define (compute-substitution/unassigned? expression unmapping)
  unmapping
  (singleton-reference-set (unassigned?-name expression)))

(define (compute-substitution/lambda expression unmapping)
  (lambda-components** expression
    (lambda (pattern bound body)
      pattern
      (compute-substitution/binder bound body unmapping))))

(define (compute-substitution/open-block expression unmapping)
  (open-block-components expression
    (lambda (bound declarations body)
      declarations
      (compute-substitution/binder bound body unmapping))))

(define (compute-substitution/binder bound body unmapping)
  (let ((free-references
	 (remove-from-reference-set bound
				    (compute-substitution body unmapping))))
    (let loop
	((identifiers bound)
	 (unmapped (map unmap-identifier bound)))
      (if (pair? identifiers)
	  (begin
	    (if (not (or (unmapping-collision? (car identifiers)
					       (car unmapped)
					       free-references)
			 (memq (car unmapped) (cdr unmapped))))
		(store-unmapping-entry! (car identifiers)
					(car unmapped)
					unmapping))
	    (loop (cdr identifiers) (cdr unmapped)))))
    free-references))

;;; Reference Set

(define (null-reference-set)
  '())

(define (singleton-reference-set identifier)
  (list (cons identifier (unmap-identifier identifier))))

(define (reference-set-union s1 s2)
  (if (pair? s1)
      (if (assq (caar s1) s2)
	  (reference-set-union (cdr s1) s2)
	  (cons (car s1) (reference-set-union (cdr s1) s2)))
      s2))

(define (add-to-reference-set identifier reference-set)
  (if (assq identifier reference-set)
      reference-set
      (cons (cons identifier (unmap-identifier identifier)) reference-set)))

(define (remove-from-reference-set identifiers reference-set)
  (delete-matching-items reference-set
    (lambda (item)
      (memq (car item) identifiers))))

(define (unmapping-collision? identifier unmapped-identifier reference-set)
  (find-matching-item reference-set
    (lambda (item)
      (and (eq? unmapped-identifier (cdr item))
	   (not (eq? identifier (car item)))))))

(define (compute-substitution/subexpression get-subexpression)
  (lambda (expression unmapping)
    (compute-substitution (get-subexpression expression) unmapping)))

(define (compute-substitution/subexpressions get-subexpressions)
  (lambda (expression unmapping)
    (let ((expressions (get-subexpressions expression)))
      (if (pair? expressions)
	  (let loop ((expressions expressions))
	    (if (pair? (cdr expressions))
		(reference-set-union (compute-substitution (car expressions)
							   unmapping)
				     (loop (cdr expressions)))
		(compute-substitution (car expressions) unmapping)))
	  (null-reference-set)))))

(define compute-substitution/access
  (compute-substitution/subexpression access-environment))

(define compute-substitution/combination
  (compute-substitution/subexpressions combination-subexpressions))

(define compute-substitution/comment
  (compute-substitution/subexpression comment-expression))

(define compute-substitution/conditional
  (compute-substitution/subexpressions conditional-subexpressions))

(define compute-substitution/definition
  (compute-substitution/subexpression definition-value))

(define compute-substitution/delay
  (compute-substitution/subexpression delay-expression))

(define compute-substitution/disjunction
  (compute-substitution/subexpressions disjunction-subexpressions))

(define compute-substitution/sequence
  (compute-substitution/subexpressions sequence-actions))

(define (compute-substitution/default expression unmapping)
  expression unmapping
  (null-reference-set))

(define compute-substitution-walker
  (make-scode-walker compute-substitution/default
		     `((ACCESS ,compute-substitution/access)
		       (ASSIGNMENT ,compute-substitution/assignment)
		       (COMBINATION ,compute-substitution/combination)
		       (COMMENT ,compute-substitution/comment)
		       (CONDITIONAL ,compute-substitution/conditional)
		       (DEFINITION ,compute-substitution/definition)
		       (DELAY ,compute-substitution/delay)
		       (DISJUNCTION ,compute-substitution/disjunction)
		       (LAMBDA ,compute-substitution/lambda)
		       (OPEN-BLOCK ,compute-substitution/open-block)
		       (SEQUENCE ,compute-substitution/sequence)
		       (UNASSIGNED? ,compute-substitution/unassigned?)
		       (VARIABLE ,compute-substitution/variable))))

;;;; Alpha substitution

(define (alpha-substitute substitution expression)
  ((scode-walk alpha-substitute-walker expression) substitution expression))

(define (alpha-substitute/variable substitution expression)
  (make-variable (substitution (variable-name expression))))

(define (alpha-substitute/assignment substitution expression)
  (make-assignment (substitution (assignment-name expression))
		   (alpha-substitute substitution
				     (assignment-value expression))))

(define (alpha-substitute/unassigned? substitution expression)
  (make-unassigned? (substitution (unassigned?-name expression))))

(define (alpha-substitute/lambda substitution expression)
  (lambda-components** expression
    (lambda (pattern bound body)
      (make-lambda** pattern
		     (map substitution bound)
		     (alpha-substitute substitution body)))))

(define (alpha-substitute/open-block substitution expression)
  (open-block-components expression
    (lambda (bound declarations body)
      (make-open-block (map substitution bound)
		       (substitute-in-declarations substitution declarations)
		       (alpha-substitute substitution body)))))

(define (alpha-substitute/declaration substitution expression)
  (make-declaration (substitute-in-declarations substitution
						(declaration-text expression))
		    (alpha-substitute substitution
				      (declaration-expression expression))))

(define (substitute-in-declarations substitution declarations)
  (map (lambda (declaration)
	 (process-declaration declaration select-object
			      (lambda (identifier selector)
				selector
				(substitution identifier))
			      (lambda (declaration selector)
				selector
				(error "Ill-formed declaration:"
				       declaration))))
       declarations))

(define (alpha-substitute/default substitution expression)
  substitution
  expression)

(define (simple-substitution reconstruct get-subexpression)
  (lambda (substitution expression)
    (reconstruct expression
		 (alpha-substitute substitution
				   (get-subexpression expression)))))

(define (combinator-substitution reconstruct get-subexpressions)
  (lambda (substitution expression)
    (reconstruct
     (map (lambda (expression)
	    (alpha-substitute substitution expression))
	  (get-subexpressions expression)))))

(define alpha-substitute/access
  (simple-substitution (lambda (expression environment)
			 (make-access environment (access-name expression)))
		       access-environment))

(define alpha-substitute/combination
  (combinator-substitution (lambda (subexpressions)
			     (make-combination (car subexpressions)
					       (cdr subexpressions)))
			   combination-subexpressions))

(define alpha-substitute/comment
  (simple-substitution (lambda (expression subexpression)
			 (make-comment (comment-text expression)
				       subexpression))
		       comment-expression))

(define alpha-substitute/conditional
  (combinator-substitution (lambda (subexpressions)
			     (make-conditional (car subexpressions)
					       (cadr subexpressions)
					       (caddr subexpressions)))
			   conditional-subexpressions))

(define alpha-substitute/definition
  (simple-substitution (lambda (expression value)
			 (make-definition (definition-name expression) value))
		       definition-value))

(define alpha-substitute/delay
  (simple-substitution (lambda (expression subexpression)
			 expression
			 (make-delay subexpression))
		       delay-expression))

(define alpha-substitute/disjunction
  (combinator-substitution (lambda (subexpressions)
			     (make-disjunction (car subexpressions)
					       (cadr subexpressions)))
			   disjunction-subexpressions))

(define alpha-substitute/sequence
  (combinator-substitution make-sequence sequence-actions))

(define alpha-substitute-walker
  (make-scode-walker alpha-substitute/default
		     `((ACCESS ,alpha-substitute/access)
		       (ASSIGNMENT ,alpha-substitute/assignment)
		       (COMBINATION ,alpha-substitute/combination)
		       (COMMENT ,alpha-substitute/comment)
		       (CONDITIONAL ,alpha-substitute/conditional)
		       (DECLARATION ,alpha-substitute/declaration)
		       (DEFINITION ,alpha-substitute/definition)
		       (DELAY ,alpha-substitute/delay)
		       (DISJUNCTION ,alpha-substitute/disjunction)
		       (LAMBDA ,alpha-substitute/lambda)
		       (OPEN-BLOCK ,alpha-substitute/open-block)
		       (SEQUENCE ,alpha-substitute/sequence)
		       (UNASSIGNED? ,alpha-substitute/unassigned?)
		       (VARIABLE ,alpha-substitute/variable))))