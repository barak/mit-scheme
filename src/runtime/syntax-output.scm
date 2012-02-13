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

;;;; Syntaxer Output Interface
;;; package: (runtime syntax output)

(declare (usual-integrations))

(define (transformer-eval output environment)
  (eval output (syntactic-environment->environment environment)))

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

(define (output/disjunction predicate alternative)
  (make-disjunction predicate alternative))

(define (output/sequence expressions)
  (make-sequence expressions))

(define (output/combination operator operands)
  (make-combination operator operands))

(define (output/lambda lambda-list body)
  (output/named-lambda lambda-tag:unnamed lambda-list body))

(define (output/named-lambda name lambda-list body)
  (call-with-values (lambda () (parse-mit-lambda-list lambda-list))
    (lambda (required optional rest)
      (make-lambda* name required optional rest body))))

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
  (let ((temps (map (lambda (name)
		      (utf8-string->uninterned-symbol
		       (string-append (symbol-name (identifier->symbol name))
				      "-value"))) names)))
    (output/let
     names (map (lambda (name) name (output/unassigned)) names)
     (make-sequence
      (cons (output/let
	     temps values
	     (make-sequence (map (lambda (name temp)
				   (make-assignment name (make-variable temp)))
				 names temps)))
	    (list
	     (let ((body (scan-defines body make-open-block)))
	       (if (open-block? body)
		   (output/let '() '() body)
		   body))))))))

(define (output/letrec* names values body)
  (output/let
   names (map (lambda (name) name (output/unassigned)) names)
   (make-sequence
    (append! (map make-assignment names values)
	     (list
	      (let ((body (scan-defines body make-open-block)))
		(if (open-block? body)
		    (output/let '() '() body)
		    body)))))))

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
  (make-combination (ucode-primitive lexical-assignment)
		    (list environment name value)))

(define (output/runtime-reference name)
  (output/access-reference name system-global-environment))

(define (output/local-declare declarations body)
  (make-declaration declarations body))

(define lambda-tag:unnamed '|#[unnamed-procedure]|)
(define lambda-tag:let '|#[let-procedure]|)
(define lambda-tag:fluid-let '|#[fluid-let-procedure]|)

;;;; Post processing

(define (output/post-process-expression expression)
  (let ((unmapping (empty-unmapping)))
    (compute-substitution expression unmapping)
    (alpha-substitute (unmapping->substitution unmapping) expression)))

(define (empty-unmapping)
  (make-strong-eq-hash-table))

(define (store-unmapping-entry! identifier unmapped-identifier unmapping)
  (hash-table/put! unmapping identifier unmapped-identifier))

(define ((unmapping->substitution unmapping) identifier)
  (or (hash-table/get unmapping identifier #f)
      (finalize-mapped-identifier identifier)))

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
	 (map-declaration-identifiers substitution declaration))
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

;;;; Identifiers

(define *rename-database*)

(define-structure (rename-database (constructor initial-rename-database ())
				   (conc-name rename-database/))
  (frame-number 0)
  (mapping-table (make-equal-hash-table) read-only #t)
  (unmapping-table (make-strong-eq-hash-table) read-only #t)
  (id-table (make-strong-eq-hash-table) read-only #t))

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
	       (utf8-string->uninterned-symbol
		(symbol-name (identifier->symbol identifier)))))
	  (hash-table/put! mapping-table key mapped-identifier)
	  (hash-table/put! (rename-database/unmapping-table *rename-database*)
			   mapped-identifier
			   key)
	  mapped-identifier))))

(define (rename-top-level-identifier identifier)
  (if (symbol? identifier)
      identifier
      ;; Generate an uninterned symbol here and now, rather than
      ;; storing anything in the rename database, because we are
      ;; creating a top-level binding for a synthetic name, which must
      ;; be globally unique.  Using the rename database causes the
      ;; substitution logic above to try to use an interned symbol
      ;; with a nicer name.  The decorations on this name are just
      ;; that -- decorations, for human legibility.  It is the use of
      ;; an uninterned symbol that guarantees uniqueness.
      (utf8-string->uninterned-symbol
       (string-append "."
		      (symbol-name (identifier->symbol identifier))
		      "."
		      (number->string (force (make-rename-id)))))))

(define (make-name-generator)
  (let ((id (make-rename-id)))
    (lambda (identifier)
      (rename-identifier identifier id))))

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

(define (map-interned-symbol symbol-to-map frame-number)
  (symbol "." symbol-to-map "." frame-number))

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

(define (map-indexed-symbol symbol-to-map frame-number index-number)
  (symbol "." symbol-to-map "." frame-number "-" index-number))

;;;; Reference Set

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