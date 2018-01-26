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

;;;; Syntaxer Output Interface
;;; package: (runtime syntax output)

(declare (usual-integrations))

(define (transformer-eval output environment)
  (eval output (syntactic-environment->environment environment)))

(define (output/variable name)
  (make-scode-variable name))

(define (output/constant datum)
  datum)

(define (output/assignment name value)
  (make-scode-assignment name value))

(define (output/top-level-definition name value)
  (make-scode-definition name
    (if (scode-lambda? value)
	(lambda-components* value
	  (lambda (name* required optional rest body)
	    (if (eq? name* lambda-tag:unnamed)
		(make-lambda* name required optional rest body)
		value)))
	value)))

(define (output/top-level-syntax-definition name value)
  (make-scode-definition name (make-macro-reference-trap-expression value)))

(define (output/conditional predicate consequent alternative)
  (make-scode-conditional predicate consequent alternative))

(define (output/disjunction predicate alternative)
  (make-scode-disjunction predicate alternative))

(define (output/sequence expressions)
  (make-scode-sequence expressions))

(define (output/combination operator operands)
  (make-scode-combination operator operands))

(define (output/lambda lambda-list body)
  (output/named-lambda lambda-tag:unnamed lambda-list body))

(define (output/named-lambda name lambda-list body)
  (call-with-values (lambda () (parse-mit-lambda-list lambda-list))
    (lambda (required optional rest)
      (make-lambda* name required optional rest body))))

(define (output/delay expression)
  (make-scode-delay expression))

(define (output/unassigned-test name)
  (make-scode-unassigned? name))

(define (output/unassigned)
  (make-unassigned-reference-trap))

(define (output/unspecific)
  unspecific)

(define (output/let names values body)
  (output/combination (output/named-lambda lambda-tag:let names body) values))

(define (output/letrec names values body)
  (let ((temps (map (lambda (name)
		      (string->uninterned-symbol
		       (string-append (symbol->string (identifier->symbol name))
				      "-value"))) names)))
    (output/let
     names (map (lambda (name) name (output/unassigned)) names)
     (make-scode-sequence
      (cons (output/let
	     temps values
	     (make-scode-sequence
	      (map (lambda (name temp)
		     (make-scode-assignment name (make-scode-variable temp)))
		   names
		   temps)))
	    (list
	     (let ((body (scan-defines body make-scode-open-block)))
	       (if (scode-open-block? body)
		   (output/let '() '() body)
		   body))))))))

(define (output/body declarations body)
  (scan-defines (let ((declarations (apply append declarations)))
		  (if (pair? declarations)
		      (make-scode-sequence
		       (list (make-block-declaration declarations)
			     body))
		      body))
		make-scode-open-block))

(define (output/definition name value)
  (make-scode-definition name value))

(define (output/top-level-sequence declarations expressions)
  (let ((declarations (apply append declarations))
	(make-scode-open-block
	 (lambda (expressions)
	   (scan-defines (make-scode-sequence expressions)
			 make-scode-open-block))))
    (if (pair? declarations)
	(make-scode-open-block
	 (cons (make-block-declaration declarations)
	       (if (pair? expressions)
		   expressions
		   (list (output/unspecific)))))
	(if (pair? expressions)
	    (if (pair? (cdr expressions))
		(make-scode-open-block expressions)
		(car expressions))
	    (output/unspecific)))))

(define (output/the-environment)
  (make-scode-the-environment))

(define (output/access-reference name environment)
  (make-scode-access environment name))

(define (output/access-assignment name environment value)
  (make-scode-combination (ucode-primitive lexical-assignment)
		    (list environment name value)))

(define (output/runtime-reference name)
  (output/access-reference name system-global-environment))

(define lambda-tag:unnamed '|#[unnamed-procedure]|)
(define lambda-tag:let '|#[let-procedure]|)
(define lambda-tag:fluid-let '|#[fluid-let-procedure]|)

;;;; Post processing

(define (output/post-process-expression expression)
  (let ((safe-set (make-strong-eq-hash-table)))
    (compute-substitution expression
			  (lambda (rename original)
			    (hash-table/put! safe-set rename original)))
    (alpha-substitute (unmapping->substitution safe-set) expression)))

(define ((unmapping->substitution safe-set) rename)
  (or (hash-table/get safe-set rename #f)
      (finalize-mapped-identifier rename)))

(define (mark-local-bindings bound body mark-safe!)
  (let ((free
	 (lset-difference eq?
			  (compute-substitution body mark-safe!)
			  bound)))
    (for-each (lambda (rename)
		(let ((original (rename->original rename)))
		  (if (not (any (lambda (rename*)
				  (eq? original (rename->original rename*)))
				free))
		      (mark-safe! rename original))))
	      bound)
    free))

;;;; Compute substitution

(define compute-substitution)
(add-boot-init!
 (lambda ()
   (define (define-cs-handler predicate handler)
     (define-predicate-dispatch-handler compute-substitution
       (list predicate any-object?)
       handler))

   (define (simple-subexpression get-subexpression)
     (lambda (expression mark-safe!)
       (compute-substitution (get-subexpression expression) mark-safe!)))

   (define (simple-subexpressions get-subexpressions)
     (lambda (expression mark-safe!)
       (reduce (lambda (s1 s2)
		 (lset-union eq? s1 s2))
	       '()
	       (map (lambda (expression)
		      (compute-substitution expression mark-safe!))
		    (get-subexpressions expression)))))

   (set! compute-substitution
	 (standard-predicate-dispatcher 'compute-substitution 2))

   (define-predicate-dispatch-default-handler compute-substitution
     (lambda (expression mark-safe!)
       (declare (ignore expression mark-safe!))
       '()))

   (define-cs-handler scode-variable?
     (lambda (expression mark-safe!)
       (declare (ignore mark-safe!))
       (list (scode-variable-name expression))))

   (define-cs-handler scode-assignment?
     (lambda (expression mark-safe!)
       (lset-adjoin eq?
		    (compute-substitution (scode-assignment-value expression)
					  mark-safe!)
		    (scode-assignment-name expression))))

   (define-cs-handler scode-unassigned??
     (lambda (expression mark-safe!)
       (declare (ignore mark-safe!))
       (list (scode-unassigned?-name expression))))

   (define-cs-handler scode-lambda?
     (lambda (expression mark-safe!)
       (lambda-components** expression
	 (lambda (pattern bound body)
	   (declare (ignore pattern))
	   (mark-local-bindings bound body mark-safe!)))))

   (define-cs-handler scode-open-block?
     (lambda (expression mark-safe!)
       (mark-local-bindings (scode-open-block-names expression)
			    (scode-open-block-actions expression)
			    mark-safe!)))

   (define-cs-handler scode-access?
     (simple-subexpression scode-access-environment))

   (define-cs-handler scode-combination?
     (simple-subexpressions
      (lambda (expr)
	(cons (scode-combination-operator expr)
	      (scode-combination-operands expr)))))

   (define-cs-handler scode-comment?
     (simple-subexpression scode-comment-expression))

   (define-cs-handler scode-conditional?
     (simple-subexpressions
      (lambda (expr)
	(list (scode-conditional-predicate expr)
	      (scode-conditional-consequent expr)
	      (scode-conditional-alternative expr)))))

   (define-cs-handler scode-definition?
     (simple-subexpression scode-definition-value))

   (define-cs-handler scode-delay?
     (simple-subexpression scode-delay-expression))

   (define-cs-handler scode-disjunction?
     (simple-subexpressions
      (lambda (expr)
	(list (scode-disjunction-predicate expr)
	      (scode-disjunction-alternative expr)))))

   (define-cs-handler scode-sequence?
     (simple-subexpressions scode-sequence-actions))

   ))

;;;; Alpha substitution

(define alpha-substitute)
(add-boot-init!
 (lambda ()

   (define (define-as-handler predicate handler)
     (define-predicate-dispatch-handler alpha-substitute
       (list any-object? predicate)
       handler))

   (define (simple-substitution reconstruct . parts)
     (lambda (substitution expression)
       (apply reconstruct
	      (map (lambda (part)
		     (alpha-substitute substitution (part expression)))
		   parts))))

   (define (partial-substitution selector reconstruct . parts)
     (lambda (substitution expression)
       (apply reconstruct
	      (map (lambda (substitute? part)
		     (if substitute?
			 (alpha-substitute substitution (part expression))
			 (part expression)))
		   selector
		   parts))))

   (define (combinator-substitution reconstruct get-subexpressions)
     (lambda (substitution expression)
       (reconstruct
	(map (lambda (expression)
	       (alpha-substitute substitution expression))
	     (get-subexpressions expression)))))

   (set! alpha-substitute
	 (standard-predicate-dispatcher 'alpha-substitute 2))

   (define-predicate-dispatch-default-handler alpha-substitute
     (lambda (substitution expression)
       (declare (ignore substitution))
       expression))

   (define-as-handler scode-variable?
     (lambda (substitution expression)
       (make-scode-variable (substitution (scode-variable-name expression)))))

   (define-as-handler scode-assignment?
     (lambda (substitution expression)
       (make-scode-assignment
	(substitution (scode-assignment-name expression))
	(alpha-substitute substitution (scode-assignment-value expression)))))

   (define-as-handler scode-unassigned??
     (lambda (substitution expression)
       (make-scode-unassigned?
	(substitution (scode-unassigned?-name expression)))))

   (define-as-handler scode-lambda?
     (lambda (substitution expression)
       (lambda-components** expression
	 (lambda (pattern bound body)
	   (make-lambda** pattern
			  (map substitution bound)
			  (alpha-substitute substitution body))))))

   (define-as-handler scode-open-block?
     (lambda (substitution expression)
       (make-scode-open-block
	(map substitution (scode-open-block-names expression))
	(map (lambda (declaration)
	       (map-declaration-identifiers substitution declaration))
	     (scode-open-block-declarations expression))
	(alpha-substitute substitution (scode-open-block-actions expression)))))

   (define-as-handler scode-declaration?
     (lambda (substitution expression)
       (make-scode-declaration
	(map (lambda (declaration)
	       (map-declaration-identifiers substitution declaration))
	     (scode-declaration-text expression))
	(alpha-substitute substitution
			  (scode-declaration-expression expression)))))

   (define-as-handler scode-access?
     (partial-substitution '(#t #f)
			   make-scode-access
			   scode-access-environment
			   scode-access-name))

   (define-as-handler scode-combination?
     (combinator-substitution
      (lambda (subexpressions)
	(make-scode-combination (car subexpressions) (cdr subexpressions)))
      (lambda (expression)
	(cons (scode-combination-operator expression)
	      (scode-combination-operands expression)))))

   (define-as-handler scode-comment?
     (partial-substitution '(#f #t)
			   make-scode-comment
			   scode-comment-text
			   scode-comment-expression))

   (define-as-handler scode-conditional?
     (simple-substitution make-scode-conditional
			  scode-conditional-predicate
			  scode-conditional-consequent
			  scode-conditional-alternative))

   (define-as-handler scode-definition?
     (partial-substitution '(#f #t)
			   make-scode-definition
			   scode-definition-name
			   scode-definition-value))

   (define-as-handler scode-delay?
     (simple-substitution make-scode-delay
			  scode-delay-expression))

   (define-as-handler scode-disjunction?
     (simple-substitution make-scode-disjunction
			  scode-disjunction-predicate
			  scode-disjunction-alternative))

   (define-as-handler scode-sequence?
     (combinator-substitution make-scode-sequence scode-sequence-actions))

   ))

;;;; Identifiers

(define-deferred *rename-database*
  (make-unsettable-parameter 'UNBOUND))

(define-structure (rename-database (constructor initial-rename-database ())
				   (conc-name rename-database/))
  (frame-number 0)
  (mapping-table (make-equal-hash-table) read-only #t)
  (unmapping-table (make-strong-eq-hash-table) read-only #t)
  (id-table (make-strong-eq-hash-table) read-only #t))

(define (make-rename-id)
  (delay
    (let* ((renames (*rename-database*))
	   (n (+ (rename-database/frame-number renames) 1)))
      (set-rename-database/frame-number! renames n)
      n)))

(define (rename-identifier identifier rename-id)
  (let ((key (cons identifier rename-id))
	(renames (*rename-database*)))
    (let ((mapping-table (rename-database/mapping-table renames)))
      (or (hash-table/get mapping-table key #f)
	  (let ((mapped-identifier
		 (string->uninterned-symbol
		  (symbol->string (identifier->symbol identifier)))))
	    (hash-table/put! mapping-table key mapped-identifier)
	    (hash-table/put! (rename-database/unmapping-table renames)
			     mapped-identifier
			     key)
	    mapped-identifier)))))

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
      (string->uninterned-symbol
       (string-append "."
		      (symbol->string (identifier->symbol identifier))
		      "."
		      (number->string (force (make-rename-id)))))))

(define (make-name-generator)
  (let ((id (make-rename-id)))
    (lambda (identifier)
      (rename-identifier identifier id))))

(define (rename->original identifier)
  (let ((entry
	 (hash-table/get (rename-database/unmapping-table
			  (*rename-database*))
			 identifier
			 #f)))
    (if entry
	(identifier->symbol (car entry))
	(begin
	  (if (not (symbol? identifier))
	      (error:bad-range-argument identifier 'RENAME->ORIGINAL))
	  identifier))))

(define (finalize-mapped-identifier identifier)
  (let ((entry
	 (hash-table/get (rename-database/unmapping-table
			  (*rename-database*))
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
  (let ((table (rename-database/id-table (*rename-database*)))
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