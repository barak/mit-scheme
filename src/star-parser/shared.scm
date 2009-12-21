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

;;;; Shared code for matchers and parsers

(declare (usual-integrations))

(define *buffer-name*)
(define *environment*)
(define *closing-environment*)
(define debug:disable-optimizers? #f)
(define debug:trace-optimization? #f)

(define (generate-external-procedure expression environment
				     preprocessor generator)
  (capture-syntactic-environment
   (lambda (closing-environment)
     (fluid-let ((*id-counters* '())
		 (*environment* environment)
		 (*closing-environment* closing-environment))
       (let ((external-bindings (list 'BINDINGS))
	     (internal-bindings (list 'BINDINGS))
	     (b (make-synthetic-identifier 'B)))
	 (let ((expression
		(preprocessor expression external-bindings internal-bindings)))
	   (let ((body
		  ;; Note that PROTECT is used here as a marker to
		  ;; identify code that has potential side effects.
		  ;; See below for an explanation.
		  (fluid-let ((*buffer-name* `(PROTECT ,b)))
		     (maybe-make-let (map (lambda (b)
					    (list (cdr b) (car b)))
					  (cdr internal-bindings))
		       (strip-protection-wrappers
			(run-optimizers
			 (generator
			  expression
			  (append (map cdr (cdr external-bindings))
				  (map cdr (cdr internal-bindings))))))))))
	     (maybe-make-let (map (lambda (b) (list (cdr b) (car b)))
				  (cdr external-bindings))
	       `(LAMBDA (,b)
		  ,@(if (> (car (count-references (list b) body)) 0)
			'()
			(list b))
		  ,body)))))))))

(define (run-optimizers expression)
  (if debug:disable-optimizers?
      expression
      (let ((expression*
	     (trace-optimizer-tl
	      "after all optimizations"
	      (peephole-optimize
	       (trace-optimizer-tl
		"after pointer optimization"
		(optimize-pointer-usage
		 (trace-optimizer-tl
		  "after substitution"
		  (optimize-by-substitution
		   (trace-optimizer-tl "before optimization"
				       expression)))))))))
	(if (equal? expression* expression)
	    expression
	    (run-optimizers expression*)))))

(define (strip-protection-wrappers expression)
  (if (pair? expression)
      (case (car expression)
	((LAMBDA)
	 `(LAMBDA ,(cadr expression)
	    ,(strip-protection-wrappers (caddr expression))))
	((LET)
	 `(LET ,(cadr expression)
	    ,(map (lambda (binding)
		    (list (car binding)
			  (strip-protection-wrappers (cadr binding))))
		  (caddr expression))
	    ,(strip-protection-wrappers (cadddr expression))))
	((PROTECT)
	 (cadr expression))
	(else
	 (map strip-protection-wrappers expression)))
      expression))

;;;; Support for preprocessing

(define (check-0-args expression)
  (check-n-args 0 expression))

(define (check-1-arg expression #!optional predicate)
  (check-n-args 1 expression predicate)
  (cadr expression))

(define (check-2-args expression #!optional predicate)
  (check-n-args 2 expression predicate))

(define (check-n-args n expression #!optional predicate)
  (if (not (and (eqv? (list?->length (cdr expression)) n)
		(or (default-object? predicate)
		    (predicate expression))))
      (error:ill-formed-expression expression)))

(define (error:ill-formed-expression expression)
  (error "Ill-formed expression:" expression))

(define (handle-complex-expression expression bindings)
  (if (or (char? expression)
	  (string? expression)
	  (symbol? expression))
      expression
      (let loop ((bindings* (cdr bindings)))
	(if (pair? bindings*)
	    (if (equal? expression (caar bindings*))
		(cdar bindings*)
		(loop (cdr bindings*)))
	    (let ((variable (generate-uninterned-symbol)))
	      (set-cdr! bindings
			(cons (cons expression variable)
			      (cdr bindings)))
	      variable)))))

(define (close expression)
  (close-syntax expression *closing-environment*))

;;;; Parser macros

(define-record-type <parser-macros>
    (%make-parser-macros parent matcher-table parser-table)
    parser-macros?
  (parent parent-macros)
  (matcher-table matcher-macros-table)
  (parser-table parser-macros-table))

(define (make-parser-macros parent)
  (if parent (guarantee-parser-macros parent 'MAKE-PARSER-MACROS))
  (%make-parser-macros (or parent *global-parser-macros*)
		       (make-strong-eq-hash-table)
		       (make-strong-eq-hash-table)))

(define *global-parser-macros*
  (%make-parser-macros #f
		       (make-strong-eq-hash-table)
		       (make-strong-eq-hash-table)))

(define (guarantee-parser-macros object procedure)
  (if (not (parser-macros? object))
      (error:wrong-type-argument object "parser macros" procedure)))

(define (define-matcher-macro name expander)
  (hash-table/put! (matcher-macros-table *parser-macros*) name expander))

(define (lookup-matcher-macro name)
  (let loop ((environment *parser-macros*))
    (and environment
	 (or (hash-table/get (matcher-macros-table environment) name #f)
	     (loop (parent-macros environment))))))

(define (define-parser-macro name expander)
  (hash-table/put! (parser-macros-table *parser-macros*) name expander))

(define (lookup-parser-macro name)
  (let loop ((environment *parser-macros*))
    (and environment
	 (or (hash-table/get (parser-macros-table environment) name #f)
	     (loop (parent-macros environment))))))

(define (with-current-parser-macros macros thunk)
  (guarantee-parser-macros macros 'WITH-CURRENT-PARSER-MACROS)
  (fluid-let ((*parser-macros* macros))
    (thunk)))

(define (current-parser-macros)
  *parser-macros*)

(define (set-current-parser-macros! macros)
  (guarantee-parser-macros macros 'SET-CURRENT-PARSER-MACROS!)
  (set! *parser-macros* macros)
  unspecific)

(define (global-parser-macros)
  *global-parser-macros*)

(define *parser-macros*
  *global-parser-macros*)

;;;; Support for code generation

(define (maybe-make-let bindings body)
  (if (pair? bindings)
      `((LAMBDA ,(map car bindings) ,body)
	,@(map cadr bindings))
      body))

(define (with-value-binding expression generator)
  `(,(let ((v (make-value-identifier)))
       `(LAMBDA (,v)
	  ,(generator v)))
    ,expression))

(define (call-with-pointer pointer procedure)
  (if pointer
      (procedure pointer)
      `(,(let ((p (make-ptr-identifier)))
	   `(LAMBDA (,p)
	      ,(procedure p)))
	,(fetch-pointer))))

(define (fetch-pointer)
  `(GET-PARSER-BUFFER-POINTER ,*buffer-name*))

(define (backtracking-kf pointer generate-body)
  (make-kf-lambda
   (lambda ()
     `(BEGIN
	(SET-PARSER-BUFFER-POINTER! ,*buffer-name* ,pointer)
	,(generate-body)))))

(define (make-kf-lambda generator)
  (make-delayed-lambda make-kf-identifier (list) generator))

(define (make-matcher-ks-lambda generator)
  (make-delayed-lambda make-ks-identifier
		       (list make-kf-identifier)
		       generator))

(define (make-parser-ks-lambda generator)
  (make-delayed-lambda make-ks-identifier
		       (list make-value-identifier make-kf-identifier)
		       generator))

(define (protect expression free-names)
  `(PROTECT ,(make-syntactic-closure *environment* free-names expression)))

(define (make-kf-identifier)
  (generate-identifier 'KF))

(define (make-ks-identifier)
  (generate-identifier 'KS))

(define (make-ptr-identifier)
  (generate-identifier 'P))

(define (make-value-identifier)
  (generate-identifier 'V))

(define (make-loop-identifier)
  (generate-identifier 'L))

(define (generate-identifier prefix)
  (string->uninterned-symbol
   (string-append
    internal-identifier-prefix
    (symbol-name prefix)
    (number->string
     (let ((entry (assq prefix *id-counters*)))
       (if entry
	   (let ((n (cdr entry)))
	     (set-cdr! entry (+ n 1))
	     n)
	   (begin
	     (set! *id-counters* (cons (cons prefix 2) *id-counters*))
	     1))))
    internal-identifier-suffix)))

(define (internal-identifier? identifier)
  (let ((string (symbol-name identifier)))
    (and (string-prefix? internal-identifier-prefix string)
	 (string-suffix? internal-identifier-suffix string))))

(define internal-identifier-prefix "#[")
(define internal-identifier-suffix "]")
 
(define *id-counters*)

;;;; Substitution optimizer 

;;; The substitution optimizer assumes that the generated code has a
;;; simplified syntax.  It further assumes that all code written by
;;; the end user has been wrapped with PROTECT forms, and it ignores
;;; anything in those forms.  Because virtually anything can appear
;;; inside a PROTECT, it's assumed that the presence of PROTECT
;;; implies a possible side-effect.  To simplify detection of side
;;; effects, the buffer name is wrapped in PROTECT, to imply that all
;;; operations on the buffer contain side effects.

(define (bind-delayed-lambdas body-generator . operands)
  `(,(let ((parameters (map (lambda (operand) ((car operand))) operands)))
       `(LAMBDA ,parameters
	  ,(apply body-generator parameters)))
    ,@(map cadr operands)))

(define (make-delayed-lambda name-generator
			     parameter-name-generators
			     body-generator)
  (list name-generator
	(let ((parameters
	       (map (lambda (generator) (generator))
		    parameter-name-generators)))
	  `(LAMBDA ,parameters
	     ,(apply body-generator parameters)))))

(define (delay-call operator . operands)
  `(,operator ,@operands))

(define (delay-reference object)
  object)

(define (lambda-expression? expression)
  (and (pair? expression)
       (eq? (car expression) 'LAMBDA)))

(define (optimize-by-substitution expression)
  (if (pair? expression)
      (case (car expression)
	((LAMBDA)
	 `(LAMBDA ,(cadr expression)
	    ,(optimize-by-substitution (caddr expression))))
	((LET)
	 (maybe-resubstitute
	  (call-with-values
	      (lambda ()
		(discard-unused-loop-bindings
		 (cadr expression)
		 (map (lambda (binding)
			`(,(car binding)
			  ,(optimize-by-substitution (cadr binding))))
		      (caddr expression))
		 (optimize-by-substitution (cadddr expression))))
	    (lambda (identifier bindings body)
	      `(LET ,identifier ,bindings ,body)))
	  expression))
	((PROTECT)
	 expression)
	((VECTOR-APPEND)
	 ;; This seems redundant, since the peephole optimizer does
	 ;; this too.  But it's needed to simplify value-aggregation
	 ;; expressions so that they are properly recognized by
	 ;; OPERAND-COPYABLE?.  For example, it's common to have a
	 ;; procedure whose body is (VECTOR-APPEND (VECTOR) V), which
	 ;; simplifies to V.  And a procedure whose body is a variable
	 ;; reference may be freely copied.
	 (optimize-group-expression-1
	  (flatten-subexpressions (map optimize-by-substitution expression))
	  'VECTOR-APPEND
	  '(VECTOR)))
	(else
	 (substitute-let-expression
	  (map optimize-by-substitution expression))))
      expression))

(define (substitute-let-expression expression)
  (let ((operator (car expression))
	(operands (cdr expression)))
    (if (lambda-expression? operator)
	(let ((body (caddr operator)))
	  (call-with-values
	      (lambda () (compute-substitutions (cadr operator) operands body))
	    (lambda (bindings substitutions)
	      (maybe-resubstitute
	       (call-with-values
		   (lambda ()
		     (discard-parameters-from-operands
		      bindings
		      (apply-substitutions substitutions body)))
		 maybe-make-let)
	       expression))))
	expression)))

(define (maybe-resubstitute result expression)
  (if (equal? result expression)
      expression
      (begin
	(trace-optimization 'SUBSTITUTE expression result)
	(optimize-by-substitution result))))

(define (compute-substitutions identifiers operands body)
  (let loop
      ((identifiers identifiers)
       (operands operands)
       (counts (count-references identifiers body))
       (bindings '())
       (substitutions '()))
    (if (pair? identifiers)
	(let ((identifier (car identifiers))
	      (operand (car operands))
	      (count (car counts)))
	  (cond ((and (= 0 count)
		      (internal-identifier? identifier)
		      (operand-discardable? operand))
		 (loop (cdr identifiers)
		       (cdr operands)
		       (cdr counts)
		       bindings
		       substitutions))
		((and (internal-identifier? identifier)
		      (or (operand-copyable? operand)
			  (and (= 1 count)
			       (operand-substitutable? operand body))))
		 (loop (cdr identifiers)
		       (cdr operands)
		       (cdr counts)
		       bindings
		       (cons (cons identifier operand) substitutions)))
		(else
		 (loop (cdr identifiers)
		       (cdr operands)
		       (cdr counts)
		       (cons (list identifier operand) bindings)
		       substitutions))))
	(values (reverse! bindings) substitutions))))

(define (apply-substitutions substitutions expression)
  (if (pair? substitutions)
      (let loop ((expression expression) (substitutions substitutions))
	(cond ((pair? expression)
	       (case (car expression)
		 ((LAMBDA)
		  `(LAMBDA ,(cadr expression)
		     ,(loop (caddr expression)
			    (delete-matching-items substitutions
			      (lambda (s)
				(memq (car s) (cadr expression)))))))
		 ((LET)
		  `(LET ,(cadr expression)
		     ,(map (lambda (binding)
			     `(,(car binding)
			       ,(loop (cadr binding) substitutions)))
			   (caddr expression))
		     ,(loop (cadddr expression)
			    (delete-matching-items substitutions
			      (lambda (s)
				(or (eq? (car s) (cadr expression))
				    (assq (car s) (caddr expression))))))))
		 ((PROTECT)
		  expression)
		 (else
		  (let ((expression
			 (map (lambda (expression)
				(loop expression substitutions))
			      expression)))
		    (if (and (lambda-expression? (car expression))
			     (null? (cadr (car expression)))
			     (null? (cdr expression)))
			(caddr (car expression))
			expression)))))
	      ((symbol? expression)
	       (let ((entry (assq expression substitutions)))
		 (if entry
		     (cdr entry)
		     expression)))
	      (else expression)))
      expression))

;;; Procedures that discard unused parameters and operands.

(define (discard-unused-loop-bindings identifier bindings body)
  ;; Discard unused parameters of a LET loop.
  (let ((discards
	 (let ((identifiers (map car bindings)))
	   (map (lambda (count identifier operand)
		  (and (= 0 count)
		       (internal-identifier? identifier)
		       (operand-discardable? operand)))
		(count-references identifiers body)
		identifiers
		(map cadr bindings)))))
    (if (there-exists? discards (lambda (discard) discard))
	(values identifier
		(apply-discards-to-list discards bindings)
		(apply-discards-to-calls identifier discards body))
	(values identifier bindings body))))

(define (discard-parameters-from-operands bindings body)
  ;; Discard unused parameters from LAMBDA expressions that are
  ;; operands of a LET.  (The unused parameters of the LET itself were
  ;; previously discarded.)
  (let loop ((bindings bindings) (body body) (bindings* '()))
    (if (pair? bindings)
	(let ((identifier (car (car bindings)))
	      (operand (cadr (car bindings))))
	  (call-with-values
	      (lambda ()
		(discard-parameters-from-operand identifier operand body))
	    (lambda (operand body)
	      (loop (cdr bindings)
		    body
		    (cons (list identifier operand) bindings*)))))
	(values (reverse! bindings*) body))))

(define (discard-parameters-from-operand identifier operand body)
  (if (lambda-expression? operand)
      (let ((identifiers (cadr operand))
	    (body* (caddr operand)))
	(let ((discards
	       (map (lambda (count) (= 0 count))
		    (count-references identifiers body*))))
	  (if (there-exists? discards (lambda (discard) discard))
	      (values `(LAMBDA ,(apply-discards-to-list discards identifiers)
			 ,body*)
		      (apply-discards-to-calls identifier discards body))
	      (values operand body))))
      (values operand body)))

(define (apply-discards-to-calls identifier discards expression)
  ;; Find each call to IDENTIFIER in EXPRESSION and apply DISCARDS to
  ;; the operands of the call.
  (let loop ((expression expression))
    (if (pair? expression)
	(if (eq? identifier (car expression))
	    (call-with-values
		(lambda ()
		  (apply-discards-to-operands discards (cdr expression)))
	      (lambda (kept not-discarded)
		(let ((call (cons identifier kept)))
		  (if (pair? not-discarded)
		      `(BEGIN ,@not-discarded ,call)
		      call))))
	    (case (car expression)
	      ((LAMBDA)
	       (if (memq identifier (cadr expression))
		   expression
		   `(LAMBDA ,(cadr expression)
		      ,(loop (caddr expression)))))
	      ((LET)
	       `(LET ,(cadr expression)
		  ,(map (lambda (binding)
			  `(,(car binding) ,(loop (cadr binding))))
			(caddr expression))
		  ,(if (or (eq? identifier (cadr expression))
			   (assq identifier (caddr expression)))
		       (cadddr expression)
		       (loop (cadddr expression)))))
	      ((PROTECT)
	       expression)
	      (else
	       (map loop expression))))
	expression)))

(define (apply-discards-to-operands discards operands)
  (let loop
      ((discards discards)
       (operands operands)
       (kept '())
       (not-discarded '()))
    (if (pair? discards)
	(if (car discards)
	    (loop (cdr discards)
		  (cdr operands)
		  kept
		  (if (operand-discardable? (car operands))
		      not-discarded
		      (cons (car operands) not-discarded)))
	    (loop (cdr discards)
		  (cdr operands)
		  (cons (car operands) kept)
		  not-discarded))
	(values (reverse! kept) (reverse! not-discarded)))))

(define (apply-discards-to-list discards items)
  (if (pair? discards)
      (if (car discards)
	  (apply-discards-to-list (cdr discards) (cdr items))
	  (cons (car items)
		(apply-discards-to-list (cdr discards) (cdr items))))
      '()))

;;; Predicates that control the substitution process.

(define (operand-copyable? operand)
  ;; Returns true iff OPERAND can be freely copied.  Any variable that
  ;; is bound to such an operand is eliminated by beta substitution.
  (or (symbol? operand)
      (and (lambda-expression? operand)
	   (let ((body (caddr operand)))
	     (or (boolean? body)
		 (symbol? body)
		 (and (syntax-match?
		       '('BEGIN
			  ('SET-PARSER-BUFFER-POINTER! EXPRESSION IDENTIFIER)
			  EXPRESSION)
		       body)
		      (or (boolean? (caddr body))
			  (symbol? (caddr body)))))))
      (equal? operand '(VECTOR))))

(define (operand-substitutable? operand body)
  ;; Returns true iff OPERAND can be moved from a binding site to a
  ;; reference site.  If a variable is bound to one of these operands
  ;; and has only one reference, it can be eliminated by beta
  ;; substitution.
  (or (lambda-expression? operand)
      (not (and (expression-may-have-side-effects? operand)
		(expression-may-have-side-effects? body)))))

(define (operand-discardable? operand)
  ;; Returns true iff OPERAND can be removed from the program,
  ;; provided that its value is unused.
  (or (lambda-expression? operand)
      (not (expression-may-have-side-effects? operand))))

(define (expression-may-have-side-effects? expression)
  (let loop ((tree expression))
    (if (pair? tree)
	(or (loop (car tree))
	    (loop (cdr tree)))
	(eq? 'PROTECT tree))))

(define (count-references identifiers expression)
  ;; For each element of IDENTIFIERS, count the number of references
  ;; in EXPRESSION.  Result is a list of counts.
  (let ((alist
	 (map (lambda (identifier)
		(cons identifier 0))
	      identifiers)))
    (let loop ((expression expression) (alist alist))
      (cond ((pair? expression)
	     (case (car expression)
	       ((LAMBDA)
		(loop (caddr expression)
		      (delete-matching-items alist
			(lambda (entry)
			  (memq (car entry) (cadr expression))))))
	       ((LET)
		(for-each (lambda (binding)
			    (loop (cadr binding) alist))
			  (caddr expression))
		(loop (cadddr expression)
		      (delete-matching-items alist
			(lambda (entry)
			  (or (eq? (car entry) (cadr expression))
			      (assq (car entry) (caddr expression)))))))
	       ((PROTECT)
		unspecific)
	       (else
		(for-each (lambda (expression)
			    (loop expression alist))
			  expression))))
	    ((identifier? expression)
	     (let ((entry (assq expression alist)))
	       (if entry
		   (set-cdr! entry (+ (cdr entry) 1)))))))
    (map cdr alist)))

;;;; Pointer optimizer

(define (optimize-pointer-usage expression)
  (optimize-pointer-usage* expression (make-empty-pointers)))

(define (optimize-pointer-usage* expression pointers)
  #|
  (fresh-line)
  (write-string ";optimize-pointer-usage*")
  (newline)
  (pp expression)
  (pp pointers)
  (newline)
  |#
  (cond ((or (find-matching-item pointer-optimizations
	       (lambda (p)
		 (syntax-match? (car p) expression)))
	     (find-matching-item default-pointer-optimizations
	       (lambda (p)
		 (syntax-match? (car p) expression))))
	 => (lambda (p)
	      (let ((expression* ((cdr p) expression pointers)))
		(if (equal? expression* expression)
		    expression
		    (begin
		      (trace-optimization 'POINTER expression expression*
					  pointers)
		      (optimize-pointer-usage* expression* pointers))))))
	((pair? expression)
	 (map (lambda (expression)
		(optimize-pointer-usage* expression pointers))
	      expression))
	((identifier? expression)
	 (canonicalize-pointer-ref expression pointers))
	(else expression)))

(define (define-pointer-optimization pattern optimizer)
  (let ((p (assoc pattern pointer-optimizations)))
    (if p
	(set-cdr! p optimizer)
	(begin
	  (set! pointer-optimizations
		(cons (cons pattern optimizer)
		      pointer-optimizations))
	  unspecific))))

(define pointer-optimizations '())

(define (define-default-pointer-optimization pattern optimizer)
  (let ((p (assoc pattern default-pointer-optimizations)))
    (if p
	(set-cdr! p optimizer)
	(begin
	  (set! default-pointer-optimizations
		(cons (cons pattern optimizer)
		      default-pointer-optimizations))
	  unspecific))))

(define default-pointer-optimizations '())

(define (make-empty-pointers)
  (cons #f '()))

(define (current-pointer? identifier pointers)
  (memq identifier (%current-pointers pointers)))

(define (new-pointer expression identifier pointers)
  (optimize-pointer-usage* expression (%new-pointer identifier pointers)))

(define (no-pointer expression pointers)
  (optimize-pointer-usage* expression (%no-pointer pointers)))

(define (drop-pointer-refs expression identifiers pointers)
  (optimize-pointer-usage* expression
			   (%drop-pointer-refs identifiers pointers)))

(define (canonicalize-pointer-ref identifier pointers)
  ;; Use outermost equivalent reference.
  (let ((ids (%id-pointers identifier pointers)))
    (if (pair? ids)
	(car (last-pair ids))
	identifier)))

(define (%new-pointer identifier pointers)
  (if (car pointers)
      (let ((ids (%current-pointers pointers)))
	(if (memq identifier ids)
	    pointers
	    (cons (car pointers)
		  (replace-item ids (cons identifier ids) (cdr pointers)))))
      (let ((ids (%id-pointers identifier pointers)))
	(if (pair? ids)
	    (cons (car (last-pair ids)) (cdr pointers))
	    (cons identifier (cons (list identifier) (cdr pointers)))))))

(define (replace-item from to items)
  (let loop ((items items))
    (if (not (pair? items))
	(error:bad-range-argument from 'REPLACE-ITEM))
    (if (eq? (car items) from)
	(cons to (cdr items))
	(cons (car items) (loop (cdr items))))))

(define (%no-pointer pointers)
  (if (car pointers)
      (cons #f (cdr pointers))
      pointers))

(define (%drop-pointer-refs identifiers pointers)
  (cons #f
	(map (lambda (ids)
	       (delete-matching-items ids
		 (lambda (id)
		   (memq id identifiers))))
	     (cdr pointers))))

(define (%current-pointers pointers)
  (if (car pointers)
      (find-matching-item (cdr pointers)
	(lambda (identifiers)
	  (memq (car pointers) identifiers)))
      '()))

(define (%id-pointers identifier pointers)
  (or (find-matching-item (cdr pointers)
	(lambda (ids)
	  (memq identifier ids)))
      '()))

(define-pointer-optimization
  '('BEGIN
     ('SET-PARSER-BUFFER-POINTER! EXPRESSION IDENTIFIER)
     EXPRESSION)
  (lambda (expression pointers)
    (let ((action (cadr expression))
	  (tail (caddr expression)))
      (let ((identifier (caddr action)))
	(if (current-pointer? identifier pointers)
	    (optimize-pointer-usage* tail pointers)
	    `(BEGIN
	       (SET-PARSER-BUFFER-POINTER!
		,(optimize-pointer-usage* (cadr action) pointers)
		,(canonicalize-pointer-ref identifier pointers))
	       ,(new-pointer tail identifier pointers)))))))

(define-pointer-optimization '(('LAMBDA (IDENTIFIER) EXPRESSION)
                               ('GET-PARSER-BUFFER-POINTER EXPRESSION))
  (lambda (expression pointers)
    (let ((identifier (car (cadr (car expression))))
          (operand (cadr expression))
          (body (caddr (car expression))))
      (let ((optimized-body (new-pointer body identifier pointers)))
        ;; Don't bind the identifier if it is internal and unused.  The
        ;; condition that the identifier be internal avoids a problem
        ;; of not counting references that are in the user's code,
        ;; which COUNT-REFERENCES can't descend into.
        (if (and (internal-identifier? identifier)
                 (zero? (car (count-references (list identifier)
                                               optimized-body))))
            optimized-body
            `((LAMBDA (,identifier)
                ;++ If we already have a pointer, the optimizer ought
                ;++ to introduce a (DECLARE (INTEGRATE ,identifier))
                ;++ here so that the scode optimizer will integrate the
                ;++ variable, even though the parser optimizer won't,
                ;++ but later passes of the parser optimizer can't
                ;++ handle LAMBDAs with more than one subform.
                ,optimized-body)
              ,(or (car pointers)
                   operand)))))))

(define-pointer-optimization '('PROTECT EXPRESSION)
  (lambda (expression pointers)
    pointers
    expression))

(define-pointer-optimization '('IF EXPRESSION EXPRESSION EXPRESSION)
  (lambda (expression pointers)
    `(IF ,(optimize-pointer-usage* (cadr expression) pointers)
	 ,(no-pointer (caddr expression) pointers)
	 ,(no-pointer (cadddr expression) pointers))))

(define-pointer-optimization '('IF EXPRESSION EXPRESSION)
  (lambda (expression pointers)
    `(IF ,(optimize-pointer-usage* (cadr expression) pointers)
	 ,(no-pointer (caddr expression) pointers))))

(define-pointer-optimization '('AND * EXPRESSION)
  (lambda (expression pointers)
    (if (pair? (cdr expression))
	`(AND ,(optimize-pointer-usage* (cadr expression) pointers)
	      ,@(map (lambda (expression) (no-pointer expression pointers))
		     (cddr expression)))
	expression)))

(define-pointer-optimization '('OR * EXPRESSION)
  (lambda (expression pointers)
    (if (pair? (cdr expression))
	`(OR ,(optimize-pointer-usage* (cadr expression) pointers)
	     ,@(map (lambda (expression) (no-pointer expression pointers))
		    (cddr expression)))
	expression)))

(define-default-pointer-optimization '('LAMBDA (* IDENTIFIER) EXPRESSION)
  (lambda (expression pointers)
    `(LAMBDA ,(cadr expression)
       ,(no-pointer (caddr expression) pointers))))

(define-default-pointer-optimization '(('LAMBDA (* IDENTIFIER) EXPRESSION)
				       . (* EXPRESSION))
  (lambda (expression pointers)
    (let ((operator (car expression))
	  (operands (cdr expression)))
      (let ((parameters (cadr operator)))
	`((LAMBDA ,parameters
	    ,(drop-pointer-refs (caddr operator) parameters pointers))
	  ,@(if (= (length operands) 1)
		(list (optimize-pointer-usage* (car operands) pointers))
		;; Here we don't know which operand goes first.
		(map (lambda (operand)
		       (no-pointer operand pointers))
		     operands)))))))

(define-default-pointer-optimization
  '('LET IDENTIFIER (* (IDENTIFIER EXPRESSION)) EXPRESSION)
  (lambda (expression pointers)
    `(LET ,(cadr expression)
       ,(map (lambda (binding)
	       `(,(car binding) ,(no-pointer (cadr binding) pointers)))
	     (caddr expression))
       ,(no-pointer (cadddr expression) pointers))))

;;;; Peephole optimizer

(define (peephole-optimize expression)
  (let loop ((entries peephole-optimizer-patterns))
    (cond ((pair? entries)
	   (if (and (syntax-match? (caar entries) expression)
		    (or (not (cadar entries))
			((cadar entries) expression)))
	       (let ((expression* ((cddar entries) expression)))
		 (if (equal? expression* expression)
		     expression
		     (begin
		       (trace-optimization 'PEEPHOLE expression expression*)
		       (peephole-optimize expression*))))
	       (loop (cdr entries))))
	  ((pair? expression)
	   (case (car expression)
	     ((LAMBDA)
	      `(LAMBDA ,(cadr expression)
		 ,@(map peephole-optimize (cddr expression))))
	     ((LET)
	      `(LET ,(cadr expression)
		 ,(map (lambda (binding)
			 `(,(car binding)
			   ,(peephole-optimize (cadr binding))))
		       (caddr expression))
		 ,(peephole-optimize (cadddr expression))))
	     (else
	      (map peephole-optimize expression))))
	  (else expression))))

(define (define-peephole-optimizer pattern predicate optimizer)
  (let ((entry (assoc pattern peephole-optimizer-patterns))
	(datum (cons predicate optimizer)))
    (if entry
	(set-cdr! entry datum)
	(begin
	  (set! peephole-optimizer-patterns
		(cons (cons pattern datum) peephole-optimizer-patterns))
	  unspecific))))

(define peephole-optimizer-patterns '())

(define (predicate-not-or expression)
  (not (and (pair? (cadr expression))
	    (eq? (caadr expression) 'OR))))

(define-peephole-optimizer '('IF EXPRESSION #T #F) predicate-not-or
  (lambda (expression)
    (cadr expression)))

(define-peephole-optimizer '('IF EXPRESSION #F #T) predicate-not-or
  (lambda (expression)
    `(NOT ,(cadr expression))))

(define-peephole-optimizer '('IF EXPRESSION EXPRESSION #F)
    (lambda (expression)
      (not (eq? (caddr expression) '#T)))
  (lambda (expression)
    `(AND ,(cadr expression) ,(caddr expression))))

(define-peephole-optimizer '('IF EXPRESSION #F EXPRESSION)
    (lambda (expression)
      (not (eq? (cadddr expression) '#T)))
  (lambda (expression)
    `(AND (NOT ,(cadr expression)) ,(cadddr expression))))

(define-peephole-optimizer '('IF EXPRESSION EXPRESSION EXPRESSION)
    (lambda (expression)
      (equal? (caddr expression) (cadddr expression)))
  (lambda (expression)
    `(BEGIN ,(cadr expression) ,(caddr expression))))

(define-peephole-optimizer '('IF EXPRESSION EXPRESSION 'UNSPECIFIC) #f
  (lambda (expression)
    `(IF ,(cadr expression) ,(caddr expression))))

(define-peephole-optimizer '('IF EXPRESSION EXPRESSION)
    (lambda (expression)
      (and (eq? (caddr expression) 'UNSPECIFIC)
	   (predicate-not-or expression)))
  (lambda (expression)
    (cadr expression)))

(define-peephole-optimizer '('IF EXPRESSION
				 ('IF EXPRESSION EXPRESSION EXPRESSION)
				 EXPRESSION)
    (lambda (expression)
      (equal? (cadddr (caddr expression))
	      (cadddr expression)))
  (lambda (expression)
    `(IF (AND ,(cadr expression) ,(cadr (caddr expression)))
	 ,(caddr (caddr expression))
	 ,(cadddr expression))))

(define-peephole-optimizer '('IF EXPRESSION
				 EXPRESSION
				 ('IF EXPRESSION EXPRESSION EXPRESSION))
    (lambda (expression)
      (equal? (caddr (cadddr expression))
	      (caddr expression)))
  (lambda (expression)
    `(IF (OR ,(cadr expression) ,(cadr (cadddr expression)))
	 ,(caddr expression)
	 ,(cadddr (cadddr expression)))))

(define-peephole-optimizer '('IF EXPRESSION
				 ('BEGIN . (+ EXPRESSION))
				 EXPRESSION)
    (lambda (expression)
      (let ((expression* (car (last-pair (caddr expression)))))
	(and (syntax-match? '('IF EXPRESSION EXPRESSION EXPRESSION)
			    expression*)
	     (equal? (cadddr expression*)
		     (cadddr expression)))))
  (lambda (expression)
    (let ((expression* (car (last-pair (caddr expression)))))
      `(IF (AND ,(cadr expression)
		(BEGIN ,@(except-last-pair (cdr (caddr expression)))
		       ,(cadr expression*)))
	   ,(caddr expression*)
	   ,(cadddr expression)))))

(define-peephole-optimizer '('IF EXPRESSION
				 EXPRESSION
				 ('BEGIN . (+ EXPRESSION)))
    (lambda (expression)
      (let ((expression* (car (last-pair (cadddr expression)))))
	(and (syntax-match? '('IF EXPRESSION EXPRESSION EXPRESSION)
			    expression*)
	     (equal? (caddr expression*)
		     (caddr expression)))))
  (lambda (expression)
    (let ((expression* (car (last-pair (cadddr expression)))))
      `(IF (OR ,(cadr expression)
	       (BEGIN ,@(except-last-pair (cdr (cadddr expression)))
		      ,(cadr expression*)))
	   ,(caddr expression)
	   ,(cadddr expression*)))))

(define-peephole-optimizer '('IF EXPRESSION
				 ('OR . (+ EXPRESSION))
				 EXPRESSION)
    (lambda (expression)
      (equal? (car (last-pair (caddr expression)))
	      (cadddr expression)))
  (lambda (expression)
    `(OR (AND ,(cadr expression)
	      (OR ,@(except-last-pair (cdr (caddr expression)))))
	 ,(cadddr expression))))

(define-peephole-optimizer '(('LAMBDA (IDENTIFIER)
			       ('IF IDENTIFIER
				    IDENTIFIER
				    EXPRESSION))
			     EXPRESSION)
    (lambda (expression)
      (let ((operator (car expression)))
	(let ((identifier (car (cadr operator)))
	      (body (caddr operator)))
	  (and (eq? identifier (cadr body))
	       (eq? identifier (caddr body))))))
  (lambda (expression)
    `(OR ,(cadr expression)
	 ,(cadddr (caddr (car expression))))))

(define-peephole-optimizer '(('LAMBDA (IDENTIFIER)
			       ('AND IDENTIFIER
				     IDENTIFIER))
			     EXPRESSION)
    (lambda (expression)
      (let ((operator (car expression)))
	(let ((identifier (car (cadr operator)))
	      (body (caddr operator)))
	  (and (eq? identifier (cadr body))
	       (eq? identifier (caddr body))))))
  (lambda (expression)
    (cadr expression)))

(define-peephole-optimizer '('LAMBDA (* IDENTIFIER) EXPRESSION) #f
  (lambda (expression)
    `(LAMBDA ,(cadr expression) ,(peephole-optimize (caddr expression)))))

(define-peephole-optimizer '('VECTOR-MAP EXPRESSION ('VECTOR EXPRESSION)) #f
  (lambda (expression)
    `(VECTOR (,(cadr expression) ,(cadr (caddr expression))))))

(define-peephole-optimizer '('VECTOR-MAP EXPRESSION ('VECTOR . (* EXPRESSION)))
    (lambda (expression)
      (or (symbol? (cadr expression))
	  (and (pair? (cadr expression))
	       (eq? 'PROTECT (car (cadr expression)))
	       (symbol? (cadr (cadr expression))))))
  (lambda (expression)
    `(VECTOR
      ,@(map (lambda (subexpression)
	       `(,(cadr expression) ,subexpression))
	     (cdr (caddr expression))))))

(define-peephole-optimizer '('NOT EXPRESSION) #f
  (lambda (expression)
    `(NOT ,(peephole-optimize (cadr expression)))))

(define-peephole-optimizer '('VECTOR-APPEND . (* EXPRESSION)) #f
  (lambda (expression)
    (optimize-group-expression expression '(VECTOR))))

(define-peephole-optimizer '('AND . (* EXPRESSION)) #f
  (lambda (expression)
    (optimize-group-expression expression '#T)))

(define-peephole-optimizer '('OR . (* EXPRESSION)) #f
  (lambda (expression)
    (optimize-group-expression expression '#F)))

(define-peephole-optimizer '('BEGIN . (+ EXPRESSION)) #f
  (lambda (expression)
    (optimize-group-expression expression 'UNSPECIFIC)))

(define (optimize-group-expression expression identity)
  (optimize-group-expression-1 (map peephole-optimize
				    (flatten-subexpressions expression))
			       (car expression)
			       identity))

(define (optimize-group-expression-1 expressions keyword identity)
  (let ((expressions (delete identity expressions)))
    (if (pair? expressions)
	(if (pair? (cdr expressions))
	    `(,keyword ,@expressions)
	    (car expressions))
	identity)))

(define (flatten-subexpressions expression)
  (flatten-expressions (cdr expression) (car expression)))

(define (flatten-expressions expressions keyword)
  (let loop ((expressions expressions))
    (if (pair? expressions)
	(if (and (pair? (car expressions))
		 (eq? (caar expressions) keyword))
	    (loop (append (cdar expressions) (cdr expressions)))
	    (cons (car expressions) (loop (cdr expressions))))
	'())))

(define (trace-optimization keyword before after . extra)
  (if (eq? debug:trace-optimization? 'ALL)
      (let ((port (trace-output-port)))
	(fresh-line port)
	(pp before port)
	(for-each (lambda (x) (pp x port)) extra)
	(write-string "==" port)
	(write keyword port)
	(write-string "==>" port)
	(newline port)
	(pp after port)
	(newline port))))

(define (trace-optimizer-tl tag expression)
  (if debug:trace-optimization?
      (let ((port (trace-output-port)))
	(fresh-line port)
	(write-string ";" port)
	(write-string tag port)
	(write-string ":" port)
	(newline)
	(pp expression port)
	(newline port)))
  expression)

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'define-peephole-optimizer 2)
;;; End:
