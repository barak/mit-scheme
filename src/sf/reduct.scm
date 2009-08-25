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

;;;; SCode Optimizer: User defined reductions
;;; package: (scode-optimizer expansion)

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution)
	 (integrate-external "object"))

;;;; Reductions and replacements

#|

REPLACE-OPERATOR declaration

Generates SF-time expanders (transformers for sf) for operations
that act differently depending on the number of arguments.

(replace-operator (<name> (<nargs1> <value1>) (<nargs2> <value2>) ...))

<name> is a symbol
<nargs1> is a non-negative integer or one of the symbols ANY, ELSE, OTHERWISE.
<valueN> is a simple expression:
  <symbol>					; means a variable
  (QUOTE <constant>) = '<constant>		; means a constant
  (PRIMITIVE <primitive name> { <arity> })	; means a primitive
  (GLOBAL <variable>)				; means a global variable

replaces non-shadowed calls to <name> with <nargsN> arguments
with a call to <valueN> with the same arguments.

Examples:

(replace-operator (map (2 map-2) (3 map-3)))

replaces (map f l) with (map-2 f l)
and (map (lambda (x) (car x)) frob l)
with (map-3 (lambda (x) (car x)) frob l)
|#

#|
REDUCE-OPERATOR declaration

Generates SF-time expanders (transformers for sf) for operations
obtained by REDUCEing a binary operator.

(reduce-operator (<name> <binop>
		  { (group <ordering>)
		    (null-value <value> <null-option>)
		    (singleton <unop>)
		    (wrapper <wrap> {<n>})
		    (maximum <m>)
		    }))

<name> is a symbol

<n> and <m> are non-negative integers.

<binop>, <value>, <unop>, and <wrap> are simple expressions as above.

<null-option> is a member of {ALWAYS, ANY, ONE, SINGLE, NONE, EMPTY}

<ordering> is a member of {LEFT, RIGHT, ASSOCIATIVE}

1) <name> is the name of the generic operation to be reduced.

2) <binop> is a binary operation which performs the reduction.

3) The group option specifies whether <binop> associates to the right
or to the left to produce <name>.

4) The null-value option specifies a value to use in the following
cases (each case is included in the following):

NONE, EMPTY: When no arguments are supplied to <name>, <value> is
returned.

ONE, SINGLE: When a single argument is provided to <name>, <value>
becomes the second argument to <binop>.

ANY, ALWAYS: <binop> is used on the "odd" argument, and <value>
provides the remaining argument to <binop>.

In the above options, when <value> is supplied to <binop>, it is
supplied on the left if grouping to the left, otherwise it is supplied
on the right.

5) The singleton option specifies a function, <unop>, to be invoked on
the single argument left.  This option supersedes the null-value option,
which can only take the value NONE.

6) The wrapper option specifies a function, <wrap>, to be invoked on the
result of the outermost call to <binop> after the expansion.
If <n> is provided it must be a non-negative integer indicating a number
of arguments that are transferred verbatim from the original call to
the wrapper.  They are passed to the left of the reduction.

7) The maximum option specifies that calls with more than <m> arguments
should not be reduced.

Examples:

(declare (reduce-operator
	  (CONS* (PRIMITIVE cons))
	  (LIST (PRIMITIVE cons)
		(NULL-VALUE '() ANY))
	  (+ %+ (NULL-VALUE 0 NONE) (GROUP RIGHT))
	  (- %- (NULL-VALUE 0 SINGLE) (GROUP LEFT))
	  (VECTOR (PRIMITIVE cons)
		  (GROUP RIGHT)
		  (NULL-VALUE '() ALWAYS)
		  (WRAPPER list->vector))
	  (APPLY (PRIMITIVE cons)
		 (GROUP RIGHT)
		 (WRAPPER (GLOBAL apply) 1))))

|#

;;;; Syntax stubs

;; Only the procedures under this heading need to be replaced to make
;; the code below work on s-expressions, scode, or other structure.
;; The only other assumption made below is that an expanders'
;; parameter list is
;;	(expr operands if-expanded if-not-expanded block)
;; Where
;;  - expr is the current expression
;;  - operands are the arguments to the "procedure" being reduced.
;;  - if-expanded is a procedure of 1 argument (the expanded expression)
;;  which must be invoked if the expansion (reduction) was succesful.
;;  - if-not-expanded is a procedure of no arguments to be invoked on
;;  failure.
;;  - block is the compile (syntax) time representation of the environment.

(define (lookup name block)
  (reference/make
   false
   block
   (or (block/lookup-name block name false)
       (block/lookup-name (integrate/get-top-level-block) name true))))

(define-integrable (handle-variable object core)
  (if (variable? object)
      (let ((name (variable/name object)))
	(core (lambda (block)
		(declare (integrate block))
		(lookup name block))))
      (core (lambda (block)
	      block			; ignore
	      object))))

(define (->expression procedure exp block)
  (define (fail)
    (error "Bad primitive expression" procedure exp))

  (define-integrable (constant value)
    (constant/make false value))

  (cond ((symbol? exp)
	 (variable/make block exp '()))
	((not (pair? exp))
	 (constant exp))
	((eq? (car exp) 'PRIMITIVE)
	 (cond ((or (null? (cdr exp)) (not (list? exp)))
		(fail))
	       ((null? (cddr exp))
		(constant (make-primitive-procedure (cadr exp))))
	       ((null? (cdddr exp))
		(constant
		 (make-primitive-procedure (cadr exp) (caddr exp))))
	       (else
		(fail))))
	((eq? (car exp) 'QUOTE)
	 (if (or (not (pair? (cdr exp)))
		 (not (null? (cddr exp))))
	     (fail))
	 (constant (cadr exp)))
	((eq? (car exp) 'GLOBAL)
	 (if (or (not (pair? (cdr exp)))
		 (not (null? (cddr exp)))
		 (not (symbol? (cadr exp))))
	     (fail))
	 (global-ref/make (cadr exp)))
	(else
	 (fail))))

;; any-shadowed? prevents reductions in any environment where any of
;; the names introduced by the reduction has been shadowed.  The
;; search stops at the environment (block) where the declaration
;; appeared, since it is assumed that the binding is shared there.

(define (any-shadowed? var-list source target)
  (let loop ((l var-list))
    (and (not (null? l))
	 (or (block/limited-lookup target (variable/name (car l)) source)
	     (loop (cdr l))))))

(define (filter-vars expr-list)
  (let loop ((l expr-list)
	     (done '()))
    (cond ((null? l)
	   done)
	  ((variable? (car l))
	   (loop (cdr l) (cons (car l) done)))
	  (else
	   (loop (cdr l) done)))))

(define (combine-1 block unop x)
  (combination/make false block unop (list x)))

(define (combine-2 block binop x y)
  (combination/make false block binop (list x y)))

;;;; Building blocks

;; The arguments to the groupers below come from this set

(define (identity-combiner block value combiner)
  block combiner			; ignored
  value)

(define (->singleton-combiner null)
  (handle-variable null
   (lambda (null)
     (declare (integrate null))
     (lambda (block value combiner)
       (combiner block value (null block))))))
  
(define (->mapper-combiner mapper)
  (handle-variable mapper
   (lambda (mapper)
     (declare (integrate mapper))
     (lambda (block value combiner)
       combiner				; ignored
       (combine-1 block (mapper block) value)))))

(define (->wrapper mapper)
  (handle-variable mapper
   (lambda (mapper)
     (declare (integrate mapper))
     (lambda (block not-reduced reduced)
       (combination/make false
			 block
			 (mapper block)
			 (append not-reduced
				 (list reduced)))))))

(define (identity-wrapper block not-reduced reduced)
  block not-reduced			; ignored
  reduced)

(define (->error-thunk name)
  (lambda (block)
    block				; ignored
    (error "REDUCER: No supplied values" name)))

(define (->value-thunk val)
  (handle-variable val
   (lambda (val)
     (declare (integrate val))
     (lambda (block)
       (val block)))))

(define (invert binop)
  (lambda (block x y)
    (binop block y x)))

;;;; Groupers

(define (make-grouper spare-args min-args max-args
		      map1 map2
		      binop source-block exprs
		      wrap last single none)
  (let ((expr (->expression 'REDUCE-OPERATOR binop source-block)))
    (let ((vars (filter-vars (cons expr exprs)))
	  (binop (map1
		  (handle-variable
		   expr
		   (lambda (expr)
		     (declare (integrate expr))
		     (lambda (block x y)
		       (combine-2 block (expr block) x y)))))))

      (lambda (expr operands if-expanded if-not-expanded block)
	(define (group l)
	  (if (null? (cdr l))
	      (last block (car l) binop)
	      (binop block
		     (car l)
		     (group (cdr l)))))

	(if (or (any-shadowed? vars source-block block)
		(let ((l (length operands)))
		  (or (< l min-args)
		      (and max-args (> l max-args)))))
	    (if-not-expanded)
	    (if-expanded
	     (reassign
	      expr
	      (let ((l1 (list-head operands spare-args))
		    (l2 (map2 (list-tail operands spare-args))))
		(cond ((null? l2)
		       (wrap block
			     l1
			     (none block)))
		      ((null? (cdr l2))
		       (wrap block
			     l1
			     (single block
				     (car l2)
				     (lambda (block x y)
				       (binop block x y)))))
		      (else
		       (wrap block
			     l1
			     (binop block (car l2)
				    (group (cdr l2))))))))))))))

(define (group-right spare-args min-args max-args
		     binop source-block exprs
		     wrap last single none)
  (make-grouper spare-args min-args max-args
		identity-procedure identity-procedure
		binop source-block exprs
		wrap last single none))

(define (group-left spare-args min-args max-args
		    binop source-block exprs
		    wrap last single none)
  (make-grouper spare-args min-args max-args
		invert reverse
		binop source-block exprs
		wrap last single none))

;;;; Keyword and convenience utilities

(define-integrable (with-arguments-from list procedure)
  (apply procedure list))

;;; Keyword decoder

(define (decode-options keywords options receiver)
  (define (collect keys)
    (if (null? keys)
	'()
	(cons
	 (let ((place (assq (car keys) options)))
	   (and place
		(cdr place)))
	 (collect (cdr keys)))))

  (define (check opts)
    ;; options is guaranteed to be a list.  No need to check for pairness.
    (cond ((null? opts)
	   'DONE)
	  ((or (not (pair? (car opts)))
	       (not (list? (car opts))))
	   (error "DECODE-OPTIONS: Bad option" (car opts)))
	  ((not (memq (caar opts) keywords))
	   (error "DECODE-OPTIONS: Unknown option" (car opts)))
	  (else
	   (check (cdr opts)))))

  (check options)
  (apply receiver (collect keywords)))

;;;; Error and indentation utilities

(define (fail name value)
  (error "REDUCE-OPERATOR: Bad option" `(,name ,@value)))

(define (incompatible name1 val1 name2 val2)
  (error "REDUCE-OPERATOR: Incompatible options"
	 `(,name1 ,val1) `(,name2 ,val2)))

(define (with-wrapper wrapper block receiver)
  (cond ((not wrapper)
	 (receiver 0 identity-wrapper '()))
	((null? (cdr wrapper))
	 (let ((expr (->expression 'REDUCE-OPERATOR (car wrapper) block)))
	   (receiver 0 (->wrapper expr) (list expr))))
	((and (null? (cddr wrapper))
	      (exact-nonnegative-integer? (cadr wrapper)))
	 (let ((expr (->expression 'REDUCE-OPERATOR (car wrapper) block)))
	   (receiver (cadr wrapper) (->wrapper expr) (list expr))))
	(else
	 (fail 'WRAPPER wrapper))))

(define (with-singleton singleton block receiver)
  (cond ((not singleton)
	 (receiver identity-combiner '()))
	((null? (cdr singleton))
	 (let ((expr (->expression 'REDUCE-OPERATOR (car singleton) block)))
	   (receiver (->mapper-combiner expr)
		     (list expr))))
	(else
	 (fail 'SINGLETON singleton))))

;;;; Reduction top level

(define (reducer/make rule block)
  (with-arguments-from rule
    (lambda (name binop . options)
      (decode-options '(NULL-VALUE GROUP SINGLETON WRAPPER MAXIMUM)
	  options
	(lambda (null-value group singleton wrapper maximum)

	  (define (make-reducer-internal grouper)
	    (with-wrapper wrapper block

	      (lambda (spare-args wrap wrap-expr)
		(with-singleton singleton block

		  (lambda (single-combiner single-expr)

		    (define (invoke min-args null-expr last single none)
		      (let ((max-args
			     (and maximum
				  (if (or (not (null? (cdr maximum)))
					  (not (exact-nonnegative-integer?
						(car maximum))))
				      (fail 'MAXIMUM maximum)
				      (car maximum)))))
			(grouper spare-args min-args max-args
				 binop block
				 (append null-expr wrap-expr single-expr)
				 wrap last single none)))

		    (cond ((not null-value)
			   (invoke (+ spare-args (if singleton 1 2))
				   '() single-combiner
				   single-combiner (->error-thunk name)))
			  ((not (= (length null-value) 2))
			   (fail 'NULL-VALUE null-value))
			  (else
			   (let* ((val (->expression 'REDUCE-OPERATOR
						     (car null-value)
						     block))
				  (combiner (->singleton-combiner val))
				  (null (->value-thunk val)))
			     (case (cadr null-value)
			       ((ANY ALWAYS)
				(if singleton
				    (incompatible 'SINGLETON singleton
						  'NULL-VALUE null-value))
				(invoke spare-args (list val) combiner
					combiner null))
			       ((ONE SINGLE)
				(if singleton
				    (incompatible 'SINGLETON singleton
						  'NULL-VALUE null-value))
				(invoke (1+ spare-args) (list val)
					identity-combiner
					combiner null))
			       ((NONE EMPTY)
				(invoke spare-args
					(list val) single-combiner
					single-combiner null))
			       (else
				(fail 'NULL-VALUE null-value)))))))))))

	  (cond ((not group)
		 (make-reducer-internal group-right))
		((not (null? (cdr group)))
		 (fail 'GROUP group))
		(else
		 (case (car group)
		   ((RIGHT ASSOCIATIVE)
		    (make-reducer-internal group-right))
		   ((LEFT)
		    (make-reducer-internal group-left))
		   (else
		    (fail 'GROUP group))))))))))

;;;; Replacement top level

(define (replacement/make replacement decl-block)
  (call-with-values
      (lambda ()
	(parse-replacement (car replacement)
			   (cdr replacement)
			   decl-block))
    (lambda (table default)
      (lambda (expr operands if-expanded if-not-expanded block)
	(let* ((len (length operands))
	       (candidate (or (and (< len (vector-length table))
				   (vector-ref table len))
			      default)))
	  (if (or (not (pair? candidate))
		  (and (car candidate)
		       (block/limited-lookup block
					     (car candidate)
					     decl-block)))
	      (if-not-expanded)
	      (if-expanded
	       (combination/make (and expr (object/scode expr))
				 block
				 (let ((frob (cdr candidate)))
				   (if (variable? frob)
				       (lookup (variable/name frob) block)
				       frob))
				 operands))))))))

(define (parse-replacement name ocases block)
  (define (collect len cases default)
    (let ((output (make-vector len false)))
      (let loop ((cases cases))
	(if (null? cases)
	    (values output default)
	    (let* ((a-case (car cases))
		   (index (car a-case)))
	      (if (vector-ref output index)
		  (error "REPLACE-OPERATOR: Duplicate arity" name ocases))
	      (vector-set! output index (cdr a-case))
	      (loop (cdr cases)))))))

  (define (fail a-case)
    (error "REPLACE-OPERATOR: Bad replacement" name a-case))

  (define (expr->case expr)
    (cons (and (symbol? expr) expr)
	  (->expression 'REPLACE-OPERATOR
			expr
			block)))

  (let parse ((cases ocases)
	      (parsed '())
	      (len 0)
	      (default false))
    (if (null? cases)
	(collect len parsed default)
	(let ((a-case (car cases)))
	  (cond ((or (not (pair? a-case))
		     (not (pair? (cdr a-case)))
		     (not (null? (cddr a-case))))
		 (fail a-case))
		((exact-nonnegative-integer? (car a-case))
		 (let ((len* (car a-case))
		       (expr (cadr a-case)))
		   (parse (cdr cases)
			  (cons (cons len* (expr->case expr))
				parsed)
			  (max (1+ len*) len)
			  default)))
		((memq (car a-case) '(ANY ELSE OTHERWISE))
		 (if default
		     (error "REPLACE-OPERATOR: Duplicate default" ocases))
		 (parse (cdr cases)
			parsed
			len
			(expr->case (cadr a-case))))
		(else
		 (fail a-case)))))))

;;; Local Variables:
;;; eval: (put 'decode-options 'scheme-indent-hook 2)
;;; eval: (put 'with-arguments-from 'scheme-indent-hook 1)
;;; eval: (put 'with-wrapper 'scheme-indent-hook 2)
;;; eval: (put 'with-singleton 'scheme-indent-hook 2)
;;; End:
