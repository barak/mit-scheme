#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/reduct.scm,v 4.1 1988/06/13 12:30:09 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: User defined reductions

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution)
	 (integrate-external "object"))

;;;; Reductions

#|

REDUCE-OPERATOR declaration

Generates syntax time expanders (transformers for sf) for operations
obtained by REDUCEing a binary operator.

(reduce-operator (<name> <binop>
		  { (group <ordering>)
		    (null-value <value> <null-option>)
		    (singleton <unop>)
		    (wrapper <wrap>)
		    }))

<name> is a symbol

<binop>, <value>, <unop>, and <wrap> are simple expressions
(currently not checked):
  '<constant>
  <variable>
  (primitive <primitive name> { <arity> })

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

Examples:

(declare (reduce-operator
	  (CONS* (primitive cons))
	  (LIST (primitive cons) (NULL-VALUE '() ANY))
	  (+ %+ (NULL-VALUE 0 NONE) (GROUP RIGHT))
	  (- %- (NULL-VALUE 0 SINGLE) (GROUP LEFT))))

|#

;;;; Syntax stubs

;; Only the procedures under this heading need to be replaced to make
;; the code below work on s-expressions, scode, or other structure.
;; The only other assumption made below is that an expanders'
;; parameter list is
;;	(operands if-expanded if-not-expanded block)
;; Where
;;  - operands are the arguments to the "procedure" being reduced.
;;  - if-expanded is a procedure of 1 argument (the expanded expression)
;;  which must be invoked if the expansion (reduction) was succesful.
;;  - if-not-expanded is a procedure of no arguments to be invoked on
;;  failure.
;;  - block is the compile (syntax) time representation of the environment.

(define (lookup name block)
  (reference/make
   block
   (or (block/lookup-name block name false)
       (block/lookup-name (integrate/get-top-level-block) name true))))

(declare (integrate-operator handle-variable))

(define (handle-variable object core)
  (declare (integrate object core))
  (if (variable? object)
      (let ((name (variable/name object)))
	(core (lambda (block)
		(declare (integrate block))
		(lookup name block))))
      (core (lambda (block)
	      block			; ignore
	      object))))

(define (->expression exp block)
  (cond ((symbol? exp)
	 (variable/make block exp '()))
	((not (pair? exp))
	 (constant/make exp))
	((eq? (car exp) 'PRIMITIVE)
	 (cond ((or (null? (cdr exp)) (not (list? exp)))
		(error "MAKE-REDUCER: Bad PRIMITIVE expression" exp))
	       ((null? (cddr exp))
		(constant/make (make-primitive-procedure (cadr exp))))
	       ((null? (cdddr exp))
		(constant/make
		 (make-primitive-procedure (cadr exp) (caddr exp))))
	       (else
		(error "MAKE-REDUCER: Bad PRIMITIVE expression" exp))))
	((eq? (car exp) 'QUOTE)
	 (cadr exp))
	(else
	 (error "MAKE-REDUCER: Bad expression" exp))))

;; any-shadowed? prevents reductions in any environment where any of
;; the names introduced by the reduction has been shadowed.  The
;; search stops at the environment (block) where the declaration
;; appeared, since it is assumed that the binding is shared there.

(define (any-shadowed? var-list source target)
  (let loop ((l var-list))
    (and (not (null? l))
	 (or (shadowed? (variable/name (car l)) source target)
	     (loop (cdr l))))))

(define (shadowed? name source target)
  (let search ((block target))
    (and (not (eq? block source))
	 (or (variable/assoc name (block/bound-variables block))
	     (let ((parent (block/parent block)))
	       (and (not (null? parent))
		    (search parent)))))))

(define (filter-vars expr-list)
  (let loop ((l expr-list)
	     (done '()))
    (cond ((null? l)
	   done)
	  ((variable? (car l))
	   (loop (cdr l) (cons (car l) done)))
	  (else
	   (loop (cdr l) done)))))

(define (combine-1 unop x)
  (combination/make unop (list x)))

(define (combine-2 binop x y)
  (combination/make binop (list x y)))

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
       (combiner value (null block))))))
  
(define (->mapper-combiner mapper)
  (handle-variable mapper
   (lambda (mapper)
     (declare (integrate mapper))
     (lambda (block value combiner)
       combiner				; ignored
       (combine-1 (mapper block) value)))))

(define (->wrapper mapper)
  (handle-variable mapper
   (lambda (mapper)
     (declare (integrate mapper))
     (lambda (block reduced)
       (combine-1 (mapper block) reduced)))))

(define (identity-wrapper block reduced)
  block					; ignored
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

(define (make-grouper map1 map2 binop source-block exprs
		      wrap last single none)
  (let ((expr (->expression binop source-block)))
    (let ((vars (filter-vars (cons expr exprs)))
	  (binop (map1
		  (handle-variable
		   expr
		   (lambda (expr)
		     (declare (integrate expr))
		     (lambda (block x y)
		       (combine-2 (expr block) x y)))))))

      (lambda (operands if-expanded if-not-expanded block)
	(define (group l)
	  (if (null? (cdr l))
	      (last block (car l) binop)
	      (binop block
		     (car l)
		     (group (cdr l)))))

	(if (any-shadowed? vars source-block block)
	    (if-not-expanded)
	    (if-expanded
	     (let ((l (map2 operands)))
	       (cond ((null? l)
		      (none block))
		     ((null? (cdr l))
		      (wrap block (single (car l) binop)))
		     (else
		      (wrap block (binop block (car l)
					 (group (cdr l)))))))))))))

(define (group-right binop source-block exprs wrap last single none)
  (make-grouper identity-procedure identity-procedure binop
		source-block exprs wrap
		last single none))

(define (group-left binop source-block exprs wrap last single none)
  (make-grouper invert reverse binop
		source-block exprs wrap
		last single none))

;;;; Keyword and convenience utilities

(declare (integrate-operator with-arguments-from))

(define (with-arguments-from list procedure)
  (declare (integrate list procedure))
  (apply procedure list))

;;; Keyword decoder

(define (decode-options keywords options receiver)
  (define (collect keys)
    (if (null? keys)
	'()
	(cons
	 (let ((place (assq (car keys) options)))
	   (if (null? place)
	       '()
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
  (error "MAKE-REDUCER: Bad option" `(,name ,@value)))

(define (incompatible name1 val1 name2 val2)
  (error "MAKE-REDUCER: Incompatible options"
	 `(,name1 ,val1) `(,name2 ,val2)))

(define (with-wrapper wrapper block receiver)
  (cond ((not wrapper)
	 (receiver identity-wrapper '()))
	((null? (cdr wrapper))
	 (let ((expr (->expression (car wrapper) block)))
	   (receiver (->wrapper expr) (list expr))))
	(else
	 (fail 'WRAPPER wrapper))))

(define (with-singleton singleton block receiver)
  (cond ((not singleton)
	 (receiver identity-combiner '()))
	((null? (cdr singleton))
	 (let ((expr (->expression (car singleton) block)))
	   (receiver (->mapper-combiner expr)
		     (list expr))))
	(else
	 (fail 'SINGLETON singleton))))

;;;; Top level

(define (reducer/make rule block)
  (with-arguments-from rule
    (lambda (name binop . options)
      (decode-options
	  '(NULL-VALUE GROUP SINGLETON WRAPPER)
	  options
	(lambda (null-value group singleton wrapper)

	  (define (make-reducer-internal grouper)
	    (with-wrapper wrapper block

	      (lambda (wrap wrap-expr)
		(with-singleton singleton block

		  (lambda (single-combiner single-expr)

		    (define (invoke null-expr last single none)
		      (grouper binop block
			       (append null-expr wrap-expr single-expr)
			       wrap last single none))

		    (cond ((not null-value)
			   (invoke '() single-combiner
				   single-combiner (->error-thunk name)))
			  ((not (= (length null-value) 2))
			   (fail 'NULL-VALUE null-value))
			  (else
			   (let* ((val (->expression (car null-value) block))
				  (combiner (->singleton-combiner val))
				  (null (->value-thunk val)))
			     (case (cadr null-value)
			       ((ANY ALWAYS)
				(if singleton
				    (incompatible 'SINGLETON singleton
						  'NULL-VALUE null-value))
				(invoke (list val) combiner
					combiner null))
			       ((ONE SINGLE)
				(if singleton
				    (incompatible 'SINGLETON singleton
						  'NULL-VALUE null-value))
				(invoke (list val) identity-combiner
					combiner null))
			       ((NONE EMPTY)
				(invoke (list val) single-combiner
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

;;; Local Variables:
;;; eval: (put 'decode-options 'scheme-indent-hook 2)
;;; eval: (put 'with-arguments-from 'scheme-indent-hook 1)
;;; eval: (put 'with-wrapper 'scheme-indent-hook 2)
;;; eval: (put 'with-singleton 'scheme-indent-hook 2)
;;; End:
