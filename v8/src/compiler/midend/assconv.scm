#| -*-Scheme-*-

$Id: assconv.scm,v 1.1 1994/11/19 02:04:29 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

;;;; Assignment Converter
;;; package: (compiler midend)

(declare (usual-integrations))

(define (assconv/top-level program)
  (assconv/expr '() program))

(define-macro (define-assignment-converter keyword bindings . body)
  (let ((proc-name (symbol-append 'ASSCONV/ keyword)))
    (call-with-values
     (lambda () (%matchup (cdr bindings) '(handler env) '(cdr form)))
     (lambda (names code)
       `(define ,proc-name
	  (let ((handler (lambda ,(cons (car bindings) names) ,@body)))
	    (named-lambda (,proc-name env form)
	      (assconv/remember ,code form))))))))

;;;; Variable manipulation forms

(define-assignment-converter LAMBDA (env lambda-list body)
  (call-with-values
   (lambda ()
     (assconv/binding-body env
			   (lambda-list->names lambda-list)
			   body))
   (lambda (shadowed body*)
     `(LAMBDA ,(if (null? shadowed)
		   lambda-list
		   (lmap (lambda (name)
			   (if (memq name shadowed)
			       (assconv/new-name 'IGNORED)
			       name))
			 lambda-list))
	,body*))))

(define-assignment-converter LET (env bindings body)
  (call-with-values
   (lambda ()
     (assconv/binding-body env (lmap car bindings) body))
   (lambda (shadowed body*)
     `(LET ,(lmap (lambda (binding)
		    (list (car binding)
			  (assconv/expr env (cadr binding))))
		  (if (null? shadowed)
		      bindings
		      (list-transform-negative bindings
			(lambda (binding)
			  (memq (car binding) shadowed)))))
	,body*))))

(define-assignment-converter LOOKUP (env name)
  (let ((binding (assconv/env-lookup env name)))
    (if (not binding)
	(free-var-error name)
	(let ((result `(LOOKUP ,name)))
	  (set-assconv/binding/references!
	   binding
	   (cons result (assconv/binding/references binding)))
	  result))))

(define-assignment-converter SET! (env name value)
  (let ((binding (assconv/env-lookup env name)))
    (if (not binding)
	(free-var-error name)
	(let ((result `(SET! ,name ,(assconv/expr env value))))
	  (set-assconv/binding/assignments!
	   binding
	   (cons result (assconv/binding/assignments binding)))
	  result))))

;;;; Trivial forms

(define-assignment-converter QUOTE (env object)
  env					; ignored
  `(QUOTE ,object))

(define-assignment-converter DECLARE (env #!rest anything)
  env					; ignored
  `(DECLARE ,@anything))

(define-assignment-converter CALL (env rator cont #!rest rands)
  `(CALL ,(assconv/expr env rator)
	 ,(assconv/expr env cont)
	 ,@(assconv/expr* env rands)))

(define-assignment-converter BEGIN (env #!rest actions)
  `(BEGIN ,@(assconv/expr* env actions)))

(define-assignment-converter IF (env pred conseq alt)
  `(IF ,(assconv/expr env pred)
       ,(assconv/expr env conseq)
       ,(assconv/expr env alt)))

;;; Dispatcher

(define (assconv/expr env expr)
  (if (not (pair? expr))
      (illegal expr))
  (case (car expr)
    ((QUOTE)
     (assconv/quote env expr))
    ((LOOKUP)
     (assconv/lookup env expr))
    ((LAMBDA)
     (assconv/lambda env expr))
    ((LET)
     (assconv/let env expr))
    ((DECLARE)
     (assconv/declare env expr))
    ((CALL)
     (assconv/call env expr))
    ((BEGIN)
     (assconv/begin env expr))
    ((IF)
     (assconv/if env expr))
    ((SET!)
     (assconv/set! env expr))
    ((LETREC)
     (not-yet-legal expr))
    ((UNASSIGNED? OR DELAY
      ACCESS DEFINE IN-PACKAGE THE-ENVIRONMENT)
     (no-longer-legal expr))
    (else
     (illegal expr))))

(define (assconv/expr* env exprs)
  (lmap (lambda (expr)
	  (assconv/expr env expr))
	exprs))

(define (assconv/remember new old)
  (code-rewrite/remember new old)
  new)

(define (assconv/new-name prefix)
  (new-variable prefix))

(define (assconv/new-cell-name prefix)
  (new-variable (string-append (symbol-name prefix) "-cell")))

;;;; Utilities for variable manipulation forms

(define-structure (assconv/binding
		   (conc-name assconv/binding/)
		   (constructor assconv/binding/make (name)))
  (name false read-only true)
  (cell-name false read-only false)
  (references '() read-only false)
  (assignments '() read-only false))

(define (assconv/binding-body env names body)
  ;; (values shadowed-names body*)
  (let* ((frame (lmap assconv/binding/make names))
	 (env* (cons frame env))
	 (body* (assconv/expr env* body))
	 (assigned
	  (list-transform-positive frame
	    (lambda (binding)
	      (not (null? (assconv/binding/assignments binding))))))
	 (ssa-candidates
	  (list-transform-positive assigned
	    (lambda (binding)
	      (let ((assignments (assconv/binding/assignments binding)))
		(and (null? (cdr assignments))
		     (assconv/single-assignment/trivial?
		      (car assignments))))))))
    (if (null? ssa-candidates)
	(assconv/bind-cells '() assigned body*)
	(call-with-values
	 (lambda ()
	   (assconv/single-analyze ssa-candidates body*))
	 (lambda (let-like letrec-like)
	   (assconv/bind-cells
	    (lmap assconv/binding/name (append let-like letrec-like))
	    (list-transform-negative assigned
	      (lambda (binding)
		(or (memq binding let-like)
		    (memq binding letrec-like))))
	    (assconv/letify 'LET
			    let-like
			    (assconv/letify 'LETREC
					    letrec-like
					    body*))))))))

(define (assconv/first-assignment body)
  (let loop ((actions (list body)))
    (and (not (null? actions))
	 (pair? (car actions))
	 (case (car (car actions))
	   ((BEGIN)
	    (loop (append (cdr (car actions)) (cdr actions))))
	   ((DECLARE)
	    (loop (cdr actions)))
	   ((SET!)
	    (and (not (null? (cdr actions)))
		 (car actions)))
	   (else
	    false)))))

(define (assconv/bind-cells shadowed-names bindings body)
  ;; (values shadowed-names body*)
  ;; Last chance to undo an assignment
  (define (finish shadowed-names bindings body)
    (if (null? bindings)
	(values shadowed-names body)
	(begin
	  (for-each assconv/cellify! bindings)
	  (values
	   shadowed-names
	   `(LET ,(lmap (lambda (binding)
			  (let ((name (assconv/binding/name binding)))
			    `(,(assconv/binding/cell-name binding)
			      (CALL (QUOTE ,%make-cell)
				    (QUOTE #F)
				    (LOOKUP ,name)
				    (QUOTE ,name)))))
			bindings)
	      ,body)))))

  (define (default)
    (finish shadowed-names bindings body))

  (cond ((null? bindings)
	 (default))
	((assconv/first-assignment body)
	 => (lambda (ass)
	      (let* ((name (cadr ass))
		     (binding
		      (list-search-positive bindings
			(lambda (binding)
			  (eq? (assconv/binding/name binding)
			       name))))
		     (value (caddr ass)))
		(if (or (not binding)
			(not (null? (cdr (assconv/binding/assignments
					  binding))))
			(memq name (form/free-vars value))) ; JSM
		    (default)
		    (begin
		      (form/rewrite! ass `(QUOTE ,%unspecific))
		      (finish (cons name shadowed-names)
			      (delq binding bindings)
			      (bind name value body)))))))
	(else (default))))

(define (assconv/letify keyword bindings body)
  `(,keyword
    ,(lmap (lambda (binding)
	     (let* ((ass (car (assconv/binding/assignments binding)))
		    (value (caddr ass)))
	       (form/rewrite! ass `(QUOTE ,%unassigned))
	       `(,(assconv/binding/name binding) ,value)))
	   bindings)
    ,body))

(define (assconv/cell-reference binding)
  `(CALL (QUOTE ,%cell-ref)
	 (QUOTE #F)
	 (LOOKUP ,(assconv/binding/cell-name binding))
	 (QUOTE ,(assconv/binding/name binding))))

(define (assconv/cell-assignment binding value)
  (let ((cell-name (assconv/binding/cell-name binding))
	(value-name (assconv/binding/name binding)))
    #|
    ;; This returns the new value
    (bind value-name value
	  `(BEGIN
	     (CALL (QUOTE ,%cell-set!)
		   (QUOTE #F)
		   (LOOKUP ,cell-name)
		   (LOOKUP ,value-name)
		   (QUOTE ,value-name))
	     (LOOKUP ,value-name)))
    |#
    ;; This returns the old value
    (bind value-name
	  `(CALL (QUOTE ,%cell-ref)
		 (QUOTE #F)
		 (LOOKUP ,cell-name)
		 (QUOTE ,value-name))
	  `(BEGIN
	     (CALL (QUOTE ,%cell-set!)
		   (QUOTE #F)
		   (LOOKUP ,cell-name)
		   ,value
		   (QUOTE ,value-name))
	     (LOOKUP ,value-name)))))

(define (assconv/cellify! binding)
  (let ((cell-name (assconv/new-cell-name (assconv/binding/name binding))))
    (set-assconv/binding/cell-name! binding cell-name)
    (for-each (lambda (ref)
		(form/rewrite!
		 ref
		 (assconv/cell-reference binding)))
	      (assconv/binding/references binding))
    (for-each (lambda (ass)
		(form/rewrite!
		 ass
		 (assconv/cell-assignment binding (caddr ass))))
	      (assconv/binding/assignments binding))))

(define (assconv/env-lookup env name)
  (let spine-loop ((env env))
    (and (not (null? env))
	 (let rib-loop ((rib (car env)))
	   (cond ((null? rib)
		  (spine-loop (cdr env)))
		 ((eq? name (assconv/binding/name (car rib)))
		  (car rib))
		 (else
		  (rib-loop (cdr rib))))))))

(define (assconv/single-assignment/trivial? assignment-form)
  (let ((name (second assignment-form))
	(value (third assignment-form)))
    (and (pair? value)
	 (or (eq? (car value) 'QUOTE)
	     (and (eq? (car value) 'LAMBDA)
		  #| (not (memq name (form/free-vars value))) |#
		     )))))

(define (assconv/single-analyze ssa-candidates body)
  ;; (values let-like letrec-like)
  ;; This only recognizes very simple patterns.
  ;; It can be improved in the future.
  (if (not (pair? body))
      (values '() '())
      (let ((single-assignments
	     (lmap (lambda (binding)
		     (cons (car (assconv/binding/assignments binding))
			   binding))
		   ssa-candidates))
	    (finish
	     (lambda (bindings)
	       (values
		(reverse
		 (list-transform-positive bindings
		   (lambda (binding)
		     (eq? (car (caddr (car (assconv/binding/assignments
					    binding))))
			  'QUOTE))))
		(reverse
		 (list-transform-positive bindings
		   (lambda (binding)
		     (eq? (car (caddr (car (assconv/binding/assignments
					    binding))))
			  'LAMBDA))))))))

	(let loop ((bindings '())
		   (actions (if (eq? (car body) 'BEGIN)
				(cdr body)
				(list body))))
	  (cond ((null? actions)
		 (finish bindings))
		((assq (car actions) single-assignments)
		 => (lambda (single-assignment)
		      (loop (cons (cdr single-assignment) bindings)
			    (cdr actions))))
		((not (pair? (car actions)))
		 (finish bindings))
		(else
		 (case (caar actions)
		   ((DECLARE)
		    (loop bindings (cdr actions)))
		   ((SET!)
		    (if (assconv/single-assignment/trivial? (car actions))
			(loop bindings (cdr actions))
			(finish bindings)))
		   (else
		    (finish bindings)))))))))