#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/utils.scm,v 4.6 1988/11/08 21:25:58 jinx Exp $

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

;;;; Compiler Utilities

(declare (usual-integrations))

;;;; Miscellaneous

(define (three-way-sort = set set* receiver)
  (let ((member? (member-procedure =)))
    (define (loop set set* receiver)
      (if (null? set)
	  (receiver '() '() set*)
	  (let ((item (member? (car set) set*)))
	    (if item
		(loop (cdr set) (delq! (car item) set*)
		  (lambda (set-only both set*-only)
		    (receiver set-only
			      (cons (cons (car set) (car item)) both)
			      set*-only)))
		(loop (cdr set) set*
		  (lambda (set-only both set*-only)
		    (receiver (cons (car set) set-only)
			      both
			      set*-only)))))))
    (loop set (list-copy set*) receiver)))

(define (discriminate-items items predicate)
  (let loop ((items items) (passed '()) (failed '()))
    (cond ((null? items)
	   (return-2 passed failed))
	  ((predicate (car items))
	   (loop (cdr items) (cons (car items) passed) failed))
	  (else
	   (loop (cdr items) passed (cons (car items) failed))))))

(define (generate-label #!optional prefix)
  (if (default-object? prefix) (set! prefix 'LABEL))
  (string->symbol
   (string-append
    (symbol->string
     (cond ((eq? prefix lambda-tag:unnamed) 'LAMBDA)
	   ((eq? prefix lambda-tag:let) 'LET)
	   ((eq? prefix lambda-tag:make-environment) 'MAKE-ENVIRONMENT)
	   ((eq? prefix lambda-tag:fluid-let) 'FLUID-LET)
	   (else prefix)))
    "-"
    (number->string (generate-label-number) 10))))

(define *current-label-number*)

(define (generate-label-number)
  (let ((number *current-label-number*))
    (set! *current-label-number* (1+ *current-label-number*))
    number))

(define (list-filter-indices items indices)
  (let loop ((items items) (indices indices) (index 0))
    (cond ((null? indices) '())
	  ((= (car indices) index)
	   (cons (car items)
		 (loop (cdr items) (cdr indices) (1+ index))))
	  (else
	   (loop (cdr items) indices (1+ index))))))

(define (all-eq? items)
  (if (null? items)
      (error "ALL-EQ? undefined for empty set"))
  (or (null? (cdr items))
      (for-all? (cdr items)
	(let ((item (car items)))
	  (lambda (item*)
	    (eq? item item*))))))

(define (all-eq-map? items map)
  (if (null? items)
      (error "ALL-EQ-MAP? undefined for empty set"))
  (let ((item (map (car items))))
    (if (or (null? (cdr items))
	    (for-all? (cdr items) (lambda (item*) (eq? item (map item*)))))
	(return-2 true item)
	(return-2 false false))))

(define (eq-set-union* set sets)
  (let loop ((set set) (sets sets) (accum '()))
    (if (null? sets)
	(eq-set-union set accum)
	(loop (car sets) (cdr sets) (eq-set-union set accum)))))

(package (transitive-closure enqueue-node! enqueue-nodes!)

(define *queue*)

(define-export (transitive-closure initialization process-node nodes)
  (fluid-let ((*queue* true))
    (if initialization (initialization))
    (set! *queue* nodes)
    (let loop ()
      (if (not (null? *queue*))
	  (begin (let ((node (car *queue*)))
		   (set! *queue* (cdr *queue*))
		   (process-node node))
		 (loop))))))

(define-export (enqueue-node! node)
  (if (and (not (eq? *queue* true))
	   (not (memq node *queue*)))
      (set! *queue* (cons node *queue*))))

(define-export (enqueue-nodes! nodes)
  (if (not (eq? *queue* true))
      (set! *queue* (eq-set-union nodes *queue*))))

)

;;;; Type Codes

(let-syntax ((define-type-code
	       (macro (var-name #!optional type-name)
		 (if (default-object? type-name) (set! type-name var-name))
		 `(DEFINE-INTEGRABLE ,(symbol-append 'TYPE-CODE: var-name)
		    ',(microcode-type type-name)))))
  (define-type-code lambda)
  (define-type-code extended-lambda)
  (define-type-code procedure)
  (define-type-code extended-procedure)
  (define-type-code cell)
  (define-type-code environment)
  (define-type-code unassigned)
  (define-type-code stack-environment)
  (define-type-code compiled-entry))

(define (scode/procedure-type-code *lambda)
  (cond ((object-type? type-code:lambda *lambda)
	 type-code:procedure)
	((object-type? type-code:extended-lambda *lambda)
	 type-code:extended-procedure)
	(else
	 (error "SCODE/PROCEDURE-TYPE-CODE: Unknown lambda type" *lambda))))

;;;; Primitive Procedures

(define (primitive-procedure? object)
  (or (eq? compiled-error-procedure object)
      (scode/primitive-procedure? object)))

(define (normal-primitive-procedure? object)
  (or (eq? compiled-error-procedure object)
      (and (scode/primitive-procedure? object)
	   (primitive-procedure-safe? object))))

(define (primitive-arity-correct? primitive argument-count)
  (if (eq? primitive compiled-error-procedure)
      (positive? argument-count)
      (let ((arity (primitive-procedure-arity primitive)))
	(or (= arity -1)
	    (= arity argument-count)))))

(define (primitive-procedure-safe? object)
  (and (object-type? (ucode-type primitive) object)
       (not (memq object unsafe-primitive-procedures))))

(define unsafe-primitive-procedures
  (let-syntax ((primitives
		(macro names
		  `'(,@(map (lambda (spec)
			      (if (pair? spec)
				  (apply make-primitive-procedure spec)
				  (make-primitive-procedure spec)))
			    names)))))
    (primitives scode-eval
		apply
		force
		error-procedure
		within-control-point
		call-with-current-continuation
		non-reentrant-call-with-current-continuation
		with-interrupt-mask
		with-interrupts-reduced
		execute-at-new-state-point
		translate-to-state-point
		set-current-history!
		with-history-disabled
		garbage-collect
		primitive-purify
		primitive-impurify
		primitive-fasdump
		dump-band
		load-band
		(primitive-eval-step 3)
		(primitive-apply-step 3)
		(primitive-return-step 2)
		(dump-world 1)
		(complete-garbage-collect 1)
		(with-saved-fluid-bindings 1)
		(global-interrupt 3)
		(get-work 1)
		(master-gc-loop 1))))

;;;; Special Compiler Support

(define compiled-error-procedure
  "Compiled error procedure")

(define lambda-tag:delay
  (make-named-tag "DELAY-LAMBDA"))

(define (non-pointer-object? object)
  ;; Any reason not to use `object/non-pointer?' here? -- cph
  (or (object-type? (ucode-type false) object)
      (object-type? (ucode-type true) object)
      (object-type? (ucode-type fixnum) object)
      (object-type? (ucode-type character) object)
      (object-type? (ucode-type unassigned) object)
      (object-type? (ucode-type the-environment) object)
      (object-type? (ucode-type manifest-nm-vector) object)
      (object-type? (ucode-type manifest-special-nm-vector) object)))

(define (object-immutable? object)
  (or (non-pointer-object? object)
      (number? object)
      (symbol? object)
      (scode/primitive-procedure? object)
      (eq? object compiled-error-procedure)))

(define (operator-constant-foldable? operator)
  (memq operator constant-foldable-primitives))

(define constant-foldable-primitives
  (append!
   (list-transform-positive
       (map (lambda (name)
	      (lexical-reference system-global-environment name))
	    '(OBJECT-TYPE OBJECT-TYPE?
	      EQ? NULL? PAIR? NUMBER? COMPLEX? REAL? RATIONAL? INTEGER?
	      ZERO? POSITIVE? NEGATIVE? ODD? EVEN? EXACT? INEXACT?
	      = < > <= >= MAX MIN
	      + - * / 1+ -1+ ABS QUOTIENT REMAINDER MODULO INTEGER-DIVIDE
	      GCD LCM FLOOR CEILING TRUNCATE ROUND
	      EXP LOG EXPT SQRT SIN COS TAN ASIN ACOS ATAN
	      FIX:ZERO? FIX:NEGATIVE? FIX:POSITIVE?
	      FIX:= FIX:< FIX:> FIX:1+ FIX:-1+ FIX:+ FIX:- FIX:*
	      FIX:DIVIDE FIX:GCD FIX:QUOTIENT FIX:REMAINDER))
     (lexical-reference system-global-environment 'PRIMITIVE-PROCEDURE?))
   (list
    (ucode-primitive &+) (ucode-primitive &-)
    (ucode-primitive &*) (ucode-primitive &/)
    (ucode-primitive &<) (ucode-primitive &>)
    (ucode-primitive &=) (ucode-primitive &atan))))