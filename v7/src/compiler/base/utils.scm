#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/utils.scm,v 4.1 1987/12/04 20:05:24 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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
  (if (unassigned? prefix) (set! prefix 'LABEL))
  (string->symbol
   (string-append
    (symbol->string
     (cond ((eq? prefix lambda-tag:unnamed) 'LAMBDA)
	   ((eq? prefix lambda-tag:let) 'LET)
	   ((eq? prefix lambda-tag:make-environment) 'MAKE-ENVIRONMENT)
	   ((or (eq? prefix lambda-tag:shallow-fluid-let)
		(eq? prefix lambda-tag:deep-fluid-let)
		(eq? prefix lambda-tag:common-lisp-fluid-let))
	    'FLUID-LET)
	   (else prefix)))
    "-"
    (number->string (generate-label-number) 10))))

(define *current-label-number*)

(define (generate-label-number)
  (let ((number *current-label-number*))
    (set! *current-label-number* (1+ *current-label-number*))
    number))

(define (copy-alist alist)
  (if (null? alist)
      '()
      (cons (cons (caar alist) (cdar alist))
	    (copy-alist (cdr alist)))))

(define (boolean=? x y)
  (if x y (not y)))

(define (warn message . irritants)
  (newline)
  (write-string "Warning: ")
  (write-string message)
  (for-each (lambda (irritant)
	      (write-string " ")
	      (write irritant))
	    irritants))

(define (show-time thunk)
  (let ((start (runtime)))
    (let ((value (thunk)))
      (write-line (- (runtime) start))
      value)))

(define (list-filter-indices items indices)
  (let loop ((items items) (indices indices) (index 0))
    (cond ((null? indices) '())
	  ((= (car indices) index)
	   (cons (car items)
		 (loop (cdr items) (cdr indices) (1+ index))))
	  (else
	   (loop (cdr items) indices (1+ index))))))

(define (there-exists? items predicate)
  (let loop ((items items))
    (and (not (null? items))
	 (or (predicate (car items))
	     (loop (cdr items))))))

(define (for-all? items predicate)
  (let loop ((items items))
    (or (null? items)
	(and (predicate (car items))
	     (loop (cdr items))))))

(define (all-eq? items)
  (if (null? items)
      (error "ALL-EQ? undefined for empty set"))
  (or (null? (cdr items))
      (for-all? (cdr items)
	(let ((item (car items)))
	  (lambda (item*)
	    (eq? item item))))))

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

;;;; Symbol Hash Tables

(define (symbol-hash-table/make n-buckets)
  (make-vector n-buckets '()))

(define (symbol-hash-table/modify! table symbol if-found if-not-found)
  (let ((hash (string-hash-mod (symbol->string symbol) (vector-length table))))
    (let ((bucket (vector-ref table hash)))
      (let ((entry (assq symbol bucket)))
	(if entry
	    (set-cdr! entry (if-found (cdr entry)))
	    (vector-set! table hash
			 (cons (cons symbol (if-not-found))
			       bucket)))))))

(define (symbol-hash-table/lookup* table symbol if-found if-not-found)
  (let ((value
	 (assq symbol
	       (vector-ref table
			   (string-hash-mod (symbol->string symbol)
					    (vector-length table))))))
    (if value
	(if-found (cdr value))
	(if-not-found))))

(define (symbol-hash-table/insert! table symbol item)
  (symbol-hash-table/modify! table symbol
			     (lambda (old-value) item)
			     (lambda () item)))

(define (symbol-hash-table/lookup table symbol)
  (symbol-hash-table/lookup* table symbol
			     identity-procedure
			     (lambda () (error "Missing item" symbol))))

(define (symbol-hash-table/bindings table)
  (apply append (vector->list table)))

(define (symbol-hash-table/positive-bindings table predicate)
  (mapcan (lambda (bucket)
	    (list-transform-positive bucket
	      (lambda (entry)
		(predicate (cdr entry)))))
	  (vector->list table)))

(define (symbol-hash-table/negative-bindings table predicate)
  (mapcan (lambda (bucket)
	    (list-transform-negative bucket
	      (lambda (entry)
		(predicate (cdr entry)))))
	  (vector->list table)))

(define-integrable string-hash-mod
  (ucode-primitive string-hash-mod))

;;;; Type Codes

(let-syntax ((define-type-code
	       (macro (var-name #!optional type-name)
		 (if (unassigned? type-name) (set! type-name var-name))
		 `(DEFINE-INTEGRABLE ,(symbol-append 'TYPE-CODE: var-name)
		    ',(microcode-type type-name)))))
  (define-type-code lambda)
  (define-type-code extended-lambda)
  (define-type-code procedure)
  (define-type-code extended-procedure)
  (define-type-code cell)
  (define-type-code compiled-expression)
  (define-type-code compiler-link)
  (define-type-code compiled-procedure)
  (define-type-code environment)
  (define-type-code stack-environment)
  (define-type-code return-address compiler-return-address)
  (define-type-code unassigned))

(define (scode/procedure-type-code *lambda)
  (cond ((primitive-type? type-code:lambda *lambda)
	 type-code:procedure)
	((primitive-type? type-code:extended-lambda *lambda)
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
      (> argument-count 1)
      (let ((arity (primitive-procedure-arity primitive)))
	(or (= arity -1)
	    (= arity argument-count)))))

(define (primitive-procedure-safe? object)
  (and (primitive-type? (ucode-type primitive) object)
       (not (memq object
		  (let-syntax ((primitives
				(macro names
				  `'(,@(map make-primitive-procedure names)))))
		    (primitives call-with-current-continuation
				non-reentrant-call-with-current-continuation
				scode-eval
				apply
				garbage-collect
				primitive-fasdump
				set-current-history!
				with-history-disabled
				force
				primitive-purify
				;;complete-garbage-collect
				dump-band
				primitive-impurify
				with-threaded-continuation
				within-control-point
				with-interrupts-reduced
				primitive-eval-step
				primitive-apply-step
				primitive-return-step
				execute-at-new-state-point
				translate-to-state-point
				with-interrupt-mask
				error-procedure))))))

;;;; Special Compiler Support

(define compiled-error-procedure
  "Compiled error procedure")

(define lambda-tag:delay
  (make-named-tag "DELAY-LAMBDA"))

(define (non-pointer-object? object)
  (or (primitive-type? (ucode-type false) object)
      (primitive-type? (ucode-type true) object)
      (primitive-type? (ucode-type fixnum) object)
      (primitive-type? (ucode-type character) object)
      (primitive-type? (ucode-type unassigned) object)
      (primitive-type? (ucode-type the-environment) object)
      (primitive-type? (ucode-type manifest-nm-vector) object)
      (primitive-type? (ucode-type manifest-special-nm-vector) object)))

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
	    '(PRIMITIVE-TYPE PRIMITIVE-TYPE?
	      EQ? NULL? PAIR? NUMBER? COMPLEX? REAL? RATIONAL? INTEGER?
	      ZERO? POSITIVE? NEGATIVE? ODD? EVEN? EXACT? INEXACT?
	      = < > <= >= MAX MIN
	      + - * / 1+ -1+ ABS QUOTIENT REMAINDER MODULO INTEGER-DIVIDE
	      GCD LCM FLOOR CEILING TRUNCATE ROUND
	      EXP LOG EXPT SQRT SIN COS TAN ASIN ACOS ATAN))
     (access primitive-procedure? system-global-environment))
   (list
    (ucode-primitive &+) (ucode-primitive &-)
    (ucode-primitive &*) (ucode-primitive &/)
    (ucode-primitive &<) (ucode-primitive &>)
    (ucode-primitive &=) (ucode-primitive &atan))))