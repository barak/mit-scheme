#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/utils.scm,v 1.92 1987/11/21 18:43:08 jinx Exp $

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
    (number->string (generate-label-number)))))

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

;;;; SCode Interface

(let-syntax ((define-scode-operator
	       (macro (name)
		 `(DEFINE ,(symbol-append 'SCODE/ name)
		    (ACCESS ,name SYSTEM-GLOBAL-ENVIRONMENT)))))
  (define-scode-operator access-components)
  (define-scode-operator access?)
  (define-scode-operator assignment?)
  (define-scode-operator assignment-components)
  (define-scode-operator assignment-name)
  (define-scode-operator assignment-value)
  (define-scode-operator combination-components)
  (define-scode-operator combination?)
  (define-scode-operator comment-expression)
  (define-scode-operator comment-text)
  (define-scode-operator comment?)
  (define-scode-operator conditional-components)
  (define-scode-operator definition-components)
  (define-scode-operator delay?)
  (define-scode-operator delay-expression)
  (define-scode-operator disjunction-components)
  (define-scode-operator in-package-components)
  (define-scode-operator lambda-components)
  (define-scode-operator lambda?)
  (define-scode-operator make-access)
  (define-scode-operator make-assignment)
  (define-scode-operator make-combination)
  (define-scode-operator make-comment)
  (define-scode-operator make-conditional)
  (define-scode-operator make-declaration)
  (define-scode-operator make-definition)
  (define-scode-operator make-disjunction)
  (define-scode-operator make-lambda)
  (define-scode-operator make-quotation)
  (define-scode-operator make-sequence)
  (define-scode-operator make-the-environment)
  (define-scode-operator make-variable)
  (define-scode-operator make-unassigned-object)
  (define-scode-operator open-block-components)
  (define-scode-operator open-block?)
  (define-scode-operator primitive-procedure?)
  (define-scode-operator procedure?)
  (define-scode-operator quotation-expression)
  (define-scode-operator sequence-actions)
  (define-scode-operator unassigned-object?)
  (define-scode-operator unassigned?-name)
  (define-scode-operator unbound?-name)
  (define-scode-operator variable-name)
  (define-scode-operator variable?))

;;; Scode constants

(define scode/constant?
  (access scode-constant? system-global-environment))

(define scode/constant?
  (access scode-constant? system-global-environment))

(define-integrable (scode/constant-value const)
  const)

(define-integrable (scode/make-constant const)
  const)

;;; Abolute variables and combinations

(define (scode/make-absolute-reference variable-name)
  (scode/make-access '() variable-name))

(define (scode/absolute-reference? obj)
  (and (scode/access? obj)
       (scode/access-components
	obj
	(lambda (environment name)
	  (null? environment)))))

(define (scode/absolute-reference-name obj)
  (scode/access-components obj (lambda (ignore name) name)))

(define (scode/make-absolute-combination name operands)
  (scode/make-combination (scode/make-absolute-reference name) operands))

(define (scode/absolute-combination? obj)
  (and (scode/combination? obj)
       (scode/combination-components
	obj
	(lambda (op ops)
	  (scode/absolute-reference? obj)))))

(define (scode/absolute-combination-components obj receiver)
  (scode/combination-components
   obj
   (lambda (op ops)
     (receiver (scode/absolute-reference-name op) ops))))

(define (scode/error-combination-components combination receiver)
  (scode/combination-components combination
    (lambda (operator operands)
      (receiver (car operands)
		(let ((irritant (cadr operands)))
		  (cond ((scode/access? irritant) '())
			((scode/absolute-combination? irritant)
			 (scode/absolute-combination-components irritant
			   (lambda (name operands)
			     (if (eq? name 'LIST)
				 operands
				 (list irritant)))))
			(else (list irritant))))))))

(define (scode/make-error-combination message operand)
  (scode/make-absolute-combination
   'ERROR-PROCEDURE
   (list message operand (scode/make-the-environment))))

(define (scode/procedure-type-code *lambda)
  (cond ((primitive-type? type-code:lambda *lambda)
	 type-code:procedure)
	((primitive-type? type-code:extended-lambda *lambda)
	 type-code:extended-procedure)
	(else
	 (error "SCODE/PROCEDURE-TYPE-CODE: Unknown lambda type" *lambda))))

(define (scode/make-let names values body)
  (scode/make-combination (scode/make-lambda lambda-tag:let names '() false '()
					     '() body)
			  values))

;;;; Type Codes

(let-syntax ((define-type-code
	       (macro (var-name type-name)
		 `(define-integrable ,var-name ',(microcode-type type-name)))))

(define-type-code type-code:lambda LAMBDA)
(define-type-code type-code:extended-lambda EXTENDED-LAMBDA)
(define-type-code type-code:procedure PROCEDURE)
(define-type-code type-code:extended-procedure EXTENDED-PROCEDURE)
(define-type-code type-code:cell CELL)
(define-type-code type-code:compiled-expression COMPILED-EXPRESSION)
(define-type-code type-code:compiler-link COMPILER-LINK)
(define-type-code type-code:compiled-procedure COMPILED-PROCEDURE)
(define-type-code type-code:environment ENVIRONMENT)
(define-type-code type-code:stack-environment STACK-ENVIRONMENT)
(define-type-code type-code:return-address COMPILER-RETURN-ADDRESS)
(define-type-code type-code:unassigned UNASSIGNED)
)

;;; Disgusting hack to replace microcode implementation.

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

;; Primitives are non pointers, but need to be updated by the fasloader;
;; they cannot appear as immediate constants in the instruction stream.
;; Therefore, for the purposes of compilation, they are treated as pointers.

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
  (memq operator constant-foldable-operators))

(define constant-foldable-operators
  (list primitive-type primitive-type?
	eq? null? pair? number? complex? real? rational? integer?
	zero? positive? negative? odd? even? exact? inexact?
	= < > <= >= max min
	+ - * / 1+ -1+ abs quotient remainder modulo integer-divide
	gcd lcm floor ceiling truncate round
	exp log expt sqrt sin cos tan asin acos atan
	(ucode-primitive &+) (ucode-primitive &-)
	(ucode-primitive &*) (ucode-primitive &/)
	(ucode-primitive &<) (ucode-primitive &>)
	(ucode-primitive &=) (ucode-primitive &atan)))