;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1986 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Compiler Utilities

;;; $Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/base/utils.scm,v 1.78 1986/12/21 14:52:59 cph Exp $

(declare (usual-integrations))
(using-syntax (access compiler-syntax-table compiler-package)

;;;; Support for tagged objects

(define (make-vector-tag parent name)
  (let ((tag (cons '() (or parent vector-tag:object))))
    (vector-tag-put! tag ':TYPE-NAME name)
    ((access add-unparser-special-object! unparser-package) tag
     (lambda (object)
       (unparse-with-brackets
	(lambda ()
	  (write-string "LIAR ")
	  (fluid-let ((*unparser-radix* 10))
	    (write (hash object)))
	  (write-string " ")
	  (fluid-let ((*unparser-radix* 16))
	    ((vector-method object ':UNPARSE) object))))))
    tag))

(define (vector-tag-put! tag key value)
  (let ((entry (assq key (car tag))))
    (if entry
	(set-cdr! entry value)
	(set-car! tag (cons (cons key value) (car tag))))))

(define (vector-tag-get tag key)
  (define (loop tag)
    (and (pair? tag)
	 (or (assq key (car tag))
	     (loop (cdr tag)))))
  (let ((value
	 (or (assq key (car tag))
	     (loop (cdr tag)))))
    (and value (cdr value))))

(define vector-tag:object (list '()))
(vector-tag-put! vector-tag:object ':TYPE-NAME 'OBJECT)

(define-integrable (vector-tag vector)
  (vector-ref vector 0))

(define (define-vector-method tag name method)
  (vector-tag-put! tag name method)
  name)

(define (vector-tag-method tag name)
  (or (vector-tag-get tag name)
      (error "Unbound method" tag name)))

(define-integrable (vector-tag-parent-method tag name)
  (vector-tag-method (cdr tag) name))

(define-integrable (vector-method vector name)
  (vector-tag-method (vector-tag vector) name))

(define (define-unparser tag unparser)
  (define-vector-method tag ':UNPARSE unparser))

(define-integrable make-tagged-vector
  vector)

(define ((tagged-vector-predicate tag) object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? tag (vector-tag object))))

(define (tagged-vector-subclass-predicate tag)
  (define (loop tag*)
    (or (eq? tag tag*)
	(and (pair? tag*)
	     (loop (cdr tag*)))))
  (lambda (object)
    (and (vector? object)
	 (not (zero? (vector-length object)))
	 (loop (vector-tag object)))))

(define tagged-vector?
  (tagged-vector-subclass-predicate vector-tag:object))

(define-unparser vector-tag:object
  (lambda (object)
    (write (vector-method object ':TYPE-NAME))))

(define (->tagged-vector object)
  (or (and (tagged-vector? object) object)
      (and (integer? object)
	   (let ((object (unhash object)))
	     (and (tagged-vector? object) object)))))

;;;; Queue

(define (make-queue)
  (cons '() '()))

(define-integrable (queue-empty? queue)
  (null? (car queue)))

(define-integrable (queued? queue item)
  (memq item (car queue)))

(define (enqueue! queue object)
  (let ((next (cons object '())))
    (if (null? (cdr queue))
	(set-car! queue next)
	(set-cdr! (cdr queue) next))
    (set-cdr! queue next)))

(define (dequeue! queue)
  (let ((next (car queue)))
    (if (null? (cdr next))
	(begin (set-car! queue '())
	       (set-cdr! queue '()))
	(set-car! queue (cdr next)))
    (car next)))

(define (queue-map! queue procedure)
  (define (loop)
    (if (not (queue-empty? queue))
	(begin (procedure (dequeue! queue))
	       (loop))))
  (loop))

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
	   ((eq? prefix lambda-tag:make-package) 'MAKE-PACKAGE)
	   ((or (eq? prefix lambda-tag:shallow-fluid-let)
		(eq? prefix lambda-tag:deep-fluid-let)
		(eq? prefix lambda-tag:common-lisp-fluid-let))
	    'FLUID-LET)
	   (else prefix)))
    "-"
    (write-to-string (generate-label-number)))))

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

(define &make-object
  (make-primitive-procedure '&MAKE-OBJECT))

;;;; Set Operations

(define (eq-set-adjoin element set)
  (if (memq element set)
      set
      (cons element set)))

(define (eqv-set-adjoin element set)
  (if (memv element set)
      set
      (cons element set)))

(define (eq-set-delete set item)
  (define (loop set)
    (cond ((null? set) '())
	  ((eq? (car set) item) (cdr set))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eqv-set-delete set item)
  (define (loop set)
    (cond ((null? set) '())
	  ((eqv? (car set) item) (cdr set))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eq-set-substitute set old new)
  (define (loop set)
    (cond ((null? set) '())
	  ((eq? (car set) old) (cons new (cdr set)))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (eqv-set-substitute set old new)
  (define (loop set)
    (cond ((null? set) '())
	  ((eqv? (car set) old) (cons new (cdr set)))
	  (else (cons (car set) (loop (cdr set))))))
  (loop set))

(define (set-search set procedure)
  (define (loop items)
    (and (not (null? items))
	 (or (procedure (car items))
	     (loop (cdr items)))))
  (loop set))

;;; The dataflow analyzer assumes that
;;; (eq? (list-tail (eq-set-union x y) n) y) for some n.

(define (eq-set-union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (if (memq (car x) y)
		      y
		      (cons (car x) y)))))))

(define (eqv-set-union x y)
  (if (null? y)
      x
      (let loop ((x x) (y y))
	(if (null? x)
	    y
	    (loop (cdr x)
		  (if (memv (car x) y)
		      y
		      (cons (car x) y)))))))

(define (eq-set-difference x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memq (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x))))))
  (loop x))

(define (eqv-set-difference x y)
  (define (loop x)
    (cond ((null? x) '())
	  ((memv (car x) y) (loop (cdr x)))
	  (else (cons (car x) (loop (cdr x))))))
  (loop x))

;;;; SCode Interface

(let-syntax ((define-scode-operator
	       (macro (name)
		 `(DEFINE ,(symbol-append 'SCODE: name)
		    (ACCESS ,name SYSTEM-GLOBAL-ENVIRONMENT)))))
  (define-scode-operator access-components)
  (define-scode-operator access?)
  (define-scode-operator assignment-components)
  (define-scode-operator combination-components)
  (define-scode-operator combination?)
  (define-scode-operator comment-expression)
  (define-scode-operator comment?)
  (define-scode-operator conditional-components)
  (define-scode-operator definition-components)
  (define-scode-operator delay-expression)
  (define-scode-operator disjunction-components)
  (define-scode-operator in-package-components)
  (define-scode-operator lambda-components)
  (define-scode-operator lambda?)
  (define-scode-operator make-access)
  (define-scode-operator make-combination)
  (define-scode-operator make-conditional)
  (define-scode-operator make-definition)
  (define-scode-operator make-lambda)
  (define-scode-operator make-quotation)
  (define-scode-operator make-sequence)
  (define-scode-operator make-variable)
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

(define scode:constant?
  (access scode-constant? system-global-environment))

(define (scode:error-combination-components combination receiver)
  (scode:combination-components combination
    (lambda (operator operands)
      (receiver (car operands)
		(let ((irritant (cadr operands)))
		  (cond ((scode:access? irritant) '())
			((scode:combination? irritant)
			 (scode:combination-components irritant
			   (lambda (operator operands)
			     (if (and (scode:access? operator)
				      (scode:access-components operator
					(lambda (environment name)
					  (and (null? environment)
					       (eq? name 'LIST)))))
				 operands
				 (list irritant)))))
			(else (list irritant))))))))

(define (scode:procedure-type-code *lambda)
  (cond ((primitive-type? type-code:lambda *lambda)
	 type-code:procedure)
	((primitive-type? type-code:extended-lambda *lambda)
	 type-code:extended-procedure)
	(else
	 (error "SCODE:PROCEDURE-TYPE-CODE: Unknown lambda type" *lambda))))

;;;; Type Codes

(define type-code:lambda
  (microcode-type 'LAMBDA))

(define type-code:extended-lambda
  (microcode-type 'EXTENDED-LAMBDA))

(define type-code:procedure
  (microcode-type 'PROCEDURE))

(define type-code:extended-procedure
  (microcode-type 'EXTENDED-PROCEDURE))

(define type-code:cell
  (microcode-type 'CELL))

(define type-code:compiled-expression
  (microcode-type 'COMPILED-EXPRESSION))

(define type-code:compiler-link
  (microcode-type 'COMPILER-LINK))

(define type-code:compiled-procedure
  (microcode-type 'COMPILED-PROCEDURE))

(define type-code:environment
  (microcode-type 'ENVIRONMENT))

(define type-code:stack-environment
  (microcode-type 'STACK-ENVIRONMENT))

(define type-code:return-address
  (microcode-type 'COMPILER-RETURN-ADDRESS))

(define type-code:unassigned
  (microcode-type 'UNASSIGNED))

;;; Disgusting hack to replace microcode implementation.

(define (primitive-procedure-safe? object)
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
			   with-history-disabled
			   force
			   primitive-purify
			   complete-garbage-collect
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
			   error-procedure)))))

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
      (primitive-type? (ucode-type extended-fixnum) object)
      (primitive-type? (ucode-type manifest-nm-vector) object)
      (primitive-type? (ucode-type manifest-special-nm-vector) object)))

(define (object-immutable? object)
  (or (non-pointer-object? object)
      (number? object)
      (symbol? object)
      (scode:primitive-procedure? object)
      (eq? object compiled-error-procedure)))

(define (operator-constant-foldable? operator)
  (memq operator constant-foldable-operators))

(define constant-foldable-operators
  (map* (list primitive-type primitive-type?
	      eq? null? pair? car cdr vector-length vector-ref
	      number? complex? real? rational? integer?
	      zero? positive? negative? odd? even? exact? inexact?
	      = < > <= >= max min
	      + - * / 1+ -1+ abs quotient remainder modulo integer-divide
	      gcd lcm floor ceiling truncate round
	      exp log expt sqrt sin cos tan asin acos atan)
	make-primitive-procedure
	'(&+ &- &* &/ &< &> &= &ATAN)))

;;; end USING-SYNTAX
)

;;; Edwin Variables:
;;; Scheme Environment: compiler-package
;;; Scheme Syntax Table: (access compiler-syntax-table compiler-package)
;;; Tags Table Pathname: (access compiler-tags-pathname compiler-package)
;;; End:
	(ucode-primitive &=) (ucode-primitive &atan)))