;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/error.scm,v 13.45 1987/04/03 00:51:34 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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

;;;; Error System

(declare (usual-integrations)
	 (integrate-primitive-procedures set-fixed-objects-vector!))

(define error-procedure
  (make-primitive-procedure 'ERROR-PROCEDURE))

(define (error-from-compiled-code message . irritant-info)
  (error-procedure message
		   (cond ((null? irritant-info) *the-non-printing-object*)
			 ((null? (cdr irritant-info)) (car irritant-info))
			 (else irritant-info))
		   (rep-environment)))

(define (error-message)
  (access error-message error-system))

(define (error-irritant) 
  (access error-irritant error-system))

(define error-prompt
  "Error->")

(define error-system
  (make-environment

(define *error-code*)
(define *error-hook*)
(define *error-decision-hook* false)

(define error-message
  "")

(define error-irritant
  *the-non-printing-object*)

;;;; REP Interface

(define (error-procedure-handler message irritant environment)
  (with-proceed-point
   proceed-value-filter
   (lambda ()
     (fluid-let ((error-message message)
		 (error-irritant irritant))
       (*error-hook* environment message irritant false)))))

(define ((error-handler-wrapper handler) error-code interrupt-enables)
  (with-interrupts-reduced INTERRUPT-MASK-GC-OK
   (lambda (old-mask)
     (fluid-let ((*error-code* error-code))
       (with-proceed-point
	proceed-value-filter
	(lambda ()
	  (set-interrupt-enables! interrupt-enables)
	  (handler (continuation-expression (rep-continuation)))))))))

(define (wrapped-error-handler wrapper)
  (access handler (procedure-environment wrapper)))

(define (start-error-rep message irritant)
  (fluid-let ((error-message message)
	      (error-irritant irritant))
    (let ((environment (continuation-environment (rep-continuation))))
      (if (continuation-undefined-environment? environment)
	  (*error-hook* (rep-environment) message irritant true)
	  (*error-hook* environment message irritant false)))))

(define (standard-error-hook environment message irritant
			     substitute-environment?)
  (push-rep environment
	    (let ((message (make-error-message message irritant)))
	      (if substitute-environment?
		  (lambda ()
		    (message)
		    (write-string "
There is no environment available;
using the current read-eval-print environment."))
		  message))
	    (standard-rep-prompt error-prompt)))

(define ((make-error-message message irritant))
  (newline)
  (write-string message)
  (if (not (eq? irritant *the-non-printing-object*))
      (let ((out (write-to-string irritant 40)))
	(write-char #\Space)
	(write-string (cdr out))
	(if (car out) (write-string "..."))))
  (if *error-decision-hook* (*error-decision-hook*)))

;;; (PROCEED) means retry error expression, (PROCEED value) means
;;; return VALUE as the value of the error subproblem.

(define (proceed-value-filter value)
  (let ((continuation (rep-continuation)))
    (if (or (null? value) (null-continuation? continuation))
	(continuation '())
	((continuation-next-continuation continuation) (car value)))))

;;;; Error Handlers

;;; All error handlers have the following form:

(define ((make-error-handler direction-alist operator-alist
			     default-handler default-combination-handler)
	 expression)
  ((let direction-loop ((alist direction-alist))
     (cond ((null? alist)
	    (cond ((combination? expression)
		   (let ((operator (combination-operator* expression)))
		     (let operator-loop ((alist operator-alist))
		       (cond ((null? alist) default-combination-handler)
			     ((memq operator (caar alist)) (cdar alist))
			     (else (operator-loop (cdr alist)))))))
		  (else default-handler)))
	   (((caar alist) expression) (cdar alist))
	   (else (direction-loop (cdr alist)))))
   expression))

;;; Then there are several methods for modifying the behavior of a
;;; given error handler.

(define expression-specific-adder)
(define operation-specific-adder)

(let ()
  (define (((alist-adder name) error-handler) filter receiver)
    (let ((environment
	   (procedure-environment (wrapped-error-handler error-handler))))
      (lexical-assignment environment
			  name
			  (cons (cons filter receiver)
				(lexical-reference environment name)))))

  (set! expression-specific-adder
	(alist-adder 'DIRECTION-ALIST))
  (set! operation-specific-adder
	(alist-adder 'OPERATOR-ALIST)))

(define default-expression-setter)
(define default-combination-setter)

(let ()
  (define (((set-default name) error-handler) receiver)
    (lexical-assignment
     (procedure-environment (wrapped-error-handler error-handler))
     name
     receiver))

  (set! default-expression-setter
	(set-default 'DEFAULT-HANDLER))
  (set! default-combination-setter
	(set-default 'DEFAULT-COMBINATION-HANDLER)))

;;;; Error Vector

;;; Initialize the error vector to the default state:

(define (error-code-or-name code)
  (let ((v (vector-ref (get-fixed-objects-vector)
		       (fixed-objects-vector-slot 'MICROCODE-ERRORS-VECTOR))))
    (if (or (>= code (vector-length v))
	    (null? (vector-ref v code)))
	code
	(vector-ref v code))))	

(define (default-error-handler expression)
  (start-error-rep "Anomalous error -- get a wizard"
		   (error-code-or-name *error-code*)))

(define system-error-vector
  (make-initialized-vector number-of-microcode-errors
    (lambda (error-code)
      (error-handler-wrapper
       (make-error-handler '()
			   '()
			   default-error-handler
			   default-error-handler)))))

;;; Use this procedure to displace the default handler completely.

(define (define-total-error-handler error-name handler)
  (vector-set! system-error-vector
	       (microcode-error error-name)
	       (error-handler-wrapper handler)))

;;; It will be installed later.

(define (install)
  (set! *error-hook* standard-error-hook)
  (vector-set! (get-fixed-objects-vector)
	       (fixed-objects-vector-slot 'SYSTEM-ERROR-VECTOR)
	       system-error-vector)
  (vector-set! (get-fixed-objects-vector)
	       (fixed-objects-vector-slot 'ERROR-PROCEDURE)
	       error-procedure-handler)
  (vector-set! (get-fixed-objects-vector)
	       (fixed-objects-vector-slot 'COMPILER-ERROR-PROCEDURE)
	       error-from-compiled-code)
  (set-fixed-objects-vector! (get-fixed-objects-vector)))

;;;; Error Definers

(define ((define-definer type definer) error-name . args)
  (apply definer
	 (type (vector-ref system-error-vector (microcode-error error-name)))
	 args))

(define ((define-specific-error error-name message) filter selector)
  ((cond ((pair? filter) define-operation-specific-error)
	 (else define-expression-specific-error))
   error-name filter message selector))

(define define-expression-specific-error
  (define-definer expression-specific-adder
    (lambda (adder filter message selector)
      (adder filter (expression-error-rep message selector)))))

(define define-operation-specific-error
  (define-definer operation-specific-adder
    (lambda (adder filter message selector)
      (adder filter (combination-error-rep message selector)))))

(define define-operand-error
  (define-definer default-combination-setter
    (lambda (setter message selector)
      (setter (combination-error-rep message selector)))))

(define define-operator-error
  (define-definer default-combination-setter
    (lambda (setter message)
      (setter (expression-error-rep message combination-operator*)))))

(define define-combination-error
  (define-definer default-combination-setter
    (lambda (setter message selector)
      (setter (expression-error-rep message selector)))))

(define define-default-error
  (define-definer default-expression-setter
    (lambda (setter message selector)
      (setter (expression-error-rep message selector)))))

(define ((expression-error-rep message selector) expression)
  (start-error-rep message (selector expression)))

(define ((combination-error-rep message selector) combination)
  (start-error-rep
   (string-append message
		  " "
		  (let ((out (write-to-string (selector combination) 40)))
		    (if (car out)
			(string-append (cdr out) "...")
			(cdr out)))
		  "\nwithin procedure")
   (combination-operator* combination)))

;;;; Combination Operations

;;; Combinations coming out of the continuation parser are either all
;;; unevaluated, or all evaluated, or all operands evaluated and the
;;; operator undefined.  Thus we must be careful about unwrapping
;;; the components when necessary.  In practice, it turns out that
;;; all but one of the interesting errors happen at the application
;;; point, at which all of the combination's components are evaluated.

(define (combination-operator* combination)
  (unwrap-evaluated-object (combination-operator combination)))

(define ((combination-operand selector) combination)
  (unwrap-evaluated-object (selector (combination-operands combination))))

(define combination-first-operand (combination-operand first))
(define combination-second-operand (combination-operand second))
(define combination-third-operand (combination-operand third))

(define (combination-operands* combination)
  (map unwrap-evaluated-object (combination-operands combination)))

(define (unwrap-evaluated-object object)
  (if (continuation-evaluated-object? object)
      (continuation-evaluated-object-value object)
      (error "Not evaluated -- get a wizard" unwrap-evaluated-object object)))

;;;; Environment Operation Errors

(define define-unbound-variable-error
  (define-specific-error 'UNBOUND-VARIABLE
    "Unbound Variable"))

(define-unbound-variable-error variable? variable-name)
(define-unbound-variable-error access? access-name)
(define-unbound-variable-error assignment? assignment-name)
(define-unbound-variable-error
  (list (make-primitive-procedure 'LEXICAL-REFERENCE)
	(make-primitive-procedure 'LEXICAL-ASSIGNMENT))
  combination-second-operand)

(define-unbound-variable-error
  (list (make-primitive-procedure 'ADD-FLUID-BINDING! true))
  (lambda (obj)
    (let ((object (combination-second-operand obj)))
      (cond ((variable? object) (variable-name object))
	    ((symbol? object) object)
	    (else (error "Handler has bad object -- GET-A-WIZARD" object))))))

(define define-unassigned-variable-error
  (define-specific-error 'UNASSIGNED-VARIABLE
    "Unassigned Variable"))

(define-unassigned-variable-error variable? variable-name)
(define-unassigned-variable-error access? access-name)
(define-unassigned-variable-error
  (list (make-primitive-procedure 'LEXICAL-REFERENCE))
  combination-second-operand)

(define define-bad-frame-error
  (define-specific-error 'BAD-FRAME
    "Illegal Environment Frame"))

(define-bad-frame-error access? access-environment)
(define-bad-frame-error in-package? in-package-environment)

#|
(define define-assignment-to-procedure-error
  (define-specific-error 'ASSIGN-LAMBDA-NAME
    "Attempt to assign procedure's name"))

(define-assignment-to-procedure-error assignment? assignment-name)
(define-assignment-to-procedure-error definition? definition-name)
(define-assignment-to-procedure-error
  (list (make-primitive-procedure 'LEXICAL-ASSIGNMENT)
	(make-primitive-procedure 'LOCAL-ASSIGNMENT)
	(make-primitive-procedure 'ADD-FLUID-BINDING! true)
	(make-primitive-procedure 'MAKE-FLUID-BINDING! true))
  combination-second-operand)
|#

;;;; Application Errors

(define-operator-error 'UNDEFINED-PROCEDURE
  "Application of Non-Procedure Object")

(define-operator-error 'UNDEFINED-PRIMITIVE-OPERATION
  "Undefined Primitive Procedure")

(define-operator-error 'UNIMPLEMENTED-PRIMITIVE
  "Unimplemented Primitive Procedure")

(define-operand-error 'WRONG-NUMBER-OF-ARGUMENTS
  "Wrong Number of Arguments"
  (lambda (combination)
    (length (combination-operands* combination))))

(let ((make
       (lambda (wta-error-code bra-error-code position-string
			       position-selector)
	 (let ((ap-string (string-append position-string " argument position"))
	       (selector (combination-operand position-selector)))
	   (define-operand-error wta-error-code
	     (string-append "Illegal datum in " ap-string)
	     selector)
	   (define-operand-error bra-error-code
	     (string-append "Datum out of range in " ap-string)
	     selector)))))
  (make 'WRONG-TYPE-ARGUMENT-0 'BAD-RANGE-ARGUMENT-0 "first" first)
  (make 'WRONG-TYPE-ARGUMENT-1 'BAD-RANGE-ARGUMENT-1 "second" second)
  (make 'WRONG-TYPE-ARGUMENT-2 'BAD-RANGE-ARGUMENT-2 "third" third)
  (make 'WRONG-TYPE-ARGUMENT-3 'BAD-RANGE-ARGUMENT-3 "fourth" fourth)
  (make 'WRONG-TYPE-ARGUMENT-4 'BAD-RANGE-ARGUMENT-4 "fifth" fifth)
  (make 'WRONG-TYPE-ARGUMENT-5 'BAD-RANGE-ARGUMENT-5 "sixth" sixth)
  (make 'WRONG-TYPE-ARGUMENT-6 'BAD-RANGE-ARGUMENT-6 "seventh" seventh)
  (make 'WRONG-TYPE-ARGUMENT-7 'BAD-RANGE-ARGUMENT-7 "eighth" eighth)
  (make 'WRONG-TYPE-ARGUMENT-8 'BAD-RANGE-ARGUMENT-8
	"ninth" (lambda (list) (general-car-cdr list #x1400)))
  (make 'WRONG-TYPE-ARGUMENT-9 'BAD-RANGE-ARGUMENT-9
	"tenth" (lambda (list) (general-car-cdr list #x3000))))

(define-operand-error 'FAILED-ARG-1-COERCION
  "Argument 1 cannot be coerced to floating point"
  combination-first-operand)

(define-operand-error 'FAILED-ARG-2-COERCION
  "Argument 2 cannot be coerced to floating point"
  combination-second-operand)

;;;; Primitive Operator Errors

(define-operation-specific-error 'FASL-FILE-TOO-BIG
  (list (make-primitive-procedure 'BINARY-FASLOAD))
  "Not enough room to Fasload"
  combination-first-operand)

(define-operation-specific-error 'FASL-FILE-BAD-DATA
  (list (make-primitive-procedure 'BINARY-FASLOAD))
  "Fasload file would not relocate correctly"
  combination-first-operand)

#|
(define-operation-specific-error 'RAN-OUT-OF-HASH-NUMBERS
  (list (make-primitive-procedure 'OBJECT-HASH))
  "Hashed too many objects -- get a wizard"
  combination-first-operand)
|#

;;; This will trap any external-primitive errors that
;;; aren't caught by special handlers.

(define-operator-error 'EXTERNAL-RETURN
  "Error during External Application")

(define-operation-specific-error 'EXTERNAL-RETURN
  (list (make-primitive-procedure 'FILE-OPEN-CHANNEL))
  "Unable to open file"
  combination-first-operand)

(define-operation-specific-error 'OUT-OF-FILE-HANDLES
  (list (make-primitive-procedure 'FILE-OPEN-CHANNEL))
  "Too many open files"
  combination-first-operand)

;;;; SCODE Syntax Errors

;;; This error gets an unevaluated combination, but it doesn't ever
;;; look at the components, so it doesn't matter.

(define define-broken-variable-error
  (define-specific-error 'BROKEN-CVARIABLE
    "Broken Compiled Variable -- get a wizard"))

(define-broken-variable-error variable? variable-name)
(define-broken-variable-error assignment? assignment-name)

;;;; System Errors

(define-total-error-handler 'BAD-ERROR-CODE
  (lambda (error-code)
    (start-error-rep "Bad Error Code -- get a wizard"
		     (error-code-or-name error-code))))

(define-default-error 'BAD-INTERRUPT-CODE
  "Illegal Interrupt Code -- get a wizard"
  identity-procedure)

(define-default-error 'EXECUTE-MANIFEST-VECTOR
  "Attempt to execute Manifest Vector -- get a wizard"
  identity-procedure)

(define-total-error-handler 'WRITE-INTO-PURE-SPACE
  (lambda (error-code)
    (newline)
    (write-string "Automagically IMPURIFYing an object....")
    (impurify (combination-first-operand
	       (continuation-expression (rep-continuation))))))
 
(define-default-error 'UNDEFINED-USER-TYPE
  "Undefined Type Code -- get a wizard"
  identity-procedure)

(define-default-error 'INAPPLICABLE-CONTINUATION
  "Inapplicable continuation -- get a wizard"
  identity-procedure)

(define-default-error 'COMPILED-CODE-ERROR
  "Compiled code error -- get a wizard"
  identity-procedure)

(define-default-error 'FLOATING-OVERFLOW
  "Floating point overflow"
  identity-procedure)

;;; end ERROR-SYSTEM package.
))
))