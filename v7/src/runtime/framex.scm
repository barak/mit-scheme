#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/framex.scm,v 14.9 1989/07/13 18:38:41 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Debugging Info
;;; package: (runtime debugging-info)

(declare (usual-integrations))

(define (debugging-info/undefined-expression? expression)
  (or (eq? expression undefined-expression)
      (debugging-info/noise? expression)))

(define (debugging-info/noise? expression)
  (and (pair? expression)
       (eq? (car expression) undefined-expression)))

(define-integrable (debugging-info/noise expression)
  (cdr expression))

(define-integrable (make-debugging-info/noise noise)
  (cons undefined-expression noise))

(define-integrable (debugging-info/undefined-environment? environment)
  (eq? environment undefined-environment))

(define-integrable (debugging-info/compiled-code? expression)
  (eq? expression compiled-code))

(define (stack-frame/debugging-info frame)
  (let ((method
	 (1d-table/get (stack-frame-type/properties (stack-frame/type frame))
		       method-tag
		       false)))
    (if (not method)
	;; (error "STACK-FRAME/DEBUGGING-INFO: missing method" frame)
	(values (make-debugging-info/noise
		 (lambda (long?)
		   (with-output-to-string
		     (lambda ()
		       (display "Unknown (methodless) ")
		       (if long?
			   (pp frame)
			   (write frame))))))
		undefined-environment)
	(method frame))))

(define (make-evaluated-object object)
  (if (scode-constant? object)
      object
      (cons evaluated-object-tag object)))

(define (debugging-info/evaluated-object? expression)
  (and (pair? expression)
       (eq? (car expression) evaluated-object-tag)))

(define-integrable (debugging-info/evaluated-object-value expression)
  (cdr expression))

(define method-tag "stack-frame/debugging-info method")
(define undefined-expression "undefined expression")
(define undefined-environment "undefined environment")
(define compiled-code "compiled code")
(define evaluated-object-tag "evaluated")

(define (method/standard frame)
  (values (stack-frame/ref frame 1) (stack-frame/ref frame 2)))

(define (method/null frame)
  frame
  (values undefined-expression undefined-environment))

(define (method/expression-only frame)
  (values (stack-frame/ref frame 1) undefined-environment))

(define (method/environment-only frame)
  (values undefined-expression (stack-frame/ref frame 2)))

(define (method/compiled-code frame)
  (values
   (let ((continuation
	  (compiled-entry/dbg-object (stack-frame/return-address frame)))
	 (lose (lambda () compiled-code)))
     (if continuation
	 (let ((source-code (dbg-continuation/source-code continuation)))
	   (if (and (vector? source-code)
		    (not (zero? (vector-length source-code))))
	       (case (vector-ref source-code 0)
		 ((SEQUENCE-2-SECOND
		   SEQUENCE-3-SECOND
		   SEQUENCE-3-THIRD
		   CONDITIONAL-DECIDE
		   ASSIGNMENT-CONTINUE
		   DEFINITION-CONTINUE
		   COMBINATION-OPERAND)
		  (vector-ref source-code 1))
		 (else
		  (lose)))
	       (lose)))
	 (lose)))
   (stack-frame/environment frame undefined-environment)))

(define (method/primitive-combination-3-first-operand frame)
  (values (stack-frame/ref frame 1) (stack-frame/ref frame 3)))

(define (method/force-snap-thunk frame)
  (values (%make-combination
	   (ucode-primitive force 1)
	   (list (make-evaluated-object (stack-frame/ref frame 1))))
	  undefined-environment))

(define ((method/application-frame index) frame)
  (values (%make-combination
	   (make-evaluated-object (stack-frame/ref frame index))
	   (stack-frame-list frame (1+ index)))
	  undefined-environment))

(define ((method/compiler-reference scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 3))
	  (stack-frame/ref frame 2)))

(define ((method/compiler-assignment scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 3)
		       (make-evaluated-object (stack-frame/ref frame 4)))
	  (stack-frame/ref frame 2)))

(define ((method/compiler-reference-trap scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 2))
	  (stack-frame/ref frame 3)))

(define ((method/compiler-assignment-trap scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 2)
		       (make-evaluated-object (stack-frame/ref frame 4)))
	  (stack-frame/ref frame 3)))

(define (method/compiler-lookup-apply-restart frame)
  (values (%make-combination (stack-frame/ref frame 3)
			     (stack-frame-list frame 5))
	  undefined-environment))

(define (method/compiler-lookup-apply-trap-restart frame)
  (values (%make-combination (make-variable (stack-frame/ref frame 2))
			     (stack-frame-list frame 6))
	  (stack-frame/ref frame 3)))

(define (stack-frame-list frame start)
  (let ((end (stack-frame/length frame)))
    (let loop ((index start))
      (if (< index end)
	  (cons (make-evaluated-object (stack-frame/ref frame index))
		(loop (1+ index)))
	  '()))))

(define (method/hardware-trap frame)
  (values (make-debugging-info/noise (hardware-trap-noise frame))
	  undefined-environment))

(define ((hardware-trap-noise frame) long?)
  (with-output-to-string
    (lambda ()
      (hardware-trap-frame/describe frame long?))))

(define (initialize-package!)
  (for-each (lambda (entry)
	      (for-each (lambda (name)
			  (let ((type
				 (or (microcode-return/code->type
				      (microcode-return name))
				     (error "Missing return type" name))))
			    (1d-table/put! (stack-frame-type/properties type)
					   method-tag
					   (car entry))))
			(cdr entry)))
	  `((,method/standard
	     ASSIGNMENT-CONTINUE
	     COMBINATION-1-PROCEDURE
	     COMBINATION-2-FIRST-OPERAND
	     COMBINATION-2-PROCEDURE
	     COMBINATION-SAVE-VALUE
	     CONDITIONAL-DECIDE
	     DEFINITION-CONTINUE
	     DISJUNCTION-DECIDE
	     EVAL-ERROR
	     PRIMITIVE-COMBINATION-2-FIRST-OPERAND
	     PRIMITIVE-COMBINATION-3-SECOND-OPERAND
	     SEQUENCE-2-SECOND
	     SEQUENCE-3-SECOND
	     SEQUENCE-3-THIRD)

	    (,method/null
	     COMBINATION-APPLY
	     GC-CHECK
	     MOVE-TO-ADJACENT-POINT
	     REENTER-COMPILED-CODE)

	    (,method/expression-only
	     ACCESS-CONTINUE
	     IN-PACKAGE-CONTINUE
	     PRIMITIVE-COMBINATION-1-APPLY
	     PRIMITIVE-COMBINATION-2-APPLY
	     PRIMITIVE-COMBINATION-3-APPLY)

	    (,method/environment-only
	     REPEAT-DISPATCH)

	    (,method/primitive-combination-3-first-operand
	     PRIMITIVE-COMBINATION-3-FIRST-OPERAND)

	    (,method/force-snap-thunk
	     FORCE-SNAP-THUNK)

	    (,(method/application-frame 3)
	     INTERNAL-APPLY)

	    (,(method/application-frame 1)
	     REPEAT-PRIMITIVE)

	    (,(method/compiler-reference identity-procedure)
	     COMPILER-REFERENCE-RESTART
	     COMPILER-SAFE-REFERENCE-RESTART)

	    (,(method/compiler-reference make-variable)
	     COMPILER-ACCESS-RESTART)

	    (,(method/compiler-reference make-unassigned?)
	     COMPILER-UNASSIGNED?-RESTART)

	    (,(method/compiler-reference
	       (lambda (name)
		 (%make-combination (ucode-primitive lexical-unbound?)
				    (list (make-the-environment) name))))
	     COMPILER-UNBOUND?-RESTART)

	    (,(method/compiler-assignment make-assignment-from-variable)
	     COMPILER-ASSIGNMENT-RESTART)

	    (,(method/compiler-assignment make-definition)
	     COMPILER-DEFINITION-RESTART)

	    (,(method/compiler-reference-trap make-variable)
	     COMPILER-REFERENCE-TRAP-RESTART
	     COMPILER-SAFE-REFERENCE-TRAP-RESTART)

	    (,(method/compiler-reference-trap make-unassigned?)
	     COMPILER-UNASSIGNED?-TRAP-RESTART)

	    (,(method/compiler-assignment-trap make-assignment)
	     COMPILER-ASSIGNMENT-TRAP-RESTART)

	    (,method/compiler-lookup-apply-restart
	     COMPILER-LOOKUP-APPLY-RESTART)

	    (,method/compiler-lookup-apply-trap-restart
	     COMPILER-LOOKUP-APPLY-TRAP-RESTART
	     COMPILER-OPERATOR-LOOKUP-TRAP-RESTART)

	    (,method/hardware-trap
	     HARDWARE-TRAP)))
  (1d-table/put!
   (stack-frame-type/properties stack-frame-type/compiled-return-address)
   method-tag
   method/compiled-code))