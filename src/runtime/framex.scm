#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Debugging Info
;;; package: (runtime debugging-info)

(declare (usual-integrations))

(define (stack-frame/debugging-info frame)
  (let ((method
	 (stack-frame-type/debugging-info-method (stack-frame/type frame))))
    (if (not method)
	;; (error "STACK-FRAME/DEBUGGING-INFO: missing method" frame)
	(stack-frame/debugging-info/default frame)
	(method frame))))

(define (stack-frame/debugging-info/default frame)
  (values (make-debugging-info/noise
	   (lambda (long?)
	     (with-output-to-string
	       (lambda ()
		 (display "Unknown (methodless) ")
		 (if long?
		     (pp frame)
		     (write frame))))))
	  undefined-environment
	  undefined-expression))

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

(define-integrable (debugging-info/unknown-expression? expression)
  (eq? expression unknown-expression))

(define-integrable (debugging-info/compiled-code? expression)
  (eq? expression compiled-code))

(define (make-evaluated-object object)
  (if (scode-constant? object)
      object
      (cons evaluated-object-tag object)))

(define (debugging-info/evaluated-object? expression)
  (and (pair? expression)
       (eq? (car expression) evaluated-object-tag)))

(define-integrable (debugging-info/evaluated-object-value expression)
  (cdr expression))

(define (validate-subexpression frame subexpression)
  (if (eq? (stack-frame/previous-type frame) stack-frame-type/pop-return-error)
      undefined-expression
      subexpression))

(define undefined-expression "undefined expression")
(define undefined-environment "undefined environment")
(define unknown-expression "unknown expression")
(define compiled-code "compiled code")
(define evaluated-object-tag "evaluated")
(define stack-frame-type/pop-return-error)

(define (method/null frame)
  frame
  (values undefined-expression undefined-environment undefined-expression))

(define (method/environment-only frame)
  (values undefined-expression (stack-frame/ref frame 2) undefined-expression))

(define ((method/standard select-subexpression) frame)
  (let ((expression (stack-frame/ref frame 1)))
    (values expression
	    (stack-frame/ref frame 2)
	    (validate-subexpression frame (select-subexpression expression)))))

(define ((method/expression-only select-subexpression) frame)
  (let ((expression (stack-frame/ref frame 1)))
    (values expression
	    undefined-environment
	    (validate-subexpression frame (select-subexpression expression)))))

(define (method/primitive-combination-3-first-operand frame)
  (let ((expression (stack-frame/ref frame 1)))
    (values expression
	    (stack-frame/ref frame 3)
	    (validate-subexpression frame (&vector-ref expression 2)))))

(define (method/combination-save-value frame)
  (let ((expression (stack-frame/ref frame 1)))
    (values expression
	    (stack-frame/ref frame 2)
	    (validate-subexpression
	     frame
	     (&vector-ref expression (stack-frame/ref frame 3))))))

(define (method/eval-error frame)
  (values (stack-frame/ref frame 1)
	  (stack-frame/ref frame 2)
	  undefined-expression))

(define (method/force-snap-thunk frame)
  (let ((promise (stack-frame/ref frame 1)))
    (values (make-combination (ucode-primitive force 1)
			      (list (make-evaluated-object promise)))
	    undefined-environment
	    (cond ((promise-forced? promise) undefined-expression)
		  ((promise-non-expression? promise) unknown-expression)
		  (else
		   (validate-subexpression frame
					   (promise-expression promise)))))))

(define ((method/application-frame index) frame)
  (values (make-combination
	   (make-evaluated-object (stack-frame/ref frame index))
	   (stack-frame-list frame (1+ index)))
	  undefined-environment
	  undefined-expression))

(define ((method/compiler-reference-trap scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 2))
	  (stack-frame/ref frame 3)
	  undefined-expression))

(define ((method/compiler-assignment-trap scode-maker) frame)
  (values (scode-maker (stack-frame/ref frame 2)
		       (make-evaluated-object (stack-frame/ref frame 4)))
	  (stack-frame/ref frame 3)
	  undefined-expression))

(define (method/compiler-lookup-apply-trap-restart frame)
  (values (make-combination (make-variable (stack-frame/ref frame 2))
			    (stack-frame-list frame 6))
	  (stack-frame/ref frame 3)
	  undefined-expression))

(define (method/compiler-error-restart frame)
  (let ((primitive (stack-frame/ref frame 2)))
    (if (primitive-procedure? primitive)
	(values (make-combination (make-variable 'apply)
				  (list primitive
					unknown-expression))
		undefined-environment
		undefined-expression)
	(stack-frame/debugging-info/default frame))))

(define (stack-frame-list frame start)
  (let ((end (stack-frame/length frame)))
    (let loop ((index start))
      (if (< index end)
	  (cons (make-evaluated-object (stack-frame/ref frame index))
		(loop (1+ index)))
	  '()))))

(define (method/hardware-trap frame)
  (values (make-debugging-info/noise (hardware-trap-noise frame))
	  undefined-environment
	  undefined-expression))

(define ((hardware-trap-noise frame) long?)
  (with-output-to-string
    (lambda ()
      (hardware-trap-frame/describe frame long?))))

(define (method/compiled-code frame)
  (let ((get-environment
	 (lambda ()
	   (stack-frame/environment frame undefined-environment))))
    (let ((object
	   (compiled-entry/dbg-object (stack-frame/return-address frame)))
	  (lose
	   (lambda ()
	     (values compiled-code (get-environment) undefined-expression))))
      (cond ((not object)
	     (lose))
	    ((dbg-continuation? object)
	     (let ((source-code (dbg-continuation/source-code object)))
	       (if (and (vector? source-code)
			(not (zero? (vector-length source-code))))
		   (let* ((expression (vector-ref source-code 1))
			  (win2
			   (lambda (environment subexp)
			     (values expression environment subexp)))
			  (win
			   (lambda (select-subexp)
			     (win2
			      (get-environment)
			      (validate-subexpression
			       frame
			       (select-subexp expression))))))
		     (case (vector-ref source-code 0)
		       ((SEQUENCE-2-SECOND)
			(win &pair-car))
		       ((ASSIGNMENT-CONTINUE
			 DEFINITION-CONTINUE)
			(win &pair-cdr))
		       ((CONDITIONAL-DECIDE)
			(win &triple-first))
		       ((COMBINATION-OPERAND)
			(values
			 expression
			 (get-environment)
			 (validate-subexpression
			  frame
			  (if (zero? (vector-ref source-code 2))
			      (combination-operator expression)
			      (list-ref (combination-operands expression)
					(-1+ (vector-ref source-code 2)))))))
		       ((COMBINATION-ELEMENT)
			(win2 undefined-environment
			      (vector-ref source-code 2)))
		       ((SEQUENCE-ELEMENT)
			(win2 undefined-environment
			      (vector-ref source-code 2)))
		       ((CONDITIONAL-PREDICATE)
			(win2 undefined-environment
			      (vector-ref source-code 2)))
		       (else
			(lose))))
		   (lose))))
	    ((dbg-procedure? object)
	     (values (lambda-body (dbg-procedure/source-code object))
		     (and (dbg-procedure/block object)
			  (get-environment))
		     undefined-expression))
	    #|
	    ((dbg-expression? object)
	     ;; no expression!
	     (lose))
	    |#
	    (else
	     (lose))))))

(define (initialize-package!)
  (set! stack-frame-type/pop-return-error
	(microcode-return/name->type 'POP-RETURN-ERROR))
  (record-method 'COMBINATION-APPLY method/null)
  (record-method 'REENTER-COMPILED-CODE method/null)
  (let ((method (method/standard &pair-car)))
    (record-method 'DISJUNCTION-DECIDE method)
    (record-method 'SEQUENCE-2-SECOND method))
  (let ((method (method/standard &pair-cdr)))
    (record-method 'ASSIGNMENT-CONTINUE method)
    (record-method 'COMBINATION-1-PROCEDURE method)
    (record-method 'DEFINITION-CONTINUE method))
  (let ((method (method/standard &triple-first)))
    (record-method 'CONDITIONAL-DECIDE method))
  (let ((method (method/standard &triple-second)))
    (record-method 'COMBINATION-2-PROCEDURE method))
  (let ((method (method/standard &triple-third)))
    (record-method 'COMBINATION-2-FIRST-OPERAND method)
    (record-method 'PRIMITIVE-COMBINATION-2-FIRST-OPERAND method))
  (record-method 'PRIMITIVE-COMBINATION-3-SECOND-OPERAND
		 (method/standard &vector-fourth))
  (let ((method (method/expression-only &pair-car)))
    (record-method 'ACCESS-CONTINUE method))
  (record-method 'PRIMITIVE-COMBINATION-1-APPLY
		 (method/expression-only &pair-cdr))
  (record-method 'PRIMITIVE-COMBINATION-2-APPLY
		 (method/expression-only &triple-second))
  (record-method 'PRIMITIVE-COMBINATION-3-APPLY
		 (method/expression-only &vector-second))
  (record-method 'COMBINATION-SAVE-VALUE method/combination-save-value)
  (record-method 'PRIMITIVE-COMBINATION-3-FIRST-OPERAND
		 method/primitive-combination-3-first-operand)
  (record-method 'EVAL-ERROR method/eval-error)
  (record-method 'FORCE-SNAP-THUNK method/force-snap-thunk)
  (let ((method (method/application-frame 3)))
    (record-method 'INTERNAL-APPLY method)
    (record-method 'INTERNAL-APPLY-VAL method))
  (let ((method (method/compiler-reference-trap make-variable)))
    (record-method 'COMPILER-REFERENCE-TRAP-RESTART method)
    (record-method 'COMPILER-SAFE-REFERENCE-TRAP-RESTART method))
  (record-method 'COMPILER-UNASSIGNED?-TRAP-RESTART
		 (method/compiler-reference-trap make-unassigned?))
  (record-method 'COMPILER-ASSIGNMENT-TRAP-RESTART
		 (method/compiler-assignment-trap make-assignment))
  (record-method 'COMPILER-LOOKUP-APPLY-TRAP-RESTART
		 method/compiler-lookup-apply-trap-restart)
  (record-method 'COMPILER-OPERATOR-LOOKUP-TRAP-RESTART
		 method/compiler-lookup-apply-trap-restart)
  (record-method 'COMPILER-ERROR-RESTART
		 method/compiler-error-restart)
  (record-method 'HARDWARE-TRAP method/hardware-trap)
  (set-stack-frame-type/debugging-info-method!
   stack-frame-type/compiled-return-address
   method/compiled-code)
  (set-stack-frame-type/debugging-info-method!
   stack-frame-type/interrupt-compiled-procedure
   method/compiled-code)
  (set-stack-frame-type/debugging-info-method!
   stack-frame-type/interrupt-compiled-expression
   method/compiled-code))

(define (&vector-second vector)
  (&vector-ref vector 1))

(define (&vector-fourth vector)
  (&vector-ref vector 3))

(define (record-method name method)
  (set-stack-frame-type/debugging-info-method!
   (microcode-return/name->type name)
   method))

(define-integrable (stack-frame-type/debugging-info-method type)
  (1d-table/get (stack-frame-type/properties type) method-tag false))

(define-integrable (set-stack-frame-type/debugging-info-method! type method)
  (1d-table/put! (stack-frame-type/properties type) method-tag method))

(define method-tag "stack-frame-type/debugging-info-method")