#| -*-Scheme-*-

$Id: rules4.scm,v 1.1 1995/01/10 20:53:06 adams Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Interpreter Calls
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Variable cache trap handling.

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont) (? extension) (? safe?))
  (QUALIFIER (interpreter-call-argument? extension))
  cont					; ignored
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension edx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 #|
	 ,@(invoke-interface/call
	    (if safe?
		code:compiler-safe-reference-trap
		code:compiler-reference-trap))
	 |#
	 ,@(invoke-hook/call (if safe?
				 entry:compiler-safe-reference-trap
				 entry:compiler-reference-trap)))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont) (? extension) (? value))
  (QUALIFIER (and (interpreter-call-argument? extension)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (let* ((set-extension
	  (interpreter-call-argument->machine-register! extension edx))
	 (set-value (interpreter-call-argument->machine-register! value ebx)))
    (LAP ,@set-extension
	 ,@set-value
	 ,@(clear-map!)
	 #|
	 ,@(invoke-interface/call code:compiler-assignment-trap)
	 |#
	 ,@(invoke-hook/call entry:compiler-assignment-trap))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  cont					; ignored
  (let ((set-extension
	 (interpreter-call-argument->machine-register! extension edx)))
    (LAP ,@set-extension
	 ,@(clear-map!)
	 ,@(invoke-interface/call code:compiler-unassigned?-trap))))

;;;; Interpreter Calls

;;; All the code that follows is obsolete.  It hasn't been used in a while.
;;; It is provided in case the relevant switches are turned off, but there
;;; is no real reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call code:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont) (? environment) (? name) (? safe?))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call (if safe? code:compiler-safe-lookup code:compiler-lookup)
	       environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call code:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? cont) (? environment) (? name))
  (QUALIFIER (interpreter-call-argument? environment))
  cont					; ignored
  (lookup-call code:compiler-unbound? environment name))

(define (lookup-call code environment name)
  (let ((set-environment
	  (interpreter-call-argument->machine-register! environment edx)))
    (LAP ,@set-environment
	 ,@(clear-map (clear-map!))
	 ,@(load-constant (INST-EA (R ,ebx)) name)
	 ,@(invoke-interface/call code))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (assignment-call code:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont) (? environment) (? name) (? value))
  (QUALIFIER (and (interpreter-call-argument? environment)
		  (interpreter-call-argument? value)))
  cont					; ignored
  (assignment-call code:compiler-set! environment name value))

(define (assignment-call code environment name value)
  (let* ((set-environment
	  (interpreter-call-argument->machine-register! environment edx))
	 (set-value (interpreter-call-argument->machine-register! value eax)))
    (LAP ,@set-environment
	 ,@set-value
	 ,@(clear-map!)
	 (MOV W ,reg:utility-arg-4 (R ,eax))
	 ,@(load-constant (INST-EA (R ,ebx)) name)
	 ,@(invoke-interface/call code))))