#| -*-Scheme-*-

$Id: rules4.scm,v 1.1 1994/11/19 02:08:04 adams Exp $

Copyright (c) 1988-1994 Massachusetts Institute of Technology

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

(define *interpreter-call-clobbered-regs*
  ;; g26 g25 g24 g23 used for argument passing, already cleared
  ;; SRA - dont think so?
  (list g31 g2 g28 g29   g26 g25 g24 g23))

(define (interpreter-call code extension extra)
  (let ((start (%load-interface-args! false extension extra false)))
    (LAP (COMMENT >> %interface-load-args)
	 ,@start
	 (COMMENT << %interface-load-args)
	 ,@(preserving-regs
	    *interpreter-call-clobbered-regs*
	    (lambda (gen-preservation-info)
	      (if (not gen-preservation-info)
		  (invoke-interface-ble code)
		  (let ((label1 (generate-label))
			(label2 (generate-label)))
		    (LAP (LDI () ,code ,g28)
			 (BLE () (OFFSET ,hook:compiler-interpreter-call 4
					 ,regnum:scheme-to-interface-ble))
			 (LDO ()
			      (OFFSET (- (- ,label2 ,label1)
					 ,*privilege-level*)
				      0 31)
			      31)
			 (LABEL ,label1)
			 ,@(gen-preservation-info)
			 (LABEL ,label2)))))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont)
				    (REGISTER (? extension))
				    (? safe?))
  cont					; ignored
  (interpreter-call (if safe?
			code:compiler-safe-reference-trap
			code:compiler-reference-trap)
		    extension false))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont)
				     (REGISTER (? extension))
				     (? value register-expression))
  cont					; ignored
  (interpreter-call code:compiler-assignment-trap extension value))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont)
				      (REGISTER (? extension)))
  cont					; ignored
  (interpreter-call code:compiler-unassigned?-trap extension false))

;;;; Interpreter Calls

;;; All the code that follows is obsolete.  It hasn't been used in a while.
;;; It is provided in case the relevant switches are turned off, but there
;;; is no real reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont)
			   (? environment register-expression)
			   (? name))
  cont					; ignored
  (lookup-call code:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont)
			   (? environment register-expression)
			   (? name)
			   (? safe?))
  cont					; ignored
  (lookup-call (if safe? code:compiler-safe-lookup code:compiler-lookup)
	       environment
	       name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont)
				(? environment register-expression)
				(? name))
  cont					; ignored
  (lookup-call code:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? cont)
			     (? environment register-expression)
			     (? name))
  cont					; ignored
  (lookup-call code:compiler-unbound? environment name))

(define (lookup-call code environment name)
  (LAP ,@(load-interface-args! false environment false false)
       ,@(load-constant name regnum:third-arg)
       ,@(invoke-interface-ble code)))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont)
			   (? environment register-expression)
			   (? name)
			   (? value register-expression))
  cont					; ignored
  (assignment-call code:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont)
			 (? environment register-expression)
			 (? name)
			 (? value register-expression))
  cont					; ignored
  (assignment-call code:compiler-set! environment name value))

(define (assignment-call code environment name value)
  (LAP ,@(load-interface-args! false environment false value)
       ,@(load-constant name regnum:third-arg)
       ,@(invoke-interface-ble code)))