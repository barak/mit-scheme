#| -*-Scheme-*-

$Id: rules4.scm,v 1.1 1993/06/08 06:13:32 gjr Exp $

Copyright (c) 1992-1993 Massachusetts Institute of Technology

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
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont)
				    (REGISTER (? extension))
				    (? safe?))
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (use-invoke-interface! 2)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_2 ("
	 ,(if safe?
	      code:compiler-safe-reference-trap
	      code:compiler-reference-trap)
	 ", &current_block[" ,(label->offset cont) "], "
	 ,extension ");\n\t")))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont)
				     (REGISTER (? extension))
				     (REGISTER (? value)))
  (let ((value (standard-source! value 'SCHEME_OBJECT))
	(extension (standard-source! extension 'SCHEME_OBJECT*)))
    (use-invoke-interface! 3)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_3 ("
	 ,code:compiler-assignment-trap
	 ", &current_block[" ,(label->offset cont) "], "
	 ,extension
	 ", " ,value ");\n\t")))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont)
				      (REGISTER (? extension)))
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (use-invoke-interface! 2)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_2 (" ,code:compiler-unassigned?-trap
	 ", &current_block[" ,(label->offset cont) "], "
	 ,extension ");\n\t")))

;;;; Interpreter Calls

;;; All the code that follows is obsolete.  It hasn't been used in a while.
;;; It is provided in case the relevant switches are turned off, but there
;;; is no real reason to do this.  Perhaps the switches should be removed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont)
			   (REGISTER (? environment))
			   (? name))
  (lookup-call code:compiler-access cont environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont)
			   (REGISTER (? environment))
			   (? name)
			   (? safe?))
  (lookup-call (if safe? code:compiler-safe-lookup code:compiler-lookup)
	       cont
	       environment
	       name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont)
				(REGISTER (? environment))
				(? name))
  (lookup-call code:compiler-unassigned? cont environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (REGISTER (? environment)) (? name))
  (lookup-call code:compiler-unbound? environment name))

(define (lookup-call code cont environment name)
  (let ((environment (standard-source! environment 'SCHEME_OBJECT)))
    (use-invoke-interface! 3)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_3 (" ,code
	 ", &current_block[" ,(label->offset cont) "], "
	 ,environment ", "
	 "current_block[" ,(object->offset name) "]);\n\t")))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont)
			   (REGISTER (? environment))
			   (? name)
			   (REGISTER (? value)))
  (assignment-call code:compiler-define cont environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont)
			 (REGISTER (? environment))
			 (? name)
			 (REGISTER (? value)))
  (assignment-call code:compiler-set! cont environment name value))

(define (assignment-call code cont environment name value)
  (let ((environment (standard-source! environment 'SCHEME_OBJECT))
	(value (standard-source! value 'SCHEME_OBJECT)))
    (use-invoke-interface! 4)
    (LAP ,@(clear-map!)
	 "INVOKE_INTERFACE_4 (" ,code
	 ", &current_block[" ,(label->offset cont) "], "
	 ,environment ", "
	 "current_block[" ,(object->offset name) "], " ,value ");\n\t")))