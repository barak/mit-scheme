#| -*-Scheme-*-

$Id: rules4.scm,v 1.3 2002/11/20 19:45:50 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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