#| -*-Scheme-*-

$Id: rules4.scm,v 1.4 1999/01/02 06:06:43 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP Generation Rules: Interpreter Calls
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Variable cache trap handling.

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont)
				    (REGISTER (? extension))
				    (? safe?))
  cont					; ignored
  (LAP ,@(load-interface-args! false extension false false)
       ,@(link-to-interface
	  (if safe?
	      code:compiler-safe-reference-trap
	      code:compiler-reference-trap))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont)
				     (REGISTER (? extension))
				     (? value register-expression))
  cont					; ignored
  (LAP ,@(load-interface-args! false extension value false)
       ,@(link-to-interface code:compiler-assignment-trap)))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont)
				      (REGISTER (? extension)))
  cont					; ignored
  (LAP ,@(load-interface-args! false extension false false)
       ,@(link-to-interface code:compiler-unassigned?-trap)))

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
       ,@(load-constant regnum:third-arg name #F #F)
       ,@(link-to-interface code)))

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
       ,@(load-constant regnum:third-arg name #F #F)
       ,@(link-to-interface code)))