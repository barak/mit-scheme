#| -*-Scheme-*-

$Id: rules4.scm,v 1.3 2002/11/20 19:45:54 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

(declare (usual-integrations))

;;;; Interpreter Calls

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? environment register-expression) (? name))
  (lookup-call code:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? environment register-expression)
			   (? name)
			   (? safe?))
  (lookup-call (if safe? code:compiler-safe-lookup code:compiler-lookup)
	       environment
	       name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? environment register-expression) (? name))
  (lookup-call code:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? environment register-expression) (? name))
  (lookup-call code:compiler-unbound? environment name))

(define (lookup-call code environment name)
  (LAP ,@(load-interface-args! false environment false false)
       ,@(load-constant regnum:third-arg name #F #F)
       ,@(link-to-interface code)))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment register-expression)
			   (? name)
			   (? value register-expression))
  (assignment-call code:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment register-expression)
			 (? name)
			 (? value register-expression))
  (assignment-call code:compiler-set! environment name value))

(define (assignment-call code environment name value)
  (LAP ,@(load-interface-args! false environment false value)
       ,@(load-constant regnum:third-arg name #F #F)
       ,@(link-to-interface code)))

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (REGISTER (? extension)) (? safe?))
  (LAP ,@(load-interface-args! false extension false false)
       ,@(link-to-interface
	  (if safe?
	      code:compiler-safe-reference-trap
	      code:compiler-reference-trap))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (REGISTER (? extension))
				     (? value register-expression))
  (LAP ,@(load-interface-args! false extension value false)
       ,@(link-to-interface code:compiler-assignment-trap)))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (REGISTER (? extension)))
  (LAP ,@(load-interface-args! false extension false false)
       ,@(link-to-interface code:compiler-unassigned?-trap)))