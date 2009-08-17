#| -*-Scheme-*-

$Id: db0c94348987cef6a0be88090ff05d0c0bcbc439 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Interpreter Calls
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Variable cache trap handling.

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? cont)
				    (REGISTER (? extension))
				    (? safe?))
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-2 (if safe?
				    code:compiler-safe-reference-trap
				    code:compiler-reference-trap)
				(c:cptr (label->offset cont))
				extension))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont)
				     (REGISTER (? extension))
				     (REGISTER (? value)))
  (let ((value (standard-source! value 'SCHEME_OBJECT))
	(extension (standard-source! extension 'SCHEME_OBJECT*)))
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-3 code:compiler-assignment-trap
				(c:cptr (label->offset cont))
				extension
				value))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont)
				      (REGISTER (? extension)))
  (let ((extension (standard-source! extension 'SCHEME_OBJECT*)))
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-2 code:compiler-unassigned?-trap
				(c:cref (label->offset cont))
				extension))))

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
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-3 code
				(c:cptr (label->offset cont))
				environment
				(c:cref (object->offset name))))))

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
    (LAP ,@(clear-map!)
	 ,(c:invoke-interface-4 code
				(c:cptr (label->offset cont))
				environment
				(c:cref (object->offset name))
				value))))