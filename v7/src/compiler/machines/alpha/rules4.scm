#| -*-Scheme-*-

$Id: rules4.scm,v 1.1 1992/08/29 13:51:32 jinx Exp $

Copyright (c) 1992 Digital Equipment Corporation (D.E.C.)

This software was developed at the Digital Equipment Corporation
Cambridge Research Laboratory.  Permission to copy this software, to
redistribute it, and to use it for any purpose is granted, subject to
the following restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to both the Digital Equipment Corporation Cambridge Research
Lab (CRL) and the MIT Scheme project any improvements or extensions
that they make, so that these may be included in future releases; and
(b) to inform CRL and MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. D.E.C. has made no warrantee or representation that the operation
of this software will be error-free, and D.E.C. is under no obligation
to provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Digital Equipment Corporation
nor of any adaptation thereof in any advertising, promotional, or
sales literature without prior written consent from D.E.C. in each
case.

|#

;;;; LAP Generation Rules: Interpreter Calls
;; Package: (compiler lap-syntaxer)
;; Syntax: lap-generator-syntax-table

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
       ,@(load-constant regnum:third-arg name #F)
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