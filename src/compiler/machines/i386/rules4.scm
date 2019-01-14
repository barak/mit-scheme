#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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