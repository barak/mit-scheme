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
  (define (get-argument value register)
    (interpreter-call-argument->machine-register! value register))
  (let ((set-extension (get-argument extension regnum:utility-arg1)))
    (LAP ,@set-extension
         ,@(clear-map!)
         #|
         ,@(invoke-interface/call
            (if safe?
                code:compiler-safe-reference-trap
                code:compiler-reference-trap)
            cont)
         |#
         ,@(invoke-hook/call
            (if safe?
                entry:compiler-safe-reference-trap
                entry:compiler-reference-trap)
            cont))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? cont) (? extension) (? value))
  (QUALIFIER (and (interpreter-call-argument? extension)
                  (interpreter-call-argument? value)))
  (define (get-argument value register)
    (interpreter-call-argument->machine-register! value register))
  (let* ((set-extension (get-argument extension regnum:utility-arg1))
         (set-value (get-argument extension regnum:utility-arg2)))
    (LAP ,@set-extension
         ,@set-value
         ,@(clear-map!)
         #|
         ,@(invoke-interface/call code:compiler-assignment-trap cont)
         |#
         ,@(invoke-hook/call entry:compiler-assignment-trap cont))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? cont) (? extension))
  (QUALIFIER (interpreter-call-argument? extension))
  (define (get-argument value register)
    (interpreter-call-argument->machine-register! value register))
  (let ((set-extension (get-argument extension regnum:utility-arg1)))
    (LAP ,@set-extension
         ,@(clear-map!)
         ,@(invoke-interface/call code:compiler-unassigned?-trap cont))))

;;; Obsolete interpreter calls, should be flushed.

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? cont) (? environment) (? name))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:ACCESS ,cont ,environment ,name)))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? cont) (? environment) (? name) (? safe?))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:LOOKUP ,cont ,environment ,name ,safe?)))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? cont) (? environment) (? name))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:UNASSIGNED? ,cont ,environment ,name)))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? cont) (? environment) (? name))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:UNBOUND? ,cont ,environment ,name)))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? cont) (? environment) (? name) (? value))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:DEFINE ,cont ,environment ,name ,value)))

(define-rule statement
  (INTERPRETER-CALL:SET! (? cont) (? environment) (? name) (? value))
  (error "Unsupported interpreter call:"
         `(INTERPRETER-CALL:SET! ,cont ,environment ,name ,value)))
