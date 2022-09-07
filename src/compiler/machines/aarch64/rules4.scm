#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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
  (INTERPRETER-CALL:CACHE-REFERENCE (? continuation)
                                    (REGISTER (? extension))
                                    (? safe?))
  ;; utility-arg1 will be the return address.
  (let* ((set-extension (load-machine-register! extension regnum:utility-arg2))
         (prefix (clear-map!)))
    (LAP ,@set-extension
         ,@prefix
         ,@(invoke-interface/call
            (if safe?
                code:compiler-safe-reference-trap
                code:compiler-reference-trap)
            continuation))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? continuation)
                                     (REGISTER (? extension))
                                     (REGISTER (? value)))
  ;; utility-arg1 will be the return address.
  (need-registers! (list regnum:utility-arg2 regnum:utility-arg3))
  (let* ((set-extension (load-machine-register! extension regnum:utility-arg2))
         (set-value (load-machine-register! value regnum:utility-arg3))
         (prefix (clear-map!)))
    (LAP ,@set-extension
         ,@set-value
         ,@prefix
         ,@(invoke-interface/call code:compiler-assignment-trap
                                  continuation))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-UNASSIGNED? (? continuation)
                                      (REGISTER (? extension)))
  ;; utility-arg1 will be the return address.
  (let* ((set-extension (load-machine-register! extension regnum:utility-arg2))
         (prefix (clear-map!)))
    (LAP ,@set-extension
         ,@prefix
         ,@(invoke-interface/call code:compiler-unassigned?-trap
                                  continuation))))

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
