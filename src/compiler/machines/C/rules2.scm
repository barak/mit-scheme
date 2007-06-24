#| -*-Scheme-*-

$Id: rules2.scm,v 1.10 2007/06/18 17:31:04 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Predicates
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule predicate
  ;; test for two registers EQ?
  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
  (%eq-test (standard-source! source1 'SCHEME_OBJECT)
	    (standard-source! source2 'SCHEME_OBJECT)))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? source)))
  (eq-test/constant constant source))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (REGISTER (? source)) (CONSTANT (? constant)))
  (eq-test/constant constant source))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (MACHINE-CONSTANT (? constant)) (REGISTER (? source)))
  (eq-test/machine-constant constant source))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (REGISTER (? source)) (MACHINE-CONSTANT (? constant)))
  (eq-test/machine-constant constant source))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			     (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? source)))
  (eq-test/non-pointer type datum source))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (REGISTER (? source))
	   (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			     (MACHINE-CONSTANT (? datum))))
  (eq-test/non-pointer type datum source))

(define-rule predicate
  ;; Branch if virtual register contains the specified type number
  (TYPE-TEST (REGISTER (? source)) (? type))
  (%eq-test (standard-source! source 'ULONG) type))

(define-rule predicate
  ;; Branch if virtual register contains a legal index fixnum
  (PRED-1-ARG INDEX-FIXNUM? (REGISTER (? source)))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (branch-on-expr (c:ecall "INDEX_FIXNUM_P" source))))

(define (eq-test/constant constant source)
  (%eq-test (standard-source! source 'SCHEME_OBJECT)
	    (c:cref (object->offset constant))))

(define (eq-test/machine-constant constant source)
  (%eq-test (standard-source! source 'SCHEME_OBJECT)
	    (c:cast 'sobj constant)))

(define (eq-test/non-pointer type datum source)
  (%eq-test (standard-source! source 'SCHEME_OBJECT)
	    (c:make-object type datum)))

(define (%eq-test source1 source2)
  (branch-on-expr (c:== source1 source2)))