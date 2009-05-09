#| -*-Scheme-*-

$Id: rules2.scm,v 4.20 2008/01/30 20:01:54 cph Exp $
$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

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

;;;; LAP Generation Rules: Predicates

(declare (usual-integrations))

(define-rule predicate
  ;; test for two registers EQ?
  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
  (compare '= (standard-source! source1) (standard-source! source2)))

(define-rule predicate
  (EQ-TEST (MACHINE-CONSTANT 0) (REGISTER (? register)))
  (compare-immediate '= 0 (standard-source! register)))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (MACHINE-CONSTANT 0))
  (compare-immediate '= 0 (standard-source! register)))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (eq-test/constant*register constant register))

(define-rule predicate
  ;; test for register EQ? to constant
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (eq-test/constant*register constant register))

(define (eq-test/constant*register constant source)
  (let ((source (standard-source! source)))
    (if (non-pointer-object? constant)
	(compare-immediate '= (non-pointer->literal constant) source)
	(let ((temp (standard-temporary!)))
	  (LAP ,@(load-constant constant temp)
	       ,@(compare '= temp source))))))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? register)))
  (eq-test/synthesized-constant*register type datum register))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (REGISTER (? register))
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (eq-test/synthesized-constant*register type datum register))

(define (eq-test/synthesized-constant*register type datum source)
  (compare-immediate '=
		     (make-non-pointer-literal type datum)
		     (standard-source! source)))

(define-rule predicate
  ;; Branch if virtual register contains the specified type number
  (TYPE-TEST (REGISTER (? register)) (? type))
  (compare-immediate '= type (standard-source! register)))


;; Combine tests for fixnum and non-negative by extracting the type
;; bits and the sign bit.

(define-rule predicate
  (PRED-1-ARG INDEX-FIXNUM?
	      (REGISTER (? source)))
  (let ((src (standard-source! source)))
    (let ((temp (standard-temporary!)))
      (LAP (EXTRU () ,src ,(- scheme-type-width 0) ,(+ scheme-type-width 1)
		  ,temp)
	   ,@(compare-immediate '= (* 2 (ucode-type fixnum)) temp)))))
