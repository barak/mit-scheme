#| -*-Scheme-*-

$Id: rules2.scm,v 4.16 2002/11/20 19:45:55 cph Exp $
$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

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
