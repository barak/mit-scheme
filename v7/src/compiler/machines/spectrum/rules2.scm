#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/rules2.scm,v 4.13 1992/02/18 16:09:39 jinx Exp $
$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

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