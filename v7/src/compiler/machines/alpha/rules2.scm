#| -*-Scheme-*-

$Id: rules2.scm,v 1.1 1992/08/29 13:51:31 jinx Exp $

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

;;;; LAP Generation Rules: Predicates
;; Package: (compiler lap-syntaxer)
;; Syntax: lap-generator-syntax-table

(declare (usual-integrations))

(define-rule predicate
  ;; test for two registers EQ?
  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
  (compare '= (standard-source! source1) (standard-source! source2)))

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
	  (LAP ,@(load-pc-relative temp
				   'CONSTANT (constant->label constant))
	       ,@(compare '= temp source))))))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			     (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? register)))
  (eq-test/synthesized-constant*register type datum register))

(define-rule predicate
  ;; test for register EQ? to synthesized constant
  (EQ-TEST (REGISTER (? register))
	   (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
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