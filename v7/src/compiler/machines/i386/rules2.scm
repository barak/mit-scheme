#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rules2.scm,v 1.3 1992/02/13 07:48:34 jinx Exp $
$MC68020-Header: rules2.scm,v 4.12 90/01/18 22:44:04 GMT cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define (set-equal-branches!)
  (set-current-branches! (lambda (label)
			   (LAP (JE (@PCR ,label))))
			 (lambda (label)
			   (LAP (JNE (@PCR ,label))))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (set-equal-branches!)
  (LAP (CMP B ,(reference-alias-register! register) (&U ,type))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (set-equal-branches!)
  (compare/register*register register-1 register-2))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (OFFSET (REGISTER (? address)) (? offset)))
  (set-equal-branches!)
  (LAP (CMP W ,(source-register-reference register)
	    ,(source-indirect-reference! address offset))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? address)) (? offset)) (REGISTER (? register)))
  (set-equal-branches!)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    ,(source-register-reference register))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? register)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (CONSTANT (? constant)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? address)) (? offset)) (CONSTANT (? constant)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (REGISTER (? register)))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register))
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (set-equal-branches!)
  (LAP (CMP W ,(any-reference register)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum)))
	   (OFFSET (REGISTER (? address)) (? offset)))
  (set-equal-branches!)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (OFFSET (REGISTER (? address)) (? offset))
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (set-equal-branches!)
  (LAP (CMP W ,(source-indirect-reference! address offset)
	    (&U ,(make-non-pointer-literal type datum)))))