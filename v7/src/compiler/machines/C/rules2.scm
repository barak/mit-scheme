#| -*-Scheme-*-

$Id: rules2.scm,v 1.1 1993/06/08 06:13:32 gjr Exp $

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

(define-rule predicate
  ;; test for two registers EQ?
  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
  (let ((source1 (standard-source! source1 'SCHEME_OBJECT))
	(source2 (standard-source! source2 'SCHEME_OBJECT)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (" ,source1 " == " ,source2 ")\n\t  goto "
	    ,if-true-label ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (" ,source1 " != " ,source2 ")\n\t  goto "
	    ,if-false-label ";\n\t")))
    (LAP)))

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
  (let ((source (standard-source! source 'ULONG)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (" ,source " == " ,type ")\n\t  goto " ,if-true-label
	    ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (" ,source " != " ,type ")\n\t  goto " ,if-false-label
	    ";\n\t")))
    (LAP)))

(define (eq-test/constant constant source)
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (" ,source " == current_block[" ,(object->offset constant)
	    "])\n\t  goto " ,if-true-label ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (" ,source " != current_block[" ,(object->offset constant)
	    "])\n\t  goto " ,if-false-label ";\n\t")))
    (LAP)))

(define (eq-test/machine-constant constant source)
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (" ,source " == ((SCHEME_OBJECT) " ,constant "))\n\t  goto "
	    ,if-true-label ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (" ,source " != ((SCHEME_OBJECT) " ,constant "))\n\t  goto "
	    ,if-false-label ";\n\t")))
    (LAP)))

(define (eq-test/non-pointer type datum source)
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (" ,source " == (MAKE_OBJECT (" ,type ", " ,datum
	    ")))\n\t  goto " ,if-true-label ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (" ,source " != (MAKE_OBJECT (" ,type ", " ,datum
	    ")))\n\t  goto " ,if-false-label ";\n\t")))
    (LAP)))