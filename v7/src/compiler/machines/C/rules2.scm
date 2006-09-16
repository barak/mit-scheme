#| -*-Scheme-*-

$Id: rules2.scm,v 1.6 2006/09/16 11:19:09 gjr Exp $

Copyright (c) 1992, 1999, 2006 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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

(define-rule predicate
  ;; Branch if virtual register contains a legal index fixnum
  (PRED-1-ARG INDEX-FIXNUM?
	      (REGISTER (? source)))
  (let ((source (standard-source! source 'ULONG)))
    (set-current-branches!
     (lambda (if-true-label)
       (LAP "if (INDEX_FIXNUM_P (" ,source "))\n\t  goto " ,if-true-label
	    ";\n\t"))
     (lambda (if-false-label)
       (LAP "if (!(INDEX_FIXNUM_P (" ,source ")))\n\t  goto " ,if-false-label
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