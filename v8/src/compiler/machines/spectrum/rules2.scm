#| -*-Scheme-*-

$Id$

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; LAP Generation Rules: Predicates

(declare (usual-integrations))

;(define-rule predicate
;  ;; test for two registers EQ?
;  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
;  (compare '= (standard-source! source1) (standard-source! source2)))
;
;(define-rule predicate
;  (EQ-TEST (MACHINE-CONSTANT 0) (REGISTER (? register)))
;  (compare-immediate '= 0 (standard-source! register)))
;
;(define-rule predicate
;  (EQ-TEST (REGISTER (? register)) (MACHINE-CONSTANT 0))
;  (compare-immediate '= 0 (standard-source! register)))

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
  ;; test for two registers, or values EQ?
  (EQ-TEST (? source1 register-expression) (? source2 register-expression))
  (compare '= (standard-source! source1) (standard-source! source2)))

(define-rule predicate
  (PRED-1-ARG FALSE? (REGISTER (? source)))
  (if compiler:generate-trap-on-null-valued-conditional?
      (let ((source (standard-source! source)))
	(set-current-branches!
	 (lambda (label)
	   (LAP (COMBN (=) ,regnum:false-value ,source (@PCR ,label))
		(COMCLR (<>) ,regnum:empty-list ,source 0)
		(BREAK () 0 0)))
	 (lambda (label)
	   (let ((local-label (generate-uninterned-symbol 'quasi-bogon-)))
	     (LAP (COMBN (=) ,regnum:false-value ,source (@PCR ,local-label))
		  (COMBN (<>) ,regnum:empty-list ,source (@PCR ,label))
		  (BREAK () 0 0)
		  (LABEL  ,local-label)))))
	(LAP))
      (compare '= regnum:false-value (standard-source! source))))

(define-rule predicate
  (PRED-1-ARG NULL? (REGISTER (? source)))
  (compare '= regnum:empty-list (standard-source! source)))

(define-rule predicate
  ;; Branch if virtual register contains the specified type number
  (TYPE-TEST (REGISTER (? register)) (? type))
  (QUALIFIER (exact-integer? type))
  (compare-immediate '= type (standard-source! register)))

(define-rule predicate
  ;; Branch if virtual register contains the specified type number
  (TYPE-TEST (OBJECT->TYPE (REGISTER (? register))) 0)
  (let ((src (standard-source! register)))
    (set-current-branches!
     (lambda (if-true)
       (LAP (EXTRU (<>)  ,src ,(-1+ scheme-type-width) ,scheme-type-width 0)
	    (B (N) (@PCR ,if-true))))
     (lambda (if-false)
       (LAP (EXTRU (=) ,src ,(-1+ scheme-type-width) ,scheme-type-width 0)
	    (B (N) (@PCR ,if-false)))))
    (LAP)))

(define-rule predicate
  (PRED-2-ARGS SMALL-FIXNUM?
	       (REGISTER (? source))
	       (MACHINE-CONSTANT (? nbits)))
  (let* ((src (standard-source! source))
	 (temp (standard-temporary!)))
    (LAP (EXTRS () ,src 31 ,(- (+ scheme-datum-width 1) nbits) ,temp)
	 ,@(COMPARE '= src temp))))

(define-rule predicate
  (PRED-1-ARG GENERIC-ADDITIVE-TEST (REGISTER (? source)))
  (let ((temp (standard-temporary!))
	(src  (standard-source! source)))
    (LAP (EXTRS () ,src 31 ,scheme-datum-width ,temp)
	 ,@(compare '= src temp))))

(define-rule predicate
  (PRED-1-ARG FIXNUM? (REGISTER (? source)))
  (let ((temp (standard-temporary!))
	(src  (standard-source! source)))
    (LAP (EXTRS () ,src 31 ,(1+ scheme-datum-width) ,temp)
	 ,@(compare '= src temp))))

#|
;; Taken care of by rewrite
(define-rule predicate
  (PRED-1-ARG INDEX-FIXNUM? (REGISTER (? source)))
  (let ((temp (standard-temporary!))
	(src  (standard-source! source)))
    (LAP (blah blah blah))))
|#

(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (REGISTER (? smaller))
	       (REGISTER (? larger)))
  (compare '<< (standard-source! smaller) (standard-source! larger)))

(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (CONSTANT (? smaller))
	       (REGISTER (? larger)))
  (compare-immediate '<< smaller (standard-source! larger)))
	       
(define-rule predicate
  (PRED-2-ARGS WORD-LESS-THAN-UNSIGNED?
	       (REGISTER (? smaller))
	       (CONSTANT (? larger)))
  (compare-immediate '>> (standard-source! smaller) larger))
	       
