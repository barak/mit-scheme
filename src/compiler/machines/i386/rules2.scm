#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

(define (set-equal-branches!)
  (set-current-branches! (lambda (label)
			   (LAP (JE (@PCR ,label))))
			 (lambda (label)
			   (LAP (JNE (@PCR ,label))))))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (set-equal-branches!)
  (LAP (CMP B ,(reference-alias-register! register 'GENERAL) (&U ,type))))

(define-rule predicate
  (EQ-TEST (REGISTER (? register-1)) (REGISTER (? register-2)))
  (set-equal-branches!)
  (compare/register*register register-1 register-2))

(define-rule predicate
  (EQ-TEST (REGISTER (? register)) (? expression rtl:simple-offset?))
  (set-equal-branches!)
  (LAP (CMP W ,(source-register-reference register)
	    ,(offset->reference! expression))))

(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?) (REGISTER (? register)))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
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
  (EQ-TEST (CONSTANT (? constant)) (? expression rtl:simple-offset?))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?) (CONSTANT (? constant)))
  (QUALIFIER (non-pointer-object? constant))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(non-pointer->literal constant)))))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant-1)) (CONSTANT (? constant-2)))
  (let ((always-jump
	 (lambda (label)
	   (LAP (JMP (@PCR ,label)))))
	(always-fall-through
	 (lambda (label)
	   label			; ignored
	   (LAP))))
    (if (eq? constant-1 constant-2)
	(set-current-branches! always-jump always-fall-through)
	(set-current-branches! always-fall-through always-jump)))
  (LAP))

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
	   (? expression rtl:simple-offset?))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(make-non-pointer-literal type datum)))))

(define-rule predicate
  (EQ-TEST (? expression rtl:simple-offset?)
	   (CONS-POINTER (MACHINE-CONSTANT (? type))
			 (MACHINE-CONSTANT (? datum))))
  (set-equal-branches!)
  (LAP (CMP W ,(offset->reference! expression)
	    (&U ,(make-non-pointer-literal type datum)))))


;; Combine tests for fixnum and non-negative by extracting the type
;; bits and the sign bit.

(define-rule predicate
  (PRED-1-ARG INDEX-FIXNUM?
	      (REGISTER (? register)))
  (let ((temp (standard-move-to-temporary! register)))
    (set-equal-branches!)
    (LAP (SHR W ,temp (& ,(- scheme-datum-width 1)))
	 (CMP B ,temp (&U ,(* 2 (ucode-type fixnum)))))))
