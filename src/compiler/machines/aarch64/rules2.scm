#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
  (EQ-TEST (REGISTER (? source1)) (REGISTER (? source2)))
  (set-equal-branches!)
  (standard-binary-effect source1 source2
    (lambda (source1 source2)
      (LAP (CMP X ,source1 ,source2)))))

(define-rule predicate
  (EQ-TEST (REGISTER (? source)) (MACHINE-CONSTANT (? immediate)))
  (eq-test/register*immediate! (standard-source! source) immediate))

(define-rule predicate
  (EQ-TEST (MACHINE-CONSTANT (? immediate)) (REGISTER (? source)))
  (eq-test/register*immediate! (standard-source! source) immediate))

(define-rule predicate
  (EQ-TEST (REGISTER (? source))
           (CONS-POINTER (MACHINE-CONSTANT (? type))
                         (MACHINE-CONSTANT (? datum))))
  (eq-test/register*tagged-immediate! (standard-source! source) type datum))

(define-rule predicate
  (EQ-TEST (CONS-POINTER (MACHINE-CONSTANT (? type))
                         (MACHINE-CONSTANT (? datum)))
           (REGISTER (? source)))
  (eq-test/register*tagged-immediate! (standard-source! source) type datum))

(define-rule predicate
  (EQ-TEST (REGISTER (? source)) (CONSTANT (? constant)))
  (QUALIFIER
   ;; Worth it only if we can confirm it's zero.
   (and (non-pointer-object? constant)
        (= 0 (non-pointer->literal constant))))
  (zero-test! (standard-source! source)))

(define-rule predicate
  (EQ-TEST (CONSTANT (? constant)) (REGISTER (? source)))
  (QUALIFIER
   ;; Worth it only if we can confirm it's zero.
   (and (non-pointer-object? constant)
        (= 0 (non-pointer->literal constant))))
  (zero-test! (standard-source! source)))

(define-rule predicate
  (TYPE-TEST (REGISTER (? register)) (? type))
  (eq-test/register*immediate! (standard-source! register) type))

;; Test tag and sign in one swell foop.

(define-rule predicate
  (PRED-1-ARG INDEX-FIXNUM? (REGISTER (? register)))
  (let* ((source (standard-source! register))
         (temp regnum:scratch-0))
    (set-equal-branches!)
    (LAP (LSR X ,temp ,source (&U ,(- scheme-datum-width 1)))
         (CMP X ,temp (&U ,(* 2 type-code:fixnum))))))

(define (zero-test! register)
  (set-equal-zero-branches! register)
  (LAP))

(define (eq-test/register*tagged-immediate! register type datum)
  (eq-test/register*immediate! register (make-non-pointer-literal type datum)))

(define (eq-test/register*immediate! register immediate)
  (if (= immediate 0)
      (zero-test! register)
      (begin
        (set-equal-branches!)
        (cmp-immediate register immediate general-temporary!))))

(define (set-always-branches!)
  (set-current-branches!
   (lambda (label) (LAP (B (@PCR ,label ,regnum:scratch-0))))
   (lambda (label) label (LAP))))

(define (set-never-branches!)
  (set-current-branches!
   (lambda (label) label (LAP))
   (lambda (label) (LAP (B (@PCR ,label ,regnum:scratch-0))))))

(define (set-equal-zero-branches! source)
  (set-current-branches!
   (lambda (label) (LAP (CBZ X ,source (@PCR ,label ,regnum:scratch-0))))
   (lambda (label) (LAP (CBNZ X ,source (@PCR ,label ,regnum:scratch-0))))))

(define (set-condition-branches! cc ~cc)
  (set-current-branches!
   (lambda (label) (LAP (B. ,cc (@PCR ,label ,regnum:scratch-0))))
   (lambda (label) (LAP (B. ,~cc (@PCR ,label ,regnum:scratch-0))))))

(define (set-carry-branches!)
  (set-condition-branches! 'CS 'CC))

(define (set-overflow-branches!)
  (set-condition-branches! 'VS 'VC))

(define (set-equal-branches!)
  (set-condition-branches! 'EQ 'NE))

(define (set-not-equal-branches!)
  (set-condition-branches! 'NE 'EQ))
