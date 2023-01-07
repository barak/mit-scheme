#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; RTL Rewrite Rules
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define-rule rewriting
  (CONS-NON-POINTER (? type) (? datum))
  ;; On aarch64, there's no difference between an address and a datum,
  ;; so the rules for constructing non-pointer objects are the same as
  ;; those for pointer objects.
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER (rtl:machine-constant? type))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (REGISTER (? type register-known-value)) (? datum))
  (QUALIFIER
   (and (rtl:object->type? type)
        (rtl:constant? (rtl:object->type-expression type))))
  (rtl:make-cons-pointer
   (rtl:make-machine-constant
    (back-end:object-type
     (rtl:constant-value (rtl:object->type-expression datum))))
   datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER (rtl:machine-constant? datum))
  (rtl:make-cons-pointer type datum))

(define-rule rewriting
  (CONS-POINTER (? type) (REGISTER (? datum register-known-value)))
  (QUALIFIER
   (and (rtl:object->datum? datum)
        (rtl:constant-non-pointer? (rtl:object->datum-expression datum))))
  (rtl:make-cons-pointer
   type
   (rtl:make-machine-constant
    (back-end:object-datum
     (rtl:constant-value (rtl:object->datum-expression datum))))))

(define-rule rewriting
  (OBJECT->TYPE (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant? source))
  (rtl:make-machine-constant
   (back-end:object-type (rtl:constant-value source))))

(define-rule rewriting
  (OBJECT->DATUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-non-pointer? source))
  (rtl:make-machine-constant
   (back-end:object-datum (rtl:constant-value source))))

(define (rtl:constant-non-pointer? expression)
  (and (rtl:constant? expression)
       (non-pointer-object? (rtl:constant-value expression))))

;;; These rules are losers because there's no abstract way to cons a
;;; statement or a predicate without also getting some CFG structure.

(define-rule rewriting
  (ASSIGN (? target) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'ASSIGN target comparand))

(define-rule rewriting
  (EQ-TEST (? source) (REGISTER (? comparand register-known-value)))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define-rule rewriting
  (EQ-TEST (REGISTER (? comparand register-known-value)) (? source))
  (QUALIFIER (rtl:immediate-zero-constant? comparand))
  (list 'EQ-TEST source comparand))

(define (rtl:immediate-zero-constant? expression)
  (cond ((rtl:constant? expression)
         (let ((value (rtl:constant-value expression)))
           (and (non-pointer-object? value)
                (zero? (back-end:object-type value))
                (zero? (back-end:object-datum value)))))
        ((rtl:cons-pointer? expression)
         (and (let ((expression (rtl:cons-pointer-type expression)))
                (and (rtl:machine-constant? expression)
                     (zero? (rtl:machine-constant-value expression))))
              (let ((expression (rtl:cons-pointer-datum expression)))
                (and (rtl:machine-constant? expression)
                     (zero? (rtl:machine-constant-value expression))))))
        (else #f)))

;;;; Fixnums

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:constant-fixnum? source))
  (rtl:make-object->fixnum source))

(define-rule rewriting
  (OBJECT->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:cons-non-pointer? source))
  (rtl:make-address->fixnum (rtl:cons-non-pointer-datum source)))

(define-rule rewriting
  (ADDRESS->FIXNUM (REGISTER (? source register-known-value)))
  (QUALIFIER (rtl:object->datum? source))
  ;; Pun: ADDRESS->FIXNUM has the same effect as OBJECT->FIXNUM even on
  ;; tagged objects.  If we ever changed the representation of
  ;; addresses (which is unlikely -- there's no temptation to disable
  ;; HEAP_IN_LOW_MEMORY because we have 58 bits for addresses) we would
  ;; have to change this.
  (rtl:make-address->fixnum (rtl:object->datum-expression source)))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
                 (REGISTER (? operand-1 register-known-value))
                 (? operand-2)
                 (? overflow?))
  (QUALIFIER (rtl:constant-fixnum-test operand-1 (lambda (n) n true)))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS MULTIPLY-FIXNUM
                 (? operand-1)
                 (REGISTER (? operand-2 register-known-value))
                 (? overflow?))
  (QUALIFIER
   (and (rtl:constant-fixnum-test operand-2 (lambda (n) n true))))
  (rtl:make-fixnum-2-args 'MULTIPLY-FIXNUM operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
                 (? operand-1)
                 (REGISTER (? operand-2 register-known-value))
                 (? overflow?))
  (QUALIFIER
   (and (memq operator '(PLUS-FIXNUM MINUS-FIXNUM))
        (rtl:register? operand-1)
        (rtl:constant-fixnum-test operand-2 zero?)))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS (? operator)
                 (? operand-1)
                 (REGISTER (? operand-2 register-known-value))
                 (? overflow?))
  (QUALIFIER
   (and (memq operator '(FIXNUM-QUOTIENT FIXNUM-REMAINDER))
        (rtl:register? operand-1)
        (rtl:constant-fixnum-test operand-2
          (lambda (value)
            (not (zero? value))))))
  (rtl:make-fixnum-2-args operator operand-1 operand-2 overflow?))

(define-rule rewriting
  (FIXNUM-2-ARGS FIXNUM-LSH
                 (? operand-1)
                 (REGISTER (? operand-2 register-known-value))
                 #F)
  (QUALIFIER (and (rtl:register? operand-1)
                  (rtl:constant-fixnum-test operand-2 (lambda (n) n true))))
  (rtl:make-fixnum-2-args 'FIXNUM-LSH operand-1 operand-2 #F))

(define (rtl:constant-fixnum? expression)
  (and (rtl:constant? expression)
       (fix:fixnum? (rtl:constant-value expression))
       (rtl:constant-value expression)))

(define (rtl:constant-fixnum-test expression predicate)
  (and (rtl:object->fixnum? expression)
       (let ((expression (rtl:object->fixnum-expression expression)))
         (and (rtl:constant? expression)
              (let ((n (rtl:constant-value expression)))
                (and (fix:fixnum? n)
                     (predicate n)))))))
