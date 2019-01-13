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

;;;; AArch64 Instruction Set Macros

(declare (usual-integrations))

(define (parse-instruction form forms early? environment)
  (assert (not early?))
  (receive (expansion bits) (process* form forms environment)
    bits
    expansion))

(define (process* form forms environment)
  (let recur ((form form) (forms forms))
    (receive (expansion bits) (process form environment)
      (if (pair? forms)
          (receive (tail bits*) (recur (car forms) (cdr forms))
            (values `(,(close-syntax 'APPEND environment) ,expansion ,tail)
                    (+ bits bits*)))
          (values expansion bits)))))

(define (process form environment)
  (if (not (pair? form))
      (error "Invalid instruction syntax:" form))
  (case (car form)
    ((IF) (process-if form environment))
    ((BITS) (process-fixed form environment))
    ((VARIABLE-WIDTH) (process-variable form environment))
    ((MACRO) (process-macro form environment))
    (else (error "Unknown instruction syntax:" form))))

(define (process-if form environment)
  (let ((condition (cadr form))
        (consequent (caddr form))
        (alternative (cadddr form)))
    (receive (con-exp con-bits) (process consequent environment)
      (receive (alt-exp alt-bits) (process alternative environment)
        (assert (eqv? con-bits alt-bits))
        (values `(,(close-syntax 'IF environment) ,condition ,con-exp ,alt-exp)
                con-bits)))))

(define (process-fixed form environment)
  (receive (expansion bits) (expand-fields (cdr form) environment)
    (values (optimize-group-syntax expansion #f environment) bits)))

(define (process-variable form environment)
  (let ((variable (cadr form))
        (expression (caddr form))
        (clauses (cdddr form)))
    (let ((options (map (process-variable-clause environment) clauses)))
      (let ((expression
             (variable-width-expression-syntaxer variable
                                                 expression
                                                 environment
                                                 options)))
        (values expression #f)))))

(define ((process-variable-clause environment) clause)
  (let ((range (car clause))
        (forms (cdr clause)))
    (receive (expansion bits) (process* (car forms) (cdr forms) environment)
      (assert bits "Variable within variable prohibited!")
      (assert (zero? (remainder bits 32)) "Wrong number of bits!")
      `(,expansion ,bits ,range))))

(define (process-macro form environment)
  (let ((width (cadr form))
        (expansion (caddr form)))
    (values ;; XXX Check the width here.  Check for cycles.
            `((,(close-syntax 'INSTRUCTION-LOOKUP environment)
               (,(close-syntax 'QUASIQUOTE environment)
                ,expansion)))
            width)))

(define (expand-fields fields environment)
  (let loop ((fields fields) (elements '()) (bits 0))
    (if (pair? fields)
        (receive (element1 bits1) (expand-field (car fields) environment)
          (loop (cdr fields) (cons element1 elements) (+ bits1 bits)))
        (values (reverse! elements) bits))))

(define (expand-field field environment)
  (let ((bits (car field))
        (expression (cadr field))
        (coercion (if (pair? (cddr field)) (caddr field) 'UNSIGNED)))
    (values
     (case coercion
       ((BLOCK-OFFSET)
        `(,(close-syntax 'LIST environment) 'BLOCK-OFFSET ,expression))
       (else
        (integer-syntaxer expression environment coercion bits)))
     bits)))
