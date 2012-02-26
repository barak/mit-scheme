#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Code to install syntax keywords in global environment
;;; package: (runtime syntax definitions)

(declare (usual-integrations))

(define (initialize-package!)
  (create-bindings (->syntactic-environment system-global-environment)))

(define (create-bindings senv)

  (define (def name item)
    (syntactic-environment/define senv name item))

  (define (define-classifier name classifier)
    (def name (make-classifier-item classifier)))

  (define-classifier 'BEGIN classifier:begin)
  (define-classifier 'DECLARE classifier:declare)
  (define-classifier 'DEFINE-SYNTAX classifier:define-syntax)
  (define-classifier 'ER-MACRO-TRANSFORMER classifier:er-macro-transformer)
  (define-classifier 'LET-SYNTAX classifier:let-syntax)
  (define-classifier 'LETREC classifier:letrec)
  (define-classifier 'LETREC* classifier:letrec*)
  (define-classifier 'LETREC-SYNTAX classifier:letrec-syntax)
  (define-classifier 'LOCAL-DECLARE classifier:local-declare)
  (define-classifier 'NON-HYGIENIC-MACRO-TRANSFORMER
    classifier:non-hygienic-macro-transformer)
  (define-classifier 'RSC-MACRO-TRANSFORMER classifier:rsc-macro-transformer)
  (define-classifier 'SC-MACRO-TRANSFORMER classifier:sc-macro-transformer)

  (define (define-compiler name compiler)
    (def name (make-compiler-item compiler)))

  (define-compiler 'DELAY compiler:delay)
  (define-compiler 'IF compiler:if)
  (define-compiler 'LAMBDA compiler:lambda)
  (define-compiler 'NAMED-LAMBDA compiler:named-lambda)
  (define-compiler 'OR compiler:or)
  (define-compiler 'QUOTE compiler:quote)
  (define-compiler 'SET! compiler:set!)
  (define-compiler 'THE-ENVIRONMENT compiler:the-environment))