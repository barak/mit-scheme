#| -*-Scheme-*-

$Id: symbol.scm,v 1.9 2003/02/14 18:28:34 cph Exp $

Copyright (c) 1992-2001 Massachusetts Institute of Technology

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

;;;; Symbols
;;; package: (runtime symbol)

(declare (usual-integrations))

(define (symbol? object)
  (or (interned-symbol? object)
      (uninterned-symbol? object)))

(define-integrable (interned-symbol? object)
  (object-type? (ucode-type interned-symbol) object))

(define-integrable (uninterned-symbol? object)
  (object-type? (ucode-type uninterned-symbol) object))

(define (string->uninterned-symbol string)
  (if (not (string? string))
      (error:wrong-type-argument string "string" 'STRING->UNINTERNED-SYMBOL))
  ((ucode-primitive system-pair-cons) (ucode-type uninterned-symbol)
				      string
				      (make-unmapped-unbound-reference-trap)))

(define (string->symbol string)
  ;; Calling STRING-COPY prevents the symbol from being affected if
  ;; the string is mutated.  The string is copied only if the symbol
  ;; is created.
  (or ((ucode-primitive find-symbol) string)
      ((ucode-primitive string->symbol) (string-copy string))))

(define (intern string)
  (if (string-lower-case? string)
      (string->symbol string)
      ((ucode-primitive string->symbol) (string-downcase string))))

(define (intern-soft string)
  ((ucode-primitive find-symbol)
   (if (string-lower-case? string)
       string
       (string-downcase string))))

(define (symbol-name symbol)
  (if (not (symbol? symbol))
      (error:wrong-type-argument symbol "symbol" 'SYMBOL-NAME))
  (system-pair-car symbol))

(define-integrable (symbol->string symbol)
  (string-copy (symbol-name symbol)))

(define (symbol-append . symbols)
  (let ((string (apply string-append (map symbol-name symbols))))
    (string-downcase! string)
    ((ucode-primitive string->symbol) string)))

(define-integrable (symbol-hash symbol)
  (string-hash (symbol-name symbol)))

(define-integrable (symbol-hash-mod symbol modulus)
  (string-hash-mod (symbol-name symbol) modulus))

(define (symbol<? x y)
  (let ((sx (system-pair-car x))
	(sy (system-pair-car y)))
    (let ((lx (string-length sx))
	  (ly (string-length sy)))
      (let ((l (if (fix:< lx ly) lx ly)))
	(let loop ((i 0))
	  (cond ((fix:= i l)
		 (fix:< lx ly))
		((fix:= (vector-8b-ref sx i) (vector-8b-ref sy i))
		 (loop (fix:+ i 1)))
		(else
		 (fix:< (vector-8b-ref sx i) (vector-8b-ref sy i)))))))))