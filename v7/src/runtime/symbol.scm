#| -*-Scheme-*-

$Id: symbol.scm,v 1.2 1993/10/11 23:16:41 cph Exp $

Copyright (c) 1992-93 Massachusetts Institute of Technology

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

;;;; Symbols
;;; package: (runtime scode)

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
  ((ucode-primitive system-pair-cons)
   (ucode-type uninterned-symbol)
   string
   ;; Magic: must match microcode and "urtrap".
   ((ucode-primitive primitive-object-set-type)
    (ucode-type reference-trap)
    2)))

(define (string->symbol string)
  ;; This prevents the symbol from being affected if the string
  ;; is mutated.  The string is copied only if the symbol is
  ;; created.
  (or ((ucode-primitive find-symbol) string)
      ((ucode-primitive string->symbol) (string-copy string))))

(define-integrable (intern string)
  ((ucode-primitive string->symbol) (string-downcase string)))

(define-integrable (intern-soft string)
  ((ucode-primitive find-symbol) (string-downcase string)))

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

(define (symbol=? x y)
  (or (eq? x y)
      (and (uninterned-symbol? x)
	   (uninterned-symbol? y)
	   (let ((sx (system-pair-car x))
		 (sy (system-pair-car y)))
	     (let ((l (string-length sx)))
	       (and (fix:= l (string-length sy))
		    (let loop ((i 0))
		      (or (fix:= i l)
			  (and (char=? (string-ref sx i) (string-ref sy i))
			       (loop (fix:+ i 1)))))))))))

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