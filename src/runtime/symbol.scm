#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Symbols
;;; package: (runtime symbol)

(declare (usual-integrations))

(declare (integrate-operator symbol?))
(define (symbol? object)
  (or (interned-symbol? object)
      (uninterned-symbol? object)))

(define-integrable (interned-symbol? object)
  (object-type? (ucode-type interned-symbol) object))

(define-integrable (uninterned-symbol? object)
  (object-type? (ucode-type uninterned-symbol) object))

(define-guarantee symbol "symbol")
(define-guarantee interned-symbol "interned symbol")
(define-guarantee uninterned-symbol "uninterned symbol")

(define (string->uninterned-symbol string #!optional start end)
  ((ucode-primitive system-pair-cons) (ucode-type uninterned-symbol)
				      (string->utf8 string start end)
				      (make-unmapped-unbound-reference-trap)))

(define (string->symbol string #!optional start end)
  ((ucode-primitive string->symbol) (string->utf8 string start end)))

(define (symbol->string symbol)
  (guarantee symbol? symbol 'symbol->string)
  (utf8->string
   (let ((name (system-pair-car symbol)))
     (cond ((bytevector? name) name)
	   ((legacy-string? name) (%legacy-string->bytevector name))
	   (else (error "Illegal symbol name:" name))))))

(define (string-head->symbol string end)
  (string->symbol (string-copy string 0 end)))

(define (string-tail->symbol string start)
  (string->symbol (string-copy string start)))

(define (symbol . objects)
  (string->symbol (%string* objects 'symbol)))

(define (intern string)
  (string->symbol (cold-load-foldcase string)))

(define (intern-soft string)
  ((ucode-primitive find-symbol) (string->utf8 (cold-load-foldcase string))))

(define (cold-load-foldcase string)
  (if (ascii-string? string)
      ;; Needed during cold load.
      (legacy-string-downcase string)
      (string-foldcase string)))

(define (symbol-name symbol)
  (if (not (symbol? symbol))
      (error:not-a symbol? symbol 'symbol-name))
  (let* ((bytes (system-pair-car symbol))
	 (string (object-new-type (ucode-type string) bytes)))
    (if (ascii-string? string)
	;; Needed during cold load.
	string
	(utf8->string bytes))))

(define (ascii-string? string)
  (and ((ucode-primitive string?) string)
       (let ((end ((ucode-primitive string-length) string)))
	 (let loop ((i 0))
	   (if (fix:< i end)
	       (and (fix:< ((ucode-primitive vector-8b-ref) string i) #x80)
		    (loop (fix:+ i 1)))
	       #t)))))

(define (symbol-hash symbol #!optional modulus)
  (string-hash (symbol-name symbol) modulus))

(define (symbol<? x y)
  (string<? (symbol-name x) (symbol-name y)))

(define (symbol>? x y)
  (string<? (symbol-name y) (symbol-name x)))