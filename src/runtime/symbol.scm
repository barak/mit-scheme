#| -*-Scheme-*-

$Id: symbol.scm,v 1.25 2008/01/30 20:02:35 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (string->uninterned-symbol string)
  (make-uninterned-symbol (if (string? string)
			      (string->utf8-string string)
			      (wide-string->utf8-string string))))

(define (utf8-string->uninterned-symbol string)
  (make-uninterned-symbol (if (utf8-string? string)
			      (string-copy string)
			      (wide-string->utf8-string string))))

(define (make-uninterned-symbol string)
  ((ucode-primitive system-pair-cons) (ucode-type uninterned-symbol)
				      string
				      (make-unmapped-unbound-reference-trap)))

(define (string->symbol string)
  ((ucode-primitive string->symbol) (if (string? string)
					(string->utf8-string string)
					(wide-string->utf8-string string))))

(define (utf8-string->symbol string)
  (if (utf8-string? string)
      (or ((ucode-primitive find-symbol) string)
	  ((ucode-primitive string->symbol) (string-copy string)))
      ((ucode-primitive string->symbol) (wide-string->utf8-string string))))

(define (substring->symbol string start end)
  (guarantee-substring string start end 'SUBSTRING->SYMBOL)
  ((ucode-primitive string->symbol) (string->utf8-string string start end)))

(define (string-head->symbol string end)
  (substring->symbol string 0 end))

(define (string-tail->symbol string start)
  (substring->symbol string start (string-length string)))

(define (symbol . objects)
  ((ucode-primitive string->symbol) (apply utf8-string objects)))

(define (intern string)
  ((ucode-primitive string->symbol)
   (utf8-string-downcase
    (if (string? string)
	string
	(wide-string->utf8-string string)))))

(define (intern-soft string)
  ((ucode-primitive find-symbol)
   (utf8-string-downcase
    (if (string? string)
	string
	(wide-string->utf8-string string)))))

(define (utf8-string-downcase string)
  (if (ascii-string? string)
      ;; Needed during cold load.
      (string-downcase string)
      (call-with-input-string string
	(lambda (input)
	  (port/set-coding input 'utf-8)
	  (call-with-output-string
	    (lambda (output)
	      (port/set-coding output 'utf-8)
	      (let loop ()
		(let ((c (read-char input)))
		  (if (not (eof-object? c))
		      (begin
			(write-char (char-downcase c) output)
			(loop)))))))))))

(define (ascii-string? string)
  (let ((end (string-length string)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (fix:< (vector-8b-ref string i) #x80)
	       (loop (fix:+ i 1)))
	  #t))))

(define (symbol-name symbol)
  (guarantee-symbol symbol 'SYMBOL-NAME)
  (system-pair-car symbol))

(define (symbol-hash symbol)
  (string-hash (symbol-name symbol)))

(define (symbol-hash-mod symbol modulus)
  (string-hash-mod (symbol-name symbol) modulus))

(define (symbol<? x y)
  (guarantee-symbol x 'SYMBOL<?)
  (guarantee-symbol y 'SYMBOL<?)
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

(define (symbol->utf8-string symbol)
  (string-copy (symbol-name symbol)))

(define (symbol->wide-string symbol)
  (utf8-string->wide-string (symbol-name symbol)))

(define (symbol->string symbol)
  (wide-string->string (symbol->wide-string symbol)))