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

(define (symbol=? symbol1 symbol2 . symbols)
  (guarantee symbol? symbol1 'symbol=?)
  (guarantee symbol? symbol2 'symbol=?)
  (and (eq? symbol1 symbol2)
       (every (lambda (symbol)
		(guarantee symbol? symbol 'symbol=?)
		(eq? symbol1 symbol))
	      symbols)))

(define (string->uninterned-symbol string #!optional start end)
  ((ucode-primitive system-pair-cons) (ucode-type uninterned-symbol)
				      (string->utf8 string start end)
				      (make-unmapped-unbound-reference-trap)))

(define (string->symbol string #!optional start end)
  ((ucode-primitive string->symbol)
   ;; Needed during cold load.
   (if (and (%ustring1? string)
	    (ustring-ascii? string)
	    (default-object? start)
	    (default-object? end))
       (->bytes string)
       (string->utf8 string start end))))

(define (symbol->string symbol)
  (guarantee symbol? symbol 'symbol->string)
  (symbol-name symbol))

(define (symbol-name symbol)
  (let ((bytes (->bytes (system-pair-car symbol))))
    (or (maybe-ascii bytes)
	(utf8->string bytes))))

(define (symbol . objects)
  (string->symbol (string* objects)))

(define (intern string)
  ((ucode-primitive string->symbol) (foldcase->utf8 string)))

(define (intern-soft string)
  ((ucode-primitive find-symbol) (foldcase->utf8 string)))

(define (symbol-hash symbol #!optional modulus)
  (string-hash (symbol->string symbol) modulus))

(define (symbol<? x y)
  (bytevector<? (->bytes (system-pair-car x))
		(->bytes (system-pair-car y))))

(define (symbol>? x y)
  (symbol<? y x))

(define generate-uninterned-symbol
  (let ((mutex (make-thread-mutex))
	(counter 0)
	(default-prefix "G"))
    (named-lambda (generate-uninterned-symbol #!optional argument)
      (let ((finish
	     (lambda (prefix)
	       (string->uninterned-symbol
		(string-append prefix
			       (number->string
				(with-thread-mutex-lock mutex
				  (lambda ()
				    (let ((n counter))
				      (set! counter (+ counter 1))
				      n)))))))))
	(cond ((or (default-object? argument) (not argument))
	       (finish default-prefix))
	      ((string? argument)
	       (finish argument))
	      ((symbol? argument)
	       (finish (symbol->string argument)))
	      ((exact-nonnegative-integer? argument)
	       (with-thread-mutex-lock mutex
		 (lambda ()
		   (set! counter argument)
		   unspecific))
	       (finish default-prefix))
	      (else
	       (error "Invalid argument to generate-uninterned-symbol:"
		      argument)))))))

(define-integrable (->bytes maybe-string)
  (object-new-type (ucode-type bytevector) maybe-string))

(define (maybe-ascii bytes)
  ;; Needed during cold load.
  (let ((string (object-new-type (ucode-type unicode-string) bytes)))
    (and (ustring-ascii? string)
	 (begin
	   (%ascii-ustring! string)
	   string))))

(define (foldcase->utf8 string)
  (if (and (%ustring1? string)
	   (ustring-ascii? string))
      ;; Needed during cold load.
      (->bytes (ascii-string-foldcase string))
      (string->utf8 (string-foldcase string))))

(define (ustring-ascii? string)
  (let ((end (ustring-length string)))
    (let loop ((i 0))
      (if (fix:< i end)
	  (and (fix:< (cp1-ref string i) #x80)
	       (loop (fix:+ i 1)))
	  #t))))

(define (ascii-string-foldcase string)
  (let ((end (ustring-length string)))
    (if (let loop ((i 0))
	  (if (fix:< i end)
	      (and (not (ascii-changes-when-case-folded? (cp1-ref string i)))
		   (loop (fix:+ i 1)))
	      #t))
	string
	(let ((string* (%ascii-ustring-allocate end)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i end))
	    (cp1-set! string*
		      i
		      (ascii-foldcase (cp1-ref string i))))
	  string*))))

(define (ascii-changes-when-case-folded? code)
  (and (fix:>= code (char->integer #\A))
       (fix:<= code (char->integer #\Z))))

(define (ascii-foldcase code)
  (if (ascii-changes-when-case-folded? code)
      (fix:+ (char->integer #\a)
	     (fix:- code (char->integer #\A)))
      code))