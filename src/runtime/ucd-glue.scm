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

;;;; UCD table glue
;;; package: (runtime ucd-table-glue)

(declare (usual-integrations))

(define-deferred char-set:numeric
  (compute-char-set
   (lambda (sv)
     (eq? 'decimal (ucd-nt-value (integer->char sv))))))

(define-deferred char-numeric?
  (char-set-predicate char-set:numeric))

(define-deferred char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))

(define-deferred char-alphanumeric?
  (char-set-predicate char-set:alphanumeric))

(define-deferred char-set:not-alphabetic
  (char-set-invert char-set:alphabetic))

(define-deferred char-set:not-alphanumeric
  (char-set-invert char-set:alphanumeric))

(define-deferred char-set:not-lower-case
  (char-set-invert char-set:lower-case))

(define-deferred char-set:not-numeric
  (char-set-invert char-set:numeric))

(define-deferred char-set:not-upper-case
  (char-set-invert char-set:upper-case))

(define-deferred char-set:not-whitespace
  (char-set-invert char-set:whitespace))

(define-deferred char-set:unicode
  (compute-char-set
   (lambda (cp)
     (case (code-point-general-category cp)
       ((other:surrogate other:not-assigned) #f)
       (else #t)))))

(define-deferred unicode-char?
  (char-set-predicate char-set:unicode))

;;;; Scheme language:

(define (symbol-constituent? sv)
  (case sv
    ;; #\" #\# #\' #\, #\; #\\ #\` #\|
    ((#x22 #x23 #x27 #x2c #x3b #x5c #x60 #x7c) #f)
    ((#x200C #x200D) #t)
    (else
     (case (code-point-general-category sv)
       ((letter:uppercase
	 letter:lowercase
	 letter:titlecase
	 letter:modifier
	 letter:other
	 mark:nonspacing
	 number:letter
	 number:other
	 punctuation:connector
	 punctuation:dash
	 punctuation:other
	 symbol:math
	 symbol:currency
	 symbol:modifier
	 symbol:other
	 other:private-use)
	#t)
       ((mark:spacing-combining
	 mark:enclosing
	 number:decimal-digit)
	'subsequent-only)
       (else #f)))))

(define-deferred char-set:symbol-constituent
  (compute-char-set symbol-constituent?))

(define-deferred char-set:folded-symbol-constituent
  (char-set-difference char-set:symbol-constituent
		       char-set:changes-when-case-folded))

(define-deferred char-set:symbol-initial
  (compute-char-set (lambda (sv) (eq? #t (symbol-constituent? sv)))))

(define-deferred char-set:folded-symbol-initial
  (char-set-difference char-set:symbol-initial
		       char-set:changes-when-case-folded))

(define-deferred char-set:normal-printing
  (compute-char-set
   (lambda (sv)
     (case (code-point-general-category sv)
       ((letter:uppercase
	 letter:lowercase
	 letter:titlecase
	 letter:modifier
	 letter:other
	 mark:nonspacing
	 mark:spacing-combining
	 mark:enclosing
	 number:decimal-digit
	 number:letter
	 number:other
	 punctuation:connector
	 punctuation:dash
	 punctuation:open
	 punctuation:close
	 punctuation:initial-quote
	 punctuation:final-quote
	 punctuation:other
	 separator:space
	 symbol:math
	 symbol:currency
	 symbol:modifier
	 symbol:other)
	#t)
       (else #f)))))