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

;;;; UCD table glue
;;; package: (runtime ucd-table-glue)

(declare (usual-integrations))

(define-deferred char-set:numeric
  (compute-char-set
   (lambda (sv)
     (eq? 'decimal (ucd-nt-value sv)))))

(define-deferred char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))

(define-deferred char-alphabetic?
  (char-set-predicate char-set:alphabetic))

(define-deferred char-alphanumeric?
  (char-set-predicate char-set:alphanumeric))

(define-deferred char-lower-case?
  (char-set-predicate char-set:lower-case))

(define-deferred char-numeric?
  (char-set-predicate char-set:numeric))

(define-deferred char-upper-case?
  (char-set-predicate char-set:upper-case))

(define-deferred char-whitespace?
  (char-set-predicate char-set:whitespace))

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

;;;; Scheme language:

(define (symbol-constituent? sv)
  (case sv
    ;; #\" #\# #\' #\, #\; #\\ #\` #\|
    ((#x22 #x23 #x27 #x2c #x3b #x5c #x60 #x7c) #f)
    ((#x200C #x200D) #t)
    (else
     (case (unicode-code-point-general-category sv)
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

(define-deferred char-set:symbol-initial
  (compute-char-set (lambda (sv) (eq? #t (symbol-constituent? sv)))))