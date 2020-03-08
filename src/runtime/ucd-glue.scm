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

(define-deferred char-set:alphanumeric
  (char-set-union char-set:alphabetic char-set:numeric))

(define-deferred char-alphanumeric?
  (char-set-predicate char-set:alphanumeric))

(define-deferred char-set:control
  (char-set-union char-set:gc=other:control
		  char-set:gc=other:format
		  char-set:gc=other:surrogate
		  char-set:gc=other:private-use
		  char-set:gc=other:not-assigned))

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

(define-deferred char-set:punctuation
  (char-set-union char-set:gc=punctuation:connector
		  char-set:gc=punctuation:dash
		  char-set:gc=punctuation:open
		  char-set:gc=punctuation:close
		  char-set:gc=punctuation:initial-quote
		  char-set:gc=punctuation:final-quote
		  char-set:gc=punctuation:other))

(define-deferred char-set:symbol
  (char-set-union char-set:gc=symbol:math
		  char-set:gc=symbol:currency
		  char-set:gc=symbol:modifier
		  char-set:gc=symbol:other))

(define-deferred char-set:unicode
  (char-set-difference (char-set-invert (char-set))
		       char-set:gc=other:surrogate
		       char-set:gc=other:not-assigned))

(define-deferred unicode-char?
  (char-set-predicate char-set:unicode))

(define-deferred char-set:graphic
  (char-set-union char-set:alphanumeric
		  char-set:punctuation
		  char-set:symbol))
(define-deferred char-set:not-graphic (char-set-invert char-set:graphic))
(define-deferred char-graphic? (char-set-predicate char-set:graphic))

(define-deferred char-set:newline (char-set #\newline #\return))
(define-deferred char-set:no-newline
  (char-set-difference char-set:unicode char-set:newline))
(define-deferred char-newline? (char-set-predicate char-set:newline))

(define-deferred char-set:printing
  (char-set-union char-set:graphic
		  char-set:whitespace))
(define-deferred char-set:not-printing (char-set-invert char-set:printing))
(define-deferred char-printing? (char-set-predicate char-set:printing))

(define-deferred char-set:standard
  (char-set-union char-set:graphic (char-set #\newline)))
(define-deferred char-set:not-standard (char-set-invert char-set:standard))
(define-deferred char-standard? (char-set-predicate char-set:standard))

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
  (char-set-difference
   (char-set-union char-set:gc=letter:lowercase
		   char-set:gc=letter:modifier
		   char-set:gc=letter:other
		   char-set:gc=letter:titlecase
		   char-set:gc=letter:uppercase
		   char-set:gc=mark:enclosing
		   char-set:gc=mark:nonspacing
		   char-set:gc=mark:spacing-combining
		   char-set:gc=number:decimal-digit
		   char-set:gc=number:letter
		   char-set:gc=number:other
		   char-set:gc=other:private-use
		   char-set:gc=punctuation:connector
		   char-set:gc=punctuation:dash
		   char-set:gc=punctuation:other
		   char-set:gc=symbol:currency
		   char-set:gc=symbol:math
		   char-set:gc=symbol:modifier
		   char-set:gc=symbol:other
		   (char-set #\x200c #\x200d))
   (char-set #\" #\# #\' #\, #\; #\\ #\` #\|)))

(define-deferred char-set:folded-symbol-constituent
  (char-set-difference char-set:symbol-constituent
		       char-set:changes-when-case-folded))

(define-deferred char-set:symbol-initial
  (char-set-difference
   char-set:symbol-constituent
   (char-set-union char-set:gc=mark:spacing-combining
		   char-set:gc=mark:enclosing
		   char-set:gc=number:decimal-digit)))

(define-deferred char-set:folded-symbol-initial
  (char-set-difference char-set:symbol-initial
		       char-set:changes-when-case-folded))

(define-deferred char-set:normal-printing
  (char-set-union char-set:gc=letter:uppercase
		  char-set:gc=letter:lowercase
		  char-set:gc=letter:titlecase
		  char-set:gc=letter:modifier
		  char-set:gc=letter:other
		  char-set:gc=mark:nonspacing
		  char-set:gc=mark:spacing-combining
		  char-set:gc=mark:enclosing
		  char-set:gc=number:decimal-digit
		  char-set:gc=number:letter
		  char-set:gc=number:other
		  char-set:gc=punctuation:connector
		  char-set:gc=punctuation:dash
		  char-set:gc=punctuation:open
		  char-set:gc=punctuation:close
		  char-set:gc=punctuation:initial-quote
		  char-set:gc=punctuation:final-quote
		  char-set:gc=punctuation:other
		  char-set:gc=separator:space
		  char-set:gc=symbol:math
		  char-set:gc=symbol:currency
		  char-set:gc=symbol:modifier
		  char-set:gc=symbol:other))

(add-boot-init! (lambda () (run-deferred-boot-actions 'ucd)))