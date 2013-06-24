#| -*-Scheme-*-

$Id: chrset.scm,v 14.16 2001/09/25 05:15:17 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

(define-structure (char-set (type-descriptor char-set-rtd))
  (table #f read-only #t))

(define-integrable (guarantee-char-set object procedure)
  (if (not (char-set? object))
      (error:wrong-type-argument object "character set" procedure)))

(define-integrable char-set-table-length 256)

(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (if (not (list-of-type? chars
	     (lambda (char)
	       (and (char? char)
		    (fix:< (char->integer char) char-set-table-length)))))
      (error:wrong-type-argument chars "ASCII chars" 'CHARS->CHAR-SET))
  (let ((table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 char-set-table-length 0)
    (do ((chars chars (cdr chars)))
	((not (pair? chars)))
      (vector-8b-set! table (char->integer (car chars)) 1))
    (make-char-set table)))

(define (string->char-set string)
  (guarantee-string string 'STRING->CHAR-SET)
  (let ((n-chars (string-length string))
	(table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 char-set-table-length 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i n-chars))
      (vector-8b-set! table (vector-8b-ref string i) 1))
    (make-char-set table)))

(define (ascii-range->char-set lower upper)
  (if (not (index-fixnum? lower))
      (error:wrong-type-argument lower "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (index-fixnum? upper))
      (error:wrong-type-argument upper "index fixnum" 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= lower upper))
      (error:bad-range-argument lower 'ASCII-RANGE->CHAR-SET))
  (if (not (fix:<= upper char-set-table-length))
      (error:bad-range-argument upper 'ASCII-RANGE->CHAR-SET))
  (let ((table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 lower 0)
    (vector-8b-fill! table lower upper 1)
    (vector-8b-fill! table upper char-set-table-length 0)
    (make-char-set table)))

(define (predicate->char-set predicate)
  (let ((table (make-string char-set-table-length)))
    (do ((code 0 (fix:+ code 1)))
	((fix:= code char-set-table-length))
      (vector-8b-set! table code (if (predicate (integer->char code)) 1 0)))
    (make-char-set table)))

(define (char-set-members char-set)
  (guarantee-char-set char-set 'CHAR-SET-MEMBERS)
  (let ((table (char-set-table char-set)))
    (let loop ((code (fix:- char-set-table-length 1)) (chars '()))
      (if (fix:= code 0)
	  (if (fix:= 0 (vector-8b-ref table code))
	      chars
	      (cons (integer->char code) chars))
	  (loop (fix:- code 1)
		(if (fix:= 0 (vector-8b-ref table code))
		    chars
		    (cons (integer->char code) chars)))))))

(define (char-set-member? char-set char)
  (guarantee-char-set char-set 'CHAR-SET-MEMBER?)
  (guarantee-char char 'CHAR-SET-MEMBER?)
  (%char-set-member? char-set char))

(define (%char-set-member? char-set char)
  (and (fix:< (char->integer char) char-set-table-length)
       (not (fix:= 0
		   (vector-8b-ref (char-set-table char-set)
				  (char->integer char))))))

(define (char-set-invert char-set)
  (guarantee-char-set char-set 'CHAR-SET-INVERT)
  (predicate->char-set
   (lambda (char)
     (not (%char-set-member? char-set char)))))

(define (char-set-union . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-UNION)
  (predicate->char-set
   (lambda (char)
     (there-exists? char-sets
       (lambda (char-set)
	 (%char-set-member? char-set char))))))

(define (char-set-intersection . char-sets)
  (guarantee-char-sets char-sets 'CHAR-SET-INTERSECTION)
  (predicate->char-set
   (lambda (char)
     (for-all? char-sets
       (lambda (char-set)
	 (%char-set-member? char-set char))))))

(define (guarantee-char-sets char-sets procedure)
  (for-each (lambda (char-set) (guarantee-char-set char-set procedure))
	    char-sets))

(define (char-set-difference include exclude)
  (guarantee-char-set include 'CHAR-SET-DIFFERENCE)
  (guarantee-char-set exclude 'CHAR-SET-DIFFERENCE)
  (predicate->char-set
   (lambda (char)
     (and (%char-set-member? include char)
	  (not (%char-set-member? exclude char))))))

;;;; System Character Sets

(define char-set:upper-case)
(define char-set:lower-case)
(define char-set:numeric)
(define char-set:graphic)
(define char-set:whitespace)
(define char-set:alphabetic)
(define char-set:alphanumeric)
(define char-set:standard)
(define char-set:newline)

(define char-set:not-upper-case)
(define char-set:not-lower-case)
(define char-set:not-numeric)
(define char-set:not-graphic)
(define char-set:not-whitespace)
(define char-set:not-alphabetic)
(define char-set:not-alphanumeric)
(define char-set:not-standard)

(define (initialize-package!)
  (set! char-set:upper-case
	(char-set-union (ascii-range->char-set #x41 #x5B)
			(ascii-range->char-set #xC0 #xD7)
			(ascii-range->char-set #xD8 #xDE)))
  (set! char-set:lower-case
	(char-set-union (ascii-range->char-set #x61 #x7B)
			(ascii-range->char-set #xE0 #xF7)
			(ascii-range->char-set #xF8 #xFF)))
  (set! char-set:numeric (ascii-range->char-set #x30 #x3A))
  (set! char-set:graphic
	(char-set-union (ascii-range->char-set #x20 #x7F)
			(ascii-range->char-set #xA0 #x100)))
  (set! char-set:whitespace
	(char-set #\newline #\tab #\linefeed #\page #\return #\space
		  (integer->char #xA0)))
  (set! char-set:alphabetic
	(char-set-union char-set:upper-case char-set:lower-case))
  (set! char-set:alphanumeric
	(char-set-union char-set:alphabetic char-set:numeric))
  (set! char-set:standard
	(char-set-union char-set:graphic (char-set #\newline)))
  (set! char-set:newline (char-set #\newline))
  (set! char-set:not-upper-case   (char-set-invert char-set:upper-case))
  (set! char-set:not-lower-case   (char-set-invert char-set:lower-case))
  (set! char-set:not-numeric      (char-set-invert char-set:numeric))
  (set! char-set:not-graphic      (char-set-invert char-set:graphic))
  (set! char-set:not-whitespace   (char-set-invert char-set:whitespace))
  (set! char-set:not-alphabetic   (char-set-invert char-set:alphabetic))
  (set! char-set:not-alphanumeric (char-set-invert char-set:alphanumeric))
  (set! char-set:not-standard     (char-set-invert char-set:standard))
  unspecific)

(define (char-upper-case? char)
  (guarantee-char char 'CHAR-UPPER-CASE?)
  (%char-upper-case? char))

(define-integrable (%char-upper-case? char)
  (%char-set-member? char-set:upper-case char))

(define (char-lower-case? char)
  (guarantee-char char 'CHAR-LOWER-CASE?)
  (%char-lower-case? char))

(define-integrable (%char-lower-case? char)
  (%char-set-member? char-set:lower-case char))

(define (char-numeric? char)
  (guarantee-char char 'CHAR-NUMERIC?)
  (%char-numeric? char))

(define-integrable (%char-numeric? char)
  (%char-set-member? char-set:numeric char))

(define (char-graphic? char)
  (guarantee-char char 'CHAR-GRAPHIC?)
  (%char-graphic? char))

(define-integrable (%char-graphic? char)
  (%char-set-member? char-set:graphic char))

(define (char-whitespace? char)
  (guarantee-char char 'CHAR-WHITESPACE?)
  (%char-whitespace? char))

(define-integrable (%char-whitespace? char)
  (%char-set-member? char-set:whitespace char))

(define (char-alphabetic? char)
  (guarantee-char char 'CHAR-ALPHABETIC?)
  (%char-alphabetic? char))

(define-integrable (%char-alphabetic? char)
  (%char-set-member? char-set:alphabetic char))

(define (char-alphanumeric? char)
  (guarantee-char char 'CHAR-ALPHANUMERIC?)
  (%char-alphanumeric? char))

(define-integrable (%char-alphanumeric? char)
  (%char-set-member? char-set:alphanumeric char))

(define (char-standard? char)
  (guarantee-char char 'CHAR-STANDARD?)
  (%char-standard? char))

(define-integrable (%char-standard? char)
  (%char-set-member? char-set:standard char))