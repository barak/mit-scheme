#| -*-Scheme-*-

$Id: chrset.scm,v 14.13 2001/06/15 20:38:37 cph Exp $

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

(define-integrable char-set-table-length 256)

(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (let ((table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 char-set-table-length 0)
    (do ((chars chars (cdr chars)))
	((not (pair? chars)))
      (vector-8b-set! table
		      (let ((code (char->integer (car chars))))
			(if (fix:>= code char-set-table-length)
			    (error:bad-range-argument chars 'CHARS->CHAR-SET))
			code)
		      1))
    (make-char-set table)))

(define (string->char-set string)
  (let ((table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 char-set-table-length 0)
    (do ((i  (fix:- (string-length string) 1)  (fix:- i 1)))
	((fix:< i 0))
      (vector-8b-set! table (vector-8b-ref string i) 1))
    (make-char-set table)))

(define (ascii-range->char-set lower upper)
  (let ((table (make-string char-set-table-length)))
    (vector-8b-fill! table 0 lower 0)
    (vector-8b-fill! table lower upper 1)
    (vector-8b-fill! table upper char-set-table-length 0)
    (make-char-set table)))

(define (predicate->char-set predicate)
  (let ((table (make-string char-set-table-length)))
    (let loop ((code 0))
      (if (fix:< code char-set-table-length)
	  (begin
	    (vector-8b-set! table
			    code
			    (if (predicate (integer->char code)) 1 0))
	    (loop (fix:+ code 1)))))
    (make-char-set table)))

(define (char-set-members char-set)
  (if (not (char-set? char-set))
      (error:wrong-type-argument char-set "character set" 'CHAR-SET-MEMBERS))
  (let ((table (char-set-table char-set)))
    (let loop ((code char-set-table-length) (chars '()))
      (if (fix:< 0 code)
	  (loop (fix:- code 1)
		(if (fix:zero? (vector-8b-ref table (fix:- code 1)))
		    chars
		    (cons (integer->char (fix:- code 1)) chars)))
	  chars))))

(define (char-set-member? char-set char)
  (if (not (char-set? char-set))
      (error:wrong-type-argument char-set "character set" 'CHAR-SET-MEMBER?))
  (let ((code (char->integer char)))
    (and (fix:< code char-set-table-length)
	 (not (fix:zero? (vector-8b-ref (char-set-table char-set) code))))))

(define (char-set-invert char-set)
  (predicate->char-set
   (lambda (char)
     (not (char-set-member? char-set char)))))

(define (char-set-union . char-sets)
  (predicate->char-set
   (lambda (char)
     (there-exists? char-sets
       (lambda (char-set)
	 (char-set-member? char-set char))))))

(define (char-set-intersection . char-sets)
  (predicate->char-set
   (lambda (char)
     (for-all? char-sets
       (lambda (char-set)
	 (char-set-member? char-set char))))))

(define (char-set-difference include exclude)
  (predicate->char-set
   (lambda (char)
     (and (char-set-member? include char)
	  (not (char-set-member? exclude char))))))

;;;; System Character Sets

(define char-set:upper-case)
(define char-set:lower-case)
(define char-set:numeric)
(define char-set:graphic)
(define char-set:whitespace)
(define char-set:alphabetic)
(define char-set:alphanumeric)
(define char-set:standard)

(define char-set:not-upper-case)
(define char-set:not-lower-case)
(define char-set:not-numeric)
(define char-set:not-graphic)
(define char-set:not-whitespace)
(define char-set:not-alphabetic)
(define char-set:not-alphanumeric)
(define char-set:not-standard)

(define char-set:not-01)
(define char-set:newline)

(define (initialize-package!)
  ;; This must be first:
  (set! char-set:not-01 (ascii-range->char-set #x02 #x100))

  (set! char-set:upper-case (ascii-range->char-set #x41 #x5B))
  (set! char-set:lower-case (ascii-range->char-set #x61 #x7B))
  (set! char-set:numeric (ascii-range->char-set #x30 #x3A))
  (set! char-set:graphic (ascii-range->char-set #x20 #x7F))
  (set! char-set:whitespace
	(char-set #\newline #\tab #\linefeed #\page #\return #\space))
  (set! char-set:alphabetic
	(char-set-union char-set:upper-case char-set:lower-case))
  (set! char-set:alphanumeric
	(char-set-union char-set:alphabetic char-set:numeric))
  (set! char-set:standard
	(char-set-union char-set:graphic (char-set #\newline)))

  (set! char-set:not-upper-case   (char-set-invert char-set:upper-case))
  (set! char-set:not-lower-case   (char-set-invert char-set:lower-case))
  (set! char-set:not-numeric      (char-set-invert char-set:numeric))
  (set! char-set:not-graphic      (char-set-invert char-set:graphic))
  (set! char-set:not-whitespace   (char-set-invert char-set:whitespace))
  (set! char-set:not-alphabetic   (char-set-invert char-set:alphabetic))
  (set! char-set:not-alphanumeric (char-set-invert char-set:alphanumeric))
  (set! char-set:not-standard     (char-set-invert char-set:standard))

  (set! char-set:newline (char-set #\newline))
  unspecific)

(define-integrable (char-upper-case? char)
  (and (fix:<= (char->integer #\A) char)
       (fix:<= char (char->integer #\Z))))

(define-integrable (char-lower-case? char)
  (and (fix:<= (char->integer #\a) char)
       (fix:<= char (char->integer #\z))))

(define-integrable (char-numeric? char)
  (and (fix:<= (char->integer #\0) char)
       (fix:<= char (char->integer #\9))))

(define-integrable (char-graphic? char)
  (char-set-member? char-set:graphic char))

(define-integrable (char-whitespace? char)
  (char-set-member? char-set:whitespace char))

(define-integrable (char-alphabetic? char)
  (char-set-member? char-set:alphabetic char))

(define-integrable (char-alphanumeric? char)
  (char-set-member? char-set:alphanumeric char))

(define-integrable (char-standard? char)
  (char-set-member? char-set:standard char))