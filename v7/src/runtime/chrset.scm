#| -*-Scheme-*-

$Id: chrset.scm,v 14.12 2001/02/05 19:20:12 cph Exp $

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

(define (char-set? object)
  (and (string? object)
       (fix:= (string-length object) 256)
       (not (string-find-next-char-in-set object char-set:not-01))))

(define (guarantee-char-set object procedure)
  (if (not (char-set? object))
      (error:wrong-type-argument object "character set" procedure)))

(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (let ((char-set (string-allocate 256)))
    (vector-8b-fill! char-set 0 256 0)
    (for-each
     (lambda (char)
       (vector-8b-set! char-set
		       (let ((code (char->integer char)))
			 (if (fix:>= code (string-length char-set))
			     (error:bad-range-argument chars 'CHARS->CHAR-SET))
			     code)
		       1))
     chars)
    char-set))

(define (string->char-set string)
  (let ((char-set (string-allocate 256)))
    (vector-8b-fill! char-set 0 256 0)
    (do ((i  (fix:- (string-length string) 1)  (fix:- i 1)))
	((fix:< i 0))
      (vector-8b-set! char-set (vector-8b-ref string i) 1))
    char-set))

(define (ascii-range->char-set lower upper)
  (let ((char-set (string-allocate 256)))
    (vector-8b-fill! char-set 0 lower 0)
    (vector-8b-fill! char-set lower upper 1)
    (vector-8b-fill! char-set upper 256 0)
    char-set))

(define (predicate->char-set predicate)
  (let ((char-set (string-allocate 256)))
    (let loop ((code 0))
      (if (fix:< code 256)
	  (begin (vector-8b-set! char-set code
				 (if (predicate (integer->char code)) 1 0))
		 (loop (fix:+ code 1)))))
    char-set))

(define (char-set-members char-set)
  (guarantee-char-set char-set 'CHAR-SET-MEMBERS)
  (let loop ((code 0))
    (cond ((fix:>= code 256) '())
	  ((fix:zero? (vector-8b-ref char-set code)) (loop (fix:+ code 1)))
	  (else (cons (integer->char code) (loop (fix:+ code 1)))))))

(define (char-set-member? char-set char)
  (guarantee-char-set char-set 'CHAR-SET-MEMBER?)
  (let ((code (char->integer char)))
    (and (fix:< code (string-length char-set))
	 (not (fix:zero? (vector-8b-ref char-set code))))))

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
  (char-set-member? char-set:upper-case char))

(define-integrable (char-lower-case? char)
  (char-set-member? char-set:lower-case char))

(define-integrable (char-numeric? char)
  (char-set-member? char-set:numeric char))

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