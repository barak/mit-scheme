#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/chrset.scm,v 14.2 1988/06/13 11:41:14 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Character Sets
;;; package: (runtime character-set)

(declare (usual-integrations))

(define (char-set? object)
  (and (string? object) (= (string-length object) 256)))

(define (char-set . chars)
  (chars->char-set chars))

(define (chars->char-set chars)
  (let ((char-set (string-allocate 256)))
    (vector-8b-fill! char-set 0 256 0)
    (for-each (lambda (char) (vector-8b-set! char-set (char->ascii char) 1))
	      chars)
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
      (if (< code 256)
	  (begin (vector-8b-set! char-set code
				 (if (predicate (ascii->char code)) 1 0))
		 (loop (1+ code)))))
    char-set))

(define (char-set-members char-set)
  (define (loop code)
    (cond ((>= code 256) '())
	  ((zero? (vector-8b-ref char-set code)) (loop (1+ code)))
	  (else (cons (ascii->char code) (loop (1+ code))))))
  (loop 0))

(define (char-set-member? char-set char)
  (let ((ascii (char-ascii? char)))
    (and ascii (not (zero? (vector-8b-ref char-set ascii))))))

(define (char-set-invert char-set)
  (predicate->char-set
   (lambda (char) (not (char-set-member? char-set char)))))

(define (char-set-union char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (or (char-set-member? char-set-1 char)
	 (char-set-member? char-set-2 char)))))

(define (char-set-intersection char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-member? char-set-1 char)
	  (char-set-member? char-set-2 char)))))

(define (char-set-difference char-set-1 char-set-2)
  (predicate->char-set
   (lambda (char)
     (and (char-set-member? char-set-1 char)
	  (not (char-set-member? char-set-2 char))))))

;;;; System Character Sets

(define char-set:upper-case)
(define char-set:lower-case)
(define char-set:numeric)
(define char-set:graphic)
(define char-set:whitespace)
(define char-set:not-whitespace)
(define char-set:alphabetic)
(define char-set:alphanumeric)
(define char-set:standard)

(define (initialize-package!)
  (set! char-set:upper-case (ascii-range->char-set #x41 #x5B))
  (set! char-set:lower-case (ascii-range->char-set #x61 #x7B))
  (set! char-set:numeric (ascii-range->char-set #x30 #x3A))
  (set! char-set:graphic (ascii-range->char-set #x20 #x7F))
  (set! char-set:whitespace
	(char-set char:newline #\Tab #\Linefeed #\Page #\Return #\Space))
  (set! char-set:not-whitespace (char-set-invert char-set:whitespace))
  (set! char-set:alphabetic
	(char-set-union char-set:upper-case char-set:lower-case))
  (set! char-set:alphanumeric
	(char-set-union char-set:alphabetic char-set:numeric))
  (set! char-set:standard
	(char-set-union char-set:graphic (char-set char:newline))))

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