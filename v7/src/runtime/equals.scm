#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/equals.scm,v 14.3 1991/01/31 07:08:51 hal Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Equality
;;; package: ()

(declare (usual-integrations))

(define (eqv? x y)
  ;; EQV? is officially supposed to work on booleans, characters, and
  ;; numbers specially, but it turns out that EQ? does the right thing
  ;; for everything but numbers, so we take advantage of that.
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (if (number? y)
	      (and (= x y)
		   (boolean=? (exact? x) (exact? y)))
	      (and (object-type? (ucode-type vector) y)
		   (zero? (vector-length x))
		   (zero? (vector-length y))))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))

(define (equal? x y)
  (or (eq? x y)
      (if (object-type? (object-type x) y)
	  (cond ((number? y)
		 (and (= x y)
		      (boolean=? (exact? x) (exact? y))))
		((object-type? (ucode-type list) y)
		 (and (equal? (car x) (car y))
		      (equal? (cdr x) (cdr y))))
		((object-type? (ucode-type vector) y)
		 (let ((size (vector-length x)))
		   (and (= size (vector-length y))
			(let loop ((index 0))
			  (or (= index size)
			      (and (equal? (vector-ref x index)
					   (vector-ref y index))
				   (loop (1+ index))))))))
		((object-type? (ucode-type cell) y)
		 (equal? (cell-contents x) (cell-contents y)))
		((object-type? (ucode-type character-string) y)
		 (string=? x y))
		((object-type? (ucode-type vector-1b) y)
		 (bit-string=? x y))
		(else false))
	  (and (number? x)
	       (number? y)
	       (= x y)
	       (boolean=? (exact? x) (exact? y))))))