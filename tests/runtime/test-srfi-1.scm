#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
    Technology

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

;;;; Informal tests of SRFI-1 implementation

(declare (usual-integrations))

;;; xcons
(xcons (list 1) 2)
;Value 1: (2 1)

;;; make-list
(make-list 3)
;Value 2: (#f #f #f)

(make-list 3 5)
;Value 3: (5 5 5)

;;; list-tablulate
(list-tabulate 5 (lambda (x) (+ x 1)))
;Value 7: (1 2 3 4 5)

(list-tabulate 5 square)
;Value 6: (0 1 4 9 16)

;;; cons*
(cons* 'a 'b (iota 3))
;Value 8: (a b 0 1 2)

;;; list-copy
(let* ((foo (iota 5))
       (bar foo)
       (baz (list-copy foo)))
  (list (eq? foo bar)
	(eq? foo baz)))
;Value 9: (#t #f)

;;; iota, 
(iota 5)
;Value 10: (0 1 2 3 4)

(iota 5 10)
;Value 11: (10 11 12 13 14)

(iota 5 10 2)
;Value 12: (10 12 14 16 18)

;;; iota-
(iota- 5)
;Value 13: (1 2 3 4 5)

(iota- 5 10)
;Value 14: (6 7 8 9 10)

(iota- 5 10 2)
;Value 15: (7 9)

;;; -iota
(-iota 5)
;Value 16: (0 1 2 3 4)

(-iota 5 10)
;Value 17: (5 6 7 8 9)

(-iota 5 10 2)
;Value 18: (5 7 9)

;;; circular-list
(fourth (circular-list 1 2 3))
;Value: 1

;;; proper-list?
(proper-list? (cons 1 (list 2)))
;Value: #t

(proper-list? (cons 1 2))
;Value: #f

(proper-list? (circular-list 1 2 3))
;Value: #f

;;; dotted-list?
(dotted-list? (cons 1 (list 2)))
;Value: #f

(dotted-list? (cons 1 2))
;Value: #t

(dotted-list? (circular-list 1 2 3))
;Value: #f

;;; circular-list?
(circular-list? (cons 1 (list 2)))
;Value: #f

(circular-list? (cons 1 2))
;Value: #f

(circular-list? (circular-list 1 2 3))
;Value: #t

;;; not-pair?
(not-pair? 5)
;Value: #t

(not-pair? '())
;Value: #t

(not-pair? (circular-list 1 2 3))
;Value: #f

;;; list= (altered)
(list= eq?
       '(a b c)
       '(a b c)
       '(a b c))
;Value: #t

(list= eq?
       '("a")
       '("a"))
;Value: #f

(list= equal?
       '("a")
       '("a"))
;Value: #t

;;; length+
(length (circular-list 1 2 3))
;The object (1 2 3 1 2 3 1 2 3...), passed as an argument to length, is not a list.
;To continue, call RESTART with an option number:
; (RESTART 2) => Return to read-eval-print level 2.
; (RESTART 1) => Return to read-eval-print level 1.

(length+ (circular-list 1 2 3))
;Value: #f

(length+ (list 1 2 3))
;Value: 3

;;; zip
(zip '(1 2 3) '(1 2 3))
;Value 2: ((1 1) (2 2) (3 3))


;;; take-right
(take-right '(a b c d e) 2)
;Value 5: (d e)


;;; drop-right
(drop-right '(a b c d e) 2)
;Value 6: (a b c)


;;; drop-right!
(let ((foo '(a b c d e)))
  (let ((bar (drop-right! foo 2)))
    foo))
;Value 7: (a b c)


;;; take
(take '(a b c d e) 2)
;Value 8: (a b)

(take '(a b c d e) -2)
;Value 9: (d e)


;;; drop
(drop '(a b c d e) 2)
;Value 15: (c d e)

(drop '(a b c d e) -2)
;Value 16: (a b c)


;;; take!
(let ((foo '(a b c d e)))
  (let ((bar (take! foo 2)))
    foo))
;Value 10: (a b)

;;; drop! (linear updates not guaranteed to modify their arguments)
(let ((foo '(a b c d e)))
  (let ((bar (drop! foo 2)))
    (list foo
	  bar)))
;Value 14: ((a b c d e) (c d e))

;;; split-at
(let ((foo '(a b c d e)))
  (receive (x y)
	   (split-at foo 2)
	   (list x y)))
;Value 17: ((a b) (c d e))

;;; split-at!
(let ((foo '(a b c d e)))
  (receive (x y)
	   (split-at! foo 2)
	   (list x y foo)))
;Value 18: ((a b) (c d e) (a b))


;;; last
(last '(a b c d e))
;Value: e


;;; unzip1-5
(unzip1 '((a b)
	  (c d)
	  (e f)))
;Value 19: (a c e)

(receive (a b)
	 (unzip2 (list (iota 10 0)
		       (iota 10 10)
		       (iota 10 20)))
	 (list a b))
;Value 34: ((0 10 20) (1 11 21))


(receive (a b c)
	 (unzip3 (list (iota 10 0)
		       (iota 10 10)
		       (iota 10 20)))
	 (list a b c))
;Value 35: ((0 10 20) (1 11 21) (2 12 22))

(receive (a b c d)
	 (unzip4 (list (iota 10 0)
		       (iota 10 10)
		       (iota 10 20)))
	 (list a b c d))
;Value 39: ((0 10 20) (1 11 21) (2 12 22) (3 13 23))

(receive (a b c d e)
	 (unzip5 (list (iota 10 0)
		       (iota 10 10)
		       (iota 10 20)))
	 (list a b c d e))
;Value 40: ((0 10 20) (1 11 21) (2 12 22) (3 13 23) (4 14 24))


;;; append! append-reverse append-reverse! concatenate concatenate!
(append! '(a b c)
	 '(d e f)
	 '(g h i))
;Value 41: (a b c d e f g h i)

(append-reverse '(a b c)
		'(d e f))
;Value 42: (c b a d e f)

(append-reverse! '(a b c)
		 '(d e f))
;Value 43: (c b a d e f)

(concatenate '((a b c)
	       (d e f)
	       (g h i)))
;Value 47: (a b c d e f g h i)

(concatenate! '((a b c)
		(d e f)
		(g h i)))
;Value 48: (a b c d e f g h i)


;;; fold/map internal utilities
(%cdrs '((a b c)
	 (d e f)
	 (g h i)))
;Value 49: ((b c) (e f) (h i))

(%cars+ '((a b c)
	  (d e f)
	  (g h i))
	0)
;Value 51: (a d g 0)

(receive (x y)
	 (%cars+cdrs '((a b c)
		       (d e f)
		       (g h i)))
	 (list x y))
;Value 53: ((a d g) ((b c) (e f) (h i)))

(receive (x y)
	 (%cars+cdrs '((a b c)
		       (d e f)
		       ()))
	 (list x y))
;Value 5: (() ())


(receive (x y)
	 (%cars+cdrs+ '((a b c)
			(d e f)
			(g h i))
		      0)
	 (list x y))
;Value 54: ((a d g 0) ((b c) (e f) (h i)))

(receive (x y)
	 (%cars+cdrs+ '((a b c)
			(d e f)
			())
		      0)
	 (list x y))
;Value 6: (() ())


(receive (x y)
	 (%cars+cdrs/no-test '((a b c)
			       (d e f)
			       (g h i)))
	 (list x y))
;Value 55: ((a d g) ((b c) (e f) (h i)))


(receive (x y)
	 (%cars+cdrs/no-test '((a b c)
			       (d e f)
			       ()))
	 (list x y))

;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

;;; count
(count even? (iota 10))
;Value: 5

(count (lambda (x y) (even? (+ x y)))
       (iota 10)
       (iota 10))
;Value: 10

;;; fold/unfold
(unfold-right null-list? car cdr (iota 10))
;Value 59: (9 8 7 6 5 4 3 2 1 0)

(unfold null-list? car cdr (iota 10) (lambda (x) (cons 'foo x)))
;Value 60: (0 1 2 3 4 5 6 7 8 9 foo)


(fold cons* '() '(a b c) '(1 2 3 4 5))
;Value 7: (c 3 b 2 a 1)


(fold-right + 0 (iota 5 1) (iota 5 6))
;Value: 55

(fold-right cons* '() (iota 10) (iota 20))
;Value 69: (0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9)


(pair-fold cons* '() '(a b c) '(1 2 3 4 5))
;Value 8: ((c) (3 4 5) (b c) (2 3 4 5) (a b c) (1 2 3 4 5))

(pair-fold-right cons* '() '(a b c) '(1 2 3 4 5))
;Value 9: ((a b c) (1 2 3 4 5) (b c) (2 3 4 5) (c) (3 4 5))

(reduce + 'none (iota 10))
;Value: 45

(reduce + 'none '())
;Value: none

(reduce-right + 'none (iota 10))
;Value: 45

(reduce-right + 'none '())
;Value: none

(append-map (lambda (x) (list x (- x))) '(1 3 8))
;Value 12: (1 -1 3 -3 8 -8)

(append-map list
	    (iota 5)
	    (iota 5 5))
;Value 15: (0 5 1 6 2 7 3 8 4 9)

(append-map! (lambda (x) (list x (- x))) '(1 3 8))
;Value 13: (1 -1 3 -3 8 -8)


(pair-for-each write-line (iota 3))
; (0 1 2)
; (1 2)
; (2)
; ;Unspecified return value

(pair-for-each (lambda (x y) (write-line (list x y)))
	       (iota 3)
	       (iota 3 3))
; ((0 1 2) (3 4 5))
; ((1 2) (4 5))
; ((2) (5))
; ;Unspecified return value

(map! +
      (iota 5)
      (iota 10))
;Value 16: (0 2 4 6 8)

(map! +
      (iota 10)
      (iota 5))
;The object (), passed as the first argument to cdr, is not the correct type.
;To continue, call RESTART with an option number:
; (RESTART 2) => Specify an argument to use in its place.
; (RESTART 1) => Return to read-eval-print level 1.

(filter-map (lambda (x) (and (even? x)
			(square x)))
	    (iota 10))
;Value 17: (0 4 16 36 64)

(let ((foo '()))
  (map-in-order (lambda (x) (set! foo (cons x foo)))
		(iota 10))
  foo)
;Value 19: (9 8 7 6 5 4 3 2 1 0)


;;; filter, remove, partition

(filter even? (iota 10))
;Value 20: (0 2 4 6 8)

(filter! even? (iota 10))
;Value 22: (0 2 4 6 8)

(remove even? (iota 10))
;Value 21: (1 3 5 7 9)

(remove! even? (iota 10))
;Value 23: (1 3 5 7 9)

(receive (x y)
	 (partition even? (iota 10))
	 (list x y))
;Value 24: ((0 2 4 6 8) (1 3 5 7 9))

(receive (x y)
	 (partition! even? (iota 10))
	 (list x y))
;Value 25: ((0 2 4 6 8) (1 3 5 7 9))


;;; delete, assoc, member
(delete 3 (iota 5))
;Value 26: (0 1 2 4)

(delete 3 (iota 5) eqv?)
;Value 49: (0 1 2 4)

(delete! 3 (iota 5))
;Value 27: (0 1 2 4)

(delete! 3 (iota 5) eqv?)
;Value 50: (0 1 2 4)


(member "b" (list "a" "b" "c"))
;Value 29: ("b" "c")

(member "b" (list "a" "b" "c") eqv?)
;Value: #f


(delete-duplicates '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9))
;Value 52: (0 1 2 3 4 5 6 7 8 9)

(delete-duplicates '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9) eqv?)
;Value 53: (0 1 2 3 4 5 6 7 8 9)


;;; alist stuff

(let ((e '((a 1) (b 2) (c 3))))
  (list (assq 'a e)
	(assq 'b e)
	(assq 'd e)))
;Value 54: ((a 1) (b 2) #f)


(assq (list 'a) '(((a)) ((b)) ((c))))
;Value: #f

(assoc (list 'a) '(((a)) ((b)) ((c))))
;Value 55: ((a))


(assq 5 '((2 3) (5 7) (11 13)))
;Value 56: (5 7)
;;; this is R5RS unspecified though

(assv 5 '((2 3) (5 7) (11 13)))
;Value 57: (5 7)
;;; but not this


;;; find find-tail take-while drop-while span break any every list-index


(find even? (iota 10 5))
;Value: 6

(find-tail even? (iota 10 5))
;Value 58: (6 7 8 9 10 11 12 13 14)

(drop-while even? '(2 18 3 10 22 9))
;Value 59: (3 10 22 9)

(take-while even? '(2 18 3 10 22 9))
;Value 60: (2 18)

(receive (x y)
	 (span even? '(2 18 3 10 22 9))
	 (list x y))
;Value 61: ((2 18) (3 10 22 9))


(receive (x y)
	 (span! even? '(2 18 3 10 22 9))
	 (list x y))
;Value 62: ((2 18) (3 10 22 9))


(any even? (iota 5 1 2))
;Value: #f

(any (lambda (x y) (odd? (+ x y)))
     (iota 10)
     (iota 10))
;Value: #f

(every odd? (iota 5 1 2))
;Value: #t

(every (lambda (x y) (even? (+ x y)))
       (iota 10)
       (iota 10))
;Value: #t

(list-index odd? '(2 18 3 10 22 9))
;Value: 2


;;; reverse!

(reverse! (iota 10))
;Value 66: (9 8 7 6 5 4 3 2 1 0)


;;; lset-*

(lset<= eq? '(a) '(a b a) '(a b c c))
;Value: #t

(lset= eq? '(b e a) '(a e b) '(e e b a)) => #t
;Value: #t


(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u) 
;Value 67: (u o i a b c d c e)


(lset-union eq? '(a a c) '(x a x))
;Value 68: (x a a c)


(lset-union eq? '(a a c) '(x a x))
;Value 69: (x a a c)


(lset-intersection eq? '(a x y a) '(x a x z))
;Value 70: (a x a)


(lset-difference eq? '(a b c d e) '(a e i o u))
;Value 71: (b c d)


(lset-xor eq? '(a b c d e) '(a e i o u))
;Value 72: (u o i b c d)


(receive (x y)
	 (lset-diff+intersection eq?
				 '(a b c d)
				 '(a e)
				 '(c e))
	 (list x y))
;Value 75: ((b d) (a c))

(receive (x y)
	 (lset-diff+intersection! eq?
				  '(a b c d)
				  '(a e)
				  '(c e))
	 (list x y))
;Value 76: ((b d) (a c))
