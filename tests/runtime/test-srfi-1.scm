#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
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

(import (except (scheme base)
		append assoc assq assv caar cadr car cdar cddr cdr cons for-each
		length list list-copy list-ref make-list map member memq memv
		null? pair? reverse set-car! set-cdr!)
	(scheme write)
	(srfi 1)
	(except (srfi 8) call-with-values lambda))

(xcons (list 1) 2)
'(expect equal? '(2 1))

(make-list 3)
'(expect (lambda (expected value)
	   (and (pair? value)
		(pair? (cdr value))
		(pair? (cddr value))
		(null? (cdddr value))))
	 #f)
(make-list 3 5)
'(expect equal? '(5 5 5))

(list-tabulate 5 (lambda (x) (+ x 1)))
'(expect equal? '(1 2 3 4 5))
(list-tabulate 5 square)
'(expect equal? '(0 1 4 9 16))

(cons* 'a 'b (iota 3))
'(expect equal? '(a b 0 1 2))

(let* ((foo (iota 5))
       (bar foo)
       (baz (list-copy foo)))
  (list (eq? foo bar)
	(eq? foo baz)))
'(expect equal? '(#t #f))

(iota 5)
'(expect equal? '(0 1 2 3 4))
(iota 5 10)
'(expect equal? '(10 11 12 13 14))
(iota 5 10 2)
'(expect equal? '(10 12 14 16 18))

;; (iota- 5)
;; '(expect equal? '(1 2 3 4 5))
;; (iota- 5 10)
;; '(expect equal? '(6 7 8 9 10))
;; (iota- 5 10 2)
;; '(expect equal? '(7 9))

;; (-iota 5)
;; '(expect equal? '(0 1 2 3 4))
;; (-iota 5 10)
;; '(expect equal? '(5 6 7 8 9))
;; (-iota 5 10 2)
;; '(expect equal? '(5 7 9))

(fourth (circular-list 1 2 3))
'(expect equal? '1)

(proper-list? (cons 1 (list 2))) 'expect-true
(proper-list? (cons 1 2)) 'expect-false
(proper-list? (circular-list 1 2 3)) 'expect-false

(dotted-list? (cons 1 (list 2))) 'expect-false
(dotted-list? (cons 1 2)) 'expect-true
(dotted-list? (circular-list 1 2 3)) 'expect-false

(circular-list? (cons 1 (list 2))) 'expect-false
(circular-list? (cons 1 2)) 'expect-false
(circular-list? (circular-list 1 2 3)) 'expect-true

(not-pair? 5) 'expect-true
(not-pair? '()) 'expect-true
(not-pair? (circular-list 1 2 3)) 'expect-false

;;; list= (altered)
(list= eq?
       '(a b c)
       '(a b c)
       '(a b c))
'expect-true

(list= eq?
       '("a")
       '("a"))
'expect-false

(list= equal?
       '("a")
       '("a"))
'expect-true

;;; length+
(length (circular-list 1 2 3))
'expect-error

(length+ (circular-list 1 2 3))
'expect-false

(length+ (list 1 2 3))
'(expect equal? '3)

(zip '(1 2 3) '(1 2 3))
'(expect equal? '((1 1) (2 2) (3 3)))

(take-right '(a b c d e) 2)
'(expect equal? '(d e))

;;; drop-right
(drop-right '(a b c d e) 2)
'(expect equal? '(a b c))

;;; drop-right!
(let ((foo '(a b c d e)))
  (let ((bar (drop-right! foo 2)))
    foo))
'(expect equal? '(a b c))

;;; take
(take '(a b c d e) 2)
'(expect equal? '(a b))

(take '(a b c d e) -2)
'expect-error

;;; drop
(drop '(a b c d e) 2)
'(expect equal? '(c d e))

(drop '(a b c d e) -2)
'expect-error

;;; take!
(let ((foo '(a b c d e)))
  (let ((bar (take! foo 2)))
    foo))
'(expect equal? '(a b))

;;; drop! (linear updates not guaranteed to modify their arguments)
;; (let ((foo '(a b c d e)))
;;   (let ((bar (drop! foo 2)))
;;     (list foo
;; 	  bar)))
;; '(expect equal? '((a b c d e) (c d e)))

;;; split-at
(let ((foo '(a b c d e)))
  (receive (x y)
	   (split-at foo 2)
	   (list x y)))
'(expect equal? '((a b) (c d e)))

;;; split-at!
(let ((foo '(a b c d e)))
  (receive (x y)
	   (split-at! foo 2)
	   (list x y foo)))
'(expect equal? '((a b) (c d e) (a b)))

;;; last
(last '(a b c d e))
'(expect equal? 'e)

;;; unzip1-5
(unzip1 '((a b)
	  (c d)
	  (e f)))
'(expect equal? '(a c e))

(receive (a b)
    (unzip2 (list (iota 10 0)
		  (iota 10 10)
		  (iota 10 20)))
  (list a b))
'(expect equal? '((0 10 20) (1 11 21)))


(receive (a b c)
    (unzip3 (list (iota 10 0)
		  (iota 10 10)
		  (iota 10 20)))
  (list a b c))
'(expect equal? '((0 10 20) (1 11 21) (2 12 22)))

(receive (a b c d)
    (unzip4 (list (iota 10 0)
		  (iota 10 10)
		  (iota 10 20)))
  (list a b c d))
'(expect equal? '((0 10 20) (1 11 21) (2 12 22) (3 13 23)))

(receive (a b c d e)
    (unzip5 (list (iota 10 0)
		  (iota 10 10)
		  (iota 10 20)))
  (list a b c d e))
'(expect equal? '((0 10 20) (1 11 21) (2 12 22) (3 13 23) (4 14 24)))

;;; append! append-reverse append-reverse! concatenate concatenate!
(append! '(a b c)
	 '(d e f)
	 '(g h i))
'(expect equal? '(a b c d e f g h i))

(append-reverse '(a b c)
		'(d e f))
'(expect equal? '(c b a d e f))

(append-reverse! '(a b c)
		 '(d e f))
'(expect equal? '(c b a d e f))

(concatenate '((a b c)
	       (d e f)
	       (g h i)))
'(expect equal? '(a b c d e f g h i))

(concatenate! '((a b c)
		(d e f)
		(g h i)))
'(expect equal? '(a b c d e f g h i))


;;; count
(count even? (iota 10))
'(expect equal? '5)

(count (lambda (x y) (even? (+ x y)))
       (iota 10)
       (iota 10))
'(expect equal? '10)

;;; fold/unfold
(unfold-right null-list? car cdr (iota 10))
'(expect equal? '(9 8 7 6 5 4 3 2 1 0))

(unfold null-list? car cdr (iota 10) (lambda (x) (cons 'foo x)))
'(expect equal? '(0 1 2 3 4 5 6 7 8 9 foo))

(fold cons* '() '(a b c) '(1 2 3 4 5))
'(expect equal? '(c 3 b 2 a 1))

(fold-right + 0 (iota 5 1) (iota 5 6))
'(expect equal? '55)

(fold-right cons* '() (iota 10) (iota 20))
'(expect equal? '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9))

(pair-fold cons* '() '(a b c) '(1 2 3 4 5))
'(expect equal? '((c) (3 4 5) (b c) (2 3 4 5) (a b c) (1 2 3 4 5)))

(pair-fold-right cons* '() '(a b c) '(1 2 3 4 5))
'(expect equal? '((a b c) (1 2 3 4 5) (b c) (2 3 4 5) (c) (3 4 5)))

(reduce + 'none (iota 10))
'(expect equal? '45)

(reduce + 'none '())
'(expect equal? 'none)

(reduce-right + 'none (iota 10))
'(expect equal? '45)

(reduce-right + 'none '())
'(expect equal? 'none)

(append-map (lambda (x) (list x (- x))) '(1 3 8))
'(expect equal? '(1 -1 3 -3 8 -8))

(append-map list
	    (iota 5)
	    (iota 5 5))
'(expect equal? '(0 5 1 6 2 7 3 8 4 9))

(append-map! (lambda (x) (list x (- x))) '(1 3 8))
'(expect equal? '(1 -1 3 -3 8 -8))

(pair-for-each (lambda (x)
		 (write x)
		 (newline))
	       (iota 3))
'(expect-output equal?
		'((0 1 2)
		  (1 2)
		  (2)))

(pair-for-each (lambda (x y)
		 (write (list x y))
		 (newline))
	       (iota 3)
	       (iota 3 3))
'(expect-output equal?
		'(((0 1 2) (3 4 5))
		  ((1 2) (4 5))
		  ((2) (5))))

(map! +
      (iota 5)
      (iota 10))
'(expect equal? '(0 2 4 6 8))

(map! +
      (iota 10)
      (iota 5))
'expect-error

(filter-map (lambda (x)
	      (and (even? x)
		   (square x)))
	    (iota 10))
'(expect equal? '(0 4 16 36 64))

(let ((foo '()))
  (map-in-order (lambda (x)
		  (set! foo (cons x foo)))
		(iota 10))
  foo)
'(expect equal? '(9 8 7 6 5 4 3 2 1 0))

;;; filter, remove, partition

(filter even? (iota 10))
'(expect equal? '(0 2 4 6 8))

(filter! even? (iota 10))
'(expect equal? '(0 2 4 6 8))

(remove even? (iota 10))
'(expect equal? '(1 3 5 7 9))

(remove! even? (iota 10))
'(expect equal? '(1 3 5 7 9))

(receive (x y)
    (partition even? (iota 10))
  (list x y))
'(expect equal? '((0 2 4 6 8) (1 3 5 7 9)))

(receive (x y)
    (partition! even? (iota 10))
  (list x y))
'(expect equal? '((0 2 4 6 8) (1 3 5 7 9)))

;;; delete, assoc, member
(delete 3 (iota 5))
'(expect equal? '(0 1 2 4))

(delete 3 (iota 5) eqv?)
'(expect equal? '(0 1 2 4))

(delete! 3 (iota 5))
'(expect equal? '(0 1 2 4))

(delete! 3 (iota 5) eqv?)
'(expect equal? '(0 1 2 4))

(member "b" (list "a" "b" "c"))
'(expect equal? '("b" "c"))

(member "b" (list "a" "b" "c") eqv?)
'expect-false

(delete-duplicates '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9))
'(expect equal? '(0 1 2 3 4 5 6 7 8 9))

(delete-duplicates '(0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9) eqv?)
'(expect equal? '(0 1 2 3 4 5 6 7 8 9))

;;; alist stuff

(let ((e '((a 1) (b 2) (c 3))))
  (list (assq 'a e)
	(assq 'b e)
	(assq 'd e)))
'(expect equal? '((a 1) (b 2) #f))

(assq (list 'a) '(((a)) ((b)) ((c))))
'expect-false

(assoc (list 'a) '(((a)) ((b)) ((c))))
'(expect equal? '((a)))

(assq 5 '((2 3) (5 7) (11 13)))
'(expect equal? '(5 7))
;;; this is R5RS unspecified though

(assv 5 '((2 3) (5 7) (11 13)))
'(expect equal? '(5 7))
;;; but not this

(find even? (iota 10 5))
'(expect equal? '6)

(find-tail even? (iota 10 5))
'(expect equal? '(6 7 8 9 10 11 12 13 14))

(drop-while even? '(2 18 3 10 22 9))
'(expect equal? '(3 10 22 9))

(take-while even? '(2 18 3 10 22 9))
'(expect equal? '(2 18))

(receive (x y)
    (span even? '(2 18 3 10 22 9))
  (list x y))
'(expect equal? '((2 18) (3 10 22 9)))

(receive (x y)
    (span! even? '(2 18 3 10 22 9))
  (list x y))
'(expect equal? '((2 18) (3 10 22 9)))

(any even? (iota 5 1 2))
'expect-false

(any (lambda (x y) (odd? (+ x y)))
     (iota 10)
     (iota 10))
'expect-false

(every odd? (iota 5 1 2))
'expect-true

(every (lambda (x y) (even? (+ x y)))
       (iota 10)
       (iota 10))
'expect-true

(list-index odd? '(2 18 3 10 22 9))
'(expect equal? '2)

(reverse! (iota 10))
'(expect equal? '(9 8 7 6 5 4 3 2 1 0))

;;; lset-*

(lset<= eq? '(a) '(a b a) '(a b c c)) 'expect-true
(lset= eq? '(b e a) '(a e b) '(e e b a)) 'expect-true

(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)
'(expect equal? '(u o i a b c d c e))

(lset-union eq? '(a a c) '(x a x))
'(expect equal? '(x a a c))


(lset-union eq? '(a a c) '(x a x))
'(expect equal? '(x a a c))


(lset-intersection eq? '(a x y a) '(x a x z))
'(expect equal? '(a x a))


(lset-difference eq? '(a b c d e) '(a e i o u))
'(expect equal? '(b c d))


(lset-xor eq? '(a b c d e) '(a e i o u))
'(expect equal? '(u o i b c d))

(receive (x y)
    (lset-diff+intersection eq?
			    '(a b c d)
			    '(a e)
			    '(c e))
  (list x y))
'(expect equal? '((b d) (a c)))

(receive (x y)
    (lset-diff+intersection! eq?
			     '(a b c d)
			     '(a e)
			     '(c e))
  (list x y))
'(expect equal? '((b d) (a c)))
