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

(import (scheme base)
	(srfi 133))

(define v (make-vector 3 3))

(vector? #(1 2 3)) 'expect-true
(vector? (make-vector 10)) 'expect-true
(vector-ref v 0) '(expect eqv? 3)
(vector-ref v 1) '(expect eqv? 3)
(vector-ref v 2) '(expect eqv? 3)
(vector-ref v -1) 'expect-error
(vector-ref v 3) 'expect-error

(vector-set! v 0 -32)
(vector-ref v 0) '(expect eqv? -32)
(vector-length v) '(expect = 3)
(vector-length '#()) '(expect = 0)

(define a2i '#(a b c d e f g h i))

(vector 0 1 2 3 4) '(expect equal? '#(0 1 2 3 4))
(vector-copy a2i) '(expect equal? a2i)
(vector-copy a2i) '(expect-not eqv? a2i)

(vector-unfold (lambda (i x) (declare (ignore i)) (values x (- x 1))) 10 0)
'(expect equal? '#(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
(vector-unfold values 7)
'(expect equal? '#(0 1 2 3 4 5 6))
(vector-unfold-right (lambda (i x) (values (cons i x) (+ x 1))) 5 0)
'(expect equal? '#((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0)))

(vector-copy a2i 6) '(expect equal? '#(g h i))
(vector-copy a2i 3 6) '(expect equal? '#(d e f))
(vector-reverse-copy '#(5 4 3 2 1 0) 1 5) '(expect equal? '#(1 2 3 4))

(vector-append '#(x) '#(y)) '(expect equal? '#(x y))
(vector-append '#(a) '#(b c d)) '(expect equal? '#(a b c d))
(vector-append '#(a #(b)) '#(#(c))) '(expect equal? '#(a #(b) #(c)))
(vector-concatenate '(#(a b) #(c d))) '(expect equal? '#(a b c d))

(vector-append-subvectors '#(a b c d e) 0 2 '#(f g h i j) 2 4)
'(expect equal? '#(a b h i))

(vector-empty? '#(a)) 'expect-false
(vector-empty? '#(())) 'expect-false
(vector-empty? '#(#())) 'expect-false
(vector-empty? '#()) 'expect-true
(vector= eq? '#(a b c d) '#(a b c d)) 'expect-true
(vector= eq? '#(a b c d) '#(a b d c)) 'expect-false
(vector= = '#(1 2 3 4 5) '#(1 2 3 4)) 'expect-false
(vector= = '#(+nan.0) '#(+nan.0)) 'expect-false
(let ((nan '+nan.0)) (vector= = (vector nan) (vector nan))) 'expect-false
(let ((nanvec '#(+nan.0))) (vector= = nanvec nanvec)) 'expect-false
(vector= eq?) 'expect-true
(vector= eq? '#(a)) 'expect-true
(vector= eq? (vector (vector 'a)) (vector (vector 'a))) 'expect-false
(vector= equal? (vector (vector 'a)) (vector (vector 'a))) 'expect-true

(define vos '#("abc" "abcde" "abcd"))
(define vec '#(0 1 2 3 4 5))
(define vec2 (vector 0 1 2 3 4))
(define vec3 (vector 1 2 3 4 5))
(define result '())
(define (sqr x) (* x x))
(vector-fold (lambda (len str) (max (string-length str) len)) 0 vos)
'(expect eqv? 5)
(vector-fold (lambda (tail elt) (cons elt tail)) '() vec)
'(expect equal? '(5 4 3 2 1 0))
(vector-fold (lambda (ctr n) (if (even? n) (+ ctr 1) ctr)) 0 vec)
'(expect eqv? 3)
(vector-fold-right (lambda (tail elt) (cons elt tail)) '() '#(a b c d))
'(expect equal? '(a b c d))
(vector-map sqr '#(1 2 3 4))
'(expect equal? '#(1 4 9 16))
(vector-map * '#(1 2 3 4 5) '#(5 4 3 2 1))
'(expect equal? '#(5 8 9 8 5))
(vector-map! sqr vec2)
(vector-copy vec2)
'(expect equal? '#(0 1 4 9 16))
(vector-map! * vec2 vec3)
(vector-copy vec2)
'(expect equal? '#(0 2 12 36 80))
(vector-for-each (lambda (x) (set! result (cons x result))) vec)
(cons (car result) (cdr result))
'(expect equal? '(5 4 3 2 1 0))
(vector-count even? '#(3 1 4 1 5 9 2 5 6))
'(expect eqv? 3)
(vector-count < '#(1 3 6 9) '#(2 4 6 8 10 12))
'(expect eqv? 2)
(vector-cumulate + 0 '#(3 1 4 1 5 9 2 5 6))
'(expect equal? '#(3 4 8 9 14 23 25 30 36))

(define (cmp a b)
  (cond
     ((< a b) -1)
     ((= a b) 0)
     (else 1)))
(define v '#(0 2 4 6 8 10 12))
(vector-index even? '#(3 1 4 1 5 9 6)) '(expect eqv? 2)
(vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2)) '(expect eqv? 1)
(vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2)) 'expect-false
(vector-index-right odd? '#(3 1 4 1 5 9 6)) '(expect eqv? 5)
(vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2)) '(expect eqv? 3)
(vector-skip number? '#(1 2 a b 3 4 c d)) '(expect eqv? 2)
(vector-skip = '#(1 2 3 4 5) '#(1 2 -3 4)) '(expect eqv? 2)
(vector-skip-right number? '#(1 2 a b 3 4 c d)) '(expect eqv? 7)
(vector-skip-right = '#(1 2 3 4 5) '#(1 2 -3 -4 5)) '(expect eqv? 3)
(vector-binary-search v 0 cmp) '(expect eqv? 0)
(vector-binary-search v 6 cmp) '(expect eqv? 3)
(vector-binary-search v 1 cmp) 'expect-false
(vector-any number? '#(1 2 x y z)) 'expect-true
(vector-any < '#(1 2 3 4 5) '#(2 1 3 4 5)) 'expect-true
(vector-any number? '#(a b c d e)) 'expect-false
(vector-any > '#(1 2 3 4 5) '#(1 2 3 4 5)) 'expect-false
(vector-every number? '#(1 2 x y z)) 'expect-false
(vector-every number? '#(1 2 3 4 5)) 'expect-true
(vector-every < '#(1 2 3) '#(2 3 3)) 'expect-false
(vector-every < '#(1 2 3) '#(2 3 4)) 'expect-true
(vector-any (lambda (x) (if (number? x) 'yes #f)) '#(1 2 x y z))
 '(expect eqv? 'yes)

(define vp
  (let-values (((new off) (vector-partition number? '#(1 x 2 y 3 z))))
    (cons new off)))
(vector-copy (car vp)) '(expect equal? '#(1 2 3 x y z))
(cdr vp) '(expect eqv? 3)

(define vs (vector 1 2 3))
(define vf0 (vector 1 2 3))
(define vf1 (vector 1 2 3))
(define vf2 (vector 1 2 3))
(define vr0 (vector 1 2 3))
(define vr1 (vector 1 2 3))
(define vr2 (vector 1 2 3))
(define vc0 (vector 1 2 3 4 5))
(define vc1 (vector 1 2 3 4 5))
(define vc2 (vector 1 2 3 4 5))
(define vrc0 (vector 1 2 3 4 5))
(define vrc1 (vector 1 2 3 4 5))
(define vrc2 (vector 1 2 3 4 5))
(define vu0 (vector 1 2 3 4 5))
(define vu1 (vector 1 2 3 4 5))
(define vu2 (vector 1 2 3 4 5))
(define vur0 (vector 1 2 3 4 5))
(define vur1 (vector 1 2 3 4 5))
(define vur2 (vector 1 2 3 4 5))
(vector-swap! vs 0 1)
(vector-copy vs)
'(expect equal? '#(2 1 3))
(vector-fill! vf0 0)
(vector-copy vf0)
'(expect equal? '#(0 0 0))
(vector-fill! vf1 0 1)
(vector-copy vf1)
'(expect equal? '#(1 0 0))
(vector-fill! vf2 0 0 1)
(vector-copy vf2)
'(expect equal? '#(0 2 3))
(vector-reverse! vr0)
(vector-copy vr0)
'(expect equal? '#(3 2 1))
(vector-reverse! vr1 1)
(vector-copy vr1)
'(expect equal? '#(1 3 2))
(vector-reverse! vr2 0 2)
(vector-copy vr2)
'(expect equal? '#(2 1 3))
(vector-copy! vc0 1 '#(10 20 30))
(vector-copy vc0)
'(expect equal? '#(1 10 20 30 5))
(vector-copy! vc1 1 '#(0 10 20 30 40) 1)
(vector-copy vc1)
'(expect equal? '#(1 10 20 30 40))
(vector-copy! vc2 1 '#(0 10 20 30 40) 1 4)
(vector-copy vc2)
'(expect equal? '#(1 10 20 30 5))
(vector-reverse-copy! vrc0 1 '#(10 20 30))
(vector-copy vrc0)
'(expect equal? '#(1 30 20 10 5))
(vector-reverse-copy! vrc1 1 '#(0 10 20 30 40) 1)
(vector-copy vrc1)
'(expect equal? '#(1 40 30 20 10))
(vector-reverse-copy! vrc2 1 '#(0 10 20 30 40) 1 4)
(vector-copy vrc2)
'(expect equal? '#(1 30 20 10 5))
(vector-unfold! (lambda (i) (+ 10 i)) vu0 1 4)
(vector-copy vu0)
'(expect equal? '#(1 11 12 13 5))
(vector-unfold! (lambda (i x) (values (+ i x) (+ x 1))) vu1 1 4 0)
(vector-copy vu1)
'(expect equal? '#(1 1 3 5 5))
(vector-unfold! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1))) vu2 1 4 0 0)
(vector-copy vu2)
'(expect equal? '#(1 1 4 7 5))
(vector-unfold-right! (lambda (i) (+ 10 i)) vur0 1 4)
(vector-copy vur0)
'(expect equal? '#(1 11 12 13 5))
(vector-unfold-right! (lambda (i x) (values (+ i x) (+ x 1))) vur1 1 4 0)
(vector-copy vur1)
'(expect equal? '#(1 3 3 3 5))
(vector-unfold-right! (lambda (i x y) (values (+ i x y) (+ x 1) (+ x 1)))
		      vur2 1 4 0 0)
(vector-copy vur2)
'(expect equal? '#(1 5 4 3 5))

(vector->list '#(1 2 3)) '(expect equal? '(1 2 3))
(vector->list '#(1 2 3) 1) '(expect equal? '(2 3))
(vector->list '#(1 2 3) 0 2) '(expect equal? '(1 2))
(list->vector '(1 2 3)) '(expect equal? '#(1 2 3))
(reverse-vector->list '#(1 2 3)) '(expect equal? '(3 2 1))
(reverse-vector->list '#(1 2 3) 1) '(expect equal? '(3 2))
(reverse-vector->list '#(1 2 3) 0 2) '(expect equal? '(2 1))
(reverse-list->vector '(1 2 3)) '(expect equal? '#(3 2 1))
(vector->string '#(#\a #\b #\c)) '(expect equal? "abc")
(vector->string '#(#\a #\b #\c) 1) '(expect equal? "bc")
(vector->string '#(#\a #\b #\c) 0 2) '(expect equal? "ab")
(string->vector "abc") '(expect equal? '#(#\a #\b #\c))
(string->vector "abc" 1) '(expect equal? '#(#\b #\c))
(string->vector "abc" 0 2) '(expect equal? '#(#\a #\b))
