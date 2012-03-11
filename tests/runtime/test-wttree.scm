#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Tests of weight-balanced trees

(declare (usual-integrations))

(load-option 'WT-TREE)

(define (make-map low high step)
  (let loop ((i low) (map (make-wt-tree string-wt-type)))
    (if (<= i high)
        (loop (+ i step) (wt-tree/add map (number->string i) i))
        map)))

(define (wt-tree->alist t)
  (wt-tree/fold (lambda (k d r) (cons (cons k d) r)) '() t))

(define (try-all operation trees)
  (map (lambda (t1)
         (map (lambda (t2)
                (operation t1 t2))
              trees))
       trees))

(define (t1) (make-map 0 99 2))
(define (t2) (make-map 1 100 2))
(define (t3) (make-map 0 100 3))

(define-test 'T3-ALIST
  (lambda ()
    ((lambda (alist) (assert-equal alist (wt-tree->alist (t3))))
     '(("0"  . 0)  ("12" . 12) ("15" . 15) ("18" . 18) ("21" . 21) ("24" . 24)
       ("27" . 27) ("3"  . 3)  ("30" . 30) ("33" . 33) ("36" . 36) ("39" . 39)
       ("42" . 42) ("45" . 45) ("48" . 48) ("51" . 51) ("54" . 54) ("57" . 57)
       ("6"  . 6)  ("60" . 60) ("63" . 63) ("66" . 66) ("69" . 69) ("72" . 72)
       ("75" . 75) ("78" . 78) ("81" . 81) ("84" . 84) ("87" . 87) ("9"  . 9)
       ("90" . 90) ("93" . 93) ("96" . 96) ("99" . 99)))))

(define (test-union expected t1 t2)
  (assert-eqv expected (wt-tree/size (wt-tree/union (t1) (t2)))))

(define-test 'T1-UNION-T1 (lambda () (test-union 50 t1 t1)))
(define-test 'T1-UNION-T2 (lambda () (test-union 100 t1 t2)))
(define-test 'T1-UNION-T3 (lambda () (test-union 67 t1 t3)))
(define-test 'T2-UNION-T1 (lambda () (test-union 100 t2 t1)))
(define-test 'T2-UNION-T2 (lambda () (test-union 50 t2 t2)))
(define-test 'T2-UNION-T3 (lambda () (test-union 67 t2 t3)))
(define-test 'T3-UNION-T1 (lambda () (test-union 67 t3 t1)))
(define-test 'T3-UNION-T2 (lambda () (test-union 67 t3 t2)))
(define-test 'T3-UNION-T3 (lambda () (test-union 34 t3 t3)))

(define (test-difference expected t1 t2)
  (assert-eqv expected (wt-tree/size (wt-tree/difference (t1) (t2)))))

(define-test 'T1-MINUS-T1 (lambda () (test-difference 0 t1 t1)))
(define-test 'T1-MINUS-T2 (lambda () (test-difference 50 t1 t2)))
(define-test 'T1-MINUS-T3 (lambda () (test-difference 33 t1 t3)))
(define-test 'T2-MINUS-T1 (lambda () (test-difference 50 t2 t1)))
(define-test 'T2-MINUS-T2 (lambda () (test-difference 0 t2 t2)))
(define-test 'T2-MINUS-T3 (lambda () (test-difference 33 t2 t3)))
(define-test 'T3-MINUS-T1 (lambda () (test-difference 17 t3 t1)))
(define-test 'T3-MINUS-T2 (lambda () (test-difference 17 t3 t2)))
(define-test 'T3-MINUS-T3 (lambda () (test-difference 0 t3 t3)))

(define (test-intersection expected t1 t2)
  (assert-eqv expected (wt-tree/size (wt-tree/intersection (t1) (t2)))))

(define-test 'T1-INTERSECT-T1 (lambda () (test-intersection 50 t1 t1)))
(define-test 'T1-INTERSECT-T2 (lambda () (test-intersection 0 t1 t2)))
(define-test 'T1-INTERSECT-T3 (lambda () (test-intersection 17 t1 t3)))
(define-test 'T2-INTERSECT-T1 (lambda () (test-intersection 0 t2 t1)))
(define-test 'T2-INTERSECT-T2 (lambda () (test-intersection 50 t2 t2)))
(define-test 'T2-INTERSECT-T3 (lambda () (test-intersection 17 t2 t3)))
(define-test 'T3-INTERSECT-T1 (lambda () (test-intersection 17 t3 t1)))
(define-test 'T3-INTERSECT-T2 (lambda () (test-intersection 17 t3 t2)))
(define-test 'T3-INTERSECT-T3 (lambda () (test-intersection 34 t3 t3)))

(define (test-eqdiff expected t1 t2)
  (assert-eqv expected
              (let ((t1 (t1)) (t2 (t2)))
                (wt-tree/set-equal? (wt-tree/difference t1 t2)
                                    (wt-tree/difference t2 t1)))))

(define-test 'T1-EQDIFF-T1 (lambda () (test-eqdiff #t t1 t1)))
(define-test 'T1-EQDIFF-T2 (lambda () (test-eqdiff #f t1 t2)))
(define-test 'T1-EQDIFF-T3 (lambda () (test-eqdiff #f t1 t3)))
(define-test 'T2-EQDIFF-T1 (lambda () (test-eqdiff #f t2 t1)))
(define-test 'T2-EQDIFF-T2 (lambda () (test-eqdiff #t t2 t2)))
(define-test 'T2-EQDIFF-T3 (lambda () (test-eqdiff #f t2 t3)))
(define-test 'T3-EQDIFF-T1 (lambda () (test-eqdiff #f t3 t1)))
(define-test 'T3-EQDIFF-T2 (lambda () (test-eqdiff #f t3 t2)))
(define-test 'T3-EQDIFF-T3 (lambda () (test-eqdiff #t t3 t3)))

(define assert-wt-valid
  (predicate-assertion wt-tree/valid? "valid wt-tree"))

(define (random-wt-tree size)
  (let ((bound (square size)))
    (let loop ((i 0) (tree (make-wt-tree number-wt-type)))
      (if (< i size)
          (loop (+ i 1)
                (let ((n (random-integer bound)))
                  (wt-tree/add tree n n)))
          tree))))

(define-test 'DELETE-MIN-PRESERVES-BALANCE
  (lambda ()
    ;; Eight tries seems good enough to catch the problem reliably.
    ;; This should also test some particular trees, not just randomly
    ;; generated ones.
    (do ((i 0 (+ i 1))) ((>= i 8))
      (do ((i 0 (+ i 1))
           (tree (random-wt-tree #x100) (wt-tree/delete-min tree)))
          ((wt-tree/empty? tree))
        (assert-wt-valid tree)))))
