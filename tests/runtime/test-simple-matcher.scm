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

;;;; Tests of simple pattern matcher

(declare (usual-integrations))

(define (test-case pattern expected-names dv-pairs)
  (lambda ()
    (let ((matcher (make-simple-matcher pattern)))
      (assert-equal expected-names (simple-matcher-names matcher))
      (for-each (lambda (p)
		  (let ((datum (car p))
			(expected-values (cadr p)))
		    (assert-equal expected-values
				  (apply-simple-matcher matcher datum))))
		dv-pairs))))

(define (all-results-test-case pattern expected-names datum expected-results)
  (lambda ()
    (let ((matcher (make-simple-matcher pattern)))
      (assert-equal expected-names (simple-matcher-names matcher))
      (let ((all-results '()))
	(apply-simple-matcher matcher datum
			      (lambda (result)
				(set! all-results (cons result all-results))
				#f))
	(assert-equal expected-results (reverse all-results))))))

(define-test 'basic
  (list (test-case 3 '() '((3 ()) (2 #f)))
	(test-case '() '() '((() ())))
	(test-case '(? a) '(a) '((3 (3))))
	(test-case '((? a))
		   '(a)
		   '((() #f)
		     ((3) (3))
		     ((3 4) #f)))
	(test-case '((? a) 3)
		   '(a)
		   '((() #f)
		     ((3) #f)
		     ((3 4) #f)
		     ((4 3) (4))))
	(test-case '((? a) (?? b) (? c))
		   '(a b c)
		   '((() #f)
		     ((3) #f)
		     ((3 4) (3 () 4))
		     ((3 4 5) (3 (4) 5))
		     ((3 4 5 6) (3 (4 5) 6))))
	(test-case '((?? a) b (?? c))
		   '(a c)
		   '((() #f)
		     ((b) (() ()))
		     ((b 3) (() (3)))
		     ((3 b) ((3) ()))
		     ((3 b 4) ((3) (4)))
		     ((3 4 b 5) ((3 4) (5)))))
	(test-case '(a ((? b) 2 3) 1 c)
		   '(b)
		   '(((a (1 2 3) 1 c) (1))))
	(test-case '(a ((? b) 2 3) (? b) c)
		   '(b)
		   '(((a (1 2 3) 2 c) #f)
		     ((a (1 2 3) 1 c) (1))))
	(all-results-test-case '(a (?? x) (?? y) (?? x) c)
			       '(x y)
			       '(a b b b b b b c)
			       '((() (b b b b b b))
				 ((b) (b b b b))
				 ((b b) (b b))
				 ((b b b) ())))
	))

(define-test 'anonymous-vars
  (list (test-case '(?) '() '((3 ())))
	(test-case '((?))
		   '()
		   '((() #f)
		     ((3) ())
		     ((3 4) #f)))
	(test-case '((?) 3)
		   '()
		   '((() #f)
		     ((3) #f)
		     ((3 4) #f)
		     ((4 3) ())))
	(test-case '((?) (??) (?))
		   '()
		   '((() #f)
		     ((3) #f)
		     ((3 4) ())
		     ((3 4 5) ())
		     ((3 4 5 6) ())))
	(test-case '((??) b (??))
		   '()
		   '((() #f)
		     ((b) ())
		     ((b 3) ())
		     ((3 b) ())
		     ((3 b 4) ())
		     ((3 4 b 5) ())))
	(test-case '(a ((?) 2 3) 1 c)
		   '()
		   '(((a (1 2 3) 1 c) ())))
	(test-case '(a ((?) 2 3) (?) c)
		   '()
		   '(((a (1 2 3) 2 c) ())
		     ((a (1 2 3) 1 c) ())))
	))

(define-test 'restricted-vars
  (list (test-case `(? a ,number?) '(a)
		   '((3 (3))
		     (#\3 #f)))
	(test-case `((? a ,number?))
		   '(a)
		   '((() #f)
		     ((3) (3))
		     ((#\3) #f)
		     ((3 4) #f)))
	(test-case `((? a ,number?) 3)
		   '(a)
		   '((() #f)
		     ((3) #f)
		     ((3 4) #f)
		     ((4 3) (4))
		     ((#\4 3) #f)))
	(test-case `((? a ,number?) (?? b) (? c))
		   '(a b c)
		   '((() #f)
		     ((3) #f)
		     ((3 4) (3 () 4))
		     ((3 4 5) (3 (4) 5))
		     ((3 4 5 6) (3 (4 5) 6))
		     ((#\3 4) #f)
		     ((3 #\4) (3 () #\4))))
	(test-case `(a ((? b ,number?) 2 3) (? b) c)
		   '(b)
		   '(((a (1 2 3) 2 c) #f)
		     ((a (1 2 3) 1 c) (1))
		     ((a (#\1 2 3) #\1 c) #f)))
	;; Restriction ignored in back-references:
	(test-case `(a ((? b) 2 3) (? b ,number?) c)
		   '(b)
		   '(((a (1 2 3) 2 c) #f)
		     ((a (1 2 3) 1 c) (1))
		     ((a (#\1 2 3) #\1 c) (#\1))))
	))