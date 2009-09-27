#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Tests of regular s-expressions

(declare (usual-integrations))

(define (test-string pattern string expected)
  (assert-equal `(match-string ',pattern ,string)
		 (match-string pattern string)
		 expected))

(define (match-string pattern string)
  (regsexp-match-string (compile-regsexp pattern) string))

(define (assert-equal expr value expected)
  (if (not (equal? value expected))
      (begin
	(fluid-let ((*unparse-abbreviate-quotations?* #t))
	  (write expr))
	(write-string " => ")
	(write value)
	(write-string " but expected ")
	(write expected)
	(newline))))

(define (test-strings pattern entries)
  (for-each (lambda (p)
	      (test-string pattern (car p) (cadr p)))
	    entries))

(define (test-no-groups pattern entries)
  (test-strings pattern
		(map (lambda (p)
		       (list (car p)
			     (and (cadr p)
				  (list (cadr p)))))
		     entries)))

(define (no-groups-tester strings)
  (lambda (pattern indices)
    (test-no-groups pattern
		    (map list strings indices))))

(define (run-tests)
  (test-no-groups '(any-char)
		  '(("" #f)
		    ("a" 1)
		    ("b" 1)
		    ("\n" #f)))

  (test-no-groups '(* (any-char))
		  '(("" 0)
		    ("a" 1)
		    ("ab" 2)
		    ("abc" 3)
		    ("ab\n" 2)
		    ("a\nb" 1)))

  (test-string '(seq "a" "b") "ab" '(2))

  (let ((test (no-groups-tester '("" "a" "b" "ab" "ba" "aab"))))
    (let ((equivalents
	   (lambda (indices . patterns)
	     (for-each (lambda (pattern)
			 (test pattern indices))
		       patterns))))

      (equivalents '(0 0 0 0 0 0)
		   ""
		   '(repeat> 0 0 "a")
		   '(repeat< 0 0 "a")
		   '(seq "" ""))

      (equivalents '(#f 1 #f 1 #f 1)
		   "a"
		   '(repeat> 1 1 "a")
		   '(repeat< 1 1 "a")
		   '(seq "a" "")
		   '(seq "" "a"))

      (equivalents '(#f #f #f #f #f 2)
		   "aa"
		   '(repeat> 2 2 "a")
		   '(repeat< 2 2 "a")
		   '(seq "a" "a")
		   '(seq "aa" "")
		   '(seq "" "aa"))

      (equivalents '(0 1 0 1 0 2)
		   '(* "a")
		   '(repeat> 0 #f "a"))

      (equivalents '(0 0 0 0 0 0)
		   '(*? "a")
		   '(repeat< 0 #f "a"))

      (equivalents '(#f 1 #f 1 #f 2)
		   '(+ "a")
		   '(seq "a" (* "a"))
		   '(repeat> 1 #f "a"))

      (equivalents '(#f 1 #f 1 #f 1)
		   '(+? "a")
		   '(seq "a" (*? "a"))
		   '(repeat< 1 #f "a"))

      (equivalents '(0 1 0 1 0 1)
		   '(? "a")
		   '(repeat> 0 1 "a"))

      (equivalents '(0 0 0 0 0 0)
		   '(?? "a")
		   '(repeat< 0 1 "a"))))

  (test-string '(seq (? "a") "a") "aab" '(2))
  (test-string '(seq (? "a") "ab") "aab" '(3))

  (test-string '(seq (?? "a") "a") "aab" '(1))
  (test-string '(seq (?? "a") "ab") "aab" '(3))

  (test-string '(repeat> 1 2 "a") "aab" '(2))
  (test-string '(seq (repeat> 1 2 "a") "b") "aab" '(3))

  (test-string '(repeat< 1 2 "a") "aab" '(1))
  (test-string '(seq (repeat< 1 2 "a") "b") "aab" '(3))

  (test-string '(repeat> 1 3 "a") "aaab" '(3))
  (test-string '(seq (repeat> 1 3 "a") "b") "aaab" '(4))

  (test-string '(repeat< 1 3 "a") "aaab" '(1))
  (test-string '(seq (repeat< 1 3 "a") "b") "aaab" '(4))

  (test-string '(seq (group foo (? "a")) "a") "aab" '(2 (foo 0 1)))
  (test-string '(seq (group foo (? "a")) "ab") "aab" '(3 (foo 0 1)))
  (test-string '(seq (group foo (? "a")) "aab") "aab" '(3 (foo 0 0)))

  (test-string '(seq (group foo (?? "a")) "a") "aab" '(1 (foo 0 0)))
  (test-string '(seq (group foo (?? "a")) "ab") "aab" '(3 (foo 0 1)))
  (test-string '(seq (group foo (?? "a")) "aab") "aab" '(3 (foo 0 0)))

  (test-string '(seq (group foo (* "a")) "b") "aab" '(3 (foo 0 2)))
  (test-string '(seq (group foo (* "a")) "ab") "aab" '(3 (foo 0 1)))
  (test-string '(seq (group foo (* "a")) "aab") "aab" '(3 (foo 0 0)))

  (test-string '(seq (group foo (*? "a")) "b") "aab" '(3 (foo 0 2)))
  (test-string '(seq (group foo (*? "a")) "ab") "aab" '(3 (foo 0 1)))
  (test-string '(seq (group foo (*? "a")) "aab") "aab" '(3 (foo 0 0)))
  )