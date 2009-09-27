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

(define (match-string pattern string)
  (regsexp-match-string (compile-regsexp pattern) string))

(define ((match-string-test pattern string expected))
  (assert-equal (match-string pattern string)
		expected
		'EXPRESSION `(match-string ',pattern ,string)))

(define (match-strings-test pattern entries)
  (map (lambda (p)
	 (match-string-test pattern (car p) (cadr p)))
       entries))

(define (no-groups-test pattern entries)
  (match-strings-test pattern
		      (map (lambda (p)
			     (list (car p)
				   (and (cadr p)
					(list (cadr p)))))
			   entries)))

(define-test 'any-char
  (no-groups-test '(any-char)
		  '(("" #f)
		    ("a" 1)
		    ("b" 1)
		    ("\n" #f))))

(define-test '*any-char
  (no-groups-test '(* (any-char))
		  '(("" 0)
		    ("a" 1)
		    ("ab" 2)
		    ("abc" 3)
		    ("ab\n" 2)
		    ("a\nb" 1))))

(define-test 'simple-seq
  (match-string-test '(seq "a" "b") "ab" '(2)))

(define-test 'repeat-equivalences-test
  (let ((equivalents
	 (lambda (indices . patterns)
	   (map (let ((strings '("" "a" "b" "ab" "ba" "aab")))
		  (lambda (pattern)
		    (no-groups-test pattern
				    (map list
					 strings
					 indices))))
		patterns))))
    (list
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
		  '(repeat< 0 1 "a")))))

(define-test 'more-repeat-tests
  (list
   (match-string-test '(seq (? "a") "a") "aab" '(2))
   (match-string-test '(seq (? "a") "ab") "aab" '(3))

   (match-string-test '(seq (?? "a") "a") "aab" '(1))
   (match-string-test '(seq (?? "a") "ab") "aab" '(3))

   (match-string-test '(repeat> 1 2 "a") "aab" '(2))
   (match-string-test '(seq (repeat> 1 2 "a") "b") "aab" '(3))

   (match-string-test '(repeat< 1 2 "a") "aab" '(1))
   (match-string-test '(seq (repeat< 1 2 "a") "b") "aab" '(3))

   (match-string-test '(repeat> 1 3 "a") "aaab" '(3))
   (match-string-test '(seq (repeat> 1 3 "a") "b") "aaab" '(4))

   (match-string-test '(repeat< 1 3 "a") "aaab" '(1))
   (match-string-test '(seq (repeat< 1 3 "a") "b") "aaab" '(4))

   (match-string-test '(seq (group foo (? "a")) "a") "aab" '(2 (foo 0 1)))
   (match-string-test '(seq (group foo (? "a")) "ab") "aab" '(3 (foo 0 1)))
   (match-string-test '(seq (group foo (? "a")) "aab") "aab" '(3 (foo 0 0)))

   (match-string-test '(seq (group foo (?? "a")) "a") "aab" '(1 (foo 0 0)))
   (match-string-test '(seq (group foo (?? "a")) "ab") "aab" '(3 (foo 0 1)))
   (match-string-test '(seq (group foo (?? "a")) "aab") "aab" '(3 (foo 0 0)))

   (match-string-test '(seq (group foo (* "a")) "b") "aab" '(3 (foo 0 2)))
   (match-string-test '(seq (group foo (* "a")) "ab") "aab" '(3 (foo 0 1)))
   (match-string-test '(seq (group foo (* "a")) "aab") "aab" '(3 (foo 0 0)))

   (match-string-test '(seq (group foo (*? "a")) "b") "aab" '(3 (foo 0 2)))
   (match-string-test '(seq (group foo (*? "a")) "ab") "aab" '(3 (foo 0 1)))
   (match-string-test '(seq (group foo (*? "a")) "aab") "aab" '(3 (foo 0 0)))

   ))