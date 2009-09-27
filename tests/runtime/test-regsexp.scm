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
  (let ((result (ignore-errors (lambda () (match-string pattern string)))))
    (if (condition? result)
	(if (and (eq? expected 'PATTERN-ERROR)
		 (condition-of-type? result condition-type:compile-regsexp))
	    #f
	    (signal-condition condition))
	(assert-equal result
		      expected
		      'EXPRESSION `(match-string ',pattern ,string)))))

(define (match-strings-test pattern entries)
  (map (lambda (p)
	 (if (string? p)
	     (match-string-test pattern p (list (string-length p)))
	     (match-string-test pattern
				(car p)
				(if (exact-nonnegative-integer? (cadr p))
				    (cdr p)
				    (cadr p)))))
       entries))

(define-test 'any-char
  (match-strings-test '(any-char)
		      '(("" #f)
			("a" 1)
			("b" 1)
			("\n" #f))))

(define-test '*any-char
  (match-strings-test '(* (any-char))
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
		    (match-strings-test pattern
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

(define-test 'palindromes
  (list
   (match-strings-test '(seq (group a (any-char))
			     (group b (any-char))
			     (any-char)
			     (group-ref b)
			     (group-ref a))
		       '(("radar" 5 (b 1 2) (a 0 1))))
   (match-strings-test '(seq (string-start)
			     (group 1 (? (any-char)))
			     (group 2 (? (any-char)))
			     (group 3 (? (any-char)))
			     (group 4 (? (any-char)))
			     (group 5 (? (any-char)))
			     (group 6 (? (any-char)))
			     (group 7 (? (any-char)))
			     (group 8 (? (any-char)))
			     (group 9 (? (any-char)))
			     (? (any-char))
			     (group-ref 9)
			     (group-ref 8)
			     (group-ref 7)
			     (group-ref 6)
			     (group-ref 5)
			     (group-ref 4)
			     (group-ref 3)
			     (group-ref 2)
			     (group-ref 1)
			     (string-end))
		       '(("civic" 5
				  (9 2 2) (8 2 2) (7 2 2) (6 2 2) (5 2 2)
				  (4 2 2) (3 2 2) (2 1 2) (1 0 1))
			 ("abba" 4
				 (9 2 2) (8 2 2) (7 2 2) (6 2 2) (5 2 2)
				 (4 2 2) (3 2 2) (2 1 2) (1 0 1))))
   (match-strings-test '(seq (string-start)
			     (group 1 (?? (any-char)))
			     (group 2 (?? (any-char)))
			     (group 3 (?? (any-char)))
			     (group 4 (?? (any-char)))
			     (group 5 (?? (any-char)))
			     (group 6 (?? (any-char)))
			     (group 7 (?? (any-char)))
			     (group 8 (?? (any-char)))
			     (group 9 (?? (any-char)))
			     (?? (any-char))
			     (group-ref 9)
			     (group-ref 8)
			     (group-ref 7)
			     (group-ref 6)
			     (group-ref 5)
			     (group-ref 4)
			     (group-ref 3)
			     (group-ref 2)
			     (group-ref 1)
			     (string-end))
		       '(("civic" 5
				  (9 1 2) (8 0 1) (7 0 0) (6 0 0) (5 0 0)
				  (4 0 0) (3 0 0) (2 0 0) (1 0 0))
			 ("abba" 4
				 (9 1 2) (8 0 1) (7 0 0) (6 0 0) (5 0 0)
				 (4 0 0) (3 0 0) (2 0 0) (1 0 0))))
   ))

(define-test 'grep-bre
  (map (lambda (entry)
	 (match-strings-test (car entry) (cdr entry)))
       '(((seq "a" (seq "b") "c")
	  "abc")
	 ((seq "a" (seq) "b")
	  "ab")
	 ((seq (* "a")
	       (seq (string-start)
		    "b"
		    (string-end))
	       (* "c"))
	  "b")
	 ((seq)
	  ("abc" 0))
	 ((seq "a"
	       (group x (* "b"))
	       "c"
	       (group-ref x)
	       "d")
	  ("abbcbd" #f)
	  ("abbcbbd" 7 (x 1 3))
	  ("abbcbbbd" #f))
	 ((seq (string-start)
	       (group x (any-char))
	       (group-ref x))
	  ("abc" #f))
	 ((seq "a"
	       (* (seq (group x (char-set "bc"))
		       (group-ref x)))
	       "d")
	  ("abbccd" 6 (x 3 4) (x 1 2))
	  ("abbcbd" #f))
	 ((seq "a"
	       (* (seq (* (group x "b"))
		       (group-ref x)))
	       "d")
	  ("abbbd" 5 (x 2 3) (x 1 2)))
	 ((seq (group x "a")
	       (group-ref x)
	       "bcd")
	  ("aabcd" 5 (x 0 1)))
	 ((seq (group x "a")
	       (group-ref x)
	       "b"
	       (* "c")
	       "d")
	  ("aabcd" 5 (x 0 1))
	  ("aabd" 4 (x 0 1))
	  ("aabcccd" 7 (x 0 1)))
	 ((seq (group x "a")
	       (group-ref x)
	       "b"
	       (* "c")
	       (char-set "ce")
	       "d")
	  ("aabcccd" 7 (x 0 1)))
	 ((seq (string-start)
	       (group x "a")
	       (group-ref x)
	       "b"
	       (* "c")
	       "cd"
	       (string-end))
	  ("aabcccd" 7 (x 0 1)))
	 ((seq (repeat> 1 1 "a") "b")
	  "ab")
	 ((seq (repeat> 1 #f "a") "b")
	  "ab")
	 ((seq (repeat> 1 2 "a") "b")
	  "aab")
	 ((seq "a" (repeat> 0 0 "b") "c")
	  "ac"
	  ("abc" #f))
	 ((seq "a" (repeat> 0 1 "b") "c")
	  "ac"
	  "abc"
	  ("abbc" #f))
	 ((seq "a" (repeat> 0 3 "b") "c")
	  "ac"
	  "abc"
	  "abbc"
	  "abbbc"
	  ("abbbbc" #f))
	 ((seq "a" (repeat> 1 0 "b") "c")
	  ("ac" pattern-error))
	 ((seq "a" (repeat> #f 1 "b") "c")
	  ("ac" pattern-error))
	 ((seq "a" (repeat> 1 1 "b") "c")
	  ("ac" #f)
	  "abc")
	 ((seq "a" (repeat> 1 3 "b") "c")
	  ("ac" #f)
	  "abc")
	 ((seq "a" (repeat> 2 2 "b") "c")
	  ("abc" #f)
	  "abbc")
	 ((seq "a" (repeat> 2 4 "b") "c")
	  ("abcabbc" #f))
	 ((seq "a"
	       (? (group x "b"))
	       "c"
	       (group-ref x)
	       "d")
	  "acd")
	 ((seq (repeat> 0 1 "-")
	       (+ (char-set "0123456789"))
	       (string-end))
	  "-5"))))