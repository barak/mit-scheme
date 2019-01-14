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

;;;; Tests of regular s-expressions

(declare (usual-integrations))

(define (match-strings-test pattern entries)
  (if (equal? entries '(pattern-error))
      (lambda ()
        (assert-error (lambda () (compile-regsexp pattern))
                      (list condition-type:compile-regsexp)))
      (let ((cr (compile-regsexp pattern)))
	(map (lambda (p)
	       (if (string? p)
		   (%match-string-test pattern cr p
                                       (list 0 (string-length p)))
		   (%match-string-test pattern cr (car p) (cadr p))))
	     entries))))

(define (match-string-test pattern string expected)
  (%match-string-test pattern (compile-regsexp pattern) string expected))

(define (%match-string-test pattern cr string expected)
  (let ((thunk (lambda () (regsexp-match-string cr string))))
    (lambda ()
      (with-test-properties
          (lambda ()
            (assert-equal (thunk) expected))
        'expression `(match-string ',pattern ,string)))))

(define (multi-match-strings-test entries)
  (map (lambda (entry)
	 (match-strings-test (car entry) (cdr entry)))
       entries))

(define (search-strings-test pattern entries)
  (if (equal? entries '(pattern-error))
      (lambda ()
	(assert-error (lambda () (compile-regsexp pattern))
		      (list condition-type:compile-regsexp)))
      (let ((cr (compile-regsexp pattern)))
	(map (lambda (p)
	       (%search-string-test pattern cr (car p) (cadr p)))
	     entries))))

(define (search-string-test pattern string expected)
  (%search-string-test pattern (compile-regsexp pattern) string expected))

(define (%search-string-test pattern cr string expected)
  (let ((thunk (lambda () (regsexp-search-string-forward cr string))))
    (lambda ()
      (with-test-properties
          (lambda ()
            (assert-equal (thunk) expected))
        'expression `(search-string ',pattern ,string)))))

(define-test 'match-any-char
  (match-strings-test '(any-char)
		      '(("" #f)
			("a" (0 1))
			("b" (0 1))
			("\n" #f))))

(define-test 'search-any-char
  (search-strings-test '(any-char)
		       '(("" #f)
			 ("a" (0 1))
			 ("b" (0 1))
			 ("\n" #f)
			 ("ab" (0 1))
			 ("\na" (1 2)))))

(define-test 'match-*any-char
  (match-strings-test '(* (any-char))
		      '(("" (0 0))
			("a" (0 1))
			("ab" (0 2))
			("abc" (0 3))
			("ab\n" (0 2))
			("a\nb" (0 1)))))

(define-test 'search-+any-char
  (search-strings-test '(+ (any-char))
		       '(("" #f)
			 ("a" (0 1))
			 ("ab" (0 2))
			 ("abc" (0 3))
			 ("ab\n" (0 2))
			 ("a\nb" (0 1))
			 ("\nab" (1 3)))))

(define-test 'match-simple-seq
  (match-string-test '(seq "a" "b") "ab" '(0 2)))

(define-test 'search-simple-seq
  (search-string-test '(seq "a" "b") "1914ab37" '(4 6)))

(define-test 'match/repeat-equivalences-test
  (let ((equivalents
	 (lambda (indices . patterns)
	   (map (let ((strings '("" "a" "b" "ab" "ba" "aab")))
		  (lambda (pattern)
		    (match-strings-test
		     pattern
		     (map (lambda (string index)
			    (list string
				  (and index (list 0 index))))
			  strings
			  indices))))
		patterns))))
    (list
     (equivalents '(0 0 0 0 0 0)
		  ""
		  '(** 0 "a")
		  '(** 0 0 "a")
		  '(**? 0 "a")
		  '(**? 0 0 "a")
		  '(seq "" ""))

     (equivalents '(#f 1 #f 1 #f 1)
		  "a"
		  '(** 1 "a")
		  '(** 1 1 "a")
		  '(**? 1 "a")
		  '(**? 1 1 "a")
		  '(seq "a" "")
		  '(seq "" "a"))

     (equivalents '(#f #f #f #f #f 2)
		  "aa"
		  '(** 2 "a")
		  '(** 2 2 "a")
		  '(**? 2 "a")
		  '(**? 2 2 "a")
		  '(seq "a" "a")
		  '(seq "aa" "")
		  '(seq "" "aa"))

     (equivalents '(0 1 0 1 0 2)
		  '(* "a")
		  '(** 0 #f "a"))

     (equivalents '(0 0 0 0 0 0)
		  '(*? "a")
		  '(**? 0 #f "a"))

     (equivalents '(#f 1 #f 1 #f 2)
		  '(+ "a")
		  '(seq "a" (* "a"))
		  '(** 1 #f "a"))

     (equivalents '(#f 1 #f 1 #f 1)
		  '(+? "a")
		  '(seq "a" (*? "a"))
		  '(**? 1 #f "a"))

     (equivalents '(0 1 0 1 0 1)
		  '(? "a")
		  '(** 0 1 "a"))

     (equivalents '(0 0 0 0 0 0)
		  '(?? "a")
		  '(**? 0 1 "a")))))

(define-test 'match-more-repeat-tests
  (list
   (match-string-test '(seq (? "a") "a") "aab" '(0 2))
   (match-string-test '(seq (? "a") "ab") "aab" '(0 3))

   (match-string-test '(seq (?? "a") "a") "aab" '(0 1))
   (match-string-test '(seq (?? "a") "ab") "aab" '(0 3))

   (match-string-test '(** 1 2 "a") "aab" '(0 2))
   (match-string-test '(seq (** 1 2 "a") "b") "aab" '(0 3))

   (match-string-test '(**? 1 2 "a") "aab" '(0 1))
   (match-string-test '(seq (**? 1 2 "a") "b") "aab" '(0 3))

   (match-string-test '(** 1 3 "a") "aaab" '(0 3))
   (match-string-test '(seq (** 1 3 "a") "b") "aaab" '(0 4))

   (match-string-test '(**? 1 3 "a") "aaab" '(0 1))
   (match-string-test '(seq (**? 1 3 "a") "b") "aaab" '(0 4))

   (match-string-test '(seq (group foo (? "a")) "a") "aab" '(0 2 (foo . "a")))
   (match-string-test '(seq (group foo (? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(seq (group foo (? "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(seq (group foo (?? "a")) "a") "aab" '(0 1 (foo . "")))
   (match-string-test '(seq (group foo (?? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(seq (group foo (?? "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(seq (group foo (* "a")) "b") "aab" '(0 3 (foo . "aa")))
   (match-string-test '(seq (group foo (* "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(seq (group foo (* "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(seq (group foo (*? "a")) "b") "aab" '(0 3 (foo . "aa")))
   (match-string-test '(seq (group foo (*? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(seq (group foo (*? "a")) "aab") "aab" '(0 3 (foo . "")))

   ))

(define-test 'search-repeat-tests
  (list
   (search-string-test '(seq (? "a") "a") "aab" '(0 2))
   (search-string-test '(seq (? "a") "a") "xaab" '(1 3))
   (search-string-test '(seq (? "a") "ab") "aab" '(0 3))
   (search-string-test '(seq (? "a") "ab") "xaab" '(1 4))

   (search-string-test '(seq (?? "a") "a") "aab" '(0 1))
   (search-string-test '(seq (?? "a") "a") "xaab" '(1 2))
   (search-string-test '(seq (?? "a") "ab") "aab" '(0 3))
   (search-string-test '(seq (?? "a") "ab") "xaab" '(1 4))

   (search-string-test '(** 1 2 "a") "aab" '(0 2))
   (search-string-test '(** 1 2 "a") "xaab" '(1 3))
   (search-string-test '(seq (** 1 2 "a") "b") "aab" '(0 3))
   (search-string-test '(seq (** 1 2 "a") "b") "xaab" '(1 4))

   (search-string-test '(**? 1 2 "a") "aab" '(0 1))
   (search-string-test '(**? 1 2 "a") "xaab" '(1 2))
   (search-string-test '(seq (**? 1 2 "a") "b") "aab" '(0 3))
   (search-string-test '(seq (**? 1 2 "a") "b") "xaab" '(1 4))

   (search-string-test '(** 1 3 "a") "aaab" '(0 3))
   (search-string-test '(** 1 3 "a") "xaaab" '(1 4))
   (search-string-test '(seq (** 1 3 "a") "b") "aaab" '(0 4))
   (search-string-test '(seq (** 1 3 "a") "b") "xaaab" '(1 5))

   (search-string-test '(**? 1 3 "a") "aaab" '(0 1))
   (search-string-test '(**? 1 3 "a") "xaaab" '(1 2))
   (search-string-test '(seq (**? 1 3 "a") "b") "aaab" '(0 4))
   (search-string-test '(seq (**? 1 3 "a") "b") "xaaab" '(1 5))

   (search-string-test '(seq (group foo (? "a")) "a") "aab" '(0 2 (foo . "a")))
   (search-string-test '(seq (group foo (? "a")) "a") "xaab" '(1 3 (foo . "a")))
   (search-string-test '(seq (group foo (? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(seq (group foo (? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(seq (group foo (? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(seq (group foo (? "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(seq (group foo (?? "a")) "a") "aab" '(0 1 (foo . "")))
   (search-string-test '(seq (group foo (?? "a")) "a") "xaab" '(1 2 (foo . "")))
   (search-string-test '(seq (group foo (?? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(seq (group foo (?? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(seq (group foo (?? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(seq (group foo (?? "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(seq (group foo (* "a")) "b") "aab" '(0 3 (foo . "aa")))
   (search-string-test '(seq (group foo (* "a")) "b") "xaab" '(1 4 (foo . "aa")))
   (search-string-test '(seq (group foo (* "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(seq (group foo (* "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(seq (group foo (* "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(seq (group foo (* "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(seq (group foo (*? "a")) "b") "aab" '(0 3 (foo . "aa")))
   (search-string-test '(seq (group foo (*? "a")) "b") "xaab" '(1 4 (foo . "aa")))
   (search-string-test '(seq (group foo (*? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(seq (group foo (*? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(seq (group foo (*? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(seq (group foo (*? "a")) "aab") "xaab" '(1 4 (foo . "")))

   ))

(define-test 'match-palindromes
  (list
   (match-strings-test '(seq (group a (any-char))
			     (group b (any-char))
			     (any-char)
			     (group-ref b)
			     (group-ref a))
		       '(("radar" (0 5 (a . "r") (b . "a")))))
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
		       '(("civic" (0 5
				   (1 . "c") (2 . "i") (3 . "") (4 . "")
				   (5 . "") (6 . "") (7 . "") (8 . "")
				   (9 . "")))
			 ("abba" (0 4
				  (1 . "a") (2 . "b") (3 . "") (4 . "")
				  (5 . "") (6 . "") (7 . "") (8 . "")
				  (9 . "")))))
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
		       '(("civic" (0 5
				   (1 . "") (2 . "") (3 . "") (4 . "")
				   (5 . "") (6 . "") (7 . "") (8 . "c")
				   (9 . "i")))
			 ("abba" (0 4
				  (1 . "") (2 . "") (3 . "") (4 . "")
				  (5 . "") (6 . "") (7 . "") (8 . "a")
				  (9 . "b")))))
   ))

;;; Ripped off from "grep/tests/bre.tests".
(define-test 'match-grep-bre
  (multi-match-strings-test
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
      ("abc" (0 0)))
     ((seq "a"
	   (group x (* "b"))
	   "c"
	   (group-ref x)
	   "d")
      ("abbcbd" #f)
      ("abbcbbd" (0 7 (x . "bb")))
      ("abbcbbbd" #f))
     ((seq (string-start)
	   (group x (any-char))
	   (group-ref x))
      ("abc" #f))
     ((seq "a"
	   (* (seq (group x (char-in "bc"))
		   (group-ref x)))
	   "d")
      ("abbccd" (0 6 (x . "b") (x . "c")))
      ("abbcbd" #f))
     ((seq "a"
	   (* (seq (* (group x "b"))
		   (group-ref x)))
	   "d")
      ("abbbd" (0 5 (x . "b") (x . "b"))))
     ((seq (group x "a")
	   (group-ref x)
	   "bcd")
      ("aabcd" (0 5 (x . "a"))))
     ((seq (group x "a")
	   (group-ref x)
	   "b"
	   (* "c")
	   "d")
      ("aabcd" (0 5 (x . "a")))
      ("aabd" (0 4 (x . "a")))
      ("aabcccd" (0 7 (x . "a"))))
     ((seq (group x "a")
	   (group-ref x)
	   "b"
	   (* "c")
	   (char-in "ce")
	   "d")
      ("aabcccd" (0 7 (x . "a"))))
     ((seq (string-start)
	   (group x "a")
	   (group-ref x)
	   "b"
	   (* "c")
	   "cd"
	   (string-end))
      ("aabcccd" (0 7 (x . "a"))))
     ((seq (** 1 "a") "b")
      "ab")
     ((seq (** 1 #f "a") "b")
      "ab")
     ((seq (** 1 2 "a") "b")
      "aab")
     ((seq "a" (** 0 "b") "c")
      "ac"
      ("abc" #f))
     ((seq "a" (** 0 1 "b") "c")
      "ac"
      "abc"
      ("abbc" #f))
     ((seq "a" (** 0 3 "b") "c")
      "ac"
      "abc"
      "abbc"
      "abbbc"
      ("abbbbc" #f))
     ((seq "a" (** 1 0 "b") "c")
      pattern-error)
     ((seq "a" (** #f 1 "b") "c")
      pattern-error)
     ((seq "a" (** 1 "b") "c")
      ("ac" #f)
      "abc")
     ((seq "a" (** 1 3 "b") "c")
      ("ac" #f)
      "abc")
     ((seq "a" (** 2 "b") "c")
      ("abc" #f)
      "abbc")
     ((seq "a" (** 2 4 "b") "c")
      ("abcabbc" #f))
     ((seq "a"
	   (? (group x "b"))
	   "c"
	   (group-ref x)
	   "d")
      "acd")
     ((seq (** 0 1 "-")
	   (+ (char-in "0123456789"))
	   (string-end))
      "-5"))))

;;; Ripped off from "grep/tests/ere.tests".
(define-test 'match-grep-ere
  (multi-match-strings-test
   '(((alt "abc" "de")
      "abc")
     ((alt "a" "b" "c")
      ("abc" (0 1)))
     ((seq "a" (any-char) "c")
      "abc")
     ((seq "a" (char-in "bc") "d")
      "abd")
     ((seq "a" (* "b") "c")
      "abc")
     ((seq "a" (+ "b") "c")
      "abc")
     ((seq "a" (? "b") "c")
      "abc")
     ((seq "a" (char-in "b") "c")
      "abc")
     ((seq "a" (char-in "ab") "c")
      "abc")
     ((seq "a" (char-not-in "ab") "c")
      ("abc" #f)
      "adc")
     ((seq "a" (char-in alphabetic) "c")
      "abc"
      "adc")
     ((seq "a" (+ (char-in numeric)) "c")
      "a019c")
     ((seq "A" (+ (char-in lower-case)) "C")
      "AabC")
     ((seq "a" (+ (char-in upper-case)) "c")
      "aBCc")
     ((seq "a" (** 20 (char-in "ab")))
      "aaaaabaaaabaaaabaaaab")
     ((seq "a"
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab"))
      "aaaaabaaaabaaaabaaaab")
     ((seq "a"
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (char-in "ab") (char-in "ab") (char-in "ab") (char-in "ab")
	   (alt "wee" "week")
	   (alt "knights" "night"))
      "aaaaabaaaabaaaabaaaabweeknights")
     ((seq (char-in "ab") (char-in "cd") (char-in "ef") (char-in "gh")
	   (char-in "ij") (char-in "kl") (char-in "mn"))
      ("acegikmoq" (0 7)))
     ((seq (char-in "ab") (char-in "cd") (char-in "ef") (char-in "gh")
	   (char-in "ij") (char-in "kl") (char-in "mn") (char-in "op"))
      ("acegikmoq" (0 8)))
     ((seq (char-in "ab") (char-in "cd") (char-in "ef") (char-in "gh")
	   (char-in "ij") (char-in "kl") (char-in "mn") (char-in "op")
	   (char-in "qr"))
      ("acegikmoqy" (0 9)))
     ((seq (char-in "ab") (char-in "cd") (char-in "ef") (char-in "gh")
	   (char-in "ij") (char-in "kl") (char-in "mn") (char-in "op")
	   (char-in "q"))
      ("acegikmoqy" (0 9)))
     ("aBc"
      ("Abc" #f))
     ((seq "a" (* (char-in "Bc")) "d")
      "acBd"
      "aBcd"
      "aBcBcBd"
      ("aBCd" #f)
      ("abcd" #f)
      ("abBCcd" #f))
     ((seq "a" (char-not-in "b") "c")
      ("abc" #f)
      "aBc"
      "adc")
     ((seq (char-in "a") "b" (char-in "c"))
      "abc")
     ((seq (char-in "a") "b" (char-in "a"))
      "aba")
     ((seq (char-in "abc") "b" (char-in "abc"))
      "abc")
     ((seq (char-in "abc") "b" (char-in "abd"))
      ("abc" #f)
      "abd")
     ((seq "a" (+ (seq (? "b") "c")) "d")
      "accd")
     ((* "a")
      ("b" (0 0)))
     ((seq (alt "wee" "week") (alt "knights" "night"))
      "weeknights")
     ((seq (alt "we" "wee" "week" "frob") (alt "knights" "night" "day"))
      "weeknights")
     ("abcdefghijklmnop"
      "abcdefghijklmnop")
     ("abcdefghijklmnopqrstuv"
      "abcdefghijklmnopqrstuv")
     ((alt (seq "CC" (char-in "13") "1")
	   (seq (** 21 "a")
		(char-in "23")
		(char-in "EO")
		(char-in "123")
		(char-in "Es")
		(char-in "12")
		(** 15 "a")
		"aa"
		(char-in "34")
		(char-in "EW")
		"aaaaaaa"
		(char-in "X")
		"a"))
      "CC11"))))

;; Ripped off from "grep/tests/khadafy.*".
(define-test 'match-grep-muammar-qaddafi
  (match-strings-test
   '(seq "M"
	 (char-in "ou")
	 (? "'")
	 "a"
	 (+ "m")
	 (char-in "ae")
	 "r "
	 (* (any-char))
	 (? (seq (char-in "AEae")
		 "l"
		 (char-in "- ")))
	 (char-in "GKQ")
	 (? "h")
	 (+ (char-in "aeu"))
	 (+ (seq (char-in "dtz")
		 (? (char-in "dhz"))))
	 "af"
	 (char-in "iy"))
   '("Muammar Qaddafi"
     "Mo'ammar Gadhafi"
     "Muammar Kaddafi"
     "Muammar Qadhafi"
     "Moammar El Kadhafi"
     "Muammar Gadafi"
     "Mu'ammar al-Qadafi"
     "Moamer El Kazzafi"
     "Moamar al-Gaddafi"
     "Mu'ammar Al Qathafi"
     "Muammar Al Qathafi"
     "Mo'ammar el-Gadhafi"
     "Moamar El Kadhafi"
     "Muammar al-Qadhafi"
     "Mu'ammar al-Qadhdhafi"
     "Mu'ammar Qadafi"
     "Moamar Gaddafi"
     "Mu'ammar Qadhdhafi"
     "Muammar Khaddafi"
     "Muammar al-Khaddafi"
     "Mu'amar al-Kadafi"
     "Muammar Ghaddafy"
     "Muammar Ghadafi"
     "Muammar Ghaddafi"
     "Muamar Kaddafi"
     "Muammar Quathafi"
     "Muammar Gheddafi"
     "Muamar Al-Kaddafi"
     "Moammar Khadafy"
     "Moammar Qudhafi"
     "Mu'ammar al-Qaddafi"
     "Mu'ammar Muhammad Abu Minyar al-Qadhafi")))

;; Ripped off from "grep/tests/spencer1.*".
(define-test 'match-grep-spencer
  (multi-match-strings-test
   '(("abc"
      "abc"
      ("xbc" #f)
      ("axc" #f)
      ("abx" #f))
     ((seq "a" (* "b") "c")
      "abc")
     ((seq "a" (* "b") "bc")
      "abc"
      "abbc"
      "abbbbc")
     ((seq "a" (+ "b") "bc")
      ("abc" #f)
      "abbc"
      "abbbbc"
      ("abq" #f))
     ((seq "a" (? "b") "bc")
      "abc"
      "abbc"
      ("abbbbc" #f))
     ((seq "a" (? "b") "c")
      "abc")
     ((seq (string-start) "abc" (string-end))
      "abc"
      ("abcc" #f))
     ((seq (string-start) "abc")
      ("abcc" (0 3)))
     ((string-start)
      ("abc" (0 0)))
     ((string-end)
      ""
      ("a" #f))
     ((seq "a" (any-char) "c")
      "abc"
      "axc")
     ((seq "a" (* (any-char)) "c")
      "axyzc"
      ("axyzd" #f))
     ((seq "a" (char-in "bc") "d")
      ("abc" #f)
      "abd")
     ((seq "a" (char-in "bcd") "e")
      ("abd" #f)
      "ace")
     ((seq "a" (char-in "bcd"))
      "ac"
      ("aac" #f))
     ((seq "a" (char-not-in "bc") "d")
      "aed"
      ("abd" #f))
     ((seq (+ "a") (+ "b") "c")
      "abc"
      "aabbc"
      ("aabbabc" #f))
     ((* (* "a"))
      ("-" (0 0)))
     ((+ (* "a"))
      ("-" (0 0)))
     ((? (* "a"))
      ("-" (0 0)))
     ((* (alt "a" (seq)))
      ("-" (0 0)))
     ((* (alt (* "a") "b"))
      ("-" (0 0)))
     ((* (alt (+ "a") "b"))
      "ab")
     ((+ (alt (+ "a") "b"))
      "ab")
     ((? (alt (+ "a") "b"))
      ("ba" (0 1))
      ("ab" (0 1)))
     ((* (char-not-in "ab"))
      "cde")
     ((seq (* (char-in "abc")) "d")
      "abbbcd")
     ((seq (* (char-in "abc")) "bcd")
      "abcd")
     ((alt "a" "b" "c" "d" "e")
      "e")
     ((seq (alt "a" "b" "c" "d" "e") "f")
      "ef")
     ((seq "abc" (* "d") "efg")
      "abcdefg")
     ("multiple words of text"
      ("uh-uh" #f))
     ("multiple words"
      ("multiple words, yeah" (0 14)))
     ((seq (group x (seq (any-char) (any-char) (any-char) (any-char)))
	   (* (any-char))
	   (group-ref x))
      ("beriberi" (0 8 (x . "beri")))))))

(define-test 're-pattern->regsexp
  (map (lambda (entry)
	 (lambda ()
	   (assert-equal (re-pattern->regsexp (car entry))
			 (cadr entry))))
       '(("[\r\n\t ]*(This file must be converted with BinHex.*[\r\n][\r\n\t ]*:"
	  (seq (* (char-in (9 . 11) 13 32))
	       "(This file must be converted with BinHex"
	       (* (any-char))
	       (char-in 10 13)
	       (* (char-in (9 . 11) 13 32))
	       ":"))

	 ("^begin +[0-7]+ +.+$"
	  (seq (line-start)
	       "begin"
	       (+ #\space)
	       (+ (char-in (48 . 56)))
	       (+ #\space)
	       (+ (any-char))
	       (line-end)))

	 ("\\`8859-[0-9]+\\'"
	  (seq (string-start) "8859-" (+ (char-in (48 . 58))) (string-end)))

	 ("\\`0x\\([0-9A-Fa-f][0-9A-Fa-f]\\)\t0x\\([0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f][0-9A-Fa-f]\\)\t"
	  (seq (string-start)
	       "0x"
	       (group 1
		      (seq (char-in (48 . 58) (65 . 71) (97 . 103))
			   (char-in (48 . 58) (65 . 71) (97 . 103))))
	       "\t0x"
	       (group
		2
		(seq (char-in (48 . 58) (65 . 71) (97 . 103))
		     (char-in (48 . 58) (65 . 71) (97 . 103))
		     (char-in (48 . 58) (65 . 71) (97 . 103))
		     (char-in (48 . 58) (65 . 71) (97 . 103))))
	       "\t"))

	 ("\\`\\s *\\(error:\\)?\\s *\\(.*\\)\\s *\\'"
	  (seq (string-start)
	       (* (legacy-char-syntax #\space))
	       (? (group 1 "error:"))
	       (* (legacy-char-syntax #\space))
	       (group 2 (* (any-char)))
	       (* (legacy-char-syntax #\space))
	       (string-end))))))