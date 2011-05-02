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

;;;; Tests of regular s-expressions

(declare (usual-integrations))

(define (match-string pattern string)
  (regsexp-match-string (compile-regsexp pattern) string))

(define ((match-string-test pattern string expected))
  (let ((thunk (lambda () (match-string pattern string))))
    (run-sub-test
     (lambda ()
       (with-test-properties
	   (lambda ()
	     (if (eq? expected 'PATTERN-ERROR)
		 (assert-error thunk (list condition-type:compile-regsexp))
		 (assert-equal (thunk) expected)))
	 'EXPRESSION `(match-string ',pattern ,string))))))

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

(define (multi-match-strings-test entries)
  (map (lambda (entry)
	 (match-strings-test (car entry) (cdr entry)))
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

(define-test 'more-repeat-tests
  (list
   (match-string-test '(seq (? "a") "a") "aab" '(2))
   (match-string-test '(seq (? "a") "ab") "aab" '(3))

   (match-string-test '(seq (?? "a") "a") "aab" '(1))
   (match-string-test '(seq (?? "a") "ab") "aab" '(3))

   (match-string-test '(** 1 2 "a") "aab" '(2))
   (match-string-test '(seq (** 1 2 "a") "b") "aab" '(3))

   (match-string-test '(**? 1 2 "a") "aab" '(1))
   (match-string-test '(seq (**? 1 2 "a") "b") "aab" '(3))

   (match-string-test '(** 1 3 "a") "aaab" '(3))
   (match-string-test '(seq (** 1 3 "a") "b") "aaab" '(4))

   (match-string-test '(**? 1 3 "a") "aaab" '(1))
   (match-string-test '(seq (**? 1 3 "a") "b") "aaab" '(4))

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

;;; Ripped off from "grep/tests/bre.tests".
(define-test 'grep-bre
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
      ("ac" pattern-error))
     ((seq "a" (** #f 1 "b") "c")
      ("ac" pattern-error))
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
	   (+ (char-set "0123456789"))
	   (string-end))
      "-5"))))

;;; Ripped off from "grep/tests/ere.tests".
(define-test 'grep-ere
  (multi-match-strings-test
   `(((alt "abc" "de")
      "abc")
     ((alt "a" "b" "c")
      ("abc" 1))
     ((seq "a" (any-char) "c")
      "abc")
     ((seq "a" (char-set "bc") "d")
      "abd")
     ((seq "a" (* "b") "c")
      "abc")
     ((seq "a" (+ "b") "c")
      "abc")
     ((seq "a" (? "b") "c")
      "abc")
     ((seq "a" (char-set "b") "c")
      "abc")
     ((seq "a" (char-set "ab") "c")
      "abc")
     ((seq "a" (inverse-char-set "ab") "c")
      ("abc" #f)
      "adc")
     ((seq "a" (char-set ,(char-set->alphabet char-set:alphabetic)) "c")
      "abc"
      "adc")
     ((seq "a" (+ (char-set ,(char-set->alphabet char-set:numeric))) "c")
      "a019c")
     ((seq "A" (+ (char-set ,(char-set->alphabet char-set:lower-case))) "C")
      "AabC")
     ((seq "a" (+ (char-set ,(char-set->alphabet char-set:upper-case))) "c")
      "aBCc")
     ((seq "a" (** 20 (char-set "ab")))
      "aaaaabaaaabaaaabaaaab")
     ((seq "a"
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab"))
      "aaaaabaaaabaaaabaaaab")
     ((seq "a"
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (char-set "ab") (char-set "ab") (char-set "ab") (char-set "ab")
	   (alt "wee" "week")
	   (alt "knights" "night"))
      "aaaaabaaaabaaaabaaaabweeknights")
     ((seq (char-set "ab") (char-set "cd") (char-set "ef") (char-set "gh")
	   (char-set "ij") (char-set "kl") (char-set "mn"))
      ("acegikmoq" 7))
     ((seq (char-set "ab") (char-set "cd") (char-set "ef") (char-set "gh")
	   (char-set "ij") (char-set "kl") (char-set "mn") (char-set "op"))
      ("acegikmoq" 8))
     ((seq (char-set "ab") (char-set "cd") (char-set "ef") (char-set "gh")
	   (char-set "ij") (char-set "kl") (char-set "mn") (char-set "op")
	   (char-set "qr"))
      ("acegikmoqy" 9))
     ((seq (char-set "ab") (char-set "cd") (char-set "ef") (char-set "gh")
	   (char-set "ij") (char-set "kl") (char-set "mn") (char-set "op")
	   (char-set "q"))
      ("acegikmoqy" 9))
     ("aBc"
      ("Abc" #f))
     ((seq "a" (* (char-set "Bc")) "d")
      "acBd"
      "aBcd"
      "aBcBcBd"
      ("aBCd" #f)
      ("abcd" #f)
      ("abBCcd" #f))
     ((seq "a" (inverse-char-set "b") "c")
      ("abc" #f)
      "aBc"
      "adc")
     ((seq (char-set "a") "b" (char-set "c"))
      "abc")
     ((seq (char-set "a") "b" (char-set "a"))
      "aba")
     ((seq (char-set "abc") "b" (char-set "abc"))
      "abc")
     ((seq (char-set "abc") "b" (char-set "abd"))
      ("abc" #f)
      "abd")
     ((seq "a" (+ (seq (? "b") "c")) "d")
      "accd")
     ((* "a")
      ("b" 0))
     ((seq (alt "wee" "week") (alt "knights" "night"))
      "weeknights")
     ((seq (alt "we" "wee" "week" "frob") (alt "knights" "night" "day"))
      "weeknights")
     ("abcdefghijklmnop"
      "abcdefghijklmnop")
     ("abcdefghijklmnopqrstuv"
      "abcdefghijklmnopqrstuv")
     ((alt (seq "CC" (char-set "13") "1")
	   (seq (** 21 "a")
		(char-set "23")
		(char-set "EO")
		(char-set "123")
		(char-set "Es")
		(char-set "12")
		(** 15 "a")
		"aa"
		(char-set "34")
		(char-set "EW")
		"aaaaaaa"
		(char-set "X")
		"a"))
      "CC11"))))

;; Ripped off from "grep/tests/khadafy.*".
(define-test 'grep-muammar-qaddafi
  (match-strings-test
   '(seq "M"
	 (char-set "ou")
	 (? "'")
	 "a"
	 (+ "m")
	 (char-set "ae")
	 "r "
	 (* (any-char))
	 (? (seq (char-set "AEae")
		 "l"
		 (char-set "- ")))
	 (char-set "GKQ")
	 (? "h")
	 (+ (char-set "aeu"))
	 (+ (seq (char-set "dtz")
		 (? (char-set "dhz"))))
	 "af"
	 (char-set "iy"))
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
(define-test 'grep-spencer
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
      ("abcc" 3))
     ((string-start)
      ("abc" 0))
     ((string-end)
      ""
      ("a" #f))
     ((seq "a" (any-char) "c")
      "abc"
      "axc")
     ((seq "a" (* (any-char)) "c")
      "axyzc"
      ("axyzd" #f))
     ((seq "a" (char-set "bc") "d")
      ("abc" #f)
      "abd")
     ((seq "a" (char-set "bcd") "e")
      ("abd" #f)
      "ace")
     ((seq "a" (char-set "bcd"))
      "ac"
      ("aac" #f))
     ((seq "a" (inverse-char-set "bc") "d")
      "aed"
      ("abd" #f))
     ((seq (+ "a") (+ "b") "c")
      "abc"
      "aabbc"
      ("aabbabc" #f))
     ((* (* "a"))
      ("-" 0))
     ((+ (* "a"))
      ("-" 0))
     ((? (* "a"))
      ("-" 0))
     ((* (alt "a" (seq)))
      ("-" 0))
     ((* (alt (* "a") "b"))
      ("-" 0))
     ((* (alt (+ "a") "b"))
      "ab")
     ((+ (alt (+ "a") "b"))
      "ab")
     ((? (alt (+ "a") "b"))
      ("ba" 1)
      ("ab" 1))
     ((* (inverse-char-set "ab"))
      "cde")
     ((seq (* (char-set "abc")) "d")
      "abbbcd")
     ((seq (* (char-set "abc")) "bcd")
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
      ("multiple words, yeah" 14))
     ((seq (group x (seq (any-char) (any-char) (any-char) (any-char)))
	   (* (any-char))
	   (group-ref x))
      ("beriberi" 8 (x 0 4))))))