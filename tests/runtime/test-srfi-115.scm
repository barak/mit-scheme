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
        (assert-error (lambda () (regexp pattern))
                      (list condition-type:compile-regexp)))
      (let ((cr (regexp pattern)))
	(map (lambda (p)
	       (if (string? p)
		   (%match-string-test pattern cr p
                                       (list 0 (string-length p)))
		   (%match-string-test pattern cr (car p) (cadr p))))
	     entries))))

(define (match-strings-test* patterns entries)
  (append-map (lambda (pattern)
		(match-strings-test pattern entries))
	      patterns))

(define (match-string-test pattern string expected)
  (%match-string-test pattern (regexp pattern) string expected))

(define (%match-string-test pattern cr string expected)
  (let ((thunk (lambda () (translate-regexp-match (regexp-matches cr string)))))
    (lambda ()
      (with-test-properties
          (lambda ()
            (assert-equal (thunk) expected))
        'expression `(regexp-matches ',pattern ,string)))))

(define (translate-regexp-match match)
  (and match
       (cons* (regexp-match-submatch-start match 0)
	      (regexp-match-submatch-end match 0)
	      (map cons
		   (cdr (regexp-match-keys match))
		   (cdr (regexp-match->list match))))))

(define (multi-match-strings-test entries)
  (map (lambda (entry)
	 (match-strings-test (car entry) (cdr entry)))
       entries))

(define (search-strings-test pattern entries)
  (if (equal? entries '(pattern-error))
      (lambda ()
	(assert-error (lambda () (regexp pattern))
		      (list condition-type:compile-regexp)))
      (let ((cr (regexp pattern)))
	(map (lambda (p)
	       (%search-string-test pattern cr (car p) (cadr p)))
	     entries))))

(define (search-strings-test* patterns entries)
  (append-map (lambda (pattern)
		(search-strings-test pattern entries))
	      patterns))

(define (search-string-test pattern string expected)
  (%search-string-test pattern (regexp pattern) string expected))

(define (%search-string-test pattern cr string expected)
  (let ((thunk (lambda () (translate-regexp-match (regexp-search cr string)))))
    (lambda ()
      (with-test-properties
          (lambda ()
            (assert-equal (thunk) expected))
        'expression `(regexp-search ',pattern ,string)))))

(define-test 'match-nonl
  (match-strings-test 'nonl
		      '(("" #f)
			("a" (0 1))
			("b" (0 1))
			("\n" #f))))

(define-test 'search-nonl
  (search-strings-test 'nonl
		       '(("" #f)
			 ("a" (0 1))
			 ("b" (0 1))
			 ("\n" #f)
			 ("ab" (0 1))
			 ("\na" (1 2)))))

(define-test 'match-*nonl
  (match-strings-test '(* nonl)
		      '(("" (0 0))
			("a" (0 1))
			("ab" (0 2))
			("abc" (0 3))
			("ab\n" (0 2))
			("a\nb" (0 1)))))

(define-test 'search-+nonl
  (search-strings-test '(+ nonl)
		       '(("" #f)
			 ("a" (0 1))
			 ("ab" (0 2))
			 ("abc" (0 3))
			 ("ab\n" (0 2))
			 ("a\nb" (0 1))
			 ("\nab" (1 3)))))

(define-test 'match-simple-seq
  (match-strings-test* '((: "a" "b") (seq "a" "b"))
		       '(("ab" (0 2)))))

(define-test 'search-simple-seq
  (search-strings-test* '((: "a" "b") (seq "a" "b"))
			'(("1914ab37" (4 6)))))

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
		  '(= 0 "a")
		  '(exactly 0 "a")
		  '(** 0 0 "a")
		  '(repeated 0 0 "a")
		  '(**? 0 0 "a")
		  '(non-greedy-repeated 0 0 "a")
		  '(: "" "")
		  '(seq "" ""))

     (equivalents '(#f 1 #f 1 #f 1)
		  "a"
		  '(= 1 "a")
		  '(exactly 1 "a")
		  '(** 1 1 "a")
		  '(repeated 1 1 "a")
		  '(**? 1 1 "a")
		  '(non-greedy-repeated 1 1 "a")
		  '(: "a" "")
		  '(seq "a" "")
		  '(: "" "a")
		  '(seq "" "a"))

     (equivalents '(#f #f #f #f #f 2)
		  "aa"
		  '(= 2 "a")
		  '(exactly 2 "a")
		  '(** 2 2 "a")
		  '(repeated 2 2 "a")
		  '(**? 2 2 "a")
		  '(non-greedy-repeated 2 2 "a")
		  '(: "a" "a")
		  '(seq "a" "a")
		  '(: "aa" "")
		  '(seq "aa" "")
		  '(: "" "aa")
		  '(seq "" "aa"))

     (equivalents '(0 1 0 1 0 2)
		  '(* "a")
		  '(zero-or-more "a")
		  '(>= 0 "a")
		  '(at-least 0 "a"))

     (equivalents '(0 0 0 0 0 0)
		  '(*? "a")
		  '(non-greedy-zero-or-more "a"))

     (equivalents '(#f 1 #f 1 #f 2)
		  '(+ "a")
		  '(one-or-more "a")
		  '(>= 1 "a")
		  '(at-least 1 "a")
		  '(: "a" (* "a"))
		  '(seq "a" (* "a")))

     (equivalents '(#f 1 #f 1 #f 1)
		  '(: "a" (*? "a"))
		  '(seq "a" (*? "a")))

     (equivalents '(0 1 0 1 0 1)
		  '(? "a")
		  '(optional "a")
		  '(** 0 1 "a")
		  '(repeated 0 1 "a"))

     (equivalents '(0 0 0 0 0 0)
		  '(?? "a")
		  '(non-greedy-optional "a")
		  '(**? 0 1 "a")
		  '(non-greedy-repeated 0 1 "a")))))

(define-test 'match-more-repeat-tests
  (list
   (match-string-test '(: (? "a") "a") "aab" '(0 2))
   (match-string-test '(: (? "a") "ab") "aab" '(0 3))

   (match-string-test '(: (?? "a") "a") "aab" '(0 1))
   (match-string-test '(: (?? "a") "ab") "aab" '(0 3))

   (match-string-test '(** 1 2 "a") "aab" '(0 2))
   (match-string-test '(: (** 1 2 "a") "b") "aab" '(0 3))

   (match-string-test '(**? 1 2 "a") "aab" '(0 1))
   (match-string-test '(: (**? 1 2 "a") "b") "aab" '(0 3))

   (match-string-test '(** 1 3 "a") "aaab" '(0 3))
   (match-string-test '(: (** 1 3 "a") "b") "aaab" '(0 4))

   (match-string-test '(**? 1 3 "a") "aaab" '(0 1))
   (match-string-test '(: (**? 1 3 "a") "b") "aaab" '(0 4))

   (match-string-test '(: (-> foo (? "a")) "a") "aab" '(0 2 (foo . "a")))
   (match-string-test '(: (-> foo (? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(: (-> foo (? "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(: (-> foo (?? "a")) "a") "aab" '(0 1 (foo . "")))
   (match-string-test '(: (-> foo (?? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(: (-> foo (?? "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(: (-> foo (* "a")) "b") "aab" '(0 3 (foo . "aa")))
   (match-string-test '(: (-> foo (* "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(: (-> foo (* "a")) "aab") "aab" '(0 3 (foo . "")))

   (match-string-test '(: (-> foo (*? "a")) "b") "aab" '(0 3 (foo . "aa")))
   (match-string-test '(: (-> foo (*? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (match-string-test '(: (-> foo (*? "a")) "aab") "aab" '(0 3 (foo . "")))

   ))

(define-test 'search-repeat-tests
  (list
   (search-string-test '(: (? "a") "a") "aab" '(0 2))
   (search-string-test '(: (? "a") "a") "xaab" '(1 3))
   (search-string-test '(: (? "a") "ab") "aab" '(0 3))
   (search-string-test '(: (? "a") "ab") "xaab" '(1 4))

   (search-string-test '(: (?? "a") "a") "aab" '(0 1))
   (search-string-test '(: (?? "a") "a") "xaab" '(1 2))
   (search-string-test '(: (?? "a") "ab") "aab" '(0 3))
   (search-string-test '(: (?? "a") "ab") "xaab" '(1 4))

   (search-string-test '(** 1 2 "a") "aab" '(0 2))
   (search-string-test '(** 1 2 "a") "xaab" '(1 3))
   (search-string-test '(: (** 1 2 "a") "b") "aab" '(0 3))
   (search-string-test '(: (** 1 2 "a") "b") "xaab" '(1 4))

   (search-string-test '(**? 1 2 "a") "aab" '(0 1))
   (search-string-test '(**? 1 2 "a") "xaab" '(1 2))
   (search-string-test '(: (**? 1 2 "a") "b") "aab" '(0 3))
   (search-string-test '(: (**? 1 2 "a") "b") "xaab" '(1 4))

   (search-string-test '(** 1 3 "a") "aaab" '(0 3))
   (search-string-test '(** 1 3 "a") "xaaab" '(1 4))
   (search-string-test '(: (** 1 3 "a") "b") "aaab" '(0 4))
   (search-string-test '(: (** 1 3 "a") "b") "xaaab" '(1 5))

   (search-string-test '(**? 1 3 "a") "aaab" '(0 1))
   (search-string-test '(**? 1 3 "a") "xaaab" '(1 2))
   (search-string-test '(: (**? 1 3 "a") "b") "aaab" '(0 4))
   (search-string-test '(: (**? 1 3 "a") "b") "xaaab" '(1 5))

   (search-string-test '(: (-> foo (? "a")) "a") "aab" '(0 2 (foo . "a")))
   (search-string-test '(: (-> foo (? "a")) "a") "xaab" '(1 3 (foo . "a")))
   (search-string-test '(: (-> foo (? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(: (-> foo (? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(: (-> foo (? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(: (-> foo (? "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(: (-> foo (?? "a")) "a") "aab" '(0 1 (foo . "")))
   (search-string-test '(: (-> foo (?? "a")) "a") "xaab" '(1 2 (foo . "")))
   (search-string-test '(: (-> foo (?? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(: (-> foo (?? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(: (-> foo (?? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(: (-> foo (?? "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(: (-> foo (* "a")) "b") "aab" '(0 3 (foo . "aa")))
   (search-string-test '(: (-> foo (* "a")) "b") "xaab" '(1 4 (foo . "aa")))
   (search-string-test '(: (-> foo (* "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(: (-> foo (* "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(: (-> foo (* "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(: (-> foo (* "a")) "aab") "xaab" '(1 4 (foo . "")))

   (search-string-test '(: (-> foo (*? "a")) "b") "aab" '(0 3 (foo . "aa")))
   (search-string-test '(: (-> foo (*? "a")) "b") "xaab" '(1 4 (foo . "aa")))
   (search-string-test '(: (-> foo (*? "a")) "ab") "aab" '(0 3 (foo . "a")))
   (search-string-test '(: (-> foo (*? "a")) "ab") "xaab" '(1 4 (foo . "a")))
   (search-string-test '(: (-> foo (*? "a")) "aab") "aab" '(0 3 (foo . "")))
   (search-string-test '(: (-> foo (*? "a")) "aab") "xaab" '(1 4 (foo . "")))

   ))

;;; Ripped off from "grep/tests/bre.tests".
(define-test 'match-grep-bre
  (multi-match-strings-test
   '(((: "a" (: "b") "c")
      "abc")
     ((: "a" (:) "b")
      "ab")
     ((: (* "a")
	 (: bos "b" eos)
	 (* "c"))
      "b")
     ((:)
      ("abc" (0 0)))
     ((: (= 1 "a") "b")
      "ab")
     ((: (>= 1 "a") "b")
      "ab")
     ((: (** 1 2 "a") "b")
      "aab")
     ((: "a" (= 0 "b") "c")
      "ac"
      ("abc" #f))
     ((: "a" (** 0 1 "b") "c")
      "ac"
      "abc"
      ("abbc" #f))
     ((: "a" (** 0 3 "b") "c")
      "ac"
      "abc"
      "abbc"
      "abbbc"
      ("abbbbc" #f))
     ((: "a" (** 1 0 "b") "c")
      pattern-error)
     ((: "a" (** #f 1 "b") "c")
      pattern-error)
     ((: "a" (= 1 "b") "c")
      ("ac" #f)
      "abc")
     ((: "a" (** 1 3 "b") "c")
      ("ac" #f)
      "abc")
     ((: "a" (= 2 "b") "c")
      ("abc" #f)
      "abbc")
     ((: "a" (** 2 4 "b") "c")
      ("abcabbc" #f))
     ((: (** 0 1 "-")
	 (+ ("0123456789"))
	 eos)
      "-5"))))

;;; Ripped off from "grep/tests/ere.tests".
(define-test 'match-grep-ere
  (multi-match-strings-test
   '(((or "abc" "de")
      "abc")
     ((or "a" "b" "c")
      ("abc" (0 1)))
     ((: "a" nonl "c")
      "abc")
     ((: "a" ("bc") "d")
      "abd")
     ((: "a" (* "b") "c")
      "abc")
     ((: "a" (+ "b") "c")
      "abc")
     ((: "a" (? "b") "c")
      "abc")
     ((: "a" ("b") "c")
      "abc")
     ((: "a" ("ab") "c")
      "abc")
     ((: "a" (~ ("ab")) "c")
      ("abc" #f)
      "adc")
     ((: "a" alphabetic "c")
      "abc"
      "adc")
     ((: "a" (+ numeric) "c")
      "a019c")
     ((: "A" (+ lower-case) "C")
      "AabC")
     ((: "a" (+ upper-case) "c")
      "aBCc")
     ((: "a" (= 20 ("ab")))
      "aaaaabaaaabaaaabaaaab")
     ((: "a"
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab"))
      "aaaaabaaaabaaaabaaaab")
     ((: "a"
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 ("ab") ("ab") ("ab") ("ab")
	 (or "wee" "week")
	 (or "knights" "night"))
      "aaaaabaaaabaaaabaaaabweeknights")
     ((: ("ab") ("cd") ("ef") ("gh")
	 ("ij") ("kl") ("mn"))
      ("acegikmoq" (0 7)))
     ((: ("ab") ("cd") ("ef") ("gh")
	 ("ij") ("kl") ("mn") ("op"))
      ("acegikmoq" (0 8)))
     ((: ("ab") ("cd") ("ef") ("gh")
	 ("ij") ("kl") ("mn") ("op")
	 ("qr"))
      ("acegikmoqy" (0 9)))
     ((: ("ab") ("cd") ("ef") ("gh")
	 ("ij") ("kl") ("mn") ("op")
	 ("q"))
      ("acegikmoqy" (0 9)))
     ("aBc"
      ("Abc" #f))
     ((: "a" (* ("Bc")) "d")
      "acBd"
      "aBcd"
      "aBcBcBd"
      ("aBCd" #f)
      ("abcd" #f)
      ("abBCcd" #f))
     ((: "a" (~ ("b")) "c")
      ("abc" #f)
      "aBc"
      "adc")
     ((: ("a") "b" ("c"))
      "abc")
     ((: ("a") "b" ("a"))
      "aba")
     ((: ("abc") "b" ("abc"))
      "abc")
     ((: ("abc") "b" ("abd"))
      ("abc" #f)
      "abd")
     ((: "a" (+ (? "b") "c") "d")
      "accd")
     ((* "a")
      ("b" (0 0)))
     ((: (or "wee" "week") (or "knights" "night"))
      "weeknights")
     ((: (or "we" "wee" "week" "frob") (or "knights" "night" "day"))
      "weeknights")
     ("abcdefghijklmnop"
      "abcdefghijklmnop")
     ("abcdefghijklmnopqrstuv"
      "abcdefghijklmnopqrstuv")
     ((or (: "CC" ("13") "1")
	  (: (= 21 "a")
	     ("23")
	     ("EO")
	     ("123")
	     ("Es")
	     ("12")
	     (= 15 "a")
	     "aa"
	     ("34")
	     ("EW")
	     "aaaaaaa"
	     ("X")
	     "a"))
      "CC11"))))

;; Ripped off from "grep/tests/khadafy.*".
(define-test 'match-grep-muammar-qaddafi
  (match-strings-test
   '(: "M"
       ("ou")
       (? "'")
       "a"
       (+ "m")
       ("ae")
       "r "
       (* nonl)
       (? ("AEae") "l" ("- "))
       ("GKQ")
       (? "h")
       (+ ("aeu"))
       (+ ("dtz") (? ("dhz")))
       "af"
       ("iy"))
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
     ((: "a" (* "b") "c")
      "abc")
     ((: "a" (* "b") "bc")
      "abc"
      "abbc"
      "abbbbc")
     ((: "a" (+ "b") "bc")
      ("abc" #f)
      "abbc"
      "abbbbc"
      ("abq" #f))
     ((: "a" (? "b") "bc")
      "abc"
      "abbc"
      ("abbbbc" #f))
     ((: "a" (? "b") "c")
      "abc")
     ((: bos "abc" eos)
      "abc"
      ("abcc" #f))
     ((: bos "abc")
      ("abcc" (0 3)))
     (bos
      ("abc" (0 0)))
     (eos
      ""
      ("a" #f))
     ((: "a" nonl "c")
      "abc"
      "axc")
     ((: "a" (* nonl) "c")
      "axyzc"
      ("axyzd" #f))
     ((: "a" ("bc") "d")
      ("abc" #f)
      "abd")
     ((: "a" ("bcd") "e")
      ("abd" #f)
      "ace")
     ((: "a" ("bcd"))
      "ac"
      ("aac" #f))
     ((: "a" (~ ("bc")) "d")
      "aed"
      ("abd" #f))
     ((: (+ "a") (+ "b") "c")
      "abc"
      "aabbc"
      ("aabbabc" #f))
     ((* (* "a"))
      ("-" (0 0)))
     ((+ (* "a"))
      ("-" (0 0)))
     ((? (* "a"))
      ("-" (0 0)))
     ((* (or "a" (:)))
      ("-" (0 0)))
     ((* (or (* "a") "b"))
      ("-" (0 0)))
     ((* (or (+ "a") "b"))
      "ab")
     ((+ (or (+ "a") "b"))
      "ab")
     ((? (or (+ "a") "b"))
      ("ba" (0 1))
      ("ab" (0 1)))
     ((* (~ ("ab")))
      "cde")
     ((: (* ("abc")) "d")
      "abbbcd")
     ((: (* ("abc")) "bcd")
      "abcd")
     ((or "a" "b" "c" "d" "e")
      "e")
     ((: (or "a" "b" "c" "d" "e") "f")
      "ef")
     ((: "abc" (* "d") "efg")
      "abcdefg")
     ("multiple words of text"
      ("uh-uh" #f))
     ("multiple words"
      ("multiple words, yeah" (0 14))))))