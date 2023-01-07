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

;;;; Tests of regular s-expressions

(declare (usual-integrations))


(define (single-test proc name)
  (lambda (expected re string #!optional start end)
    (let ((thunk
	   (lambda ()
	     (translate-regexp-match
	      (proc re string start end)))))
      (lambda ()
	(with-test-properties
	    (lambda ()
	      (assert-equal (thunk) expected))
	  'expression `(,name ',re ,string))))))

(define (translate-regexp-match match)
  (and match
       (cons* (regexp-match-submatch-start match 0)
	      (regexp-match-submatch-end match 0)
	      (map cons
		   (cdr (regexp-match-keys match))
		   (cdr (regexp-match->list match))))))

(define match-some-test (single-test regexp-matches-some 'regexp-matches-some))
(define match-all-test (single-test regexp-matches 'regexp-matches))
(define search-test (single-test regexp-search 'regexp-search))

(define (match-some-test* sre entries)
  (map (lambda (entry)
	 (if (string? entry)
	     (match-some-test (list 0 (string-length entry)) sre entry)
	     (match-some-test (cadr entry) sre (car entry))))
       entries))

(define (search-test* sre entries)
  (map (lambda (p)
	 (search-test (cadr p) sre (car p)))
       entries))

(define (pattern-error-test sre)
  (lambda ()
    (assert-error (lambda () (regexp sre))
		  (list condition-type:compile-regexp))))

(define-test 'match-nonl
  (match-some-test* 'nonl
		    '(("" #f)
		      ("a" (0 1))
		      ("b" (0 1))
		      ("\n" #f))))

(define-test 'search-nonl
  (search-test* 'nonl
		'(("" #f)
		  ("a" (0 1))
		  ("b" (0 1))
		  ("\n" #f)
		  ("ab" (0 1))
		  ("\na" (1 2)))))

(define-test 'match-*nonl
  (match-some-test* '(* nonl)
		    '(("" (0 0))
		      ("a" (0 1))
		      ("ab" (0 2))
		      ("abc" (0 3))
		      ("ab\n" (0 2))
		      ("a\nb" (0 1)))))

(define-test 'search-+nonl
  (search-test* '(+ nonl)
		'(("" #f)
		  ("a" (0 1))
		  ("ab" (0 2))
		  ("abc" (0 3))
		  ("ab\n" (0 2))
		  ("a\nb" (0 1))
		  ("\nab" (1 3)))))

(define-test 'match-simple-seq
  (map (lambda (sre)
	 (match-some-test '(0 2) sre "ab"))
       '((: "a" "b")
	 (seq "a" "b"))))

(define-test 'search-simple-seq
  (map (lambda (sre)
	 (search-test '(4 6) sre "1914ab37"))
       '((: "a" "b")
	 (seq "a" "b"))))

(define-test 'match/repeat-equivalences-test
  (let ((equivalents
	 (lambda (indices . sres)
	   (map (let ((strings '("" "a" "b" "ab" "ba" "aab")))
		  (lambda (sre)
		    (match-some-test*
		     sre
		     (map (lambda (string index)
			    (list string
				  (and index (list 0 index))))
			  strings
			  indices))))
		sres))))
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
  (match-some-test '(0 2) '(: (? "a") "a") "aab")
  (match-some-test '(0 3) '(: (? "a") "ab") "aab")

  (match-some-test '(0 1) '(: (?? "a") "a") "aab")
  (match-some-test '(0 3) '(: (?? "a") "ab") "aab")

  (match-some-test '(0 2) '(** 1 2 "a") "aab")
  (match-some-test '(0 3) '(: (** 1 2 "a") "b") "aab")

  (match-some-test '(0 1) '(**? 1 2 "a") "aab")
  (match-some-test '(0 3) '(: (**? 1 2 "a") "b") "aab")

  (match-some-test '(0 3) '(** 1 3 "a") "aaab")
  (match-some-test '(0 4) '(: (** 1 3 "a") "b") "aaab")

  (match-some-test '(0 1) '(**? 1 3 "a") "aaab")
  (match-some-test '(0 4) '(: (**? 1 3 "a") "b") "aaab")

  (match-some-test '(0 2 (foo . "a")) '(: (-> foo (? "a")) "a") "aab")
  (match-some-test '(0 3 (foo . "a")) '(: (-> foo (? "a")) "ab") "aab")
  (match-some-test '(0 3 (foo . "")) '(: (-> foo (? "a")) "aab") "aab")

  (match-some-test '(0 1 (foo . "")) '(: (-> foo (?? "a")) "a") "aab")
  (match-some-test '(0 3 (foo . "a")) '(: (-> foo (?? "a")) "ab") "aab")
  (match-some-test '(0 3 (foo . "")) '(: (-> foo (?? "a")) "aab") "aab")

  (match-some-test '(0 3 (foo . "aa")) '(: (-> foo (* "a")) "b") "aab")
  (match-some-test '(0 3 (foo . "a")) '(: (-> foo (* "a")) "ab") "aab")
  (match-some-test '(0 3 (foo . "")) '(: (-> foo (* "a")) "aab") "aab")

  (match-some-test '(0 3 (foo . "aa")) '(: (-> foo (*? "a")) "b") "aab")
  (match-some-test '(0 3 (foo . "a")) '(: (-> foo (*? "a")) "ab") "aab")
  (match-some-test '(0 3 (foo . "")) '(: (-> foo (*? "a")) "aab") "aab")

  )

(define-test 'search-repeat-tests
  (search-test '(0 2) '(: (? "a") "a") "aab")
  (search-test '(1 3) '(: (? "a") "a") "xaab")
  (search-test '(0 3) '(: (? "a") "ab") "aab")
  (search-test '(1 4) '(: (? "a") "ab") "xaab")

  (search-test '(0 1) '(: (?? "a") "a") "aab")
  (search-test '(1 2) '(: (?? "a") "a") "xaab")
  (search-test '(0 3) '(: (?? "a") "ab") "aab")
  (search-test '(1 4) '(: (?? "a") "ab") "xaab")

  (search-test '(0 2) '(** 1 2 "a") "aab")
  (search-test '(1 3) '(** 1 2 "a") "xaab")
  (search-test '(0 3) '(: (** 1 2 "a") "b") "aab")
  (search-test '(1 4) '(: (** 1 2 "a") "b") "xaab")

  (search-test '(0 1) '(**? 1 2 "a") "aab")
  (search-test '(1 2) '(**? 1 2 "a") "xaab")
  (search-test '(0 3) '(: (**? 1 2 "a") "b") "aab")
  (search-test '(1 4) '(: (**? 1 2 "a") "b") "xaab")

  (search-test '(0 3) '(** 1 3 "a") "aaab")
  (search-test '(1 4) '(** 1 3 "a") "xaaab")
  (search-test '(0 4) '(: (** 1 3 "a") "b") "aaab")
  (search-test '(1 5) '(: (** 1 3 "a") "b") "xaaab")

  (search-test '(0 1) '(**? 1 3 "a") "aaab")
  (search-test '(1 2) '(**? 1 3 "a") "xaaab")
  (search-test '(0 4) '(: (**? 1 3 "a") "b") "aaab")
  (search-test '(1 5) '(: (**? 1 3 "a") "b") "xaaab")

  (search-test '(0 2 (foo . "a")) '(: (-> foo (? "a")) "a") "aab")
  (search-test '(1 3 (foo . "a")) '(: (-> foo (? "a")) "a") "xaab")
  (search-test '(0 3 (foo . "a")) '(: (-> foo (? "a")) "ab") "aab")
  (search-test '(1 4 (foo . "a")) '(: (-> foo (? "a")) "ab") "xaab")
  (search-test '(0 3 (foo . "")) '(: (-> foo (? "a")) "aab") "aab")
  (search-test '(1 4 (foo . "")) '(: (-> foo (? "a")) "aab") "xaab")

  (search-test '(0 1 (foo . "")) '(: (-> foo (?? "a")) "a") "aab")
  (search-test '(1 2 (foo . "")) '(: (-> foo (?? "a")) "a") "xaab")
  (search-test '(0 3 (foo . "a")) '(: (-> foo (?? "a")) "ab") "aab")
  (search-test '(1 4 (foo . "a")) '(: (-> foo (?? "a")) "ab") "xaab")
  (search-test '(0 3 (foo . "")) '(: (-> foo (?? "a")) "aab") "aab")
  (search-test '(1 4 (foo . "")) '(: (-> foo (?? "a")) "aab") "xaab")

  (search-test '(0 3 (foo . "aa")) '(: (-> foo (* "a")) "b") "aab")
  (search-test '(1 4 (foo . "aa")) '(: (-> foo (* "a")) "b") "xaab")
  (search-test '(0 3 (foo . "a")) '(: (-> foo (* "a")) "ab") "aab")
  (search-test '(1 4 (foo . "a")) '(: (-> foo (* "a")) "ab") "xaab")
  (search-test '(0 3 (foo . "")) '(: (-> foo (* "a")) "aab") "aab")
  (search-test '(1 4 (foo . "")) '(: (-> foo (* "a")) "aab") "xaab")

  (search-test '(0 3 (foo . "aa")) '(: (-> foo (*? "a")) "b") "aab")
  (search-test '(1 4 (foo . "aa")) '(: (-> foo (*? "a")) "b") "xaab")
  (search-test '(0 3 (foo . "a")) '(: (-> foo (*? "a")) "ab") "aab")
  (search-test '(1 4 (foo . "a")) '(: (-> foo (*? "a")) "ab") "xaab")
  (search-test '(0 3 (foo . "")) '(: (-> foo (*? "a")) "aab") "aab")
  (search-test '(1 4 (foo . "")) '(: (-> foo (*? "a")) "aab") "xaab")

  )

;;; Ripped off from "grep/tests/bre.tests".
(define-test 'match-grep-bre
  (match-some-test* '(: "a" (: "b") "c")
		    '("abc"))
  (match-some-test* '(: "a" (:) "b")
		    '("ab"))
  (match-some-test* '(: (* "a")
			(: bos "b" eos)
			(* "c"))
		    '("b"))
  (match-some-test* '(:)
		    '(("abc" (0 0))))
  (match-some-test* '(: (= 1 "a") "b")
		    '("ab"))
  (match-some-test* '(: (>= 1 "a") "b")
		    '("ab"))
  (match-some-test* '(: (** 1 2 "a") "b")
		    '("aab"))
  (match-some-test* '(: "a" (= 0 "b") "c")
		    '("ac"
		      ("abc" #f)))
  (match-some-test* '(: "a" (** 0 1 "b") "c")
		    '("ac"
		      "abc"
		      ("abbc" #f)))
  (match-some-test* '(: "a" (** 0 3 "b") "c")
		    '("ac"
		      "abc"
		      "abbc"
		      "abbbc"
		      ("abbbbc" #f)))
  (match-some-test* '(: "a" (= 1 "b") "c")
		    '(("ac" #f)
		      "abc"))
  (match-some-test* '(: "a" (** 1 3 "b") "c")
		    '(("ac" #f)
		      "abc"))
  (match-some-test* '(: "a" (= 2 "b") "c")
		    '(("abc" #f)
		      "abbc"))
  (match-some-test* '(: "a" (** 2 4 "b") "c")
		    '(("abcabbc" #f)))
  (match-some-test* '(: (** 0 1 "-")
			(+ ("0123456789"))
			eos)
		    '("-5"))
  (pattern-error-test '(: "a" (** 1 0 "b") "c"))
  (pattern-error-test '(: "a" (** #f 1 "b") "c")))

;;; Ripped off from "grep/tests/ere.tests".
(define-test 'match-grep-ere
  (match-some-test* '(or "abc" "de")
		    '("abc"))
  (match-some-test* '(or "a" "b" "c")
		    '(("abc" (0 1))))
  (match-some-test* '(: "a" nonl "c")
		    '("abc"))
  (match-some-test* '(: "a" ("bc") "d")
		    '("abd"))
  (match-some-test* '(: "a" (* "b") "c")
		    '("abc"))
  (match-some-test* '(: "a" (+ "b") "c")
		    '("abc"))
  (match-some-test* '(: "a" (? "b") "c")
		    '("abc"))
  (match-some-test* '(: "a" ("b") "c")
		    '("abc"))
  (match-some-test* '(: "a" ("ab") "c")
		    '("abc"))
  (match-some-test* '(: "a" (~ ("ab")) "c")
		    '(("abc" #f)
		      "adc"))
  (match-some-test* '(: "a" alphabetic "c")
		    '("abc"
		      "adc"))
  (match-some-test* '(: "a" (+ numeric) "c")
		    '("a019c"))
  (match-some-test* '(: "A" (+ lower-case) "C")
		    '("AabC"))
  (match-some-test* '(: "a" (+ upper-case) "c")
		    '("aBCc"))
  (match-some-test* '(: "a" (= 20 ("ab")))
		    '("aaaaabaaaabaaaabaaaab"))
  (match-some-test* '(: "a"
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab"))
		    '("aaaaabaaaabaaaabaaaab"))
  (match-some-test* '(: "a"
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			("ab") ("ab") ("ab") ("ab")
			(or "wee" "week")
			(or "knights" "night"))
		    '("aaaaabaaaabaaaabaaaabweeknights"))
  (match-some-test* '(: ("ab") ("cd") ("ef") ("gh")
			("ij") ("kl") ("mn"))
		    '(("acegikmoq" (0 7))))
  (match-some-test* '(: ("ab") ("cd") ("ef") ("gh")
			("ij") ("kl") ("mn") ("op"))
		    '(("acegikmoq" (0 8))))
  (match-some-test* '(: ("ab") ("cd") ("ef") ("gh")
			("ij") ("kl") ("mn") ("op")
			("qr"))
		    '(("acegikmoqy" (0 9))))
  (match-some-test* '(: ("ab") ("cd") ("ef") ("gh")
			("ij") ("kl") ("mn") ("op")
			("q"))
		    '(("acegikmoqy" (0 9))))
  (match-some-test* '"aBc"
		    '(("Abc" #f)))
  (match-some-test* '(: "a" (* ("Bc")) "d")
		    '("acBd"
		      "aBcd"
		      "aBcBcBd"
		      ("aBCd" #f)
		      ("abcd" #f)
		      ("abBCcd" #f)))
  (match-some-test* '(: "a" (~ ("b")) "c")
		    '(("abc" #f)
		      "aBc"
		      "adc"))
  (match-some-test* '(: ("a") "b" ("c"))
		    '("abc"))
  (match-some-test* '(: ("a") "b" ("a"))
		    '("aba"))
  (match-some-test* '(: ("abc") "b" ("abc"))
		    '("abc"))
  (match-some-test* '(: ("abc") "b" ("abd"))
		    '(("abc" #f)
		      "abd"))
  (match-some-test* '(: "a" (+ (? "b") "c") "d")
		    '("accd"))
  (match-some-test* '(* "a")
		    '(("b" (0 0))))
  (match-some-test* '(: (or "wee" "week") (or "knights" "night"))
		    '("weeknights"))
  (match-some-test* '(: (or "we" "wee" "week" "frob")
			(or "knights" "night" "day"))
		    '("weeknights"))
  (match-some-test* '"abcdefghijklmnop"
		    '("abcdefghijklmnop"))
  (match-some-test* '"abcdefghijklmnopqrstuv"
		    '("abcdefghijklmnopqrstuv"))
  (match-some-test* '(or (: "CC" ("13") "1")
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
		    '("CC11")))

;; Ripped off from "grep/tests/khadafy.*".
(define-test 'match-grep-muammar-qaddafi
  (match-some-test*
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
  (match-some-test* '"abc"
		    '("abc"
		      ("xbc" #f)
		      ("axc" #f)
		      ("abx" #f)))
  (match-some-test* '(: "a" (* "b") "c")
		    '("abc"))
  (match-some-test* '(: "a" (* "b") "bc")
		    '("abc"
		      "abbc"
		      "abbbbc"))
  (match-some-test* '(: "a" (+ "b") "bc")
		    '(("abc" #f)
		      "abbc"
		      "abbbbc"
		      ("abq" #f)))
  (match-some-test* '(: "a" (? "b") "bc")
		    '("abc"
		      "abbc"
		      ("abbbbc" #f)))
  (match-some-test* '(: "a" (? "b") "c")
		    '("abc"))
  (match-some-test* '(: bos "abc" eos)
		    '("abc"
		      ("abcc" #f)))
  (match-some-test* '(: bos "abc")
		    '(("abcc" (0 3))))
  (match-some-test* 'bos
		    '(("abc" (0 0))))
  (match-some-test* 'eos
		    '(""
		      ("a" #f)))
  (match-some-test* '(: "a" nonl "c")
		    '("abc"
		      "axc"))
  (match-some-test* '(: "a" (* nonl) "c")
		    '("axyzc"
		      ("axyzd" #f)))
  (match-some-test* '(: "a" ("bc") "d")
		    '(("abc" #f)
		      "abd"))
  (match-some-test* '(: "a" ("bcd") "e")
		    '(("abd" #f)
		      "ace"))
  (match-some-test* '(: "a" ("bcd"))
		    '("ac"
		      ("aac" #f)))
  (match-some-test* '(: "a" (~ ("bc")) "d")
		    '("aed"
		      ("abd" #f)))
  (match-some-test* '(: (+ "a") (+ "b") "c")
		    '("abc"
		      "aabbc"
		      ("aabbabc" #f)))
  (match-some-test* '(* (* "a"))
		    '(("-" (0 0))))
  (match-some-test* '(+ (* "a"))
		    '(("-" (0 0))))
  (match-some-test* '(? (* "a"))
		    '(("-" (0 0))))
  (match-some-test* '(* (or "a" (:)))
		    '(("-" (0 0))))
  (match-some-test* '(* (or (* "a") "b"))
		    '(("-" (0 0))))
  (match-some-test* '(* (or (+ "a") "b"))
		    '("ab"))
  (match-some-test* '(+ (or (+ "a") "b"))
		    '("ab"))
  (match-some-test* '(? (or (+ "a") "b"))
		    '(("ba" (0 1))
		      ("ab" (0 1))))
  (match-some-test* '(* (~ ("ab")))
		    '("cde"))
  (match-some-test* '(: (* ("abc")) "d")
		    '("abbbcd"))
  (match-some-test* '(: (* ("abc")) "bcd")
		    '("abcd"))
  (match-some-test* '(or "a" "b" "c" "d" "e")
		    '("e"))
  (match-some-test* '(: (or "a" "b" "c" "d" "e") "f")
		    '("ef"))
  (match-some-test* '(: "abc" (* "d") "efg")
		    '("abcdefg"))
  (match-some-test* '"multiple words of text"
		    '(("uh-uh" #f)))
  (match-some-test* '"multiple words"
		    '(("multiple words, yeah" (0 14)))))

;; Ripped off from Chibi 0.8.0.

(define-test 'chibi-match/search
  (match-all-test '(0 5 (1 . "abab"))
		  '(: ($ (* "ab")) "c")
		  "ababc")
  (match-all-test '(1 6 (1 . "abab"))
		  '(: ($ (* "ab")) "c")
		  "xababc" 1)
  (search-test '(1 2) '(: "y") "xy")
  (search-test '(1 6 (1 . "abab"))
	       '(: ($ (* "ab")) "c")
	       "xababc")
  (match-all-test #f
		  '(: (* any) ($ "foo" (* any)) ($ "bar" (* any)))
		  "fooxbafba")
  (match-all-test '(0 11 (1 . "fooxbarf") (2 . "bar"))
		  '(: (* any) ($ "foo" (* any)) ($ "bar" (* any)))
		  "fooxbarfbar")
  (match-all-test '(0 4 (1 . "abcd"))
		  '($ (* (or "ab" "cd")))
		  "abcd")
  (match-all-test '(0 2 (foo . "ab"))
		  '(or (-> foo "ab") (-> foo "cd"))
		  "ab")
  (match-all-test '(0 2 (foo . "cd"))
		  '(or (-> foo "ab") (-> foo "cd"))
		  "cd")

  ;; non-deterministic case from issue #229
  (let* ((elapsed
	  '(: (** 1 2 numeric) ":" numeric numeric (? ":" numeric numeric)))
         (span `(: ,elapsed "-" ,elapsed)))
    (search-test '(1 16) span " 1:45:02-2:06:13 "))

  (match-all-test '(0 5 (1 . "abab"))
		  '(: bos ($ (* "ab")) "c")
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: ($ (* "ab")) "c" eos)
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: bos ($ (* "ab")) "c" eos)
		  "ababc")
  (match-all-test #f
		  '(: bos ($ (* "ab")) eos "c")
		  "ababc")
  (match-all-test #f
		  '(: ($ (* "ab")) bos "c" eos)
		  "ababc")

  (match-all-test '(0 5 (1 . "abab"))
		  '(: bol ($ (* "ab")) "c")
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: ($ (* "ab")) "c" eol)
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: bol ($ (* "ab")) "c" eol)
		  "ababc")
  (match-all-test #f
		  '(: bol ($ (* "ab")) eol "c")
		  "ababc")
  (match-all-test #f
		  '(: ($ (* "ab")) bol "c" eol)
		  "ababc")
  (match-all-test '(0 5 (1 . "abc"))
		  '(: (* #\newline) bol ($ (* alpha)) eol (* #\newline))
		  "\nabc\n")
  (match-all-test #f
		  '(: (* #\newline) bol ($ (* alpha)) eol (* #\newline))
		  "\n'abc\n")
  (match-all-test #f
		  '(: (* #\newline) bol ($ (* alpha)) eol (* #\newline))
		  "\nabc.\n")

  (match-all-test '(0 5 (1 . "abab"))
		  '(: bow ($ (* "ab")) "c")
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: ($ (* "ab")) "c" eow)
		  "ababc")
  (match-all-test '(0 5 (1 . "abab"))
		  '(: bow ($ (* "ab")) "c" eow)
		  "ababc")
  (match-all-test #f
		  '(: bow ($ (* "ab")) eow "c")
		  "ababc")
  (match-all-test #f
		  '(: ($ (* "ab")) bow "c" eow)
		  "ababc")
  (match-all-test '(0 7 (1 . "abc"))
		  '(: (* space) bow ($ (* alpha)) eow (* space))
		  "  abc  ")
  (match-all-test #f
		  '(: (* space) bow ($ (* alpha)) eow (* space))
		  " 'abc  ")
  (match-all-test #f
		  '(: (* space) bow ($ (* alpha)) eow (* space))
		  " abc.  ")
  (match-all-test '(0 5 (1 . "abc"))
		  '(: ($ (* alpha)) (* any))
		  "abc  ")
  (match-all-test '(0 5 (1 . ""))
		  '(: ($ (*? alpha)) (* any))
		  "abc  ")
  (match-all-test '(0 20 (1 . "em>Hello World</em"))
		  '(: "<" ($ (* any)) ">" (* any))
		  "<em>Hello World</em>")
  (match-all-test '(0 20 (1 . "em"))
		  '(: "<" ($ (*? any)) ">" (* any))
		  "<em>Hello World</em>")
  (search-test '(1 4) '(: "foo") " foo ")
  (search-test #f '(: nwb "foo" nwb) " foo ")
  (search-test '(1 4) '(: nwb "foo" nwb) "xfoox")

  (match-all-test '(0 4) '(* (/ "af")) "beef")

  (match-all-test '(0 9 (1 . "beef"))
		  '(: (* numeric) ($ (* (/ "af"))))
		  "12345beef")

  (let ((number '($ (+ numeric))))
    (list
     (search-test '(0 12 (1 . "555") (2 . "867") (3 . "5309"))
		  `(: ,number "-" ,number "-" ,number)
		  "555-867-5309")
     (search-test '(0 12 (1 . "555") (2 . "5309"))
		  `(: ,number "-" (w/nocapture ,number) "-" ,number)
		  "555-867-5309")))

  (match-all-test '(0 9 (1 . "BeeF"))
		  '(: (* numeric) (w/nocase ($ (* (/ "af")))))
		  "12345BeeF")

  (match-all-test #f '(* lower) "abcD")
  (match-all-test '(0 4) '(w/nocase (* lower)) "abcD")
  (match-all-test '(0 2) '(* lower) "σζ")
  (match-all-test '(0 1) '(* upper) "Σ")
  (match-all-test '(0 1) '(* title) "\x01C5;")
  (match-all-test '(0 3) '(w/nocase (* lower)) "σζ\x01C5;")

  (match-all-test '(0 9) '(* alpha) "кириллица")
  (match-all-test #f '(w/ascii (* alpha)) "кириллица")
  (match-all-test '(0 9) '(w/nocase "КИРИЛЛИЦА") "кириллица")

  (match-all-test '(0 5) '(* numeric) "１２３４５")
  (match-all-test #f '(w/ascii (* numeric)) "１２３４５")

  (match-all-test '(0 1) 'grapheme (string->nfc "한"))
  (match-all-test '(0 1) 'grapheme (string->nfc "글"))

  (match-all-test '(0 1) '(: bog grapheme eog) (string->nfc "한"))
  (match-all-test #f '(: "ᄒ" bog grapheme eog "ᆫ") (string->nfc "한")))

(define-test 'chibi-extract
  (lambda ()
    (assert-equal (regexp-extract '(+ numeric) "abc123def456ghi789")
		  '("123" "456" "789"))
    (assert-equal (regexp-extract '(* numeric) "abc123def456ghi789")
		  '("123" "456" "789"))
    (assert-equal (regexp-extract
		   'grapheme
		   (utf8->string
		    '#u8(#xe1 #x84 #x92 #xe1 #x85 #xa1 #xe1 #x86 #xab
			      #xe1 #x84 #x80 #xe1 #x85 #xb3 #xe1 #x86 #xaf)))
		  '("한" "글"))))

(define-test 'chibi-split
  (lambda ()
    (assert-equal (regexp-split '(+ numeric) "abc123def456ghi789")
		  '("abc" "def" "ghi" ""))
    (assert-equal (regexp-split '(* numeric) "abc123def456ghi789")
		  '("abc" "def" "ghi" ""))
    (assert-equal (regexp-split '(+ whitespace) "a b") '("a" "b"))
    (assert-equal (regexp-split '(",;") "a,,b")
		  '("a" "" "b"))
    (assert-equal (regexp-split '(",;") "a,,b,")
		  '("a" "" "b" ""))))

(define-test 'chibi-partition
  (lambda ()
    (assert-equal (regexp-partition '(* numeric) "")
		  '(""))
    (assert-equal (regexp-partition '(* numeric) "abc123def456ghi")
		  '("abc" "123" "def" "456" "ghi"))
    (assert-equal (regexp-partition '(* numeric) "abc123def456ghi789")
		  '("abc" "123" "def" "456" "ghi" "789"))))

(define-test 'chibi-replace
  (lambda ()
    (assert-equal (regexp-replace '(+ space) "abc \t\n def" " ") "abc def")

    (assert-equal (regexp-replace '(: ($ (+ alpha)) ":" (* space)) "  abc: "
				  '(1 "-" 1))
		  "  abc-abc")
    (assert-equal (regexp-replace '(: ($ (+ alpha)) ":" (* space)) "  abc: "
				  '(1 "-" pre 1))
		  "  abc-  abc")

    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0)
		  "-abc \t\n d ef  ")
    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0 #f 0)
		  "-abc \t\n d ef  ")
    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0 #f 1)
		  "  abc-d ef  ")
    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0 #f 2)
		  "  abc \t\n d-ef  ")
    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0 #f 3)
		  "  abc \t\n d ef-")
    (assert-equal (regexp-replace '(+ space) "  abc \t\n d ef  " "-" 0 #f 4)
		  "  abc \t\n d ef  ")
    (assert-equal (regexp-replace-all '(+ space) "  abc \t\n d ef  " " ")
		  " abc d ef ")))