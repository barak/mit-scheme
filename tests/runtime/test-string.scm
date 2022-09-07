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

;;;; Tests of string operations

(declare (usual-integrations))

;;;; Tests adapted from the Larceny R7RS test suite:

(define-test 'larceny-string
  (lambda ()

    (assert-string-ci< "A" "z")
    (assert-string-ci< "A" "z")
    (assert-string-ci< "a" "Z")
    (assert-string-ci< "a" "Z")
    (assert-string-ci<= "A" "z")
    (assert-string-ci<= "A" "z")
    (assert-string-ci<= "Z" "z")
    (assert-string-ci<= "Z" "z")
    (assert-string-ci<= "a" "Z")
    (assert-string-ci<= "a" "Z")
    (assert-string-ci<= "z" "Z")
    (assert-string-ci<= "z" "Z")
    (assert-string-ci= "z" "Z")
    (assert-string-ci!= "z" "a")
    (assert-string-ci> "Z" "a")
    (assert-string-ci> "Z" "a")
    (assert-string-ci> "z" "A")
    (assert-string-ci> "z" "A")
    (assert-string-ci>= "Z" "a")
    (assert-string-ci>= "Z" "a")
    (assert-string-ci>= "Z" "z")
    (assert-string-ci>= "Z" "z")
    (assert-string-ci>= "z" "A")
    (assert-string-ci>= "z" "A")
    (assert-string-ci>= "z" "Z")
    (assert-string-ci>= "z" "Z")

    (assert-string= (string-upcase "Hi") "HI")
    (assert-string= (string-upcase "HI") "HI")
    (assert-string= (string-downcase "Hi") "hi")
    (assert-string= (string-downcase "hi") "hi")
    (assert-string= (string-foldcase "Hi") "hi")
    (assert-string= (string-foldcase "HI") "hi")
    (assert-string= (string-foldcase "hi") "hi")
    (assert-string= (string-downcase "STRASSE")  "strasse")

    (assert-string= (string-upcase "Stra\xDF;e") "STRASSE")
    (assert-string= (string-downcase "Stra\xDF;e") "stra\xDF;e")
    (assert-string= (string-foldcase "Stra\xDF;e") "strasse")
    (assert-string= (string-downcase "\x3A3;") "\x3C3;")

    (assert-string= (string-upcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x39E;\x391;\x39F;\x3A3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C2;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C3;\x3C2;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;\x3C3;")
    ;; Would be "\x3BE;\x3B1;\x3BF;\x3C2; \x3C3;" with final sigma
    (assert-string= (string-downcase "\x39E;\x391;\x39F;\x3A3; \x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3; \x3C3;")
    (assert-string= (string-foldcase "\x39E;\x391;\x39F;\x3A3;")
		    "\x3BE;\x3B1;\x3BF;\x3C3;")
    (assert-string= (string-upcase "\x3BE;\x3B1;\x3BF;\x3C3;")
		    "\x39E;\x391;\x39F;\x3A3;")
    (assert-string= (string-upcase "\x3BE;\x3B1;\x3BF;\x3C2;")
		    "\x39E;\x391;\x39F;\x3A3;")

    (assert-string= (string-downcase "A\x3A3;'x") ; ' is a MidLetter
		    "a\x3C3;'x")

    (assert-string-ci= "Strasse" "Stra\xDF;e")
    (assert-string-ci= "STRASSE" "Stra\xDF;e")
    (assert-string-ci= "\x3BE;\x3B1;\x3BF;\x3C2;" "\x39E;\x391;\x39F;\x3A3;")
    (assert-string-ci= "\x3BE;\x3B1;\x3BF;\x3C3;" "\x39E;\x391;\x39F;\x3A3;")))

(define-test 'string-builder
  (lambda ()
    (let ((end (length latin-alphabet)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i end)))
	(let ((chars (take latin-alphabet i)))
	  (let ((result (build-string chars)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (chars->string chars))))
	(let ((strings (make-test-strings i latin-alphabet #f)))
	  (let ((result (build-string strings)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (string-append* strings))))
	(let ((strings (make-test-strings i latin-alphabet #t)))
	  (let ((result (build-string strings)))
	    (assert-true (legacy-string? result))
	    (assert-string= result (string-append* strings))))))
    (let ((end (length greek-alphabet)))
      (do ((i 0 (fix:+ i 1)))
	  ((not (fix:< i end)))
	(let ((chars (take greek-alphabet i)))
	  (assert-string= (build-string chars)
			  (chars->string chars)))
	(let ((strings (make-test-strings i greek-alphabet #f)))
	  (assert-string= (build-string strings)
			  (string-append* strings)))
	(let ((strings (make-test-strings i greek-alphabet #t)))
	  (assert-string= (build-string strings)
			  (string-append* strings)))))))

(define legacy-string?
  (make-primitive-procedure 'string? 1))

(define latin-alphabet
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(define greek-alphabet
  '(#\x3B1 #\x3B2 #\x3B3 #\x3B4 #\x3B5
    #\x3B6 #\x3B7 #\x3B8 #\x3B9 #\x3BA
    #\x3BB #\x3BC #\x3BD #\x3BE #\x3BF
    #\x3C0 #\x3C1 #\x3C2 #\x3C3 #\x3C4
    #\x3C5 #\x3C6 #\x3C7 #\x3C8 #\x3C9))

(define (build-string objects)
  (let ((builder (string-builder)))
    (for-each builder objects)
    (builder)))

(define (chars->string chars)
  (let ((s (make-string (length chars))))
    (do ((chars chars (cdr chars))
	 (i 0 (fix:+ i 1)))
	((not (pair? chars)))
      (string-set! s i (car chars)))
    s))

(define (make-test-strings n alphabet reverse?)
  (let loop ((k 0) (strings '()))
    (if (fix:< k n)
	(loop (fix:+ k 1)
	      (cons (chars->string (take alphabet k))
		    strings))
	(if reverse?
	    strings
	    (reverse! strings)))))

(define-test 'string-trim
  (lambda ()
    (define-integrable (= expected value) (assert-string= value expected))
    (= "foo" (string-trim "foo   "))
    (= "foo" (string-trim "   foo"))
    (= "foo" (string-trim "   foo   "))
    (= "foo   " (string-trim-left "   foo   "))
    (= "   foo" (string-trim-right "   foo   "))
    (= "" (string-trim "\"\"" (char-set-invert (char-set #\"))))
    (= "" (string-trim-left "\"\"" (char-set-invert (char-set #\"))))
    (= "" (string-trim-right "\"\"" (char-set-invert (char-set #\"))))
    (= "foo" (string-trim "aaafooaaa" (char-set #\f #\o)))
    (= "fooaaa" (string-trim-left "aaafooaaa" (char-set #\f #\o)))
    (= "aaafoo" (string-trim-right "aaafooaaa" (char-set #\f #\o)))))

(define (string-copy!:all-indices to from)
  (let ((to-length (string-length to)))
    (append-map (lambda (s+e)
		  (map (lambda (at)
			 (cons at s+e))
		       (iota (- to-length (- (cadr s+e) (car s+e))))))
		(substring:all-indices from))))

(define (substring:all-indices string)
  (let ((limit (+ (string-length string) 1)))
    (append-map (lambda (start)
		  (map (lambda (end)
			 (list start end))
		       (iota (- limit start) start)))
		(iota limit))))

(define-test 'string-copy!:different-strings
  (let ((s1 "abcdefghijklmnopqrstuvwxyz")
	(s2 "0123456789"))
    (map (lambda (indices)
	   (let ((at (car indices))
		 (start (cadr indices))
		 (end (caddr indices)))
	     (lambda ()
	       (let ((sut (string-copy s1)))
		 (string-copy! sut at s2 start end)
		 (assert-string= sut
				 (string-copy!:predict s1 at s2 start end)
				 'expression
				 `(string-copy! ,s1 ,at ,s2 ,start ,end))))))
	 (string-copy!:all-indices s1 s2))))

(define-test 'string-copy!:same-string
  (let ((s1 "abcdefghijklmnopqrstuvwxyz"))
    (map (lambda (indices)
	   (let ((at (car indices))
		 (start (cadr indices))
		 (end (caddr indices)))
	     (let ((expr `(string-copy! ,s1 ,at ,s1 ,start ,end)))
	       (lambda ()
		 (let ((sut (string-copy s1)))
		   (string-copy! sut at sut start end)
		   (assert-string= sut
				   (string-copy!:predict s1 at s1 start end)
				   'expression expr))))))
	 (string-copy!:all-indices s1 s1))))

(define (string-copy!:predict s1 at s2 #!optional start end)
  (list->string
   (let ((l1 (string->list s1))
	 (l2 (string->list s2 start end)))
     (append (take l1 at)
	     l2
	     (drop l1 (+ at (length l2)))))))

(define-test 'string-slice
  (let ((s "abcdefghijklmnopqrstuvwxyz"))
    (map (lambda (indices)
	   (let ((start (car indices))
		 (end (cadr indices)))
	     (let ((expr `(string-slice ,s ,start ,end)))
	       (lambda ()
		 (let ((sut (string-slice s start end))
		       (expected (substring:predict s start end)))
		   (assert-string= sut expected 'expression expr)
		   (assert-string= (call-with-output-string
				     (lambda (port)
				       (write sut port)))
				   (string-append "\"" expected "\"")))))))
	 (substring:all-indices s))))

(define (substring:predict s i j)
  (list->string (sublist (string->list s) i j)))

(define-test 'string-any
  (lambda ()
    (let ((s1 "1234xy4321")
	  (s2 "56789a9876")
	  (s3 "12345")
	  (s4 "12345x"))

      (assert-true (string-any char-alphabetic? s1))
      (assert-true (string-any char-alphabetic? s2))
      (assert-false (string-any char-alphabetic? s3))
      (assert-true (string-any char-alphabetic? s4))

      (assert-eqv (string-any alpha-value s1) #\x)
      (assert-eqv (string-any alpha-value s2) #\a)
      (assert-eqv (string-any alpha-value s4) #\x)

      (assert-true (string-any both-alphabetic? s1 s2))
      (assert-false (string-any both-alphabetic? s1 s3))
      (assert-true (string-any both-alphabetic? s1 s4))
      (assert-false (string-any both-alphabetic? s2 s3))
      (assert-true (string-any both-alphabetic? s2 s4))

      (assert-equal (string-any both-alpha-value s1 s2)
		    '(#\y #\a))
      (assert-equal (string-any both-alpha-value s1 s4)
		    '(#\y #\x))
      (assert-equal (string-any both-alpha-value s2 s4)
		    '(#\a #\x)))))

(define-test 'string-every
  (lambda ()
    (let ((s1 "abcdefgh")
	  (s2 "wxyz")
	  (s3 "abcd12dcba"))

      (assert-true (string-every char-alphabetic? s1))
      (assert-true (string-every char-alphabetic? s2))
      (assert-false (string-every char-alphabetic? s3))

      (assert-eqv (string-every alpha-value s1) #\h)
      (assert-eqv (string-every alpha-value s2) #\z)

      (assert-true (string-every both-alphabetic? s1 s2))
      (assert-false (string-every both-alphabetic? s1 s3))
      (assert-true (string-every both-alphabetic? s2 s3))

      (assert-equal (string-every both-alpha-value s1 s2)
		    '(#\d #\z))
      (assert-equal (string-every both-alpha-value s2 s3)
		    '(#\z #\d)))))

(define (alpha-value c)
  (and (char-alphabetic? c)
       c))

(define (both-alphabetic? c1 c2)
  (and (char-alphabetic? c1)
       (char-alphabetic? c2)))

(define (both-alpha-value c1 c2)
  (and (char-alphabetic? c1)
       (char-alphabetic? c2)
       (list c1 c2)))