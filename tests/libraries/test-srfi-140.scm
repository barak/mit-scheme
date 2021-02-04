;; -*- coding: utf-8 -*-

;;; Copyright (C) Per Bothner (2017).
;;; Copyright (C) William D Clinger (2016).
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.

(import (except (scheme base)
		list->string
		string
		string->list
		string->utf8
		string->vector
		string-append
		string-for-each
		string-length
		string-map
		string-ref
		string<=?
		string<?
		string=?
		string>=?
		string>?
		string?
		substring
		utf8->string
		vector->string)
	(scheme write)
        (except (scheme char)
		string-ci<=?
		string-ci<?
		string-ci=?
		string-ci>=?
		string-ci>?
		string-downcase
		string-foldcase
		string-upcase)
	(srfi 140))

(define ABC
  (list->string (map integer->char
		     '(#x3b1 #x3b2 #x3b3))))
(define ABCDEF
  (list->string (map integer->char
		     '(#x0c0 #x062 #x0c7 #x064 #x0c9 #x066))))
(define DEFABC
   (list->string (map integer->char
		      '(#x064 #x0c9 #x066 #x0c0 #x062 #x0c7))))
(define eszett (integer->char #xDF))
(define fuss (string #\F #\u eszett))
(define chaos0
  (list->string (map integer->char
		     '(#x39E #x391 #x39F #x3A3))))
(define chaos1
  (list->string (map integer->char
		     '(#x3BE #x3B1 #x3BF #x3C2))))
(define chaos2
  (list->string (map integer->char
		     '(#x3BE #x3B1 #x3BF #x3C3))))
(define beyondBMP
  (list->string (map integer->char
		     '(#x61 #xc0 #x3bf
			    #x1d441 #x1d113 #x1d110 #x7a))))

(define (check-istring str)
  (list (istring? str) (string-length str)))

;;; Predicates

(string? (string)) 'expect-true
(string? #\a) 'expect-false
(string-null? (string)) 'expect-true
(string-null? ABC) 'expect-false

(check-istring "")
'(expect equal? '(#t 0))
(check-istring "abcd")
'(expect equal? '(#t 4))
(check-istring (string #\A #\b #\c #\d))
'(expect equal? '(#t 4))
(check-istring (substring (make-string 4 #\X) 1 4))
'(expect equal? '(#t 3))
(check-istring (make-string 4 #\X))
'(expect equal? '(#f 4))
(check-istring (string-copy (make-string 4 #\X)))
'(expect equal? '(#f 4))
(check-istring (string-copy (make-string 4 #\X) 1 4))
'(expect equal? '(#f 3))
(check-istring (vector->string #(#\x #\y #\z)))
'(expect equal? '(#t 3))
(check-istring (vector->string #(#\x #\y #\z)))
'(expect equal? '(#t 3))
(check-istring (list->string '(#\x #\y #\z)))
'(expect equal? '(#t 3))
(check-istring (reverse-list->string '(#\x #\y #\z)))
'(expect equal? '(#t 3))
(check-istring (utf8->string (string->utf8 "abc")))
'(expect equal? '(#t 3))
(check-istring (utf16->string (string->utf16 "abc")))
'(expect equal? '(#t 3))
(check-istring (utf16be->string (string->utf16be "abc")))
'(expect equal? '(#t 3))
(check-istring (utf16le->string (string->utf16le "abc")))
'(expect equal? '(#t 3))
(check-istring (string-take "abcd" 2))
'(expect equal? '(#t 2))
(check-istring (string-drop "abcd" 2))
'(expect equal? '(#t 2))
(check-istring (string-take-right "abcd" 2))
'(expect equal? '(#t 2))
(check-istring (string-drop-right "abcd" 2))
'(expect equal? '(#t 2))
(check-istring (string-pad "abcd" 5))
'(expect equal? '(#t 5))
(check-istring (string-pad-right "abcd" 3))
'(expect equal? '(#t 3))
(check-istring (string-trim "  A "))
'(expect equal? '(#t 2))
(check-istring (string-trim-right "  A "))
'(expect equal? '(#t 3))
(check-istring (string-trim-both "  A "))
'(expect equal? '(#t 1))
(check-istring (string-replace "AB" "X" 1 1))
'(expect equal? '(#t 3))
(check-istring (string-upcase (make-string 3 #\X)))
'(expect equal? '(#t 3))
(check-istring (string-downcase (make-string 3 #\x)))
'(expect equal? '(#t 3))
(check-istring (string-foldcase (make-string 3 #\x)))
'(expect equal? '(#t 3))
(check-istring (string-titlecase (make-string 3 #\X)))
'(expect equal? '(#t 3))
(check-istring (string-append "abcd" "XY"))
'(expect equal? '(#t 6))
(check-istring (string-concatenate (list "abcd" "XY")))
'(expect equal? '(#t 6))
(check-istring (string-concatenate-reverse  (list "abcd" "XY")))
'(expect equal? '(#t 6))
(check-istring (string-join (list "abc" "xyz")))
'(expect equal? '(#t 7))
(check-istring (string-map char-upcase "abc"))
'(expect equal? '(#t 3))
(check-istring (string-repeat "ab" 3))
'(expect equal? '(#t 6))
(check-istring (xsubstring "abcdef" -4 10))
'(expect equal? '(#t 14))
(check-istring (cadr (string-split "ab cef" " ")))
'(expect equal? '(#t 3))
(check-istring (symbol->string 'Hello))
'(expect equal? '(#t 5))

(string-every (lambda (c) (if (char? c) c #f)) (string))
'(expect eqv? #t)
(string-every (lambda (c) (if (char? c) c #f)) "abc")
'(expect eqv? #\c)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc")
'(expect eqv? #f)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)
'(expect eqv? #\c)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1)
'(expect eqv? #t)
(string-any (lambda (c) (if (char? c) c #f)) (string))
'(expect eqv? #f)
(string-any (lambda (c) (if (char? c) c #f)) "abc")
'(expect eqv? #\a)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc")
'(expect eqv? #\c)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)
'(expect eqv? #\c)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2)
'(expect eqv? #f)

(string-every (lambda (c) (if (char? c) c #f)) "")
'(expect eqv? #t)
(string-every (lambda (c) (if (char? c) c #f)) "abc")
'(expect eqv? #\c)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc")
'(expect eqv? #f)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)
'(expect eqv? #\c)
(string-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1)
'(expect eqv? #t)
(string-any (lambda (c) (if (char? c) c #f)) "")
'(expect eqv? #f)
(string-any (lambda (c) (if (char? c) c #f)) "abc")
'(expect eqv? #\a)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc")
'(expect eqv? #\c)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2)
'(expect eqv? #\c)
(string-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2)
'(expect eqv? #f)

;;; Constructors

(string-tabulate (lambda (i)
                   (integer->char (+ i (char->integer #\a))))
                 0)
'(expect equal? "")

(define s1
   (string-tabulate (lambda (i)
                      (integer->char (+ i (char->integer #\a))))
                    3))
(check-istring s1) '(expect equal? '(#t 3))
s1 '(expect equal? "abc")

(define s2
  (let ((p (open-input-string "abc")))
    (string-unfold eof-object?
                   values
                   (lambda (x) (read-char p))
                   (read-char p))))
(check-istring s2) '(expect equal? '(#t 3))
s2 '(expect equal? "abc")

(string-unfold null? car cdr '())
'(expect equal? "")

(string-unfold null? car cdr (string->list "abc"))
'(expect equal? "abc")

(string-unfold null? car cdr '() "def")
'(expect equal? "def")

(string-unfold null?
	       car
	       cdr
	       (string->list "abc")
	       "def"
	       (lambda (x) (if (null? x) (string #\G) "")))
'(expect equal? "defabcG")

(string-unfold-right null? car cdr '())
'(expect equal? "")

(string-unfold-right null? car cdr (string->list "abc"))
'(expect equal? "cba")

(string-unfold-right null? car cdr '() "def")
'(expect equal? "def")
(check-istring (string-unfold-right null? car cdr '() "def"))
'(expect equal? '(#t 3))

(string-unfold-right null? car cdr
		     (string->list "abc")
		     "def"
		     (lambda (x) (if (null? x) (string #\G) "")))
'(expect equal? "Gcbadef")

(string-unfold null? car cdr '() "def")
'(expect equal? "def")

(string-unfold null? car cdr
	       (string->list "abc")
	       "def"
	       (lambda (x) (if (null? x) "G" "")))
'(expect equal? "defabcG")

(string-unfold null? car cdr
	       (string->list "abc")
	       #\d
	       (lambda (x) (if (null? x) "G" "")))
'(expect equal? "dabcG")

(string-unfold (lambda (n) (char>? (integer->char n) #\z))
               (lambda (n)
                 (let ((c (integer->char n)))
                   (cond ((char<=? #\a c #\z) c)
                         ((char<=? #\A c #\Z) (string c #\space))
                         (else (make-string 200 #\*)))))
               (lambda (n) (+ n 1))
               (char->integer #\@)
               "%="
               (lambda (n) #\space))
'(expect equal?
	 (string-append "%="
                        (make-string 200 #\*)
                        "A B C D E F G H I J K L M "
                        "N O P Q R S T U V W X Y Z "
                        (make-string (* 200 (- (char->integer #\a)
                                               (char->integer #\Z)
                                               1))
                                     #\*)
                        "abcdefghijklmnopqrstuvwxyz"
                        " "))

(string-unfold-right null? car cdr '() "def")
'(expect equal? "def")

(string-unfold-right null? car cdr
                     (string->list "abc")
                     "def"
                     (lambda (x) (if (null? x) "G" "")))
'(expect equal? "Gcbadef")

(string-unfold-right null? car cdr
                     (string->list "abc")
                     #\d
                     (lambda (x) (if (null? x) "G" "")))
'(expect equal? "Gcbad")

(string-unfold-right (lambda (n) (char>? (integer->char n) #\z))
		     (lambda (n)
		       (let ((c (integer->char n)))
			 (cond ((char<=? #\a c #\z) c)
			       ((char<=? #\A c #\Z) (string c #\space))
			       (else (make-string 200 #\*)))))
		     (lambda (n) (+ n 1))
		     (char->integer #\@)
		     "%="
		     (lambda (n) #\space))
'(expect equal? (string-append " "
                               (list->string
				(reverse
				 (string->list "abcdefghijklmnopqrstuvwxyz")))
                               (make-string (* 200 (- (char->integer #\a)
                                                      (char->integer #\Z)
                                                      1))
                                            #\*)
                               "Z Y X W V U T S R Q P O N "
                               "M L K J I H G F E D C B A "
                               (make-string 200 #\*)
                               "%="))

(string-unfold-right (lambda (n) (< n (char->integer #\A)))
                     (lambda (n)
                       (char-downcase (integer->char n)))
                     (lambda (n) (- n 1))
                     (char->integer #\Z)
                     #\space
                     (lambda (n) " The English alphabet: "))
'(expect equal? " The English alphabet: abcdefghijklmnopqrstuvwxyz ")

;;; Conversion

(define s3 (string #\s #\t #\r))
(string? s3) 'expect-true
(istring? s3) 'expect-true
s3 '(expect equal? "str")

(string) '(expect equal? "")
(substring (string) 0 0) '(expect equal? "")
(string #\a #\b #\c) '(expect equal? "abc")
(substring (string #\a #\b #\c) 3 3) '(expect equal? "")
(substring (string #\a #\b #\c) 1 3) '(expect equal? "bc")

(substring "" 0) '(expect equal? "")
(substring "" 0 0) '(expect equal? "")
(substring "abc" 3 3) '(expect equal? "")
(substring "abc" 1 3) '(expect equal? "bc")

(string->vector (string)) '(expect equal? '#())
(string->vector (string) 0) '(expect equal? '#())
(string->vector (string) 0 0) '(expect equal? '#())
(string->vector (string #\a #\b #\c)) '(expect equal? '#(#\a #\b #\c))
(string->vector (string #\a #\b #\c) 3) '(expect equal? '#())
(string->vector (string #\a #\b #\c) 1 3) '(expect equal? '#(#\b #\c))

(string->vector "") '(expect equal? '#())
(string->vector "" 0) '(expect equal? '#())
(string->vector "" 0 0) '(expect equal? '#())
(string->vector "abc") '(expect equal? '#(#\a #\b #\c))
(string->vector "abc" 3) '(expect equal? '#())
(string->vector "abc" 1 3) '(expect equal? '#(#\b #\c))

(string->list (string)) '(expect equal? '())
(string->list (string) 0) '(expect equal? '())
(string->list (string) 0 0) '(expect equal? '())
(string->list (string #\a #\b #\c)) '(expect equal? '(#\a #\b #\c))
(string->list (string #\a #\b #\c) 3) '(expect equal? '())
(string->list (string #\a #\b #\c) 1 3) '(expect equal? '(#\b #\c))

(string->list "") '(expect equal? '())
(string->list "" 0) '(expect equal? '())
(string->list "" 0 0) '(expect equal? '())
(string->list "abc") '(expect equal? '(#\a #\b #\c))
(string->list "abc" 3) '(expect equal? '())
(string->list "abc" 1 3) '(expect equal? '(#\b #\c))

"" '(expect equal? "")
(substring "" 0 0) '(expect equal? "")
(substring "abc" 1 3) '(expect equal? "bc")
(substring "abc" 3 3) '(expect equal? "")
(substring "abc" 1 2) '(expect equal? "b")
(substring "abc" 1 3) '(expect equal? "bc")

(vector->string '#()) '(expect equal? "")
(vector->string '#() 0) '(expect equal? "")
(vector->string '#() 0 0) '(expect equal? "")
(vector->string '#(#\a #\b #\c)) '(expect equal? "abc")
(vector->string '#(#\a #\b #\c) 1) '(expect equal? "bc")
(vector->string '#(#\a #\b #\c) 3) '(expect equal? "")
(vector->string '#(#\a #\b #\c) 1 2) '(expect equal? "b")
(vector->string '#(#\a #\b #\c) 1 3) '(expect equal? "bc")

(list->string '()) '(expect equal? "")

(list->string '() 0) '(expect equal? "")
(list->string '() 0 0) '(expect equal? "")
(list->string '(#\a #\b #\c)) '(expect equal? "abc")
(list->string '(#\a #\b #\c) 1) '(expect equal? "bc")
(list->string '(#\a #\b #\c) 3) '(expect equal? "")
(list->string '(#\a #\b #\c) 1 2) '(expect equal? "b")
(list->string '(#\a #\b #\c) 1 3) '(expect equal? "bc")

(reverse-list->string '()) '(expect equal? "")
(reverse-list->string '(#\a #\b #\c)) '(expect equal? "cba")

(string->utf8 "abc")
'(expect equal? '#u8(97 98 99))
(string->utf8 "xxxabcyyyzzz" 3)
'(expect equal? '#u8(97 98 99 121 121 121 122 122 122))
(string->utf8 "xxxabcyyyzzz" 3 6)
'(expect equal? '#u8(97 98 99))

(string->utf16 "abc")
'(expect equal?
	 (cond-expand (big-endian '#u8(254 255 0 97 0 98 0 99))
                      (else '#u8(255 254 97 0 98 0 99 0))))

(string->utf16 "xxxabcyyyzzz" 3)
'(expect
  equal?
  (cond-expand
    (big-endian
     '#u8(254 255 0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122))
    (else '#u8(255 254 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0))))

(string->utf16 "xxxabcyyyzzz" 3 6)
'(expect equal?
	 (cond-expand (big-endian '#u8(254 255 0 97 0 98 0 99))
                      (else '#u8(255 254 97 0 98 0 99 0))))


(string->utf16be "abc")
'(expect equal? '#u8(0 97 0 98 0 99))
(string->utf16be "xxxabcyyyzzz" 3)
'(expect equal? '#u8(0 97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122))
(string->utf16be "xxxabcyyyzzz" 3 6)
'(expect equal? '#u8(0 97 0 98 0 99))


(string->utf16le "abc")
'(expect equal? '#u8(97 0 98 0 99 0))
(string->utf16le "xxxabcyyyzzz" 3)
'(expect equal? '#u8(97 0 98 0 99 0 121 0 121 0 121 0 122 0 122 0 122 0))
(string->utf16le "xxxabcyyyzzz" 3 6)
'(expect equal? '#u8(97 0 98 0 99 0))

(utf8->string '#u8(97 98 99))
'(expect equal? "abc")
(utf8->string '#u8(0 1 2 97 98 99 121 121 121 122 122 122) 3)
'(expect equal? "abcyyyzzz")
(utf8->string '#u8(41 42 43 97 98 99 100 101 102) 3 6)
'(expect equal? "abc")

(utf16->string '#u8(254 255 0 97 0 98 0 99)) '(expect equal? "abc")
(utf16->string '#u8(255 254 97 0 98 0 99 0)) '(expect equal? "abc")
(utf16->string (string->utf16 "abc") 2) '(expect equal? "abc")
(utf16->string (string->utf16 "abcdef") 4) '(expect equal? "bcdef")
(utf16->string (string->utf16 "abcdef") 4 10) '(expect equal? "bcd")

(utf16be->string '#u8(0 97 0 98 0 99)) '(expect equal? "abc")
(utf16be->string (string->utf16be "abc") 2) '(expect equal? "bc")
(utf16be->string (string->utf16be "abcdef") 2 8) '(expect equal? "bcd")

(utf16le->string '#u8(97 0 98 0 99 0)) '(expect equal? "abc")
(utf16le->string (string->utf16le "abc") 2) '(expect equal? "bc")
(utf16le->string (string->utf16le "abcdef") 2 8) '(expect equal? "bcd")

(string->utf8 beyondBMP)
'(expect equal?
	 '#u8(97 195 128 206 191 240 157 145 129 240 157 132 147 240
		 157 132 144 122))

(string->utf16 beyondBMP)
'(expect equal?
	 (cond-expand (big-endian
		       '#u8(254 255 0 97 0 192 3 191 216 53 220 65 216
				52 221 19 216 52 221 16 0 122))
		      (else
		       '#u8(255 254 97 0 192 0 191 3 53 216 65 220 52
				216 19 221 52 216 16 221 122 0))))

(string->utf16be beyondBMP)
'(expect equal?
	 '#u8(0 97 0 192 3 191 216 53 220 65 216 52 221 19 216 52 221 16 0 122))
(string->utf16le beyondBMP)
'(expect equal?
	 '#u8(97 0 192 0 191 3 53 216 65 220 52 216 19 221 52 216 16 221 122 0))
(utf8->string
 '#u8(97 195 128 206 191 240 157 145 129 240 157 132 147 240 157 132 144 122))
'(expect equal? beyondBMP)
(utf16->string (string->utf16 beyondBMP))
'(expect equal? beyondBMP)
(utf16->string (string->utf16 beyondBMP) 2)
'(expect equal? beyondBMP)
(utf16be->string (string->utf16be beyondBMP))
'(expect equal? beyondBMP)
(utf16le->string (string->utf16le beyondBMP))
'(expect equal? beyondBMP)
(utf16be->string '#u8(254 255 0 97 0 98 0 99))
'(expect equal? (string-append (string (integer->char #xfeff)) "abc"))
(utf16le->string '#u8(255 254 97 0 98 0 99 0))
'(expect equal? (string-append (string (integer->char #xfeff)) "abc"))

;;; Selection

(string-length (string)) '(expect eqv? 0)
(string-length ABCDEF) '(expect eqv? 6)
(string-length (make-string 1234 (string-ref ABC 0))) '(expect eqv? 1234)

(string-ref (string #\a #\b #\c) 0) '(expect eqv? #\a)
(string-ref (string #\a #\b #\c) 2) '(expect eqv? #\c)
(string-length (string)) '(expect eqv? 0)
(string-length ABCDEF) '(expect eqv? 6)
(string-length (make-string 1234 (string-ref ABC 0))) '(expect eqv? 1234)

(string-ref (string #\a #\b #\c) 0) '(expect eqv? #\a)
(string-ref (string #\a #\b #\c) 2) '(expect eqv? #\c)
(substring (string) 0 0) '(expect equal? "")
(substring "abcdef" 0 0) '(expect equal? "")
(substring "abcdef" 4 4) '(expect equal? "")
(substring "abcdef" 6 6) '(expect equal? "")
(substring "abcdef" 0 4) '(expect equal? "abcd")
(substring "abcdef" 2 5) '(expect equal? "cde")
(substring "abcdef" 2 6) '(expect equal? "cdef")
(substring "abcdef" 0 6) '(expect equal? "abcdef")

(substring (string) 0 0) '(expect equal? "")
(substring "abcdef" 0 0) '(expect equal? "")

(substring "abcdef" 4 4) '(expect equal? "")
(substring "abcdef" 6 6) '(expect equal? "")
(substring "abcdef" 0 4) '(expect equal? "abcd")
(substring "abcdef" 2 5) '(expect equal? "cde")
(substring "abcdef" 2 6) '(expect equal? "cdef")
(substring "abcdef" 0 6) '(expect equal? "abcdef")

(substring "" 0 0) '(expect equal? "")
(substring "abcdef" 0 0) '(expect equal? "")
(substring "abcdef" 4 4) '(expect equal? "")
(substring "abcdef" 6 6) '(expect equal? "")
(substring "abcdef" 0 4) '(expect equal? "abcd")
(substring "abcdef" 2 5) '(expect equal? "cde")
(substring "abcdef" 2 6) '(expect equal? "cdef")
(substring "abcdef" 0 6) '(expect equal? "abcdef")

(string-copy (string)) '(expect equal? "")

(define s4 "abcdef")
(define s5 (string-copy s4))
s5 '(expect equal? "abcdef")
s5 '(expect-not eqv? s4)

(string-copy "") '(expect equal? "")
(string-copy "abcdef") '(expect equal? "abcdef")

(string-copy (string) 0) '(expect equal? "")
(string-copy "abcdef" 0) '(expect equal? "abcdef")
(string-copy "abcdef" 4) '(expect equal? "ef")
(string-copy "abcdef" 6) '(expect equal? "")

(string-copy "" 0) '(expect equal? "")
(string-copy "abcdef" 0) '(expect equal? "abcdef")
(string-copy "abcdef" 4) '(expect equal? "ef")
(string-copy "abcdef" 6) '(expect equal? "")

(string-copy (string) 0 0) '(expect equal? "")
(string-copy "abcdef" 0 0) '(expect equal? "")
(string-copy "abcdef" 4 4) '(expect equal? "")

(string-copy "abcdef" 6 6) '(expect equal? "")
(string-copy "abcdef" 0 4) '(expect equal? "abcd")
(string-copy "abcdef" 2 5) '(expect equal? "cde")
(string-copy "abcdef" 2 6) '(expect equal? "cdef")
(string-copy "abcdef" 0 6) '(expect equal? "abcdef")

(string-copy "" 0 0) '(expect equal? "")
(string-copy "abcdef" 0 0) '(expect equal? "")
(string-copy "abcdef" 4 4) '(expect equal? "")
(string-copy "abcdef" 6 6) '(expect equal? "")
(string-copy "abcdef" 0 4) '(expect equal? "abcd")
(string-copy "abcdef" 2 5) '(expect equal? "cde")
(string-copy "abcdef" 2 6) '(expect equal? "cdef")
(string-copy "abcdef" 0 6) '(expect equal? "abcdef")

(string-take (string) 0) '(expect equal? "")
(string-take "abcdef" 0) '(expect equal? "")
(string-take "abcdef" 2) '(expect equal? "ab")

(string-drop "" 0) '(expect equal? "")
(string-drop "abcdef" 0) '(expect equal? "abcdef")
(string-drop "abcdef" 2) '(expect equal? "cdef")

(string-take-right (string) 0) '(expect equal? "")
(string-take-right "abcdef" 0) '(expect equal? "")
(string-take-right "abcdef" 2) '(expect equal? "ef")

(string-drop-right (string) 0) '(expect equal? "")
(string-drop-right "abcdef" 0) '(expect equal? "abcdef")
(string-drop-right "abcdef" 2) '(expect equal? "abcd")

(string-take "" 0) '(expect equal? "")
(string-take "abcdef" 0) '(expect equal? "")
(string-take "abcdef" 2) '(expect equal? "ab")
(string-drop "" 0) '(expect equal? "")
(string-drop "abcdef" 0) '(expect equal? "abcdef")
(string-drop "abcdef" 2) '(expect equal? "cdef")

(string-take-right "" 0) '(expect equal? "")
(string-take-right "abcdef" 0) '(expect equal? "")
(string-take-right "abcdef" 2) '(expect equal? "ef")
(string-drop-right "" 0) '(expect equal? "")
(string-drop-right "abcdef" 0) '(expect equal? "abcdef")
(string-drop-right "abcdef" 2) '(expect equal? "abcd")

(string-pad "" 0) '(expect equal? "")
(string-pad "" 5) '(expect equal? "     ")
(string-pad "325" 5) '(expect equal? "  325")
(string-pad "71325" 5) '(expect equal? "71325")
(string-pad "8871325" 5) '(expect equal? "71325")
(string-pad "" 0 #\*) '(expect equal? "")
(string-pad "" 5 #\*) '(expect equal? "*****")
(string-pad "325" 5 #\*) '(expect equal? "**325")
(string-pad "71325" 5 #\*) '(expect equal? "71325")
(string-pad "8871325" 5 #\*) '(expect equal? "71325")
(string-pad "" 0 #\* 0) '(expect equal? "")
(string-pad "" 5 #\* 0) '(expect equal? "*****")
(string-pad "325" 5 #\* 0) '(expect equal? "**325")
(string-pad "71325" 5 #\* 0) '(expect equal? "71325")
(string-pad "8871325" 5 #\* 0) '(expect equal? "71325")
(string-pad "325" 5 #\* 1) '(expect equal? "***25")
(string-pad "71325" 5 #\* 1) '(expect equal? "*1325")
(string-pad "8871325" 5 #\* 1) '(expect equal? "71325")
(string-pad "" 0 #\* 0 0) '(expect equal? "")
(string-pad "" 5 #\* 0 0) '(expect equal? "*****")
(string-pad "325" 5 #\* 0 3) '(expect equal? "**325")
(string-pad "71325" 5 #\* 0 3) '(expect equal? "**713")
(string-pad "8871325" 5 #\* 0 3) '(expect equal? "**887")
(string-pad "325" 5 #\* 1 3) '(expect equal? "***25")
(string-pad "71325" 5 #\* 1 4) '(expect equal? "**132")
(string-pad "8871325" 5 #\* 1 5) '(expect equal? "*8713")

(string-pad-right "" 0) '(expect equal? "")
(string-pad-right "" 5) '(expect equal? "     ")
(string-pad-right "325" 5) '(expect equal? "325  ")
(string-pad-right "71325" 5) '(expect equal? "71325")
(string-pad-right "8871325" 5) '(expect equal? "88713")
(string-pad-right "" 0 #\*) '(expect equal? "")
(string-pad-right "" 5 #\*) '(expect equal? "*****")
(string-pad-right "325" 5 #\*) '(expect equal? "325**")
(string-pad-right "71325" 5 #\*) '(expect equal? "71325")
(string-pad-right "8871325" 5 #\*) '(expect equal? "88713")
(string-pad-right "" 0 #\* 0) '(expect equal? "")
(string-pad-right "" 5 #\* 0) '(expect equal? "*****")
(string-pad-right "325" 5 #\* 0) '(expect equal? "325**")
(string-pad-right "71325" 5 #\* 0) '(expect equal? "71325")
(string-pad-right "8871325" 5 #\* 0) '(expect equal? "88713")
(string-pad-right "325" 5 #\* 1) '(expect equal? "25***")
(string-pad-right "71325" 5 #\* 1) '(expect equal? "1325*")
(string-pad-right "8871325" 5 #\* 1) '(expect equal? "87132")
(string-pad-right "" 0 #\* 0 0) '(expect equal? "")
(string-pad-right "" 5 #\* 0 0) '(expect equal? "*****")
(string-pad-right "325" 5 #\* 0 3) '(expect equal? "325**")
(string-pad-right "71325" 5 #\* 0 3) '(expect equal? "713**")
(string-pad-right "8871325" 5 #\* 0 3) '(expect equal? "887**")
(string-pad-right "325" 5 #\* 1 3) '(expect equal? "25***")
(string-pad-right "71325" 5 #\* 1 4) '(expect equal? "132**")
(string-pad-right "8871325" 5 #\* 1 5) '(expect equal? "8713*")

(string-pad "" 0) '(expect equal? "")
(string-pad "" 5) '(expect equal? "     ")
(string-pad "325" 5) '(expect equal? "  325")
(string-pad "71325" 5) '(expect equal? "71325")
(string-pad "8871325" 5) '(expect equal? "71325")
(string-pad "" 0 #\*) '(expect equal? "")
(string-pad "" 5 #\*) '(expect equal? "*****")
(string-pad "325" 5 #\*) '(expect equal? "**325")
(string-pad "71325" 5 #\*) '(expect equal? "71325")
(string-pad "8871325" 5 #\*) '(expect equal? "71325")
(string-pad "" 0 #\* 0) '(expect equal? "")
(string-pad "" 5 #\* 0) '(expect equal? "*****")
(string-pad "325" 5 #\* 0) '(expect equal? "**325")
(string-pad "71325" 5 #\* 0) '(expect equal? "71325")
(string-pad "8871325" 5 #\* 0) '(expect equal? "71325")
(string-pad "325" 5 #\* 1) '(expect equal? "***25")
(string-pad "71325" 5 #\* 1) '(expect equal? "*1325")
(string-pad "8871325" 5 #\* 1) '(expect equal? "71325")
(string-pad "" 0 #\* 0 0) '(expect equal? "")
(string-pad "" 5 #\* 0 0) '(expect equal? "*****")
(string-pad "325" 5 #\* 0 3) '(expect equal? "**325")
(string-pad "71325" 5 #\* 0 3) '(expect equal? "**713")
(string-pad "8871325" 5 #\* 0 3) '(expect equal? "**887")
(string-pad "325" 5 #\* 1 3) '(expect equal? "***25")
(string-pad "71325" 5 #\* 1 4) '(expect equal? "**132")
(string-pad "8871325" 5 #\* 1 5) '(expect equal? "*8713")

(string-pad-right "" 0) '(expect equal? "")
(string-pad-right "" 5) '(expect equal? "     ")
(string-pad-right "325" 5) '(expect equal? "325  ")
(string-pad-right "71325" 5) '(expect equal? "71325")
(string-pad-right "8871325" 5) '(expect equal? "88713")
(string-pad-right "" 0 #\*) '(expect equal? "")
(string-pad-right "" 5 #\*) '(expect equal? "*****")
(string-pad-right "325" 5 #\*) '(expect equal? "325**")
(string-pad-right "71325" 5 #\*) '(expect equal? "71325")
(string-pad-right "8871325" 5 #\*) '(expect equal? "88713")
(string-pad-right "" 0 #\* 0) '(expect equal? "")
(string-pad-right "" 5 #\* 0) '(expect equal? "*****")
(string-pad-right "325" 5 #\* 0) '(expect equal? "325**")
(string-pad-right "71325" 5 #\* 0) '(expect equal? "71325")
(string-pad-right "8871325" 5 #\* 0) '(expect equal? "88713")
(string-pad-right "325" 5 #\* 1) '(expect equal? "25***")
(string-pad-right "71325" 5 #\* 1) '(expect equal? "1325*")
(string-pad-right "8871325" 5 #\* 1) '(expect equal? "87132")
(string-pad-right "" 0 #\* 0 0) '(expect equal? "")
(string-pad-right "" 5 #\* 0 0) '(expect equal? "*****")
(string-pad-right "325" 5 #\* 0 3) '(expect equal? "325**")
(string-pad-right "71325" 5 #\* 0 3) '(expect equal? "713**")
(string-pad-right "8871325" 5 #\* 0 3) '(expect equal? "887**")
(string-pad-right "325" 5 #\* 1 3) '(expect equal? "25***")
(string-pad-right "71325" 5 #\* 1 4) '(expect equal? "132**")
(string-pad-right "8871325" 5 #\* 1 5) '(expect equal? "8713*")

(string-trim "") '(expect equal? "")
(string-trim "  a  b  c  ") '(expect equal? "a  b  c  ")
(string-trim "" char-whitespace?) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace?) '(expect equal? "a  b  c  ")
(string-trim "  a  b  c  " char?) '(expect equal? "")
(string-trim "" char-whitespace? 0) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 0) '(expect equal? "a  b  c  ")
(string-trim "  a  b  c  " char? 0) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3) '(expect equal? "b  c  ")
(string-trim "  a  b  c  " char? 3) '(expect equal? "")
(string-trim "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3 11) '(expect equal? "b  c  ")
(string-trim "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3 8) '(expect equal? "b  ")
(string-trim "  a  b  c  " char? 3 8) '(expect equal? "")

(string-trim-right "") '(expect equal? "")
(string-trim-right "  a  b  c  ") '(expect equal? "  a  b  c")
(string-trim-right "" char-whitespace?) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace?) '(expect equal? "  a  b  c")
(string-trim-right "  a  b  c  " char?) '(expect equal? "")
(string-trim-right "" char-whitespace? 0) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 0) '(expect equal? "  a  b  c")
(string-trim-right "  a  b  c  " char? 0) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3) '(expect equal? "  b  c")
(string-trim-right "  a  b  c  " char? 3) '(expect equal? "")
(string-trim-right "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3 11) '(expect equal? "  b  c")
(string-trim-right "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim-right "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3 8) '(expect equal? "  b")
(string-trim-right "  a  b  c  " char? 3 8) '(expect equal? "")

(string-trim-both "") '(expect equal? "")
(string-trim-both "  a  b  c  ") '(expect equal? "a  b  c")
(string-trim-both "" char-whitespace?) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace?) '(expect equal? "a  b  c")
(string-trim-both "  a  b  c  " char?) '(expect equal? "")
(string-trim-both "" char-whitespace? 0) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 0) '(expect equal? "a  b  c")
(string-trim-both "  a  b  c  " char? 0) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3) '(expect equal? "b  c")
(string-trim-both "  a  b  c  " char? 3) '(expect equal? "")
(string-trim-both "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3 11) '(expect equal? "b  c")
(string-trim-both "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim-both "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3 8) '(expect equal? "b")
(string-trim-both "  a  b  c  " char? 3 8) '(expect equal? "")

(string-trim "") '(expect equal? "")
(string-trim "  a  b  c  ") '(expect equal? "a  b  c  ")
(string-trim "" char-whitespace?) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace?) '(expect equal? "a  b  c  ")
(string-trim "  a  b  c  " char?) '(expect equal? "")
(string-trim "" char-whitespace? 0) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 0) '(expect equal? "a  b  c  ")
(string-trim "  a  b  c  " char? 0) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3) '(expect equal? "b  c  ")
(string-trim "  a  b  c  " char? 3) '(expect equal? "")
(string-trim "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3 11) '(expect equal? "b  c  ")
(string-trim "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim "  a  b  c  " char-whitespace? 3 8) '(expect equal? "b  ")
(string-trim "  a  b  c  " char? 3 8) '(expect equal? "")

(string-trim-right "") '(expect equal? "")
(string-trim-right "  a  b  c  ") '(expect equal? "  a  b  c")
(string-trim-right "" char-whitespace?) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace?) '(expect equal? "  a  b  c")
(string-trim-right "  a  b  c  " char?) '(expect equal? "")
(string-trim-right "" char-whitespace? 0) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 0) '(expect equal? "  a  b  c")
(string-trim-right "  a  b  c  " char? 0) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3) '(expect equal? "  b  c")
(string-trim-right "  a  b  c  " char? 3) '(expect equal? "")
(string-trim-right "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3 11) '(expect equal? "  b  c")
(string-trim-right "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim-right "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim-right "  a  b  c  " char-whitespace? 3 8) '(expect equal? "  b")
(string-trim-right "  a  b  c  " char? 3 8) '(expect equal? "")

(string-trim-both "") '(expect equal? "")
(string-trim-both "  a  b  c  ") '(expect equal? "a  b  c")
(string-trim-both "" char-whitespace?) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace?) '(expect equal? "a  b  c")
(string-trim-both "  a  b  c  " char?) '(expect equal? "")
(string-trim-both "" char-whitespace? 0) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 0) '(expect equal? "a  b  c")
(string-trim-both "  a  b  c  " char? 0) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3) '(expect equal? "b  c")
(string-trim-both "  a  b  c  " char? 3) '(expect equal? "")
(string-trim-both "  a  b  c  " char? 0 11) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3 11) '(expect equal? "b  c")
(string-trim-both "  a  b  c  " char? 3 11) '(expect equal? "")
(string-trim-both "  a  b  c  " char? 0 8) '(expect equal? "")
(string-trim-both "  a  b  c  " char-whitespace? 3 8) '(expect equal? "b")
(string-trim-both "  a  b  c  " char? 3 8) '(expect equal? "")

;;; Replacement

(string-replace "It's easy to code it up in Scheme." "lots of fun" 5 9)
'(expect equal? "It's lots of fun to code it up in Scheme.")

(string-replace "The TCL programmer endured daily ridicule."
                "another miserable perl drone"
                4 7 8 22)
'(expect equal? "The miserable perl programmer endured daily ridicule.")

(string-replace "It's easy to code it up in Scheme."
                "really "
                5 5)
'(expect equal? "It's really easy to code it up in Scheme.")

(string-replace "Runs in O(n) time." (string #\1) 10 11)
'(expect equal? "Runs in O(1) time.")

;;; Comparison
;;;
;;; The comparison tests aren't perfectly black-box because the
;;; specification of these comparison procedures allows them to
;;; use an ordering other than the usual lexicographic ordering.
;;; The sample implementations use lexicographic ordering, however,
;;; and a test program that discourages implementations from using
;;; orderings that differ from the usual on such simple cases is
;;; probably doing a public service.

(string=? "Strasse" "Strasse") 'expect-true
(string=? "Strasse" "Strasse" "Strasse") 'expect-true

(string<? "z" "z") 'expect-false
(string<? "z" "zz") 'expect-true
(string<? "z" "Z") 'expect-false
(string<=? "z" "zz") 'expect-true
(string<=? "z" "Z") 'expect-false
(string<=? "z" "z") 'expect-true

(string<? "z" "z") 'expect-false
(string>? "z" "zz") 'expect-false
(string>? "z" "Z") 'expect-true
(string>=? "z" "zz") 'expect-false
(string>=? "z" "Z") 'expect-true
(string>=? "z" "z") 'expect-true

(define s6w "a")
(define s6x "abc")
(define s6y "def")
(define s6z (string #\a #\b #\c))

(string=? s6x s6y s6z) 'expect-false
(string=? s6x s6x s6z) 'expect-true
(string=? s6w s6x s6y) 'expect-false
(string=? s6y s6x s6w) 'expect-false

(string<? s6x s6y s6z) 'expect-false
(string<? s6x s6x s6z) 'expect-false
(string<? s6w s6x s6y) 'expect-true
(string<? s6y s6x s6w) 'expect-false

(string>? s6x s6y s6z) 'expect-false
(string>? s6x s6x s6z) 'expect-false
(string>? s6w s6x s6y) 'expect-false
(string>? s6y s6x s6w) 'expect-true

(string<=? s6x s6y s6z) 'expect-false
(string<=? s6x s6x s6z) 'expect-true
(string<=? s6w s6x s6y) 'expect-true
(string<=? s6y s6x s6w) 'expect-false

(string>=? s6x s6y s6z) 'expect-false
(string>=? s6x s6x s6z) 'expect-true
(string>=? s6w s6x s6y) 'expect-false
(string>=? s6y s6x s6w) 'expect-true

(string=? s6x s6x) 'expect-true
(string=? s6w s6x) 'expect-false
(string=? s6y s6x) 'expect-false

(string<? s6x s6x) 'expect-false
(string<? s6w s6x) 'expect-true
(string<? s6y s6x) 'expect-false

(string>? s6x s6x) 'expect-false
(string>? s6w s6x) 'expect-false
(string>? s6y s6x) 'expect-true

(string<=? s6x s6x) 'expect-true
(string<=? s6w s6x) 'expect-true
(string<=? s6y s6x) 'expect-false

(string>=? s6x s6x) 'expect-true
(string>=? s6w s6x) 'expect-false
(string>=? s6y s6x) 'expect-true

(string-ci<? "a" "Z") 'expect-true
(string-ci<? "A" "z") 'expect-true
(string-ci<? "Z" "a") 'expect-false
(string-ci<? "z" "A") 'expect-false
(string-ci<? "z" "Z") 'expect-false
(string-ci<? "Z" "z") 'expect-false
(string-ci>? "a" "Z") 'expect-false
(string-ci>? "A" "z") 'expect-false
(string-ci>? "Z" "a") 'expect-true
(string-ci>? "z" "A") 'expect-true
(string-ci>? "z" "Z") 'expect-false
(string-ci>? "Z" "z") 'expect-false
(string-ci=? "z" "Z") 'expect-true
(string-ci=? "z" "a") 'expect-false
(string-ci<=? "a" "Z") 'expect-true
(string-ci<=? "A" "z") 'expect-true
(string-ci<=? "Z" "a") 'expect-false
(string-ci<=? "z" "A") 'expect-false
(string-ci<=? "z" "Z") 'expect-true
(string-ci<=? "Z" "z") 'expect-true
(string-ci>=? "a" "Z") 'expect-false
(string-ci>=? "A" "z") 'expect-false
(string-ci>=? "Z" "a") 'expect-true
(string-ci>=? "z" "A") 'expect-true
(string-ci>=? "z" "Z") 'expect-true
(string-ci>=? "Z" "z") 'expect-true

(string=? ABCDEF DEFABC) 'expect-false
(string=? DEFABC ABCDEF) 'expect-false
(string=? DEFABC DEFABC) 'expect-true

(string<? ABCDEF DEFABC) 'expect-false
(string<? DEFABC ABCDEF) 'expect-true
(string<? DEFABC DEFABC) 'expect-false

(string>? ABCDEF DEFABC) 'expect-true
(string>? DEFABC ABCDEF) 'expect-false
(string>? DEFABC DEFABC) 'expect-false

(string<=? ABCDEF DEFABC) 'expect-false
(string<=? DEFABC ABCDEF) 'expect-true
(string<=? DEFABC DEFABC) 'expect-true

(string>=? ABCDEF DEFABC) 'expect-true
(string>=? DEFABC ABCDEF) 'expect-false
(string>=? DEFABC DEFABC) 'expect-true

(string=? "Fuss" fuss) 'expect-false
(string=? "Fuss" "Fuss" fuss) 'expect-false
(string=? "Fuss" fuss "Fuss") 'expect-false
(string=? fuss "Fuss" "Fuss") 'expect-false
(string<? "z" (string eszett)) 'expect-true
(string<? (string eszett) "z") 'expect-false
(string<=? "z" (string eszett)) 'expect-true
(string<=? (string eszett) "z") 'expect-false
(string>? "z" (string eszett)) 'expect-false
(string>? (string eszett) "z") 'expect-true
(string>=? "z" (string eszett)) 'expect-false
(string>=? (string eszett) "z") 'expect-true

(string-ci=? fuss "Fuss") 'expect-true
(string-ci=? fuss "FUSS") 'expect-true
(string-ci=? chaos0 chaos1 chaos2) 'expect-true

;;; Prefixes and suffixes

(string-prefix-length ABC ABCDEF) '(expect eqv? 0)
(string-prefix-length ABCDEF ABC) '(expect eqv? 0)
(string-prefix-length ABCDEF DEFABC) '(expect eqv? 0)
(string-prefix-length DEFABC DEFABC) '(expect eqv? 6)
(string-prefix-length "" "") '(expect eqv? 0)
(string-prefix-length "" "aabbccddee") '(expect eqv? 0)
(string-prefix-length "aisle" "") '(expect eqv? 0)
(string-prefix-length "" "aabbccddee") '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee") '(expect eqv? 1)
(string-prefix-length "bail" "aabbccddee") '(expect eqv? 0)
(string-prefix-length "prefix" "preface") '(expect eqv? 4)
(string-prefix-length "" "" 0) '(expect eqv? 0)
(string-prefix-length "" "aabbccddee" 0) '(expect eqv? 0)
(string-prefix-length "aisle" "" 0) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 0) '(expect eqv? 1)
(string-prefix-length "bail" "aabbccddee" 0) '(expect eqv? 0)
(string-prefix-length "prefix" "preface" 0) '(expect eqv? 4)
(string-prefix-length "aisle" "" 1) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 1) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 1) '(expect eqv? 1)
(string-prefix-length "prefix" "preface" 1) '(expect eqv? 0)
(string-prefix-length "" "" 0 0) '(expect eqv? 0)
(string-prefix-length "" "aabbccddee" 0 0) '(expect eqv? 0)
(string-prefix-length "aisle" "" 0 4) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 0 4) '(expect eqv? 1)
(string-prefix-length "bail" "aabbccddee" 0 1) '(expect eqv? 0)
(string-prefix-length "aisle" "" 1 4) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 1 4) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 1 4) '(expect eqv? 1)
(string-prefix-length "prefix" "preface" 1 5) '(expect eqv? 0)
(string-prefix-length "" "" 0 0 0) '(expect eqv? 0)
(string-prefix-length "" "aabbccddee" 0 0 0) '(expect eqv? 0)
(string-prefix-length "aisle" "" 0 4 0) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 0 4 2) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 0 1 2) '(expect eqv? 1)
(string-prefix-length "prefix" "preface" 0 5 1) '(expect eqv? 0)
(string-prefix-length "aisle" "" 1 4 0) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 1 4 3) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 1 4 3) '(expect eqv? 0)
(string-prefix-length "prefix" "preface" 1 5 1) '(expect eqv? 3)
(string-prefix-length "" "" 0 0 0 0) '(expect eqv? 0)
(string-prefix-length "" "aabbccddee" 0 0 0 0) '(expect eqv? 0)
(string-prefix-length "aisle" "" 0 4 0 0) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 0 4 2 10) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 0 1 2 10) '(expect eqv? 1)
(string-prefix-length "prefix" "preface" 0 5 1 6) '(expect eqv? 0)
(string-prefix-length "aisle" "" 1 4 0 0) '(expect eqv? 0)
(string-prefix-length "aisle" "aabbccddee" 1 4 3 3) '(expect eqv? 0)
(string-prefix-length "bail" "aabbccddee" 1 4 3 6) '(expect eqv? 0)
(string-prefix-length "prefix" "preface" 1 5 1 7) '(expect eqv? 3)

(string-suffix-length ABC ABCDEF) '(expect eqv? 0)
(string-suffix-length ABCDEF ABC) '(expect eqv? 0)
(string-suffix-length ABCDEF DEFABC) '(expect eqv? 0)
(string-suffix-length DEFABC DEFABC) '(expect eqv? 6)
(string-suffix-length "" "") '(expect eqv? 0)
(string-suffix-length "" "aabbccddee") '(expect eqv? 0)
(string-suffix-length "aisle" "") '(expect eqv? 0)
(string-suffix-length "" "aabbccddee") '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee") '(expect eqv? 1)
(string-suffix-length "bail" "aabbccddee") '(expect eqv? 0)
(string-suffix-length "place" "preface") '(expect eqv? 3)
(string-suffix-length "" "" 0) '(expect eqv? 0)
(string-suffix-length "" "aabbccddee" 0) '(expect eqv? 0)
(string-suffix-length "aisle" "" 0) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 0) '(expect eqv? 1)
(string-suffix-length "bail" "aabbccddee" 0) '(expect eqv? 0)
(string-suffix-length "place" "preface" 0) '(expect eqv? 3)
(string-suffix-length "aisle" "" 1) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 1) '(expect eqv? 1)
(string-suffix-length "bail" "aabbccddee" 1) '(expect eqv? 0)
(string-suffix-length "place" "preface" 1) '(expect eqv? 3)
(string-suffix-length "" "" 0 0) '(expect eqv? 0)
(string-suffix-length "" "aabbccddee" 0 0) '(expect eqv? 0)
(string-suffix-length "aisle" "" 0 4) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 0 4) '(expect eqv? 0)
(string-suffix-length "bail" "aabbccddee" 0 1) '(expect eqv? 0)
(string-suffix-length "aisle" "" 1 4) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 1 4) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 1 5) '(expect eqv? 1)
(string-suffix-length "bail" "aabbccddee" 1 4) '(expect eqv? 0)
(string-suffix-length "place" "preface" 1 5) '(expect eqv? 3)
(string-suffix-length "" "" 0 0 0) '(expect eqv? 0)
(string-suffix-length "" "aabbccddee" 0 0 0) '(expect eqv? 0)
(string-suffix-length "aisle" "" 0 4 0) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 0 4 2) '(expect eqv? 0)
(string-suffix-length "bail" "aabbccddee" 0 1 2) '(expect eqv? 0)
(string-suffix-length "place" "preface" 0 5 1) '(expect eqv? 3)
(string-suffix-length "aisle" "" 1 4 0) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 1 4 3) '(expect eqv? 0)
(string-suffix-length "bail" "aabbccddee" 1 4 3) '(expect eqv? 0)
(string-suffix-length "place" "preface" 1 5 1) '(expect eqv? 3)
(string-suffix-length "" "" 0 0 0 0) '(expect eqv? 0)
(string-suffix-length "" "aabbccddee" 0 0 0 0) '(expect eqv? 0)
(string-suffix-length "aisle" "" 0 4 0 0) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 0 5 2 10) '(expect eqv? 1)
(string-suffix-length "bail" "aabbccddee" 0 1 2 4) '(expect eqv? 1)
(string-suffix-length "place" "preface" 0 5 1 6) '(expect eqv? 0)
(string-suffix-length "place" "preface" 0 4 1 6) '(expect eqv? 2)
(string-suffix-length "aisle" "" 1 4 0 0) '(expect eqv? 0)
(string-suffix-length "aisle" "aabbccddee" 1 4 3 3) '(expect eqv? 0)
(string-suffix-length "bail" "aabbccddee" 1 4 3 6) '(expect eqv? 0)
(string-suffix-length "place" "preface" 1 5 1 7) '(expect eqv? 3)

(string-prefix? ABC ABCDEF) 'expect-false
(string-prefix? ABCDEF ABC) 'expect-false
(string-prefix? ABCDEF DEFABC) 'expect-false
(string-prefix? DEFABC DEFABC) 'expect-true
(string-prefix? "" "") 'expect-true
(string-prefix? "" "abc") 'expect-true
(string-prefix? "a" "abc") 'expect-true
(string-prefix? "c" "abc") 'expect-false
(string-prefix? "ab" "abc") 'expect-true
(string-prefix? "ac" "abc") 'expect-false
(string-prefix? "abc" "abc") 'expect-true
(string-suffix? ABC ABCDEF) 'expect-false
(string-suffix? ABCDEF ABC) 'expect-false
(string-suffix? ABCDEF DEFABC) 'expect-false
(string-suffix? DEFABC DEFABC) 'expect-true
(string-suffix? "" "") 'expect-true
(string-suffix? "" "abc") 'expect-true
(string-suffix? "a" "abc") 'expect-false
(string-suffix? "c" "abc") 'expect-true
(string-suffix? "ac" "abc") 'expect-false
(string-suffix? "bc" "abc") 'expect-true
(string-suffix? "abc" "abc") 'expect-true
(string-prefix? "" "" 0) 'expect-true
(string-prefix? "" "abc" 0) 'expect-true
(string-prefix? "a" "abc" 0) 'expect-true
(string-prefix? "c" "abc" 0) 'expect-false
(string-prefix? "ab" "abc" 0) 'expect-true
(string-prefix? "ac" "abc" 0) 'expect-false
(string-prefix? "abc" "abc" 0) 'expect-true
(string-suffix? "" "" 0) 'expect-true
(string-suffix? "" "abc" 0) 'expect-true
(string-suffix? "a" "abc" 0) 'expect-false
(string-suffix? "c" "abc" 0) 'expect-true
(string-suffix? "ac" "abc" 0) 'expect-false
(string-suffix? "bc" "abc" 0) 'expect-true
(string-suffix? "abc" "abc" 0) 'expect-true
(string-prefix? "ab" "abc" 2) 'expect-true
(string-prefix? "ac" "abc" 2) 'expect-true
(string-prefix? "abc" "abc" 2) 'expect-false
(string-suffix? "ac" "abc" 2) 'expect-true
(string-suffix? "bc" "abc" 2) 'expect-true
(string-suffix? "abc" "abc" 2) 'expect-true

(string-prefix? "" "" 0 0) 'expect-true
(string-prefix? "" "abc" 0 0) 'expect-true
(string-prefix? "a" "abc" 0 0) 'expect-true
(string-prefix? "c" "abc" 0 1) 'expect-false
(string-prefix? "ab" "abc" 0 1) 'expect-true
(string-prefix? "ab" "abc" 0 2) 'expect-true
(string-prefix? "ac" "abc" 0 2) 'expect-false
(string-prefix? "abc" "abc" 0 3) 'expect-true
(string-suffix? "" "" 0 0) 'expect-true
(string-suffix? "" "abc" 0 0) 'expect-true
(string-suffix? "a" "abc" 0 1) 'expect-false
(string-suffix? "c" "abc" 0 1) 'expect-true
(string-suffix? "ac" "abc" 1 2) 'expect-true
(string-suffix? "ac" "abc" 0 2) 'expect-false
(string-suffix? "bc" "abc" 0 2) 'expect-true
(string-suffix? "abc" "abc" 0 3) 'expect-true
(string-prefix? "ab" "abc" 2 2) 'expect-true
(string-prefix? "ac" "abc" 2 2) 'expect-true
(string-prefix? "abc" "abc" 2 3) 'expect-false
(string-suffix? "ac" "abc" 2 2) 'expect-true
(string-suffix? "bc" "abc" 2 2) 'expect-true
(string-suffix? "abc" "abc" 2 3) 'expect-true

(string-prefix? "" "" 0 0 0) 'expect-true
(string-prefix? "" "abc" 0 0 0) 'expect-true
(string-prefix? "a" "abc" 0 0 0) 'expect-true
(string-prefix? "c" "abc" 0 1 0) 'expect-false
(string-prefix? "ab" "abc" 0 1 0) 'expect-true
(string-prefix? "ab" "abc" 0 2 0) 'expect-true
(string-prefix? "ac" "abc" 0 2 0) 'expect-false
(string-prefix? "abc" "abc" 0 3 0) 'expect-true
(string-suffix? "" "" 0 0 0) 'expect-true
(string-suffix? "" "abc" 0 0 0) 'expect-true
(string-suffix? "a" "abc" 0 1 0) 'expect-false
(string-suffix? "c" "abc" 0 1 0) 'expect-true
(string-suffix? "ac" "abc" 1 2 0) 'expect-true
(string-suffix? "ac" "abc" 0 2 0) 'expect-false
(string-suffix? "bc" "abc" 0 2 0) 'expect-true
(string-suffix? "abc" "abc" 0 3 0) 'expect-true
(string-prefix? "ab" "abc" 2 2 0) 'expect-true
(string-prefix? "ac" "abc" 2 2 0) 'expect-true
(string-prefix? "abc" "abc" 2 3 0) 'expect-false
(string-suffix? "ac" "abc" 2 2 0) 'expect-true
(string-suffix? "bc" "abc" 2 2 0) 'expect-true
(string-suffix? "abc" "abc" 2 3 0) 'expect-true
(string-prefix? "" "abc" 0 0 1) 'expect-true
(string-prefix? "a" "abc" 0 0 1) 'expect-true
(string-prefix? "c" "abc" 0 1 2) 'expect-true
(string-prefix? "ab" "abc" 0 1 2) 'expect-false
(string-prefix? "ab" "abc" 0 2 1) 'expect-false
(string-prefix? "ac" "abc" 0 2 1) 'expect-false
(string-prefix? "abc" "abc" 0 3 1) 'expect-false
(string-suffix? "a" "abc" 0 1 2) 'expect-false
(string-suffix? "c" "abc" 0 1 1) 'expect-true
(string-suffix? "ac" "abc" 1 2 2) 'expect-true
(string-suffix? "bc" "abc" 0 2 1) 'expect-true
(string-suffix? "bc" "abc" 0 2 2) 'expect-false

(string-prefix? "" "" 0 0 0 0) 'expect-true
(string-prefix? "" "abc" 0 0 0 3) 'expect-true
(string-prefix? "a" "abc" 0 0 0 3) 'expect-true
(string-prefix? "c" "abc" 0 1 0 3) 'expect-false
(string-prefix? "ab" "abc" 0 1 0 3) 'expect-true
(string-prefix? "ab" "abc" 0 2 0 3) 'expect-true
(string-prefix? "ac" "abc" 0 2 0 3) 'expect-false
(string-prefix? "abc" "abc" 0 3 0 3) 'expect-true
(string-suffix? "" "abc" 0 0 0 3) 'expect-true
(string-suffix? "a" "abc" 0 1 0 3) 'expect-false
(string-suffix? "c" "abc" 0 1 0 3) 'expect-true
(string-suffix? "ac" "abc" 1 2 0 3) 'expect-true
(string-suffix? "ac" "abc" 0 2 0 3) 'expect-false
(string-suffix? "bc" "abc" 0 2 0 3) 'expect-true
(string-suffix? "abc" "abc" 0 3 0 3) 'expect-true
(string-prefix? "ab" "abc" 2 2 0 3) 'expect-true
(string-prefix? "ac" "abc" 2 2 0 3) 'expect-true
(string-prefix? "abc" "abc" 2 3 0 3) 'expect-false
(string-suffix? "ac" "abc" 2 2 0 3) 'expect-true
(string-suffix? "bc" "abc" 2 2 0 3) 'expect-true
(string-suffix? "abc" "abc" 2 3 0 3) 'expect-true
(string-prefix? "" "abc" 0 0 1 3) 'expect-true
(string-prefix? "a" "abc" 0 0 1 3) 'expect-true
(string-prefix? "c" "abc" 0 1 2 3) 'expect-true
(string-prefix? "ab" "abc" 0 1 2 3) 'expect-false
(string-prefix? "ab" "abc" 0 2 1 3) 'expect-false
(string-prefix? "ac" "abc" 0 2 1 3) 'expect-false
(string-prefix? "abc" "abc" 0 3 1 3) 'expect-false
(string-suffix? "a" "abc" 0 1 2 3) 'expect-false
(string-suffix? "c" "abc" 0 1 1 3) 'expect-true
(string-suffix? "ac" "abc" 1 2 2 3) 'expect-true
(string-suffix? "bc" "abc" 0 2 1 3) 'expect-true
(string-suffix? "bc" "abc" 0 2 2 3) 'expect-false

(string-prefix? "" "abc" 0 0 0 2) 'expect-true
(string-prefix? "a" "abc" 0 0 0 2) 'expect-true
(string-prefix? "c" "abc" 0 1 0 2) 'expect-false
(string-prefix? "ab" "abc" 0 1 0 2) 'expect-true
(string-prefix? "abc" "abc" 0 3 0 2) 'expect-false
(string-suffix? "" "abc" 0 0 0 2) 'expect-true
(string-suffix? "c" "abc" 0 1 0 2) 'expect-false
(string-suffix? "ac" "abc" 1 2 0 2) 'expect-false

;;; Searching

(string-index "" char?) 'expect-false
(string-index "abcdef" char?) '(expect eqv? 0)
(string-index "abcdef" (lambda (c) (char>? c #\d))) '(expect eqv? 4)
(string-index "abcdef" char-whitespace?) 'expect-false
(string-index-right "" char?) 'expect-false
(string-index-right "abcdef" char?) '(expect eqv? 5)
(string-index-right "abcdef" (lambda (c) (char>? c #\d))) '(expect eqv? 5)

(string-index-right "abcdef" char-whitespace?) 'expect-false
(string-skip "" string?) 'expect-false
(string-skip "abcdef" string?) '(expect eqv? 0)
(string-skip "abcdef" (lambda (c) (char<=? c #\d))) '(expect eqv? 4)
(string-skip "abcdef" char?) 'expect-false
(string-skip-right "" string?) 'expect-false
(string-skip-right "abcdef" string?) '(expect eqv? 5)
(string-skip-right "abcdef" (lambda (c) (char<=? c #\d))) '(expect eqv? 5)
(string-skip-right "abcdef" char?) 'expect-false

(string-index "abcdef" char? 2) '(expect eqv? 2)
(string-index "abcdef" (lambda (c) (char>? c #\d)) 2) '(expect eqv? 4)
(string-index "abcdef" char-whitespace? 2) 'expect-false
(string-index-right "abcdef" char? 2) '(expect eqv? 5)
(string-index-right "abcdef" (lambda (c) (char>? c #\d)) 2) '(expect eqv? 5)
(string-index-right "abcdef" char-whitespace? 2) 'expect-false
(string-skip "abcdef" string? 2) '(expect eqv? 2)
(string-skip "abcdef" (lambda (c) (char<=? c #\d)) 2) '(expect eqv? 4)
(string-skip "abcdef" char? 2) 'expect-false
(string-skip-right "abcdef" string? 2) '(expect eqv? 5)
(string-skip-right "abcdef" (lambda (c) (char<=? c #\d)) 2) '(expect eqv? 5)
(string-skip-right "abcdef" char? 2) 'expect-false

(string-index "abcdef" char? 2 5) '(expect eqv? 2)
(string-index "abcdef" (lambda (c) (char>? c #\d)) 2 5) '(expect eqv? 4)
(string-index "abcdef" char-whitespace? 2 5) 'expect-false
(string-index-right "abcdef" char? 2 5) '(expect eqv? 4)
(string-index-right "abcdef" (lambda (c) (char>? c #\d)) 2 5) '(expect eqv? 4)
(string-index-right "abcdef" char-whitespace? 2 5) 'expect-false

(string-skip "abcdef" string? 2 5) '(expect eqv? 2)
(string-skip "abcdef" (lambda (c) (char<=? c #\d)) 2 5) '(expect eqv? 4)
(string-skip "abcdef" char? 2 5) 'expect-false
(string-skip-right "abcdef" string? 2 5) '(expect eqv? 4)
(string-skip-right "abcdef" (lambda (c) (char<=? c #\d)) 2 5) '(expect eqv? 4)
(string-skip-right "abcdef" char? 2 5) 'expect-false

(string-contains "" "") '(expect eqv? 0)
(string-contains "abcdeffffoo" "") '(expect eqv? 0)
(string-contains "abcdeffffoo" "a") '(expect eqv? 0)
(string-contains "abcdeffffoo" "ff") '(expect eqv? 5)
(string-contains "abcdeffffoo" "eff") '(expect eqv? 4)
(string-contains "abcdeffffoo" "foo") '(expect eqv? 8)
(string-contains "abcdeffffoo" "efffoo") 'expect-false
(string-contains-right "" "") '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "") '(expect eqv? 11)
(string-contains-right "abcdeffffoo" "a") '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "ff") '(expect eqv? 7)
(string-contains-right "abcdeffffoo" "eff") '(expect eqv? 4)
(string-contains-right "abcdeffffoo" "foo") '(expect eqv? 8)
(string-contains-right "abcdeffffoo" "efffoo") 'expect-false

(string-contains "" "" 0) '(expect eqv? 0)
(string-contains "abcdeffffoo" "" 2) '(expect eqv? 2)
(string-contains "abcdeffffoo" "a" 2) 'expect-false
(string-contains "abcdeffffoo" "ff" 2) '(expect eqv? 5)
(string-contains "abcdeffffoo" "eff" 2) '(expect eqv? 4)
(string-contains "abcdeffffoo" "foo" 2) '(expect eqv? 8)
(string-contains "abcdeffffoo" "efffoo" 2) 'expect-false
(string-contains-right "" "" 0) '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "" 2) '(expect eqv? 11)
(string-contains-right "abcdeffffoo" "a" 2) 'expect-false
(string-contains-right "abcdeffffoo" "ff" 2) '(expect eqv? 7)
(string-contains-right "abcdeffffoo" "eff" 2) '(expect eqv? 4)
(string-contains-right "abcdeffffoo" "foo" 2) '(expect eqv? 8)
(string-contains-right "abcdeffffoo" "efffoo" 2) 'expect-false

(string-contains "" "" 0 0) '(expect eqv? 0)
(string-contains "abcdeffffoo" "" 2 10) '(expect eqv? 2)
(string-contains "abcdeffffoo" "a" 2 10) 'expect-false
(string-contains "abcdeffffoo" "ff" 2 10) '(expect eqv? 5)
(string-contains "abcdeffffoo" "eff" 2 10) '(expect eqv? 4)
(string-contains "abcdeffffoo" "foo" 2 10) 'expect-false
(string-contains "abcdeffffoo" "efffoo" 2 10) 'expect-false
(string-contains-right "" "" 0 0) '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "" 2 10) '(expect eqv? 10)
(string-contains-right "abcdeffffoo" "a" 2 10) 'expect-false
(string-contains-right "abcdeffffoo" "ff" 2 10) '(expect eqv? 7)
(string-contains-right "abcdeffffoo" "eff" 2 10) '(expect eqv? 4)
(string-contains-right "abcdeffffoo" "foo" 2 10) 'expect-false
(string-contains-right "abcdeffffoo" "efffoo" 2 10) 'expect-false

(string-contains "" "" 0 0 0) '(expect eqv? 0)
(string-contains "abcdeffffoo" "" 2 10 0) '(expect eqv? 2)
(string-contains "abcdeffffoo" "a" 2 10 1) '(expect eqv? 2)
(string-contains "abcdeffffoo" "ff" 2 10 1) '(expect eqv? 5)
(string-contains "abcdeffffoo" "eff" 2 10 1) '(expect eqv? 5)
(string-contains "abcdeffffoo" "foo" 2 10 1) 'expect-false
(string-contains "abcdeffffoo" "efffoo" 2 10 1) 'expect-false
(string-contains-right "" "" 0 0 0) '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "" 2 10 0) '(expect eqv? 10)
(string-contains-right "abcdeffffoo" "a" 2 10 1) '(expect eqv? 10)
(string-contains-right "abcdeffffoo" "ff" 2 10 1) '(expect eqv? 8)
(string-contains-right "abcdeffffoo" "eff" 2 10 1) '(expect eqv? 7)
(string-contains-right "abcdeffffoo" "foo" 2 10 1) 'expect-false
(string-contains-right "abcdeffffoo" "efffoo" 2 10 1) 'expect-false

(string-contains "" "" 0 0 0 0) '(expect eqv? 0)
(string-contains "abcdeffffoo" "" 2 10 0 0) '(expect eqv? 2)
(string-contains "abcdeffffoo" "a" 2 10 1 1) '(expect eqv? 2)
(string-contains "abcdeffffoo" "ff" 2 10 1 2) '(expect eqv? 5)
(string-contains "abcdeffffoo" "eff" 2 10 1 2) '(expect eqv? 5)
(string-contains "abcdeffffoo" "foo" 2 10 1 2) '(expect eqv? 9)
(string-contains "abcdeffffoo" "efffoo" 2 10 0 2) '(expect eqv? 4)
(string-contains-right "" "" 0 0 0 0) '(expect eqv? 0)
(string-contains-right "abcdeffffoo" "" 2 10 0 0) '(expect eqv? 10)
(string-contains-right "abcdeffffoo" "a" 2 10 1 1) '(expect eqv? 10)
(string-contains-right "abcdeffffoo" "ff" 2 10 1 2) '(expect eqv? 8)
(string-contains-right "abcdeffffoo" "eff" 2 10 1 2) '(expect eqv? 8)
(string-contains-right "abcdeffffoo" "foo" 2 10 1 2) '(expect eqv? 9)
(string-contains-right "abcdeffffoo" "efffoo" 2 10 1 3) '(expect eqv? 7)

;;; Case conversion

;;; FIXME: should test some non-ASCII cases here.

(string-upcase "1234Strikes") '(expect equal? "1234STRIKES")
(string-upcase "1234strikes") '(expect equal? "1234STRIKES")
(string-upcase "1234STRIKES") '(expect equal? "1234STRIKES")
(string-downcase "1234Strikes") '(expect equal? "1234strikes")
(string-downcase "1234strikes") '(expect equal? "1234strikes")
(string-downcase "1234STRIKES") '(expect equal? "1234strikes")
(string-foldcase "1234Strikes") '(expect equal? "1234strikes")
(string-foldcase "1234strikes") '(expect equal? "1234strikes")
(string-foldcase "1234STRIKES") '(expect equal? "1234strikes")
(string-titlecase "and with THREE STRIKES you are oUT")
 '(expect equal? "And With Three Strikes You Are Out")

;;; Concatenation

(string-append) '(expect equal? "")
(string-append "" "a" "bcd" "" "ef" "" "") '(expect equal? "abcdef")
(string-concatenate '()) '(expect equal? "")
(string-concatenate '("" "a" "bcd" "" "ef" "" "")) '(expect equal? "abcdef")

;;; string-concatenate is likely to have special cases for longer strings.

(define alphabet "abcdefghijklmnopqrstuvwxyz")
(define str1 alphabet)
(define str10 (apply string-append (vector->list (make-vector 10 str1))))
(define str100 (apply string-append (vector->list (make-vector 10 str10))))
(define str100-500 (substring str100 100 500))
(define str600-999 (substring str100 600 999))
(define alph1 (string-copy alphabet))
(define alph10 (string-concatenate (vector->list (make-vector 10 alph1))))
(define alph100 (string-concatenate (vector->list (make-vector 10 alph10))))
(define t100-500 (substring alph100 100 500))
(define t600-999 (substring alph100 600 999))

alph10 '(expect equal? str10)
alph100 '(expect equal? str100)
t100-500 '(expect equal? str100-500)
t600-999 '(expect equal? str600-999)

;; concatenating a short string with a long string

(string-concatenate (list alph1 t600-999))
'(expect equal? (string-append str1 str600-999))

(string-concatenate (list alph1 (string-copy t600-999)))
'(expect equal? (string-append str1 str600-999))

(string-concatenate (list t600-999 alph1))
'(expect equal? (string-append str600-999 str1))

(string-concatenate (list (string-copy t600-999) alph1))
'(expect equal? (string-append str600-999 str1))

(string-concatenate-reverse '()) '(expect equal? "")
(string-concatenate-reverse '("" "a" "bcd" "" "ef" "" ""))
 '(expect equal? "efbcda")
(string-concatenate-reverse '() "huh?") '(expect equal? "huh?")
(string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy")
 '(expect equal? "efbcdaxy")
(string-concatenate-reverse '() "huh?" 3) '(expect equal? "huh")
(string-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1)
 '(expect equal? "efbcdax")

(string-join '()) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "")) '(expect equal? " ab cd  e f ")
(string-join '() "") '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "") '(expect equal? "abcdef")
(string-join '() "xyz") '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "xyz")
 '(expect equal? "xyzabxyzcdxyzxyzexyzfxyz")
(string-join '() "" 'infix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "" 'infix) '(expect equal? "abcdef")
(string-join '() "xyz" 'infix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix)
'(expect equal? "xyzabxyzcdxyzxyzexyzfxyz")
(string-join '("foo" "bar" "baz")) '(expect equal? "foo bar baz")
(string-join '("foo" "bar" "baz") "") '(expect equal? "foobarbaz")
(string-join '("foo" "bar" "baz") ":") '(expect equal? "foo:bar:baz")
(string-join '("foo" "bar" "baz") ":" 'suffix) '(expect equal? "foo:bar:baz:")
(string-join '() ":") '(expect equal? "")
(string-join '("") ":") '(expect equal? "")
(string-join '()  ":" 'infix) '(expect equal? "")
(string-join '()  ":" 'strict-infix) 'expect-error
(string-join '("A")  ":" 'strict-infix) '(expect equal? "A")
(string-join '("A" "B")  ":" 'strict-infix) '(expect equal? "A:B")
(string-join '()  ":" 'suffix) '(expect equal? "")
(string-join '("") ":" 'suffix) '(expect equal? ":")
(string-join '() "" 'strict-infix) 'expect-error
(string-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix)
'(expect equal? "abcdef")
(string-join '() "xyz" 'strict-infix) 'expect-error
(string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix)
'(expect equal? "xyzabxyzcdxyzxyzexyzfxyz")
(string-join '() "" 'suffix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix) '(expect equal? "abcdef")
(string-join '() "xyz" 'suffix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix)
'(expect equal? "xyzabxyzcdxyzxyzexyzfxyzxyz")
(string-join '() "" 'prefix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix) '(expect equal? "abcdef")
(string-join '() "xyz" 'prefix) '(expect equal? "")
(string-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix)
'(expect equal? "xyzxyzabxyzcdxyzxyzexyzfxyz")

;;; Fold & map & friends

(string-fold (lambda (c count)
               (if (char-whitespace? c)
                   (+ count 1)
                   count))
             0
             " ...a couple of spaces in this one... ")
'(expect eqv? 8)
(string-fold (lambda (c count)
                       (if (char-whitespace? c)
                           (+ count 1)
                           count))
                     0
                     " ...a couple of spaces in this one... "
                     1)
'(expect eqv? 7)
(string-fold (lambda (c count)
               (if (char-whitespace? c)
                   (+ count 1)
                   count))
             0
             " ...a couple of spaces in this one... "
             1
             32)
'(expect eqv? 6)
(string-fold-right cons '() "abcdef") '(expect equal? (string->list "abcdef"))
(string-fold-right cons '() "abcdef" 3) '(expect equal? (string->list "def"))
(string-fold-right cons '() "abcdef" 2 5) '(expect equal? (string->list "cde"))
(let* ((s "abracadabra")
       (ans-len (string-fold (lambda (c sum)
                               (+ sum (if (char=? c #\a) 2 1)))
                             0 s))
       (ans (make-string ans-len)))
  (string-fold (lambda (c i)
                 (let ((i (if (char=? c #\a)
                              (begin (string-set! ans i #\a)
                                     (+ i 1))
                              i)))
                   (string-set! ans i c)
                   (+ i 1)))
               0 s)
  ans)
'(expect equal? "aabraacaadaabraa")

(string-map string "abc") '(expect equal? "abc")
(string-map char-upcase "abc") '(expect equal? "ABC")

(string-map (lambda (c0 c1 c2)
              (case c0
                ((#\1) c1)
                ((#\2) (string c2))
                ((#\-) (string #\- c1))))
            "1222-1111-2222"
            "Hi There!"
            "Dear John")
'(expect equal? "Hear-here!")

(let ((q (open-output-string)))
  (string-for-each (lambda (c) (write-char c q))
                   "abc")
  (get-output-string q))
'(expect equal? "abc")

(let ((x '()))
  (string-for-each (lambda (c1 c2 c3)
                     (set! x (cons (string c1 c2 c3) x)))
                   "abc"
                   "defxyz"
                   "ghijklmnopqrstuvwxyz")
  x)
'(expect equal? '("cfi" "beh" "adg"))

(string-map-index (lambda (i)
                    (integer->char (+ i (char->integer #\a))))
		  "xyz")
'(expect equal? "abc")

(define s7
  (string-map-index (lambda (i)
                      (integer->char (+ i (char->integer #\a))))
                    "xyz***" 3))
(check-istring s7) '(expect equal? '(#t 3))
s7 '(expect equal? "def")

(string-map-index (lambda (i)
                    (integer->char (+ i (char->integer #\a))))
                  "......" 2 5)
'(expect equal? "cde")

(let ((s "abcde")
      (v '()))
  (string-for-each-index
   (lambda (i)
     (set! v (cons (char->integer (string-ref s i)) v)))
   s)
  v)
'(expect equal? '(101 100 99 98 97))

(let ((s "abcde")
      (v '()))
  (string-for-each-index
   (lambda (i)
     (set! v (cons (char->integer (string-ref s i)) v)))
   s 2)
  v)
'(expect equal? '(101 100 99))

(let ((s "abcde")
      (v '()))
  (string-for-each-index
   (lambda (i)
     (set! v (cons (char->integer (string-ref s i)) v)))
   s 1 3)
  v)
'(expect equal? '(99 98))

(string-count "abcdef" char?) '(expect eqv? 6)
(string-count "counting  whitespace, again " char-whitespace? 5)
'(expect eqv? 4)
(string-count "abcdefwxyz" (lambda (c) (odd? (char->integer c))) 2 8)
'(expect eqv? 3)

(define s8
  (string-filter (lambda (c) (memv c (string->list "aeiou")))
                 "What is number, that man may know it?"))
s8 '(expect equal? "aiueaaaoi")
(check-istring s8) '(expect equal? '(#t 9))

(define s9
  (string-remove (lambda (c) (memv c (string->list "aeiou")))
                 "And woman, that she may know number?"))
s9 '(expect equal? "And wmn, tht sh my knw nmbr?")
(check-istring s9) '(expect equal? '(#t 28))

(string-filter (lambda (c) (memv c (string->list "aeiou")))
	       "What is number, that man may know it?" 4)
'(expect equal? "iueaaaoi")
(string-remove (lambda (c) (memv c (string->list "aeiou")))
	       "And woman, that she may know number?" 6)
'(expect equal? "mn, tht sh my knw nmbr?")
(string-filter (lambda (c) (memv c (string->list "aeiou")))
	       "What is number, that man may know it?" 16 32)
'(expect equal? "aaao")
(string-remove (lambda (c) (memv c (string->list "eiu")))
	       "And woman, that she may know number?" 0 28)
'(expect equal? "And woman, that sh may know")

#|
(string-reverse "") '(expect equal? "")
(string-reverse "abcdef") '(expect equal?  "fedcba")
(string-reverse "" 0) '(expect equal? "")
(string-reverse "abcdef" 0) '(expect equal? "fedcba")
(string-reverse "abcdef" 2) '(expect equal? "fedc")
(string-reverse "" 0 0) '(expect equal? "")
(string-reverse "abcdef" 0 6) '(expect equal? "fedcba")
(string-reverse "abcdef" 2 5) '(expect equal? "edc")
|#

;;; Replication and splitting

(string-repeat #\X 0) '(expect equal? "")
(string-repeat #\X 3) '(expect equal? "XXX")
(string-repeat "abc" 0) '(expect equal? "")
(string-repeat "abc" 3) '(expect equal? "abcabcabc")

(xsubstring "abcdef" -4 10) '(expect equal? "cdefabcdefabcd")
(xsubstring "abcdef" 90 103 1) '(expect equal? "bcdefbcdefbcd")
(xsubstring "abcdef" -13 -3 2 5) '(expect equal? "ecdecdecde")
(xsubstring "abcdef" 2 8) '(expect equal? "cdefab")
(xsubstring "abcdef" -2 4) '(expect equal? "efabcd")
(xsubstring "abc" 0 7) '(expect equal? "abcabca")

(string-split "" "")
'(expect equal? '())
(string-split "abc" "")
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " ")
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***")
'(expect equal? '("" "there" "ya" "go" ""))
(string-split "" "" 'infix)
'(expect equal? '())
(string-split "abc" "" 'infix)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'infix)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'infix)
'(expect equal? '("" "there" "ya" "go" ""))
(string-split "" "" 'strict-infix) 'expect-error
(string-split "abc" "" 'strict-infix)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'strict-infix)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'strict-infix)
'(expect equal? '("" "there" "ya" "go" ""))
(string-split "" "" 'prefix)
'(expect equal? '())
(string-split "abc" "" 'prefix)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'prefix)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'prefix)
'(expect equal? '("there" "ya" "go" ""))
(string-split "" "" 'suffix)
'(expect equal? '())
(string-split "abc" "" 'suffix)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'suffix)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'suffix)
'(expect equal? '("" "there" "ya" "go"))

(string-split "" "" 'infix #f)
'(expect equal? '())
(string-split "abc" "" 'infix #f)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'infix #f)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'infix #f)
'(expect equal? '("" "there" "ya" "go" ""))
(string-split "" "" 'strict-infix #f) 'expect-error
(string-split "abc" "" 'strict-infix #f)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'strict-infix #f)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'strict-infix #f)
'(expect equal? '("" "there" "ya" "go" ""))
(string-split "" "" 'prefix #f)
'(expect equal? '())
(string-split "abc" "" 'prefix #f)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'prefix #f)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'prefix #f)
'(expect equal? '("there" "ya" "go" ""))
(string-split "" "" 'suffix #f)
'(expect equal? '())
(string-split "abc" "" 'suffix #f)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'suffix #f)
'(expect equal? '("too" "" "much" "" "data"))
(string-split "***there***ya***go***" "***" 'suffix #f)
'(expect equal? '("" "there" "ya" "go"))

(string-split "" "" 'strict-infix 3) 'expect-error
(string-split "abc" "" 'strict-infix 3)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'strict-infix 3)
'(expect equal? '("too" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'strict-infix 3)
'(expect equal? '("" "there" "ya" "go***"))
(string-split "" "" 'prefix 3)
'(expect equal? '())
(string-split "abc" "" 'prefix 3)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'prefix 3)
'(expect equal? '("too" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'prefix 3)
'(expect equal? '("there" "ya" "go***"))
(string-split "" "" 'suffix 3)
'(expect equal? '())
(string-split "abc" "" 'suffix 3)
'(expect equal? '("a" "b" "c"))
(string-split "too  much  data" " " 'suffix 3)
'(expect equal? '("too" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'suffix 3)
'(expect equal? '("" "there" "ya" "go***"))
(string-split "" "" 'strict-infix 3 0) 'expect-error
(string-split "abc" "" 'strict-infix 3 1)
'(expect equal? '("b" "c"))
(string-split "too  much  data" " " 'strict-infix 3 1)
'(expect equal? '("oo" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'strict-infix 3 1)
'(expect equal? '("**there" "ya" "go" ""))
(string-split "" "" 'prefix 3 0)
'(expect equal? '())
(string-split "abc" "" 'prefix 3 1)
'(expect equal? '("b" "c"))
(string-split "too  much  data" " " 'prefix 3 1)
'(expect equal? '("oo" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'prefix 3 1)
'(expect equal? '("**there" "ya" "go" ""))
(string-split "" "" 'suffix 3 0)
'(expect equal? '())
(string-split "abc" "" 'suffix 3 1)
'(expect equal? '("b" "c"))
(string-split "too  much  data" " " 'suffix 3 1)
'(expect equal? '("oo" "" "much" " data"))
(string-split "***there***ya***go***" "***" 'suffix 3 1)
'(expect equal? '("**there" "ya" "go"))

(string-split "" "" 'strict-infix 3 0 0) 'expect-error
(string-split "abc" "" 'strict-infix 3 1 2)
'(expect equal? '("b"))
(string-split "too  much  data" " " 'strict-infix 3 1 11)
'(expect equal? '("oo" "" "much" " "))
(string-split "" "" 'prefix 3 0 0)
'(expect equal? '())
(string-split "abc" "" 'prefix 3 1 2)
'(expect equal? '("b"))
(string-split "too  much  data" " " 'prefix 3 1 11)
'(expect equal? '("oo" "" "much" " "))
(string-split "" "" 'suffix 3 0 0)
'(expect equal? '())
(string-split "abc" "" 'suffix 3 1 2)
'(expect equal? '("b"))
(string-split "too  much  data" " " 'suffix 3 1 11)
'(expect equal? '("oo" "" "much" " "))

;; **** string-append! not yet implemented
;; (define (translate-space-to-newline str)
;;   (let ((result (make-string 0)))
;;     (string-for-each
;;      (lambda (ch)
;;        (string-append! result
;;                        (if (char=? ch #\space) #\newline ch)))
;;      str)
;;     result))
;; (translate-space-to-newline "ab cd x") '(expect equal? "ab\ncd\nx")

;; **** string-replace! not yet implemented
;; (define s10 (make-string 3 #\😂))
;; (string-length s10) '(expect equal? 3)
;; (string-replace! s10 1 2 "abc")
;; s10 '(expect equal? "😂abc😂")
;; (string-replace! s10 5 5 s10 3)
;; s10 '(expect equal? "😂abc😂c😂")
;; (string-replace! s10 0 2 "ABC" 1 2)
;; s10 '(expect equal? "Bbc😂c😂")
;; (string-length s10) '(expect equal? 6)
;; (string-ref s10 2) '(expect equal? #\c)
;; (string-ref s10 3) '(expect equal? #\x1f602)
;; (string-ref s10 4) '(expect equal? #\c)

(reverse-list->string '(#\a #\😂 #\b #\😼 #\c)) '(expect equal? "c😼b😂a")

(xsubstring "a😼xy😂" 3 9) '(expect equal? "y😂a😼xy")
(xsubstring "a😼xy😂" -2 2) '(expect equal? "y😂a😼")