#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/string.scm,v 14.5 1992/08/28 16:05:58 jinx Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Character String Operations
;;; package: ()

(declare (usual-integrations))

;;;; Primitives

(define-primitives
  string-allocate string? string-ref string-set!
  string-length string-maximum-length set-string-length!
  substring=? substring-ci=? substring<?
  substring-move-right! substring-move-left!
  substring-find-next-char-in-set
  substring-find-previous-char-in-set
  substring-match-forward substring-match-backward
  substring-match-forward-ci substring-match-backward-ci
  substring-upcase! substring-downcase! string-hash string-hash-mod

  vector-8b-ref vector-8b-set! vector-8b-fill!
  vector-8b-find-next-char vector-8b-find-previous-char
  vector-8b-find-next-char-ci vector-8b-find-previous-char-ci)

;;; Character Covers

(define-integrable (substring-fill! string start end char)
  (vector-8b-fill! string start end (char->ascii char)))

(define-integrable (substring-find-next-char string start end char)
  (vector-8b-find-next-char string start end (char->ascii char)))

(define-integrable (substring-find-previous-char string start end char)
  (vector-8b-find-previous-char string start end (char->ascii char)))

(define-integrable (substring-find-next-char-ci string start end char)
  (vector-8b-find-next-char-ci string start end (char->ascii char)))

(define-integrable (substring-find-previous-char-ci string start end char)
  (vector-8b-find-previous-char-ci string start end (char->ascii char)))

;;; Special, not implemented in microcode.

(define (substring-ci<? string1 start1 end1 string2 start2 end2)
  (let ((match (substring-match-forward-ci string1 start1 end1
					   string2 start2 end2))
	(len1 (- end1 start1))
	(len2 (- end2 start2)))
    (and (not (= match len2))
	 (or (= match len1)
	     (char-ci<? (string-ref string1 (+ match start1))
			(string-ref string2 (+ match start2)))))))

;;; Substring Covers

(define (string=? string1 string2)
  (substring=? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci=? string1 string2)
  (substring-ci=? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string<? string1 string2)
  (substring<? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci<? string1 string2)
  (substring-ci<? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string>? string1 string2)
  (substring<? string2 0 (string-length string2)
	       string1 0 (string-length string1)))

(define (string-ci>? string1 string2)
  (substring-ci<? string2 0 (string-length string2)
		  string1 0 (string-length string1)))

(define (string>=? string1 string2)
  (not (substring<? string1 0 (string-length string1)
		    string2 0 (string-length string2))))

(define (string-ci>=? string1 string2)
  (not (substring-ci<? string1 0 (string-length string1)
		       string2 0 (string-length string2))))

(define (string<=? string1 string2)
  (not (substring<? string2 0 (string-length string2)
		    string1 0 (string-length string1))))

(define (string-ci<=? string1 string2)
  (not (substring-ci<? string2 0 (string-length string2)
		       string1 0 (string-length string1))))

(define (string-fill! string char)
  (substring-fill! string 0 (string-length string) char))

(define (string-find-next-char string char)
  (substring-find-next-char string 0 (string-length string) char))

(define (string-find-previous-char string char)
  (substring-find-previous-char string 0 (string-length string) char))

(define (string-find-next-char-ci string char)
  (substring-find-next-char-ci string 0 (string-length string) char))

(define (string-find-previous-char-ci string char)
  (substring-find-previous-char-ci string 0 (string-length string) char))

(define (string-find-next-char-in-set string char-set)
  (substring-find-next-char-in-set string 0 (string-length string) char-set))

(define (string-find-previous-char-in-set string char-set)
  (substring-find-previous-char-in-set string 0 (string-length string)
				       char-set))

(define (string-match-forward string1 string2)
  (substring-match-forward string1 0 (string-length string1)
			   string2 0 (string-length string2)))

(define (string-match-backward string1 string2)
  (substring-match-backward string1 0 (string-length string1)
			    string2 0 (string-length string2)))

(define (string-match-forward-ci string1 string2)
  (substring-match-forward-ci string1 0 (string-length string1)
			      string2 0 (string-length string2)))

(define (string-match-backward-ci string1 string2)
  (substring-match-backward-ci string1 0 (string-length string1)
			       string2 0 (string-length string2)))

;;;; Basic Operations

(define (make-string length #!optional char)
  (if (default-object? char)
      (string-allocate length)
      (let ((result (string-allocate length)))
	(substring-fill! result 0 length char)
	result)))

(define-integrable (string-null? string)
  (fix:= 0 (string-length string)))

(define (substring string start end)
  (let ((result (string-allocate (fix:- end start))))
    (substring-move-right! string start end result 0)
    result))

(define-integrable (string-head string end)
  (substring string 0 end))

(define (string-tail string start)
  (substring string start (string-length string)))

(define (list->string chars)
  (let ((result (string-allocate (length chars))))
    (let loop ((index 0) (chars chars))
      (if (null? chars)
	  result
	  (begin (string-set! result index (car chars))
		 (loop (fix:+ index 1) (cdr chars)))))))

(define (string . chars)
  (list->string chars))

(define char->string string)

(define (string->list string)
  (substring->list string 0 (string-length string)))

(define (substring->list string start end)
  (let loop ((index start))
    (if (fix:< index end)
	(cons (string-ref string index)
	      (loop (fix:+ index 1)))
	'())))

(define (string-copy string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      result)))

(define (string-append . strings)
  (let ((result
	 (string-allocate
	  (let loop ((strings strings))
	    (if (null? strings)
		0
		(fix:+ (string-length (car strings))
		       (loop (cdr strings))))))))
    (let loop ((strings strings) (index 0))
      (if (null? strings)
	  result
	  (let ((size (string-length (car strings))))
	    (substring-move-right! (car strings) 0 size result index)
	    (loop (cdr strings) (fix:+ index size)))))))

;;;; Case

(define (string-upper-case? string)
  (substring-upper-case? string 0 (string-length string)))

(define (substring-upper-case? string start end)
  (let find-upper ((start start))
    (and (fix:< start end)
	 (let ((char (string-ref string start)))
	   (if (char-upper-case? char)
	       (let search-rest ((start (fix:+ start 1)))
		 (or (fix:= start end)
		     (and (not (char-lower-case? (string-ref string start)))
			  (search-rest (fix:+ start 1)))))
	       (and (not (char-lower-case? char))
		    (find-upper (fix:+ start 1))))))))

(define (string-upcase string)
  (let ((string (string-copy string)))
    (string-upcase! string)
    string))

(define (string-upcase! string)
  (substring-upcase! string 0 (string-length string)))

(define (string-lower-case? string)
  (substring-lower-case? string 0 (string-length string)))

(define (substring-lower-case? string start end)
  (let find-lower ((start start))
    (and (fix:< start end)
	 (let ((char (string-ref string start)))
	   (if (char-lower-case? char)
	       (let search-rest ((start (fix:+ start 1)))
		 (or (fix:= start end)
		     (and (not (char-upper-case? (string-ref string start)))
			  (search-rest (fix:+ start 1)))))
	       (and (not (char-upper-case? char))
		    (find-lower (fix:+ start 1))))))))

(define (string-downcase string)
  (let ((string (string-copy string)))
    (string-downcase! string)
    string))

(define (string-downcase! string)
  (substring-downcase! string 0 (string-length string)))

(define (string-capitalized? string)
  (substring-capitalized? string 0 (string-length string)))

(define (substring-capitalized? string start end)
  (and (fix:< start end)
       (char-upper-case? (string-ref string start))
       (substring-lower-case? string (fix:+ start 1) end)))

(define (string-capitalize string)
  (let ((string (string-copy string)))
    (string-capitalize! string)
    string))

(define (string-capitalize! string)
  (let ((length (string-length string)))
    (if (zero? length) (error "String must have non-zero length" string))
    (substring-upcase! string 0 1)
    (substring-downcase! string 1 length)))

;;;; Replace

(define (string-replace string char1 char2)
  (let ((string (string-copy string)))
    (string-replace! string char1 char2)
    string))

(define (substring-replace string start end char1 char2)
  (let ((string (string-copy string)))
    (substring-replace! string start end char1 char2)
    string))

(define (string-replace! string char1 char2)
  (substring-replace! string 0 (string-length string) char1 char2))

(define (substring-replace! string start end char1 char2)
  (let loop ((start start))
    (let ((index (substring-find-next-char string start end char1)))
      (if index
	  (begin
	    (string-set! string index char2)
	    (loop (fix:+ index 1)))))))

;;;; Compare

(define (string-compare string1 string2 if= if< if>)
  (let ((size1 (string-length string1))
	(size2 (string-length string2)))
    (let ((match (substring-match-forward string1 0 size1 string2 0 size2)))
      ((if (= match size1)
	   (if (= match size2) if= if<)
	   (if (= match size2) if>
	       (if (char<? (string-ref string1 match)
			   (string-ref string2 match))
		   if< if>)))))))

(define (string-prefix? string1 string2)
  (substring-prefix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward string1 start1 end1
				     string2 start2 end2)
	    length))))

(define (string-suffix? string1 string2)
  (substring-suffix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-backward string1 start1 end1
				      string2 start2 end2)
	    length))))

(define (string-compare-ci string1 string2 if= if< if>)
  (let ((size1 (string-length string1))
	(size2 (string-length string2)))
    (let ((match (substring-match-forward-ci string1 0 size1 string2 0 size2)))
      ((if (= match size1)
	   (if (= match size2) if= if<)
	   (if (= match size2) if>
	       (if (char-ci<? (string-ref string1 match)
			      (string-ref string2 match))
		   if< if>)))))))

(define (string-prefix-ci? string1 string2)
  (substring-prefix-ci? string1 0 (string-length string1)
			string2 0 (string-length string2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward-ci string1 start1 end1
					string2 start2 end2)
	    length))))

(define (string-suffix-ci? string1 string2)
  (substring-suffix-ci? string1 0 (string-length string1)
			string2 0 (string-length string2)))

(define (substring-suffix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-backward-ci string1 start1 end1
					 string2 start2 end2)
	    length))))

;;;; Trim/Pad

(define (string-trim-left string #!optional char-set)
  (let ((index
	 (string-find-next-char-in-set string
				       (if (default-object? char-set)
					   char-set:not-whitespace
					   char-set)))
	(length (string-length string)))
    (if (not index)
	""
	(substring string index length))))

(define (string-trim-right string #!optional char-set)
  (let ((index
	 (string-find-previous-char-in-set string
					   (if (default-object? char-set)
					       char-set:not-whitespace
					       char-set))))
    (if (not index)
	""
	(substring string 0 (fix:+ index 1)))))

(define (string-trim string #!optional char-set)
  (let ((char-set
	 (if (default-object? char-set) char-set:not-whitespace char-set)))
    (let ((index (string-find-next-char-in-set string char-set)))
      (if (not index)
	  ""
	  (substring string
		     index
		     (fix:+ (string-find-previous-char-in-set string char-set)
			    1))))))

(define (string-pad-right string n #!optional char)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n)))
	  (if (fix:> length n)
	      (substring-move-right! string 0 n result 0)
	      (begin
		(substring-move-right! string 0 length result 0)
		(let ((char (if (default-object? char) #\space char)))
		  (substring-fill! result length n char))))
	  result))))

(define (string-pad-left string n #!optional char)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n))
	      (i (fix:- n length)))
	  (if (negative? i)
	      (substring-move-right! string 0 n result 0)
	      (begin
		(let ((char (if (default-object? char) #\space char)))
		  (substring-fill! result 0 i char))
		(substring-move-right! string 0 length result i)))
	  result))))

(define (substring? substring string)
  ;; Returns starting-position or #f if not true.
  (if (string-null? substring)
      0
      (let ((len (string-length substring))
	    (end (string-length string))
	    (char (string-ref substring 0)))
	(let loop ((posn -1))
	  (let ((posn* (substring-find-next-char string (1+ posn) end char)))
	    (and posn*
		 (let ((end* (+ posn* len)))
		   (and (<= end* end)
			(if (substring=? substring 0 len
					 string posn* end*)
			    posn*
			    (loop posn*))))))))))