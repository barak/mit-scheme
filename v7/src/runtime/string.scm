;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/string.scm,v 13.43 1987/12/17 20:32:25 cph Rel $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Character String Operations

(declare (usual-integrations))

;;;; Primitives

(let-syntax ((define-primitives
	       (macro names
		 `(BEGIN ,@(map (lambda (name)
				  `(LOCAL-ASSIGNMENT
				    SYSTEM-GLOBAL-ENVIRONMENT
				    ',name
				    ,(make-primitive-procedure name)))
				names)))))
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
   vector-8b-find-next-char-ci vector-8b-find-previous-char-ci))

;;; Character Covers

(define (substring-fill! string start end char)
  (vector-8b-fill! string start end (char->ascii char)))

(define (substring-find-next-char string start end char)
  (vector-8b-find-next-char string start end (char->ascii char)))

(define (substring-find-previous-char string start end char)
  (vector-8b-find-previous-char string start end (char->ascii char)))

(define (substring-find-next-char-ci string start end char)
  (vector-8b-find-next-char-ci string start end (char->ascii char)))

(define (substring-find-previous-char-ci string start end char)
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
  (if (unassigned? char)
      (string-allocate length)
      (let ((result (string-allocate length)))
	(substring-fill! result 0 length char)
	result)))

(define (string-null? string)
  (zero? (string-length string)))

(define (substring string start end)
  (let ((result (string-allocate (- end start))))
    (substring-move-right! string start end result 0)
    result))

(define (list->string chars)
  (let ((result (string-allocate (length chars))))
    (define (loop index chars)
      (if (null? chars)
	  result
	  (begin (string-set! result index (car chars))
		 (loop (1+ index) (cdr chars)))))
    (loop 0 chars)))

(define (char->string . chars)
  (list->string chars))

(define (string->list string)
  (substring->list string 0 (string-length string)))

(define (substring->list string start end)
  (define (loop index)
    (if (= index end)
	'()
	(cons (string-ref string index)
	      (loop (1+ index)))))
  (loop start))

(define (string-copy string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      result)))

(define (string-append . strings)
  (define (count strings)
    (if (null? strings)
	0
	(+ (string-length (car strings))
	   (count (cdr strings)))))

  (let ((result (string-allocate (count strings))))
    (define (move strings index)
      (if (null? strings)
	  result
	  (let ((size (string-length (car strings))))
	    (substring-move-right! (car strings) 0 size result index)
	    (move (cdr strings) (+ index size)))))

    (move strings 0)))

;;;; Case

(define (string-upper-case? string)
  (substring-upper-case? string 0 (string-length string)))

(define (substring-upper-case? string start end)
  (define (find-upper start)
    (and (not (= start end))
	 ((if (char-upper-case? (string-ref string start))
	      search-rest
	      find-upper)
	  (1+ start))))
  (define (search-rest start)
    (or (= start end)
	(and (not (char-lower-case? (string-ref string start)))
	     (search-rest (1+ start)))))
  (find-upper start))

(define (string-upcase string)
  (let ((string (string-copy string)))
    (string-upcase! string)
    string))

(define (string-upcase! string)
  (substring-upcase! string 0 (string-length string)))

(define (string-lower-case? string)
  (substring-lower-case? string 0 (string-length string)))

(define (substring-lower-case? string start end)
  (define (find-lower start)
    (and (not (= start end))
	 ((if (char-lower-case? (string-ref string start))
	      search-rest
	      find-lower)
	  (1+ start))))
  (define (search-rest start)
    (or (= start end)
	(and (not (char-upper-case? (string-ref string start)))
	     (search-rest (1+ start)))))
  (find-lower start))

(define (string-downcase string)
  (let ((string (string-copy string)))
    (string-downcase! string)
    string))

(define (string-downcase! string)
  (substring-downcase! string 0 (string-length string)))

(define (string-capitalized? string)
  (substring-capitalized? string 0 (string-length string)))

(define (substring-capitalized? string start end)
  (and (not (= start end))
       (char-upper-case? (string-ref string 0))
       (substring-lower-case? string (1+ start) end)))

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
  (define (loop start)
    (let ((index (substring-find-next-char string start end char1)))
      (if index
	  (begin (string-set! string index char2)
		 (loop (1+ index))))))
  (loop start))

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
  (and (<= (- end1 start1) (- end2 start2))
       (= (substring-match-forward string1 start1 end1
				   string2 start2 end2)
	  end1)))

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
  (and (<= (- end1 start1) (- end2 start2))
       (= (substring-match-forward-ci string1 start1 end1
				      string2 start2 end2)
	  end1)))

;;;; Trim/Pad

(define (string-trim-left string #!optional char-set)
  (if (unassigned? char-set) (set! char-set char-set:not-whitespace))
  (let ((index (string-find-next-char-in-set string char-set))
	(length (string-length string)))
    (if (not index)
	""
	(substring string index length))))

(define (string-trim-right string #!optional char-set)
  (if (unassigned? char-set) (set! char-set char-set:not-whitespace))
  (let ((index (string-find-previous-char-in-set string char-set)))
    (if (not index)
	""
	(substring string 0 (1+ index)))))

(define (string-trim string #!optional char-set)
  (if (unassigned? char-set) (set! char-set char-set:not-whitespace))
  (let ((index (string-find-next-char-in-set string char-set)))
    (if (not index)
	""
	(substring string index
		   (1+ (string-find-previous-char-in-set string char-set))))))

(define (string-pad-right string n #!optional char)
  (if (unassigned? char) (set! char #\Space))
  (let ((length (string-length string)))
    (if (= length n)
	string
	(let ((result (string-allocate n)))
	  (if (> length n)
	      (substring-move-right! string 0 n result 0)
	      (begin (substring-move-right! string 0 length result 0)
		     (substring-fill! result length n char)))
	  result))))

(define (string-pad-left string n #!optional char)
  (if (unassigned? char) (set! char #\Space))
  (let ((length (string-length string)))
    (if (= length n)
	string
	(let ((result (string-allocate n))
	      (i (- n length)))
	  (if (negative? i)
	      (substring-move-right! string 0 n result 0)
	      (begin (substring-fill! result 0 i char)
		     (substring-move-right! string 0 length result i)))
	  result))))