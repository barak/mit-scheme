#| -*-Scheme-*-

$Id: string.scm,v 14.12 1995/07/27 21:33:44 adams Exp $

Copyright (c) 1988-1995 Massachusetts Institute of Technology

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
  (guarantee-2-strings string1 string2 'string=?)
  (substring=? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci=? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci=?)
  (substring-ci=? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string<? string1 string2)
  (guarantee-2-strings string1 string2 'string<?)
  (substring<? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci<? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci<?)
  (substring-ci<? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string>? string1 string2)
  (guarantee-2-strings string1 string2 'string>?)
  (substring<? string2 0 (string-length string2)
	       string1 0 (string-length string1)))

(define (string-ci>? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci>?)
  (substring-ci<? string2 0 (string-length string2)
		  string1 0 (string-length string1)))

(define (string>=? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci>=?)
  (not (substring<? string1 0 (string-length string1)
		    string2 0 (string-length string2))))

(define (string-ci>=? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci>=?)
  (not (substring-ci<? string1 0 (string-length string1)
		       string2 0 (string-length string2))))

(define (string<=? string1 string2)
  (guarantee-2-strings string1 string2 'string<=?)
  (not (substring<? string2 0 (string-length string2)
		    string1 0 (string-length string1))))

(define (string-ci<=? string1 string2)
  (guarantee-2-strings string1 string2 'string-ci<=?)
  (not (substring-ci<? string2 0 (string-length string2)
		       string1 0 (string-length string1))))

(define (string-fill! string char)
  (guarantee-string string 'string-fill!)
  (substring-fill! string 0 (string-length string) char))

(define (string-find-next-char string char)
  (guarantee-string string 'string-find-next-char)
  (substring-find-next-char string 0 (string-length string) char))

(define (string-find-previous-char string char)
  (guarantee-string string 'string-find-previous-char)
  (substring-find-previous-char string 0 (string-length string) char))

(define (string-find-next-char-ci string char)
  (guarantee-string string 'string-find-next-char-ci)
  (substring-find-next-char-ci string 0 (string-length string) char))

(define (string-find-previous-char-ci string char)
  (guarantee-string string 'string-find-previous-char-ci)
  (substring-find-previous-char-ci string 0 (string-length string) char))

(define (string-find-next-char-in-set string char-set)
  (guarantee-string string 'string-find-next-char-in-set)
  (substring-find-next-char-in-set string 0 (string-length string) char-set))

(define (string-find-previous-char-in-set string char-set)
  (guarantee-string string 'string-find-previous-char-in-set)
  (substring-find-previous-char-in-set string 0 (string-length string)
				       char-set))

(define (string-match-forward string1 string2)
  (guarantee-2-strings string1 string2 'string-match-forward)
  (substring-match-forward string1 0 (string-length string1)
			   string2 0 (string-length string2)))

(define (string-match-backward string1 string2)
  (guarantee-2-strings string1 string2 'string-match-backward)
  (substring-match-backward string1 0 (string-length string1)
			    string2 0 (string-length string2)))

(define (string-match-forward-ci string1 string2)
  (guarantee-2-strings string1 string2 'string-match-forward-ci)
  (substring-match-forward-ci string1 0 (string-length string1)
			      string2 0 (string-length string2)))

(define (string-match-backward-ci string1 string2)
  (guarantee-2-strings string1 string2 'string-match-backward-ci)
  (substring-match-backward-ci string1 0 (string-length string1)
			       string2 0 (string-length string2)))

;;;; Basic Operations

(define (make-string length #!optional char)
  (guarantee-index/string length 'make-string)
  (if (default-object? char)
      (string-allocate length)
      (let ((result (string-allocate length)))
	(substring-fill! result 0 length char)
	result)))

(define (string-null? string)
  (guarantee-string string 'string-null?)
  (%string-null? string))

(define-integrable (%string-null? string)
  (fix:= 0 (string-length string)))

(define-integrable (%substring string start end)
  (let ((start start)
	(end end))
    (let ((result (string-allocate (fix:- end start))))
      (substring-move-right! string start end result 0)
      result)))

(define (substring string start end)
  (guarantee-string string 'substring)
  (guarantee-index/string start 'substring)
  (guarantee-index/string end   'substring)
  (%substring string start end))

(define (string-head string end)
  (guarantee-string string 'string-head)
  (guarantee-index/string end 'string-head)
  (%substring string 0 end))

(define (string-tail string start)
  (guarantee-string string 'string-tail)
  (guarantee-index/string start 'string-tail)
  (%substring string start (string-length string)))

(define (list->string chars)
  ;; This should check that each element of CHARS satisfies CHAR? but at
  ;; worst it will generate strings containing rubbish from the
  ;; addresses of the objects ...
  (let ((result (string-allocate (length chars))))
    (let loop ((index 0) (chars chars))
      (if (null? chars)
	  result
	  ;; LENGTH would have barfed if input is not a proper list:
	  (begin (string-set! result index (car chars))
		 (loop (fix:+ index 1) (cdr chars)))))))

(define (string . chars)
  (list->string chars))

(define char->string string)

(define (string->list string)
  (guarantee-string string 'string->list)
  (%substring->list string 0 (string-length string)))

;; This version is unnecessarily recursive:
;;
;;(define (%substring->list string start end)
;;  (let loop ((index start))
;;    (if (fix:< index end)
;;	(cons (string-ref string index)
;;	      (loop (fix:+ index 1)))
;;	'())))

(define (%substring->list string start end)
  (let loop ((index (fix:- end 1)) (list '()))
    (if (fix:>= index start)
	(loop (fix:- index 1)
	      (cons (string-ref string index) list))
	list)))

(define (substring->list string start end)
  (guarantee-string string 'substring->list)
  (guarantee-index/string start 'substring->list)
  (guarantee-string-bound end string 'substring->list)
  (%substring->list string start end))

(define (string-copy string)
  (guarantee-string string 'string-copy)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (substring-move-right! string 0 size result 0)
      result)))

(define (%string-append strings)
  (let ((result
	 (string-allocate
	  (let loop ((strings strings) (length 0))
	    (if (null? strings)
		length
		(begin
		  (guarantee-string (car strings) 'string-append)
		  (loop (cdr strings)
			(fix:+ (string-length (car strings)) length))))))))

    (let loop ((strings strings) (index 0))
      (if (null? strings)
	  result
	  (let ((size (string-length (car strings))))
	    (substring-move-right! (car strings) 0 size result index)
	    (loop (cdr strings) (fix:+ index size)))))))

(define (string-append . strings)
  (%string-append strings))

;;;; Case

(define (string-upper-case? string)
  (guarantee-string string 'string-upper-case?)
  (%substring-upper-case? string 0 (string-length string)))

(define (substring-upper-case? string start end)
  (guarantee-string string 'substring-upper-case?)
  (guarantee-index/string start 'substring-upper-case?)
  (guarantee-string-bound end string 'substring-upper-case?)
  (%substring-upper-case? string start end))

(define (%substring-upper-case? string start end)
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
    (substring-upcase! string 0 (string-length string))
    string))

(define (string-upcase! string)
  (guarantee-string string 'string-upcase!)
  (substring-upcase! string 0 (string-length string)))

;;

(define (string-lower-case? string)
  (guarantee-string string 'string-lower-case?)
  (%substring-lower-case? string 0 (string-length string)))

(define (substring-lower-case? string start end)
  (guarantee-string string 'substring-lower-case?)
  (guarantee-index/string start 'substring-lower-case?)
  (guarantee-string-bound end string 'substring-lower-case?)
  (%substring-lower-case? string start end))
  
(define (%substring-lower-case? string start end)
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
    (substring-downcase! string 0 (string-length string))
    string))

(define (string-downcase! string)
  (guarantee-string string 'string-downcase!)
  (substring-downcase! string 0 (string-length string)))

(define (string-capitalized? string)
  (guarantee-string string 'string-capitalized?)
  (substring-capitalized? string 0 (string-length string)))

(define (substring-capitalized? string start end)
  (guarantee-string string 'substring-capitalized?)
  (guarantee-index/string start 'substring-capitalized?)
  (guarantee-string-bound end string 'substring-capitalized?)
  (%substring-capitalized? string start end))

(define (%substring-capitalized? string start end)
  ;; Testing for capitalization is somewhat more involved than testing
  ;; for upper or lower case.  This algorithm requires that the first
  ;; word be capitalized, and that the subsequent words be either
  ;; lower case or capitalized.  This is a very general definition of
  ;; capitalization; if you need something more specific you should
  ;; call this procedure on the individual words.
  (letrec
      ((find-first-word
	(lambda (start)
	  (and (fix:< start end)
	       (let ((char (string-ref string start)))
		 (if (char-upper-case? char)
		     (scan-word-tail (fix:+ start 1))
		     (and (not (char-lower-case? char))
			  (find-first-word (fix:+ start 1))))))))
       (scan-word-tail
	(lambda (start)
	  (or (fix:= start end)
	      (let ((char (string-ref string start)))
		(if (char-lower-case? char)
		    (scan-word-tail (fix:+ start 1))
		    (and (not (char-upper-case? char))
			 (find-subsequent-word (fix:+ start 1))))))))
       (find-subsequent-word
	(lambda (start)
	  (or (fix:= start end)
	      (let ((char (string-ref string start)))
		(if (char-alphabetic? char)
		    (scan-word-tail (fix:+ start 1))
		    (find-subsequent-word (fix:+ start 1))))))))
    (find-first-word start)))

(define (string-capitalize string)
  (let ((string (string-copy string)))
    (substring-capitalize! string 0 (string-length string))
    string))

(define (string-capitalize! string)
  (guarantee-string string 'string-capitalize!)
  (substring-capitalize! string 0 (string-length string)))

(define (substring-capitalize! string start end)
  ;; This algorithm capitalizes the first word in the substring and
  ;; downcases the subsequent words.  This is arbitrary, but seems
  ;; useful if the substring happens to be a sentence.  Again, if you
  ;; need finer control, parse the words yourself.
  (let ((index
	 (substring-find-next-char-in-set string start end
					  char-set:alphabetic)))
    (if index
	(begin
	  (substring-upcase! string index (fix:+ index 1))
	  (substring-downcase! string (fix:+ index 1) end)))))

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
  (guarantee-string string 'string-replace!)
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
  (guarantee-2-strings string1 string2 'string-compare)
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
  (guarantee-2-strings string1 string2 'string-prefix?)
  (substring-prefix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward string1 start1 end1
				     string2 start2 end2)
	    length))))

(define (string-suffix? string1 string2)
  (guarantee-2-strings string1 string2 'string-suffix?)
  (substring-suffix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-backward string1 start1 end1
				      string2 start2 end2)
	    length))))

(define (string-compare-ci string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'string-compare-ci)
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
  (guarantee-2-strings string1 string2 'string-prefix-ci?)
  (substring-prefix-ci? string1 0 (string-length string1)
			string2 0 (string-length string2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward-ci string1 start1 end1
					string2 start2 end2)
	    length))))

(define (string-suffix-ci? string1 string2)
  (guarantee-2-strings string1 string2 'string-suffix-ci?)
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
  (guarantee-string string 'string-pad-right)
  (guarantee-index/string n 'string-pad-right)
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
  (guarantee-string string 'string-pad-left)
  (guarantee-index/string n 'string-pad-left)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n))
	      (i (fix:- n length)))
	  (if (fix:< i 0)
	      (substring-move-right! string (fix:- 0 i) length result 0)
	      (begin
		(let ((char (if (default-object? char) #\space char)))
		  (substring-fill! result 0 i char))
		(substring-move-right! string 0 length result i)))
	  result))))

(define (substring? substring string)
  ;; Returns starting-position or #f if not true.
  (guarantee-string substring 'substring?)
  (guarantee-string string 'substring?)
  (if (%string-null? substring)
      0
      (let ((len (string-length substring))
	    (end (string-length string))
	    (char (string-ref substring 0)))
	(let loop ((posn -1))
	  (let ((posn*
		 (substring-find-next-char string (fix:+ posn 1) end char)))
	    (and posn*
		 (let ((end* (fix:+ posn* len)))
		   (and (fix:<= end* end)
			(if (substring=? substring 0 len
					 string posn* end*)
			    posn*
			    (loop posn*))))))))))

(define-integrable (guarantee-string object procedure)
  (if (not (string? object))
      (error:wrong-type-argument object "string" procedure)))

(define-integrable (guarantee-2-strings object1 object2 procedure)
  (if (and (string? object1)
	   (string? object2))
      unspecific
      (guarantee-2-strings/fail object1 object2 procedure)))

(define (guarantee-2-strings/fail object1 object2 procedure)
  (cond ((not (string? object1))
	 (error:wrong-type-argument object1 "string" procedure))
	((not (string? object2))
	 (error:wrong-type-argument object1 "string" procedure))))

(define-integrable (guarantee-index/string object procedure)
  (if (not (index-fixnum? object))
      (guarantee-index/string/fail object procedure)))

(define (guarantee-index/string/fail object procedure)
  (error:wrong-type-argument object "valid string index"
			     procedure))
;; Not used:
;;(define-integrable (guarantee-string-index object string procedure)
;;  (guarantee-index/string object procedure)
;;  (if (not (fix:< object (string-length string)))
;;      (error:bad-range-argument object procedure)))

(define-integrable (guarantee-string-bound object string procedure)
  (guarantee-index/string object procedure)
  (if (not (fix:<= object (string-length string)))
      (error:bad-range-argument object procedure)))

