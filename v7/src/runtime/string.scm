#| -*-Scheme-*-

$Id: string.scm,v 14.27 1999/07/31 18:39:29 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; Character String Operations
;;; package: (runtime string)

;; NOTE
;;
;; This file is designed to be compiled with type and range checking
;; turned off. The advertised user-visible procedures all explicitly
;; check their arguments.
;;
;; Many of the procedures are split into several user versions that just
;; validate their arguments and pass them on to an internal version
;; (prefixed with `%') that assumes all arguments have been checked.
;; This avoids repeated argument checks.

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
  (guarantee-2-strings string1 string2 'STRING=?)
  (substring=? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI=?)
  (substring-ci=? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string<? string1 string2)
  (guarantee-2-strings string1 string2 'STRING<?)
  (substring<? string1 0 (string-length string1)
	       string2 0 (string-length string2)))

(define (string-ci<? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-ci<?)
  (substring-ci<? string1 0 (string-length string1)
		  string2 0 (string-length string2)))

(define (string>? string1 string2)
  (guarantee-2-strings string1 string2 'STRING>?)
  (substring<? string2 0 (string-length string2)
	       string1 0 (string-length string1)))

(define (string-ci>? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI>?)
  (substring-ci<? string2 0 (string-length string2)
		  string1 0 (string-length string1)))

(define (string>=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI>=?)
  (not (substring<? string1 0 (string-length string1)
		    string2 0 (string-length string2))))

(define (string-ci>=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-CI>=?)
  (not (substring-ci<? string1 0 (string-length string1)
		       string2 0 (string-length string2))))

(define (string<=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING<=?)
  (not (substring<? string2 0 (string-length string2)
		    string1 0 (string-length string1))))

(define (string-ci<=? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-ci<=?)
  (not (substring-ci<? string2 0 (string-length string2)
		       string1 0 (string-length string1))))

(define (string-fill! string char)
  (guarantee-string string 'STRING-FILL!)
  (substring-fill! string 0 (string-length string) char))

(define (string-find-next-char string char)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR)
  (substring-find-next-char string 0 (string-length string) char))

(define (string-find-previous-char string char)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR)
  (substring-find-previous-char string 0 (string-length string) char))

(define (string-find-next-char-ci string char)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR-CI)
  (substring-find-next-char-ci string 0 (string-length string) char))

(define (string-find-previous-char-ci string char)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR-CI)
  (substring-find-previous-char-ci string 0 (string-length string) char))

(define (string-find-next-char-in-set string char-set)
  (guarantee-string string 'STRING-FIND-NEXT-CHAR-IN-SET)
  (substring-find-next-char-in-set string 0 (string-length string) char-set))

(define (string-find-previous-char-in-set string char-set)
  (guarantee-string string 'STRING-FIND-PREVIOUS-CHAR-IN-SET)
  (substring-find-previous-char-in-set string 0 (string-length string)
				       char-set))

(define (string-match-forward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD)
  (substring-match-forward string1 0 (string-length string1)
			   string2 0 (string-length string2)))

(define (string-match-backward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD)
  (substring-match-backward string1 0 (string-length string1)
			    string2 0 (string-length string2)))

(define (string-match-forward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD-CI)
  (substring-match-forward-ci string1 0 (string-length string1)
			      string2 0 (string-length string2)))

(define (string-match-backward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD-CI)
  (substring-match-backward-ci string1 0 (string-length string1)
			       string2 0 (string-length string2)))

;;;; Basic Operations

(define (make-string length #!optional char)
  (guarantee-index/string length 'MAKE-STRING)
  (if (default-object? char)
      (string-allocate length)
      (let ((result (string-allocate length)))
	(substring-fill! result 0 length char)
	result)))

(define (string-null? string)
  (guarantee-string string 'STRING-NULL?)
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
  (guarantee-substring string start end 'SUBSTRING)
  (%substring string start end))

(define (string-head string end)
  (guarantee-string string 'STRING-HEAD)
  (guarantee-index/string end 'STRING-HEAD)
  (%substring string 0 end))

(define (string-tail string start)
  (guarantee-string string 'STRING-TAIL)
  (guarantee-index/string start 'STRING-TAIL)
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
  (guarantee-string string 'STRING->LIST)
  (%substring->list string 0 (string-length string)))

(define (%substring->list string start end)
  (let loop ((index (fix:- end 1)) (list '()))
    (if (fix:>= index start)
	(loop (fix:- index 1)
	      (cons (string-ref string index) list))
	list)))

(define (substring->list string start end)
  (guarantee-substring string start end 'SUBSTRING->LIST)
  (%substring->list string start end))

(define (string-copy string)
  (guarantee-string string 'STRING-COPY)
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
		  (guarantee-string (car strings) 'STRING-APPEND)
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

(define (burst-string string delimiter allow-runs?)
  (guarantee-string string 'BURST-STRING)
  (if (not (char? delimiter))
      (error:wrong-type-argument delimiter "character" 'BURST-STRING))
  (let ((end (string-length string)))
    (let loop ((start 0) (index 0) (result '()))
      (cond ((fix:= index end)
	     (reverse!
	      (if (and allow-runs? (fix:= start index))
		  result
		  (cons (substring string start index) result))))
	    ((char=? delimiter (string-ref string index))
	     (loop (fix:+ index 1)
		   (fix:+ index 1)
		   (if (and allow-runs? (fix:= start index))
		       result
		       (cons (substring string start index) result))))
	    (else
	     (loop start (fix:+ index 1) result))))))

(define (reverse-string string)
  (guarantee-string string 'REVERSE-STRING)
  (%reverse-substring string 0 (string-length string)))

(define (reverse-substring string start end)
  (guarantee-substring string start end 'REVERSE-SUBSTRING)
  (%reverse-substring string start end))

(define (%reverse-substring string start end)
  (let ((result (make-string (fix:- end start)))
	(k (fix:- end 1)))
    (do ((i start (fix:+ i 1)))
	((fix:= i end))
      (string-set! result (fix:- k i) (string-ref string i)))
    result))

(define (reverse-string! string)
  (guarantee-string string 'REVERSE-STRING!)
  (%reverse-substring! string 0 (string-length string)))

(define (reverse-substring! string start end)
  (guarantee-substring string start end 'REVERSE-SUBSTRING!)
  (%reverse-substring! string start end))

(define (%reverse-substring! string start end)
  (let ((k (fix:+ start (fix:quotient (fix:- end start) 2))))
    (do ((i start (fix:+ i 1))
	 (j (fix:- end 1) (fix:- j 1)))
	((fix:= i k))
      (let ((char (string-ref string j)))
	(string-set! string j (string-ref string i))
	(string-set! string i char)))))

;;;; Case

(define (string-upper-case? string)
  (guarantee-string string 'STRING-UPPER-CASE?)
  (%substring-upper-case? string 0 (string-length string)))

(define (substring-upper-case? string start end)
  (guarantee-substring string start end 'SUBSTRING-UPPER-CASE?)
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
  (guarantee-string string 'STRING-UPCASE!)
  (substring-upcase! string 0 (string-length string)))


(define (string-lower-case? string)
  (guarantee-string string 'STRING-LOWER-CASE?)
  (%substring-lower-case? string 0 (string-length string)))

(define (substring-lower-case? string start end)
  (guarantee-substring string start end 'SUBSTRING-LOWER-CASE?)
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
  (guarantee-string string 'STRING-DOWNCASE!)
  (substring-downcase! string 0 (string-length string)))

(define (string-capitalized? string)
  (guarantee-string string 'STRING-CAPITALIZED?)
  (substring-capitalized? string 0 (string-length string)))

(define (substring-capitalized? string start end)
  (guarantee-substring string start end 'SUBSTRING-CAPITALIZED?)
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
  (guarantee-string string 'STRING-CAPITALIZE!)
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
  (guarantee-string string 'STRING-REPLACE!)
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
  (guarantee-2-strings string1 string2 'STRING-COMPARE)
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
  (guarantee-2-strings string1 string2 'STRING-PREFIX?)
  (substring-prefix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-prefix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward string1 start1 end1
				     string2 start2 end2)
	    length))))

(define (string-suffix? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-SUFFIX?)
  (substring-suffix? string1 0 (string-length string1)
		     string2 0 (string-length string2)))

(define (substring-suffix? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-backward string1 start1 end1
				      string2 start2 end2)
	    length))))

(define (string-compare-ci string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'STRING-COMPARE-CI)
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
  (guarantee-2-strings string1 string2 'STRING-PREFIX-CI?)
  (substring-prefix-ci? string1 0 (string-length string1)
			string2 0 (string-length string2)))

(define (substring-prefix-ci? string1 start1 end1 string2 start2 end2)
  (let ((length (- end1 start1)))
    (and (<= length (- end2 start2))
	 (= (substring-match-forward-ci string1 start1 end1
					string2 start2 end2)
	    length))))

(define (string-suffix-ci? string1 string2)
  (guarantee-2-strings string1 string2 'STRING-SUFFIX-CI?)
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
	(%substring string index length))))

(define (string-trim-right string #!optional char-set)
  (let ((index
	 (string-find-previous-char-in-set string
					   (if (default-object? char-set)
					       char-set:not-whitespace
					       char-set))))
    (if (not index)
	""
	(%substring string 0 (fix:+ index 1)))))

(define (string-trim string #!optional char-set)
  (let ((char-set
	 (if (default-object? char-set) char-set:not-whitespace char-set)))
    (let ((index (string-find-next-char-in-set string char-set)))
      (if (not index)
	  ""
	  (%substring string
		      index
		      (fix:+ (string-find-previous-char-in-set string char-set)
			     1))))))

(define (string-pad-right string n #!optional char)
  (guarantee-string string 'STRING-PAD-RIGHT)
  (guarantee-index/string n 'STRING-PAD-RIGHT)
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
  (guarantee-string string 'STRING-PAD-LEFT)
  (guarantee-index/string n 'STRING-PAD-LEFT)
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

;;;; String Search

(define (substring? pattern text)
  (and (string-search-forward pattern text) #t))

(define (string-search-forward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-FORWARD)
  (guarantee-string text 'STRING-SEARCH-FORWARD)
  (%substring-search-forward text 0 (string-length text)
			     pattern 0 (string-length pattern)))

(define (substring-search-forward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-FORWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-FORWARD)
  (%substring-search-forward text tstart tend
			     pattern 0 (string-length pattern)))

(define (string-search-backward pattern text)
  (guarantee-string pattern 'STRING-SEARCH-BACKWARD)
  (guarantee-string text 'STRING-SEARCH-BACKWARD)
  (%substring-search-backward text 0 (string-length text)
			      pattern 0 (string-length pattern)))

(define (substring-search-backward pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-BACKWARD)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-BACKWARD)
  (%substring-search-backward text tstart tend
			      pattern 0 (string-length pattern)))

(define (string-search-all pattern text)
  (guarantee-string pattern 'STRING-SEARCH-ALL)
  (guarantee-string text 'STRING-SEARCH-ALL)
  (%bm-substring-search-all text 0 (string-length text)
			    pattern 0 (string-length pattern)))

(define (substring-search-all pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-ALL)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-ALL)
  (%bm-substring-search-all text tstart tend
			    pattern 0 (string-length pattern)))

(define (%substring-search-forward text tstart tend pattern pstart pend)
  ;; Returns index of first matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-forward text tstart tend pattern pstart pend)
      (%bm-substring-search-forward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-forward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((leader (string-ref pattern pstart))
	     (plen (fix:- pend pstart))
	     (tend (fix:- tend plen)))
	(let loop ((tstart tstart))
	  (let ((tstart
		 (let find-leader ((tstart tstart))
		   (and (fix:<= tstart tend)
			(if (char=? leader (string-ref text tstart))
			    tstart
			    (find-leader (fix:+ tstart 1)))))))
	    (and tstart
		 (if (substring=? text (fix:+ tstart 1) (fix:+ tstart plen)
				  pattern (fix:+ pstart 1) pend)
		     tstart
		     (loop (fix:+ tstart 1)))))))))

(define (%substring-search-backward text tstart tend pattern pstart pend)
  ;; Returns index following last matched char, or #F.
  (if (fix:< (fix:- pend pstart) 4)
      (%dumb-substring-search-backward text tstart tend pattern pstart pend)
      (%bm-substring-search-backward text tstart tend pattern pstart pend)))

(define (%dumb-substring-search-backward text tstart tend pattern pstart pend)
  (if (fix:= pstart pend)
      0
      (let* ((pend-1 (fix:- pend 1))
	     (trailer (string-ref pattern pend-1))
	     (plen (fix:- pend pstart))
	     (tstart+plen (fix:+ tstart plen)))
	(let loop ((tend tend))
	  (let ((tend
		 (let find-trailer ((tend tend))
		   (and (fix:<= tstart+plen tend)
			(if (char=? trailer (string-ref text (fix:- tend 1)))
			    tend
			    (find-trailer (fix:- tend 1)))))))
	    (and tend
		 (if (substring=? text (fix:- tend plen) (fix:- tend 1)
				  pattern pstart pend-1)
		     tend
		     (loop (fix:- tend 1)))))))))

;;;; Boyer-Moore String Search

;;; Cormen, Leiserson, and Rivest, "Introduction to Algorithms",
;;; Chapter 34, "String Matching".

(define (%bm-substring-search-forward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma
	 (compute-good-suffix-function pattern pstart pend
				       (compute-gamma0 pattern pstart pend))))
    (let ((tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart))
	(and (fix:<= s tend-m)
	     (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pstart pj)
		       s
		       (inner (fix:- pj 1) (fix:- tj 1)))
		   (outer
		    (fix:+ s
			   (fix:max (fix:- (fix:- pj pstart-1)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pj pstart))))))))))))

(define (%bm-substring-search-backward text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pend-1 (fix:- pend 1))
	(rpattern (reverse-substring pattern pstart pend)))
    (let ((tstart+m (fix:+ tstart m))
	  (lambda* (compute-last-occurrence-function rpattern 0 m))
	  (gamma
	   (compute-good-suffix-function rpattern 0 m
					 (compute-gamma0 rpattern 0 m))))
      (let outer ((s tend))
	(and (fix:>= s tstart+m)
	     (let inner ((pj pstart) (tj (fix:- s m)))
	       (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		   (if (fix:= pend-1 pj)
		       s
		       (inner (fix:+ pj 1) (fix:+ tj 1)))
		   (outer
		    (fix:- s
			   (fix:max (fix:- (fix:- pend pj)
					   (lambda* (vector-8b-ref text tj)))
				    (gamma (fix:- pend-1 pj))))))))))))

(define (%bm-substring-search-all text tstart tend pattern pstart pend)
  (let ((m (fix:- pend pstart))
	(pstart-1 (fix:- pstart 1))
	(pend-1 (fix:- pend 1))
	(lambda* (compute-last-occurrence-function pattern pstart pend))
	(gamma0 (compute-gamma0 pattern pstart pend)))
    (let ((gamma (compute-good-suffix-function pattern pstart pend gamma0))
	  (tend-m (fix:- tend m))
	  (m-1 (fix:- m 1)))
      (let outer ((s tstart) (occurrences '()))
	(if (fix:<= s tend-m)
	    (let inner ((pj pend-1) (tj (fix:+ s m-1)))
	      (if (fix:= (vector-8b-ref pattern pj) (vector-8b-ref text tj))
		  (if (fix:= pstart pj)
		      (outer (fix:+ s gamma0) (cons s occurrences))
		      (inner (fix:- pj 1) (fix:- tj 1)))
		  (outer (fix:+ s
				(fix:max (fix:- (fix:- pj pstart-1)
						(lambda*
						 (vector-8b-ref text tj)))
					 (gamma (fix:- pj pstart))))
			 occurrences)))
	    (reverse! occurrences))))))

(define (compute-last-occurrence-function pattern pstart pend)
  (let ((lam (make-vector 256 0)))
    (do ((j pstart (fix:+ j 1)))
	((fix:= j pend))
      (vector-set! lam
		   (vector-8b-ref pattern j)
		   (fix:+ (fix:- j pstart) 1)))
    (lambda (symbol)
      (vector-ref lam symbol))))

(define (compute-good-suffix-function pattern pstart pend gamma0)
  (let ((m (fix:- pend pstart)))
    (let ((pi
	   (compute-prefix-function (reverse-substring pattern pstart pend)
				    0 m))
	  (gamma (make-vector m gamma0))
	  (m-1 (fix:- m 1)))
      (do ((l 0 (fix:+ l 1)))
	  ((fix:= l m))
	(let ((j (fix:- m-1 (vector-ref pi l)))
	      (k (fix:- (fix:+ 1 l) (vector-ref pi l))))
	  (if (fix:< k (vector-ref gamma j))
	      (vector-set! gamma j k))))
      (lambda (index)
	(vector-ref gamma index)))))

(define (compute-gamma0 pattern pstart pend)
  (let ((m (fix:- pend pstart)))
    (fix:- m
	   (vector-ref (compute-prefix-function pattern pstart pend)
		       (fix:- m 1)))))

(define (compute-prefix-function pattern pstart pend)
  (let* ((m (fix:- pend pstart))
	 (pi (make-vector m)))
    (vector-set! pi 0 0)
    (let outer ((k 0) (q 1))
      (if (fix:< q m)
	  (let ((k
		 (let ((pq (vector-8b-ref pattern (fix:+ pstart q))))
		   (let inner ((k k))
		     (cond ((fix:= pq (vector-8b-ref pattern (fix:+ pstart k)))
			    (fix:+ k 1))
			   ((fix:= k 0)
			    k)
			   (else
			    (inner (vector-ref pi (fix:- k 1)))))))))
	    (vector-set! pi q k)
	    (outer k (fix:+ q 1)))))
    pi))

;;;; Guarantors
;;
;; The guarantors are integrated.  Most are structured as combination of
;; simple tests which the compiler can open-code, followed by a call to a
;; GUARANTEE-.../FAIL version which does the tests again to signal a
;; menaingful message. Structuring the code this way significantly
;; reduces code bloat from large integrated procedures.


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
	 (error:wrong-type-argument object2 "string" procedure))))

(define-integrable (guarantee-index/string object procedure)
  (if (not (index-fixnum? object))
      (guarantee-index/string/fail object procedure)))

(define (guarantee-index/string/fail object procedure)
  (error:wrong-type-argument object "valid string index"
			     procedure))


(define-integrable (guarantee-substring string start end procedure)
  (if (not (and (string? string)
		(index-fixnum? start)
		(index-fixnum? end)
		(fix:<= start end)
		(fix:<= end (string-length string))))
      (guarantee-substring/fail string start end procedure)))

(define (guarantee-substring/fail string start end procedure)
  (guarantee-string string procedure)
  (guarantee-index/string start procedure)
  (guarantee-index/string end procedure)
  (if (not (fix:<= end (string-length string)))
      (error:bad-range-argument end procedure))
  (if (not (fix:<= start end))
      (error:bad-range-argument start procedure)))