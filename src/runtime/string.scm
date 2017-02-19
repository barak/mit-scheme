#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Character String Operations
;;; package: (runtime string)

;;; This file is designed to be compiled with type and range checking
;;; turned off. The advertised user-visible procedures all explicitly
;;; check their arguments.
;;;
;;; Many of the procedures are split into several user versions that
;;; just validate their arguments and pass them on to an internal
;;; version (prefixed with `%') that assumes all arguments have been
;;; checked.  This avoids repeated argument checks.

(declare (usual-integrations)
	 (integrate-external "char")
	 (integrate-external "chrset"))

;;;; Primitives

(define-primitives
  (set-string-length! 2)
  (string-allocate 1)
  (string-length 1)
  (string-ref 2)
  (string-set! 3)
  (string? 1)
  vector-8b-fill!
  vector-8b-find-next-char
  vector-8b-find-next-char-ci
  vector-8b-find-previous-char
  vector-8b-find-previous-char-ci
  (vector-8b-ref 2)
  (vector-8b-set! 3))

;;;; Basic Operations

(define (make-string length #!optional char)
  (guarantee-string-index length 'MAKE-STRING)
  (if (default-object? char)
      (string-allocate length)
      (begin
	(guarantee-char char 'MAKE-STRING)
	(let ((result (string-allocate length)))
	  (substring-fill! result 0 length char)
	  result))))

(define (make-vector-8b length #!optional ascii)
  (make-string length (if (default-object? ascii) ascii (integer->char ascii))))

(define (ascii-string-copy string)
  (guarantee-string string 'ASCII-STRING-COPY)
  (%ascii-string-copy string))

(define (%ascii-string-copy string)
  (let ((size (string-length string)))
    (let ((result (string-allocate size)))
      (and (%ascii-substring-move! string 0 size result 0)
	   result))))

(define (string-maximum-length string)
  (guarantee-string string 'STRING-MAXIMUM-LENGTH)
  (fix:- (fix:lsh (fix:- (system-vector-length string) 1)
		  %words->octets-shift)
	 1))

(define %octets->words-shift
  (let ((chars-per-word (vector-ref (gc-space-status) 0)))
    (case chars-per-word
      ((4) -2)
      ((8) -3)
      (else (error "Can't support this word size:" chars-per-word)))))

(define %words->octets-shift
  (- %octets->words-shift))

(define (char->string char)
  (guarantee 8-bit-char? char 'CHAR->STRING)
  (make-string 1 char))

;;; Almost all symbols are ascii, so it is worthwhile to handle them
;;; specially.  In this procedure, we `optimistically' move the
;;; characters, but if we find any non-ascii characters, we
;;; immediately return #F.  Success is signalled by returning the
;;; second string.  NOTE that the second string will likely be mutated
;;; in either case.
(define (%ascii-substring-move! string1 start1 end1 string2 start2)
  (let-syntax
      ((unrolled-move-left
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(LET ((CODE (VECTOR-8B-REF STRING1 START1)))
		(AND (FIX:< CODE #x80)
                     (BEGIN
                       (VECTOR-8B-SET! STRING2 START2 CODE)
                       ,(let loop ((i 1))
                          (if (< i n)
                              `(LET ((CODE
				      (VECTOR-8B-REF STRING1
						     (FIX:+ START1 ,i))))
                                 (AND (FIX:< CODE #x80)
                                      (BEGIN
					(VECTOR-8B-SET! STRING2
							(FIX:+ START2 ,i)
							CODE)
                                        ,(loop (+ i 1)))))
                              'STRING2)))))))))
       (unrolled-move-right
	(sc-macro-transformer
	 (lambda (form environment)
	   environment
	   (let ((n (cadr form)))
	     `(LET ((CODE (VECTOR-8B-REF STRING1 (FIX:+ START1 ,(- n 1)))))
		(AND (FIX:< CODE #x80)
                     (BEGIN
                       (VECTOR-8B-SET! STRING2 (FIX:+ START2 ,(- n 1)) CODE)
                       ,(let loop ((i (- n 1)))
                          (if (> i 0)
                              `(LET ((CODE
				      (VECTOR-8B-REF STRING1
						     (FIX:+ START1 ,(- i 1)))))
                                 (AND (FIX:< CODE #x80)
                                      (BEGIN
				       (VECTOR-8B-SET! STRING2
						       (FIX:+ START2 ,(- i 1))
						       CODE)
                                        ,(loop (- i 1)))))
                              'STRING2))))))))))
    (let ((n (fix:- end1 start1)))
      (if (or (not (eq? string2 string1)) (fix:< start2 start1))
	  (cond ((fix:> n 4)
		 (let loop ((i1 start1) (i2 start2))
		   (if (fix:< i1 end1)
		       (let ((code (vector-8b-ref string1 i1)))
			 (and (fix:< code #x80)
			      (begin
				(vector-8b-set! string2 i2 code)
				(loop (fix:+ i1 1) (fix:+ i2 1)))))
		       string2)))
		((fix:= n 4) (unrolled-move-left 4))
		((fix:= n 3) (unrolled-move-left 3))
		((fix:= n 2) (unrolled-move-left 2))
		((fix:= n 1) (unrolled-move-left 1)))
	  (cond ((fix:> n 4)
		 (let loop ((i1 end1) (i2 (fix:+ start2 n)))
		   (if (fix:> i1 start1)
		       (let ((i1 (fix:- i1 1))
			     (i2 (fix:- i2 1)))
			 (let ((code (vector-8b-ref string1 i1)))
			   (and (fix:< code #x80)
				(begin
				  (vector-8b-set! string2 i2 code)
				  (loop i1 i2)))))
		       string2)))
		((fix:= n 4) (unrolled-move-right 4))
		((fix:= n 3) (unrolled-move-right 3))
		((fix:= n 2) (unrolled-move-right 2))
		((fix:= n 1) (unrolled-move-right 1)))))))

(define (reverse-string string)
  (guarantee-string string 'REVERSE-STRING)
  (%reverse-substring string 0 (string-length string)))

(define (reverse-substring string start end)
  (guarantee-substring string start end 'REVERSE-SUBSTRING)
  (%reverse-substring string start end))

(define (%reverse-substring string start end)
  (let ((n (fix:- end start)))
    (let ((result (make-string n)))
      (do ((i start (fix:+ i 1))
	   (j (fix:- n 1) (fix:- j 1)))
	  ((fix:= i end))
	(string-set! result j (string-ref string i)))
      result)))

(define (vector-8b->hexadecimal bytes)
  (define-integrable (hex-char k)
    (string-ref "0123456789abcdef" (fix:and k #x0F)))
  (guarantee-string bytes 'VECTOR-8B->HEXADECIMAL)
  (let ((n (vector-8b-length bytes)))
    (let ((s (make-string (fix:* 2 n))))
      (do ((i 0 (fix:+ i 1))
	   (j 0 (fix:+ j 2)))
	  ((not (fix:< i n)))
	(string-set! s j (hex-char (fix:lsh (vector-8b-ref bytes i) -4)))
	(string-set! s (fix:+ j 1) (hex-char (vector-8b-ref bytes i))))
      s)))

(define (hexadecimal->vector-8b string)
  (guarantee-string string 'HEXADECIMAL->VECTOR-8B)
  (let ((end (string-length string))
	(lose
	 (lambda ()
	   (error:bad-range-argument string 'HEXADECIMAL->VECTOR-8B))))
    (define-integrable (hex-digit char)
      (let ((i (char->integer char))
	    (d0 (char->integer #\0))
	    (d9 (char->integer #\9))
	    (la (char->integer #\a))
	    (lf (char->integer #\f))
	    (UA (char->integer #\A))
	    (UF (char->integer #\F)))
	(cond ((and (fix:<= d0 i) (fix:<= i d9)) (fix:- i d0))
	      ((and (fix:<= la i) (fix:<= i lf)) (fix:+ #xa (fix:- i la)))
	      ((and (fix:<= UA i) (fix:<= i UF)) (fix:+ #xA (fix:- i UA)))
	      (else (lose)))))
    (if (not (fix:= (fix:and end 1) 0))
	(lose))
    (let ((bytes (make-vector-8b (fix:lsh end -1))))
      (do ((i 0 (fix:+ i 2))
	   (j 0 (fix:+ j 1)))
	  ((not (fix:< i end)))
	(vector-8b-set! bytes j
			(fix:+ (fix:lsh (hex-digit (string-ref string i)) 4)
			       (hex-digit (string-ref string (fix:+ i 1))))))
      bytes)))

;;;; Case

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
  (guarantee-string string 'STRING-CAPITALIZE)
  (let ((string (string-copy string)))
    (%substring-capitalize! string 0 (string-length string))
    string))

(define (%substring-capitalize! string start end)
  ;; This algorithm capitalizes the first word in the substring and
  ;; downcases the subsequent words.  This is arbitrary, but seems
  ;; useful if the substring happens to be a sentence.  Again, if you
  ;; need finer control, parse the words yourself.
  (let ((index
	 (substring-find-next-char-in-set string start end
					  char-set:alphabetic)))
    (if index
	(begin
	  (%substring-upcase! string index (fix:+ index 1))
	  (%substring-downcase! string (fix:+ index 1) end)))))

(define (%substring-upcase! string start end)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (string-set! string i (char-upcase (string-ref string i)))))

(define (%substring-downcase! string start end)
  (do ((i start (fix:+ i 1)))
      ((fix:= i end))
    (string-set! string i (char-downcase (string-ref string i)))))

;;;; CamelCase support

(define (camel-case-string->lisp string)
  (call-with-input-string string
    (lambda (input)
      (call-with-output-string
	(lambda (output)
	  (let loop ((prev #f))
	    (let ((c (read-char input)))
	      (if (not (eof-object? c))
		  (begin
		    (if (and prev (char-upper-case? c))
			(write-char #\- output))
		    (write-char (char-downcase c) output)
		    (loop c))))))))))

(define (lisp-string->camel-case string #!optional upcase-initial?)
  (call-with-input-string string
    (lambda (input)
      (call-with-output-string
	(lambda (output)
	  (let loop
	      ((upcase?
		(if (default-object? upcase-initial?)
		    #t
		    upcase-initial?)))
	    (let ((c (read-char input)))
	      (if (not (eof-object? c))
		  (if (char-alphabetic? c)
		      (begin
			(write-char (if upcase? (char-upcase c) c) output)
			(loop #f))
		      (begin
			(if (or (char-numeric? c)
				(eq? c #\_))
			    (write-char c output))
			(loop #t)))))))))))

;;;; Replace

(define (string-replace string char1 char2)
  (guarantee-string string 'STRING-REPLACE)
  (guarantee-char char1 'STRING-REPLACE)
  (guarantee-char char2 'STRING-REPLACE)
  (let ((string (string-copy string)))
    (%substring-replace! string 0 (string-length string) char1 char2)
    string))

(define (substring-replace string start end char1 char2)
  (guarantee-substring string start end 'SUBSTRING-REPLACE)
  (guarantee-char char1 'SUBSTRING-REPLACE)
  (guarantee-char char2 'SUBSTRING-REPLACE)
  (let ((string (string-copy string)))
    (%substring-replace! string start end char1 char2)
    string))

(define (%substring-replace! string start end char1 char2)
  (let loop ((start start))
    (let ((index (substring-find-next-char string start end char1)))
      (if index
	  (begin
	    (string-set! string index char2)
	    (loop (fix:+ index 1)))))))

;;;; Compare

(define (string-compare string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'STRING-COMPARE)
  (%string-compare string1 string2 if= if< if>))

(define (%string-compare string1 string2 if= if< if>)
  (let ((length1 (string-length string1))
	(length2 (string-length string2)))
    (let ((end (fix:min length1 length2)))
      (let loop ((index 0))
	(cond ((fix:= index end)
	       (if (fix:= index length1)
		   (if (fix:= index length2)
		       (if=)
		       (if<))
		   (if>)))
	      ((char=? (string-ref string1 index)
		       (string-ref string2 index))
	       (loop (fix:+ index 1)))
	      ((char<? (string-ref string1 index)
		       (string-ref string2 index))
	       (if<))
	      (else
	       (if>)))))))

(define (string-compare-ci string1 string2 if= if< if>)
  (guarantee-2-strings string1 string2 'STRING-COMPARE-CI)
  (%string-compare-ci string1 string2 if= if< if>))

(define (%string-compare-ci string1 string2 if= if< if>)
  (let ((length1 (string-length string1))
	(length2 (string-length string2)))
    (let ((end (fix:min length1 length2)))
      (let loop ((index 0))
	(cond ((fix:= index end)
	       (if (fix:= index length1)
		   (if (fix:= index length2)
		       (if=)
		       (if<))
		   (if>)))
	      ((char-ci=? (string-ref string1 index)
			  (string-ref string2 index))
	       (loop (fix:+ index 1)))
	      ((char-ci<? (string-ref string1 index)
			  (string-ref string2 index))
	       (if<))
	      (else
	       (if>)))))))

(define (string-match-forward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD)
  (%substring-match-forward string1 0 (string-length string1)
			    string2 0 (string-length string2)))

(define (substring-match-forward string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-FORWARD)
  (%substring-match-forward string1 start1 end1 string2 start2 end2))

(define (%substring-match-forward string1 start1 end1 string2 start2 end2)
  (let ((end (fix:+ start1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (let loop ((i1 start1) (i2 start2))
      (if (or (fix:= i1 end)
	      (not (char=? (string-ref string1 i1)
			   (string-ref string2 i2))))
	  (fix:- i1 start1)
	  (loop (fix:+ i1 1) (fix:+ i2 1))))))

(define (string-match-forward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-FORWARD-CI)
  (%substring-match-forward-ci string1 0 (string-length string1)
			       string2 0 (string-length string2)))

(define (substring-match-forward-ci string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-FORWARD-CI)
  (%substring-match-forward-ci string1 start1 end1 string2 start2 end2))

(define (%substring-match-forward-ci string1 start1 end1 string2 start2 end2)
  (let ((end (fix:+ start1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (let loop ((i1 start1) (i2 start2))
      (if (or (fix:= i1 end)
	      (not (char-ci=? (string-ref string1 i1)
			      (string-ref string2 i2))))
	  (fix:- i1 start1)
	  (loop (fix:+ i1 1) (fix:+ i2 1))))))

(define (string-match-backward string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD)
  (%substring-match-backward string1 0 (string-length string1)
			     string2 0 (string-length string2)))

(define (substring-match-backward string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-BACKWARD)
  (%substring-match-backward string1 start1 end1 string2 start2 end2))

(define (%substring-match-backward string1 start1 end1 string2 start2 end2)
  (let ((start (fix:- end1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (if (fix:= end1 start)
	0
	(let loop ((i1 (fix:- end1 1)) (i2 (fix:- end2 1)))
	  (if (char=? (string-ref string1 i1) (string-ref string2 i2))
	      (if (fix:= i1 start)
		  (fix:- end1 i1)
		  (loop (fix:- i1 1) (fix:- i2 1)))
	      (fix:- end1 (fix:+ i1 1)))))))

(define (string-match-backward-ci string1 string2)
  (guarantee-2-strings string1 string2 'STRING-MATCH-BACKWARD-CI)
  (%substring-match-backward-ci string1 0 (string-length string1)
				string2 0 (string-length string2)))

(define (substring-match-backward-ci string1 start1 end1 string2 start2 end2)
  (guarantee-2-substrings string1 start1 end1
			  string2 start2 end2
			  'SUBSTRING-MATCH-BACKWARD-CI)
  (%substring-match-backward-ci string1 start1 end1 string2 start2 end2))

(define (%substring-match-backward-ci string1 start1 end1 string2 start2 end2)
  (let ((start (fix:- end1 (fix:min (fix:- end1 start1) (fix:- end2 start2)))))
    (if (fix:= end1 start)
	0
	(let loop ((i1 (fix:- end1 1)) (i2 (fix:- end2 1)))
	  (if (char-ci=? (string-ref string1 i1) (string-ref string2 i2))
	      (if (fix:= i1 start)
		  (fix:- end1 i1)
		  (loop (fix:- i1 1) (fix:- i2 1)))
	      (fix:- end1 (fix:+ i1 1)))))))

;;;; Trim

(define (string-trim-left string #!optional char-set)
  (let ((index
	 (string-find-next-char-in-set string
				       (if (default-object? char-set)
					   char-set:not-whitespace
					   char-set))))
    (if index
	(substring string index (string-length string))
	"")))

(define (string-trim-right string #!optional char-set)
  (let ((index
	 (string-find-previous-char-in-set string
					   (if (default-object? char-set)
					       char-set:not-whitespace
					       char-set))))
    (if index
	(substring string 0 (fix:+ index 1))
	"")))

(define (string-trim string #!optional char-set)
  (let* ((char-set
	 (if (default-object? char-set)
	     char-set:not-whitespace
	     char-set))
	 (index (string-find-next-char-in-set string char-set)))
    (if index
	(substring string
		   index
		   (fix:+ (string-find-previous-char-in-set string char-set)
			  1))
	"")))

;;;; Pad

(define (string-pad-right string n #!optional char)
  (guarantee-string string 'STRING-PAD-RIGHT)
  (guarantee-string-index n 'STRING-PAD-RIGHT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n)))
	  (if (fix:> length n)
	      (string-copy! result 0 string 0 n)
	      (begin
		(string-copy! result 0 string 0 length)
		(string-fill! result
			      (if (default-object? char)
				  #\space
				  (begin
				    (guarantee-char char 'STRING-PAD-RIGHT)
				    char))
			      length
			      n)))
	  result))))

(define (string-pad-left string n #!optional char)
  (guarantee-string string 'STRING-PAD-LEFT)
  (guarantee-string-index n 'STRING-PAD-LEFT)
  (let ((length (string-length string)))
    (if (fix:= length n)
	string
	(let ((result (string-allocate n))
	      (i (fix:- n length)))
	  (if (fix:< i 0)
	      (string-copy! result 0 string (fix:- 0 i) length)
	      (begin
		(string-fill! result
			      (if (default-object? char)
				  #\space
				  (begin
				    (guarantee-char char 'STRING-PAD-RIGHT)
				    char))
			      0
			      i)
		(string-copy! result i string 0 length)))
	  result))))

;;;; String search

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
  (%substring-search-all text 0 (string-length text)
			 pattern 0 (string-length pattern)))

(define (substring-search-all pattern text tstart tend)
  (guarantee-string pattern 'SUBSTRING-SEARCH-ALL)
  (guarantee-substring text tstart tend 'SUBSTRING-SEARCH-ALL)
  (%substring-search-all text tstart tend
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

(define (%substring-search-all text tstart tend pattern pstart pend)
  (let ((plen (fix:- pend pstart)))
    (cond ((fix:= plen 1)
	   (let ((c (string-ref pattern pstart)))
	     (let loop ((ti tend) (occurrences '()))
	       (let ((index (substring-find-previous-char text tstart ti c)))
		 (if index
		     (loop index (cons index occurrences))
		     occurrences)))))
	  #;    ;This may not be worthwhile -- I have no measurements.
	  ((fix:< plen 4)
	   (let loop ((ti tend) (occurrences '()))
	     (let ((index
		    (%dumb-substring-search-backward text tstart ti
						     pattern pstart pend)))
	       (if index
		   (loop (fix:+ index (fix:- plen 1)) (cons index occurrences))
		   occurrences))))
	  (else
	   (%bm-substring-search-all text tstart tend pattern pstart pend)))))

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
;; meaningful message.  Structuring the code this way significantly
;; reduces code bloat from large integrated procedures.

(declare (integrate-operator guarantee-string))
(define-guarantee string "string")

(define-integrable (guarantee-2-strings object1 object2 procedure)
  (if (not (and (string? object1) (string? object2)))
      (guarantee-2-strings/fail object1 object2 procedure)))

(define (guarantee-2-strings/fail object1 object2 procedure)
  (cond ((not (string? object1))
	 (error:wrong-type-argument object1 "string" procedure))
	((not (string? object2))
	 (error:wrong-type-argument object2 "string" procedure))))

(define-integrable (guarantee-string-index object caller)
  (if (not (index-fixnum? object))
      (error:wrong-type-argument object "string index" caller)))

(define-integrable (guarantee-substring string start end caller)
  (if (not (and (string? string)
		(index-fixnum? start)
		(index-fixnum? end)
		(fix:<= start end)
		(fix:<= end (string-length string))))
      (guarantee-substring/fail string start end caller)))

(define (guarantee-substring/fail string start end caller)
  (guarantee-string string caller)
  (guarantee-substring-end-index end (string-length string) caller)
  (guarantee-substring-start-index start end caller))

(define-integrable (guarantee-substring-end-index end length caller)
  (guarantee-string-index end caller)
  (if (not (fix:<= end length))
      (error:bad-range-argument end caller))
  end)

(define-integrable (guarantee-substring-start-index start end caller)
  (guarantee-string-index start caller)
  (if (not (fix:<= start end))
      (error:bad-range-argument start caller))
  start)

(define-integrable (guarantee-2-substrings string1 start1 end1
					   string2 start2 end2
					   procedure)
  (guarantee-substring string1 start1 end1 procedure)
  (guarantee-substring string2 start2 end2 procedure))