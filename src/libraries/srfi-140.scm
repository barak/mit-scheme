#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018 Massachusetts Institute of Technology

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

;;;; SRFI 140 Strings

(define-library (srfi 140)
  (import (scheme base)
	  (scheme char)
	  (only (srfi 1)
		drop-right
		last)
	  (srfi 143)
	  (only (mit legacy runtime)
		char->string
		default-object?
		error:bad-range-argument
		fix:end-index
		fix:start-index
		guarantee
		istring?
		non-negative-fixnum?
		string->immutable
		string->utf16
		string->utf16be
		string->utf16le
		string-concatenate
		string-builder
		string-fold
		string-fold-right
		string-joiner*
		string-null?
		string-padder
		string-search-backward
		string-search-forward
		string-slice
		string-titlecase
		string-trimmer
		unspecific
		utf16->string
		utf16be->string
		utf16le->string))
  (export istring?
	  list->string
	  ;make-string
	  ;mstring?
	  reverse-list->string
	  string
	  string->list
	  string->utf16
	  string->utf16be
	  string->utf16le
	  string->utf8
	  string->vector
	  string-any
	  string-append
	  ;string-append!
	  string-ci<=?
	  string-ci<?
	  string-ci=?
	  string-ci>=?
	  string-ci>?
	  string-concatenate
	  string-concatenate-reverse
	  string-contains
	  string-contains-right
	  ;string-copy
	  ;string-copy!
	  string-count
	  string-downcase
	  string-drop
	  string-drop-right
	  string-every
	  ;string-fill!
	  string-filter
	  string-fold
	  string-fold-right
	  string-foldcase
	  string-for-each
	  string-for-each-index
	  string-index
	  string-index-right
	  string-join
	  string-length
	  string-map
	  string-map-index
	  string-null?
	  string-pad
	  string-pad-right
	  string-prefix-length
	  string-prefix?
	  string-ref
	  string-remove
	  string-repeat
	  string-replace
	  ;string-replace!
	  ;string-set!
	  string-skip
	  string-skip-right
	  string-split
	  string-suffix-length
	  string-suffix?
	  string-tabulate
	  string-take
	  string-take-right
	  string-titlecase
	  string-trim
	  string-trim-both
	  string-trim-right
	  string-unfold
	  string-unfold-right
	  string-upcase
	  string<=?
	  string<?
	  string=?
	  string>=?
	  string>?
	  string?
	  substring
	  utf16->string
	  utf16be->string
	  utf16le->string
	  utf8->string
	  vector->string
	  xsubstring)
  (begin

(define (string-every pred string #!optional start end)
  (let ((end (fix:end-index end (string-length string) 'string-every)))
    (let loop ((index (fix:start-index start end 'string-every)) (value #t))
      (if (fx<? index end)
	  (let ((value (pred (string-ref string index))))
	    (and value
		 (loop (fx+ index 1) value)))
	  value))))

(define (string-any pred string #!optional start end)
  (let ((end (fix:end-index end (string-length string) 'string-any)))
    (let loop ((index (fix:start-index start end 'string-any)))
      (and (fx<? index end)
	   (or (pred (string-ref string index))
	       (loop (fx+ index 1)))))))

(define (string . chars)
  (list->string chars))

(define (reverse-list->string chars)
  (list->string (reverse chars)))

(define (substring string #!optional start end)
  (if (istring? string)
      (string-slice string start end)
      (string->immutable (string-slice string start end))))

(define (string-take string nchars)
  (substring string 0 nchars))

(define (string-drop string nchars)
  (substring string nchars (string-length string)))

(define (string-take-right string nchars)
  (let ((n (string-length string)))
    (substring string (fx- n nchars) n)))

(define (string-drop-right string nchars)
  (substring string 0 (fx- (string-length string) nchars)))

(define (string-tabulate proc n)
  (let ((builder (string-builder n)))
    (do ((i 0 (fx+ i 1)))
	((not (fx<? i n)) (builder))
      (builder (proc i)))))

;; TODO: move this into string.scm and make it fast.
(define (reverse-string-builder #!optional initial-buffer-length)
  (let ((elts '()))
    (lambda (#!optional object)
      (if (or (char? object) (string? object))
	  (begin
	    (set! elts (cons object elts))
	    unspecific)
	  (let ((builder (string-builder initial-buffer-length)))
	    (for-each builder elts)
	    (builder object))))))

(define (unfolder make-builder)
  (lambda (stop? mapper successor seed #!optional base make-final)
    (let ((builder (make-builder)))
      (if (not (default-object? base))
	  (builder base))
      (let loop ((seed seed))
	(cond ((not (stop? seed))
	       (builder (mapper seed))
	       (loop (successor seed)))
	      ((not (default-object? make-final))
	       (builder (make-final seed)))))
      (builder))))

(define string-unfold
  (unfolder string-builder))

(define string-unfold-right
  (unfolder reverse-string-builder))

(define (padder where)
  (lambda (string len #!optional char start end)
    ((string-padder 'where where
		    'fill-with (if (default-object? char)
				   " "
				   (char->string char))
		    'clip? #t)
     (string-slice string start end)
     len)))

(define string-pad (padder 'leading))
(define string-pad-right (padder 'trailing))

(define (trimmer where)
  (lambda (string #!optional pred start end)
    ((string-trimmer 'where where
		     'to-trim (if (default-object? pred) char-whitespace? pred)
		     'copier substring)
     (string-slice string start end))))

(define string-trim (trimmer 'leading))
(define string-trim-right (trimmer 'trailing))
(define string-trim-both (trimmer 'both))

;; TODO: don't need a full string-builder here, could be faster.
(define (string-replace string1 string2 start1 end1 #!optional start2 end2)
  (let ((len1 (string-length string1))
	(len2 (string-length string2)))
    (let ((end1 (fix:end-index end1 len1 'string-replace))
	  (end2 (fix:end-index end2 len2 'string-replace)))
      (let ((start1 (fix:start-index start1 end1 'string-replace))
	    (start2 (fix:start-index start2 end2 'string-replace)))
	(let ((builder
	       (string-builder (fx+ (fx- len1 (fx- end1 start1))
				    (fx- end2 start2)))))
	  (builder (string-slice string1 0 start1))
	  (builder (string-slice string2 start2 end2))
	  (builder (string-slice string1 end1 len1))
	  (builder))))))

(define (string-prefix-length string1 string2
			      #!optional start1 end1 start2 end2)
  (let ((end1
	 (fix:end-index end1 (string-length string1) 'string-prefix-length))
	(end2
	 (fix:end-index end2 (string-length string2) 'string-prefix-length)))
    (%string-prefix-length string1
			   (fix:start-index start1 end1 'string-prefix-length)
			   end1
			   string2
			   (fix:start-index start2 end2 'string-prefix-length)
			   end2)))

(define (%string-prefix-length string1 start1 end1 string2 start2 end2)
  (let loop ((i start1) (j start2))
    (if (and (fx<? i end1)
	     (fx<? j end2)
	     (char=? (string-ref string1 i) (string-ref string2 j)))
	(loop (fx+ i 1) (fx+ j 1))
	(fx- i start1))))

(define (string-suffix-length string1 string2
			      #!optional start1 end1 start2 end2)
  (let ((end1
	 (fix:end-index end1 (string-length string1) 'string-suffix-length))
	(end2
	 (fix:end-index end2 (string-length string2) 'string-suffix-length)))
    (%string-suffix-length string1
			   (fix:start-index start1 end1 'string-suffix-length)
			   end1
			   string2
			   (fix:start-index start2 end2 'string-suffix-length)
			   end2)))

(define (%string-suffix-length string1 start1 end1 string2 start2 end2)
  (let loop ((i (fx- end1 1)) (j (fx- end2 1)))
    (if (and (fx>=? i start1)
	     (fx>=? j start2)
	     (char=? (string-ref string1 i) (string-ref string2 j)))
	(loop (fx- i 1) (fx- j 1))
	(fx- (fx- end1 1) i))))

(define (string-prefix? string1 string2 #!optional start1 end1 start2 end2)
  (let ((end1 (fix:end-index end1 (string-length string1) 'string-prefix?))
	(end2 (fix:end-index end2 (string-length string2) 'string-prefix?)))
    (%string-prefix? string1
		     (fix:start-index start1 end1 'string-prefix?)
		     end1
		     string2
		     (fix:start-index start2 end2 'string-prefix?)
		     end2)))

(define (%string-prefix? string1 start1 end1 string2 start2 end2)
  (let loop ((i start1) (j start2))
    (if (fx<? i end1)
	(and (fx<? j end2)
	     (char=? (string-ref string1 i) (string-ref string2 j))
	     (loop (fx+ i 1) (fx+ j 1)))
	#t)))

(define (string-suffix? string1 string2 #!optional start1 end1 start2 end2)
  (let ((end1 (fix:end-index end1 (string-length string1) 'string-suffix?))
	(end2 (fix:end-index end2 (string-length string2) 'string-suffix?)))
    (%string-suffix? string1
		     (fix:start-index start1 end1 'string-suffix?)
		     end1
		     string2
		     (fix:start-index start2 end2 'string-suffix?)
		     end2)))

(define (%string-suffix? string1 start1 end1 string2 start2 end2)
  (let loop ((i (fx- end1 1)) (j (fx- end2 1)))
    (if (fx>=? i start1)
	(and (fx>=? j start2)
	     (char=? (string-ref string1 i) (string-ref string2 j))
	     (loop (fx- i 1) (fx- j 1)))
	#t)))

(define (string-index string pred #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-index))
	 (start (fix:start-index start end 'string-index)))
    (let loop ((i start))
      (and (fx<? i end)
	   (if (pred (string-ref string i))
	       i
	       (loop (fx+ i 1)))))))

(define (string-index-right string pred #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-index-right))
	 (start (fix:start-index start end 'string-index-right)))
    (let loop ((i (fx- end 1)))
      (and (fx>=? i start)
	   (if (pred (string-ref string i))
	       i
	       (loop (fx- i 1)))))))

(define (string-skip string pred #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-skip))
	 (start (fix:start-index start end 'string-skip)))
    (let loop ((i start))
      (and (fx<? i end)
	   (if (pred (string-ref string i))
	       (loop (fx+ i 1))
	       i)))))

(define (string-skip-right string pred #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-skip-right))
	 (start (fix:start-index start end 'string-skip-right)))
    (let loop ((i (fx- end 1)))
      (and (fx>=? i start)
	   (if (pred (string-ref string i))
	       (loop (fx- i 1))
	       i)))))

(define (string-contains string1 string2 #!optional start1 end1 start2 end2)
  (let* ((pattern (string-slice string2 start2 end2))
	 (end1 (fix:end-index end1 (string-length string1) 'string-contains))
	 (start1 (fix:start-index start1 end1 'string-contains)))
    (if (string-null? pattern)
	start1
	(string-search-forward pattern string1 start1 end1))))

(define (string-contains-right string1 string2
			       #!optional start1 end1 start2 end2)
  (let* ((pattern (string-slice string2 start2 end2))
	 (end1
	  (fix:end-index end1 (string-length string1) 'string-contains-right))
	 (start1 (fix:start-index start1 end1 'string-contains-right)))
    (if (string-null? pattern)
	end1
	(string-search-backward pattern string1 start1 end1))))

;; TODO: make this faster.
(define (string-concatenate-reverse strings #!optional final-string end)
  (string-concatenate
   (reverse
    (if (default-object? final-string)
	strings
	(cons (string-slice final-string 0 end)
	      strings)))))

(define (string-join strings #!optional delimiter grammar)
  (let ((delimiter (if (default-object? delimiter) " " delimiter)))
    (case grammar
      ((#!default infix)
       ((string-joiner* 'infix delimiter) strings))
      ((strict-infix)
       (if (null? strings)
	   (error:bad-range-argument strings 'string-join))
       ((string-joiner* 'infix delimiter) strings))
      ((suffix)
       ((string-joiner* 'suffix delimiter) strings))
      ((prefix)
       ((string-joiner* 'prefix delimiter) strings))
      (else
       (error:bad-range-argument grammar 'string-join)))))

(define (string-map-index proc string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-map-index))
	 (start (fix:start-index start end 'string-map-index)))
    (let ((builder (string-builder)))
      (do ((index start (fx+ index 1)))
	  ((not (fx<? index end)))
	(builder (proc index)))
      (builder))))

(define (string-for-each-index proc string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-for-each-index))
	 (start (fix:start-index start end 'string-for-each-index)))
    (do ((index start (fx+ index 1)))
	((not (fx<? index end)))
      (proc index))
    unspecific))

(define (string-count string pred #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-count))
	 (start (fix:start-index start end 'string-count)))
    (do ((index start (fx+ index 1))
	 (count 0
		(if (pred (string-ref string index))
		    (fx+ count 1)
		    count)))
	((not (fx<? index end)) count))))

(define (string-filter pred string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-filter))
	 (start (fix:start-index start end 'string-filter)))
    (let ((builder (string-builder)))
      (do ((index start (fx+ index 1)))
	  ((not (fx<? index end)))
	(if (pred (string-ref string index))
	    (builder (string-ref string index))))
      (builder))))

(define (string-remove pred string #!optional start end)
  (let* ((end (fix:end-index end (string-length string) 'string-remove))
	 (start (fix:start-index start end 'string-remove)))
    (let ((builder (string-builder)))
      (do ((index start (fx+ index 1)))
	  ((not (fx<? index end)))
	(if (not (pred (string-ref string index)))
	    (builder (string-ref string index))))
      (builder))))

(define (string-repeat kernel n)
  (guarantee non-negative-fixnum? n 'string-repeat)
  (let ((builder (string-builder)))
    (do ((i 0 (fx+ i 1)))
	((not (fx<? i n)))
      (builder kernel))
    (builder)))

(define (xsubstring string #!optional from to start end)
  (let* ((end (fix:end-index end (string-length string) 'xsubstring))
	 (start (fix:start-index start end 'xsubstring))
	 (n (fx- end start))
	 (from
	  (if (default-object? from)
	      0
	      (guarantee exact-integer? from 'xsubstring)))
	 (to
	  (if (default-object? to)
	      (+ from n)
	      (guarantee exact-integer? to 'xsubstring))))
    (if (= from to)
	""
	(begin
	  (if (not (< from to))
	      (error:bad-range-argument from 'xsubstring))
	  (if (fx=? start end)
	      (error:bad-range-argument start 'xsubstring))
	  (let ((builder (string-builder)))
	    (do ((i from (+ i 1)))
		((not (< i to)))
	      (builder (string-ref string (+ start (modulo i n)))))
	    (builder))))))

(define (string-split string delimiter #!optional grammar limit start end)
  (let* ((end (fix:end-index end (string-length string) 'string-split))
	 (start (fix:start-index start end 'string-split))
	 (dn (string-length delimiter))
	 (limit
	  (if (or (default-object? limit) (not limit))
	      #f
	      (guarantee non-negative-fixnum? limit))))

    (define (do-split)
      (case dn
	((0) (do-split-0))
	((1) (do-split-1 (string-ref delimiter 0)))
	(else (do-split-n))))

    (define (do-split-0)

      (define (without-limit index)
	(if (fx<? index end)
	    (cons (substring string index (fx+ index 1))
		  (without-limit (fx+ index 1)))
	    '()))

      (define (with-limit index limit)
	(if (fx<? index end)
	    (if (fx>? limit 0)
		(cons (substring string index (fx+ index 1))
		      (with-limit (fx+ index 1) (fx- limit 1)))
		(list (substring string index end)))
	    '()))

      (if limit
	  (with-limit start limit)
	  (without-limit start)))

    (define (do-split-1 char)

      (define (without-limit index)
	(let ((match (find-match index)))
	  (if match
	      (cons (substring string index match)
		    (without-limit (fx+ match dn)))
	      (list (substring string index end)))))

      (define (with-limit index limit)
	(let ((match (and (fx>? limit 0) (find-match index))))
	  (if match
	      (cons (substring string index match)
		    (with-limit (fx+ match dn) (fx- limit 1)))
	      (list (substring string index end)))))

      (define (find-match index)
	(and (fx<? index end)
	     (if (char=? char (string-ref string index))
		 index
		 (find-match (fx+ index 1)))))

      (if limit
	  (with-limit start limit)
	  (without-limit start)))

    (define (do-split-n)

      (define (without-limit index)
	(let ((match (find-match index)))
	  (if match
	      (cons (substring string index match)
		    (without-limit (fx+ match dn)))
	      (list (substring string index end)))))

      (define (with-limit index limit)
	(let ((match (and (fx>? limit 0) (find-match index))))
	  (if match
	      (cons (substring string index match)
		    (with-limit (fx+ match dn) (fx- limit 1)))
	      (list (substring string index end)))))

      (define (find-match index)
	(and (fx<? index end)
	     (if (%string-prefix? delimiter 0 dn string index end)
		 index
		 (find-match (fx+ index 1)))))

      (if limit
	  (with-limit start limit)
	  (without-limit start)))

    (case grammar
      ((#!default infix)
       (do-split))
      ((strict-infix)
       (if (fx=? start end)
	   (error:bad-range-argument string 'string-split))
       (do-split))
      ((prefix)
       (let ((result (do-split)))
	 (if (and (pair? result)
		  (string-null? (car result)))
	     (cdr result)
	     result)))
      ((suffix)
       (let ((result (do-split)))
	 (if (and (pair? result)
		  (string-null? (last result)))
	     (drop-right result 1)
	     result)))
      (else
       (error:bad-range-argument grammar 'string-split)))))

;; end of library
))