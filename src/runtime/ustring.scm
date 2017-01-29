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

;;;; Unicode strings
;;; package: (runtime ustring)

;;; This implementation supports all R7RS and some MIT/GNU string operations in
;;; which all the names have "string" replaced by "ustring".  This is a
;;; transitional implementation to convert MIT/GNU Scheme to full Unicode string
;;; support.
;;;
;;; For simplicity, the implementation uses the UTF-32 encoding for non-ASCII
;;; strings.  This is not a good long-term approach and should be revisited once
;;; the runtime system has been converted to this string abstraction.
;;;
;;; At some point in the future we'll eliminate legacy string support and rename
;;; everything to "string".

(declare (usual-integrations))

;;;; Utilities

(define-integrable (x-copy-maker from-length from-ref make-to to-set! caller)
  (lambda (from #!optional start end)
    (let* ((end (fix:end-index end (from-length from) caller))
	   (start (fix:start-index start end caller))
	   (to (make-to (fix:- end start))))
      (copy-loop to-set! to 0
		 from-ref from start end)
      to)))

(define-integrable (x-copy!-maker from-length from-ref to-set! caller)
  (lambda (to at from #!optional start end)
    (let* ((end (fix:end-index end (from-length from) caller))
	   (start (fix:start-index start end caller)))
      (copy-loop to-set! to at
		 from-ref from start end))))

(define-integrable (copy-loop to-set! to at
			      from-ref from start end)
  (do ((i start (fix:+ i 1))
       (j at (fix:+ j 1)))
      ((not (fix:< i end)))
    (to-set! to j (from-ref from i))))

(define-integrable (every-loop proc ref string start end)
  (let loop ((i start))
    (if (fix:< i end)
	(and (proc (ref string i))
	     (loop (fix:+ i 1)))
	#t)))

(define (min-length string-length string strings)
  (do ((strings strings (cdr strings))
       (n (string-length string)
	  (fix:min n (string-length (car strings)))))
      ((null? strings) n)))

;;;; U32 vectors

(define-integrable (u32->byte-index index)
  (fix:* index 4))

(define-integrable (byte->u32-index index)
  (fix:quotient index 4))

(define (make-u32-vector length)
  (make-bytevector (u32->byte-index length)))

(define (u32-vector-length bytes)
  (byte->u32-index (bytevector-length bytes)))

(define (u32-vector-ref bytes index)
  (bytevector-u32be-ref bytes (u32->byte-index index)))

(define (u32-vector-set! bytes index u32)
  (bytevector-u32be-set! bytes (u32->byte-index index) u32))

(define (u32-vector-copy! to at from start end)
  (bytevector-copy! to (u32->byte-index at)
		    from (u32->byte-index start) (u32->byte-index end)))

(define (u32-vector-fill! bytes start end u32)
  (do ((i start (fix:+ i 1)))
      ((not (fix:< i end)))
    (u32-vector-set! bytes i u32)))

;;;; UTF-32 strings

(define (make-utf32-string k #!optional char)
  (let ((v (make-u32-vector k)))
    (if (not (default-object? char))
	(u32-vector-fill! v 0 k (char->integer char)))
    (%record %utf32-string-tag v)))

(define (utf32-string? object)
  (and (%record? object)
       (fix:= 2 (%record-length object))
       (eq? %utf32-string-tag (%record-ref object 0))))

(define %utf32-string-tag
  '|#[(runtime ustring)utf32-string]|)

(define (utf32-string-vector string caller)
  (guarantee utf32-string? string caller)
  (%record-ref string 1))

(define-integrable (utf32-end-index end string caller)
  (fix:end-index end (utf32-string-length string) caller))

(define (utf32-string-length string)
  (u32-vector-length (utf32-string-vector string 'utf32-string-length)))

(define (utf32-string-ref string index)
  (integer->char
   (u32-vector-ref (utf32-string-vector string 'utf32-string-ref) index)))

(define (utf32-string-set! string index char)
  (u32-vector-set! (utf32-string-vector string 'utf32-string-set!)
		   index
		   (char->integer char)))

(define (utf32-string-copy string #!optional start end)
  (let* ((end (utf32-end-index end string 'utf32-string-copy))
	 (start (fix:start-index start end 'utf32-string-copy)))
    (%utf32-string-copy string start end)))

(define (%utf32-string-copy string start end)
  (let ((to (make-utf32-string (fix:- end start))))
    (%utf32-string-copy! to 0 string start end utf32-string-copy)
    to))

(define (utf32-string-copy! to at from #!optional start end)
  (let* ((end (utf32-end-index end from 'utf32-string-copy!))
	 (start (fix:start-index start end 'utf32-string-copy!)))
    (%utf32-string-copy! to at from start end 'utf32-string-copy!)))

(define-integrable (%utf32-string-copy! to at from start end caller)
  (u32-vector-copy! (utf32-string-vector to caller) at
		    (utf32-string-vector from caller) start end))

(define (utf32-string-fill! string char #!optional start end)
  (let* ((end (utf32-end-index end string 'utf32-string-fill!))
	 (start (fix:start-index start end 'utf32-string-fill!)))
    (u32-vector-fill! (utf32-string-vector string 'utf32-string-fill!)
		      start
		      end
		      (char->integer char))))

(define (utf32-string->list string #!optional start end)
  (let* ((end (utf32-end-index end string 'utf32-string->list))
	 (start (fix:start-index start end 'utf32-string->list)))
    (do ((i (fix:- end 1) (fix:- i 1))
	 (chars '() (cons (utf32-string-ref string i) chars)))
	((not (fix:>= i start)) chars))))

(define utf32-string->vector
  (x-copy-maker utf32-string-length utf32-string-ref make-vector vector-set!
		'utf32-string->vector))

(define (utf32-string-find-first-index proc string #!optional start end)
  (let* ((caller 'utf32-string-find-next-index)
	 (end (utf32-end-index end string caller))
	 (start (fix:start-index start end caller)))
    (let loop ((i start))
      (and (fix:< i end)
	   (if (proc (utf32-string-ref string i))
	       i
	       (loop (fix:+ i 1)))))))

(define (utf32-string-find-last-index proc string #!optional start end)
  (let* ((caller 'utf32-string-find-last-index)
	 (end (utf32-end-index end string caller))
	 (start (fix:start-index start end caller)))
    (let loop ((i (fix:- end 1)))
      (and (fix:>= i start)
	   (if (proc (utf32-string-ref string i))
	       i
	       (loop (fix:- i 1)))))))

(define (utf32-string-map proc string . strings)
  (if (null? strings)
      (let* ((n (utf32-string-length string))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i (proc (utf32-string-ref string i))))
	result)
      (let* ((n (min-length utf32-string-length string strings))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i
			     (apply proc
				    (utf32-string-ref string i)
				    (map (lambda (string)
					   (utf32-string-ref string i))
					 strings))))
	result)))

(define (utf32-string-for-each procedure string . strings)
  (if (null? strings)
      (let ((n (utf32-string-length string)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (procedure (utf32-string-ref string i))))
      (let ((n (min-length utf32-string-length string strings)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (apply procedure
		 (utf32-string-ref string i)
		 (map (lambda (string)
			(utf32-string-ref string i))
		      strings))))))

;; Incorrect implementation
(define (utf32-string-upcase string)
  (utf32-string-map char-upcase string))

;; Incorrect implementation
(define (utf32-string-downcase string)
  (utf32-string-map char-downcase string))

;; Random and probably incorrect.
(define (utf32-string-capitalize string)
  (let ((index (utf32-string-find-first-index char-alphabetic? string))
	(string (utf32-string-copy string)))
    (if index
	(utf32-string-set! string
			   index
			   (char-upcase (utf32-string-ref string index))))
    string))

;;;; String

(define (ustring? object)
  (or (legacy-string? object)
      (utf32-string? object)))

(define (register-ustring-predicates!)
  (register-predicate! utf32-string? 'utf32-string)
  (register-predicate! ustring? 'ustring)
  (set-predicate<=! legacy-string? ustring?)
  (set-predicate<=! utf32-string? ustring?)
  (register-predicate! ->ustring-component? '->ustring-component))

(define (make-ustring k #!optional char)
  (guarantee index-fixnum? k 'make-ustring)
  (if (fix:> k 0)
      (make-utf32-string k char)
      (make-legacy-string 0)))

(define (ustring-length string)
  (cond ((legacy-string? string) (legacy-string-length string))
	((utf32-string? string) (utf32-string-length string))
	(else (error:not-a ustring? string 'ustring-length))))

(define (ustring-ref string index)
  (cond ((legacy-string? string) (legacy-string-ref string index))
	((utf32-string? string) (utf32-string-ref string index))
	(else (error:not-a ustring? string 'ustring-ref))))

(define (ustring-set! string index char)
  (cond ((legacy-string? string) (legacy-string-set! string index char))
	((utf32-string? string) (utf32-string-set! string index char))
	(else (error:not-a ustring? string 'ustring-set!))))

(define (ustring-append . strings)
  (%ustring-append* strings))

(define (ustring-append* strings)
  (guarantee list? strings 'ustring-append*)
  (%ustring-append* strings))

(define (%ustring-append* strings)
  (let ((string
	 (do ((strings strings (cdr strings))
	      (n 0 (fix:+ n (ustring-length (car strings))))
	      (ascii? #t (and ascii? (ustring-ascii? (car strings)))))
	     ((not (pair? strings))
	      (if ascii?
		  (make-legacy-string n)
		  (make-utf32-string n))))))
    (let loop ((strings strings) (i 0))
      (if (pair? strings)
	  (let ((n (ustring-length (car strings))))
	    (ustring-copy! string i (car strings) 0 n)
	    (loop (cdr strings) (fix:+ i n)))))
    string))

(define (list->ustring chars)
  (let ((string
	 (let ((n (length chars)))
	   (if (every char-ascii? chars)
	       (make-legacy-string n)
	       (make-utf32-string n)))))
    (do ((chars chars (cdr chars))
	 (i 0 (fix:+ i 1)))
	((not (pair? chars)))
      (ustring-set! string i (car chars)))
    string))

(define (ustring-ascii? string)
  (cond ((legacy-string? string) (legacy-string-ascii? string))
	((utf32-string? string) (utf32-string-ascii? string))
	(else (error:not-a ustring? string 'ustring-ascii?))))

(define (legacy-string-ascii? string)
  (%legacy-string-ascii? string 0 (legacy-string-length string)))

(define (%legacy-string-ascii? string start end)
  (every-loop char-ascii? legacy-string-ref string start end))

(define (ustring->ascii string)
  (cond ((legacy-string? string)
	 (and (legacy-string-ascii? string)
	      string))
	((utf32-string? string)
	 (and (utf32-string-ascii? string)
	      (utf32-string->ascii string)))
	(else
	 (error:not-a ustring? string 'ustring->ascii))))

(define (utf32-string-ascii? string)
  (%utf32-string-ascii? string 0 (utf32-string-length string)))

(define (%utf32-string-ascii? string start end)
  (every-loop char-ascii? utf32-string-ref string start end))

(define (utf32-string->ascii string)
  (%utf32-string->ascii string 0 (utf32-string-length string)))

(define (%utf32-string->ascii string start end)
  (let ((to (make-legacy-string (fix:- end start))))
    (copy-loop legacy-string-set! to 0
	       utf32-string-ref string start end)
    to))

(define (ustring-copy string #!optional start end)
  (let* ((end (fix:end-index end (ustring-length string) 'ustring-copy))
	 (start (fix:start-index start end 'ustring-copy)))
    (cond ((legacy-string? string)
	   (if (%legacy-string-ascii? string start end)
	       (legacy-string-copy string start end)
	       (let ((result (make-utf32-string (fix:- end start))))
		 (legacy->utf32-copy! result 0 string start end)
		 result)))
	  ((utf32-string? string)
	   (if (%utf32-string-ascii? string start end)
	       (%utf32-string->ascii string start end)
	       (%utf32-string-copy string start end)))
	  (else
	   (error:not-a ustring? string 'ustring-copy)))))

(define (ustring-copy! to at from #!optional start end)
  (cond ((legacy-string? to)
	 (cond ((legacy-string? from)
		(legacy-string-copy! to at from start end))
	       ((utf32-string? from)
		(utf32->legacy-copy! to at from start end))
	       (else
		(error:not-a ustring? from 'ustring-copy!))))
	((utf32-string? to)
	 (cond ((legacy-string? from)
		(legacy->utf32-copy! to at from start end))
	       ((utf32-string? from)
		(utf32-string-copy! to at from start end))
	       (else
		(error:not-a ustring? from 'ustring-copy!))))
	(else
	 (error:not-a ustring? to 'ustring-copy!))))

(define utf32->legacy-copy!
  (x-copy!-maker utf32-string-length utf32-string-ref legacy-string-set!
		 'ustring-copy!))

(define legacy->utf32-copy!
  (x-copy!-maker legacy-string-length legacy-string-ref utf32-string-set!
		 'legacy->utf32-copy!))

(define (ustring-fill! string char #!optional start end)
  (cond ((legacy-string? string) (legacy-string-fill! string char start end))
	((utf32-string? string) (utf32-string-fill! string char start end))
	(else (error:not-a ustring? string 'ustring-fill!))))

(define (%ustring=? string1 string2)
  (and (fix:= (ustring-length string1) (ustring-length string2))
       (ustring-every char=? string1 string2)))

(define (%ustring-ci=? string1 string2)
  (and (fix:= (ustring-length string1) (ustring-length string2))
       (ustring-every char-ci=? string1 string2)))

;; Incorrect implementation.
(define-integrable (%string-comparison-maker c= c<)
  (lambda (string1 string2)
    (let ((end1 (ustring-length string1))
	  (end2 (ustring-length string2)))
      (let ((end (fix:min end1 end2)))
	(let loop ((i 0))
	  (if (fix:< i end)
	      (let ((c1 (ustring-ref string1 i))
		    (c2 (ustring-ref string2 i)))
		(if (c= c1 c2)
		    (loop (fix:+ i 1))
		    (c< c1 c2)))
	      (fix:< end1 end2)))))))

(define %ustring<? (%string-comparison-maker char=? char<?))
(define %ustring<=? (%string-comparison-maker char=? char<=?))
(define %ustring>? (%string-comparison-maker char=? char>?))
(define %ustring>=? (%string-comparison-maker char=? char>=?))

(define %ustring-ci<? (%string-comparison-maker char-ci=? char-ci<?))
(define %ustring-ci<=? (%string-comparison-maker char-ci=? char-ci<=?))
(define %ustring-ci>? (%string-comparison-maker char-ci=? char-ci>?))
(define %ustring-ci>=? (%string-comparison-maker char-ci=? char-ci>=?))

(define-integrable (string-comparison-maker %compare)
  (lambda (string1 string2 . strings)
    (let loop ((string1 string1) (string2 string2) (strings strings))
      (if (pair? strings)
	  (and (%compare string1 string2)
	       (loop string2 (car strings) (cdr strings)))
	  (%compare string1 string2)))))

(define ustring=? (string-comparison-maker %ustring=?))
(define ustring<? (string-comparison-maker %ustring<?))
(define ustring<=? (string-comparison-maker %ustring<=?))
(define ustring>? (string-comparison-maker %ustring>?))
(define ustring>=? (string-comparison-maker %ustring>=?))

(define ustring-ci=? (string-comparison-maker %ustring-ci=?))
(define ustring-ci<? (string-comparison-maker %ustring-ci<?))
(define ustring-ci<=? (string-comparison-maker %ustring-ci<=?))
(define ustring-ci>? (string-comparison-maker %ustring-ci>?))
(define ustring-ci>=? (string-comparison-maker %ustring-ci>=?))

(define (ustring-prefix? prefix string)
  (let ((n (ustring-length prefix)))
    (and (fix:<= n (ustring-length string))
	 (let loop ((i 0))
	   (if (fix:< i n)
	       (and (eq? (ustring-ref prefix i) (ustring-ref string i))
		    (loop (fix:+ i 1)))
	       #t)))))

(define (ustring-suffix? suffix string)
  (let ((n (ustring-length suffix)))
    (and (fix:<= n (ustring-length string))
	 (let loop ((i (fix:- n 1)))
	   (if (fix:>= i 0)
	       (and (eq? (ustring-ref suffix i) (ustring-ref string i))
		    (loop (fix:- i 1)))
	       #t)))))

;; Incorrect implementation
(define (ustring-prefix-ci? prefix string)
  (let ((n (ustring-length prefix)))
    (and (fix:<= n (ustring-length string))
	 (let loop ((i 0))
	   (if (fix:< i n)
	       (and (char-ci=? (ustring-ref prefix i) (ustring-ref string i))
		    (loop (fix:+ i 1)))
	       #t)))))

;; Incorrect implementation
(define (ustring-suffix-ci? suffix string)
  (let ((n (ustring-length suffix)))
    (and (fix:<= n (ustring-length string))
	 (let loop ((i (fix:- n 1)))
	   (if (fix:>= i 0)
	       (and (char-ci=? (ustring-ref suffix i) (ustring-ref string i))
		    (loop (fix:- i 1)))
	       #t)))))

(define (ustring-head string end)
  (ustring-copy string 0 end))

(define (ustring-tail string start)
  (ustring-copy string start))

(define (ustring->list string #!optional start end)
  (cond ((legacy-string? string) (legacy-string->list string start end))
	((utf32-string? string) (utf32-string->list string start end))
	(else (error:not-a ustring? string 'ustring->list))))

(define (ustring->vector string #!optional start end)
  (cond ((legacy-string? string) (legacy-string->vector string start end))
	((utf32-string? string) (utf32-string->vector string start end))
	(else (error:not-a ustring? string 'ustring->vector))))

(define (ustring-for-each proc string . strings)
  (if (null? strings)
      (let ((n (ustring-length string)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (proc (ustring-ref string i))))
      (let ((n (min-length ustring-length string strings)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (apply proc
		 (ustring-ref string i)
		 (map (lambda (string)
			(ustring-ref string i))
		      strings))))))

(define (ustring-map proc string . strings)
  (if (null? strings)
      (let* ((n (ustring-length string))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i (proc (ustring-ref string i))))
	result)
      (let* ((n (min-length ustring-length string strings))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i
			     (apply proc
				    (ustring-ref string i)
				    (map (lambda (string)
					   (ustring-ref string i))
					 strings))))
	result)))

(define (ustring-any proc string . strings)
  (cond ((null? strings)
	 (let ((n (ustring-length string)))
	   (let loop ((i 0))
	     (and (fix:< i n)
		  (if (proc (ustring-ref string i))
		      #t
		      (loop (fix:+ i 1)))))))
	((null? (cdr strings))
	 (let* ((string2 (car strings))
		(n (fix:min (ustring-length string)
			    (ustring-length string2))))
	   (let loop ((i 0))
	     (and (fix:< i n)
		  (if (proc (ustring-ref string i)
			    (ustring-ref string2 i))
		      #t
		      (loop (fix:+ i 1)))))))
	(else
	 (let ((n (min-length ustring-length string strings)))
	   (let loop ((i 0))
	     (and (fix:< i n)
		  (if (apply proc
			     (ustring-ref string i)
			     (map (lambda (string)
				    (ustring-ref string i))
				  strings))
		      #t
		      (loop (fix:+ i 1)))))))))

(define (ustring-every proc string . strings)
  (cond ((null? strings)
	 (let ((n (ustring-length string)))
	   (let loop ((i 0))
	     (if (fix:< i n)
		 (and (proc (ustring-ref string i))
		      (loop (fix:+ i 1)))
		 #t))))
	((null? (cdr strings))
	 (let* ((string2 (car strings))
		(n (fix:min (ustring-length string)
			    (ustring-length string2))))
	   (let loop ((i 0))
	     (if (fix:< i n)
		 (and (proc (ustring-ref string i)
			    (ustring-ref string2 i))
		      (loop (fix:+ i 1)))
		 #t))))
	(else
	 (let ((n (min-length ustring-length string strings)))
	   (let loop ((i 0))
	     (if (fix:< i n)
		 (and (apply proc
			     (ustring-ref string i)
			     (map (lambda (string)
				    (ustring-ref string i))
				  strings))
		      (loop (fix:+ i 1)))
		 #t))))))

(define (ustring-find-first-index proc string #!optional start end)
  (cond ((legacy-string? string)
	 (legacy-string-find-first-index proc string start end))
	((utf32-string? string)
	 (utf32-string-find-first-index proc string start end))
	(else
	 (error:not-a ustring? string 'ustring-find-first-index))))

(define (ustring-find-last-index proc string #!optional start end)
  (cond ((legacy-string? string)
	 (legacy-string-find-last-index proc string start end))
	((utf32-string? string)
	 (utf32-string-find-last-index proc string start end))
	(else
	 (error:not-a ustring? string 'ustring-find-last-index))))

(define (legacy-string-find-first-index proc string #!optional start end)
  (let* ((caller 'legacy-string-find-next-index)
	 (end (fix:end-index end (legacy-string-length string) caller))
	 (start (fix:start-index start end caller)))
    (let loop ((i start))
      (and (fix:< i end)
	   (if (proc (legacy-string-ref string i))
	       i
	       (loop (fix:+ i 1)))))))

(define (legacy-string-find-last-index proc string #!optional start end)
  (let* ((caller 'legacy-string-find-last-index)
	 (end (fix:end-index end (legacy-string-length string) caller))
	 (start (fix:start-index start end caller)))
    (let loop ((i (fix:- end 1)))
      (and (fix:>= i start)
	   (if (proc (legacy-string-ref string i))
	       i
	       (loop (fix:- i 1)))))))

(define (ustring-find-first-char string char #!optional start end)
  (ustring-find-first-index (char=-predicate char) string start end))

(define (ustring-find-last-char string char #!optional start end)
  (ustring-find-last-index (char=-predicate char) string start end))

(define (ustring-find-first-char-in-set string char-set #!optional start end)
  (ustring-find-first-index (char-set-predicate char-set) string start end))

(define (ustring-find-last-char-in-set string char-set #!optional start end)
  (ustring-find-last-index (char-set-predicate char-set) string start end))

(define (ustring-upcase string)
  (cond ((legacy-string? string) (legacy-string-upcase string))
	((utf32-string? string) (utf32-string-upcase string))
	(else (error:not-a ustring? string 'ustring-upcase))))

(define (ustring-downcase string)
  (cond ((legacy-string? string) (legacy-string-downcase string))
	((utf32-string? string) (utf32-string-downcase string))
	(else (error:not-a ustring? string 'ustring-downcase))))

(define (ustring-capitalize string)
  (cond ((legacy-string? string) (legacy-string-capitalize string))
	((utf32-string? string) (utf32-string-capitalize string))
	(else (error:not-a ustring? string 'ustring-capitalize))))

(define (ustring-hash string #!optional modulus)
  (legacy-string-hash
   (cond ((legacy-string? string) string)
	 ((utf32-string? string) (string->utf8 string))
	 (else (error:not-a ustring? string 'ustring-hash)))
   modulus))

(define (ustring . objects)
  (%ustring* objects 'ustring))

(define (ustring* objects)
  (guarantee list? objects 'ustring*)
  (%ustring* objects 'ustring*))

(define (%ustring* objects caller)
  (%ustring-append*
   (map (lambda (object)
	  (->ustring object caller))
	objects)))

(define (->ustring object caller)
  (cond ((not object) "")
	((char? object) (make-ustring 1 object))
	((ustring? object) object)
	((symbol? object) (symbol->string object))
	((pathname? object) (->namestring object))
	((number? object) (number->string object))
	((uri? object) (uri->string object))
	(else (error:not-a ->ustring-component? object caller))))

(define (->ustring-component? object)
  (cond (not object)
	(char? object)
	(ustring? object)
	(symbol? object)
	(pathname? object)
	(number? object)
	(uri? object)))

(define (string-for-primitive string)
  (or (ustring->ascii string)
      (string->utf8 string)))