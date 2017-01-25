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

;;;; Extended strings
;;; package: (runtime xstring)

;;; This implementation supports all R7RS string operations in which all the
;;; names have "string" replaced by "xstring".  This is a transitional
;;; implementation to convert MIT/GNU Scheme to full Unicode string support.
;;;
;;; At some point in the future we'll renaming everything back to "string".

;; get-output-xstring
;; number->xstring
;; open-output-xstring
;; read-xstring
;; symbol->xstring
;; utf8->xstring
;; vector->xstring
;; write-xstring
;; xstring->number
;; xstring->symbol
;; xstring->utf8

(declare (usual-integrations))

;;;; U32 vector

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
  (bytevector-copy! to (u32->byte-index to)
		    from (u32->byte-index start) (u32->byte-index end)))

(define (u32-vector-fill! bytes start end u32)
  (do ((i start (fix:+ i 1)))
      ((not (fix:< i end)))
    (u32-vector-set! bytes i u32)))

;;;; UTF-32 string

(define (make-utf32-string k #!optional char)
  (let ((v (make-u32-vector k)))
    (if (not (default-object? char))
	(u32-vector-fill! v 0 k (char->integer char)))
    (%make-utf32-string v)))

(define (utf32-string . chars)
  (list->utf32-string chars))

(define (list->utf32-string chars)
  (let ((v (make-u32-vector (length chars))))
    (do ((chars chars (cdr chars))
	 (i 0 (fix:+ i 1)))
	((not (pair? chars)))
      (u32-vector-set! v i (char->integer (car chars))))
    (%make-utf32-string v)))

(define-record-type <utf32-string>
    (%make-utf32-string vector)
    utf32-string?
  (vector utf32-string-vector))

(define (utf32-string-length string)
  (u32-vector-length (utf32-string-vector string)))

(define (utf32-string-ref string index)
  (integer->char (u32-vector-ref (utf32-string-vector string) index)))

(define (utf32-string-set! string index char)
  (u32-vector-set! (utf32-string-vector string)
		   index
		   (char->integer char)))

(define (utf32-string-copy string #!optional start end)
  (let* ((end (get-end end (utf32-string-length string) 'utf32-string-copy))
	 (start (get-start start end 'utf32-string-copy))
	 (to (make-utf32-string (fix:- end start))))
    (%utf32-string-copy! to 0 string start end)
    to))

(define (utf32-string-copy! to at from #!optional start end)
  (let* ((end (get-end end (utf32-string-length from) 'utf32-string-copy!))
	 (start (get-start start end 'utf32-string-copy!)))
    (%utf32-string-copy! to at from start end)))

(define-integrable (%utf32-string-copy! to at from start end)
  (u32-vector-copy! (utf32-string-vector to) at
		    (utf32-string-vector from) start end))

(define (utf32-string-fill! string char #!optional start end)
  (let* ((end (get-end end (utf32-string-length string) 'utf32-string-fill!))
	 (start (get-start start end 'utf32-string-fill!)))
    (u32-vector-fill! (utf32-string-vector string) start end
		      (char->integer char))))

(define (utf32-string->list string #!optional start end)
  (let* ((end (get-end end (utf32-string-length string) 'utf32-string->list))
	 (start (get-start start end 'utf32-string->list)))
    (do ((i (fix:- end 1) (fix:- i 1))
	 (chars '() (cons (utf32-string-ref string i) chars)))
	((not (fix:>= i start)) chars))))

(define (utf32-string->vector string #!optional start end)
  (let* ((end (get-end end (utf32-string-length string) 'utf32-string->vector))
	 (start (get-start start end 'utf32-string->vector))
	 (v (make-vector (fix:- end start))))
    (do ((i start (fix:+ i 1)))
	((not (fix:< i end)))
      (vector-set! v i (utf32-string-ref string i)))
    v))

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

;;;; String

(define (xstring? object)
  (or (legacy-string? object)
      (utf32-string? object)))

(define (create-xstring-registrations!)
  (register-predicate! legacy-string? 'legacy-string)
  (register-predicate! utf32-string? 'utf32-string)
  (register-predicate! xstring? 'xstring
		       '<= legacy-string?
		       '<= utf32-string?))

(define (xstring-append . strings)
  (let ((string
	 (make-utf32-string
	  (do ((strings strings (cdr strings))
	       (n 0 (fix:+ n (xstring-length (car strings)))))
	      ((not (pair? strings)) n)))))
    (let loop ((strings strings) (i 0))
      (if (pair? strings)
	  (let ((n (xstring-length (car strings))))
	    (xstring-copy! string i (car strings) 0 n)
	    (loop (cdr strings) (fix:+ i n)))))
    string))

(define (xstring-length string)
  (cond ((legacy-string? string) (legacy-string-length string))
	((utf32-string? string) (utf32-string-length string))
	(else (error:not-a xstring? string 'xstring-length))))

(define (xstring-ref string index)
  (cond ((legacy-string? string) (legacy-string-ref string index))
	((utf32-string? string) (utf32-string-ref string index))
	(else (error:not-a xstring? string 'xstring-ref))))

(define (xstring-set! string index char)
  (cond ((legacy-string? string) (legacy-string-set! string index char))
	((utf32-string? string) (utf32-string-set! string index char))
	(else (error:not-a xstring? string 'xstring-set!))))

(define (xstring-copy string #!optional start end)
  (cond ((legacy-string? string) (legacy-string-copy string start end))
	((utf32-string? string) (utf32-string-copy string start end))
	(else (error:not-a xstring? string 'xstring-copy))))

(define (xstring-copy! to at from #!optional start end)
  (cond ((legacy-string? to)
	 (cond ((legacy-string? from)
		(legacy-string-copy! to at from start end))
	       ((utf32-string? from)
		(utf32->legacy-copy! to at from start end))
	       (else
		(error:not-a xstring? from 'xstring-copy!))))
	((utf32-string? to)
	 (cond ((legacy-string? from)
		(legacy->utf32-copy! to at from start end))
	       ((utf32-string? from)
		(utf32-string-copy! to at from start end))
	       (else
		(error:not-a xstring? from 'xstring-copy!))))
	(else
	 (error:not-a xstring? to 'xstring-copy!))))

(define (utf32->legacy-copy! to at from #!optional start end)
  (let* ((end (get-end end (utf32-string-length from) 'xstring-copy!))
	 (start (get-start start end 'xstring-copy!)))
    (do ((i start (fix:+ i 1))
	 (j at (fix:+ j 1)))
	((not (fix:< i end)))
      (legacy-string-set! to j (utf32-string-ref from i)))))

(define (legacy->utf32-copy! to at from #!optional start end)
  (let* ((end (get-end end (legacy-string-length from) 'xstring-copy!))
	 (start (get-start start end 'xstring-copy!)))
    (do ((i start (fix:+ i 1))
	 (j at (fix:+ j 1)))
	((not (fix:< i end)))
      (utf32-string-set! to j (legacy-string-ref from i)))))

(define (xstring-fill! string char #!optional start end)
  (cond ((legacy-string? string) (legacy-string-fill! string char start end))
	((utf32-string? string) (utf32-string-fill! string char start end))
	(else (error:not-a xstring? string 'xstring-fill!))))

(define (xstring->list string #!optional start end)
  (cond ((legacy-string? string) (legacy-string->list string start end))
	((utf32-string? string) (utf32-string->list string start end))
	(else (error:not-a xstring? string 'xstring->list))))

(define (xstring->vector string #!optional start end)
  (cond ((legacy-string? string) (legacy-string->vector string start end))
	((utf32-string? string) (utf32-string->vector string start end))
	(else (error:not-a xstring? string 'xstring->vector))))

(define (xstring-for-each procedure string . strings)
  (if (null? strings)
      (let ((n (xstring-length string)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (procedure (xstring-ref string i))))
      (let ((n (min-length xstring-length string strings)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (apply procedure
		 (xstring-ref string i)
		 (map (lambda (string)
			(xstring-ref string i))
		      strings))))))

(define (xstring-map proc string . strings)
  (if (null? strings)
      (let* ((n (xstring-length string))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i (proc (xstring-ref string i))))
	result)
      (let* ((n (min-length xstring-length string strings))
	     (result (make-utf32-string n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)))
	  (utf32-string-set! result i
			     (apply proc
				    (xstring-ref string i)
				    (map (lambda (string)
					   (xstring-ref string i))
					 strings))))
	result)))

;; (xstring-ci<=? string1 string2 . strings)
;; (xstring-ci<? string1 string2 . strings)
;; (xstring-ci=? string1 string2 . strings)
;; (xstring-ci>=? string1 string2 . strings)
;; (xstring-ci>? string1 string2 . strings)
;; (xstring-downcase string)
;; (xstring-foldcase string)
;; (xstring-upcase string)
;; (xstring<=? string1 string2 . strings)
;; (xstring<? string1 string2 . strings)
;; (xstring=? string1 string2 . strings)
;; (xstring>=? string1 string2 . strings)
;; (xstring>? string1 string2 . strings)

(define (get-end end length caller)
  (if (default-object? end)
      length
      (begin
	(guarantee index-fixnum? end caller)
	(if (not (fix:<= end length))
	    (error:bad-range-argument end caller))
	end)))

(define (get-start start end caller)
  (if (default-object? start)
      0
      (begin
	(guarantee index-fixnum? start caller)
	(if (not (fix:<= start end))
	    (error:bad-range-argument start caller))
	start)))

(define (min-length string-length string strings)
  (do ((strings strings (cdr strings))
       (n (string-length string)
	  (fix:min n (string-length (car strings)))))
      ((null? strings) n)))