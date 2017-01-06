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

;;;; R7RS bytevectors
;;; package: (runtime bytevector)

(declare (usual-integrations))

(define (byte? object)
  (and (index-fixnum? object)
       (fix:< object #x100)))

(define-primitives
  (allocate-bytevector 1)
  (bytevector-fill! 4)
  (bytevector-length 1)
  (bytevector-u8-ref 2)
  (bytevector-u8-set! 3)
  (bytevector? 1))

(add-boot-init!
 (lambda ()
   (register-predicate! byte? 'byte '<= exact-nonnegative-integer?)
   (register-predicate! bytevector? 'bytevector)))

(define (make-bytevector k #!optional byte)
  (let ((bytevector (allocate-bytevector k)))
    (if (not (default-object? byte))
	(bytevector-fill! bytevector 0 k byte))
    bytevector))

(define (bytevector . bytes)
  (let ((bytevector (allocate-bytevector (length bytes))))
    (do ((bytes bytes (cdr bytes))
	 (i 0 (fix:+ i 1)))
	((not (pair? bytes)))
      (bytevector-u8-set! bytevector i (car bytes)))
    bytevector))

(define (bytevector-append . bytevectors)
  (let* ((k
	  (do ((bytevectors bytevectors (cdr bytevectors))
	       (k 0 (fix:+ k (bytevector-length (car bytevectors)))))
	      ((not (pair? bytevectors)) k)))
	 (bytevector (allocate-bytevector k)))
    (do ((bytevectors bytevectors (cdr bytevectors))
	 (index 0 (fix:+ index (bytevector-length (car bytevectors)))))
	((not (pair? bytevectors)))
      (bytevector-copy! bytevector index (car bytevectors)))
    bytevector))

(define (bytevector-copy bytevector #!optional start end)
  ((ucode-primitive bytevector-copy 3)
   bytevector
   (if (default-object? start) 0 start)
   (if (default-object? end) (bytevector-length bytevector) end)))

(define (bytevector-copy! to at from #!optional start end)
  ((ucode-primitive bytevector-copy! 5)
   to
   at
   from
   (if (default-object? start) 0 start)
   (if (default-object? end) (bytevector-length from) end)))

(define (string->utf8 string #!optional start end)
  (guarantee string? string 'string->utf8)
  (let* ((end
	  (if (default-object? end)
	      (string-length string)
	      (begin
		(guarantee index-fixnum? end 'string->utf8)
		(if (not (fix:<= end (string-length string)))
		    (error:bad-range-argument end 'string->utf8))
		end)))
	 (start
	  (if (default-object? start)
	      0
	      (begin
		(guarantee index-fixnum? start 'string->utf8)
		(if (not (fix:<= start end))
		    (error:bad-range-argument start 'string->utf8))
		start))))
    (let ((buffer (allocate-bytevector (%count-utf8-bytes string start end))))
      (do ((from start (fix:+ from 1))
	   (to 0 (fix:+ to (%char->utf8! buffer to (string-ref string from)))))
	  ((not (fix:< from end))))
      buffer)))

(define (%char->utf8! buffer index char)
  (let ((cp (char->integer char)))

    (define-integrable (initial-byte n-bits offset)
      (fix:or (fix:and (fix:lsh #xFF (fix:+ n-bits 1)) #xFF)
	      (fix:lsh cp (fix:- 0 offset))))

    (define-integrable (trailing-byte offset)
      (fix:or #x80 (fix:and (fix:lsh cp (fix:- 0 offset)) #x3F)))

    (define-integrable (put-byte! offset byte)
      (bytevector-u8-set! buffer (fix:+ index offset) byte))

    (cond ((fix:< cp #x00000080)
	   (put-byte! 0 cp)
	   1)
	  ((fix:< cp #x00000800)
	   (put-byte! 0 (initial-byte 5 6))
	   (put-byte! 1 (trailing-byte 0))
	   2)
	  ((fix:< cp #x00010000)
	   (if (surrogate? cp)
	       (error "Code point is a UTF-16 surrogate:" cp))
	   (if (non-character? cp)
	       (error "Code point is a non-character:" cp))
	   (put-byte! 0 (initial-byte 4 12))
	   (put-byte! 1 (trailing-byte 6))
	   (put-byte! 2 (trailing-byte 0))
	   3)
	  (else
	   (if (non-character? cp)
	       (error "Code point is a non-character:" cp))
	   (put-byte! 0 (initial-byte 3 18))
	   (put-byte! 1 (trailing-byte 12))
	   (put-byte! 2 (trailing-byte 6))
	   (put-byte! 3 (trailing-byte 0))
	   4))))

(define (%count-utf8-bytes string start end)
  (do ((index start (fix:+ index 1))
       (n-bytes 0 (fix:+ n-bytes (char-utf8-bytes (string-ref string index)))))
      ((not (fix:< index end)) n-bytes)))

(define (char-utf8-bytes char)
  (let ((cp (char->integer char)))
    (cond ((fix:< cp #x00000080) 1)
	  ((fix:< cp #x00000800) 2)
	  ((fix:< cp #x00010000) 3)
	  ((fix:< cp #x00110000) 4)
	  (else (error "Not a unicode character:" char)))))

(define (utf8->string bytevector #!optional start end)
  (guarantee bytevector? bytevector 'utf8->string)
  (let* ((end
	  (if (default-object? end)
	      (bytevector-length bytevector)
	      (begin
		(guarantee index-fixnum? end 'utf8->string)
		(if (not (fix:<= end (bytevector-length bytevector)))
		    (error:bad-range-argument end 'utf8->string))
		end)))
	(start
	 (if (default-object? start)
	     0
	     (begin
	       (guarantee index-fixnum? start 'utf8->string)
	       (if (not (fix:<= start end))
		   (error:bad-range-argument start 'utf8->string))
	       start))))
    (%utf8->string bytevector start end)))

(define (%utf8->string bytevector start end)
  (let ((string (make-string (%count-utf8-chars bytevector start end))))
    (let loop ((from start) (to 0))

      (define-integrable (get-byte offset)
	(bytevector-u8-ref bytevector (fix:+ from offset)))

      (define-integrable (put-char! cp)
	(string-set! string to (integer->char cp)))

      (if (fix:< from end)
	  (let ((b0 (get-byte 0)))
	    (cond ((fix:< b0 #x80)
		   (put-char! b0)
		   (loop (fix:+ from 1) (fix:+ to 1)))
		  ((fix:< b0 #xE0)
		   (put-char! (decode-utf8-2 b0 (get-byte 1)))
		   (loop (fix:+ from 2) (fix:+ to 1)))
		  ((fix:< b0 #xF0)
		   (put-char! (decode-utf8-3 b0 (get-byte 1) (get-byte 2)))
		   (loop (fix:+ from 3) (fix:+ to 1)))
		  (else
		   (put-char!
		    (decode-utf8-4 b0 (get-byte 1) (get-byte 2) (get-byte 3)))
		   (loop (fix:+ from 4) (fix:+ to 1)))))))
    string))

(define (%count-utf8-chars bytevector start end)
  (let loop ((index start) (n-chars 0))
    (if (fix:< index end)
	(let ((b0 (bytevector-u8-ref bytevector index)))
	  (let ((index*
		 (fix:+ index
			(cond ((fix:< b0 #x80) 1)
			      ((fix:< b0 #xE0) 2)
			      ((fix:< b0 #xF0) 3)
			      (else 4)))))
	    (if (not (fix:<= index* end))
		(error "Truncated UTF-8 sequence:"
		       (bytevector-copy bytevector index end)))
	    (loop index* (fix:+ n-chars 1))))
	n-chars)))

(define (decode-utf8-2 b0 b1)
  (if (not (and (fix:> b0 #xC1)
		(trailing-byte? b1)))
      (error "Ill-formed UTF-8 sequence:" b0 b1))
  (fix:or (extract b0 #x1F 6)
	  (extract b1 #x3F 0)))

(define (decode-utf8-3 b0 b1 b2)
  (if (not (and (or (fix:> b0 #xE0) (fix:> b1 #x9F))
		(trailing-byte? b1)
		(trailing-byte? b2)))
      (error "Ill-formed UTF-8 sequence:" b0 b1 b2))
  (let ((cp
	 (fix:or (fix:or (extract b0 #x0F 12)
			 (extract b1 #x3F 6))
		 (extract b2 #x3F 0))))
    (if (surrogate? cp)
	(error "Code point is a UTF-16 surrogate:" cp))
    (if (non-character? cp)
	(error "Code point is a non-character:" cp))
    cp))

(define (decode-utf8-4 b0 b1 b2 b3)
  (if (not (and (or (fix:> b0 #xF0) (fix:> b1 #x8F))
		(trailing-byte? b1)
		(trailing-byte? b2)
		(trailing-byte? b3)))
      (error "Ill-formed UTF-8 sequence:" b0 b1 b2 b3))
  (let ((cp
	 (fix:or (fix:or (extract b0 #x07 18)
			 (extract b1 #x3F 12))
		 (fix:or (extract b2 #x3F 6)
			 (extract b3 #x3F 0)))))
    (if (not (fix:< cp #x110000))
	(error "Value is not a code point:" cp))
    (if (non-character? cp)
	(error "Code point is a non-character:" cp))
    cp))

(define-integrable (extract b m n)
  (fix:lsh (fix:and b m) n))

(define-integrable (trailing-byte? b)
  (fix:= (fix:and #xC0 b) #x80))

(define-integrable (surrogate? cp)
  (and (fix:<= #xD800 cp) (fix:< cp #xDFFF)))

(define-integrable (non-character? cp)
  (or (and (fix:<= #xFDD0 cp) (fix:< cp #xFDF0))
      (fix:= (fix:and #xFFFE cp) #xFFFE)))