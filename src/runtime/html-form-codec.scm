#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Codecs for HTML forms
;;; package: (runtime html-form-codec)

;;; Assumption: octets less than #x80 are ASCII.

(declare (usual-integrations))

;;;; Decoder

(define (decode-www-form-urlencoded string start end)
  (guarantee 8-bit-string? string 'decode-www-form-urlencoded)
  (let ((input (open-input-string string start end)))
    (let loop ((data '()))
      (let ((char (read-char input)))
	(if (eof-object? char)
	    (reverse! data)
	    (begin
	      (unread-char char input)
	      (let ((name (decode-segment input #t)))
		(loop
		 (cons (cons name (decode-segment input #f))
		       data)))))))))

(define (decode-segment input name?)
  (let ((builder (string-builder)))
    (let ((out
	   (if name?
	       (lambda (char)
		 (builder (if (fix:< (char->integer char) #x80)
			      (char-downcase char)
			      char)))
	       builder))
	  (digit
	   (lambda ()
	     (let ((char (read-char input)))
	       (if (eof-object? char)
		   (error "Incomplete %-escape in HTML form data."))
	       (or (char->digit char 16)
		   (error "Illegal character in % escape:" char))))))
      (let loop ()
	(let ((char (read-char input)))
	  (cond ((eof-object? char)
		 (if name?
		     (error
		      "Improperly terminated name in HTML form data.")))
		((char=? char #\return)
		 (if (not (eqv? (peek-char input) #\newline))
		     (out char))
		 (loop))
		((or (char-unreserved? char)
		     (char=? char #\newline))
		 (out char)
		 (loop))
		((char=? char #\=)
		 (if (not name?)
		     (error "Char in illegal position in HTML form data:"
			    char)))
		((or (char=? char #\&)
		     (char=? char #\;))
		 (if name?
		     (error "Char in illegal position in HTML form data:"
			    char)))
		((char=? char #\+)
		 (out #\space)
		 (loop))
		((char=? char #\%)
		 (let ((d1 (digit)))
		   (out (integer->char (fix:or (fix:lsh d1 4) (digit)))))
		 (loop))
		(else
		 (error "Illegal character in HTML form data:" char))))))
    (builder)))

;;;; Encoder

(define (encode-www-form-urlencoded data)
  (guarantee-list-of-type data
			  (lambda (p)
			    (and (pair? p)
				 (interned-symbol? (car p))
				 (string? (cdr p))))
			  "HTML form data alist"
			  'encode-www-form-urlencoded)
  (let ((builder (string-builder)))

     (define (write-datum datum)
       (encode-segment (symbol->string (car datum)))
       (builder #\=)
       (encode-segment (cdr datum)))

     (define (encode-segment string)
       (let ((end (string-length string)))
	 (do ((i 0 (fix:+ i 1)))
	     ((not (fix:< i end)))
	   (encode-octet (string-ref string i)))))

     (define (encode-octet char)
       (cond ((char-unreserved? char)
	      (builder char))
	     ((char=? char #\space)
	      (builder #\+))
	     ((char=? char #\newline)
	      (builder #\return)
	      (builder #\linefeed))
	     (else
	      (let ((octet (char->integer char)))
		(builder #\%)
		(builder (digit->char (fix:lsh (fix:and octet #xF0) -4) 16))
		(builder (digit->char (fix:and octet #x0F) 16))))))

     (if (pair? data)
	 (begin
	   (write-datum (car data))
	   (do ((data (cdr data) (cdr data)))
	       ((not (pair? data)))
	     (write-char #\&)
	     (write-datum (car data)))))
     (builder)))

(define-deferred char-set:unreserved
  (char-set-difference char-set:ascii
		       (char-set-union char-set:ctls
				       (string->char-set " +%=&;"))))

(define-deferred char-unreserved?
  (char-set-predicate char-set:unreserved))