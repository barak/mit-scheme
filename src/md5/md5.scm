#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; MD5 wrapper
;;; package: (md5)

(declare (usual-integrations))

(C-include "md5")

(define-integrable (mhash-available?) #f)

(define (%md5-init)
  ;; Create and return an MD5 digest context.
  (let ((context (make-string (C-sizeof "MD5_CTX"))))
    (C-call "MD5_INIT" context)
    context))

(define (%md5-update context string start end)
  ;; Update CONTEXT with the contents of the substring (STRING,START,END).
  (guarantee-md5-context context '%MD5-UPDATE)
  (guarantee-substring string start end '%MD5-UPDATE)
  (C-call "do_MD5_UPDATE" context string start end))

(define (%md5-final context)
  ;; Finalize CONTEXT and return the digest as a 16-byte string.
  (guarantee-md5-context context '%MD5-FINAL)
  (let ((result (make-string (C-enum "MD5_DIGEST_LENGTH"))))
    (C-call "do_MD5_FINAL" context result)
    result))

(define (guarantee-md5-context object operator)
  (if (and (string? object)
	   (= (string-length object) (C-sizeof "MD5_CTX")))
      object
      (error:bad-range-argument object
				"an MD5 context"
				operator)))

(define (%md5 string)
  ;; Generate an MD5 digest of string.
  ;; The digest is returned as a 16-byte string.
  (guarantee-string string '%MD5)
  (let ((length (string-length string))
	(result (make-string (C-enum "MD5_DIGEST_LENGTH"))))
    (C-call "do_MD5" string length result)
    result))

(define (md5-available?)
  (or (mhash-available?)
      (%md5-available?)))

(define (%md5-available?)
  (let ((path (ignore-errors (lambda ()
			       (system-library-pathname "md5-shim.so")))))
    (and (pathname? path)
	 (file-loadable? path))))

(define (md5-file filename)
  (cond ((mhash-available?)
	 (mhash-file 'MD5 filename))
	((%md5-available?)
	 (%md5-file filename))
	(else
	 (error "This Scheme system was built without MD5 support."))))

(define (%md5-file filename)
  (call-with-binary-input-file filename
    (lambda (port)
      (let ((buffer (make-string 4096))
	    (context (%md5-init)))
	(dynamic-wind (lambda ()
			unspecific)
		      (lambda ()
			(let loop ()
			  (let ((n (read-substring! buffer 0 4096 port)))
			    (if (fix:= 0 n)
				(%md5-final context)
				(begin
				  (%md5-update context buffer 0 n)
				  (loop))))))
		      (lambda ()
			(string-fill! buffer #\NUL)))))))

(define (md5-string string)
  (md5-substring string 0 (string-length string)))

(define (md5-substring string start end)
  (cond ((mhash-available?)
	 (mhash-substring 'MD5 string start end))
	((%md5-available?)
	 (%md5-substring string start end))
	(else
	 (error "This Scheme system was built without MD5 support."))))

(define (%md5-substring string start end)
  (let ((context (%md5-init)))
    (%md5-update context string start end)
    (%md5-final context)))

(define (md5-sum->number sum)
  (let ((l (string-length sum)))
    (do ((i 0 (fix:+ i 1))
	 (n 0 (+ (* n #x100) (vector-8b-ref sum i))))
	((fix:= i l) n))))

(define (md5-sum->hexadecimal sum)
  (let ((n (string-length sum))
	(digits "0123456789abcdef"))
    (let ((s (make-string (fix:* 2 n))))
      (do ((i 0 (fix:+ i 1)))
	  ((fix:= i n))
	(string-set! s (fix:* 2 i)
		     (string-ref digits
				 (fix:lsh (vector-8b-ref sum i) -4)))
	(string-set! s (fix:+ (fix:* 2 i) 1)
		     (string-ref digits
				 (fix:and (vector-8b-ref sum i) #x0F))))
      s)))