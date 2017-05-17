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

;;;; The MD5 option.
;;; package: (md5)

(declare (usual-integrations))

(define (import-md5)
  (let ((target-environment (nearest-repl/environment))
	(source-environment (->environment '(md5))))
    (for-each (lambda (name)
		(link-variables target-environment name
				source-environment name))
	      '(md5-bytevector
		md5-file
		md5-string))))

(C-include "md5")

(define (%md5-init)
  (let ((context (make-bytevector (C-sizeof "MD5_CTX"))))
    (C-call "MD5_INIT" context)
    context))

(define (%md5-update context bytevector start end)
  (C-call "do_MD5_UPDATE" context bytevector start end))

(define (%md5-final context)
  (let ((result (make-bytevector (C-enum "MD5_DIGEST_LENGTH"))))
    (C-call "do_MD5_FINAL" context result)
    result))

(define (md5-file filename)
  (call-with-binary-input-file filename
    (port-consumer %md5-init %md5-update %md5-final)))

(define (md5-string string #!optional start end)
  (md5-bytevector (string->utf8 string start end)))

(define (md5-bytevector bytes #!optional start end)
  (let ((end (fix:end-index end (bytevector-length bytes) 'md5-bytevector))
	(start (fix:start-index start end 'md5-bytevector))
	(context (%md5-init)))
    (%md5-update context bytes start end)
    (%md5-final context)))

(define (port-consumer initialize update finalize)
  (lambda (port)
    (call-with-buffer #x1000
      (lambda (buffer)
	(let ((context (initialize)))
	  (let loop ()
	    (let ((n (read-bytevector! buffer port)))
	      (if (and n (not (eof-object? n)))
		  (begin
		    (update context buffer 0 n)
		    (loop)))))
	  (finalize context))))))

(define (call-with-buffer n procedure)
  (let ((buffer (make-bytevector n)))
    (dynamic-wind
	(lambda ()
	  unspecific)
	(lambda ()
	  (procedure buffer))
	(lambda ()
	  (bytevector-fill! buffer 0)))))