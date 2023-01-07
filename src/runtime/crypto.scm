#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Interface to cryptography libraries
;;; package: (runtime crypto)

(declare (usual-integrations))

;;;; MD5

(define (md5-available?)
  #t)

(define (md5-file filename)
  (call-with-binary-input-file filename
    (port-consumer (ucode-primitive md5-init 0)
		   (ucode-primitive md5-update 4)
		   (ucode-primitive md5-final 1))))

(define (md5-string string #!optional start end)
  (md5-bytevector (string->utf8 string start end)))

(define (md5-bytevector bytes #!optional start end)
  (let ((end (fix:end-index end (bytevector-length bytes) 'md5-bytevector))
	(start (fix:start-index start end 'md5-bytevector))
	(context ((ucode-primitive md5-init 0))))
    ((ucode-primitive md5-update 4) context bytes start end)
    ((ucode-primitive md5-final 1) context)))

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