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

;;;; The mcrypt library

(define mcrypt-linked? #f)

(define (mcrypt-available?)
  (and (plugin-available? "mcrypt")
       (or mcrypt-linked?
	   (begin
	     (load-option 'mcrypt)
	     (mcrypt-link!)
	     #t))))

(define (mcrypt-link!)
  (for-each
    (let ((runtime (->environment '(runtime crypto)))
	  (mcrypt (->environment '(mcrypt))))
      (lambda (name)
	(environment-link-name runtime mcrypt name)))
    mcrypt-names)
  (set! mcrypt-linked? #t))

(define mcrypt-names
  '(mcrypt-algorithm-name
    mcrypt-algorithm-names
    mcrypt-block-algorithm-mode?
    mcrypt-block-algorithm?
    mcrypt-block-mode?
    mcrypt-context?
    mcrypt-decrypt!
    mcrypt-encrypt
    mcrypt-encrypt!
    mcrypt-encrypt-port
    mcrypt-end
    mcrypt-init
    mcrypt-init-vector-size
    mcrypt-key-size
    mcrypt-mode-name
    mcrypt-mode-names
    mcrypt-open-module
    mcrypt-self-test
    mcrypt-supported-key-sizes))

(define mcrypt-algorithm-name)
(define mcrypt-algorithm-names)
(define mcrypt-block-algorithm-mode?)
(define mcrypt-block-algorithm?)
(define mcrypt-block-mode?)
(define mcrypt-context?)
(define mcrypt-decrypt!)
(define mcrypt-encrypt)
(define mcrypt-encrypt!)
(define mcrypt-encrypt-port)
(define mcrypt-end)
(define mcrypt-init)
(define mcrypt-init-vector-size)
(define mcrypt-key-size)
(define mcrypt-mode-name)
(define mcrypt-mode-names)
(define mcrypt-open-module)
(define mcrypt-self-test)
(define mcrypt-supported-key-sizes)