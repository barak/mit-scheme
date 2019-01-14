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

;;;; Test the MCRYPT option.

(define (random-string length)
  (list->string (make-initialized-list length
				       (lambda (i)
					 (declare (ignore i))
					 (integer->char (random 256))))))

(if (not (member "tripledes" (mcrypt-algorithm-names)))
    (error "No tripledes."))

(if (not (member "cfb" (mcrypt-mode-names)))
    (error "No cipher-feedback mode."))

(let ((key (let ((sizes (mcrypt-supported-key-sizes "tripledes")))
	     (if (not (vector? sizes))
		 (error "Bogus key sizes for tripledes."))
	     (random-bytevector (vector-ref sizes
					    (-1+ (vector-length sizes))))))
      (init-vector (let* ((context
			   ;; Unfortunately the size is
			   ;; available only from the MCRYPT(?)!
			   (mcrypt-open-module "tripledes" "cfb"))
			  (size (mcrypt-init-vector-size context)))
		     (mcrypt-end context)
		     (random-bytevector size))))

  (call-with-binary-input-file "mcrypt.scm"
    (lambda (input)
      (call-with-binary-output-file "encrypted"
	(lambda (output)
	  (let ((copy (bytevector-copy init-vector)))
	    (mcrypt-encrypt-port "tripledes" "cfb"
				 input output key init-vector #t)
	    (if (not (bytevector=? copy init-vector))
		(error "Init vector modified.")))))))

  (call-with-binary-input-file "encrypted"
    (lambda (input)
      (call-with-binary-output-file "decrypted"
	(lambda (output)
	  (mcrypt-encrypt-port "tripledes" "cfb"
			       input output key init-vector #f))))))

(if (not (= 0 (run-shell-command "cmp mcrypt.scm decrypted")))
    (error "En/Decryption failed."))