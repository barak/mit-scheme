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

;;;; Test the BLOWFISH option.

(let ((sample (string->utf8 "Some text to encrypt and decrypt.")))
  (call-with-binary-output-file "test"
    (lambda (output)
      (blowfish-encrypt-port (open-input-bytevector sample)
			     output
			     (string->utf8 "secret")
			     (write-blowfish-file-header output)
			     #t)))
  (let ((read-back
	 (call-with-binary-input-file "test"
	   (lambda (input)
	     (call-with-output-bytevector
	      (lambda (output)
		(blowfish-encrypt-port input output (string->utf8 "secret")
				       (read-blowfish-file-header input)
				       #f)))))))
    (if (not (bytevector=? sample read-back))
	(error "sample did not decrypt correctly"))))