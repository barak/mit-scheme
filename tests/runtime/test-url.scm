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

;;;; Test URLs.

(declare (usual-integrations))

(define-test 'PATHNAME->URI->PATHNAME
  (lambda ()
    (assert-true (pathname=? (->pathname "./file")
			     (uri->pathname (pathname->uri "./file"))))))

(define-test 'MERGE-URIS
  (lambda ()
    (assert-eqv (test-merge-uris) 0)))

(define (test-merge-uris #!optional verbose?)
  (let ((verbose? (if (default-object? verbose?) #f verbose?))
	(base-uri
	 (string->uri "http://a/b/c/d;p?q"))
	(normal-examples
	 '(("g:h"           "g:h")
	   ("g"             "http://a/b/c/g")
	   ("./g"           "http://a/b/c/g")
	   ("g/"            "http://a/b/c/g/")
	   ("/g"            "http://a/g")
	   ("//g"           "http://g")
	   ("?y"            "http://a/b/c/d;p?y")
	   ("g?y"           "http://a/b/c/g?y")
	   ("#s"            "http://a/b/c/d;p?q#s")
	   ("g#s"           "http://a/b/c/g#s")
	   ("g?y#s"         "http://a/b/c/g?y#s")
	   (";x"            "http://a/b/c/;x")
	   ("g;x"           "http://a/b/c/g;x")
	   ("g;x?y#s"       "http://a/b/c/g;x?y#s")
	   (""              "http://a/b/c/d;p?q")
	   ("."             "http://a/b/c/")
	   ("./"            "http://a/b/c/")
	   (".."            "http://a/b/")
	   ("../"           "http://a/b/")
	   ("../g"          "http://a/b/g")
	   ("../.."         "http://a/")
	   ("../../"        "http://a/")
	   ("../../g"       "http://a/g")))
	(abnormal-examples
	 '(("../../../g"    "http://a/g")
	   ("../../../../g" "http://a/g")
	   ("/./g"          "http://a/g")
	   ("/../g"         "http://a/g")
	   ("g."            "http://a/b/c/g.")
	   (".g"            "http://a/b/c/.g")
	   ("g.."           "http://a/b/c/g..")
	   ("..g"           "http://a/b/c/..g")
	   ("./../g"        "http://a/b/g")
	   ("./g/."         "http://a/b/c/g/")
	   ("g/./h"         "http://a/b/c/g/h")
	   ("g/../h"        "http://a/b/c/h")
	   ("g;x=1/./y"     "http://a/b/c/g;x=1/y")
	   ("g;x=1/../y"    "http://a/b/c/y")
	   ("g?y/./x"       "http://a/b/c/g?y/./x")
	   ("g?y/../x"      "http://a/b/c/g?y/../x")
	   ("g#s/./x"       "http://a/b/c/g#s/./x")
	   ("g#s/../x"      "http://a/b/c/g#s/../x")
	   ("http:g"        "http:g")))
	(n-errors 0))
    (let ((run-examples
	   (lambda (examples)
	     (for-each (lambda (p)
			 (let ((reference (car p))
			       (result (cadr p)))
			   (let ((s
				  (uri->string
				   (merge-uris reference base-uri))))
			     (cond ((not (string=? s result))
				    (set! n-errors (+ n-errors 1))
				    (write-line (list reference result s)))
				   (verbose?
				    (write-line (list reference result s)))))))
		       examples))))
      (if verbose? (write-string "Normal examples:\n"))
      (run-examples normal-examples)
      (if verbose? (write-string "\nAbnormal examples:\n"))
      (run-examples abnormal-examples)
      (if verbose? (newline))
      (if verbose?
	  (begin
	    (if (> n-errors 0)
		(write n-errors)
		(write-string "No"))
	    (write-string " errors found")
	    (newline)))
      n-errors)))