#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (run-turtle-tests #!optional root)
  (fresh-line)
  (let ((base-uri
	 (string->uri "http://www.w3.org/2001/sw/DataAccess/df1/tests/"))
	(root
	 (if (default-object? root)
	     "~/xml/turtle-tests/"
	     root)))
    (let ((run-test
	   (lambda (name)
	     (write-string name)
	     (write-string ":")
	     (do ((i (string-length name) (+ i 1)))
		 ((not (< i 24)) unspecific)
	       (write-char #\space))
	     (let ((pathname (merge-pathnames name root)))
	       (let ((c
		      (ignore-errors
		       (lambda ()
			 (read-rdf/turtle-file pathname base-uri)))))
		 (if (condition? c)
		     (write-condition-report c (current-output-port))
		     (begin
		       (write-string "Parsed")
		       #|
		       (call-with-output-file
			   (pathname-new-type pathname "foo")
			 (lambda (port)
			   (for-each (lambda (triple)
				       (write-rdf/nt triple port))
				     c)))
		       |#
		       ))))
	     (newline))))
      (do ((i 0 (+ i 1)))
	  ((not (<= i 25)) unspecific)
	(if (not (or (= i 14)))		;test-14 runs out of memory
	    (run-test (string-append "test-"
				     (if (< i 10) "0" "")
				     (number->string i)
				     ".ttl"))))
      (run-test "rdf-schema.ttl")
      (run-test "rdfs-namespace.ttl")
      (run-test "rdfq-results.ttl")
      (do ((i 0 (+ i 1)))
	  ((not (<= i 14)) unspecific)
	(run-test (string-append "bad-"
				 (if (< i 10) "0" "")
				 (number->string i)
				 ".ttl"))))))