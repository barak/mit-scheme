#| -*-Scheme-*-

$Id: 99379d6e984661d225e27be83983c253d46a83b8 $

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

(define (run-xml-tests #!optional root)
  (let ((root
	 (merge-pathnames "xmlconf/xmltest/"
			  (if (default-object? root)
			      "~/xml/"
			      (pathname-as-directory root)))))
    (for-each (lambda (dir)
		(newline)
		(write-string ";")
		(write-string dir)
		(newline)
		(test-directory (merge-pathnames dir root)))
	      '("valid/sa" "valid/ext-sa" "valid/not-sa"
			   "invalid"
			   "not-wf/sa" "not-wf/ext-sa" "not-wf/not-sa"))))

(define (test-directory directory)
  (call-with-temporary-file-pathname
   (lambda (temp)
     (map (lambda (pathname)
	    (write-string ";")
	    (write-string (file-namestring pathname))
	    (write-string ":\t")
	    (let ((v (ignore-errors (lambda () (read-xml-file pathname)))))
	      (cond ((not v)
		     (write-string "No match."))
		    ((condition? v)
		     (write-condition-report v (current-output-port)))
		    (else
		     (let ((???
			    (lambda (operation thunk)
			      (let ((c (ignore-errors thunk)))
				(if (condition? c)
				    (begin
				      (write-string "Can't ")
				      (write-string operation)
				      (write-string ": ")
				      (write-condition-report
				       c
				       (current-output-port))
				      #f)
				    #t)))))
		       (if (??? "write"
				(lambda ()
				  (write-xml-file v temp)))
			   (if (??? "re-read"
				    (lambda ()
				      (read-xml-file temp)))
			       (write-string "Parsed"))))))
	      (fresh-line)
	      v))
	  (directory-read
	   (merge-pathnames "*.xml" (pathname-as-directory directory)))))))

(define (run-output-tests output #!optional root)
  (let ((root
	 (merge-pathnames "xmlconf/xmltest/"
			  (if (default-object? root)
			      "~/xml/"
			      (pathname-as-directory root))))
	(output (pathname-as-directory output)))
    (for-each (lambda (pathname)
		(write-string ";")
		(write-string (file-namestring pathname))
		(write-string ":\t")
		(let ((v (ignore-errors (lambda () (read-xml-file pathname)))))
		  (cond ((not v)
			 (write-string "No match.")
			 (newline))
			((condition? v)
			 (write-condition-report v (current-output-port))
			 (newline))
			(else
			 (write-string "Parsed: ")
			 (write v)
			 (newline)
			 (call-with-output-file
			     (merge-pathnames (file-pathname pathname) output)
			   (lambda (port)
			     (write-xml v port)))))
		  v))
	      (directory-read (merge-pathnames "valid/sa/*.xml" root)))))