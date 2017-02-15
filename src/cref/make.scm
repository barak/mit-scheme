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

;;;; Package Model: System Construction

(declare (usual-integrations))

(with-loader-base-uri (system-library-uri "cref/")
  (lambda ()
    (load-package-set "cref")))

;;; Patch the package loader in 9.2 host runtimes.
(if (string-prefix? "9.2" (get-subsystem-version-string "Release"))
    (eval
     '(begin
	(define (link-description? object)
	  (and (vector? object)
	       (cond ((fix:= (vector-length object) 2)
		      (and (symbol? (vector-ref object 0))
			   (package-name? (vector-ref object 1))))
		     ((fix:= (vector-length object) 3)
		      (and (symbol? (vector-ref object 0))
			   (package-name? (vector-ref object 1))
			   (symbol? (vector-ref object 2))))
		     ((fix:= (vector-length object) 4)
		      (and (symbol? (vector-ref object 0))
			   (package-name? (vector-ref object 1))
			   (symbol? (vector-ref object 2))
			   (or (eq? #f (vector-ref object 3))
			       (eq? 'deprecated (vector-ref object 3)))))
		     (else #f))))
	(define (create-links-from-description description)
	  (let ((environment
		 (find-package-environment (package-description/name description))))
	    (let ((bindings (package-description/exports description)))
	      (let ((n (vector-length bindings)))
		(do ((i 0 (fix:+ i 1)))
		    ((fix:= i n))
		  (let ((binding (vector-ref bindings i)))
		    (link-variables (find-package-environment (vector-ref binding 1))
				    (if (fix:= (vector-length binding) 3)
					(vector-ref binding 2)
					(vector-ref binding 0))
				    environment
				    (vector-ref binding 0))))))
	    (let ((bindings (package-description/imports description)))
	      (let ((n (vector-length bindings)))
		(do ((i 0 (fix:+ i 1)))
		    ((fix:= i n))
		  (let ((binding (vector-ref bindings i)))
		    (let ((source-environment
			   (find-package-environment (vector-ref binding 1)))
			  (source-name
			   (if (fix:>= (vector-length binding) 3)
			       (vector-ref binding 2)
			       (vector-ref binding 0))))
		      (guarantee-binding source-environment source-name)
		      (link-variables environment (vector-ref binding 0)
				      source-environment source-name)))))))))
     (->environment '(package))))

(add-subsystem-identification! "CREF" '(2 4))