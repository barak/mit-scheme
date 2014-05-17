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

;;;; System Packaging

(declare (usual-integrations))

(cond ((name->package '(gc-wabbits))
       (display "\n; Package already loaded under some other alias")
       'ok)
      (else
       (load-package-set "wabbit")
       (add-subsystem-identification! "Wabbit Hunting / Headhunting GC" '(1 0))

       (let ()
	 (define (package-initialize package-name
				     #!optional procedure-name mandatory?)
	   (let ((procedure-name
		  (if (default-object? procedure-name)
		      'INITIALIZE-PACKAGE!
		      procedure-name))
		 (mandatory?
		  (or (default-object? mandatory?) mandatory?)))
	     (define (print-name string)
	       (display "\n")
	       (display string)
	       (display " (")
	       (let loop ((name package-name))
		 (if (not (null? name))
		     (begin
		       (if (not (eq? name package-name))
			   (display " "))
		       (display (system-pair-car (car name)))
		       (loop (cdr name)))))
	       (display ")"))

	     (define (package-reference name)
	       (package/environment (find-package name)))

	     (let ((env (package-reference package-name)))
	       (cond ((not procedure-name))
		     ((not (lexical-unreferenceable? env procedure-name))
		      (print-name "initialize:")
		      (if (not (eq? procedure-name 'INITIALIZE-PACKAGE!))
			  (begin
			    (display " [")
			    (display (system-pair-car procedure-name))
			    (display "]")))
		      ((lexical-reference env procedure-name)))
		     ((not mandatory?)
		      (print-name "* skipping:"))
		     (else
		      ;; Missing mandatory package! Report it and die.
		      (print-name "Package")
		      (display " is missing initialization procedure ")
		      (display (system-pair-car procedure-name))
		      (error "Could not initialize a required package."))))))

	 (package-initialize '(gc-wabbits)))))

;;; fini
