#| -*-Scheme-*-

$Id: load.scm,v 1.3 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1994-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; System Packaging

(declare (usual-integrations))

(cond ((name->package '(gc-wabbits))
       (display "\n; Package already loaded under some other alias")
       'ok)
      (else
       (package/system-loader "wabbit" '() 'QUERY)
       (add-identification! "Wabbit Hunting / Headhunting GC" 1 0)

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

