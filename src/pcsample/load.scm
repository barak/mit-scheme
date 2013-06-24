#| -*-Scheme-*-

$Id: load.scm,v 1.6 2001/12/20 16:13:18 cph Exp $

Copyright (c) 1995-1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; System Packaging

(declare (usual-integrations))

(package/system-loader "pcs" '() 'QUERY)
(add-identification! "PC Sampler" 1 0)

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

  (for-each package-initialize
	    '((pribinut)
	      (pc-sample interrupt-handler)
	      (pc-sample)
	      (pc-sample interp-procs)
	      (pc-sample code-blocks)
	      (pc-sample display)
	      (pc-sample zones))))