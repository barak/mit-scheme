#| -*-Scheme-*-

$Id: load.scm,v 1.5 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1995-1999 Massachusetts Institute of Technology

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

;; This kludge keeps the 7.4 and 8.0 sources the same:

(let ((compiler-info (->environment '(runtime compiler-info))))
  (if (environment-bound? compiler-info 'COMPILED-ENTRY/FILENAME)
      (in-package compiler-info
	(define compiled-entry/filename-and-index compiled-entry/filename)
	(define compiled-code-block/filename-and-index
	  compiled-code-block/filename))))

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