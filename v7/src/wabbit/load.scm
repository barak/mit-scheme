#| -*-Scheme-*-

$Id: load.scm,v 1.1 1995/07/11 02:03:43 ziggy Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; System Packaging

(declare (usual-integrations))

(cond ((name->package '(gc-wabbits))
       (display "\n; Package already loaded under some other alias")
       'ok)
      (else
       (package/system-loader "wabbit" '() 'QUERY)
       (add-system! (make-system "Wabbit Hunting / Headhunting GC" 1 0 '()))

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

