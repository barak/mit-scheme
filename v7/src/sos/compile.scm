;;; -*-Scheme-*-
;;;
;;; $Id: compile.scm,v 1.1 1997/06/04 06:08:30 cph Exp $
;;;
;;; Copyright (c) 1995-97 Massachusetts Institute of Technology
;;;
;;; This material was developed by the Scheme project at the
;;; Massachusetts Institute of Technology, Department of Electrical
;;; Engineering and Computer Science.  Permission to copy this
;;; software, to redistribute it, and to use it for any purpose is
;;; granted, subject to the following restrictions and understandings.
;;;
;;; 1. Any copy made of this software must include this copyright
;;; notice in full.
;;;
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions
;;; that they make, so that these may be included in future releases;
;;; and (b) to inform MIT of noteworthy uses of this software.
;;;
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the
;;; usual standards of acknowledging credit in academic research.
;;;
;;; 4. MIT has made no warrantee or representation that the operation
;;; of this software will be error-free, and MIT is under no
;;; obligation to provide any services, by way of maintenance, update,
;;; or otherwise.
;;;
;;; 5. In conjunction with products arising from the use of this
;;; material, there shall be no use of the name of the Massachusetts
;;; Institute of Technology nor of any adaptation thereof in any
;;; advertising, promotional, or sales literature without prior
;;; written consent from MIT in each case.

(load-option 'CREF)

(define compile-file-override-usual-integrations '())
(define compile-file-sf-only? #f)
(define compile-file)
(let ((scm-pathname (lambda (path) (pathname-new-type path "scm")))
      (bin-pathname (lambda (path) (pathname-new-type path "bin")))
      (ext-pathname (lambda (path) (pathname-new-type path "ext")))
      (com-pathname (lambda (path) (pathname-new-type path "com"))))

  (define (process-file input-file output-file dependencies processor)
    (let ((reasons
	   (let ((output-time (file-modification-time output-file)))
	     (if (not output-time)
		 (list input-file)
		 (list-transform-positive (cons input-file dependencies)
		   (lambda (dependency)
		     (let ((dep-time (file-modification-time dependency)))
		       (if dep-time
			   (> dep-time output-time)
			   (begin
			     (warn "Missing dependency:"
				   (->namestring dependency))
			     #f)))))))))
      (if (not (null? reasons))
	  (begin
	    (newline)
	    (write-string ";Generating ")
	    (write (->namestring output-file))
	    (write-string " because of:")
	    (for-each (lambda (reason)
			(write-char #\space)
			(write (->namestring reason)))
		      reasons)
	    (processor input-file output-file dependencies)))))

  (set! compile-file
	(named-lambda (compile-file file #!optional dependencies syntax-table)
	  (process-file (scm-pathname file)
			(bin-pathname file)
			(map ext-pathname
			     (if (default-object? dependencies)
				 '()
				 dependencies))
	    (lambda (input-file output-file dependencies)
	      (fluid-let ((sf/default-syntax-table
			   (if (default-object? syntax-table)
			       #f
			       syntax-table))
			  (sf/default-declarations
			   `((USUAL-INTEGRATIONS
			      ,@compile-file-override-usual-integrations)
			     ,@(if (null? dependencies)
				   '()
				   `((INTEGRATE-EXTERNAL ,@dependencies))))))
		(sf input-file output-file))))
	  (if (not compile-file-sf-only?)
	      (process-file (bin-pathname file)
			    (com-pathname file)
			    '()
		(lambda (input-file output-file dependencies)
		  dependencies
		  (fluid-let ((compiler:coalescing-constant-warnings? #f))
		    (compile-bin-file input-file output-file))))))))

(with-working-directory-pathname (directory-pathname (current-load-pathname))
  (lambda ()
    (compile-file "class")
    (compile-file "instance" '() syntax-table/system-internal)
    (compile-file "macros")
    (compile-file "method")
    (compile-file "printer")
    (compile-file "slot")
    (cref/generate-constructors "sos")
    (sf "sos.con")
    (sf "sos.ldr")))