#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v8/src/compiler/etc/xcbfdir.scm,v 1.7 1991/05/27 17:56:12 jinx Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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

;;;; Distributed directory recompilation.

(declare (usual-integrations))

(define (process-directory directory processor extension)
  (for-each
   (lambda (pathname)
     (let ((one (pathname-new-type pathname extension))
	   (two (pathname-new-type pathname "tch")))
       (call-with-current-continuation
	(lambda (here)
	  (bind-condition-handler (list condition-type:error)
	      (lambda (condition)
		(let ((port (current-output-port)))
		  (newline port)
		  (write-string ";; *** Aborting " port)
		  (display pathname port)
		  (write-string " ***" port)
		  (newline port)
		  (write-condition-report condition port)
		  (newline port))
		(here 'next))
	    (lambda ()
	      (let ((touch-created-file? false))
		(dynamic-wind
		 (lambda ()
		   ;; file-touch returns #T if the file did not exist,
		   ;; #F if it did.
		   (set! touch-created-file? (file-touch two))
		   unspecific)
		 (lambda ()
		   (if (and touch-created-file?
			    (let ((one-time (file-modification-time one)))
			      (or (not one-time)
				  (< one-time
				     (file-modification-time pathname)))))
		       (processor pathname
				  (pathname-new-type pathname extension))))
		 (lambda ()
		   (if touch-created-file?
		       (delete-file two)))))))))))
   (directory-read
    (merge-pathnames (pathname-as-directory (->pathname directory))
		     (->pathname "*.bin")))))

(define (recompile-directory dir)
  (process-directory dir compile-bin-file "com"))

(define (cross-compile-directory dir)
  (process-directory dir cross-compile-bin-file "moc"))