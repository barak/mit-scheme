#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/butils.scm,v 4.4 1991/08/22 17:59:32 jinx Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; Build utilities

(declare (usual-integrations))

(define (directory-processor input-type output-type process-file)
  (let ((directory-read
	 (let ((input-pattern
		(make-pathname false false '() 'WILD input-type 'NEWEST)))
	   (lambda (directory)
	     (directory-read
	      (merge-pathnames (pathname-as-directory
				(->pathname directory))
			       input-pattern))))))
    (lambda (input-directory #!optional output-directory force?)
      (let ((output-directory
	     (if (default-object? output-directory) false output-directory))
	    (force? (if (default-object? force?) false force?)))
	(for-each (let ((output-directory-path
			 (and output-directory
			      (->pathname output-directory))))
		    (lambda (pathname)
		      (if (or force?
			      (not
			       (compare-file-modification-times
				(pathname-default-type pathname input-type)
				(let ((output-pathname
				       (pathname-new-type pathname
							  output-type)))
				  (if output-directory-path
				      (merge-pathnames output-directory-path
						       output-pathname)
				      output-pathname)))))
			  (process-file pathname output-directory))))
		  (if (pair? input-directory)
		      (mapcan directory-read input-directory)
		      (directory-read input-directory)))))))

(define sf-directory
  (directory-processor "scm" "bin"
		       (lambda (pathname output-directory)
			 (sf pathname output-directory))))

(define compile-directory
  (directory-processor "bin" "com"
		       (lambda (pathname output-directory)
			 (compile-bin-file pathname output-directory))))

(define sf-directory?)
(define compile-directory?)
(let ((show-pathname
       (lambda (pathname output-directory)
	 output-directory
	 (newline)
	 (write-string "Process file: ")
	 (write-string (pathname->string pathname)))))
  (set! sf-directory? (directory-processor "scm" "bin" show-pathname))
  (set! compile-directory? (directory-processor "bin" "com" show-pathname)))

(define (sf-conditionally filename #!optional echo-up-to-date?)
  (let ((kernel
	 (lambda (filename)
	   (cond ((not (file-processed? filename "scm" "bin"))
		  (sf filename))
		 ((and (not (default-object? echo-up-to-date?))
		       echo-up-to-date?)
		  (newline)
		  (write-string "Syntax file: ")
		  (write filename)
		  (write-string " is up to date"))))))
    (if (pair? filename)
	(for-each kernel filename)
	(kernel filename))))

(define (file-processed? filename input-type output-type)
  (let ((pathname (->pathname filename)))
    (compare-file-modification-times
     (pathname-default-type pathname input-type)
     (pathname-new-type pathname output-type))))

(define (compare-file-modification-times source target)
  (let ((source (file-modification-time source)))
    (and source
	 (let ((target (file-modification-time target)))
	   (and target
		(<= source target))))))