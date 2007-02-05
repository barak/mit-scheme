#| -*-Scheme-*-

$Id: butils.scm,v 4.16 2007/01/05 21:19:29 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

;;;; Build utilities

(declare (usual-integrations))

(define (directory-processor input-type output-type process-file)
  (let ((directory-read
	 (let ((input-pattern
		(make-pathname #f #f #f 'WILD input-type 'NEWEST)))
	   (lambda (directory)
	     (directory-read
	      (merge-pathnames
	       (pathname-as-directory (merge-pathnames directory))
	       input-pattern))))))
    (lambda (input-directory #!optional output-directory force?)
      (let ((output-directory
	     (if (default-object? output-directory) #f output-directory))
	    (force? (if (default-object? force?) #f force?))
	    (output-type (output-type)))
	(for-each (lambda (pathname)
		    (if (or force?
			    (not (file-modification-time<=?
				  (pathname-default-type pathname input-type)
				  (let ((output-pathname
					 (pathname-new-type pathname
							    output-type)))
				    (if output-directory
					(merge-pathnames output-directory
							 output-pathname)
					output-pathname)))))
			(process-file pathname output-directory)))
		  (if (pair? input-directory)
		      (append-map! directory-read input-directory)
		      (directory-read input-directory)))))))

(define sf-directory
  (directory-processor
   "scm"
   (lambda () "bin")
   (lambda (pathname output-directory)
     (sf pathname output-directory))))

(define compile-directory
  (directory-processor
   "bin"
   (lambda ()
     (if (environment-lookup (->environment '(compiler))
			     'compiler:cross-compiling?)
	 "moc"
	 (environment-lookup (->environment '(compiler top-level))
						 
			     'compiled-output-extension)))
   (lambda (pathname output-directory)
     (compile-bin-file pathname output-directory))))

(define sf-directory?)
(define compile-directory?)
(let ((show-pathname
       (lambda (pathname output-directory)
	 output-directory
	 (newline)
	 (write-string "Process file: ")
	 (write-string (enough-namestring pathname)))))
  (set! sf-directory? (directory-processor "scm" "bin" show-pathname))
  (set! compile-directory? (directory-processor "bin" "com" show-pathname)))

(define (sf-conditionally filename #!optional echo-up-to-date?)
  (let ((kernel
	 (lambda (filename)
	   (call-with-values
	       (lambda () (sf/pathname-defaulting filename #f #f))
	     (lambda (input output spec)
	       spec
	       (cond ((not (file-modification-time<=? input output))
		      (sf filename))
		     ((and (not (default-object? echo-up-to-date?))
			   echo-up-to-date?)
		      (newline)
		      (write-string "Syntax file: ")
		      (write filename)
		      (write-string " is up to date"))))))))
    (if (pair? filename)
	(for-each kernel filename)
	(kernel filename))))