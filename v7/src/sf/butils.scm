#| -*-Scheme-*-

$Id: butils.scm,v 4.10 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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

;;;; Build utilities

(declare (usual-integrations))

(define (directory-processor input-type output-type process-file)
  (let ((directory-read
	 (let ((input-pattern
		(make-pathname false false false 'WILD input-type 'NEWEST)))
	   (lambda (directory)
	     (directory-read
	      (merge-pathnames
	       (pathname-as-directory (merge-pathnames directory))
	       input-pattern))))))
    (lambda (input-directory #!optional output-directory force?)
      (let ((output-directory
	     (if (default-object? output-directory) false output-directory))
	    (force? (if (default-object? force?) false force?))
	    (output-type (output-type)))
	(for-each (lambda (pathname)
		    (if (or force?
			    (not (compare-file-modification-times
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
	       (cond ((not (compare-file-modification-times input output))
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

(define (file-processed? filename input-type output-type)
  (compare-file-modification-times
   (pathname-default-type filename input-type)
   (pathname-new-type filename output-type)))

(define (compare-file-modification-times source target)
  (let ((source (file-modification-time-indirect source)))
    (and source
	 (let ((target (file-modification-time-indirect target)))
	   (and target
		(<= source target))))))