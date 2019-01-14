#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

(define (guarded-system-loader package-name place #!optional filename)
  (let ((dirs
	 (let ((here (directory-pathname (current-load-pathname))))
	   `(,here
	     ,@(let ((d (pathname-directory here)))
		 (if (pair? (cdr d))
		     (list (pathname-new-directory here (except-last-pair d)))
		     '()))
	     ,@(let ((d (get-environment-variable "MITSCHEME_INF_DIRECTORY")))
		 (if d
		     (list d)
		     '())))))
	(files
	 (if (default-object? filename)
	     (list "make" "load")
	     (list filename))))
    (let ((try-dir
	   (lambda (base-dir)
	     (let ((dir
		    (pathname-as-directory
		     (merge-pathnames place
				      (pathname-as-directory base-dir)))))
	       (let file-loop ((files files))
		 (if (pair? files)
		     (let ((pathname (merge-pathnames (car files) dir)))
		       (if (file-loadable? pathname)
			   (values dir pathname)
			   (file-loop (cdr files))))
		     (values #f #f))))))
	  (finish
	   (lambda (dir pathname)
	     (with-working-directory-pathname dir
	       (lambda ()
		 (load pathname '(runtime))))))
	  (lose (lambda () (error "Unable to find package directory:" place))))
      (lambda ()
	(if (not (name->package package-name))
	    (if (condition?
		 (ignore-errors
		  (lambda ()
		    (load (merge-pathnames
			   place
			   (system-library-directory-pathname "lib"))))))
		(let dir-loop ((dirs dirs))
		  (if (not (pair? dirs))
		      (lose))
		  (receive (dir pathname) (try-dir (car dirs))
		    (if dir
			(finish dir pathname)
			(dir-loop (cdr dirs)))))
		(receive (dir pathname)
		    (try-dir
		     (let ((d (system-library-directory-pathname "lib")))
		       (pathname-new-directory d (except-last-pair
						  (pathname-directory d)))))
		  (if (not dir)
		      (lose))
		  (finish dir pathname))))))))

(define-load-option 'compiler
  (lambda () (load-option 'sf))
  (guarded-system-loader '(compiler) "compiler"
			 (if (eq? microcode-id/compiled-code-type 'C)
			     "machines/C/make"
			     "make")))

(define-load-option 'cref
  (guarded-system-loader '(cross-reference) "cref"))

(define-load-option 'edwin
  (guarded-system-loader '(edwin) "edwin"))

(define-load-option 'ffi
  (guarded-system-loader '(ffi) "ffi"))

(define-load-option 'imail
  (guarded-system-loader '(edwin imail) "imail"))

(define-load-option '*parser
  (guarded-system-loader '(runtime *parser) "star-parser"))

(define-load-option 'sf
  (guarded-system-loader '(scode-optimizer) "sf"))

(define-load-option 'sos
  (guarded-system-loader '(sos) "sos"))

(define-load-option 'ssp
  (guarded-system-loader '(runtime ssp) "ssp"))

(define-load-option 'student
  (guarded-system-loader '(student) "6001"))

(define-load-option 'win32
  (guarded-system-loader '(win32) "win32"))

(define-load-option 'x11
  (guarded-system-loader '(x11) "x11"))

(define-load-option 'x11-screen
  (guarded-system-loader '(edwin screen x11-screen) "x11-screen"))

(define-load-option 'xdoc
  (guarded-system-loader '(runtime ssp xdoc) "xdoc"))

(define-load-option 'xml
  (guarded-system-loader '(runtime xml) "xml"))

(further-load-options standard-load-options)