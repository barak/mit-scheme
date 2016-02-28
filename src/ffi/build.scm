#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016
    Massachusetts Institute of Technology

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

;;;; Build Utilities
;;; package: (ffi build)

(define (write-file name writer)
  (let ((tmp (pathname-new-type name "tmp")))
    (call-with-exclusive-output-file tmp writer)
    (rename-file tmp name)))

(define (rewrite-file name rewriter)
  (write-file
   name
   (lambda (out)
     (call-with-input-file name
       (lambda (in)
	 (rewriter in out))))))

(define (update-optiondb directory)
  (rewrite-file
   (merge-pathnames "optiondb.scm" directory)
   (lambda (in out)
     (do ((line (read-line in) (read-line in)))
	 ((or (eof-object? line)
	      (string-prefix? "(further-load-options" line))
	  (if (not (eof-object? line))
	      (begin
		(write-string line out)
		(newline out))))
       (write-string line out)
       (newline out))
     (write-string
      (string-append ";;; DO NOT EDIT the remainder of this file."
		     "  Any edits will be clobbered."
		     "\n") out)
     (for-each
       (lambda (name)
	 (write-string "\n(define-load-option '" out)
	 (write-string name out)
	 (write-string "\n  (standard-system-loader \"" out)
	 (write-string name out)
	 (write-string "\"))\n" out))
       ;; plugin-names
       (sort
	(let loop ((files (directory-read directory))
		   (names '()))
	  (if (pair? files)
	      (loop (cdr files)
		    (if (and (file-directory? (car files))
			     ;; The only core subsystem with a make.scm:
			     (not (string=? "ffi" (pathname-name (car files))))
			     (file-exists?
			      (merge-pathnames "make.scm"
					       (pathname-as-directory
						(car files)))))
			(cons (pathname-name (car files)) names)
			names))
	      names))
	string<?)))))

(define (update-html-index directory)
  ;;(parameterize* (list (cons param:suppress-loading-message? #t)
  ;;  (lambda () (load-option 'XML)))
  (rewrite-file
   (merge-pathnames "index.html" directory)
   (lambda (in out)
     (do ((line (read-line in) (read-line in)))
	 ((or (eof-object? line)
	      (string-prefix? "<ul id=\"plugins\"" line))
	  (if (not (eof-object? line))
	      (begin
		(write-string line out)
		(newline out))))
       (write-string line out)
       (newline out))
     (write-string (string-append "<!-- DO NOT EDIT this list."
				  "  Any edits will be clobbered. -->\n") out)
     (for-each
       (lambda (name.title)
	 (write-string "<li><a href=\"" out)
	 (write-string (car name.title) out)
	 (write-string "\">" out)
	 (write-string (cdr name.title) out)
	 (write-string "</a></li>\n" out))
       (sort
	(let loop ((files (directory-read directory))
		   (names.titles '()))
	  (if (pair? files)
	      (loop (cdr files)
		    (if (and (pathname-type (car files))
			     (string=? "html" (pathname-type (car files)))
			     (string-prefix? "mit-scheme-"
					     (pathname-name (car files))))
			(let ((name (string-tail (pathname-name (car files))
						 (string-length "mit-scheme-")))
			      (title (read-html-title (car files))))
			  (cons (cons name title) names.titles))
			names.titles))
	      (if (pair? names.titles)
		  names.titles
		  (begin
		    (write-string "<li><i>None currently installed.</i></li>\n"
				  out)
		    '()))))
	(lambda (a b) (string<? (car a) (car b)))))
     ;; Skip old list.
     (do ((line (read-line in) (read-line in)))
	 ((or (eof-object? line)
	      (string-prefix? "</ul>" line))
	  (if (eof-object? line)
	      (error "Premature end of HTML index.")
	      (begin
		(write-string line out)
		(newline out)))))
     ;; Copy the rest.
     (do ((line (read-line in) (read-line in)))
	 ((eof-object? line))
       (write-string line out)
       (newline out)))))

(define (read-html-title pathname)
  (call-with-input-file pathname
    (lambda (in)
      (let loop ()
	(let ((line (read-line in)))
	  (if (eof-object? line)
	      (error "Could not find HTML title:" pathname)
	      (let ((regs (re-string-match "<title>\\(.*\\)</title>" line)))
		(if (not regs)
		    (loop)
		    (re-match-extract line regs 1)))))))))