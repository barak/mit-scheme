#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Configuration and setup utilities

(declare (usual-integrations))

(load-option (quote CREF))

(define (generate-c-bundles bundles cc-arch)
  (for-each (lambda (bundle)
	      (generate-c-bundle bundle cc-arch))
	    (map write-to-string bundles)))

(define (generate-c-bundle bundle cc-arch)
  (with-notification (lambda (port)
		       (write-string "Generating bundle rule for " port)
		       (write-string bundle port))
    (lambda ()
      (let ((names (bundle-files bundle))
	    (so-file (string-append bundle ".so")))
	(receive (script-dir include-dir)
	    (cond ((eq? microcode-id/compiled-code-type 'C)
		   (let ((dir
			  (lambda (name)
			    (->namestring
			     (directory-pathname-as-file
			      (system-library-directory-pathname name))))))
		     (values (dir "")
			     (dir "include"))))
		  ((string-ci=? cc-arch "C")
		   (values "$(top_builddir)/microcode"
			   "$(top_builddir)/microcode"))
		  (else
		   (values #f #f)))
	  (call-with-output-file (string-append bundle "/Makefile-bundle")
	    (lambda (port)
	      (if script-dir
		  (begin
		    (newline port)
		    (write-rule port ".c.o")
		    (write-command port
				   (string-append "@" script-dir "/liarc-cc")
				   "$*.o"
				   "$*.c"
				   (string-append "-I" include-dir))
		    (newline port)
		    (let ((init-root (string-append bundle "-init")))
		      (write-rule port "compile-liarc-bundle" so-file)
		      (newline port)
		      (let ((prereqs
			     (cons (string-append init-root ".o")
				   (files+suffix names ".o"))))
			(write-rule port so-file prereqs)
			(write-command port
				       (string-append "@" script-dir "/liarc-ld")
				       "$@"
				       prereqs))
		      (newline port)
		      (let ((prereqs (files+suffix names ".c")))
			(write-rule port
				    (string-append init-root ".c")
				    prereqs)
			(write-command port
				       "$(top_srcdir)/etc/c-bundle.sh"
				       script-dir
				       "library"
				       init-root
				       prereqs))
		      (newline port)
		      (write-rule port "install-liarc-bundle" so-file)
		      (write-command port
				     "$(mkinstalldirs)"
				     "$(DESTDIR)$(AUXDIR)/lib")
		      (write-command port
				     "$(INSTALL_DATA)"
				     so-file
				     "$(DESTDIR)$(AUXDIR)/lib/.")
		      (newline port)
		      (write-rule port
				  ".PHONY"
				  "compile-liarc-bundle"
				  "install-liarc-bundle")
		      ))))))))))

(define (bundle-files bundle)
  (let ((pkg-name (if (string=? bundle "star-parser") "parser" bundle)))
    (cons (string-append pkg-name "-unx")
	  (sort (let ((names
		       (map ->namestring
			    (cref/package-files
			     (string-append bundle
					    "/"
					    pkg-name
					    ".pkg")
			     'unix))))
		  (cond ((or (string=? bundle "6001")
			     (string=? bundle "cref")
			     (string=? bundle "runtime")
			     (string=? bundle "sf"))
			 (cons "make" names))
			((string=? bundle "compiler")
			 (cons* (compiler-make-file)
				"base/make"
				names))
			((string=? bundle "edwin")
			 (cons* "make"
				"edwin"
				"rename"
				names))
			(else names)))
		string<?))))

(define (compiler-make-file)
  (string-append
   (or (file-symbolic-link? "compiler/machine")
       (error "Missing compiler/machine link."))
   "/make"))

(define (write-header output)
  (write-string "# This file automatically generated at " output)
  (write-string (universal-time->local-iso8601-string (get-universal-time))
		output)
  (write-string "." output)
  (newline output)
  (newline output))

(define (write-rule port lhs . rhs)
  (write-string lhs port)
  (write-string ":" port)
  (write-items (flatten-items rhs) port)
  (newline port))

(define (write-macro port lhs . rhs)
  (write-string lhs port)
  (write-string " =" port)
  (write-items (flatten-items rhs) port)
  (newline port))

(define (write-command port program . args)
  (write-char #\tab port)
  (write-string program port)
  (write-items (flatten-items args) port)
  (newline port))

(define (flatten-items items)
  (append-map (lambda (item)
		(if (list? item)
		    (flatten-items item)
		    (list item)))
	      items))

(define (write-items items port)
  (for-each (lambda (item)
	      (write-string " " port)
	      (write-item item port))
	    items))

(define (write-item item port)
  (if (>= (+ (output-port/column port)
	     (string-length item))
	  78)
      (begin
	(write-char #\\ port)
	(newline port)
	(write-char #\tab port)
	(write-string "  " port)))
  (write-string item port))

(define (files+suffix files suffix)
  (map (lambda (file)
	 (string-append file suffix))
       files))