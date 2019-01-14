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

;;;; ISO 8859 <-> Unicode character mapping conversion

(declare (usual-integrations))

(define (read-iso-8859-directory directory)
  (let ((directory (pathname-as-directory directory)))
    (let loop ((pathnames (directory-read directory)))
      (if (pair? pathnames)
	  (let ((pathname (car pathnames)))
	    (let ((name (pathname-name pathname)))
	      (if (re-string-match "\\`8859-[0-9]+\\'" name)
		  (cons (list (intern (string-append "ISO-" name))
			      (read-iso-8859-file pathname))
			(loop (cdr pathnames)))
		  (loop (cdr pathnames)))))
	  '()))))

(define (read-iso-8859-file pathname)
  (call-with-input-file pathname
    (lambda (port)
      (let ((v (make-vector #x100 #f))
	    (re
	     (rexp-compile
	      (let ((hex (string->char-set "0123456789abcdefABCDEF")))
		(rexp-sequence (rexp-string-start)
			       "0x" (rexp-group hex hex)
			       "\t0x" (rexp-group hex hex hex hex)
			       "\t"))))
	    (hex
	     (lambda (line regs i)
	       (string->number (re-match-extract line regs i) 16))))
	(let loop ()
	  (let ((line (read-line port)))
	    (if (not (eof-object? line))
		(let ((regs (re-string-match re line)))
		  (if regs
		      (let ((i (hex line regs 1))
			    (j (hex line regs 2)))
			(let ((c (integer->char j)))
			  (if (vector-ref v i)
			      (error "Character defined:" i c)
			      (vector-set! v i c)))))
		  (loop)))))
	v))))