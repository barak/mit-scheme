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

;;;; Subsystem Identification
;;; package: (runtime system)

(declare (usual-integrations))

(define (add-subsystem-identification! name version)
  (if (not (and (string? name) (not (string-null? name))))
      (error:wrong-type-argument name "non-null string"
				 'add-subsystem-identification!))
  (let ((version
	 (let loop ((version version))
	   (append-map (lambda (version)
			 (cond ((exact-nonnegative-integer? version)
				(list version))
			       ((string? version)
				(if (string-null? version)
				    '()
				    (list version)))
			       ((list? version)
				(loop version))
			       (else
				(error "Illegal subsystem version:"
				       version))))
		       version))))
    (let ((entry (find-entry name)))
      (if entry
	  (begin
	    (set-car! entry name)
	    (set-cdr! entry version))
	  (begin
	    (set! subsystem-identifications
		  (append! subsystem-identifications
			   (list (cons name version))))
	    unspecific)))))

(define (remove-subsystem-identification! name)
  (let loop ((previous #f) (entries subsystem-identifications))
    (if (pair? entries)
	(if (match-entry? name (car entries))
	    (begin
	      (if previous
		  (set-cdr! previous (cdr entries))
		  (set! subsystem-identifications (cdr entries)))
	      (loop previous (cdr entries)))
	    (loop entries (cdr entries))))))

(define (get-subsystem-names)
  (map (lambda (entry)
	 (let ((s (car entry)))
	   (let ((i (string-find-next-char s #\space)))
	     (if i
		 (string-head s i)
		 s))))
       subsystem-identifications))

(define (get-subsystem-version name)
  (let ((entry (find-entry name)))
    (and entry
	 (list-copy (cdr entry)))))

(define (get-subsystem-version-string name)
  (let ((entry (find-entry name)))
    (and entry
	 (version-string (cdr entry)))))

(define (get-subsystem-identification-string name)
  (let ((entry (find-entry name)))
    (and entry
	 (let ((name (car entry))
	       (s (version-string (cdr entry))))
	   (and s
		(if (string-null? s)
		    (string-copy name)
		    (string-append name " " s)))))))

(define (version-string version)
  (if (pair? version)
      (let loop ((version version))
	(let ((s
	       (if (string? (car version))
		   (car version)
		   (number->string (car version)))))
	  (if (pair? (cdr version))
	      (string-append s "." (loop (cdr version)))
	      s)))
      ""))

(define (find-entry name)
  (find (lambda (entry)
	  (match-entry? name entry))
	subsystem-identifications))

(define (match-entry? name entry)
  (let ((s (car entry)))
    (string-ci=? name
		 (let ((space (string-find-next-char s #\space)))
		   (if space
		       (string-slice s 0 space)
		       s)))))

(define subsystem-identifications '())