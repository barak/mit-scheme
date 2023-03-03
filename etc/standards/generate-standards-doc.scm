#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Generate texinfo standards support

(declare (usual-integrations))

(define (generate-standards.texi sfile smfile)
  (call-with-output-file sfile generate-standards-file)
  (call-with-output-file smfile generate-menu-entries))

(define (generate-standards-file port)
  (write-string "@node " port)
  (write-string top-node port)
  (write-string ", GNU Free Documentation License, Graphics, Top\n" port)
  (write-string "@chapter " port)
  (write-string top-node port)
  (newline port)
  (newline port)
  (write-string "@menu\n" port)
  (generate-menu-entries port)
  (write-string "@end menu\n" port)
  (let loop ((stds (doc-standards)) (prev top-node))
    (when (pair? stds)
      (generate-std-node (car stds)
			 (if (pair? (cdr stds))
			     (std-node-name (cadr stds))
			     " ")
			 prev
			 port)
      (loop (cdr stds)
	    (std-node-name (car stds))))))

(define (generate-menu-entries port)
  (for-each (lambda (std)
	      (write-string "* " port)
	      (write-string (std-node-name std) port)
	      (write-string "::\n" port))
	    (doc-standards)))

(define (doc-standards)
  (filter (lambda (std)
	    (or (srfi? std)
		(std-prop std 'node-name #f #f)))
	  standards))

(define (std-node-name std)
  (if (srfi? std)
      (string-append "SRFI " (srfi-number-string std))
      (std-prop std 'node-name cadr)))

(define top-node "Standards Support")

(define (generate-std-node std next-node prev-node port)
  (let ((node (std-node-name std))
	(support (std-prop std 'support cdr))
	(global (std-prop std 'global cdr))
	(libs (std-libraries std)))
    (newline port)
    (write-string "@node " port)
    (write-string node port)
    (write-string ", " port)
    (write-string next-node port)
    (write-string ", " port)
    (write-string prev-node port)
    (write-string ", " port)
    (write-string top-node port)
    (newline port)
    (write-string "@section " port)
    (write-string (std-title std) port)
    (newline port)
    (for-each (lambda (name)
		(write-string "@findex " port)
		(write name port)
		(newline port))
	      (std-all-names std))
    (newline port)
    (write-string "@cartouche\n" port)
    (write-string "@table @b\n" port)
    (write-string "@item Description\n" port)
    (wrap-line (std-description std) port)
    (write-string "@item URL\n" port)
    (write-string "@url{" port)
    (write-string (std-url std) port)
    (write-string "}\n" port)
    (write-string "@item Support\n" port)
    (write-string (case (car support)
		    ((full) "Fully")
		    ((partial) "Partially")
		    (else (error "Invalid support:" support)))
		  port)
    (write-string " supported" port)
    (if (pair? (cdr support))
	(begin
	  (write-string ", except:\n" port)
	  (write-string "@itemize @bullet\n" port)
	  (for-each (lambda (exception)
		      (write-string "@item\n" port)
		      (wrap-line (string-concatenate exception) port))
		    (cdr support))
	  (write-string "@end itemize\n" port))
	(write-string ".\n" port))
    (write-string "@item Libraries\n" port)
    (cond ((null? libs)
	   (write-string "No libraries.\n" port))
	  ((and (= 2 (length libs))
		(string? (cadr libs)))
	   (write-nicode (car libs) port)
	   (write-string " " port)
	   (write-string (cadr libs) port)
	   (newline port))
	  (else
	   (let loop ((libs libs) (col 0))
	     (when (pair? libs)
	       (write-nicode (car libs) port)
	       (if (and (pair? (cdr libs)) (= 3 col))
		   (write-string "@*" port))
	       (newline port)
	       (loop (cdr libs) (modulo (+ col 1) 4))))))
    (write-string "@item Global\n" port)
    (wrap-line
     (string-append (case (car global)
		      ((all) "All names are bound")
		      ((some) "Some bindings")
		      ((none) "No bindings")
		      (else (error "Invalid global:" std)))
		    " in the @mitgnu{} global environment."
		    (if (not (null? (cdr global)))
			(string-concatenate (cons "  " (cdr global)))
			""))
     port)
    (write-string "@end table\n" port)
    (write-string "@end cartouche\n" port)
    (format-bindings std port)))

(define (format-bindings std port)
  (let ((n-cols (std-prop std 'columns cadr #f))
	(bound (std-bound std))
	(unbound (std-unbound std))
	(unimplemented (std-unimplemented std)))
    (when (> (count (lambda (names) (not (null? names)))
		    (list bound unbound unimplemented))
	     0)
      (if (not (null? bound))
	  (format-names (and (not (and (null? unbound)
				       (null? unimplemented)))
			     'bound)
			bound
			n-cols
			port))
      (if (not (null? unbound))
	  (format-names 'unbound unbound n-cols port))
      (if (not (null? unimplemented))
	  (format-names 'unimplemented unimplemented n-cols port)))))

(define (format-names name-type names n-cols port)
  (if name-type
      (begin
	(write-string "\n@noindent\n" port)
	(write-string (label-for name-type) port)
	(write-string ":\n" port)))
  (newline port)
  (if (> (length names) 1)
      (let ((n-cols (or n-cols 2)))
	(write-string "@multitable @columnfractions " port)
	(write-string (case n-cols
			((2) ".5 .5")
			((3) ".33 .33 .33")
			(else (error "Invalid #cols:" n-cols)))
		      port)
	(newline port)
	(for-each (lambda (col name)
		    (write-string (if (= 0 col) "@item" "@tab") port)
		    (write-string " " port)
		    (write-nicode name port)
		    (newline port))
		  (columns n-cols (length names))
		  names)
	(write-string "@end multitable\n" port))
      (begin
	(write-string "@noindent\n" port)
	(write-nicode (car names) port)
	(newline port))))

(define (columns n-cols n-items)
  (generator->list (apply circular-generator (iota n-cols))
		   n-items))

(define (write-nicode name port)
  (write-string "@nicode{" port)
  (write name port)
  (write-string "}" port))

(define (label-for name-type)
  (case name-type
    ((bound) "Names that are bound in the @mitgnu{} global environment")
    ((unbound) "Names that are not bound in the @mitgnu{} global environment")
    ((unimplemented) "Unimplemented names")
    (else (error "Invalid name type:" name-type))))

(define (widest names)
  (fold (lambda (name width)
          (max (string-length (symbol->string name)) width))
        0
        names))

(define (wrap-line string port)
  (let loop
      ((words (word-splitter string))
       (col 0)
       (start-sentence? #f))
    (if (pair? words)
	(let* ((word (car words))
	       (spacer
		(cond ((= 0 col) "")
		      (start-sentence? "  ")
		      (else " ")))
	       (n (string-length word))
	       (col* (+ col (string-length spacer) n))
	       (start-sentence?*
		(or (string-suffix? "." word)
		    (string-suffix? "?" word)
		    (string-suffix? "!" word))))
	  (if (<= col* 70)
	      (begin
		(write-string spacer port)
		(write-string word port)
		(loop (cdr words) col* start-sentence?*))
	      (begin
		(newline port)
		(write-string word port)
		(loop (cdr words) n start-sentence?*))))))
  (newline port))

(define word-splitter
  (string-splitter))