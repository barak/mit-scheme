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

;;;; XHTML+Scheme expander

(declare (usual-integrations))

(define-mime-handler '(application/xhtml+xml "xhtml" "ssp")
  (lambda (pathname port)
    (expand-xhtml-file pathname port)))

(define (expand-xhtml-file pathname port)
  (let ((document
	 (read/expand-xml-file pathname
			       (make-expansion-environment pathname))))
    (if (not (xml-document-dtd document))
	(begin
	  (set-xml-document-dtd! document html-1.0-dtd)
	  (set-xml-document-misc-2! document
				    (cons "\n"
					  (xml-document-misc-2 document)))))
    (let ((root (xml-document-root document)))
      (if (not (find-xml-attr 'xmlns root))
	  (set-xml-element-attributes!
	   root
	   (cons (make-xml-attribute 'xmlns html-uri)
		 (xml-element-attributes root))))
      (set-xml-element-contents!
       root
       (cons* "\n"
	      (make-xml-comment
	       (string-append
		" This document was automatically generated from \""
		(file-namestring pathname)
		"\"\n     on "
		(universal-time->local-time-string (get-universal-time))
		". "))
	      (xml-element-contents root))))
    (if (in-mod-lisp?)
	(http-response-header 'content-type
			      (string-append (html-content-type)
					     "; charset="
					     (xml-document-charset document))))
    (write-xml document port 'INDENT-DTD? #t)))

(define (xml-document-charset document)
  (or (let ((decl (xml-document-declaration document)))
	(and decl
	     (xml-declaration-encoding decl)))
      "utf-8"))

(define (read/expand-xml-file pathname environment)
  (let ((pathname (merge-pathnames pathname)))
    (with-working-directory-pathname (directory-pathname pathname)
      (lambda ()
	(parameterize ((current-load-pathname pathname)
		       (current-load-environment environment))
	  (fluid-let ((*sabbr-table* (make-strong-eq-hash-table)))
	    (read-xml-file pathname
			   `((scheme ,(pi-expander environment))
			     (svar ,svar-expander)
			     (sabbr ,sabbr-expander)))))))))

(define (make-expansion-environment pathname)
  (let ((environment (extend-top-level-environment expander-environment)))
    (environment-define environment
			'document-pathname
			(merge-pathnames pathname))
    environment))

(define ((pi-expander environment) text)
  (fluid-let ((*outputs* (cons '() '())))
    (parameterize ((param:suppress-loading-message? #t))
      (let ((port (open-input-string text)))
	(let loop ()
	  (let ((expression (read port)))
	    (if (not (eof-object? expression))
		(begin
		  (expander-eval expression environment)
		  (loop)))))))
    (car *outputs*)))

(define expander-eval eval)

(define (svar-expander text)
  (list (make-xml-element 'code '() (list (string-trim text)))))

(define (sabbr-expander text)
  (get-sabbr (intern (string-trim text))))

(define (define-sabbr name expansion)
  (hash-table-set! *sabbr-table* name (flatten expansion)))

(define (get-sabbr name)
  (let ((expansion (hash-table-ref/default *sabbr-table* name 'NO-EXPANSION)))
    (if (eq? expansion 'NO-EXPANSION)
	(error "Invalid sabbr name:" name))
    expansion))

(define (emit . content)
  (emit* content *outputs*))

(define (emit* content q)
  (let ((tail (flatten content)))
    (if (pair? tail)
	(begin
	  (if (pair? (cdr q))
	      (set-cdr! (cdr q) tail)
	      (set-car! q tail))
	  (set-cdr! q (last-pair tail))))))

(define (flatten items)
  (cond ((pair? items) (append-map! flatten items))
	((null? items) '())
	(else (list items))))

(define *outputs*)
(define *sabbr-table*)