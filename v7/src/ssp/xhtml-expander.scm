#| -*-Scheme-*-

$Id: xhtml-expander.scm,v 1.2 2003/12/29 07:31:22 uid67408 Exp $

Copyright 2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; XHTML+Scheme expander

(declare (usual-integrations))

(define (expand-xhtml-directory directory)
  (for-each expand-xhtml-file (directory-read directory)))

(define (expand-xhtml-file input #!optional output)
  (let ((document
	 (read/expand-xml-file input
			       (make-expansion-environment input))))
    (let ((root (xml-document-root document)))
      (set-xml-element-contents!
       root
       (cons* "\n"
	      (make-xml-comment
	       (string-append
		" This document was automatically generated from \""
		(file-namestring input)
		"\"\n     on "
		(universal-time->local-time-string (get-universal-time))
		". "))
	      (xml-element-contents root))))
    (let ((output
	   (if (default-object? output)
	       (pathname-new-type input "html")
	       output)))
      ((if (output-port? output) write-xml write-xml-file)
       document output 'INDENT-DTD? #t))))

(define (read/expand-xml-file pathname environment)
  (with-working-directory-pathname (directory-pathname pathname)
    (lambda ()
      (fluid-let ((*sabbr-table* (make-eq-hash-table)))
	(read-xml-file pathname
		       `((scheme ,(pi-expander environment))
			 (svar ,svar-expander)
			 (sabbr ,sabbr-expander)))))))

(define (make-expansion-environment pathname)
  (let ((pathname (merge-pathnames pathname))
	(environment (extend-top-level-environment expander-environment)))
    (environment-define environment 'document-pathname pathname)
    (environment-define environment 'load
			(let ((directory (directory-pathname pathname)))
			  (lambda (pathname)
			    (load (merge-pathnames pathname directory)
				  environment))))
    environment))

(define ((pi-expander environment) text)
  (fluid-let ((*outputs* (cons '() '()))
	      (load/suppress-loading-message? #t))
    (let ((port (open-input-string text)))
      (let loop ()
	(let ((expression (read port)))
	  (if (not (eof-object? expression))
	      (begin
		(expander-eval expression environment)
		(loop))))))
    (car *outputs*)))

(define expander-eval eval)

(define (svar-expander text)
  (list (make-xml-element 'code '() (list (string-trim text)))))

(define (sabbr-expander text)
  (get-sabbr (intern (string-trim text))))

(define (define-sabbr name expansion)
  (hash-table/put! *sabbr-table* name (flatten expansion)))

(define (get-sabbr name)
  (let ((expansion (hash-table/get *sabbr-table* name 'NO-EXPANSION)))
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