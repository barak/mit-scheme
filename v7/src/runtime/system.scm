#| -*-Scheme-*-

$Id: system.scm,v 14.17 2007/01/05 15:33:10 cph Exp $

Copyright 1986,1987,1988,1989,1991,1998 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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
				 'ADD-SUBSYSTEM-IDENTIFICATION!))
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
  (find-matching-item subsystem-identifications
    (lambda (entry)
      (match-entry? name entry))))

(define (match-entry? name entry)
  (let ((s (car entry)))
    (substring-ci=? name 0 (string-length name)
		    s 0
		    (or (string-find-next-char s #\space)
			(string-length s)))))

(define subsystem-identifications '())