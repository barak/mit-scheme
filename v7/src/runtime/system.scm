#| -*-Scheme-*-

$Id: system.scm,v 14.10 1998/02/12 05:56:48 cph Exp $

Copyright (c) 1988-98 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Subsystem Identification
;;; package: (runtime system)

(declare (usual-integrations))

(define (add-subsystem-identification! name version)
  (if (not (and (string? name) (not (string-null? name))))
      (error:wrong-type-argument name "non-null string"
				 'ADD-SUBSYSTEM-IDENTIFICATION!))
  (let ((version
	 (let loop ((version version))
	   (append-map
	    (lambda (version)
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
    (remove-subsystem-identification! name)
    (set! subsystem-identifications
	  (append! subsystem-identifications (list (cons name version)))))
  unspecific)

(define (remove-subsystem-identification! name)
  (let loop ((previous #f) (entries subsystem-identifications))
    (if (not (null? entries))
	(if (match-entry? name (car entries))
	    (begin
	      (if previous
		  (set-cdr! previous (cdr entries))
		  (set! subsystem-identifications (cdr entries)))
	      (loop previous (cdr entries)))
	    (loop entries (cdr entries))))))

(define (get-subsystem-names)
  (map car subsystem-identifications))

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
  (if (null? version)
      ""
      (let loop ((version version))
	(let ((s
	       (if (string? (car version))
		   (car version)
		   (number->string (car version)))))
	  (if (null? (cdr version))
	      s
	      (string-append s "." (loop (cdr version))))))))

(define (find-entry name)
  (list-search-positive subsystem-identifications
    (lambda (entry)
      (match-entry? name entry))))

(define (match-entry? name entry)
  (let ((s (car entry)))
    (substring-ci=? name 0 (string-length name)
		    s 0
		    (or (string-find-next-char s #\space)
			(string-length s)))))

(define subsystem-identifications '())

;;; Upwards compatibility.

(define (add-identification! name version modification)
  (add-subsystem-identification! name (list version modification)))