#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; LAP Generator
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Constants

(define *next-constant*)
(define *interned-constants*)
(define *interned-variables*)
(define *interned-assignments*)
(define *interned-uuo-links*)
(define *interned-global-links*)
(define *interned-static-variables*)

(define (allocate-named-label prefix)
  (let ((label
	 (string->uninterned-symbol
	  (string-append prefix (number->string *next-constant*)))))
    (set! *next-constant* (+ *next-constant* 1))
    label))

(define (allocate-constant-label)
  (allocate-named-label "CONSTANT-"))

(define (warning-assoc obj pairs)
  (let ((pair
	 (call-with-current-continuation
	  (lambda (k)
	    (bind-condition-handler (list condition-type:unassigned-variable)
		(lambda (condition)
		  condition
		  (k #f))
	      (lambda ()
		(assoc obj pairs)))))))
    (if (and compiler:coalescing-constant-warnings?
	     (pair? pair)
	     (not (let ((obj* (car pair)))
		    (or (eqv? obj obj*)
			(and (string? obj)
			     (string? obj*)
			     (fix:= 0 (string-length obj))
			     (fix:= 0 (string-length obj*)))))))
	(warn "Coalescing two copies of constant object" obj))
    pair))

(define ((object->label find read write allocate-label) object)
  (let ((entry (find object (read))))
    (if entry
	(cdr entry)
	(let ((label (allocate-label object)))
	  (write (cons (cons object label) (read)))
	  label))))

(let-syntax
    ((->label
      (sc-macro-transformer
       (let ((pattern `(EXPRESSION IDENTIFIER ? ,string?)))
	 (lambda (form environment)
	   (if (syntax-match? pattern (cdr form))
	       (let ((find (close-syntax (cadr form) environment))
		     (var (close-syntax (caddr form) environment))
		     (suffix (and (pair? (cdddr form)) (cadddr form))))
		 `(OBJECT->LABEL ,find
				 (LAMBDA () ,var)
				 (LAMBDA (NEW)
				   (DECLARE (INTEGRATE NEW))
				   (SET! ,var NEW))
				 ,(if suffix
				      `(LAMBDA (OBJECT)
					 (ALLOCATE-NAMED-LABEL
					  (STRING-APPEND
					   (SYMBOL->STRING OBJECT)
					   ,suffix)))
				      `(LAMBDA (OBJECT)
					 OBJECT ; ignore
					 (ALLOCATE-NAMED-LABEL "OBJECT-")))))
	       (ill-formed-syntax form)))))))

(define constant->label
  (->label warning-assoc *interned-constants*))

(define free-reference-label
  (->label assq *interned-variables* "-READ-CELL-"))

(define free-assignment-label
  (->label assq *interned-assignments* "-WRITE-CELL-"))

(define free-static-label
  (->label assq *interned-static-variables* "-HOME-"))

)

;; These are different because different uuo-links are used for different
;; numbers of arguments.

(define (allocate-uuo-link-label prefix name frame-size)
  (allocate-named-label
   (string-append prefix
		  (symbol->string name)
		  "-"
		  (number->string (-1+ frame-size))
		  "-ARGS-")))

(define-integrable (uuo-link-label read write! prefix)
  (lambda (name frame-size)
    (let* ((all (read))
	   (entry (assq name all)))
      (if entry
	  (let ((place (assv frame-size (cdr entry))))
	    (if place
		(cdr place)
		(let ((label (allocate-uuo-link-label prefix name frame-size)))
		  (set-cdr! entry
			    (cons (cons frame-size label)
				  (cdr entry)))
		  label)))
	  (let ((label (allocate-uuo-link-label prefix name frame-size)))
	    (write! (cons (list name (cons frame-size label))
			  all))
	    label)))))

(define free-uuo-link-label
  (uuo-link-label (lambda () *interned-uuo-links*)
		  (lambda (new)
		    (set! *interned-uuo-links* new))
		  ""))

(define global-uuo-link-label
  (uuo-link-label (lambda () *interned-global-links*)
		  (lambda (new)
		    (set! *interned-global-links* new))
		  "GLOBAL-"))

(define (prepare-constants-block)
  (generate/constants-block *interned-constants*
			    *interned-variables*
			    *interned-assignments*
			    *interned-uuo-links*
			    *interned-global-links*
			    *interned-static-variables*))