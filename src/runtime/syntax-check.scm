#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Syntax Checking
;;;  Based on a design by Alan Bawden.

(declare (usual-integrations))

(define (syntax-check pattern form)
  (if (not (syntax-match? (cdr pattern) (cdr form)))
      (ill-formed-syntax form)))

(define (ill-formed-syntax form)
  (syntax-error "Ill-formed special form:" form))

(define (syntax-match? pattern object)
  (let ((match-error
	 (lambda ()
	   (error:bad-range-argument pattern 'SYNTAX-MATCH?))))
    (cond ((procedure? pattern)
	   (pattern object))
	  ((symbol? pattern)
	   (case pattern
	     ((SYMBOL) (symbol? object))
	     ((IDENTIFIER) (identifier? object))
	     ((DATUM EXPRESSION FORM) #t)
	     ((R4RS-BVL) (r4rs-lambda-list? object))
	     ((MIT-BVL) (mit-lambda-list? object))
	     ((STRING) (string? object))
	     ((CHAR) (char? object))
	     ((URI) (->uri object #f))
	     ((INDEX) (exact-nonnegative-integer? object))
	     (else (match-error))))
	  ((pair? pattern)
	   (case (car pattern)
	     ((*)
	      (if (pair? (cdr pattern))
		  (let ((head (cadr pattern))
			(tail (cddr pattern)))
		    (let loop ((object object))
		      (or (and (pair? object)
			       (syntax-match? head (car object))
			       (loop (cdr object)))
			  (syntax-match? tail object))))
		  (match-error)))
	     ((+)
	      (if (pair? (cdr pattern))
		  (let ((head (cadr pattern))
			(tail (cddr pattern)))
		    (and (pair? object)
			 (syntax-match? head (car object))
			 (let loop ((object (cdr object)))
			   (or (and (pair? object)
				    (syntax-match? head (car object))
				    (loop (cdr object)))
			       (syntax-match? tail object)))))
		  (match-error)))
	     ((?)
	      (if (pair? (cdr pattern))
		  (or (and (pair? object)
			   (syntax-match? (cadr pattern) (car object))
			   (syntax-match? (cddr pattern) (cdr object)))
		      (syntax-match? (cddr pattern) object))
		  (match-error)))
	     ((QUOTE)
	      (if (and (pair? (cdr pattern))
		       (null? (cddr pattern)))
		  (eqv? (cadr pattern) object)
		  (match-error)))
	     (else
	      (and (pair? object)
		   (syntax-match? (car pattern) (car object))
		   (syntax-match? (cdr pattern) (cdr object))))))
	  (else
	   (eqv? pattern object)))))

(define (syntax-match?* patterns instance)
  (any (lambda (pattern)
	 (syntax-match? pattern instance))
       patterns))