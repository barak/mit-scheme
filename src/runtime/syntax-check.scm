#| -*-Scheme-*-

$Id: a6ac3237237fe28f43ece99666555090ab45ee45 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (syntax-check pattern form history)
  (if (not (syntax-match? (cdr pattern) (cdr form)))
      (syntax-error history "Ill-formed special form:" form)))

(define (ill-formed-syntax form)
  (call-with-syntax-error-procedure
   (lambda (syntax-error)
     (syntax-error "Ill-formed special form:" form))))

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

;;;; Lambda lists

(define (r4rs-lambda-list? object)
  (let loop ((object object) (seen '()))
    (or (null? object)
	(if (identifier? object)
	    (not (memq object seen))
	    (and (pair? object)
		 (identifier? (car object))
		 (not (memq (car object) seen))
		 (loop (cdr object) (cons (car object) seen)))))))

(define (mit-lambda-list? object)
  (letrec
      ((parse-required
	(lambda (object seen)
	  (or (null? object)
	      (if (identifier? object)
		  (not (memq object seen))
		  (and (pair? object)
		       (cond ((eq? (car object) lambda-optional-tag)
			      (and (pair? (cdr object))
				   (parse-parameter (cadr object) seen
				     (lambda (seen)
				       (parse-optional (cddr object) seen)))))
			     ((eq? (car object) lambda-rest-tag)
			      (parse-rest (cdr object) seen))
			     (else
			      (parse-parameter (car object) seen
				(lambda (seen)
				  (parse-required (cdr object) seen))))))))))
       (parse-optional
	(lambda (object seen)
	  (or (null? object)
	      (if (identifier? object)
		  (not (memq object seen))
		  (and (pair? object)
		       (cond ((eq? (car object) lambda-optional-tag)
			      #f)
			     ((eq? (car object) lambda-rest-tag)
			      (parse-rest (cdr object) seen))
			     (else
			      (parse-parameter (car object) seen
				(lambda (seen)
				  (parse-optional (cdr object) seen))))))))))
       (parse-rest
	(lambda (object seen)
	  (and (pair? object)
	       (parse-parameter (car object) seen
		 (lambda (seen)
		   seen
		   (null? (cdr object)))))))
       (parse-parameter
	(lambda (object seen k)
	  (if (identifier? object)
	      (and (not (memq object seen))
		   (k (cons object seen)))
	      (and (pair? object)
		   (identifier? (car object))
		   (list? (cdr object))
		   (not (memq (car object) seen))
		   (k (cons (car object) seen)))))))
    (parse-required object '())))

(define (parse-mit-lambda-list lambda-list)
  (let ((required (list '()))
	(optional (list '())))
    (define (parse-parameters cell pattern)
      (let loop ((pattern pattern))
	(cond ((null? pattern) (finish #f))
	      ((identifier? pattern) (finish pattern))
	      ((not (pair? pattern)) (bad-lambda-list pattern))
	      ((eq? (car pattern) lambda-rest-tag)
	       (if (and (pair? (cdr pattern)) (null? (cddr pattern)))
		   (cond ((identifier? (cadr pattern)) (finish (cadr pattern)))
			 ((and (pair? (cadr pattern))
			       (identifier? (caadr pattern)))
			  (finish (caadr pattern)))
			 (else (bad-lambda-list (cdr pattern))))
		   (bad-lambda-list (cdr pattern))))
	      ((eq? (car pattern) lambda-optional-tag)
	       (if (eq? cell required)
		   (parse-parameters optional (cdr pattern))
		   (bad-lambda-list pattern)))
	      ((identifier? (car pattern))
	       (set-car! cell (cons (car pattern) (car cell)))
	       (loop (cdr pattern)))
	      ((and (pair? (car pattern)) (identifier? (caar pattern)))
	       (set-car! cell (cons (caar pattern) (car cell)))
	       (loop (cdr pattern)))
	      (else (bad-lambda-list pattern)))))

    (define (finish rest)
      (let ((required (reverse! (car required)))
	    (optional (reverse! (car optional))))
	(do ((parameters
	      (append required optional (if rest (list rest) '()))
	      (cdr parameters)))
	    ((null? parameters))
	  (if (memq (car parameters) (cdr parameters))
	      (error "lambda list has duplicate parameter:"
		     (car parameters)
		     (error-irritant/noise " in")
		     lambda-list)))
	(values required optional rest)))

    (define (bad-lambda-list pattern)
      (error "Ill-formed lambda list:" pattern))

    (parse-parameters required lambda-list)))