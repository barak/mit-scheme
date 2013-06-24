#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;;; Lambda lists

(declare (usual-integrations))

(define (r4rs-lambda-list? object)
  (let loop ((object object) (seen '()))
    (or (null? object)
	(if (identifier? object)
	    (not (memq object seen))
	    (and (pair? object)
		 (identifier? (car object))
		 (not (memq (car object) seen))
		 (loop (cdr object) (cons (car object) seen)))))))

(define-guarantee r4rs-lambda-list "R4RS lambda list")

(define (parse-r4rs-lambda-list bvl)
  (let loop ((bvl* bvl) (required '()))
    (cond ((and (pair? bvl*)
		(identifier? (car bvl*)))
	   (loop (cdr bvl*)
		 (cons (car bvl*) required)))
	  ((null? bvl*)
	   (values (reverse! required) #f))
	  ((identifier? bvl*)
	   (values (reverse! required) bvl*))
	  (else
	   (error:not-r4rs-lambda-list bvl)))))

(define (map-r4rs-lambda-list procedure bvl)
  (let loop ((bvl* bvl))
    (cond ((and (pair? bvl*)
		(identifier? (car bvl*)))
	   (cons (procedure (car bvl*))
		 (loop (cdr bvl*))))
	  ((null? bvl*)
	   '())
	  ((identifier? bvl*)
	   (procedure bvl*))
	  (else
	   (error:not-r4rs-lambda-list bvl)))))

(define (mit-lambda-list? object)
  (letrec
      ((parse-required
	(lambda (object seen)
	  (or (null? object)
	      (if (identifier? object)
		  (not (memq object seen))
		  (and (pair? object)
		       (cond ((eq? (car object) lambda-tag:optional)
			      (and (pair? (cdr object))
				   (parse-parameter (cadr object) seen
				     (lambda (seen)
				       (parse-optional (cddr object) seen)))))
			     ((eq? (car object) lambda-tag:rest)
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
		       (cond ((eq? (car object) lambda-tag:optional)
			      #f)
			     ((eq? (car object) lambda-tag:rest)
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

(define-guarantee mit-lambda-list "MIT/GNU Scheme lambda list")

(define lambda-tag:optional (object-new-type (ucode-type constant) 3))
(define lambda-tag:rest (object-new-type (ucode-type constant) 4))
(define lambda-tag:key (object-new-type (ucode-type constant) 5))
(define lambda-tag:aux (object-new-type (ucode-type constant) 8))

(define (lambda-tag? object)
  (or (eq? object lambda-tag:aux)
      (eq? object lambda-tag:key)
      (eq? object lambda-tag:optional)
      (eq? object lambda-tag:rest)

      ;; The following ones are called `lambda-tag', but they are
      ;; semantically quite different from lambda list keywords.
      ;; This should be fixed some day.

      ;; From lambda.scm
      (eq? object lambda-tag:internal-lambda)
      (eq? object lambda-tag:internal-lexpr)

      ;; From syntax-output.scm
      (eq? object lambda-tag:fluid-let)
      (eq? object lambda-tag:let)
      (eq? object lambda-tag:unnamed)
      ))

(define (parse-mit-lambda-list lambda-list)
  (let ((required (list '()))
	(optional (list '())))
    (define (parse-parameters cell pattern)
      (let loop ((pattern pattern))
	(cond ((null? pattern) (finish #f))
	      ((identifier? pattern) (finish pattern))
	      ((not (pair? pattern)) (bad-lambda-list pattern))
	      ((eq? (car pattern) lambda-tag:rest)
	       (if (and (pair? (cdr pattern)) (null? (cddr pattern)))
		   (cond ((identifier? (cadr pattern)) (finish (cadr pattern)))
			 ((and (pair? (cadr pattern))
			       (identifier? (caadr pattern)))
			  (finish (caadr pattern)))
			 (else (bad-lambda-list (cdr pattern))))
		   (bad-lambda-list (cdr pattern))))
	      ((eq? (car pattern) lambda-tag:optional)
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
      (error:not-mit-lambda-list pattern 'PARSE-MIT-LAMBDA-LIST))

    (parse-parameters required lambda-list)))

(define (map-mit-lambda-list procedure bvl)
  (let loop ((bvl bvl))
    (if (pair? bvl)
	(cons (if (or (eq? (car bvl) lambda-tag:optional)
		      (eq? (car bvl) lambda-tag:rest))
		  (car bvl)
		  (procedure (car bvl)))
	      (loop (cdr bvl)))
	(if (identifier? bvl)
	    (procedure bvl)
	    '()))))