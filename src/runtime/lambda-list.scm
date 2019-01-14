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

(define (fold-r4rs-lambda-list procedure initial bvl)
  (let loop ((bvl* bvl))
    (cond ((and (pair? bvl*) (identifier? (car bvl*)))
	   (procedure (car bvl*) (loop (cdr bvl*))))
	  ((null? bvl*) (initial #f))
	  ((identifier? bvl*) (initial bvl*))
	  (else (error:not-a r4rs-lambda-list? bvl)))))

(define (parse-r4rs-lambda-list bvl)
  (let ((parsed
	 (fold-r4rs-lambda-list (lambda (var parsed)
				  (cons (cons var (car parsed))
					(cdr parsed)))
				(lambda (var)
				  (cons '() var))
				bvl)))
    (values (car parsed) (cdr parsed))))

(define (r4rs-lambda-list-names bvl)
  (fold-r4rs-lambda-list cons
			 (lambda (var)
			   (if var
			       (list var)
			       '()))
			 bvl))

(define (r4rs-lambda-list-arity bvl)
  (let ((arity
	 (fold-r4rs-lambda-list (lambda (var arity)
				  (declare (ignore var))
				  (cons (fix:+ 1 (car arity))
					(and (cdr arity)
					     (fix:+ 1 (cdr arity)))))
				(lambda (var)
				  (cons 0 (if var #f 0)))
				bvl)))
    (make-procedure-arity (car arity) (cdr arity) #t)))

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
	   (error:not-a r4rs-lambda-list? bvl)))))

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
      (eq? object scode-lambda-name:internal-lambda)

      ;; From syntax-output.scm
      (eq? object scode-lambda-name:fluid-let)
      (eq? object scode-lambda-name:let)
      (eq? object scode-lambda-name:unnamed)
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
      (error:not-a mit-lambda-list? pattern 'parse-mit-lambda-list))

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

;;; Aux is almost always the empty list.
(define (make-lambda-list required optional rest aux)
  (guarantee list-of-unique-symbols? required)
  (guarantee list-of-unique-symbols? optional)
  (if rest
      (guarantee symbol? rest))
  (guarantee list-of-unique-symbols? aux)
  (let ((rest-aux-tail (if (not rest)
			   (if (null? aux)
			       '()
			       (cons lambda-tag:aux aux))
			   (if (null? aux)
			       rest
			       (cons* lambda-tag:rest rest
				      lambda-tag:aux aux)))))
    (append required
	    (if (null? optional)
		rest-aux-tail
		(cons lambda-tag:optional
		      (append optional rest-aux-tail))))))