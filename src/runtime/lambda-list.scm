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
  (define (parse-required object seen)
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
			    (parse-required (cdr object) seen)))))))))
  (define (parse-optional object seen)
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
			    (parse-optional (cdr object) seen)))))))))
  (define (parse-rest object seen)
    (and (pair? object)
	 (parse-parameter (car object) seen
	   (lambda (seen)
	     seen
	     (null? (cdr object))))))
  (define (parse-parameter object seen k)
    (if (identifier? object)
	(and (not (memq object seen))
	     (k (cons object seen)))
	(and (pair? object)
	     (identifier? (car object))
	     (list? (cdr object))
	     (not (memq (car object) seen))
	     (k (cons (car object) seen)))))
  (parse-required object '()))

(define lambda-tag:optional (object-new-type (ucode-type constant) 3))
(define lambda-tag:rest (object-new-type (ucode-type constant) 4))
(define lambda-tag:key (object-new-type (ucode-type constant) 5))

(define (lambda-tag? object)
  (or (eq? object lambda-tag:key)
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

;;; #!aux is neither implemented nor used, so ignore it.
(define (make-lambda-list required optional rest #!optional aux)
  (declare (ignore aux))
  (guarantee list-of-unique-symbols? required)
  (guarantee list-of-unique-symbols? optional)
  (if rest (guarantee symbol? rest))
  (let ((rest-tail (if (not rest) '() rest)))
    (append required
	    (if (null? optional)
		rest-tail
		(cons lambda-tag:optional
		      (append optional rest-tail))))))

(define (parse-mit-lambda-list bvl)
  (let-values (((all required optional rest)
		(%parse-mit-lambda-list bvl 'parse-mit-lambda-list)))
    (declare (ignore all))
    (values (reverse! required)
	    (reverse! optional)
	    rest)))

(define (mit-lambda-list-names bvl)
  (let-values (((all required optional rest)
		(%parse-mit-lambda-list bvl 'mit-lambda-list-names)))
    (declare (ignore required optional rest))
    (reverse! all)))

(define (%parse-mit-lambda-list bvl caller)

  (define (do-reqs pattern all reqs)
    (do-rest pattern
	     all
	     (lambda (all rest)
	       (finish all reqs '() rest))
	     (lambda ()
	       (if (eq? (car pattern) lambda-tag:optional)
		   (if (pair? (cdr pattern))
		       (let ((id (do-id (cadr pattern))))
			 (do-opts (cddr pattern)
				  (cons id all)
				  reqs
				  (list id)))
		       (lose))
		   (let ((id (do-id (car pattern))))
		     (do-reqs (cdr pattern)
			      (cons id all)
			      (cons id reqs)))))))

  (define (do-opts pattern all reqs opts)
    (do-rest pattern
	     all
	     (lambda (all rest)
	       (finish all reqs opts rest))
	     (lambda ()
	       (let ((id (do-id (car pattern))))
		 (do-opts (cdr pattern)
			  (cons id all)
			  reqs
			  (cons id opts))))))

  (define (do-rest pattern all s f)
    (cond ((null? pattern)
	   (s all #f))
	  ((identifier? pattern)
	   (s (cons pattern all) pattern))
	  ((not (pair? pattern))
	   (lose))
	  ((eq? (car pattern) lambda-tag:rest)
	   (if (and (pair? (cdr pattern))
		    (null? (cddr pattern)))
	       (let ((id (do-id (cadr pattern))))
		 (s (cons id all) id))
	       (lose)))
	  (else
	   (f))))

  (define (do-id pattern)
    (cond ((identifier? pattern) pattern)
	  ((and (pair? pattern) (identifier? (car pattern))) (car pattern))
	  (else (lose))))

  (define (finish all reqs opts rest)
    (if (any-duplicates? all eq?)
	(lose)
	(values all reqs opts rest)))

  (define (lose)
    (error:not-a mit-lambda-list? bvl caller))

  (do-reqs bvl '() '()))