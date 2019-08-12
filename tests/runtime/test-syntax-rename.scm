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

;;;; Test of identifier renaming

(declare (usual-integrations))

(define test-environment
  (the-environment))

(define-syntax outer
  (sc-macro-transformer
   (lambda (form use-env)
     (syntax-check '(_ identifier) form)
     (let* ((raw (cadr form))
	    (closed (close-syntax raw use-env)))
       `(define-syntax ,(close-syntax 'inner use-env)
	  (sc-macro-transformer
	   (lambda (form use-env)
	     (syntax-check '(_) form)
	     `(,(quote-identifier ,raw)
	       ,(quote ,raw)
	       ,(quote-identifier ,closed)
	       ,(quote ,closed)))))))))

;; A fairly complicated test that shows how quote-identifier works,
;; how it's different from quote, and that weird binding combinations
;; work as they should.
(define-test 'quoting
  (lambda ()
    (let ((expr
	   '(let ((car 13))
	      (outer car)
	      (let ((car 15))
		(car (inner))))))
      (assert-equal (unsyntax (syntax expr test-environment))
		    '(let ((.car.1 13))
		       (let ((.car.2 15))
			 (.car.2 (car car .car.1 car))))))))

(define-test 'keyword-environments
  (lambda ()
    (assert-equal (unsyntax
		   (syntax* '((let-syntax
				  ((foobar
				    (er-macro-transformer
				     (lambda (form rename compare)
				       `(,(rename 'define)
					 ,(cadr form)
					 ,(caddr form))))))
				(foobar a 3)
				(foobar b 4))

			      (define (c x)
				(+ a x)))
			    system-global-environment))
		  '(begin (define a 3) (define b 4) (define (c x) (+ a x))))))