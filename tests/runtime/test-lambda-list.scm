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

;;;; Tests of lambda-list operations

(declare (usual-integrations))

(define r4rs-valid-bvls
  '((() () () #f ())
    ((a) (a) () #f (a))
    ((a b) (a b) () #f (a b))
    (a () () a (a))
    ((a . b) (a) () b (a b))
    ((a b . c) (a b) () c (a b c))))

(define mit-valid-bvls
  '(((#!optional a) () (a) #f (a))
    ((#!optional a b) () (a b) #f (a b))
    ((#!optional a . b) () (a) b (a b))
    ((#!optional a #!rest b) () (a) b (a b))
    ((#!rest a) () () a (a))
    ((a #!optional b) (a) (b) #f (a b))
    ((a #!optional b . c) (a) (b) c (a b c))
    ((a #!optional b #!rest c) (a) (b) c (a b c))
    ((a #!rest b) (a) () b (a b))
    ((a b #!rest c) (a b) () c (a b c))))

(define invalid-bvls
  '(#f
    1
    (1)
    (a 2)
    (a . 3)
    #!optional
    (a #!optional)
    (a #!optional 1)
    (a #!optional b 1)
    (a #!optional b #!optional)
    (a #!optional b #!optional c)
    #!rest
    (#!rest)
    (#!rest 2)
    (#!rest a b)
    (#!rest a #!optional b)))

(define-test 'r4rs-valid
  (map (lambda (e)
	 (apply (lambda (bvl reqs opts rest all)
		  (declare (ignore opts))
		  (lambda ()
		    (assert-true (r4rs-lambda-list? bvl))
		    (let-values (((a b) (parse-r4rs-lambda-list bvl)))
		      (assert-equal a reqs)
		      (assert-equal b rest))
		    (assert-equal (r4rs-lambda-list-names bvl) all)))
		e))
       r4rs-valid-bvls))

(define-test 'r4rs-invalid
  (map (lambda (bvl)
	 (lambda ()
	   (assert-false (r4rs-lambda-list? bvl))
	   (assert-error (lambda () (parse-r4rs-lambda-list bvl)))
	   (assert-error (lambda () (r4rs-lambda-list-names bvl)))))
       (append (map car mit-valid-bvls) invalid-bvls)))

(define-test 'mit-valid
  (map (lambda (e)
	 (apply (lambda (bvl reqs opts rest all)
		  (lambda ()
		    (assert-true (mit-lambda-list? bvl))
		    (let-values (((a b c) (parse-mit-lambda-list bvl)))
		      (assert-equal a reqs)
		      (assert-equal b opts)
		      (assert-equal c rest))
		    (assert-equal (mit-lambda-list-names bvl) all)))
		e))
       (append r4rs-valid-bvls mit-valid-bvls)))

(define-test 'mit-invalid
  (map (lambda (bvl)
	 (lambda ()
	   (assert-false (mit-lambda-list? bvl))
	   (assert-error (lambda () (parse-mit-lambda-list bvl)))
	   (assert-error (lambda () (mit-lambda-list-names bvl)))))
       invalid-bvls))