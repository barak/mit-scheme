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

;;;; Tests of error reporting

(define-test 'r7rs-error-extension
  (lambda ()

    (define-record-type <foo>
	(make-foo r7rs-error-message r7rs-error-irritants)
	foo?
      (r7rs-error-message foo-message)
      (r7rs-error-irritants foo-irritants))

    (define foo (make-foo "foo:" '(a b c)))

    (assert-true (error-object? foo))
    (assert-equal (error-object-message foo) (foo-message foo))
    (assert-equal (error-object-irritants foo) (foo-irritants foo))

    (define (test-condition condition)
      (assert-true (r7rs-error-condition? condition))
      (assert-eq (access-condition condition 'object) foo)
      (assert-equal (condition/report-string condition) "foo: a b c"))

    (test-condition (ignore-errors (lambda () (raise foo))))
    (test-condition (ignore-errors (lambda () (raise-continuable foo))))

    (assert-true (call-with-current-continuation
		   (lambda (k)
		     (with-exception-handler
			 (lambda (object)
			   (assert-eq object foo)
			   (k #t))
		       (lambda ()
			 (raise foo)
			 #f)))))

    (assert-error (lambda ()
		    (with-exception-handler
			(lambda (object)
			  (assert-eq object foo)
			  #t)
		      (lambda ()
			(raise foo)
			#f)))
		  r7rs-error-condition?)

    (assert-true (call-with-current-continuation
		   (lambda (k)
		     (with-exception-handler
			 (lambda (object)
			   (assert-eq object foo)
			   (k #t))
		       (lambda ()
			 (raise-continuable foo)
			 #f)))))

    (assert-eq (with-exception-handler
		   (lambda (object)
		     (assert-eq object foo)
		     'continue)
		 (lambda ()
		   (assert-eq (raise-continuable foo) 'continue)
		   'done))
	       'done)

    ))