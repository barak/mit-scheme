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

;;;; Tests of the dynamic environment

(declare (usual-integrations))

(define-test 'PARAMETERS
  (lambda ()
    (assert-eqv make-unsettable-parameter make-parameter)
    (let ((p (make-parameter 1))
	  (q (make-parameter 2
			     (lambda (v)
			       (guarantee exact-nonnegative-integer? v)))))
      (assert-eqv (p) 1)
      (assert-equal (parameterize ((p "7") (q 9))
		      (cons (p) (q)))
		    '("7" . 9))
      (assert-equal (cons (p) (q))
		    '(1 . 2))
      (assert-error (lambda ()
		      (parameterize ((q "7"))
			(q)))
		    (list condition-type:wrong-type-argument)))))

;; From node "Dynamic Binding" in doc/ref-manual/special-forms.texi:
(define (complicated-dynamic-parameter)
  (let ((variable (make-settable-parameter 1))
        (inside-continuation))
    (write-line (variable))
    (call-with-current-continuation
     (lambda (outside-continuation)
       (parameterize ((variable 2))
         (write-line (variable))
         (variable 3)
         (call-with-current-continuation
          (lambda (k)
            (set! inside-continuation k)
            (outside-continuation #t)))
         (write-line (variable))
         (set! inside-continuation #f))))
    (write-line (variable))
    (if inside-continuation
        (begin
          (variable 4)
          (inside-continuation #f)))))

(define-test 'COMPLICATED-DYNAMIC-PARAMETER
  (lambda ()
    (assert-equal
     (call-with-output-string
      (lambda (port)
	(parameterize* (list (cons current-output-port port))
		       complicated-dynamic-parameter)))
     "1
2
1
3
4
")))