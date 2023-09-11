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

;;;; Tests of control points

(define reference-cp)
(call-with-current-continuation
  (lambda (k)
    (set! reference-cp (continuation/control-point k))))

(define-test 'raw-generator
  (lambda ()
    (let ((results
	   (generator->list
	    (control-point->raw-frame-generator reference-cp))))
      (let new-ref ((results results) (ref reference-cp))
	(let ((end (system-vector-length ref)))
	  (let loop ((results results) (index 2))
	    (if (pair? results)
		(let ((result (car results)))
		  (assert-true (fix:< index end))
		  (assert-true (vector? result))
		  (assert-eqv (vector-length result) 3)
		  (let ((start (vector-ref result 0))
			(cp-end (vector-ref result 1))
			(frame (vector-ref result 2)))
		    (assert-eqv start index)
		    (assert-eqv cp-end end)
		    (assert-true (vector? frame))
		    (let ((n (vector-length frame)))
		      (assert-true (> n 0))
		      (assert-true (return-address? (vector-ref frame 0)))
		      (do ((i 0 (fix:+ i 1))
			   (j index (fix:+ j 1)))
			  ((not (fix:< i n))
			   (if (and (fix:= j end)
				    (fix:= n 2)
				    (eq? (ucode-return-address join-stacklets)
					 (vector-ref frame 0)))
			       (new-ref (cdr results) (vector-ref frame 1))
			       (loop (cdr results) j)))
			(assert-eqv (vector-ref frame i)
				    (safe-system-vector-ref ref j))))))
		(assert-eqv index end))))))))