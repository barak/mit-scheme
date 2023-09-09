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

(define reference-cont)
(call-with-current-continuation
  (lambda (k)
    (set! reference-cont k)))

(define-test 'decoded-frames
  (lambda ()
    (let ((frames (continuation->cframe-stream reference-cont)))
      (stream-for-each
       (lambda (frame)
	 (let* ((raw (cframe-raw frame))
		(end (vector-length raw)))
	   (assert-eqv (cframe-length frame) end)
	   (for-each (lambda (i)
		       (assert-eqv (cframe-ref frame i)
				   (vector-ref raw i)))
		     (iota end)))
	 (let ((return (cframe-ref frame 0)))
	   (assert-eqv (cframe-return-address frame)
		       return)
	   (assert-eqv (cframe-compiled-code? frame)
		       (compiled-return-address? return))
	   (assert-eqv (cframe-return-code frame)
		       (and (interpreter-return-address? return)
			    (return-address/code return)))))
       frames)
      (let ((cp (cframe-stream->control-point frames)))
	(assert-true (control-point? cp))
	(let ((end (system-vector-length cp))
	      (reference-cp (continuation/control-point reference-cont)))
	  (assert-eqv end (system-vector-length reference-cp))
	  (for-each (lambda (i)
		      (assert-eqv (system-vector-ref cp i)
				  (system-vector-ref reference-cp i)))
		    (iota end)))))))