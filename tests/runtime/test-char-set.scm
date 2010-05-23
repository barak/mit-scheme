#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; Test of character-set abstraction

(declare (usual-integrations))

(define (test-canonicalize-scalar-value-list n-items n-iter)
  (run-cpl-test n-items n-iter canonicalize-scalar-value-list))

(define (test-char-set->scalar-values n-items n-iter)
  (run-cpl-test n-items n-iter
		(lambda (cpl)
		  (char-set->scalar-values (scalar-values->char-set cpl)))))

(define (run-cpl-test n-items n-iter procedure)
  (do ((i 0 (+ i 1))
       (failures '()
		 (let ((cpl (make-test-cpl n-items)))
		   (guarantee-well-formed-scalar-value-list cpl)
		   (let ((cpl* (procedure cpl)))
		     (if (canonical-scalar-value-list? cpl*)
			 failures
			 (cons (cons cpl cpl*) failures))))))
      ((not (< i n-iter))
       (let ((n-failures (length failures)))
	 (if (> n-failures 0)
	     (begin
	       (write-string "Got ")
	       (write n-failures)
	       (write-string " failure")
	       (if (> n-failures 1)
		   (write-string "s"))
	       (write-string " out of ")
	       (write n-iter)
	       (newline)
	       (pp failures)))))))

(define (make-test-cpl n-items)
  (make-initialized-list n-items
    (lambda (i)
      (let loop ()
	(let ((n (random #x10000)))
	  (if (unicode-scalar-value? n)
	      (let ((m (random #x100)))
		(if (fix:= m 0)
		    n
		    (if (unicode-scalar-value? (fix:+ n m))
			(fix:+ n m)
			(loop))))
	      (loop)))))))

(define (canonical-scalar-value-list? items)
  (and (well-formed-scalar-value-list? items)
       (if (pair? items)
	   (let loop ((a (car items)) (items (cdr items)))
	     (if (pair? items)
		 (let ((b (car items))
		       (items (cdr items)))
		   (and (fix:< (fix:+ (if (pair? a) (cdr a) a) 1)
			       (if (pair? b) (car b) b))
			(loop b items)))
		 #t))
	   #t)))