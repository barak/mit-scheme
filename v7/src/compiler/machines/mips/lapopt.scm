#| -*-Scheme-*-

$Id: lapopt.scm,v 1.7 2008/01/30 20:01:51 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; LAP Optimizer for MIPS.

(declare (usual-integrations))

(define (optimize-linear-lap instructions)
  ;; Find occurrences of LW/LBU/LWC1 followed by NOP, and delete the
  ;; NOP if the instruction following it has no reference to the
  ;; target register of the load.

  ;; **** This is pretty fragile. ****
  (letrec
      ((find-load
	(lambda (instructions)
	  (cond ((null? instructions) '())
		((and (pair? (car instructions))
		      (or (eq? 'LW (caar instructions))
			  (eq? 'LBU (caar instructions))
			  (eq? 'LWC1 (caar instructions))))
		 instructions)
		(else (find-load (cdr instructions))))))
       (get-next
	(lambda (instructions)
	  (let ((instructions (cdr instructions)))
	    (cond ((null? instructions) '())
		  ((or (not (pair? (car instructions)))
		       (eq? 'LABEL (caar instructions))
		       (eq? 'COMMENT (caar instructions)))
		   (get-next instructions))
		  (else instructions)))))
       (refers-to-register?
	(lambda (instruction register)
	  (let loop ((x instruction))
	    (if (pair? x)
		(or (loop (car x))
		    (loop (cdr x)))
		(eqv? register x))))))
    (let loop ((instructions instructions))
      (let ((first (find-load instructions)))
	(if (not (null? first))
	    (let ((second (get-next first)))
	      (if (not (null? second))
		  (let ((third (get-next second)))
		    (if (not (null? third))
			(if (and (equal? '(NOP) (car second))
				 ;; This is a crude way to test for a
				 ;; reference to the target register
				 ;; -- it will sometimes incorrectly
				 ;; say that there is a reference, but
				 ;; it will never incorrectly say that
				 ;; there is no reference.
				 (not (refers-to-register? (car third)
							   (cadar first)))
				 (or (not (and (eq? 'LWC1 (caar first))
					       (odd? (cadar first))))
				     (not (refers-to-register?
					   (car third)
					   (- (cadar first) 1)))))
			    (begin
			      (let loop ((this (cdr first)) (prev first))
				(if (eq? second this)
				    (set-cdr! prev (cdr this))
				    (loop (cdr this) this)))
			      (loop (if (equal? '(NOP) (car third))
					first
					third)))
			    (loop second))))))))))
  instructions)