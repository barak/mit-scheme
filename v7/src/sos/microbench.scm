#| -*-Scheme-*-

$Id: microbench.scm,v 1.4 2003/02/14 18:25:21 cph Exp $

Copyright 1993-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Micro-benchmarks for SOS

(declare (usual-integrations))

(define (f1 x)
  x)

(define (f2 x y)
  y
  x)

(define (rf . x)
  x)

(define (get-f5)
  (lambda (x)
    x))

(define (get-f6 y)
  (lambda (x)
    x
    y))

(define (fv x)
  (vector-ref x 1))

(define-class <c1> ()
  x)

(define-class <c2> (<c1>)
  )

(define-class <c3> (<c1>)
  )

(define fx1 (slot-accessor <c1> 'X))
(define fx2 (slot-accessor <c1> 'X))
(define fx3 (slot-accessor <c1> 'X))

(define-generic fx1* (instance))
(define-generic fx2* (instance))
(define-generic fx3* (instance))
(let ((method (slot-accessor-method <c1> 'X)))
  (add-method fx1* method)
  (add-method fx2* method)
  (add-method fx3* method))

(define-generic g1 (instance))
(define-method g1 ((instance <c1>)) instance)
(define (get-g1) g1)

(define-generic g2 (instance))
(define-method g2 ((instance <c1>)) instance)
(define-method g2 ((instance <c2>)) instance)

(define-generic g3 (instance other))
(define-method g3 ((instance <c1>) other) other instance)

(define (null-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000)))))

(define (f1-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (f1 i1))))

(define (f2-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (f2 i1 i2))))

(define (f3-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (rf i1))))

(define (f4-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (rf i1 i2))))

(define (f5-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '())))
	(f5 (get-f5)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (f5 i1))))

(define (f6-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '())))
	(f6 (get-f6 0)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (f6 i1))))

(define (fv-test)
  (let ((i1 (vector 'A 'B)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fv i1))))

(define (fx1-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx1 i1))))

(define (fx2-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (set-slot-value! i2 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx2 i1)
      (fx2 i2))))

(define (fx3-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (set-slot-value! i2 'X 0)
    (set-slot-value! i3 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx3 i1)
      (fx3 i2)
      (fx3 i3))))

(define (fx1*-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx1* i1))))

(define (fx2*-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (set-slot-value! i2 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx2* i1)
      (fx2* i2))))

(define (fx3*-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (set-slot-value! i1 'X 0)
    (set-slot-value! i2 'X 0)
    (set-slot-value! i3 'X 0)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (fx3* i1)
      (fx3* i2)
      (fx3* i3))))

(define (g1-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (g1 i1))))

(define (g2-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (g2 i1)
      (g2 i2))))

(define (g3-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '()))))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (g3 i1 i2))))

(define (g4-test)
  (let ((i1 ((instance-constructor <c1> '())))
	(i2 ((instance-constructor <c2> '())))
	(i3 ((instance-constructor <c3> '())))
	(g1 (get-g1)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i 100000))
      (g1 i1))))

(define (run-test test)
  (test)				;warm up
  (let loop ((n 3) (time 0))
    (if (= n 0)
	(/ time 300)
	(begin
	  (gc-flip)
	  (let ((process-start (process-time-clock)))
	    (test)
	    (let ((process-end (process-time-clock)))
	      (loop (- n 1)
		    (+ time (- process-end process-start)))))))))

(define (run-tests)
  (let ((f1-time (run-test f1-test)))
    (let ((report
	   (lambda (name time scale)
	     (fluid-let ((flonum-unparser-cutoff '(ABSOLUTE 2)))
	       (newline)
	       (write name)
	       (write-string "-test:\t")
	       (write (exact->inexact time))
	       (write-string "\t")
	       (write (exact->inexact (/ (/ time scale) f1-time)))))))
      (report 'f1 f1-time 1)
      (for-each (lambda (name test scale)
		  (report name (run-test test) scale))
		'(f2 f3 f4 f5 f6 fv fx1 fx2 fx3 fx1* fx2* fx3* g1 g2 g3 g4)
		(list f2-test f3-test f4-test f5-test f6-test fv-test
		      fx1-test fx2-test fx3-test fx1*-test fx2*-test fx3*-test
		      g1-test g2-test g3-test g4-test)
		'(1 1 1 1 1 1 1 2 3 1 2 3 1 2 1 1)))))