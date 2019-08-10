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

;;;; Tests of the printer
;;; package: (runtime printer)

(declare (usual-integrations))

(define-test 'find-shared-objects/shared-once
  (lambda ()
    (let ((c (cons 0 0)))
      (set-cdr! c c)
      (let ((s (find-shared-objects c #f)))
        (assert-= (length s) 1)
        (assert-eq (car s) c)))))

(define-test 'find-shared-objects/shared-twice
  (lambda ()
    (let ((c (cons 0 0)))
      (set-car! c c)
      (set-cdr! c c)
      (let ((s (find-shared-objects c #f)))
        (assert-= (length s) 1)
        (assert-eq (car s) c)))))

(define (assert-prints-as printer object expected . properties)
  (apply assert-string=
	 (call-with-output-string
	   (lambda (port)
	     (printer object port)))
	 expected
	 properties))

(define-test 'print-cyclic-objects
  (lambda ()
    (define (use-printer printer)
      (let ((clist (circular-list 1 3 5 7)))
	(assert-prints-as printer clist
			  "#0=(1 3 5 7 . #0#)")
	(assert-prints-as printer (list clist)
			  "(#0=(1 3 5 7 . #0#))")
	(assert-prints-as printer (vector (circular-list 1 3 5 7))
			  "#(#0=(1 3 5 7 . #0#))")
	(assert-prints-as printer (circular-list clist)
			  "#0=(#1=(1 3 5 7 . #1#) . #0#)")
	(assert-prints-as printer (circular-list clist clist)
			  "#0=(#1=(1 3 5 7 . #1#) #1# . #0#)"))
      (let ((cvector (vector 2 4 6 8)))
	(vector-set! cvector 1 cvector)
	(assert-prints-as printer cvector
			  "#0=#(2 #0# 6 8)")
	(assert-prints-as printer (list cvector cvector)
			  "(#0=#(2 #0# 6 8) #0#)")
	(assert-prints-as printer (vector cvector cvector)
			  "#(#0=#(2 #0# 6 8) #0#)")
	(assert-prints-as printer (circular-list cvector cvector)
			  "#0=(#1=#(2 #1# 6 8) #1# . #0#)")))
    (use-printer write)
    (use-printer write-shared)))

(define-test 'print-shared-objects
  (lambda ()
    (let ((x
	   (let ((x (list 1 2)))
	     (list x x))))
      (assert-prints-as write x
			"((1 2) (1 2))")
      (assert-prints-as write-shared x
			"(#0=(1 2) #0#)"))
    (let ((x
	   (let ((x (vector 1 2)))
	     (vector 3 x 4 x 5))))
      (assert-prints-as write x
			"#(3 #(1 2) 4 #(1 2) 5)")
      (assert-prints-as write-shared x
			"#(3 #0=#(1 2) 4 #0# 5)"))))

(define-test 'general-item-printer
  (lambda ()
    (assert-prints-as write '() "()")
    (assert-prints-as write '#() "#()")
    (assert-prints-as write '#u8() "#u8()")
    (assert-prints-as write '(2) "(2)")
    (assert-prints-as write '#(2) "#(2)")
    (assert-prints-as write '#u8(2) "#u8(2)")
    (assert-prints-as write '(2 3 5 7 11 13 17 19)
		      "(2 3 5 7 11 13 17 19)")
    (assert-prints-as write '#(2 3 5 7 11 13 17 19)
		      "#(2 3 5 7 11 13 17 19)")
    (assert-prints-as write '#u8(2 3 5 7 11 13 17 19)
		      "#u8(2 3 5 7 11 13 17 19)")
    (assert-prints-as write '(2 3 5 7 11 13 17 19 . foo)
		      "(2 3 5 7 11 13 17 19 . foo)")))

(define-test 'list-breadth-limit
  (lambda ()
    (parameterize ((param:printer-list-breadth-limit 1))
      (assert-prints-as write '() "()")
      (assert-prints-as write '#() "#()")
      (assert-prints-as write '#u8() "#u8()")
      (assert-prints-as write '(2) "(2)")
      (assert-prints-as write '#(2) "#(2)")
      (assert-prints-as write '#u8(2) "#u8(2)")
      (assert-prints-as write '(2 3 5 7 11 13 17 19)
			"(2 ...)")
      (assert-prints-as write '#(2 3 5 7 11 13 17 19)
			"#(2 ...)")
      (assert-prints-as write '#u8(2 3 5 7 11 13 17 19)
			"#u8(2 ...)")
      (assert-prints-as write '(2 3 5 7 11 13 17 19 . foo)
			"(2 ...)"))
    (parameterize ((param:printer-list-breadth-limit 2))
      (assert-prints-as write '(2 3 5 7 11 13 17 19)
			"(2 3 ...)")
      (assert-prints-as write '#(2 3 5 7 11 13 17 19)
			"#(2 3 ...)")
      (assert-prints-as write '#u8(2 3 5 7 11 13 17 19)
			"#u8(2 3 ...)")
      (assert-prints-as write '(2 3 5 7 11 13 17 19 . foo)
			"(2 3 ...)"))
    (parameterize ((param:printer-list-breadth-limit 3))
      (assert-prints-as write '(2 3 5 7 11 13 17 19)
			"(2 3 5 ...)")
      (assert-prints-as write '#(2 3 5 7 11 13 17 19)
			"#(2 3 5 ...)")
      (assert-prints-as write '#u8(2 3 5 7 11 13 17 19)
			"#u8(2 3 5 ...)")
      (assert-prints-as write '(2 3 5 7 11 13 17 19 . foo)
			"(2 3 5 ...)"))))

(define-primitives
  primitive-type-set!)

(define (insert-nmv! v i n)
  (vector-set! v i n)
  (primitive-type-set! v (+ i 1) (ucode-type manifest-nm-vector)))

(define-test 'partially-marked-vector
  (lambda ()
    (let ((v (make-vector 10)))
      (insert-nmv! v 2 5)
      (assert-prints-as write v
			"#(#f #f |#[non-marked section of length 5]| #f #f)"))
    (let ((v (make-vector 10)))
      (insert-nmv! v 0 5)
      (assert-prints-as write v
			"#(|#[non-marked section of length 5]| #f #f #f #f)"))
    (let ((v (make-vector 10)))
      (insert-nmv! v 4 5)
      (assert-prints-as write v
			"#(#f #f #f #f |#[non-marked section of length 5]|)"))))