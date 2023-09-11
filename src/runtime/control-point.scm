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

;;;; Control Points
;;; package: (runtime new-control-point)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables))

(define-primitives
  (control-point-next-frame 2))

(define (control-point? object)
  (object-type? (ucode-type control-point) object))
(register-predicate! control-point? 'control-point)

(define (make-control-point* raw-frames)
  (let ((cp
	 (safe-system-vector-cons (ucode-type control-point)
				  (fold (lambda (frame n)
					  (fix:+ (vector-length frame) n))
					2
					raw-frames))))
    (safe-system-vector-set! cp 0 #f)
    (safe-system-vector-set! cp 1 0)
    (let loop ((raw-frames raw-frames) (index 2))
      (if (pair? raw-frames)
	  (let* ((frame (car raw-frames))
		 (n (vector-length frame)))
	    (let copy ((i 0) (index* index))
	      (if (fix:< i n)
		  (begin
		    (safe-system-vector-set! cp index* (vector-ref frame i))
		    (copy (fix:+ i 1) (fix:+ index* 1)))
		  (loop (cdr raw-frames) index*))))))
    cp))

(define-integrable (control-point-start-index)
  2)

(define-integrable (control-point-length control-point)
  (system-vector-length control-point))

(define (control-point->raw-frame-generator control-point)
  (let ((index)
	(end))

    (define (new-cp! cp)
      (set! control-point cp)
      (set! index (control-point-start-index))
      (set! end (control-point-length cp)))

    (define (generator)
      (if (fix:< index end)
	  (let* ((index* (control-point-next-frame control-point index))
		 (frame (make-vector (fix:- index* index)))
		 (result (vector index end frame)))
	    (do ((i index (fix:+ i 1))
		 (j 0 (fix:+ j 1)))
		((not (fix:< i index*))
		 (set! index index*))
	      (vector-set! frame j (safe-system-vector-ref control-point i)))
	    (if (eq? (ucode-return-address join-stacklets)
		     (vector-ref frame 0))
		(begin
		  (assert (fix:= index* end))
		  (new-cp! (vector-ref frame 1))))
	    result)
	  (eof-object)))

    (new-cp! control-point)
    generator))