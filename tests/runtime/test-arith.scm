#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

;;;; Test of arithmetic

(declare (usual-integrations))

;;; XXX The nonsense about IDENTITY-PROCEDURE here serves to fake
;;; out bogus constant-folding which needs to be fixed in SF (and
;;; probably LIAR too).

(define (zero)
  (identity-procedure 0.))

(define (nan)
  (flo:with-exceptions-untrapped (flo:exception:invalid-operation)
    (lambda ()
      (flo:/ (zero) (zero)))))

(define (inf+)
  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
    (lambda ()
      (flo:/ +1. (zero)))))

(define (inf-)
  (flo:with-exceptions-untrapped (flo:exception:divide-by-zero)
    (lambda ()
      (flo:/ -1. (zero)))))

(define (assert-nan object)
  (assert-true (flo:flonum? object))
  (assert-false (flo:= object object)))

(define (define-enumerated-test prefix elements procedure)
  (let ((n (vector-length elements)))
    (do ((i 0 (+ i 1))) ((>= i n))
      (define-test (symbol prefix ': (vector-ref elements i))
        (lambda ()
          (procedure (vector-ref elements i)))))))

(define (define-enumerated^2-test* prefix elements procedure)
  (let ((n (vector-length elements)))
    (do ((i 0 (+ i 1))) ((>= i n))
      (do ((j 0 (+ j 1))) ((>= j n))
        (define-test
            (symbol prefix
                    ': (vector-ref elements i)
                    ': (vector-ref elements j))
          (lambda ()
            (procedure i (vector-ref elements i)
                       j (vector-ref elements j))))))))

(define (define-enumerated^2-test prefix elements procedure)
  (define-enumerated^2-test* prefix elements
    (lambda (i vi j vj)
      i j                               ;ignore
      (procedure vi vj))))

(define-enumerated^2-test 'ZEROS-ARE-EQUAL (vector -0. 0 +0.) =)

(define-enumerated^2-test* 'ORDER-WITH-INFINITIES
  (vector (inf-) -2. -1 -0.5 0 +0.5 +1 +2. (inf+))
  (lambda (i vi j vj)
    (if (< i j)
        (assert-true (< vi vj))
        (assert-false (< vi vj)))))

(let ((elements (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))))
  (define-enumerated-test '!NAN<X elements
    (lambda (v) (assert-false (< (nan) v))))
  (define-enumerated-test '!X<NAN elements
    (lambda (v) (assert-false (< v (nan))))))

(let ((elements (vector -2. -1 -0. 0 +0. +1 +2.)))

  (define-enumerated-test 'MIN-INF-/X elements
    (lambda (v) (assert-= (min (inf-) v) (inf-))))
  (define-enumerated-test 'MIN-INF+/X elements
    (lambda (v) (assert-= (min (inf+) v) v)))
  (define-enumerated-test 'MIN-X/INF- elements
    (lambda (v) (assert-= (min v (inf-)) (inf-))))
  (define-enumerated-test 'MIN-X/INF+ elements
    (lambda (v) (assert-= (min v (inf+)) v)))

  (define-enumerated-test 'MAX-INF-/X elements
    (lambda (v) (assert-= (max (inf-) v) v)))
  (define-enumerated-test 'MAX-INF+/X elements
    (lambda (v) (assert-= (max (inf+) v) (inf+))))
  (define-enumerated-test 'MAX-X/INF- elements
    (lambda (v) (assert-= (max v (inf-)) v)))
  (define-enumerated-test 'MAX-X/INF+ elements
    (lambda (v) (assert-= (max v (inf+)) (inf+)))))

(let ((elements (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))))
  (define-enumerated-test 'MIN-NAN/X elements
    (lambda (v) (assert-= (min (nan) v) v)))
  (define-enumerated-test 'MIN-X/NAN elements
    (lambda (v) (assert-= (min v (nan)) v)))
  (define-enumerated-test 'MAX-NAN/X elements
    (lambda (v) (assert-= (max (nan) v) v)))
  (define-enumerated-test 'MAX-X/NAN elements
    (lambda (v) (assert-= (max v (nan)) v))))

(define-enumerated-test 'NAN*X
  (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))
  (lambda (v) (assert-nan (* (nan) v))))

(define-enumerated-test 'X*NAN
  (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))
  (lambda (v) (assert-nan (* v (nan)))))

(define-enumerated-test 'NAN/X
  (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))
  (lambda (v) (assert-nan (/ (nan) v))))

(define-enumerated-test 'X/NAN
  (vector (inf-) -2. -1 -0. 0 +0. +1 +2. (inf+))
  (lambda (v) (assert-nan (/ v (nan)))))
