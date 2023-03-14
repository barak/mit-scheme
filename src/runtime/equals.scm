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

;;;; Equality
;;; package: (runtime equality)

(declare (usual-integrations))

(define-primitives
  eq?)

(define (eqv? x y)
  (or (eq? x y)
      (and (number? x)
	   (number? y)
	   (number:eqv? x y))))

(define (equal? x y)
  (equal?-limited x y (lambda () (equal?-detected x y))))

(define (equal?-limited x y if-limited)
  (let ((limit equal?-limit)
        (count 0))
    (let loop
        ((x x)
         (y y)
         (s (lambda () #t))
         (f (lambda () #f)))
      (if (fx<? count limit)
          (begin
            (set! count (fx+ count 1))
            (if (eqv? x y)
                (s)
                (equal?-step loop x y s f)))
          (if-limited)))))

(define-integrable equal?-limit
  100000)

(define (equal?-detected x y)

  (define (loop x y s f)
    (if (eqv? x y)
        (s)
	(detect-circ x y s f)))

  (define detect-circ
    (make-detect-circ
     (lambda (x y s f)
       (equal?-step loop x y s f))))

  (loop x y (lambda () #t) (lambda () #f)))

(define (make-detect-circ continue)
  continue)

(define ((make-mdc make-ht) continue)
  ;; HT must be pair-eqv table.
  (let ((ht (make-ht)))
    (lambda (x y s f)
      (let ((key (cons x y)))
        (hash-table-ref ht key
                        (lambda ()
                          (hash-table-set! ht key #t)
                          (continue x y s
                                    (lambda ()
                                      (hash-table-set! ht key #f)
                                      (f))))
                        (lambda (v)
                          (if v (s) (f))))))))

;;; This file gets loaded before the boot-dependency code, so defer registration
;;; until it's available.
(define (initialize-package!)
  (add-boot-deps! '(runtime comparator)
		  '(runtime hash-table))
  (add-boot-init!
   (lambda ()
     (set! make-detect-circ
	   (make-mdc
	    (hash-table-constructor
	     (make-pair-comparator eqv-comparator eqv-comparator))))
     unspecific)))

(define (equal-hash key)
  (cond ((primitive-object-hash key))
	((string? key) (string-hash key))
	((pathname? key) (string-hash (->namestring key)))
	((bit-string? key)
	 (primitive-object-hash (bit-string->unsigned-integer key)))
	(else (eq-hash key))))

(define-integrable (equal?-step loop x y s f)
  (cond ((pair? x)
         (if (pair? y)
             (loop (car x)
                   (car y)
                   (lambda () (loop (cdr x) (cdr y) s f))
                   f)
             (f)))
        ((vector? x)
         (if (vector? y)
             (let ((n (vector-length x)))
               (if (fx=? n (vector-length y))
                   (if (fx>? n 0)
                       (let ((n-1 (fx- n 1)))
                         (let per-elt ((i 0))
                           (loop (vector-ref x i)
                                 (vector-ref y i)
                                 (if (fx<? i n-1)
                                     (lambda () (per-elt (fx+ i 1)))
                                     s)
                                 f)))
                       (s))
                   (f)))
             (f)))
        ((weak-pair? x)
         (if (weak-pair? y)
             (loop (weak-car x)
                   (weak-car y)
                   (lambda () (loop (weak-cdr x) (weak-cdr y) s f))
                   f)
             (f)))
        ((cell? x)
         (if (cell? y)
             (loop (cell-contents x)
                   (cell-contents y)
                   s
                   f)
             (f)))
        ((equal?-helper x y) (s))
	(else (f))))

(define-integrable (equal?-helper x y)
  (cond ((number? x)
	 (and (number? y)
	      (number:eqv? x y)))
	((bytevector? x)
	 (and (bytevector? y)
	      (bytevector=? x y)))
	((string? x)
	 (and (string? y)
	      (string=? x y)))
	((bit-string? x)
	 (and (bit-string? y)
	      (bit-string=? x y)))
	((pathname? x)
	 (and (pathname? y)
	      (pathname=? x y)))
	((char-set? x)
	 (and (char-set? y)
	      (char-set= x y)))
	(else #f)))