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

;;;; Tests of pretty-printer

(declare (usual-integrations))

(define assert-string
  (predicate-assertion string? "string"))

(define-test 'circular/simple
  (lambda ()
    (define (doit)
      (let ((c (cons 0 0)))
        (set-cdr! c c)
        (call-with-output-string
          (lambda (p)
            (parameterize ((param:pp-avoid-circularity? #t))
              (pp c p))))))
    (assert-equal
     (carefully doit (lambda () 'stack-overflow) (lambda () 'timeout))
     "(0 0 . #[circularity (current parenthetical level, downstream 1 cdr.)])\n")))

(define-test 'circular/hairy
  (lambda ()
    (define (doit)
      (let ((u (vector 1 2 3))
            (v (vector 4 5 6)))
        (vector-set! v 2 u)
        (vector-set! u 1 v)
        (let ((c (cons 0 0)))
          (set-car! c u)
          (set-cdr! c v)
          (vector-set! v 1 c))
        (call-with-output-string
          (lambda (p)
            (parameterize ((param:pp-avoid-circularity? #t))
              (pp v p))))))
    (expect-failure
     (lambda ()
       ;; XXX Figure out what string it should print by hand.
       (assert-string
        (carefully doit (lambda () 'stack-overflow) (lambda () 'timeout)))))))

(define-test 'custom
  (lambda ()
    (let ((tag (cons 0 0)))
      (define (loser? object)
        (and (vector? object)
             (<= 1 (vector-length object))
             (eq? tag (vector-ref object 0))))
      (register-predicate! loser? 'loser? '<= vector?)
      (define-print-method loser?
        (standard-print-method
         (lambda (object) object "LOSER")
         (lambda (object) object '(42))))
      (let* ((loser (make-vector 1000 tag))
             (hash (number->string (hash-object loser))))
        (assert-equal
         (call-with-output-string (lambda (port) (pp loser port)))
         (string-append "#[LOSER " hash " 42]\n"))))))
