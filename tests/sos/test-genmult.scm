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

;;; Tests of Multiplexed Generic Procedures

(declare (usual-integrations))

(load-option 'sos)

(define-test 'REGRESSION:REMOVE-GENERIC-PROCEDURE-GENERATOR
  (lambda ()
    (define generic (make-generic-procedure 1))
    ;; Bug exhibited itself in the face of a default generator.
    (set-generic-procedure-default-generator!
     generic
     (lambda _ _ (lambda _ _ 'not-specialised)))
    (assert-equal (generic #\a) 'not-specialised)
    ;; Add some named generators (for easier removal).
    (define (bool-generator p tags)
      p                                 ;ignore
      (if (equal? tags (list (predicate->dispatch-tag boolean?)))
          (lambda (x) (cons 'boolean x))
          #f))
    (add-generic-procedure-generator generic bool-generator)
    (assert-equal (generic #t) '(boolean . #t))
    (define (fixnum-generator p tags)
      p                                 ;ignore
      (if (equal? tags (list (predicate->dispatch-tag fix:fixnum?)))
          (lambda (x) (cons 'fixnum x))
          #f))
    (add-generic-procedure-generator generic fixnum-generator)
    (assert-equal (generic 42) '(fixnum . 42))
    ;; Remove members, ensuring they are all removed.
    (remove-generic-procedure-generator generic bool-generator)
    (remove-generic-procedure-generator generic fixnum-generator)
    (assert-eq (generic-procedure-generator-list generic) '())))
