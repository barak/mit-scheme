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

;;;; Test of entities and apply hooks

(declare (usual-integrations))

(define (some-procedure foo)
  foo)

(define some-extra
  (list 'FOO 42))

((lambda (descriptors)
   ((lambda (f)
      (for-each (lambda (descriptor) (apply f descriptor)) descriptors))
    (lambda (name constructor predicate get-procedure get-extra)
      (define-test (symbol name '?)
        (lambda ()
          (assert-true (predicate (constructor some-procedure some-extra)))))
      (define-test (symbol name '- 'PROCEDURE)
        (lambda ()
          (assert-eq some-procedure
                     (get-procedure (constructor some-procedure some-extra)))))
      (define-test (symbol name '- 'EXTRA)
        (lambda ()
          (assert-eq
           some-extra
           (get-extra (constructor some-procedure some-extra)))))))
   ((lambda (f)
      (for-each (lambda (descriptor)
                  (for-each (lambda (descriptor*)
                              (if (not (eq? (car descriptor)
                                            (car descriptor*)))
                                  (apply f (append descriptor descriptor*))))
                            descriptors))
                descriptors))
    (lambda (name constructor predicate get-procedure get-extra
             name* constructor* predicate* get-procedure* get-extra*)
      constructor predicate* get-procedure* get-extra*
      (define-test (symbol name '? '/ name*)
        (lambda ()
          (assert-false (predicate (constructor* some-procedure some-extra)))))
      (define-test (symbol name '? '/ 'JUNK)
        (lambda ()
          (assert-false (predicate some-extra))))
      (define-test (symbol name '- 'PROCEDURE '/ name*)
        (lambda ()
          (let ((object* (constructor* some-procedure some-extra)))
            (assert-error (lambda ()
                            (get-procedure object*))
                          (list condition-type:wrong-type-argument)))))
      (define-test (symbol name '- 'PROCEDURE '/ 'JUNK)
        (lambda ()
          (assert-error (lambda () (get-procedure some-extra))
                        (list condition-type:wrong-type-argument))))
      (define-test (symbol name '- 'EXTRA '/ name*)
        (lambda ()
          (let ((object* (constructor* some-procedure some-extra)))
            (assert-error (lambda () (get-extra object*))
                          (list condition-type:wrong-type-argument)))))
      (define-test (symbol name '- 'EXTRA '/ 'JUNK)
        (lambda ()
          (assert-error (lambda () (get-extra some-extra))
                        (list condition-type:wrong-type-argument)))))))
 `((ENTITY
    ,make-entity ,entity? ,entity-procedure ,entity-extra)
   (APPLY-HOOK
    ,make-apply-hook ,apply-hook? ,apply-hook-procedure ,apply-hook-extra)))

(define-test 'ENTITY-APPLICATION/0
  (lambda ()
    (let ((entity (make-entity some-procedure some-extra)))
      (assert-eq entity (entity)))))

(define-test 'ENTITY-APPLICATION/1
  (lambda ()
    (let ((entity (make-entity some-procedure some-extra)))
      (assert-error (lambda () (entity 42))
                    (list condition-type:wrong-number-of-arguments)))))

(define-test 'APPLY-HOOK-APPLICATION/0
  (lambda ()
    (let ((apply-hook (make-apply-hook some-procedure some-extra)))
      (assert-error (lambda () (apply-hook))
                    (list condition-type:wrong-number-of-arguments)))))

(define-test 'ENTITY-APPLICATION/1
  (lambda ()
    (assert-eqv 42 ((make-apply-hook some-procedure some-extra) 42))))

(define-test 'ENTITY-CHAIN
  (lambda ()
    (let* ((e0 (make-entity some-procedure some-extra))
           (e1 (make-entity e0 'ZARQUON))
           (e2 (make-entity e1 'QUAGGA)))
      (assert-error (lambda ()
                      (set-entity-procedure! e0 e2))
                    (list condition-type:bad-range-argument)))))
