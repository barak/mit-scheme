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

;;;; Populations
;;; package: (runtime population)

(declare (usual-integrations))

;;; A population is a weak collection of objects.  A serial
;;; population is a population with a mutex to serialize its operations.

(define (%make-population mutex)
  (%record population-tag mutex (weak-list-set eqv?)))

(define (population? object)
  (and (%record? object)
       (eq? population-tag (%record-ref object 0))))
(register-predicate! population? 'population '<= %record?)

(define-integrable population-tag
  '|#[(runtime population)population]|)

(define-integrable (%pop-mutex pop) (%record-ref pop 1))
(define-integrable (%pop-set pop) (%record-ref pop 2))

(define-print-method population?
  (standard-print-method 'population))

(define population-of-populations
  (%make-population (make-thread-mutex)))

(define (make-population)
  (let ((population (%make-population #f)))
    (add-new-to-population! population-of-populations population)
    population))

(define (make-population/unsafe)
  (let ((population (%make-population #f)))
    (add-new-to-population!/unsafe population-of-populations population)
    population))

(define (make-serial-population)
  (let ((population (%make-population (make-thread-mutex))))
    (add-new-to-population! population-of-populations population)
    population))

(define (make-serial-population/unsafe)
  (let ((population (%make-population (make-thread-mutex))))
    (add-new-to-population!/unsafe population-of-populations population)
    population))

(define (%maybe-lock! population thunk)
  (if (%pop-mutex population)
      (with-thread-mutex-lock (%pop-mutex population) thunk)
      (thunk)))

(define (clean-population! population)
  (%maybe-lock! population
    (lambda ()
      (weak-list-set-clean! (%pop-set population)))))

(define (clean-all-populations!)
  (clean-population! population-of-populations)
  (map-over-population! population-of-populations clean-population!))

(add-boot-init!
 (lambda ()
   (add-secondary-gc-daemon!/unsafe clean-all-populations!)))

(define (add-to-population! population object)
  (guarantee population? population 'add-to-population!)
  (%maybe-lock! population
    (lambda ()
      (weak-list-set-add! object (%pop-set population)))))

(define (add-new-to-population! population object)
  (guarantee population? population 'add-new-to-population!)
  (%maybe-lock! population
    (lambda ()
      (weak-list-set-add-new! object (%pop-set population)))))

(define (add-new-to-population!/unsafe population object)
  (weak-list-set-add-new! object (%pop-set population)))

(define (remove-from-population! population object)
  (guarantee population? population 'remove-from-population!)
  (%maybe-lock! population
    (lambda ()
      (weak-list-set-delete! object (%pop-set population)))))

(define (empty-population! population)
  (guarantee population? population 'empty-population!)
  (%maybe-lock! population
    (lambda ()
      (weak-list-set-clear! (%pop-set population)))))

;;;; Read-only operations

;;; These are safe without serialization.

(define (map-over-population population procedure)
  (weak-list-set-fold (lambda (item acc)
			(cons (procedure item) acc))
		      '()
		      (%pop-set population)))

(define (map-over-population! population procedure)
  (weak-list-set-for-each procedure (%pop-set population)))

(define (for-all-inhabitants? population predicate)
  (weak-list-set-every predicate (%pop-set population)))

(define (exists-an-inhabitant? population predicate)
  (weak-list-set-any predicate (%pop-set population)))