#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Tests for predicate metadata

(declare (usual-integrations))

(define-test 'non-predicate
  (lambda ()
    (let ((np (lambda (object) object #f)))
      (assert-false (predicate? np))
      (assert-type-error (lambda () (predicate->tag np)))
      (assert-type-error (lambda () (predicate-name np)))
      (assert-true (string? (predicate-description np))))))

(define-test 'simple-predicate
  (lambda ()
    (test-predicate-operations number? 'number)
    (test-predicate-operations boolean? 'boolean)
    (test-predicate-operations string? 'string)))

(define (test-predicate-operations predicate name)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->tag predicate)))
    (assert-true (tag? tag))
    (assert-eqv (tag->predicate tag) predicate)
    (assert-equal (predicate-name predicate) name)
    (assert-equal (tag-name tag) name)
    (assert-equal (predicate-description predicate) (tag-description tag))))

(define-test 'simple-predicate-tagging
  (lambda ()
    (test-tagging number? '(41) '(foo))
    (test-tagging boolean? '(#t) '(foo))
    (test-tagging string? '("41") '(foo))))

(define (test-tagging predicate data non-data)
  (let ((tagger (predicate-tagger predicate))
	(untagger (predicate-untagger predicate))
	(tagging-strategy (predicate-tagging-strategy predicate)))
    (for-each
     (lambda (datum)
       (let ((object (tagger datum)))
	 (assert-true (predicate object))
	 (assert-eq datum (untagger object))
	 (cond ((eqv? tagging-strategy predicate-tagging-strategy:never)
		(assert-eq datum object))
	       ((eqv? tagging-strategy predicate-tagging-strategy:always)
		(assert-!eq datum object))
	       (else
		(if (predicate<= (object->predicate datum) predicate)
		    (assert-eq datum object)
		    (assert-!eq datum object))))))
     data)
    (for-each (lambda (non-datum)
		(assert-type-error (lambda () (tagger non-datum))))
	      non-data)))