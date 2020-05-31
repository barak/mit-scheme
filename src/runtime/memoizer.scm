#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; Memoizers
;;; package: (runtime memoizer)

(declare (usual-integrations))

(add-boot-deps! '(runtime hash-table) '(runtime comparator))

(define-record-type <memoizer-metadata>
    (%make-memoizer-metadata table procedure)
    memoizer-metadata?
  (table %memoizer-metadata-table)
  (procedure %memoizer-metadata-procedure))

(define (memoizer? object)
  (and (apply-hook? object)
       (memoizer-metadata? (apply-hook-extra object))))

(define (make-memoizer table procedure impl)
  (guarantee hash-table? table 'make-memoizer)
  (guarantee procedure? procedure 'make-memoizer)
  (guarantee procedure? impl 'make-memoizer)
  (make-apply-hook impl (%make-memoizer-metadata table procedure)))

(define (memoizer-table memoizer)
  (guarantee memoizer? memoizer 'memoizer-table)
  (%memoizer-metadata-table (apply-hook-extra memoizer)))

(define (memoizer-procedure memoizer)
  (guarantee memoizer? memoizer 'memoizer-procedure)
  (%memoizer-metadata-procedure (apply-hook-extra memoizer)))

(define (clear-memoizer! memoizer)
  (hash-table-clear! (memoizer-table memoizer)))

(define-deferred ordered-memoizer
  (%standard-memoizer uniform-list-comparator #f))

(define-deferred weak-ordered-memoizer
  (%standard-memoizer uniform-weak-list-comparator #t))

(define-deferred unordered-memoizer
  (%standard-memoizer lset-comparator #f))

(define-deferred weak-unordered-memoizer
  (%standard-memoizer weak-lset-comparator #t))

(define-integrable (%standard-memoizer combinator weak?)
  (lambda (comparator procedure)
    (let ((arity (procedure-arity procedure)))
      (let ((n
	     (let ((n (procedure-arity-min arity)))
	       (and (eqv? n (procedure-arity-max arity))
		    n))))
	(let ((table
	       (let ((c (if (eqv? 1 n) comparator (combinator comparator))))
		 (if weak?
		     (make-hash-table c 'weak-keys)
		     (make-hash-table c)))))

	  (define-integrable (get-impl klist list->klist)
	    (case n
	      ((1)
	       (lambda (a)
		 (hash-table-intern! table a
				     (lambda () (procedure a)))))
	      ((2)
	       (lambda (a1 a2)
		 (hash-table-intern! table (klist a1 a2)
				     (lambda () (procedure a1 a2)))))
	      ((3)
	       (lambda (a1 a2 a3)
		 (hash-table-intern! table (klist a1 a2 a3)
				     (lambda () (procedure a1 a2 a3)))))
	      ((4)
	       (lambda (a1 a2 a3 a4)
		 (hash-table-intern! table (klist a1 a2 a3 a4)
				     (lambda () (procedure a1 a2 a3 a4)))))
	      (else
	       (lambda args
		 (hash-table-intern! table (list->klist args)
				     (lambda () (apply procedure args)))))))

	  (define-integrable (list->list args)
	    args)

	  (make-memoizer table procedure
	    (if weak?
		(get-impl weak-list list->weak-list)
		(get-impl list list->list))))))))