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

;;;; Memoizers
;;; package: (runtime memoizer)

(declare (usual-integrations))

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

(define (weak-eqv-memoizer get-key get-datum)
  (let ((table (make-key-weak-eqv-hash-table)))
    (make-memoizer table
                   get-datum
                   (lambda args
                     (hash-table-intern! table
                                         (apply get-key args)
                                         (lambda () (apply get-datum args)))))))

(define (all-args-memoizer elt= get-key get-datum)
  (let ((memoizer
         (list-memoizer elt=
                        (lambda (args)
                          (apply get-key args))
                        (lambda (args)
                          (apply get-datum args)))))
    (make-memoizer (memoizer-table memoizer)
                   get-datum
                   (lambda args (memoizer args)))))

(define (make-list-memoizer make-list= dedup?)
  (lambda (elt= get-key get-datum)
    (let ((list= (make-list= elt=)))
      (let ((table (make-hash-table list= (equality-predicate-hasher list=))))
        (make-memoizer
         table
         get-datum
         (lambda (list)
           (let ((list
                  (if dedup?
                      (delete-duplicates list elt=)
                      list)))
             (hash-table-intern! table
                                 (get-key list)
                                 (lambda () (get-datum list))))))))))

(define (make-list= elt=)
  (let ((compare
         (lambda (a b)
           (list= elt= a b))))
    (set-equality-predicate-properties!
     compare
     (%make-list-hash elt=)
     (equality-predicate-rehash-after-gc? elt=))
    compare))

(define (make-lset= elt=)
  (let ((compare
         (lambda (a b)
           (lset= elt= a b))))
    (set-equality-predicate-properties!
     compare
     (%make-list-hash elt=)
     (equality-predicate-rehash-after-gc? elt=))
    compare))

(define (%make-list-hash elt=)
  (let ((elt-hash (equality-predicate-hasher elt=)))
    (lambda (lset #!optional modulus)
      (let ((hash
             (apply +
                    (map (lambda (elt)
                           (elt-hash elt modulus))
                         lset))))
        (if (default-object? modulus)
            hash
            (modulo hash modulus))))))

(define list-memoizer)
(define lset-memoizer)
(add-boot-init!
 (lambda ()
   (set! list-memoizer (make-list-memoizer make-list= #f))
   (set! lset-memoizer (make-list-memoizer make-lset= #t))
   unspecific))

(define (make-simple-list-memoizer list-memoizer)
  (lambda (elt= get-key get-datum)
    (let ((memoizer
           (list-memoizer elt=
                          (lambda (args)
                            (apply get-key args))
                          (lambda (args)
                            (apply get-datum args)))))
      (lambda args
        (memoizer args)))))

(define simple-list-memoizer)
(define simple-lset-memoizer)
(add-boot-init!
 (lambda ()
   (set! simple-list-memoizer (make-simple-list-memoizer list-memoizer))
   (set! simple-lset-memoizer (make-simple-list-memoizer lset-memoizer))
   unspecific))