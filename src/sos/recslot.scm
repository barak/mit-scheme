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

;;;; Record Slot Access

(declare (usual-integrations))

(define (%record-accessor-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-accessor index)))))

(define (%record-modifier-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-modifier index)))))

(define (%record-initpred-generator name)
  (lambda (generic tags)
    generic
    (let ((index (%record-slot-index (%record (car tags)) name)))
      (and index
	   (%record-initpred index)))))

(define-syntax generate-index-cases
  (sc-macro-transformer
   (lambda (form environment)
     (let ((index (close-syntax (cadr form) environment))
	   (limit (caddr form))
	   (expand-case (close-syntax (cadddr form) environment)))
       `(CASE ,index
	  ,@(let loop ((i 1))
	      (if (= i limit)
		  `((ELSE (,expand-case ,index)))
		  `(((,i) (,expand-case ,i)) ,@(loop (+ i 1))))))))))

(define (%record-accessor index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index)
	       (ignore-reference-traps (set record-slot-uninitialized)))
      (lambda (record)
	(if (eq? record-slot-uninitialized (%record-ref record index))
	    (error:uninitialized-slot record index)
	    (%record-ref record index))))))

(define (%record-modifier index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index))
      (lambda (record value) (%record-set! record index value)))))

(define (%record-initpred index)
  (generate-index-cases index 16
    (lambda (index)
      (declare (integrate index)
	       (ignore-reference-traps (set record-slot-uninitialized)))
      (lambda (record)
	(not (eq? record-slot-uninitialized (%record-ref record index)))))))

(define (%record-slot-name record index)
  (if (not (and (exact-integer? index) (positive? index)))
      (error:wrong-type-argument index "record index" '%RECORD-SLOT-NAME))
  (let ((names
	 (call-with-current-continuation
	  (lambda (k)
	    (bind-condition-handler (list condition-type:no-applicable-methods)
		(lambda (condition) condition (k 'UNKNOWN))
	      (lambda ()
		(%record-slot-names record))))))
	(index (- index 1)))
    (and (list? names)
	 (< index (length names))
	 (list-ref names index))))

(define %record-slot-index
  (make-generic-procedure 2 '%record-slot-index))

(add-generic-procedure-generator %record-slot-index
  (lambda (generic tags)
    generic
    (and (record-type? (car tags))
	 (lambda (record name)
	   (record-type-field-index (record-type-descriptor record)
				    name
				    #f)))))
(define %record-slot-names
  (make-generic-procedure 1 '%record-slot-names))

(add-generic-procedure-generator %record-slot-names
  (lambda (generic tags)
    generic
    (and (record-type? (car tags))
	 (lambda (record)
	   (record-type-field-names (record-type-descriptor record))))))