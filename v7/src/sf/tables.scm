#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/sf/tables.scm,v 4.1 1988/06/13 12:31:31 cph Rel $

Copyright (c) 1987 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; SCode Optimizer: Tables

(declare (usual-integrations)
	 (integrate-external "object"))

;;;; Operations

(define (operations/make)
  (cons '() '()))

(define (operations/lookup operations variable if-found if-not)
  (let ((entry (assq variable (car operations)))
	(finish
	 (lambda (entry)
	   (if-found (vector-ref (cdr entry) 1)
		     (vector-ref (cdr entry) 2)))))
    (if entry
	(if (cdr entry) (finish entry) (if-not))
	(let ((entry (assq (variable/name variable) (cdr operations))))
	  (if entry (finish entry) (if-not))))))

(define (operations/shadow operations variables)
  (cons (map* (car operations)
	      (lambda (variable) (cons variable false))
	      variables)
	(cdr operations)))

(define (operations/bind-global operations operation export? names values)
  (cons (car operations)
	(map* (cdr operations)
	      (lambda (name value)
		(cons name (vector export? operation value)))
	      names values)))

(define (operations/bind operations operation export? names values)
  (cons (let ((make-binding
	       (lambda (name value)
		 (cons name (vector export? operation value)))))
	  (if (eq? values 'NO-VALUES)
	      (map* (car operations)
		    (lambda (name) (make-binding name false))
		    names)
	      (map* (car operations) make-binding names values)))
	(cdr operations)))

(define (operations/extract-external operations procedure)
  (let loop ((elements (car operations)))
    (if (null? elements)
	'()
	(let ((value (cdar elements)) (rest (loop (cdr elements))))
	  (if (and value (vector-ref value 0))
	      (procedure (caar elements) (vector-ref value 1)
			 (vector-ref value 2)
			 (lambda (value) (cons value rest))
			 (lambda () rest))
	      rest)))))