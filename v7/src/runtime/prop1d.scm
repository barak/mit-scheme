#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/prop1d.scm,v 14.3 1989/06/06 22:28:51 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

;;;; One Dimensional Property Tables
;;; package: (runtime 1d-property)

(declare (usual-integrations))

(define (initialize-package!)
  (set! population-of-1d-tables (make-population))
  (add-secondary-gc-daemon! gc-1d-tables!))

(define (initialize-unparser!)
  (unparser/set-tagged-pair-method! 1d-table-tag
				    (unparser/standard-method '1D-TABLE)))

(define population-of-1d-tables)

(define (gc-1d-tables!)
  (map-over-population! population-of-1d-tables 1d-table/clean!))

(define (make-1d-table)
  (let ((table (list 1d-table-tag)))
    (add-to-population! population-of-1d-tables table)
    table))

(define (1d-table? object)
  (and (pair? object)
       (eq? (car object) 1d-table-tag)))

(define 1d-table-tag
  "1D table")

(define false-key
  "false key")

(define-integrable (weak-cons car cdr)
  (system-pair-cons (ucode-type weak-cons) car cdr))

(define (weak-assq key table)
  (let loop ((previous table) (alist (cdr table)))
    (and (not (null? alist))
	 (let ((entry (car alist))
	       (next (cdr alist)))
	   (let ((key* (system-pair-car entry)))
	     (cond ((not key*)
		    (set-cdr! previous next)
		    (loop previous next))
		   ((eq? key* key)
		    entry)
		   (else
		    (loop alist next))))))))

(define (1d-table/get table key default)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(system-pair-cdr entry)
	default)))

(define (1d-table/lookup table key if-found if-not-found)
  (let ((entry (weak-assq (or key false-key) table)))
    (if entry
	(if-found (system-pair-cdr entry))
	(if-not-found))))

(define (1d-table/put! table key value)
  (let ((key (or key false-key)))
    (let ((entry (weak-assq key table)))
      (if entry
	  (system-pair-set-cdr! entry value)
	  (set-cdr! table
		    (cons (weak-cons key value)
			  (cdr table))))
      unspecific)))

(define (1d-table/remove! table key)
  (let ((key (or key false-key)))
    (let loop ((previous table) (alist (cdr table)))
      (if (not (null? alist))
	  (let ((key* (system-pair-car (car alist)))
		(next (cdr alist)))
	    (loop (if (or (not key*) (eq? key* key))
		      ;; Might as well clean whole list.
		      (begin
			(set-cdr! previous next)
			previous)
		      alist)
		  next))))))

(define (1d-table/clean! table)
  (let loop ((previous table) (alist (cdr table)))
    (if (not (null? alist))
	(let ((next (cdr alist)))
	  (loop (if (system-pair-car (car alist))
		    alist
		    (begin
		      (set-cdr! previous next)
		      previous))
		next)))))

(define (1d-table/alist table)
  (let loop ((previous table) (alist (cdr table)) (result '()))
    (if (null? alist)
	result
	(let ((entry (car alist))
	      (next (cdr alist)))
	  (let ((key (system-pair-car entry)))
	    (if (not key)
		(begin
		  (set-cdr! previous next)
		  (loop previous next result))
		(loop alist
		      next
		      (cons (cons key (system-pair-cdr entry)) result))))))))