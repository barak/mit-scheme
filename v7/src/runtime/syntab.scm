#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/Attic/syntab.scm,v 14.2 1988/06/13 11:52:05 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Syntax Table
;;; package: (runtime syntax-table)

(declare (usual-integrations))

(define-structure (syntax-table (constructor %make-syntax-table)
				(conc-name syntax-table/))
  alist
  (parent false read-only true))

(define (make-syntax-table #!optional parent)
  (%make-syntax-table '()
		      (if (default-object? parent)
			  false
			  (guarantee-syntax-table parent))))

(define (guarantee-syntax-table table)
  (if (not (syntax-table? table)) (error "Illegal syntax table" table))
  table)

(define (syntax-table/ref table name)
  (guarantee-syntax-table table)
  (let loop ((table table))
    (and table
	 (let ((entry (assq name (syntax-table/alist table))))
	   (if entry
	       (cdr entry)
	       (loop (syntax-table/parent table)))))))

(define syntax-table-ref
  syntax-table/ref)

(define (syntax-table/define table name transform)
  (guarantee-syntax-table table)
  (let ((entry (assq name (syntax-table/alist table))))
    (if entry
	(set-cdr! entry transform)
	(set-syntax-table/alist! table
				 (cons (cons name transform)
				       (syntax-table/alist table))))))

(define syntax-table-define
  syntax-table/define)

(define (syntax-table/copy table)
  (guarantee-syntax-table table)
  (let loop ((table table))
    (and table
	 (%make-syntax-table (alist-copy (syntax-table/alist table))
			     (loop (syntax-table/parent table))))))

(define (syntax-table/extend table alist)
  (guarantee-syntax-table table)
  (%make-syntax-table (alist-copy alist) table))