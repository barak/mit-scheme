#| -*-Scheme-*-

$Id: syntab.scm,v 14.9 2001/12/21 18:22:36 cph Exp $

Copyright (c) 1988-1999, 2001 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.
|#

;;;; Syntax Table
;;; package: (runtime syntax-table)

(declare (usual-integrations))

(define-structure (syntax-table (constructor %make-syntax-table)
				(predicate %syntax-table?)
				(conc-name syntax-table/))
  alist
  (parent #f read-only #t))

(define (syntax-table? object)
  (or (%syntax-table? object)
      (environment? object)))

(define (make-syntax-table parent)
  (guarantee-syntax-table parent 'MAKE-SYNTAX-TABLE)
  (%make-syntax-table '() parent))

(define (guarantee-syntax-table table procedure)
  (if (not (syntax-table? table))
      (error:wrong-type-argument table "syntax table" procedure))
  table)

(define (syntax-table/ref table name)
  (guarantee-syntax-table table 'SYNTAX-TABLE/REF)
  (let loop ((table table))
    (if (%syntax-table? table)
	(let ((entry (assq name (syntax-table/alist table))))
	  (if entry
	      (cdr entry)
	      (let ((parent (syntax-table/parent table)))
		(if (eq? parent 'NONE)
		    #f
		    (loop parent)))))
	(and (environment-bound? table name)
	     (environment-lookup-macro table name)))))

(define (syntax-table/define table name transform)
  (guarantee-syntax-table table 'SYNTAX-TABLE/DEFINE)
  (if (%syntax-table? table)
      (let ((entry (assq name (syntax-table/alist table))))
	(if entry
	    (set-cdr! entry transform)
	    (set-syntax-table/alist! table
				     (cons (cons name transform)
					   (syntax-table/alist table)))))
      (environment-define-macro table name transform)))

(define (syntax-table/extend table alist)
  (guarantee-syntax-table table 'SYNTAX-TABLE/EXTEND)
  (%make-syntax-table (alist-copy alist) table))

(define (syntax-table/environment table)
  (guarantee-syntax-table table 'SYNTAX-TABLE/ENVIRONMENT)
  (let loop ((table table))
    (if (%syntax-table? table)
	(loop (syntax-table/parent table))
	table)))