#| -*-Scheme-*-

$Id: syntab.scm,v 14.5 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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
			  (guarantee-syntax-table parent 'MAKE-SYNTAX-TABLE))))

(define (guarantee-syntax-table table procedure)
  (if (not (syntax-table? table))
      (error:wrong-type-argument table "syntax table" procedure))
  table)

(define (syntax-table/ref table name)
  (guarantee-syntax-table table 'SYNTAX-TABLE/REF)
  (let loop ((table table))
    (and table
	 (let ((entry (assq name (syntax-table/alist table))))
	   (if entry
	       (cdr entry)
	       (loop (syntax-table/parent table)))))))

(define syntax-table-ref
  syntax-table/ref)

(define (syntax-table/define table name transform)
  (guarantee-syntax-table table 'SYNTAX-TABLE/DEFINE)
  (let ((entry (assq name (syntax-table/alist table))))
    (if entry
	(set-cdr! entry transform)
	(set-syntax-table/alist! table
				 (cons (cons name transform)
				       (syntax-table/alist table))))))

(define syntax-table-define
  syntax-table/define)

(define (syntax-table/defined-names table)
  (map car (syntax-table/alist table)))

(define (syntax-table/copy table)
  (guarantee-syntax-table table 'SYNTAX-TABLE/COPY)
  (let loop ((table table))
    (and table
	 (%make-syntax-table (alist-copy (syntax-table/alist table))
			     (loop (syntax-table/parent table))))))

(define (syntax-table/extend table alist)
  (guarantee-syntax-table table 'SYNTAX-TABLE/EXTEND)
  (%make-syntax-table (alist-copy alist) table))