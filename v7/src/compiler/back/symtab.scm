#| -*-Scheme-*-

$Id: symtab.scm,v 1.48 2002/11/20 19:45:47 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Symbol Tables
;;; package: (compiler assembler)

(declare (usual-integrations))

(define make-symbol-table
  (strong-hash-table/constructor eq-hash-mod eq? #t))

(define (symbol-table-define! table key value)
  (let ((binding (hash-table/get table key #f)))
    (if binding
	(begin
	  (error "Redefining symbol:" key)
	  (set-binding-value! binding value))
	(hash-table/put! table key (make-binding value)))))

(define (symbol-table-value table key)
  (let ((binding (hash-table/get table key #f)))
    (if (not binding)
	(error "Undefined key:" key))
    (let ((value (binding-value binding)))
      (if (not value)
	  (error "Key has no value:" key))
      value)))

(define (symbol-table->assq-list table)
  (map (lambda (pair)
	 (cons (car pair) (binding-value (cdr pair))))
       (symbol-table-bindings table)))

(define-integrable (symbol-table-bindings table)
  (hash-table->alist table))

(define-integrable (make-binding initial-value)
  (cons initial-value '()))

(define-integrable (binding-value binding)
  (car binding))

(define (set-binding-value! binding value)
  (set-car! binding value))