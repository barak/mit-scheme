#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/symtab.scm,v 1.42 1987/06/24 04:53:40 cph Exp $

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

;;;; Symbol Tables

(declare (usual-integrations))

(define-integrable (make-symbol-table)
  (symbol-hash-table/make 271))

(define-integrable (symbol-table-bindings table)
  (map (lambda (entry)
	 (cons (car entry)
	       (or (binding-value (cdr entry))
		   (error "Missing binding value" entry))))
       (symbol-hash-table/bindings table)))

(define (symbol-table-define! table key value)
  (symbol-hash-table/modify! table key
    (lambda (binding)
      (set-binding-value! binding value)
      binding)
    (lambda ()
      (make-binding value))))

(define (symbol-table-binding table key)
  (symbol-hash-table/lookup* table key
    identity-procedure
    (lambda ()
      (let ((nothing (make-binding #F)))
	(symbol-hash-table/insert! table key nothing)
	nothing))))

(define (symbol-table-value table key)
  (symbol-hash-table/lookup* table key
    (lambda (binding)
      (or (binding-value binding)
	  (error "SYMBOL-TABLE-VALUE: no value" key)))
    (lambda ()
      (error "SYMBOL-TABLE-VALUE: Undefined key" key))))

(define-integrable (symbol-table-undefined-names table)
  (map car (symbol-hash-table/negative-bindings table binding-value)))

(define-integrable (make-binding initial-value)
  (vector initial-value '()))

(define-integrable (binding-value binding)
  (vector-ref binding 0))

(define (set-binding-value! binding value)
  (if (vector-ref binding 0)
      (error "Attempt to redefine variable" binding))
  (vector-set! binding 0 value)
  (for-each (lambda (daemon) (daemon binding))
	    (vector-ref binding 1)))

(define (add-binding-daemon! binding daemon)
  (vector-set! binding 1 (cons daemon (vector-ref binding 1))))

(define (remove-binding-daemon! binding daemon)
  (vector-set! binding 1 (delq! daemon (vector-ref binding 1))))