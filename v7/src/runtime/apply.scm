#| -*-Scheme-*-

$Id: apply.scm,v 1.4 2001/12/23 17:20:59 cph Exp $

Copyright (c) 1992, 1999, 2001 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; Definition of APPLY
;;; package: (runtime apply)

(declare (usual-integrations apply))

;;;  This is not a definition because APPLY is needed to boot,
;;;  so there is a binary (primitive) version of apply installed
;;;  at boot time, and this code replaces it.

(define (apply-2 f a0)
  (define (fail)
    (error "apply: Improper argument list" a0))

  (let-syntax ((apply-dispatch&bind
		(non-hygienic-macro-transformer
		 (lambda (var clause . clauses)
		   (if (null? clauses)
		       (cadr clause)
		       (let walk ((lv var)
				  (clause clause)
				  (clauses clauses))
			 `(if (not (pair? ,lv))
			      (if (null? ,lv)
				  ,(cadr clause)
				  (fail))
			      ,(if (null? (cdr clauses))
				   (cadr (car clauses))
				   (let ((lv* (generate-uninterned-symbol))
					 (av* (car clause)))
				     `(let ((,lv* (cdr ,lv))
					    (,av* (car ,lv)))
					,(walk lv* (car clauses)
					       (cdr clauses))))))))))))
    (apply-dispatch&bind a0
			 (v0 (f))
			 (v1 (f v0))
			 (v2 (f v0 v1))
			 (v3 (f v0 v1 v2))
			 (v4 (f v0 v1 v2 v3))
			 (v5 (f v0 v1 v2 v3 v4))
			 #|
			 (v6 (f v0 v1 v2 v3 v4 v5))
			 (v7 (f v0 v1 v2 v3 v4 v5 v6))
			 |#
			 (else ((ucode-primitive apply) f a0)))))
  
(define (apply-entity-procedure self f . args)
  ;; This is safe because args is a newly-consed list
  ;; shared with no other code (modulo debugging).

  (define (splice! last next)
    (if (null? (cdr next))
	(set-cdr! last (car next))
	(splice! next (cdr next))))

  self					; ignored
  (apply-2 f
	   (cond ((null? args) '())
		 ((null? (cdr args))
		  (car args))
		 (else
		  (splice! args (cdr args))
		  args))))

(define (initialize-package!)
  (set! apply
	(make-entity
	 apply-entity-procedure
	 (vector (fixed-objects-item 'ARITY-DISPATCHER-TAG)
		 (lambda ()
		   (error "apply needs at least one argument"))
		 (lambda (f)
		   (f))
		 apply-2)))
  unspecific)