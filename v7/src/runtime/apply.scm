#| -*-Scheme-*-

$Id: apply.scm,v 1.6 2002/11/20 19:46:18 cph Exp $

Copyright (c) 1992, 1999, 2001, 2002 Massachusetts Institute of Technology

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

;;;; Definition of APPLY
;;; package: (runtime apply)

(declare (usual-integrations apply))

;;;  This is not a definition because APPLY is needed to boot,
;;;  so there is a binary (primitive) version of apply installed
;;;  at boot time, and this code replaces it.

(define (apply-2 f a0)
  (let ((fail (lambda () (error "apply: Improper argument list" a0))))
    (let-syntax
	((apply-dispatch&bind
	  (sc-macro-transformer
	   (lambda (form environment)
	     (let ((var (close-syntax (cadr form) environment))
		   (clause (caddr form))
		   (clauses (cdddr form)))
	       (if (pair? clauses)
		   (let walk
		       ((lv var)
			(clause clause)
			(clauses clauses)
			(free '()))
		     `(COND ((PAIR? ,lv)
			     ,(if (pair? (cdr clauses))
				  (let ((av (car clause))
					(lv* (make-synthetic-identifier 'L)))
				    `(LET ((,av (CAR ,lv))
					   (,lv* (CDR ,lv)))
				       ,(walk lv*
					      (car clauses)
					      (cdr clauses)
					      (cons av free))))
				  (make-syntactic-closure environment free
				    (cadr (car clauses)))))
			    ((NULL? ,lv)
			     ,(make-syntactic-closure environment free
				(cadr clause)))
			    (ELSE (FAIL))))
		   (make-syntactic-closure environment '() (cadr clause))))))))
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
			   (else ((ucode-primitive apply) f a0))))))
  
(define (apply-entity-procedure self f . args)
  self					; ignored
  (apply-2 f
	   (if (pair? args)
	       (if (pair? (cdr args))
		   (begin
		     ;; This is safe because args is a newly-consed list
		     ;; shared with no other code (modulo debugging).
		     (let loop ((last args) (next (cdr args)))
		       (if (pair? (cdr next))
			   (loop next (cdr next))
			   (set-cdr! last (car next))))
		     args)
		   (car args))
	       '())))

(define (initialize-package!)
  (set! apply
	(make-entity
	 apply-entity-procedure
	 (vector (fixed-objects-item 'ARITY-DISPATCHER-TAG)
		 (lambda ()
		   (error:wrong-number-of-arguments apply '(1 . #F) '()))
		 (lambda (f) (f))
		 apply-2)))
  unspecific)