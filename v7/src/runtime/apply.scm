#| -*-Scheme-*-

$Id: apply.scm,v 1.1 1992/11/03 22:40:41 jinx Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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
		(macro (var clause . clauses)
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
					      (cdr clauses)))))))))))

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
			 (else
			  ((ucode-primitive apply) f a0)))))
  
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