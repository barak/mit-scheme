#| -*-Scheme-*-

$Id: dbgstr.scm,v 1.3 1994/11/25 23:03:33 adams Exp $

Copyright (c) 1994 Massachusetts Institute of Technology

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

(declare (usual-integrations))

(define-structure (new-dbg-expression
		   (conc-name new-dbg-expression/)
		   (constructor new-dbg-expression/make (expr)))
  (expr false read-only true)
  (block false read-only false))

(define-structure (new-dbg-procedure
		   (conc-name new-dbg-procedure/)
		   (constructor new-dbg-procedure/make (lam-expr lambda-list))
		   (constructor new-dbg-procedure/%make))
  (lam-expr false read-only true)
  (lambda-list false read-only true)
  (block false read-only false))

(define (new-dbg-procedure/copy dbg-proc)
  (new-dbg-procedure/%make (new-dbg-procedure/lam-expr dbg-proc)
			   (new-dbg-procedure/lambda-list dbg-proc)
			   (new-dbg-procedure/block dbg-proc)))

(define-structure (new-dbg-continuation
		   (conc-name new-dbg-continuation/)
		   (constructor new-dbg-continuation/make (type outer inner)))
  (type false read-only true)
  (outer false read-only true)
  (inner false read-only true)
  (block false read-only false))

(define-structure (new-dbg-variable
		   (conc-name new-dbg-variable/)
		   (constructor new-dbg-variable/make (name block)))
  (name false read-only true)
  (original-name name read-only true)
  (block false read-only false)
  (original-block block read-only false)
  (offset false read-only false)
  (extra false read-only false))

(define-structure (new-dbg-block
		   (conc-name new-dbg-block/)
		   (constructor new-dbg-block/make (type parent)))
  (type false read-only false)
  (variables '() read-only false)
  (parent false read-only false)
  (flattened false read-only false))

(define (new-dbg-expression->old-dbg-expression label new-info)
  ;; The old info format does not contain source for expressions!
  (and new-info
       (make-dbg-expression
	(new-dbg-block->old-dbg-block (new-dbg-expression/block new-info))
	label)))

(define (new-dbg-procedure->old-dbg-procedure label type new-info)
  (and new-info				; (lam-expr lambda-list block)
       (call-with-values
	(lambda ()
	  (lambda-list/parse (new-dbg-procedure/lambda-list new-info)))
	(lambda (required optional rest aux)
	  ;; This does not set the external label!
	  (make-dbg-procedure
	   (new-dbg-block->old-dbg-block
	    (new-dbg-procedure/block new-info))
	   label			; internal-label
	   type
	   (car required)		; name
	   (cdr required)		; true required
	   optional
	   rest
	   aux
	   (new-dbg-procedure/lam-expr new-info))))))

(define (new-dbg-continuation->old-dbg-continuation label frame-size new-info)
  (and new-info
       (new-dbg-continuation/outer new-info)
       (new-dbg-continuation/inner new-info)
       (let ((frame-size (+ frame-size 1))
	     (type (new-dbg-continuation/type new-info))
	     (new-block (new-dbg-block->old-dbg-block
			 (new-dbg-continuation/block new-info)))
	     (aggregate
	      (new-dbg-expression/expr
	       (new-dbg-continuation/outer new-info)))
	     (element
	      (new-dbg-expression/expr
	       (new-dbg-continuation/inner new-info))))	     
	 (make-dbg-continuation
	  new-block
	  label
	  false				; ?? type
	  frame-size
	  (vector (case type
		    ((RATOR-OR-RAND)
		     'COMBINATION-ELEMENT)
		    ((BEGIN)
		     'SEQUENCE-ELEMENT)
		    ((PREDICATE)
		     'CONDITIONAL-PREDICATE)
		    (else
		     "new-dbg-continuation->old-dbg-continuation: Unkown type"
		     type))
		  aggregate
		  element)))))

(define (new-dbg-block->old-dbg-block block)
  ;; For now
  block					; ignored
  false)