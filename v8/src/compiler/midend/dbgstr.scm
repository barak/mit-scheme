#| -*-Scheme-*-

$Id: dbgstr.scm,v 1.8 1995/01/19 04:55:13 adams Exp $

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

(define-structure
    (new-dbg-expression
     (conc-name new-dbg-expression/)
     (constructor new-dbg-expression/make (expr))
     (constructor new-dbg-expression/make2 (expr block))
     (print-procedure
      (standard-unparser-method 'NEW-DBG-EXPRESSION
	(lambda (expr port)
	  (write-char #\Space port)
	  (display (new-dbg-expression/expr expr) port)))))
  (expr false read-only true)
  (block false read-only false))


(define (new-dbg-expression/new-block dbg-expr block*)
  (new-dbg-expression/make2 (new-dbg-expression/expr dbg-expr)
			    block*))

(define-structure
    (new-dbg-procedure
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

(define (new-dbg-procedure/new-block dbg-proc block*)
  (new-dbg-procedure/%make (new-dbg-procedure/lam-expr dbg-proc)
			   (new-dbg-procedure/lambda-list dbg-proc)
			   block*))

(define-structure
    (new-dbg-continuation
     (conc-name new-dbg-continuation/)
     (constructor new-dbg-continuation/make (type outer inner))
     (constructor new-dbg-continuation/%make))
  (type false read-only true)
  (outer false read-only true)
  (inner false read-only true)
  (block false read-only false))

(define (new-dbg-continuation/new-block dbg-cont block*)
  (new-dbg-continuation/%make (new-dbg-continuation/type dbg-cont)
			      (new-dbg-continuation/outer dbg-cont)
			      (new-dbg-continuation/inner dbg-cont)
			      block*))

(define-structure
    (new-dbg-variable
     (conc-name new-dbg-variable/)
     (constructor new-dbg-variable/make (name block))
     (constructor new-dbg-variable/%make (name expression block extra))
     (print-procedure
      (standard-unparser-method 'NEW-DBG-VARIABLE
	(lambda (var port)
	  (write-char #\Space port)
	  (write (new-dbg-variable/name var) port)
	  (write-string " -> " port)
	  (fluid-let ((*unparser-list-breadth-limit* 5)
		      (*unparser-list-depth-limit*   3))
	    (write (new-dbg-variable/expression var) port))))))
  (name false read-only true)
  (expression `(lookup ,name) read-only true)
  (block false read-only false)
  (extra false read-only false))

(define (new-dbg-variable/new-expression&block variable expression* block*)
  (new-dbg-variable/%make (new-dbg-variable/name variable)
			  expression*
			  block*
			  (new-dbg-variable/extra variable)))

(define-structure 
  (new-dbg-block
   (conc-name new-dbg-block/)
   (constructor new-dbg-block/make (type parent))
   (constructor new-dbg-block/%make)
   (print-procedure
    (standard-unparser-method 'NEW-DBG-BLOCK
      (lambda (block port)
	(write-char #\Space port)
	(write (new-dbg-block/type block) port)
	(if (null? (new-dbg-block/variables block))
	    (write-string " (no vars)")
	    (begin
	      (write-string " vars:" port)
	      (for-each (lambda (var)
			  (write-char #\Space port)
			  (write (new-dbg-variable/name var) port))
			(new-dbg-block/variables block))))))))
  (type false read-only false)
  (variables '() read-only false)
  (parent false read-only false)
  (flattened false read-only false))

(define (new-dbg-block/copy-transforming expression-copier block)
  ;; Copy entire environmnet model structure whilst transforming the
  ;; variable expressions.
  (define (new-variables variables block*)
    (map (lambda (variable)
	   (new-dbg-variable/new-expression&block
	    variable
	    (expression-copier (new-dbg-variable/expression variable))
	    block*))
	 variables))
  (let copy-block ((block block))
    (and block
	 (let ((block*  (new-dbg-block/%make
			 (new-dbg-block/type block)
			 '()
			 (copy-block (new-dbg-block/parent block))
			 (new-dbg-block/flattened block))))
	   (set-new-dbg-block/variables!
	    block*
	    (new-variables (new-dbg-block/variables block) block*))
	   block*))))



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


(define (new-dbg-form/block object)
  (cond ((new-dbg-expression? object)    (new-dbg-expression/block object))
	((new-dbg-procedure? object)     (new-dbg-procedure/block object))
	((new-dbg-continuation? object)  (new-dbg-continuation/block object))
	(else (internal-error "Not a dbg expression or procedure" object))))

(define (new-dbg-form/new-block object block*)
  (cond ((new-dbg-expression? object)
	 (new-dbg-expression/new-block object block*))
	((new-dbg-procedure? object)
	 (new-dbg-procedure/new-block object block*))
	((new-dbg-continuation? object)
	 (new-dbg-continuation/new-block object block*))
	(else (internal-error "Not a dbg expression or procedure" object))))
  

