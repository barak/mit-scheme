;;; -*- Scheme -*-

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
			      