#| -*-Scheme-*-

$Id: dbgstr.scm,v 1.18 1995/09/08 02:44:17 adams Exp $

Copyright (c) 1994-1995 Massachusetts Institute of Technology

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
     (type vector)
     (named
      ((ucode-primitive string->symbol)
       "#[(runtime compiler-info)new-dbg-expression]"))
     (conc-name new-dbg-expression/)
     (constructor new-dbg-expression/make (source-code outer))
     (constructor new-dbg-expression/make2 (source-code block outer))
     (print-procedure
      (standard-unparser-method 'NEW-DBG-EXPRESSION
	(lambda (expr port)
	  (write-char #\Space port)
	  (display (new-dbg-expression/source-code expr) port)))))
  (block false read-only false)
  (label false)
  (source-code false)			; SCode
  (outer false))			; SCode countaining form, or #F


(define (new-dbg-expression/new-block dbg-expr block*)
  (new-dbg-expression/make2 (new-dbg-expression/source-code dbg-expr)
			    block*
			    (new-dbg-expression/outer dbg-expr)))

(define-structure
    (new-dbg-procedure
     (type vector)
     (named
      ((ucode-primitive string->symbol)
       "#[(runtime compiler-info)new-dbg-procedure]"))
     (conc-name new-dbg-procedure/)
     (constructor new-dbg-procedure/make (source-code))
     (constructor new-dbg-procedure/%make))
  (block false read-only false)
  (label false read-only false)
  (source-code false read-only true))

(define (new-dbg-procedure/copy dbg-proc)
  (new-dbg-procedure/%make (new-dbg-procedure/block dbg-proc)
			   (new-dbg-procedure/label dbg-proc)
			   (new-dbg-procedure/source-code dbg-proc)))

(define (new-dbg-procedure/new-block dbg-proc block*)
  (new-dbg-procedure/%make block*
			   (new-dbg-procedure/label dbg-proc)
			   (new-dbg-procedure/source-code dbg-proc)))

(define (new-dbg-procedure/label-offset procedure)
  (dbg-label/offset
   (or ;;(dbg-procedure/external-label procedure)
       (new-dbg-procedure/label procedure))))

(define-integrable (new-dbg-procedure<? x y)
  (< (new-dbg-procedure/label-offset x) (new-dbg-procedure/label-offset y)))

(define-structure
    (new-dbg-continuation
     (type vector)
     (named
      ((ucode-primitive string->symbol)
       "#[(runtime compiler-info)new-dbg-continuation]"))
     (conc-name new-dbg-continuation/)
     (constructor new-dbg-continuation/make (type outer inner))
     (constructor new-dbg-continuation/%make))
  (block false)
  (label false)
  (type false read-only true)
  (outer false)
  (inner false))

(define (new-dbg-continuation/new-block dbg-cont block*)
  (new-dbg-continuation/%make block*
			      (new-dbg-continuation/label dbg-cont)
			      (new-dbg-continuation/type dbg-cont)
			      (new-dbg-continuation/outer dbg-cont)
			      (new-dbg-continuation/inner dbg-cont)))


(define (new-dbg-variable? object)
  (and (pair? object) (symbol? (car object))))

(define-integrable (new-dbg-variable/make name) (cons name #F))
(define-integrable (new-dbg-variable/name var) (car var))
(define-integrable (new-dbg-variable/path var) (cdr var))
(define-integrable (set-new-dbg-variable/path! var path) (set-cdr! var path))

;;Copying version:
(define (new-dbg-variable/new-path variable path*)
  (cons (new-dbg-variable/name variable) path*))


(define-structure 
  (new-dbg-block
   (type vector)
   (named
    ((ucode-primitive string->symbol)
     "#[(runtime compiler-info)new-dbg-block]"))
   (conc-name new-dbg-block/)
   (constructor new-dbg-block/make (type parent))
   (constructor new-dbg-block/%make)
   (print-procedure
    (standard-unparser-method 'NEW-DBG-BLOCK
      (lambda (block port)
	(write-char #\Space port)
	(write (new-dbg-block/type block) port)
	(let* ((vars  (new-dbg-block/variables block)))
	  (if (zero? (vector-length vars))
	      (write-string " (no vars)" port)
	      (begin
		(write-string " vars:" port)
		(for-each-vector-element vars
		  (lambda (var)
		    (write-char #\Space port)
		    (write (if (new-dbg-variable? var)
			       (new-dbg-variable/name var)
			       var)
			   port))))))))))
  ;; TYPE is one of 'NESTED, 'FIRST-CLASS
  (type false read-only false)
  ;; PARENT is either
  ;;  . a DBG-BLOCK
  ;;  . 'IC, in which case PARENT-PATH-PREFIX must yield an IC environment
  ;;  . #F if there is no environment.
  (parent false read-only false)
  ;; . PARENT-PATH-PREFIX is the prefix of any element in the parent
  ;;   path. Typically used to access closed environments and first
  ;;   class environments.
  ;; . For 'FIRST-CLASS environments PARENT-PATH-PREFIX initially holds the
  ;;   name of the bound to the environment. (i.e. it's own, not
  ;;   parent, prefix)
  (parent-path-prefix false read-only false)
  ;; VARIABLES is a vector of NEW-DBG-VARIABLEs
  (variables '#() read-only false)
  (procedure #F))

(define (new-dbg-expression->old-dbg-expression label new-info)
  ;; The old info format does not contain source for expressions!
  (and new-info
       (begin
	 (set-new-dbg-expression/label! new-info label)
	 (set-new-dbg-expression/source-code! new-info #F) ;save space
	 new-info)))

(define (new-dbg-procedure->old-dbg-procedure label type new-info)
  type
  (and new-info
       (begin
	 (set-new-dbg-procedure/label! new-info label)
	 new-info)))


(define (new-dbg-continuation->old-dbg-continuation label frame-size new-info)
  frame-size				; ignored
  (and new-info
       ;;(new-dbg-continuation/outer new-info)
       (new-dbg-continuation/inner new-info)
       (let* ((element
	       (new-dbg-expression/source-code
		(new-dbg-continuation/inner new-info)))
	      (aggregate
	       ;; This condition is true when a user level form has internal
	       ;; invisible continuations
	       (if (or (not (new-dbg-continuation/outer new-info))
		       (eq? (new-dbg-continuation/outer new-info)
			    (new-dbg-continuation/inner new-info)))
		   (new-dbg-expression/outer
		    (new-dbg-continuation/inner new-info))
		   (new-dbg-expression/source-code
		    (new-dbg-continuation/outer new-info)))))
	 (and aggregate
	      (begin
		(set-new-dbg-continuation/label! new-info label)
		(set-new-dbg-continuation/outer! new-info aggregate)
		(set-new-dbg-continuation/inner! new-info element)
		new-info)))))


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

#|

Invariants:

The block associated with a procedure, continuation, expression etc is
the invocation frame.

The parent of the invocation frame is parsable from (i.e. contains
access paths rooted at)

 . the stack-frame object for closures
 . the entry for other entry kinds

|#