#| -*-Scheme-*-

$Id: dbgstr.scm,v 1.15 1995/07/08 15:01:24 adams Exp $

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
     (conc-name new-dbg-expression/)
     (constructor new-dbg-expression/make (expr))
     (constructor new-dbg-expression/make2 (expr block))
     (print-procedure
      (standard-unparser-method 'NEW-DBG-EXPRESSION
	(lambda (expr port)
	  (write-char #\Space port)
	  (display (new-dbg-expression/expr expr) port)))))
  (block false read-only false)
  (label false read-only true)
  (expr false read-only true))


(define (new-dbg-expression/new-block dbg-expr block*)
  (new-dbg-expression/make2 (new-dbg-expression/expr dbg-expr)
			    block*))

(define-structure
    (new-dbg-procedure
     (conc-name new-dbg-procedure/)
     (constructor new-dbg-procedure/make (lam-expr))
     (constructor new-dbg-procedure/%make))
  (block false read-only false)
  (label false read-only false)
  (lam-expr false read-only true))

(define (new-dbg-procedure/copy dbg-proc)
  (new-dbg-procedure/%make (new-dbg-procedure/block dbg-proc)
			   (new-dbg-procedure/label dbg-proc)
			   (new-dbg-procedure/lam-expr dbg-proc)))

(define (new-dbg-procedure/new-block dbg-proc block*)
  (new-dbg-procedure/%make block*
			   (new-dbg-procedure/label dbg-proc)
			   (new-dbg-procedure/lam-expr dbg-proc)))

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
     (constructor new-dbg-variable/make (name))
     (constructor new-dbg-variable/%make (name expression))
     (print-procedure
      (standard-unparser-method 'NEW-DBG-VARIABLE
	(lambda (var port)
	  (write-char #\Space port)
	  (write (new-dbg-variable/name var) port)
	  (write-char #\Space port)
	  (write (new-dbg-variable/expression var) port)
	  ))))
  (name false read-only true)
  (expression #F read-only false))

(define (new-dbg-variable/new-expression variable expression*)
  (new-dbg-variable/%make (new-dbg-variable/name variable)
			  expression*))

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
  (variables '#() read-only false))

(define (new-dbg-block/layout block)
  (new-block/variables block))
(define (set-new-dbg-block/layout! block layout)
  (set-new-dbg-block/variables! block layout))


(define (new-dbg-expression->old-dbg-expression label new-info)
  ;; The old info format does not contain source for expressions!
  (and new-info
       (make-dbg-expression
	(new-dbg-block->old-dbg-block (new-dbg-expression/block new-info))
	label)))

(define (new-dbg-procedure->old-dbg-procedure label type new-info)
  (and new-info
       (begin
	 (if (not (new-dbg-procedure? new-info))
	     (internal-error "Not a new-dbg-procedure" new-info))
	 (let ((source-lambda (new-dbg-procedure/lam-expr new-info)))
	   (lambda-components source-lambda
	     (lambda (name required optional rest auxiliary block-decls body)
	       block-decls body		; ignored
	       (pp `(,source-lambda))
	       ;; This does not set the external label!
	       (make-dbg-procedure
		(new-dbg-block->old-dbg-block
		 (new-dbg-procedure/block new-info))
		label			; internal-label
		type
		name
		required
		optional
		rest
		auxiliary
		source-lambda)))))))

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
  

(define-structure
    (dbg-use
     (conc-name dbg-use/)
     (constructor dbg-use/make (name))
     (print-procedure
      (standard-unparser-method 'DBG-USE
	(lambda (u port)
	  (write-char #\Space port)
	  (display (dbg-use/name u) port)))))
  (name #F read-only true)		; e.g. n-15
  (definitions '() read-only false)	; n-15 -> n-15-43
					; n-15 -> cell-ref(n-15-cell)
  ;;(indirect-definitions '() read-only false)
  (expressions '() read-only false)	; accessor(accessor(n-15))
  )

(define *dbg-rewrites*)

(define (dbg-info/make-rewrites)
  (cons 'HEAD '()))

(define (dbg-info/remember from to)
  (define (unconstructable? form)
    (and (CALL/? form)
	 (QUOTE/? (call/operator form))
	 (hash-table/get *dbg-unconstructable-operators*
			 (quote/text (call/operator form)) #F)))
  (let ((to (if (LOOKUP/? to) (lookup/name to) to)))
    (if (and (not (unconstructable? to))
	     (not (continuation-variable? from))
	     (not (eq? from to)))
	(set-cdr! *dbg-rewrites*
		  (cons (vector from to) (cdr *dbg-rewrites*))))))

(define *dbg-unconstructable-operators* (make-eq-hash-table))

(define (dbg-info/for-all-dbg-expressions! procedure)
  (for-each (lambda (from+to)
	      (procedure (vector-ref from+to 1)))
    (cdr *dbg-rewrites*)))

(let ((forbid
       (lambda (operator)
	 (hash-table/put! *dbg-unconstructable-operators* operator #T))))
  (forbid %make-heap-closure)
  (forbid CONS)
  (forbid %cons)
  (forbid %vector))
