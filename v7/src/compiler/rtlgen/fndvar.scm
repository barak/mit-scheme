#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlgen/fndvar.scm,v 1.4 1990/03/28 06:11:14 jinx Exp $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; RTL Generation: Variable Locatives

(declare (usual-integrations))

(define (find-variable context variable if-compiler if-ic if-cached)
  (if (variable/value-variable? variable)
      (if-compiler
       (let ((continuation (reference-context/procedure context)))
	 (if (continuation/ever-known-operator? continuation)
	     (continuation/register continuation)
	     register:value)))
      (find-variable-internal context variable
	(lambda (variable locative)
	  (if-compiler
	   (if (variable-in-cell? variable)
	       (rtl:make-fetch locative)
	       locative)))
	(lambda (variable block locative)
	  (cond ((variable-in-known-location? context variable)
		 (if-compiler
		  (rtl:locative-offset locative
				       (variable-offset block variable))))
		((ic-block/use-lookup? block)
		 (if-ic locative (variable-name variable)))
		(else
		 (if-cached (variable-name variable))))))))

(define (find-known-variable context variable)
  (find-variable context variable identity-procedure
    (lambda (environment name)
      environment
      (error "Known variable found in IC frame" name))
    (lambda (name)
      (error "Known variable found in IC frame" name))))

(define (find-closure-variable context variable)
  (find-variable-internal context variable
    (lambda (variable locative)
      variable
      locative)
    (lambda (variable block locative)
      block locative
      (error "Closure variable in IC frame" variable))))

(define (find-stack-overwrite-variable context variable)
  (find-variable-no-tricks context variable
    (lambda (variable locative)
      variable
      locative)
    (lambda (variable block locative)
      block locative
      (error "Stack overwrite slot in IC frame" variable))))

(define (find-variable-internal context variable if-compiler if-ic)
  (let ((rvalue (lvalue-known-value variable)))
    (if (and rvalue
	     (rvalue/procedure? rvalue)
	     (procedure/closure? rvalue)
	     (block-ancestor-or-self? (reference-context/block context)
				      (procedure-block rvalue)))
	(begin
	  ;; This is just for paranoia.
	  (if (procedure/trivial-closure? rvalue)
	      (error "Trivial closure value encountered"))
	  (if-compiler
	   variable
	   (block-ancestor-or-self->locative
	    context
	    (procedure-block rvalue)
	    0
	    (procedure-closure-offset rvalue))))
	(let loop ((variable variable))
	  (let ((indirection (variable-indirection variable)))
	    (if indirection
		(loop indirection)
		(let ((register (variable/register variable)))
		  (if register
		      (if-compiler variable (register-locative register))
		      (find-variable-no-tricks context variable
					       if-compiler if-ic)))))))))

(define (find-variable-no-tricks context variable if-compiler if-ic)
  (find-block/variable context variable
    (lambda (offset-locative)
      (lambda (block locative)
	(if-compiler variable
		     (offset-locative locative
				      (variable-offset block variable)))))
    (lambda (block locative)
      (if-ic variable block locative))))

(define (find-definition-variable context lvalue)
  (find-block/variable context lvalue
    (lambda (offset-locative)
      offset-locative
      (lambda (block locative)
	block locative
	(error "Definition of compiled variable" lvalue)))
    (lambda (block locative)
      block
      (values locative (variable-name lvalue)))))

(define (find-block/variable context variable if-known if-ic)
  (with-values
      (lambda ()
	(find-block context
		    0
		    (lambda (block)
		      (if (not block)
			  (error "Unable to find variable" variable))
		      (or (memq variable (block-bound-variables block))
			  (and (not (block-parent block))
			       (memq variable
				     (block-free-variables block)))))))
    (lambda (block locative)
      ((enumeration-case block-type (block-type block)
	 ((STACK) (if-known stack-locative-offset))
	 ((CLOSURE) (if-known rtl:locative-offset))
	 ((IC) if-ic)
	 (else (error "Illegal result type" block)))
       block locative))))

(define (nearest-ic-block-expression context)
  (with-values
      (lambda ()
	(find-block context 0 (lambda (block) (not (block-parent block)))))
    (lambda (block locative)
      (if (not (ic-block? block))
	  (error "NEAREST-IC-BLOCK-EXPRESSION: No IC block"))
      locative)))

(define (closure-ic-locative context block)
  (with-values
      (lambda ()
	(find-block context 0 (lambda (block*) (eq? block* block))))
    (lambda (block locative)
      (if (not (ic-block? block))
	  (error "Closure parent not IC block"))
      locative)))

(define (block-ancestor-or-self->locative context block prefix suffix)
  (stack-locative-offset
   (with-values
       (lambda ()
	 (find-block context prefix (lambda (block*) (eq? block* block))))
     (lambda (block* locative)
       (if (not (eq? block* block))
	   (error "Block is not an ancestor" context block))
       locative))
   suffix))

(define (popping-limit/locative context block prefix suffix)
  (rtl:make-address
   (block-ancestor-or-self->locative context
				     block
				     prefix
				     (+ (block-frame-size block) suffix))))

(define (block-closure-locative context)
  ;; BLOCK must be the invocation block of a closure.
  (stack-locative-offset
   (rtl:make-fetch register:stack-pointer)
   (+ (procedure-closure-offset (reference-context/procedure context))
      (reference-context/offset context))))

(define (register-locative register)
  register)