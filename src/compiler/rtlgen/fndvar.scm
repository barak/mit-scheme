#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

This file is part of MIT/GNU Scheme.

MIT/GNU Scheme is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

MIT/GNU Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT/GNU Scheme; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; RTL Generation: Variable Locatives
;;; package: (compiler rtl-generator)

(declare (usual-integrations))

(define-integrable (find-variable/locative context variable
					   if-compiler if-ic if-cached)
  (find-variable false context variable if-compiler if-ic if-cached))

(define-integrable (find-variable/value context variable
					if-compiler if-ic if-cached)
  (find-variable true context variable if-compiler if-ic if-cached))

(define-integrable (find-variable/value/simple context variable message)
  (find-variable/value context variable
		       identity-procedure
		       (lambda (environment name)
			 environment	; ignored
			 (error message name))
		       (lambda (name)
			 (error message name))))

(define (find-known-variable context variable)
  (find-variable/value/simple
   context variable
   "find-known-variable: Known variable found in IC frame"))

(define (find-closure-variable context variable)
  (find-variable-internal context variable
    identity-procedure
    (lambda (variable locative)
      variable				; ignored
      (rtl:make-fetch locative))
    (lambda (variable block locative)
      block locative			; ignored
      (error "Closure variable in IC frame" variable))))

(define (find-stack-overwrite-variable context variable)
  (find-variable-no-tricks context variable
    (lambda (variable locative)
      variable				; ignored
      locative)
    (lambda (variable block locative)
      block locative			; ignored
      (error "Stack overwrite slot in IC frame" variable))))

(define (find-variable get-value? context variable if-compiler if-ic if-cached)
  (if (variable/value-variable? variable)
      (begin
	(if (not get-value?)
	    (error "Can't take locative of value variable" variable))
	(if-compiler
	 (if (lvalue-integrated? variable)
	     (let ((rvalue (lvalue-known-value variable)))
	       (cond ((rvalue/constant? rvalue)
		      (rtl:make-constant (constant-value rvalue)))
		     ((and (rvalue/procedure? rvalue)
			   (procedure/trivial-or-virtual? rvalue))
		      (make-trivial-closure-cons rvalue))
		     (else
		      (error "illegal integrated value variable" variable))))
	     (rtl:make-fetch
	      (let ((continuation (reference-context/procedure context)))
		(if (continuation/ever-known-operator? continuation)
		    (continuation/register continuation)
		    register:value))))))
      (let ((if-locative
	     (if get-value?
		 (lambda (locative)
		   (if-compiler (rtl:make-fetch locative)))
		 if-compiler)))
	(find-variable-internal context variable
	  (and get-value? if-compiler)
	  (lambda (variable locative)
	    (if-locative
	     (if (variable-in-cell? variable)
		 (rtl:make-fetch locative)
		 locative)))
	  (lambda (variable block locative)
	    (cond ((variable-in-known-location? context variable)
		   (if-locative
		    (rtl:locative-offset locative
					 (variable-offset block variable))))
		  ((ic-block/use-lookup? block)
		   (if-ic locative (variable-name variable)))
		  (else
		   (if-cached (variable-name variable)))))))))

(define (find-variable-internal context variable if-value if-locative if-ic)
  (define (loop variable)
    (let ((indirection (variable-indirection variable)))
      (cond ((not indirection)
	     (let ((register (variable/register variable)))
	       (if register
		   (if-locative variable (register-locative register))
		   (find-variable-no-tricks context variable
					    if-locative if-ic))))
	    ((not (cdr indirection))
	     (loop (car indirection)))
	    (else
	     (error "find-variable-internal: Indirection not for value"
		    variable)))))

  (let ((rvalue (lvalue-known-value variable)))
    (cond ((or (not if-value)
	       (not rvalue))
	   (loop variable))
	  ((rvalue/block? rvalue)
	   (let* ((sblock (block-nearest-closure-ancestor
			   (reference-context/block context)))
		  (cblock (and sblock (block-parent sblock))))
	     (if (and cblock (eq? rvalue (block-shared-block cblock)))
		 (if-value
		  (redirect-closure context
				    sblock
				    (block-procedure sblock)
				    (indirection-block-procedure rvalue)))
		 (loop variable))))
	  ((not (rvalue/procedure? rvalue))
	   (loop variable))
	  ((procedure/trivial-or-virtual? rvalue)
	   (if-value (make-trivial-closure-cons rvalue)))
	  ((not (procedure/closure? rvalue))
	   (error "find-variable-internal: Reference to open procedure"
		  context variable)
	   (loop variable))
	  (else
	   (let ((nearest-closure (block-nearest-closure-ancestor
				   (reference-context/block context)))
		 (closing-block (procedure-closing-block rvalue)))
	     (if (and nearest-closure
		      (eq? (block-shared-block closing-block)
			   (block-shared-block
			    (block-parent nearest-closure))))
		 (if-value
		  (redirect-closure context
				    nearest-closure
				    (block-procedure nearest-closure)
				    rvalue))
		 (let ((indirection (variable-indirection variable)))
		   (cond ((not indirection)
			  (loop variable))
			 ((not (cdr indirection))
			  (loop (car indirection)))
			 (else
			  (let ((source (car indirection)))
			    ;; Should not be indirected.
			    (find-variable-no-tricks
			     context source
			     (lambda (variable locative)
			       variable	; ignored
			       (if-value (make-closure-redirection
					  (rtl:make-fetch locative)
					  (indirection-block-procedure
					   (lvalue-known-value source))
					  rvalue)))
			     (lambda (new-variable block locative)
			       new-variable block locative ; ignored
			       (error "find-variable-internal: Bad indirection"
				      variable)))))))))))))

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