#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Debugging Info
;;; package: (runtime new-debugging-info)

(declare (usual-integrations))

(define-record-type <debugging-info>
    make-debugging-info
    debugging-info*?
  (expression debugging-info*/expression)
  (environment debugging-info*/environment)
  (subexpression debugging-info*/subexpression))

(define-record-type <printer>
    make-printer
    debugging-info*/printer?
  (procedure printer-procedure))

(define (debugging-info*/apply-printer printer verbose? port)
  ((printer-procedure printer) verbose? port))

(define-record-type <undefined-expression>
    make-undefined-expression
    debugging-info*/undefined-expression?)

(define undefined-exp (make-undefined-expression))

(define-record-type <undefined-environment>
    make-undefined-environment
    debugging-info*/undefined-environment?)

(define undefined-env (make-undefined-environment))

(define-record-type <unknown-expression>
    make-unknown-expression
    debugging-info*/unknown-expression?)

(define unknown-exp (make-unknown-expression))

(define-record-type <compiled-code>
    make-compiled-code
    debugging-info*/compiled-code?)

(define compiled-code (make-compiled-code))

(define (stack-frame*/debugging-info* frame)
  ((get-frame-generator frame) frame))

(define (define-return-code-generator type generator)
  (alist-table-set! return-code-generators type generator))

(define (define-return-type-generator frame-type generator)
  (alist-table-set! return-type-generators frame-type generator))

(define (get-frame-generator frame)
  (let ((cpoint (stack-frame*/cpoint-frame frame)))
    (or (let ((code (cpoint-frame-return-code cpoint)))
	  (and code
	       (alist-table-ref return-code-generators
				(microcode-return/code->name code)
				#f)))
	(alist-table-ref return-type-generators
			 (cpoint-frame-return-type cpoint)))))

(define return-code-generators (alist-table eq?))
(define return-type-generators (alist-table eq?))

(define (select-subexp exp)
  (cond ((scode-access? exp) (scode-access-environment exp))
	((scode-assignment? exp) (scode-assignment-value exp))
	((scode-conditional? exp) (scode-conditional-predicate exp))
	((scode-definition? exp) (scode-definition-value exp))
	((scode-disjunction? exp) (scode-disjunction-predicate exp))
	((scode-sequence? exp) (scode-sequence-first exp))
	(else (error "Can't select subexpression:" exp))))

(define (validate-subexp subexp frame)
  (if (eq? (stack-frame*/previous-type frame) 'pop-return-error)
      undefined-exp
      subexp))

(define (generate-null frame)
  (declare (ignore frame))
  (make-debugging-info undefined-exp
		       undefined-env
		       undefined-exp))

(define (generate-default frame)
  (make-debugging-info (make-printer
			(lambda (verbose? port)
			  (write-string "Unknown " port)
			  (if verbose?
			      (pp frame port)
			      (write frame port))))
		       undefined-env
		       undefined-exp))

(define (generate-application frame)
  (make-debugging-info (make-scode-combination
			(stack-frame*/field-value frame 'procedure)
			(stack-frame*/field-value frame 'arguments))
		       undefined-env
		       undefined-exp))

(define (generate-compiled-address frame)
  (or (let ((entry (stack-frame*/return-address frame)))
	(and entry
	     (let ((dbg (compiled-entry/dbg-object entry)))
	       (and dbg
		    (cond ((dbg-continuation? dbg)
			   (gen-cc-continuation dbg frame))
			  ((dbg-procedure? dbg)
			   (gen-cc-procedure dbg frame))
			  (else #f))))))
      (generate-default frame)))

(define (gen-cc-continuation dbg frame)
  (let ((source (dbg-continuation/source-code dbg)))
    (and (vector? source)
	 (fix:>= (vector-length source) 2)
	 (let ((exp (vector-ref source 1)))
	   (case (vector-ref source 0)
	     ((access-continue
	       assignment-continue
	       conditional-decide
	       conditional-predicate
	       definition-continue
	       sequence-continue)
	      (make-debugging-info
	       exp
	       (stack-frame*/environment frame undefined-env)
	       (validate-subexp (select-subexp exp) frame)))
	     ((combination-operand)
	      (make-debugging-info
	       exp
	       (stack-frame*/environment frame undefined-env)
	       (validate-subexp
		(scode-combination-element exp (vector-ref source 2))
		frame)))
	     ((combination-element
	       conditional-predicate
	       sequence-element)
	      (make-debugging-info exp
				   undefined-env
				   (vector-ref source 2)))
	     (else #f))))))

(define (gen-cc-procedure dbg frame)
  (make-debugging-info (scode-lambda-body (dbg-procedure/source-code dbg))
		       (and (dbg-procedure/block dbg)
			    (stack-frame*/environment frame undefined-env))
		       undefined-exp))

;; TODO: requires changes in "environment.scm".
(define (stack-frame*/environment frame undefined-env)
  (declare (ignore frame))
  undefined-env)

;; type: with-arg-subproblem

(define-return-code-generator 'access-continue
  (lambda (frame)
    (let ((exp (stack-frame*/field-value frame 'expression)))
      (make-debugging-info exp
			   undefined-env
			   (validate-subexp (select-subexp exp) frame)))))

;; type: exp+env

(define-return-type-generator 'exp+env
  (lambda (frame)
    (let ((exp (stack-frame*/field-value frame 'expression)))
      (make-debugging-info exp
			   (stack-frame*/field-value frame 'environment)
			   (validate-subexp (select-subexp exp) frame)))))

(define-return-code-generator 'eval-error
  (lambda (frame)
    (make-debugging-info (stack-frame*/field-value frame 'expression)
			 (stack-frame*/field-value frame 'environment)
			 undefined-exp)))

;; type: apply

(define-return-type-generator 'apply
  generate-application)

;; type: combination-apply

(define-return-type-generator 'combination-apply
  generate-null)

;; type: return-to-compiled-code-subproblem

(define-return-code-generator 'compiler-assignment-trap-restart
  (lambda (frame)
    (make-debugging-info
     (make-scode-assignment (stack-frame*/field-value frame 'variable)
			    (stack-frame*/field-value frame 'value))
     (stack-frame*/field-value frame 'environment)
     undefined-exp)))

(define-return-code-generator 'compiler-error-restart
  (lambda (frame)
    (let ((primitive (stack-frame*/field-value frame 'primitive)))
      (if (primitive-procedure? primitive)
	  (make-debugging-info
	   (make-scode-combination (make-scode-variable 'apply)
				   (list primitive unknown-exp))
	   undefined-env
	   undefined-exp)
	  (generate-default frame)))))

(define-return-code-generator 'compiler-interrupt-restart
  generate-compiled-address)

(define (generate-compiler-lookup-apply-trap-restart frame)
  (make-debugging-info
   (make-scode-combination
    (make-scode-variable (stack-frame*/field-value frame 'variable))
    (stack-frame*/field-value frame 'arguments))
   (stack-frame*/field-value frame 'environment)
   undefined-exp))

(define-return-code-generator 'compiler-lookup-apply-trap-restart
  generate-compiler-lookup-apply-trap-restart)

(define-return-code-generator 'compiler-operator-lookup-trap-restart
  generate-compiler-lookup-apply-trap-restart)

(define-return-code-generator 'compiler-reference-trap-restart
  (lambda (frame)
    (make-debugging-info
     (make-scode-variable (stack-frame*/field-value frame 'variable))
     (stack-frame*/field-value frame 'environment)
     undefined-exp)))

(define-return-code-generator 'compiler-safe-reference-trap-restart
  (lambda (frame)
    (make-debugging-info
     (make-scode-variable (stack-frame*/field-value frame 'variable) #t)
     (stack-frame*/field-value frame 'environment)
     undefined-exp)))

(define-return-code-generator 'compiler-unassigned?-trap-restart
  (lambda (frame)
    (make-debugging-info
     (make-scode-unassigned? (stack-frame*/field-value frame 'variable))
     (stack-frame*/field-value frame 'environment)
     undefined-exp)))

(define-return-code-generator 'reenter-compiled-code
  generate-null)

;;; other types

(define-return-type-generator 'combination-save
  (lambda (frame)
    (let ((exp (stack-frame*/field-value frame 'expression))
	  (arg (fix:- (stack-frame*/field-value frame 'number-of-blanks) 1)))
      (make-debugging-info exp
			   (stack-frame*/field-value frame 'environment)
			   (validate-subexp (scode-combination-operand exp arg)
					    frame)))))

(define-return-type-generator 'hardware-trap
  (lambda (frame)
    (make-debugging-info
     (make-printer
      (lambda (verbose? port)
	(describe-cpoint-hardware-trap-frame
	 (stack-frame*/cpoint-frame frame) verbose? port)))
     undefined-env
     undefined-exp)))

(define-return-type-generator 'compiled-address
  generate-compiled-address)

(define-return-type-generator 'cc-bkpt
  generate-application)