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
    undefined-expression
    debugging-info*/undefined-expression?)

(define-record-type <unknown-expression>
    unknown-expression
    debugging-info*/unknown-expression?)

(define-record-type <compiled-code>
    compiled-code
    debugging-info*/compiled-code?)

(define-record-type <undefined-environment>
    undefined-environment
    debugging-info*/undefined-environment?)

(define (cpoint-frame-debugging-info* cpoint)
  ((get-frame-generator cpoint) cpoint))

(define (define-return-code-generator type generator)
  (alist-table-set! return-code-generators type generator))

(define (define-return-type-generator frame-type generator)
  (alist-table-set! return-type-generators frame-type generator))

(define (get-frame-generator cpoint)
  (or (let ((code (cpoint-frame-return-code cpoint)))
	(and code
	     (alist-table-ref return-code-generators
			      (microcode-return/code->name code)
			      #f)))
      (alist-table-ref return-type-generators
		       (cpoint-frame-return-code cpoint))))

(define return-code-generators (alist-table eq?))
(define return-type-generators (alist-table eq?))

(define (select-subexpression exp)
  (cond ((scode-access? exp) (scode-access-environment exp))
	((scode-assignment? exp) (scode-assignment-value exp))
	((scode-conditional? exp) (scode-conditional-predicate exp))
	((scode-definition? exp) (scode-definition-value exp))
	((scode-disjunction? exp) (scode-disjunction-predicate exp))
	((scode-sequence? exp) (scode-sequence-first exp))
	(else (error "Can't select subexpression:" exp))))

(define (generate-null cpoint)
  (declare (ignore cpoint))
  (make-debugging-info (undefined-expression)
		       (undefined-environment)
		       (undefined-expression)))

(define (generate-default cpoint)
  (make-debugging-info (make-printer
			(lambda (verbose? port)
			  (write-string "Unknown " port)
			  (if verbose?
			      (pp cpoint port)
			      (write cpoint port))))
		       (undefined-environment)
		       (undefined-expression)))

(define (generate-application cpoint)
  (make-debugging-info (make-scode-combination
			(cpoint-frame-field-value cpoint 'procedure)
			(cpoint-frame-field-value cpoint 'arguments))
		       (undefined-environment)
		       (undefined-expression)))

;; TODO: Needs to look at debugging information in the compiled code.
(define (generate-compiled-address cpoint)
  (if (cpoint-frame-field-name? cpoint 'procedure)
      (generate-application cpoint)
      (generate-default cpoint)))

;; type: with-arg-subproblem

(define-return-code-generator 'access-continue
  (lambda (cpoint)
    (let ((exp (cpoint-frame-field-value cpoint 'expression)))
      (make-debugging-info exp
			   (undefined-environment)
			   (select-subexpression exp)))))

;; type: exp+env

(define-return-type-generator 'exp+env
  (lambda (cpoint)
    (let ((exp (cpoint-frame-field-value cpoint 'expression)))
      (make-debugging-info exp
			   (cpoint-frame-field-value cpoint 'environment)
			   (select-subexpression exp)))))

(define-return-code-generator 'eval-error
  (lambda (cpoint)
    (make-debugging-info (cpoint-frame-field-value cpoint 'expression)
			 (cpoint-frame-field-value cpoint 'environment)
			 (undefined-expression))))

;; type: apply

(define-return-type-generator 'apply
  generate-application)

;; type: combination-apply

(define-return-type-generator 'combination-apply
  generate-null)

;; type: return-to-compiled-code-subproblem

(define-return-code-generator 'compiler-assignment-trap-restart
  (lambda (cpoint)
    (make-debugging-info
     (make-scode-assignment (cpoint-frame-field-value cpoint 'variable)
			    (cpoint-frame-field-value cpoint 'value))
     (cpoint-frame-field-value cpoint 'environment)
     (undefined-expression))))

(define-return-code-generator 'compiler-error-restart
  (lambda (cpoint)
    (let ((primitive (cpoint-frame-field-value cpoint 'primitive)))
      (if (primitive-procedure? primitive)
	  (make-debugging-info
	   (make-scode-combination (make-scode-variable 'apply)
				   (list primitive (unknown-expression)))
	   (undefined-environment)
	   (undefined-expression))
	  (generate-default cpoint)))))

(define-return-code-generator 'compiler-interrupt-restart
  generate-compiled-address)

(define (generate-compiler-lookup-apply-trap-restart cpoint)
  (make-debugging-info
   (make-scode-combination
    (make-scode-variable (cpoint-frame-field-value cpoint 'variable))
    (cpoint-frame-field-value cpoint 'arguments))
   (cpoint-frame-field-value cpoint 'environment)
   (undefined-expression)))

(define-return-code-generator 'compiler-lookup-apply-trap-restart
  generate-compiler-lookup-apply-trap-restart)

(define-return-code-generator 'compiler-operator-lookup-trap-restart
  generate-compiler-lookup-apply-trap-restart)

(define-return-code-generator 'compiler-reference-trap-restart
  (lambda (cpoint)
    (make-debugging-info
     (make-scode-variable (cpoint-frame-field-value cpoint 'variable))
     (cpoint-frame-field-value cpoint 'environment)
     (undefined-expression))))

(define-return-code-generator 'compiler-safe-reference-trap-restart
  (lambda (cpoint)
    (make-debugging-info
     (make-scode-variable (cpoint-frame-field-value cpoint 'variable) #t)
     (cpoint-frame-field-value cpoint 'environment)
     (undefined-expression))))

(define-return-code-generator 'compiler-unassigned?-trap-restart
  (lambda (cpoint)
    (make-debugging-info
     (make-scode-unassigned? (cpoint-frame-field-value cpoint 'variable))
     (cpoint-frame-field-value cpoint 'environment)
     (undefined-expression))))

(define-return-code-generator 'reenter-compiled-code
  generate-null)

;;; other types

(define-return-type-generator 'combination-save
  (lambda (cpoint)
    (let ((exp (cpoint-frame-field-value cpoint 'expression))
	  (arg (fix:- (cpoint-frame-field-value cpoint 'number-of-blanks) 1)))
      (make-debugging-info exp
			   (cpoint-frame-field-value cpoint 'environment)
			   (scode-combination-operand exp arg)))))

(define-return-type-generator 'hardware-trap
  (lambda (cpoint)
    (make-debugging-info
     (make-printer
      (lambda (verbose? port)
	(describe-cpoint-hardware-trap-frame cpoint verbose? port)))
     (undefined-environment)
     (undefined-expression))))

(define-return-type-generator 'compiled-address
  generate-compiled-address)

(define-return-type-generator 'cc-bkpt
  generate-application)