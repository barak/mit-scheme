#| -*-Scheme-*-

$Id: d62f9cad88b1320ff58ff57ad92b2a9f6ba2eab2 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; LAP Generation Rules: Flonum rules

(declare (usual-integrations))

(define (flonum-source! register)
  (float-register->fpr (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (float-register->fpr (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (float-register->fpr (allocate-temporary-register! 'FLOAT)))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let ((source (fpr->float-register (flonum-source! source))))
    (let ((target (standard-target! target)))
      (LAP
       ; (SW 0 (OFFSET 0 ,regnum:free))	; make heap parsable forwards
       (ORI ,regnum:free ,regnum:free #b100) ; Align to odd quad byte
       ,@(deposit-type-address (ucode-type flonum) regnum:free target)
       ,@(with-values
	     (lambda ()
	       (immediate->register
		(make-non-pointer-literal (ucode-type manifest-nm-vector) 2)))
	   (lambda (prefix alias)
	     (LAP ,@prefix
		  (SW ,alias (OFFSET 0 ,regnum:free)))))
       ,@(fp-store-doubleword 4 regnum:free source)
       (ADDI ,regnum:free ,regnum:free 12)))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let ((source (standard-move-to-temporary! source)))
    (let ((target (fpr->float-register (flonum-target! target))))
      (LAP ,@(object->address source source)
	   ,@(fp-load-doubleword 4 source target #T)))))

;; Floating-point vector support

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset))))
  (let* ((base (standard-source! base))
	 (target (fpr->float-register (flonum-target! target))))
    (fp-load-doubleword (* 8 offset) base target #T)))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base))
	(source (fpr->float-register (flonum-source! source))))
    (fp-store-doubleword (* 8 offset) base source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index))))
  (with-indexed-address base index 3
    (lambda (address)
      (fp-load-doubleword 0 address
			  (fpr->float-register (flonum-target! target)) #T))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (REGISTER (? source)))
  (with-indexed-address base index 3
    (lambda (address)
      (fp-store-doubleword 0 address
			   (fpr->float-register (flonum-source! source))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset))))
  (let* ((base (standard-source! base))
	 (target (fpr->float-register (flonum-target! target))))
    (fp-load-doubleword (+ (* 4 w-offset) (* 8 f-offset)) base target #T)))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(MACHINE-CONSTANT (? f-offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base))
	(source (fpr->float-register (flonum-source! source))))
    (fp-store-doubleword (+ (* 4 w-offset) (* 8 f-offset)) base source)))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(REGISTER (? index))))
  (with-indexed-address base index 3
    (lambda (address)
      (fp-load-doubleword (* 4 w-offset) address
			  (fpr->float-register (flonum-target! target))
			  #T))))

(define-rule statement
  (ASSIGN (FLOAT-OFFSET (OFFSET-ADDRESS (REGISTER (? base))
					(MACHINE-CONSTANT (? w-offset)))
			(REGISTER (? index)))
	  (REGISTER (? source)))
  (with-indexed-address base index 3
    (lambda (address)
      (fp-store-doubleword (* 4 w-offset) address
			   (fpr->float-register (flonum-source! source))))))

;;;; Flonum Arithmetic

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-1-ARG (? operation) (REGISTER (? source)) (? overflow?)))
  overflow?				;ignore
  (let ((source (flonum-source! source)))
    ((flonum-1-arg/operator operation) (flonum-target! target) source)))

(define (flonum-1-arg/operator operation)
  (lookup-arithmetic-method operation flonum-methods/1-arg))

(define flonum-methods/1-arg
  (list 'FLONUM-METHODS/1-ARG))

;;; Notice the weird ,', syntax here.
;;; If LAP changes, this may also have to change.

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FLONUM-METHODS/1-ARG
	     (LAMBDA (TARGET SOURCE)
	       (LAP (,(caddr form) ,',TARGET ,',SOURCE))))))))
  (define-flonum-operation flonum-abs ABS.D)
  (define-flonum-operation flonum-negate NEG.D))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLONUM-2-ARGS (? operation)
			 (REGISTER (? source1))
			 (REGISTER (? source2))
			 (? overflow?)))
  overflow?				;ignore
  (let ((source1 (flonum-source! source1))
	(source2 (flonum-source! source2)))
    ((flonum-2-args/operator operation) (flonum-target! target)
					source1
					source2)))

(define (flonum-2-args/operator operation)
  (lookup-arithmetic-method operation flonum-methods/2-args))

(define flonum-methods/2-args
  (list 'FLONUM-METHODS/2-ARGS))

(let-syntax
    ((define-flonum-operation
       (sc-macro-transformer
	(lambda (form environment)
	  environment
	  `(DEFINE-ARITHMETIC-METHOD ',(cadr form) FLONUM-METHODS/2-ARGS
	     (LAMBDA (TARGET SOURCE1 SOURCE2)
	       (LAP (,(caddr form) ,',TARGET ,',SOURCE1 ,',SOURCE2))))))))
  (define-flonum-operation flonum-add ADD.D)
  (define-flonum-operation flonum-subtract SUB.D)
  (define-flonum-operation flonum-multiply MUL.D)
  (define-flonum-operation flonum-divide DIV.D))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((temp (flonum-temporary!))
	(source (flonum-source! source)))
    (LAP (MTC1 0 ,temp)
	 (MTC1 0 ,(+ temp 1))
	 (NOP)
	 ,@(flonum-compare
	    (case predicate
	      ((FLONUM-ZERO?) 'C.EQ.D)
	      ((FLONUM-NEGATIVE?) 'C.LT.D)
	      ((FLONUM-POSITIVE?) 'C.GT.D)
	      (else (error "unknown flonum predicate" predicate)))
	    source temp))))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (flonum-compare (case predicate
		    ((FLONUM-EQUAL?) 'C.EQ.D)
		    ((FLONUM-LESS?) 'C.LT.D)
		    ((FLONUM-GREATER?) 'C.GT.D)
		    (else (error "unknown flonum predicate" predicate)))
		  (flonum-source! source1)
		  (flonum-source! source2)))

(define (flonum-compare cc r1 r2)
  (set-current-branches!
   (lambda (label)
     (LAP (BC1T (@PCR ,label)) (NOP)))
   (lambda (label)
     (LAP (BC1F (@PCR ,label)) (NOP))))
  (if (eq? cc 'C.GT.D)
      (LAP (C.LT.D ,r2 ,r1) (NOP))
      (LAP (,cc ,r1 ,r2) (NOP))))