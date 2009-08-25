#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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
;; Package: (compiler lap-syntaxer)

(declare (usual-integrations))

(define fpr:zero (float-register->fpr regnum:fp-zero))

(define (flonum-source! register)
  (float-register->fpr (load-alias-register! register 'FLOAT)))

(define (flonum-target! pseudo-register)
  (delete-dead-registers!)
  (float-register->fpr (allocate-alias-register! pseudo-register 'FLOAT)))

(define (flonum-temporary!)
  (float-register->fpr (allocate-temporary-register! 'FLOAT)))

(define-integrable flonum-size
  (quotient float-width scheme-object-width))

(define-rule statement
  ;; convert a floating-point number to a flonum object
  (ASSIGN (REGISTER (? target))
	  (FLOAT->OBJECT (REGISTER (? source))))
  (let* ((source (flonum-source! source))
	 (target (standard-target! target)))
    (LAP
     ,@(with-values
	 (lambda ()
	   (immediate->register
	    (make-non-pointer-literal (ucode-type manifest-nm-vector)
				      flonum-size)))
	 (lambda (prefix alias)
	   (LAP ,@prefix
		(STQ ,alias (OFFSET 0 ,regnum:free)))))
     ,@(deposit-type-address (ucode-type flonum) regnum:free target)
     (STT ,source (OFFSET ,address-units-per-object ,regnum:free))
     (ADDQ ,regnum:free (& ,(* address-units-per-object (+ 1 flonum-size)))
	   ,regnum:free))))

(define-rule statement
  ;; convert a flonum object to a floating-point number
  (ASSIGN (REGISTER (? target)) (OBJECT->FLOAT (REGISTER (? source))))
  (let* ((source (standard-source! source))
	 (temp (standard-temporary!))
	 (target (flonum-target! target)))
    (LAP ,@(object->address source temp)
	 (LDT ,target (OFFSET ,address-units-per-object ,temp)))))

;; Floating-point vector support

(define-rule statement
  ;; Load an unboxed  floating pointer number given a register and offset
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset))))
  (let* ((base (standard-source! base))
	 (target (fpr->float-register (flonum-target! target))))
    (LAP (LDT ,target (OFFSET ,(* address-units-per-float offset)
			      ,base)))))

(define-rule statement
  ;; Store an unboxed floating point number
  (ASSIGN (FLOAT-OFFSET (REGISTER (? base))
			(MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((base (standard-source! base))
	(source (fpr->float-register (flonum-source! source))))
    (LAP (STT ,source (OFFSET ,(* address-units-per-float offset) ,base)))))

#| ********** Code from the MIPS back-end

This isn't needed (we assume) on the Alpha because the front-end
(rtlgen/opncod) notices that on the Alpha a floating point number and
the vector length header are the same size.

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
************************ MIPS |#

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

(define-arithmetic-method 'FLONUM-ABS flonum-methods/1-arg
  (lambda (target source)
    (LAP (CPYS ,fpr:zero ,source ,target))))

(define-arithmetic-method 'FLONUM-NEGATE flonum-methods/1-arg
  (lambda (target source)
    ; The following line is suggested by the Alpha instruction manual
    ; but it looks like it might generate a negative 0.0
    ; (LAP (CPYSN ,source ,source ,target))
    (LAP (SUBT ,fpr:zero ,source ,target))))

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
	       (LAP (,(caddr form) ,',SOURCE1 ,',SOURCE2 ,',TARGET))))))))
  (define-flonum-operation flonum-add ADDT)
  (define-flonum-operation flonum-subtract SUBT)
  (define-flonum-operation flonum-multiply MULT)
  (define-flonum-operation flonum-divide DIVT))

;;;; Flonum Predicates

(define-rule predicate
  (FLONUM-PRED-1-ARG (? predicate) (REGISTER (? source)))
  ;; No immediate zeros, easy to generate by subtracting from itself
  (let ((source (flonum-source! source)))
    (flonum-compare source
     (case predicate
       ((FLONUM-ZERO?) '(FBEQ FBNE))
       ((FLONUM-NEGATIVE?) '(FBLT FBGE))
       ((FLONUM-POSITIVE?) '(FBGT FBLE))
       (else (error "unknown flonum predicate" predicate))))
    (LAP)))

(define-rule predicate
  (FLONUM-PRED-2-ARGS (? predicate)
		      (REGISTER (? source1))
		      (REGISTER (? source2)))
  (let* ((source1 (flonum-source! source1))
	 (source2 (flonum-source! source2))
	 (temp (flonum-temporary!)))
    (flonum-compare temp '(FBNE FBEQ))
    (case predicate
      ((FLONUM-EQUAL?) (LAP (CMPTEQ ,source1 ,source2 ,temp)))
      ((FLONUM-LESS?) (LAP (CMPTLT ,source1 ,source2 ,temp)))
      ((FLONUM-GREATER?) (LAP (CMPTLT ,source2 ,source1 ,temp)))
      (else (error "unknown flonum predicate" predicate)))))

(define (flonum-compare source opcodes)
  (set-current-branches!
   (lambda (label)
     (LAP (,(car opcodes) ,source (@PCR ,label))))
   (lambda (label)
     (LAP (,(cadr opcodes) ,source (@PCR ,label))))))
