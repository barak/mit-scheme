#| -*-Scheme-*-

$Id: 1a0833eb86f078fc51f432e9d102a9cd3113356d $

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

;;;; LAP Generation Rules: Data Transfers
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Simple Operations

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  However, it is
;;; necessary to derive the effective address of the source
;;; expression(s) before deleting the dead registers.  Otherwise any
;;; source expression containing dead registers might refer to aliases
;;; which have been reused.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (standard-move-to-target! source target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((type (standard-move-to-temporary! type))
	 (target (standard-move-to-target! datum target)))
    (LAP (SLL ,type ,type ,(- 32 scheme-type-width))
	 (AND ,target ,target ,regnum:address-mask)
	 (OR ,target ,type ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((type (standard-move-to-temporary! type))
	 (target (standard-move-to-target! datum target)))
    (LAP (SLL ,type ,type ,(- 32 scheme-type-width))
	 (OR ,target ,type ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (standard-unary-conversion source target
    (lambda (source target)
      (deposit-type-address type source target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (standard-unary-conversion source target
    (lambda (source target)
      (deposit-type-datum type source target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (standard-unary-conversion source target object->type))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (standard-unary-conversion source target object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source target object->address))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (REGISTER (? index))))
  (shifted-add target base index 2))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate (* 4 offset) source target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (REGISTER (? index))))
  (shifted-add target base index 0))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate offset source target))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? index))))
  (shifted-add target base index 3))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate (* 8 offset) source target))))

(define (shifted-add target base index shift)
  (if (zero? shift)
      (standard-binary-conversion base index target
       (lambda (base index target)
	 (LAP (ADDU ,target ,base ,index))))
      (let ((base (standard-source! base))
	    (index (standard-source! index))
	    (temp (standard-temporary!)))
	(let ((target (standard-target! target)))
	  (LAP (SLL ,temp ,index ,shift)
	       (ADDU ,target ,base ,temp))))))

(define (with-indexed-address base index shift recvr)
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!)))
    (if (zero? shift)
	(LAP (ADDU ,temp ,base ,index)
	     ,@(recvr temp))
	(LAP (SLL ,temp ,index ,shift)
	     (ADDU ,temp ,base ,temp)
	     ,@(recvr temp)))))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (load-immediate (standard-target! target) source #T))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant (standard-target! target) source #T #T))

(define-rule statement
  ;; load the type part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (CONSTANT (? constant))))
  (load-immediate (standard-target! target)
		  (make-non-pointer-literal 0 (object-type constant))
		  #T))

(define-rule statement
  ;; load the datum part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (non-pointer-object? constant))
  (load-immediate (standard-target! target)
		  (make-non-pointer-literal 0 (careful-object-datum constant))
		  #T))

(define-rule statement
  ;; load a synthesized constant
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			    (MACHINE-CONSTANT (? datum))))
  (load-immediate (standard-target! target)
		  (make-non-pointer-literal type datum)
		  #T))

(define-rule statement
  ;; load the address of a variable reference cache
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative (standard-target! target)
		    'CONSTANT
		    (free-reference-label name)
		    true))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (standard-target! target)
		    'CONSTANT
		    (free-assignment-label name)
		    true))

(define-rule statement
  ;; load the address of a procedure's entry point
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address (standard-target! target) 'CODE label))

(define-rule statement
  ;; load the address of a continuation
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address (standard-target! target) 'CODE label))

(define-rule statement
  ;; load a procedure object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (load-entry target type label))

(define-rule statement
  ;; load a return address object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (load-entry target type label))

(define (load-entry target type label)
  (let ((temporary (standard-temporary!))
	(target (standard-target! target)))
    ;; Loading the address into a temporary makes it more useful,
    ;; because it can be reused later.
    (LAP ,@(load-pc-relative-address temporary 'CODE label)
	 ,@(deposit-type-address type temporary target))))

;;;; Transfers from memory

#|
(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? base)) (REGISTER (? index))))
  (with-indexed-address base index 2
   (lambda (address)
     (let ((target (standard-target! target)))
       (LAP (LW ,target (OFFSET 0 ,address))
	    (NOP))))))
|#

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LW ,target (OFFSET ,(* 4 offset) ,address))
	   (NOP)))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 3) 1))
  (LAP (LW ,(standard-target! target) (OFFSET 0 ,regnum:stack-pointer))
       (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)))

;;;; Transfers to memory

#|
(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (with-indexed-address base index 2
    (lambda (address)
      (LAP (SW ,(standard-source! source) (OFFSET 0 ,address))))))
|#

(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (SW ,(standard-source! source)
	   (OFFSET ,(* 4 offset) ,(standard-source! address)))))

(define-rule statement
  ;; Push an object register on the heap
  (ASSIGN (POST-INCREMENT (REGISTER 9) 1)
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (SW ,(standard-source! source) (OFFSET 0 ,regnum:free))
       (ADDI ,regnum:free ,regnum:free 4)))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER 3) -1)
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (ADDI ,regnum:stack-pointer ,regnum:stack-pointer -4)
       (SW ,(standard-source! source)
	   (OFFSET 0 ,regnum:stack-pointer))))

;; Cheaper, common patterns.

#|
(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (MACHINE-CONSTANT 0))
  (with-indexed-address base index 2
    (lambda (address)
      (LAP (SW 0 (OFFSET 0 ,address))))))
|#

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (MACHINE-CONSTANT 0))
  (LAP (SW 0 (OFFSET ,(* 4 offset) ,(standard-source! address)))))

(define-rule statement
  ; Push NIL (or whatever is represented by a machine 0) on heap
  (ASSIGN (POST-INCREMENT (REGISTER 9) 1) (MACHINE-CONSTANT 0))
  (LAP (SW 0 (OFFSET 0 ,regnum:free))
       (ADDI ,regnum:free ,regnum:free 4)))

(define-rule statement
  ; Ditto, but on stack
  (ASSIGN (PRE-INCREMENT (REGISTER 3) -1) (MACHINE-CONSTANT 0))
  (LAP (ADDI ,regnum:stack-pointer ,regnum:stack-pointer -4)
       (SW 0 (OFFSET 0 ,regnum:stack-pointer))))

;;;; CHAR->ASCII/BYTE-OFFSET

#|
(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? base))
			       (REGISTER (? index)))))
  (with-indexed-address base index 2
    (lambda (address)
      (let ((target (standard-target! target)))
	(LAP (LBU ,target
		  (OFFSET ,(if (eq? endianness 'LITTLE)
			       0
			       3)
			  ,address))
	     (NOP))))))
|#

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address))
			       (MACHINE-CONSTANT (? offset)))))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LBU ,target
		(OFFSET ,(let ((offset (* 4 offset)))
			   (if (eq? endianness 'LITTLE)
			       offset
			       (+ offset 3)))
			,address))
	   (NOP)))))

#|
(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? base))
		       (REGISTER (? index))))
  (with-indexed-address base index 0
    (lambda (address)
      (let ((target (standard-target! target)))
	(LAP (LBU ,target (OFFSET 0 ,address))
	     (NOP))))))
|#

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LBU ,target (OFFSET ,offset ,address))
	   (NOP)))))

(define-rule statement
  ;; convert char object to ASCII byte
  ;; Missing optimization: If source is home and this is the last
  ;; reference (it is dead afterwards), an LB could be done instead of
  ;; an LW followed by an ANDI.  This is unlikely since the value will
  ;; be home only if we've spilled it, which happens rarely.
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (standard-unary-conversion source target
    (lambda (source target)
      (LAP (ANDI ,target ,source #xFF)))))

#|
(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? base))
		       (REGISTER (? index)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (with-indexed-address base index 0
    (lambda (address)
      (LAP (SB 0 (OFFSET 0 ,address))))))
|#

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? source))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (LAP (SB 0 (OFFSET ,offset ,(standard-source! source)))))

#|
(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? base))
		       (REGISTER (? index)))
	  (REGISTER (? source)))
  (with-indexed-address base index 0
    (lambda (address)
      (LAP (SB ,(standard-source! source) (OFFSET 0 ,address))))))
|#

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (LAP (SB ,(standard-source! source)
	   (OFFSET ,offset ,(standard-source! address)))))

#|
(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (REGISTER (? index)))
	  (CHAR->ASCII (REGISTER (? source))))
  (with-indexed-address base index 0
    (lambda (address)
      (LAP (SB ,(standard-source! source) (OFFSET 0 ,address))))))
|#

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (REGISTER (? source))))
  (LAP (SB ,(standard-source! source)
	   (OFFSET ,offset ,(standard-source! address)))))