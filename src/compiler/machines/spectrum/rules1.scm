#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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
  ;; tag the contents of a register
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((type (standard-source! type))
	 (target (standard-move-to-target! datum target)))
    (LAP (DEP () ,type ,(-1+ scheme-type-width) ,scheme-type-width ,target))))

(define-rule statement
  ;; tag the contents of a register
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  ;; (QUALIFIER (fits-in-5-bits-signed? type))
  ;; This qualifier does not work because the qualifiers are not
  ;; tested in the rtl compressor.  The qualifier is combined with
  ;; the rule body into a single procedure, and the rtl compressor
  ;; cannot invoke it since it is not in the context of the lap
  ;; generator.  Thus the qualifier is not checked, the RTL instruction
  ;; is compressed, and then the lap generator fails when the qualifier
  ;; fails.
  (deposit-type type (standard-move-to-target! source target)))

(define-rule statement
  ;; extract the type part of a register's contents
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (standard-unary-conversion source target object->type))

(define-rule statement
  ;; extract the datum part of a register's contents
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (standard-unary-conversion source target object->datum))

(define-rule statement
  ;; convert the contents of a register to an address
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (object->address (standard-move-to-target! source target)))

(define-rule statement
  ;; pop an object off the stack
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? reg)) 1))
  (QUALIFIER (= reg regnum:stack-pointer))
  (LAP
   (LDWM () (OFFSET 4 0 ,regnum:stack-pointer) ,(standard-target! target))))

;;;; Indexed modes

(define-rule statement
  ;; read an object from memory
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-word (* 4 offset) base target))))

(define-rule statement
  ;; read an object from memory
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? base)) (REGISTER (? offset))))
  (let ((base (standard-source! base))
	(offset (standard-source! offset)))
    (let ((target (standard-target! target)))
      (LAP (LDWX (S) (INDEX ,offset 0 ,base) ,target)))))

;;;; Address manipulation

(define-rule statement
  ;; add a constant offset (in long words) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-offset (* 4 offset) base target))))

(define-rule statement
  ;; add a constant offset (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-offset offset base target))))

(define-rule statement
  ;; add a constant offset (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-offset (* 8 offset) base target))))

(define-rule statement
  ;; add a computed offset (in long words) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (REGISTER (? offset))))
  (indexed-load-address target base offset 4))

(define-rule statement
  ;; add a computed offset (in long words) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (REGISTER (? offset))))
  (indexed-load-address target base offset 1))

(define-rule statement
  ;; add a computed offset (in long words) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? offset))))
  (indexed-load-address target base offset 8))

;;; Optimized address operations

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
			  (OBJECT->DATUM (REGISTER (? index)))))
  (indexed-object->address target base index 4))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
			       (OBJECT->DATUM (REGISTER (? index)))))
  (indexed-object->address target base index 1))

;; These have to be here because the instruction combiner
;; operates by combining one piece at a time, and the intermediate
;; pieces can be generated.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
			  (REGISTER (? index))))
  (indexed-object->address target base index 4))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (OBJECT->ADDRESS (REGISTER (? base)))
			       (REGISTER (? index))))
  (indexed-object->address target base index 1))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (OBJECT->DATUM (REGISTER (? index)))))
  (indexed-object->datum target base index 4))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (OBJECT->DATUM (REGISTER (? index)))))
  (indexed-object->datum target base index 1))

(define (indexed-load-address target base index scale)
  (let ((base (standard-source! base))
	(index (standard-source! index)))
    (%indexed-load-address (standard-target! target) base index scale)))

(define (indexed-object->datum target base index scale)
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!)))
    (let ((target (standard-target! target)))
      (LAP ,@(object->datum index temp)
	   ,@(%indexed-load-address target base temp scale)))))

(define (indexed-object->address target base index scale)
  (let ((base (standard-source! base))
	(index (standard-source! index)))
    (let ((target (standard-target! target)))
      (LAP ,@(%indexed-load-address target base index scale)
	   ,@(object->address target)))))

(define (%indexed-load-address target base index scale)
  (case scale
    ((4)
     (LAP (SH2ADDL () ,index ,base ,target)))
    ((8)
     (LAP (SH3ADDL () ,index ,base ,target)))
    ((1)
     (LAP (ADDL () ,index ,base ,target)))
    ((2)
     (LAP (SH1ADDL () ,index ,base ,target)))
    (else
     (error "%indexed-load-address: Unknown scale"))))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (load-immediate source (standard-target! target)))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant source (standard-target! target)))

(define-rule statement
  ;; load the type part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (CONSTANT (? constant))))
  (load-non-pointer 0 (object-type constant) (standard-target! target)))

(define-rule statement
  ;; load the datum part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (non-pointer-object? constant))
  (load-non-pointer 0
		    (careful-object-datum constant)
		    (standard-target! target)))

(define-rule statement
  ;; load a synthesized constant
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(MACHINE-CONSTANT (? datum))))
  (load-non-pointer type datum (standard-target! target)))

(define-rule statement
  ;; load the address of a variable reference cache
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (load-pc-relative (free-reference-label name) 
		    (standard-target! target)
		    'CONSTANT))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (free-assignment-label name)
		    (standard-target! target)
		    'CONSTANT))

(define-rule statement
  ;; load the address of a procedure's entry point
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address label (standard-target! target) 'CODE))

(define-rule statement
  ;; load the address of a continuation
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address label (standard-target! target) 'CODE))

;;; Spectrum optimizations

(define (load-entry label target)
  (let ((target (standard-target! target)))
    (LAP ,@(load-pc-relative-address label target 'CODE)
	 ,@(address->entry target))))

(define-rule statement
  ;; load a procedure object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (QUALIFIER (= type (ucode-type compiled-entry)))
  (load-entry label target))

(define-rule statement
  ;; load a return address object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (QUALIFIER (= type (ucode-type compiled-entry)))
  (load-entry label target))

;;;; Transfers to Memory

(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (store-word (standard-source! source)
	      (* 4 offset)
	      (standard-source! base)))

(define-rule statement
  ;; Push an object register on the heap
  ;; *** IMPORTANT: This uses a STWS instruction with the cache hint set.
  ;; The cache hint prevents newer HP PA processors from loading a cache
  ;; line from memory when it is about to be overwritten.
  ;; In theory this could cause a problem at the very end (64 bytes) of the
  ;; heap, since the last cache line may overlap the next area (the stack).
  ;; ***
  (ASSIGN (POST-INCREMENT (REGISTER (? reg)) 1) (? source register-expression))
  (QUALIFIER (and (= reg regnum:free-pointer)
		  (word-register? source)))
  (LAP
   (STWS (MA C) ,(standard-source! source) (OFFSET 4 0 ,regnum:free-pointer))))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? reg)) -1) (? source register-expression))
  (QUALIFIER (and (word-register? source)
		  (= reg regnum:stack-pointer)))
  (LAP
   (STWM () ,(standard-source! source) (OFFSET -4 0 ,regnum:stack-pointer))))

;; Cheaper, common patterns.

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (MACHINE-CONSTANT 0))
  (store-word 0
	      (* 4 offset)
	      (standard-source! base)))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER (? reg)) 1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= reg regnum:free-pointer))
  (LAP (STWS (MA C) 0 (OFFSET 4 0 ,regnum:free-pointer))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER (? reg)) -1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= reg regnum:stack-pointer))
  (LAP (STWM () 0 (OFFSET -4 0 ,regnum:stack-pointer))))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? base))
			       (MACHINE-CONSTANT (? offset)))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-byte (+ 3 (* 4 offset)) base target))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? base))
		       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion base target
    (lambda (base target)
      (load-byte offset base target))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? base))
		       (REGISTER (? offset))))
  (let ((base (standard-source! base))
	(offset (standard-source! offset)))
    (let ((target (standard-target! target)))
      (LAP (LDBX () (INDEX ,offset 0 ,base) ,target)))))

(define-rule statement
  ;; convert char object to ASCII byte
  ;; Missing optimization: If source is home and this is the last
  ;; reference (it is dead afterwards), an LDB could be done instead
  ;; of an LDW followed by an object->datum.  This is unlikely since
  ;; the value will be home only if we've spilled it, which happens
  ;; rarely.
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (standard-unary-conversion source target
    (lambda (source target)
      (LAP (EXTRU () ,source 31 8 ,target)))))

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (store-byte 0 offset (standard-source! base)))

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (store-byte (standard-source! source) offset (standard-source! base)))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? base)) (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (REGISTER (? source))))
  (store-byte (standard-source! source) offset (standard-source! base)))