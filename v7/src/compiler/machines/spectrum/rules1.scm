#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/spectrum/rules1.scm,v 4.33 1990/07/22 18:55:17 jinx Rel $
$MC68020-Header: rules1.scm,v 4.33 90/05/03 15:17:28 GMT jinx Exp $

Copyright (c) 1989, 1990 Massachusetts Institute of Technology

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
  ;; *** Why doesn't it work when qualifier is used? ***
  ;; (QUALIFIER (fits-in-5-bits-signed? type))
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
  ;; add a constant offset (in long words) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (standard-unary-conversion source target
    (lambda (source target)
      (load-offset (* 4 offset) source target))))

(define-rule statement
  ;; add a constant offset (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (standard-unary-conversion source target
    (lambda (source target)
      (load-offset offset source target))))

(define-rule statement
  ;; read an object from memory
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (standard-unary-conversion address target
    (lambda (address target)
      (load-word (* 4 offset) address target))))

(define-rule statement
  ;; pop an object off the stack
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 22) 1))
  (LAP (LDWM () (OFFSET 4 0 22) ,(standard-target! target))))

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
		    (standard-target! target)))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (free-assignment-label name)
		    (standard-target! target)))

(define-rule statement
  ;; load the address of a procedure's entry point
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (load-pc-relative-address label (standard-target! target)))

(define-rule statement
  ;; load the address of a continuation
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (load-pc-relative-address label (standard-target! target)))

;;; Spectrum optimizations

(define (load-entry label target)
  (let ((target (standard-target! target)))
    (LAP ,@(load-pc-relative-address label target)
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
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (store-word (standard-source! source)
	      (* 4 offset)
	      (standard-source! address)))

(define-rule statement
  ;; Push an object register on the heap
  (ASSIGN (POST-INCREMENT (REGISTER 21) 1) (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (STWM () ,(standard-source! source) (OFFSET 4 0 21))))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER 22) -1) (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (STWM () ,(standard-source! source) (OFFSET -4 0 22))))

;; Cheaper, common patterns.

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset)) (MACHINE-CONSTANT 0))
  (store-word 0
	      (* 4 offset)
	      (standard-source! address)))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 21) 1) (MACHINE-CONSTANT 0))
  (LAP (STWM () 0 (OFFSET 4 0 21))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 22) -1) (MACHINE-CONSTANT 0))
  (LAP (STWM () 0 (OFFSET -4 0 22))))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (standard-unary-conversion address target
    (lambda (address target)
      (load-byte (+ 3 (* 4 offset)) address target))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address)) (? offset)))
  (standard-unary-conversion address target
    (lambda (address target)
      (load-byte offset address target))))

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
  (ASSIGN (BYTE-OFFSET (REGISTER (? source)) (? offset))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (store-byte 0 offset (standard-source! source)))

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (REGISTER (? source)))
  (store-byte (standard-source! source) offset (standard-source! address)))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (REGISTER (? source))))
  (store-byte (standard-source! source) offset (standard-source! address)))