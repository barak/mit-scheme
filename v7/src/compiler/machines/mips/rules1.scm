#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/mips/rules1.scm,v 1.4 1991/07/25 02:46:10 cph Exp $
$MC68020-Header: rules1.scm,v 4.33 90/05/03 15:17:28 GMT jinx Exp $

Copyright (c) 1989-91 Massachusetts Institute of Technology

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
  (let* ((type (standard-move-to-temporary! type))
	 (target (standard-move-to-target! datum target)))
    (LAP (SLL ,type ,type ,(- 32 scheme-type-width))
	 (AND ,target ,target ,regnum:address-mask)
	 (OR ,target ,type ,target))))

(define-rule statement
  ;; tag the contents of a register
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (let ((target (standard-move-to-target! source target)))
    (deposit-type type target)))

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
  (let ((target (standard-move-to-target! source target)))
    (object->address target)))

(define-rule statement
  ;; add a distance (in longwords) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate (* 4 offset) source target))))

(define-rule statement
  ;; add a distance (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate offset source target))))

(define-rule statement
  ;; read an object from memory
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LW ,target (OFFSET ,(* 4 offset) ,address))
	   (NOP)))))

(define-rule statement
  ;; pop an object off the stack
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 3) 1))
  (LAP (LW ,(standard-target! target) (OFFSET 0 ,regnum:stack-pointer))
       (ADDI ,regnum:stack-pointer ,regnum:stack-pointer 4)))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (load-immediate source (standard-target! target)))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant source (standard-target! target) #T))

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
  (load-pc-relative (standard-target! target)
		    'CONSTANT (free-reference-label name)
		    true))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (standard-target! target)
		    'CONSTANT (free-assignment-label name)
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
	 (AND ,target ,temporary ,regnum:address-mask)
	 ,@(put-type type target))))

;;;; Transfers to Memory
		    
(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
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

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (? offset))
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

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address)) (? offset))))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LBU ,target
		(OFFSET ,(let ((offset (* 4 offset)))
			   (if (eq? endianness 'LITTLE)
			       offset
			       (+ offset 3)))
			,address))
	   (NOP)))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address)) (? offset)))
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

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? source)) (? offset))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (LAP (SB 0 (OFFSET ,offset ,(standard-source! source)))))

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (REGISTER (? source)))
  (LAP (SB ,(standard-source! source)
	   (OFFSET ,offset ,(standard-source! address)))))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address)) (? offset))
	  (CHAR->ASCII (REGISTER (? source))))
  (LAP (SB ,(standard-source! source)
	   (OFFSET ,offset ,(standard-source! address)))))