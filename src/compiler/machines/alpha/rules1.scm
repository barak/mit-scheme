#| -*-Scheme-*-

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
;; Package: (compiler lap-syntaxer)

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
  (rules1-make-object target type datum))

(define-rule statement
  ;; tag the contents of a register
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (rules1-make-object target type datum))

(define (rules1-make-object target type datum)
  (let* ((type (standard-source! type))
	 (datum (standard-source! datum))
	 (target (standard-target! target)))
    (LAP (SLL ,type (& ,scheme-datum-width) ,target)
	 (BIS ,datum ,target ,target))))

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
  (standard-unary-conversion source target object->address))

(define-rule statement
  ;; add a distance (in longwords) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (REGISTER (? offset))))
  (address-add target source offset address-units-per-object))

(define-rule statement
  ;; add a distance (in longwords) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate (* address-units-per-object offset)
		     source target))))

(define-rule statement
  ;; add a distance (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (REGISTER (? offset))))
  (address-add target source offset 1))

(define-rule statement
  ;; add a distance (in bytes) to a register's contents
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate offset source target))))

(define-rule statement
  ;; add a distance (in "size of floating point constants") to a
  ;; register's contents.
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? index))))
  (address-add target base index address-units-per-float))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion source target
    (lambda (source target)
      (add-immediate (* address-units-per-float offset)
		     source target))))

(define (address-add target base index size-in-address-units)
  (case size-in-address-units
    ((1) (standard-binary-conversion base index target
	  (lambda (base index target)
	    (LAP (ADDQ ,index ,base ,target)))))
    ((4) (standard-binary-conversion base index target
	  (lambda (base index target)
	    (LAP (S4ADDQ ,index ,base ,target)))))
    ((8) (standard-binary-conversion base index target
	  (lambda (base index target)
	    (LAP (S8ADDQ ,index ,base ,target)))))
    (else (error "address-add: size of object isn't 1, 4, or 8 bytes"
		 size-in-address-units))))

(define (with-indexed-address base index size-in-address-units recvr)
  (let ((base (standard-source! base))
	(index (standard-source! index))
	(temp (standard-temporary!)))
    (case size-in-address-units
      ((0) (LAP (ADDQ ,base ,index ,temp)
		,@(recvr temp)))
      ((4) (LAP (S4ADDQ ,index ,base ,temp)
		,@(recvr temp)))
      ((8) (LAP (S8ADDQ ,index ,base ,temp)
		,@(recvr temp)))
      (else
       (error
	"with-indexed-address: size of object isn't 1, 4,or 8 bytes"
	size-in-address-units)))))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (load-immediate (standard-target! target) source #T))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (load-constant (standard-target! target) source #T))

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
		    (free-reference-label name)))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (load-pc-relative (standard-target! target)
		    'CONSTANT
		    (free-assignment-label name)))

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

(define-rule statement
  ;; read an object from memory
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address))
					(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion address target
    (lambda (address target)
      (LAP (LDQ ,target
		(OFFSET ,(* address-units-per-object offset)
			,address))))))

;; Note: we don't seem to need
;;  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? base))
;;	  				  (REGISTER (? index))))

(define-rule statement
  ;; Pop stack to register
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? stack)) 1))
  (QUALIFIER (= stack regnum:stack-pointer))
  (LAP (LDQ ,(standard-target! target) (OFFSET 0 ,regnum:stack-pointer))
       (ADDQ ,regnum:stack-pointer (& ,address-units-per-object)
	     ,regnum:stack-pointer)))

;;;; Transfers to memory

(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (? source register-expression))
  (QUALIFIER (word-register? source))
  (LAP (STQ ,(standard-source! source)
	    (OFFSET ,(* address-units-per-object offset)
		    ,(standard-source! address)))))

;; Note: we don't seem to need
;; (ASSIGN (OFFSET (REGISTER (? base)) (REGISTER (? index)))
;;         (? source register-expression))

(define-rule statement
  ;; Push an object register on the heap
  (ASSIGN (POST-INCREMENT (REGISTER (? Free)) 1)
	  (? source register-expression))
  (QUALIFIER (and (= free regnum:free) (word-register? source)))
  (LAP (STQ ,(standard-source! source) (OFFSET 0 ,regnum:free))
       (ADDQ ,regnum:free (& ,address-units-per-object) ,regnum:free)))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? stack)) -1)
	  (? source register-expression))
  (QUALIFIER (and (= stack regnum:stack-pointer) (word-register? source)))
  (LAP (STQ ,(standard-source! source)
	    (OFFSET ,(- address-units-per-object) ,regnum:stack-pointer))
       (SUBQ ,regnum:stack-pointer (& ,address-units-per-object)
	     ,regnum:stack-pointer)))

;; Cheaper, common patterns.

;; We don't need 
;;  (ASSIGN (OFFSET (REGISTER (? base)) (REGISTER (? index)))
;;          (MACHINE-CONSTANT 0))
;; since it simplifies to (... (OFFSET REGISTER MACHINE-CONSTANT) ...)

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (MACHINE-CONSTANT 0))
  (LAP (STQ ,regnum:zero (OFFSET ,(* address-units-per-object offset)
				 ,(standard-source! address)))))

(define-rule statement
  ; Push NIL (or whatever is represented by a machine 0) on heap
  (ASSIGN (POST-INCREMENT (REGISTER (? free)) 1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= free regnum:free))
  (LAP (STQ ,regnum:zero (OFFSET 0 ,regnum:free))
       (ADDQ ,regnum:free (& ,address-units-per-object) ,regnum:free)))

(define-rule statement
  ; Ditto, but on stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? stack)) -1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= stack regnum:stack-pointer))
  (LAP (STQ ,regnum:zero
	    (OFFSET ,(- address-units-per-object) ,regnum:stack-pointer))
       (SUBQ ,regnum:stack-pointer (& ,address-units-per-object)
	     ,regnum:stack-pointer)))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  ;; convert char object to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (standard-unary-conversion source target
    (lambda (source target)
      (LAP (AND ,source (& #xFF) ,target)))))

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? source))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (modify-byte (standard-source! source) offset
    (lambda (data-register offset-register)
      data-register			; Ignored
      offset-register			; Ignored
      (LAP))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset))))
  (load-byte address offset target))

(define-rule statement
  ;; store ASCII byte in memory.  There may be a FIXNUM typecode.
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((source (standard-source! source))
	(address (standard-source! address)))
    (store-byte address offset source)))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((source (standard-source! source))
	(address (standard-source! address)))
    (store-byte address offset source)))

(define (modify-byte source offset update-byte)
  (let* ((temp (standard-temporary!))
	 (byte-offset (modulo offset address-units-per-object)))
    (if (and (zero? byte-offset) (fits-in-16-bits-signed? byte-offset))
	(LAP (LDQ_U ,temp (OFFSET ,offset ,source))
	     (MSKBL ,temp ,source ,temp) ; Zero byte to modify
	     ,@(update-byte temp source)
	     (STQ_U ,temp (OFFSET ,offset ,source)))
	(let ((address-temp (standard-temporary!)))
	  (LAP (LDA ,address-temp (OFFSET ,offset ,source))
	       (LDQ_U ,temp (OFFSET 0 ,address-temp))
	       (MSKBL ,temp ,address-temp ,temp) ; Zero byte to modify
	       ,@(update-byte temp address-temp)
	       (STQ_U ,temp (OFFSET 0 ,address-temp)))))))

(define (store-byte address offset source)
  (let ((temp (standard-temporary!)))
    (modify-byte address offset
      (lambda (data-register offset-register)
	;; data-register has the contents of memory with the desired
	;; byte set to zero; offset-register has the number of the
	;; machine register that holds the byte offset within word. 
	;; INSBL moves the byte to be stored into the correct position
	;; BIS   ORs the two together, completing the byte insert
	(LAP (INSBL ,source ,offset-register ,temp)
	     (BIS ,data-register ,temp ,data-register))))))

(define (load-byte address offset target)
  (let* ((source (standard-source! address))
	 (target (standard-target! target))
	 (byte-offset (modulo offset address-units-per-object)))
    (if (zero? byte-offset)
	(LAP (LDQ_U ,target (OFFSET ,offset ,source))
	     (EXTBL ,target ,source ,target))
	(let ((temp (standard-temporary!)))
	  (LAP (LDQ_U ,target (OFFSET ,offset ,source))
	       (LDA ,temp (OFFSET ,byte-offset ,source))
	       (EXTBL ,target ,temp ,target))))))
