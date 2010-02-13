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
  (standard-move-to-target! source target))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((datum (standard-source! datum 'SCHEME_OBJECT*))
	 (type (standard-source! type 'ULONG))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:make-pointer-object type datum)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((datum (standard-source! datum 'SCHEME_OBJECT*))
	 (type (standard-source! type 'ULONG))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:make-object type datum)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (let* ((datum (standard-source! source 'SCHEME_OBJECT*))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:make-pointer-object type datum)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (let* ((datum (standard-source! source 'ULONG))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:make-object type datum)))))

(define (standard-unary-conversion source source-type target target-type
				   conversion)
  (let* ((source (standard-source! source source-type))
	 (target (standard-target! target target-type)))
    (conversion source target)))

(define (standard-binary-conversion source1 source1-type source2 source2-type
				    target target-type conversion)
  (let* ((source1 (standard-source! source1 source1-type))
	 (source2 (standard-source! source2 source2-type))
	 (target (standard-target! target target-type)))
    (conversion source1 source2 target)))

(define (object->type source target)
  (LAP ,(c:= target (c:object-type source))))

(define (object->datum source target)
  (LAP ,(c:= target (c:object-datum source))))

(define (object->address source target)
  (LAP ,(c:= target (c:object-address source))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'ULONG
			     object->type))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'ULONG
			     object->datum))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'SCHEME_OBJECT*
			     object->address))


;; long the right type here???
(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? base))
			  (REGISTER (? index))))
  (standard-binary-conversion
   base 'SCHEME_OBJECT*
   index 'LONG
   target 'SCHEME_OBJECT*
   (lambda (base index target)
     (LAP ,(c:= target (c:aptr base index))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   source 'SCHEME_OBJECT* target 'SCHEME_OBJECT*
   (lambda (source target)
     (LAP ,(c:= target (c:aptr source offset))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (REGISTER (? index))))
  (standard-binary-conversion
   base 'CHAR*
   index 'LONG
   target 'CHAR*
   (lambda (base index target)
     (LAP ,(c:= target (c:aptr base index))))))

;; This rule is not written in the obvious way (commented out) because
;; it is used by the code generator to bump closures.  Sometimes the
;; target is the value register (type scheme object) and the obvious
;; code would imply an implicit cast from pointer to integer, which
;; some compilers (e.g. -std1 on alpha) do not like.

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? source))
			       (MACHINE-CONSTANT (? offset))))
  #|
  (standard-unary-conversion
   source 'CHAR* target 'CHAR*
   (lambda (source target)
     (LAP ,(c:= target (c:aptr source offset)))))
  |#
  (standard-unary-conversion
   source 'LONG target 'ULONG
   (lambda (source target)
     (LAP ,(c:= target (c:cast 'ulong (c:+ source offset)))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? index))))
  (standard-binary-conversion
   base 'DOUBLE*
   index 'LONG
   target 'DOUBLE*
   (lambda (base index target)
     (LAP ,(c:= target (c:aptr base index))))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   source 'DOUBLE* target 'DOUBLE*
   (lambda (source target)
     (LAP ,(c:= target (c:aptr source offset))))))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:cast 'sobj source)))))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:cref (object->offset source))))))

(define-rule statement
  ;; load the type part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (CONSTANT (? constant))))
  (let ((target (standard-target! target 'ULONG)))
    (LAP ,(c:= target (c:object-type (c:cref (object->offset constant)))))))

(define-rule statement
  ;; load the datum part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (non-pointer-object? constant))
  (let ((target (standard-target! target 'ULONG)))
    (LAP ,(c:= target (c:object-datum (c:cref (object->offset constant)))))))

(define-rule statement
  ;; load a synthesized constant
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			    (MACHINE-CONSTANT (? datum))))
  (let((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:make-object type datum)))))

(define-rule statement
  ;; load the address of a variable reference cache
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= target
	       (c:cast 'sobj* (c:cref (free-reference->offset name)))))))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= target
	       (c:cast 'sobj* (c:cref (free-assignment->offset name)))))))

(define-rule statement
  ;; load the address of a procedure's entry point
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= target (c:cptr (label->offset label))))))

(define-rule statement
  ;; load the address of a continuation
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,(c:= target (c:cptr (label->offset label))))))

(define-rule statement
  ;; load a procedure object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target
	       (c:make-pointer-object type (c:cptr (label->offset label)))))))

(define-rule statement
  ;; load a return address object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target
	       (c:make-pointer-object type (c:cptr (label->offset label)))))))

;;;; Transfers from memory

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   address 'SCHEME_OBJECT* target 'SCHEME_OBJECT
   (lambda (address target)
     (LAP ,(c:= target (c:aref address offset))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? rsp)) 1))
  (QUALIFIER (= rsp regnum:stack-pointer))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,(c:= target (c:pop)))))

;;;; Transfers to memory

(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (QUALIFIER (word-register? source))
  (let* ((source (standard-source! source 'SCHEME_OBJECT))
	 (address (standard-source! address 'SCHEME_OBJECT*)))
    (LAP ,(c:= (c:aref address offset) source))))

(define-rule statement
  ;; Push an object register on the heap
  (ASSIGN (POST-INCREMENT (REGISTER (? rfree)) 1)
	  (REGISTER (? source)))
  (QUALIFIER (and (word-register? source)
		  (= rfree regnum:free)))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (LAP ,(c:= (c:* (c:postinc (c:free-reg))) source))))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? rsp)) -1)
	  (REGISTER (? source)))
  (QUALIFIER (and (word-register? source)
		  (= rsp regnum:stack-pointer)))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (LAP ,(c:push source))))

;; Cheaper, common patterns.

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (MACHINE-CONSTANT 0))
  (let ((address (standard-source! address 'SCHEME_OBJECT*)))
    (LAP ,(c:= (c:aref address offset) (c:cast 'sobj 0)))))

(define-rule statement
  ; Push NIL (or whatever is represented by a machine 0) on heap
  (ASSIGN (POST-INCREMENT (REGISTER (? rfree)) 1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= rfree regnum:free))
    (LAP ,(c:= (c:* (c:postinc (c:free-reg))) (c:cast 'sobj 0))))

(define-rule statement
  ;; Push 0 on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? rsp)) -1)
	  (MACHINE-CONSTANT (? const)))
  (QUALIFIER (= rsp regnum:stack-pointer))
  (LAP ,(c:push (c:cast 'sobj const))))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address))
			       (MACHINE-CONSTANT (? offset)))))
  (standard-unary-conversion
   address 'SCHEME_OBJECT* target 'ULONG
   (lambda (address target)
     (LAP ,(c:= target (c:ecall "CHAR_TO_ASCII" (c:aref address offset)))))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion address 'CHAR* target 'ULONG
    (lambda (address target)
      (LAP ,(c:= target
		 (c:cast 'ulong (c:aref (c:cast 'uchar* address) offset)))))))

(define-rule statement
  ;; convert char object to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'ULONG
    (lambda (source target)
      (LAP ,(c:= target (c:ecall "CHAR_TO_ASCII" source))))))

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (let ((address (standard-source! address 'CHAR*)))
    (LAP ,(c:= (c:aref address offset) "'\\0'"))))

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((address (standard-source! address 'CHAR*))
	(source (standard-source! source 'ULONG)))
    (LAP ,(c:= (c:aref address offset) (c:cast 'char source)))))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((address (standard-source! address 'CHAR*))
	(source (standard-source! source 'SCHEME_OBJECT)))
    (LAP ,(c:= (c:aref address offset)
	       (c:cast 'char (c:ecall "CHAR_TO_ASCII" source))))))
