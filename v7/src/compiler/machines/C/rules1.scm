#| -*-Scheme-*-

$Id: rules1.scm,v 1.9 2002/11/20 19:45:50 cph Exp $

Copyright (c) 1992-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
    (LAP ,target " = (MAKE_POINTER_OBJECT (" ,type ", " ,datum "));\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (REGISTER (? type)) (REGISTER (? datum))))
  (let* ((datum (standard-source! datum 'SCHEME_OBJECT*))
	 (type (standard-source! type 'ULONG))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_OBJECT (" ,type ", " ,datum "));\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (let* ((datum (standard-source! source 'SCHEME_OBJECT*))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_POINTER_OBJECT (" ,type ", " ,datum "));\n\t")))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type)) (REGISTER (? source))))
  (let* ((datum (standard-source! source 'ULONG))
	 (target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_OBJECT (" ,type ", " ,datum "));\n\t")))

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
  (LAP ,target " = (OBJECT_TYPE (" ,source "));\n\t"))

(define (object->datum source target)
  (LAP ,target " = (OBJECT_DATUM (" ,source "));\n\t"))

(define (object->address source target)
  (LAP ,target " = (OBJECT_ADDRESS (" ,source "));\n\t"))

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
     (LAP ,target " = &" ,base "[" ,index "];\n\t"))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET-ADDRESS (REGISTER (? source))
			  (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   source 'SCHEME_OBJECT* target 'SCHEME_OBJECT*
   (lambda (source target)
     (LAP ,target " = &" ,source "[" ,offset "];\n\t"))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET-ADDRESS (REGISTER (? base))
			       (REGISTER (? index))))
  (standard-binary-conversion
   base 'CHAR*
   index 'LONG
   target 'CHAR*
   (lambda (base index target)
     (LAP ,target " = &" ,base "[" ,index "];\n\t"))))

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
     (LAP ,target " = &" ,source "[" ,offset "];\n\t")))
  |#
  (standard-unary-conversion
   source 'LONG target 'ULONG
   (lambda (source target)
     (LAP ,target " = ((unsigned long) (" ,source " + " ,offset "));\n\t"))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? base))
				(REGISTER (? index))))
  (standard-binary-conversion
   base 'DOUBLE*
   index 'LONG
   target 'DOUBLE*
   (lambda (base index target)
     (LAP ,target " = &" ,base "[" ,index "];\n\t"))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (FLOAT-OFFSET-ADDRESS (REGISTER (? source))
				(MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   source 'DOUBLE* target 'DOUBLE*
   (lambda (source target)
     (LAP ,target " = &" ,source "[" ,offset "];\n\t"))))

;;;; Loading of Constants

(define-rule statement
  ;; load a machine constant
  (ASSIGN (REGISTER (? target)) (MACHINE-CONSTANT (? source)))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = ((SCHEME_OBJECT) " ,source ");\n\t")))

(define-rule statement
  ;; load a Scheme constant
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = current_block[" ,(object->offset source) "];\n\t")))

(define-rule statement
  ;; load the type part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (CONSTANT (? constant))))
  (let ((target (standard-target! target 'ULONG)))
    (LAP ,target " = (OBJECT_TYPE (current_block["
	 ,(object->offset constant) "]));\n\t")))

(define-rule statement
  ;; load the datum part of a Scheme constant
  (ASSIGN (REGISTER (? target)) (OBJECT->DATUM (CONSTANT (? constant))))
  (QUALIFIER (non-pointer-object? constant))
  (let ((target (standard-target! target 'ULONG)))
    (LAP ,target " = (OBJECT_DATUM (current_block["
	 ,(object->offset constant) "]));\n\t")))

(define-rule statement
  ;; load a synthesized constant
  (ASSIGN (REGISTER (? target))
	  (CONS-NON-POINTER (MACHINE-CONSTANT (? type))
			    (MACHINE-CONSTANT (? datum))))
  (let((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_OBJECT (" ,type ", " ,datum "));\n\t")))

(define-rule statement
  ;; load the address of a variable reference cache
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,target " = ((SCHEME_OBJECT *) current_block["
	 ,(free-reference->offset name) "]);\n\t")))

(define-rule statement
  ;; load the address of an assignment cache
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,target " = ((SCHEME_OBJECT *) current_block["
	 ,(free-assignment->offset name) "]);\n\t")))

(define-rule statement
  ;; load the address of a procedure's entry point
  (ASSIGN (REGISTER (? target)) (ENTRY:PROCEDURE (? label)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,target " = &current_block[" ,(label->offset label) "];\n\t")))

(define-rule statement
  ;; load the address of a continuation
  (ASSIGN (REGISTER (? target)) (ENTRY:CONTINUATION (? label)))
  (let ((target (standard-target! target 'SCHEME_OBJECT*)))
    (LAP ,target " = &current_block[" ,(label->offset label) "];\n\t")))

(define-rule statement
  ;; load a procedure object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:PROCEDURE (? label))))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_POINTER_OBJECT (" ,type ", &current_block["
	 ,(label->offset label) "]));\n\t")))

(define-rule statement
  ;; load a return address object
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (MACHINE-CONSTANT (? type))
			(ENTRY:CONTINUATION (? label))))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = (MAKE_POINTER_OBJECT (" ,type ", &current_block["
	 ,(label->offset label) "]));\n\t")))

;;;; Transfers from memory

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion
   address 'SCHEME_OBJECT* target 'SCHEME_OBJECT
   (lambda (address target)
     (LAP ,target " = " ,address "[" ,offset "];\n\t"))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER (? rsp)) 1))
  (QUALIFIER (= rsp regnum:stack-pointer))
  (let ((target (standard-target! target 'SCHEME_OBJECT)))
    (LAP ,target " = *Rsp++;\n\t")))

;;;; Transfers to memory

(define-rule statement
  ;; store an object in memory
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (QUALIFIER (word-register? source))
  (let* ((source (standard-source! source 'SCHEME_OBJECT))
	 (address (standard-source! address 'SCHEME_OBJECT*)))
    (LAP ,address "[" ,offset "] = " ,source ";\n\t")))

(define-rule statement
  ;; Push an object register on the heap
  (ASSIGN (POST-INCREMENT (REGISTER (? rfree)) 1)
	  (REGISTER (? source)))
  (QUALIFIER (and (word-register? source)
		  (= rfree regnum:free)))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (LAP "*Rhp++ = " ,source ";\n\t")))

(define-rule statement
  ;; Push an object register on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? rsp)) -1)
	  (REGISTER (? source)))
  (QUALIFIER (and (word-register? source)
		  (= rsp regnum:stack-pointer)))
  (let ((source (standard-source! source 'SCHEME_OBJECT)))
    (LAP "*--Rsp = " ,source ";\n\t")))

;; Cheaper, common patterns.

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? address)) (MACHINE-CONSTANT (? offset)))
	  (MACHINE-CONSTANT 0))
  (let ((address (standard-source! address 'SCHEME_OBJECT*)))
    (LAP ,address "[" ,offset "] = ((SCHEME_OBJECT) 0);\n\t")))

(define-rule statement
  ; Push NIL (or whatever is represented by a machine 0) on heap
  (ASSIGN (POST-INCREMENT (REGISTER (? rfree)) 1) (MACHINE-CONSTANT 0))
  (QUALIFIER (= rfree regnum:free))
  (LAP "*Rhp++ = ((SCHEME_OBJECT) 0);\n\t"))

(define-rule statement
  ;; Push 0 on the stack
  (ASSIGN (PRE-INCREMENT (REGISTER (? rsp)) -1)
	  (MACHINE-CONSTANT (? const)))
  (QUALIFIER (= rsp regnum:stack-pointer))
  (LAP "*--Rsp = ((SCHEME_OBJECT) " ,const ");\n\t"))

;;;; CHAR->ASCII/BYTE-OFFSET

(define-rule statement
  ;; load char object from memory and convert to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (OFFSET (REGISTER (? address))
			       (MACHINE-CONSTANT (? offset)))))
  (standard-unary-conversion
   address 'SCHEME_OBJECT* target 'ULONG
   (lambda (address target)
     (LAP ,target " = (CHAR_TO_ASCII (" ,address "[" ,offset "]));\n\t"))))

(define-rule statement
  ;; load ASCII byte from memory
  (ASSIGN (REGISTER (? target))
	  (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset))))
  (standard-unary-conversion address 'CHAR* target 'ULONG
    (lambda (address target)
      (LAP ,target " = ((unsigned long) (((unsigned char *) " ,address ")["
	   ,offset "]));\n\t"))))

(define-rule statement
  ;; convert char object to ASCII byte
  (ASSIGN (REGISTER (? target))
	  (CHAR->ASCII (REGISTER (? source))))
  (standard-unary-conversion source 'SCHEME_OBJECT target 'ULONG
    (lambda (source target)
      (LAP ,target " = (CHAR_TO_ASCII (" ,source "));\n\t"))))

(define-rule statement
  ;; store null byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (CONSTANT #\NUL)))
  (let ((address (standard-source! address 'CHAR*)))
    (LAP ,address "[" ,offset "] = '\\0';\n\t")))

(define-rule statement
  ;; store ASCII byte in memory
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (REGISTER (? source)))
  (let ((address (standard-source! address 'CHAR*))
	(source (standard-source! source 'ULONG)))
    (LAP ,address "[" ,offset "] = ((char) " ,source ");\n\t")))

(define-rule statement
  ;; convert char object to ASCII byte and store it in memory
  ;; register + byte offset <- contents of register (clear top bits)
  (ASSIGN (BYTE-OFFSET (REGISTER (? address))
		       (MACHINE-CONSTANT (? offset)))
	  (CHAR->ASCII (REGISTER (? source))))
  (let ((address (standard-source! address 'CHAR*))
	(source (standard-source! source 'SCHEME_OBJECT)))
    (LAP ,address "[" ,offset "] = ((char) (CHAR_TO_ASCII (" ,source
	 ")));\n\t")))
