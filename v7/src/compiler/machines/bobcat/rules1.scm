#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules1.scm,v 4.1 1987/12/30 07:05:45 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Transfers to Registers

(define-rule statement
  (ASSIGN (REGISTER 15) (REGISTER (? source)))
  (LAP (MOV L ,(coerce->any source) (A 7))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER (? source)) (? offset)))
  (QUALIFIER (pseudo-register? source))
  (LAP (LEA ,(indirect-reference! source offset) (A 7))))

(define-rule statement
  (ASSIGN (REGISTER 15) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (increment-anl 7 n))

(define-rule statement
  (ASSIGN (REGISTER 12) (REGISTER 15))
  (LAP (MOV L (A 7) (A 4))))

(define-rule statement
  (ASSIGN (REGISTER 12) (OFFSET-ADDRESS (REGISTER 15) (? offset)))
  (LAP (LEA (@AO 7 ,(* 4 offset)) (A 4))))

;;; The following rule always occurs immediately after an instruction
;;; of the form
;;;
;;; (ASSIGN (REGISTER (? source)) (POST-INCREMENT (REGISTER 15) 1))
;;;
;;; in which case it could be implemented very efficiently using the
;;; sequence
;;;
;;; (LAP (CLR (@A 7)) (MOV L (@A+ 7) (A 4)))
;;;
;;; but unfortunately we have no mechanism to take advantage of this.

(define-rule statement
  (ASSIGN (REGISTER 12) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? source))
  (if (and (dead-register? source)
	   (register-has-alias? source 'DATA))
      (let ((source (register-reference (register-alias source 'DATA))))
	(LAP (AND L ,mask-reference ,source)
	     (MOV L ,source (A 4))))
      (let ((temp (reference-temporary-register! 'DATA)))
	(LAP (MOV L ,(coerce->any source) ,temp)
	     (AND L ,mask-reference ,temp)
	     (MOV L ,temp (A 4))))))

;;; All assignments to pseudo registers are required to delete the
;;; dead registers BEFORE performing the assignment.  This is because
;;; the register being assigned may be PSEUDO-REGISTER=? to one of the
;;; dead registers, and thus would be flushed if the deletions
;;; happened after the assignment.

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET-ADDRESS (REGISTER 15) (? n)))
  (QUALIFIER (pseudo-register? target))
  (LAP
   (LEA (@AO 7 ,(* 4 n))
	,(reference-assignment-alias! target 'ADDRESS))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (CONSTANT (? source)))
  (QUALIFIER (pseudo-register? target))
  (LAP ,(load-constant source (coerce->any target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (VARIABLE-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L
	    (@PCR ,(free-reference-label name))
	    ,(reference-assignment-alias! target 'DATA))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (ASSIGNMENT-CACHE (? name)))
  (QUALIFIER (pseudo-register? target))
  (LAP (MOV L
	    (@PCR ,(free-assignment-label name))
	    ,(reference-assignment-alias! target 'DATA))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (REGISTER (? source)))
  (QUALIFIER (pseudo-register? target))
  (move-to-alias-register! source 'DATA target)
  (LAP))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->ADDRESS (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (AND L ,mask-reference ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OBJECT->TYPE (REGISTER (? source))))
  (QUALIFIER (pseudo-register? target))
  (let ((target (move-to-alias-register! source 'DATA target)))
    (LAP (RO L L (& 8) ,target))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (OFFSET (REGISTER (? address)) (? offset)))
  (QUALIFIER (pseudo-register? target))
  (let ((source (indirect-reference! address offset)))
    (delete-dead-registers!)
    ;; The fact that the target register here is a data register is a
    ;; heuristic that works reasonably well since if the value is a
    ;; pointer, we will probably want to dereference it, which
    ;; requires that we first mask it.
    (LAP (MOV L
	      ,source
	      ,(register-reference
		(allocate-alias-register! target 'DATA))))))

(define-rule statement
  (ASSIGN (REGISTER (? target)) (POST-INCREMENT (REGISTER 15) 1))
  (QUALIFIER (pseudo-register? target))
  (delete-dead-registers!)
  (LAP (MOV L
	    (@A+ 7)
	    ,(register-reference
	      (allocate-alias-register! target 'DATA)))))

(define-rule statement
  (ASSIGN (REGISTER (? target))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? datum))))
  (QUALIFIER (pseudo-register? target))
  (let ((datum (coerce->any datum)))
    (delete-dead-registers!)
    (let ((target* (coerce->any target)))
      (if (register-effective-address? target*)
	  (LAP (MOV L ,datum ,reg:temp)
	       (MOV B (& ,type) ,reg:temp)
	       (MOV L ,reg:temp ,target*))
	  (LAP (MOV L ,datum ,target*)
	       (MOV B (& ,type) ,target*))))))

;;;; Transfers to Memory

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONSTANT (? object)))
  (LAP ,(load-constant object (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (REGISTER (? r)))
  (LAP (MOV L
	    ,(coerce->any r)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (POST-INCREMENT (REGISTER 15) 1))
  (LAP (MOV L
	    (@A+ 7)
	    ,(indirect-reference! a n))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a)) (? n))
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (let ((target (indirect-reference! a n)))
    (LAP (MOV L ,(coerce->any r) ,target)
	 (MOV B (& ,type) ,target))))

(define-rule statement
  (ASSIGN (OFFSET (REGISTER (? a0)) (? n0))
	  (OFFSET (REGISTER (? a1)) (? n1)))
  (let ((source (indirect-reference! a1 n1)))
    (LAP (MOV L
	      ,source
	      ,(indirect-reference! a0 n0)))))

;;;; Consing

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (INST-EA (@A+ 5)))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (REGISTER (? r)))
  (LAP (MOV L ,(coerce->any r) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@A+ 5))))

(define-rule statement
  (ASSIGN (POST-INCREMENT (REGISTER 13) 1) (ENTRY:PROCEDURE (? label)))
  (let ((temporary
	 (register-reference (allocate-temporary-register! 'ADDRESS))))
    (LAP (LEA (@PCR ,(rtl-procedure/external-label (label->object label)))
	      ,temporary)
	 (MOV L ,temporary (@A+ 5))
	 (MOV B (& ,(ucode-type compiled-expression)) (@AO 5 -4)))))

;;;; Pushes

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (CONSTANT (? object)))
  (LAP ,(load-constant object (INST-EA (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (UNASSIGNED))
  (LAP ,(load-non-pointer (ucode-type unassigned) 0 (INST-EA (@-A 7)))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (REGISTER (? r)))
  (LAP (MOV L ,(coerce->any r) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1)
	  (CONS-POINTER (CONSTANT (? type)) (REGISTER (? r))))
  (LAP (MOV L ,(coerce->any r) (@-A 7))
       (MOV B (& ,type) (@A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (OFFSET (REGISTER (? r)) (? n)))
  (LAP (MOV L ,(indirect-reference! r n) (@-A 7))))

(define-rule statement
  (ASSIGN (PRE-INCREMENT (REGISTER 15) -1) (ENTRY:CONTINUATION (? label)))
  (LAP (PEA (@PCR ,label))
       (MOV B (& ,(ucode-type compiler-return-address)) (@A 7))))