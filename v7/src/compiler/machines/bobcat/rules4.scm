#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/bobcat/rules4.scm,v 1.3 1987/07/16 10:12:01 jinx Exp $

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

;;;; LAP Generation Rules: Interpreter Calls

(declare (usual-integrations))

;;;; Interpreter Calls

(define-rule statement
  (INTERPRETER-CALL:ACCESS (? environment) (? name))
  (lookup-call entry:compiler-access environment name))

(define-rule statement
  (INTERPRETER-CALL:LOOKUP (? environment) (? name) (? safe?))
  (lookup-call (if safe? entry:compiler-safe-lookup entry:compiler-lookup)
	       environment name))

(define-rule statement
  (INTERPRETER-CALL:UNASSIGNED? (? environment) (? name))
  (lookup-call entry:compiler-unassigned? environment name))

(define-rule statement
  (INTERPRETER-CALL:UNBOUND? (? environment) (? name))
  (lookup-call entry:compiler-unbound? environment name))

(define (lookup-call entry environment name)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((clear-map (clear-map!)))
      (LAP ,@set-environment
	   ,@clear-map
	   ,(load-constant name (INST-EA (A 1)))
	   (JSR ,entry)
	   ,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:ENCLOSE (? number-pushed))
  (decrement-frame-pointer-offset!
   number-pushed
   (LAP (MOV L (A 5) ,reg:enclose-result)
	(MOV B (& ,(ucode-type vector)) ,reg:enclose-result)
	,(load-non-pointer (ucode-type manifest-vector) number-pushed
			   (INST-EA (@A+ 5)))
     
	,@(generate-n-times
	   number-pushed 5
	   (lambda () (INST (MOV L (@A+ 7) (@A+ 5))))
	   (lambda (generator)
	     (generator (allocate-temporary-register! 'DATA)))))
   #| Alternate sequence which minimizes code size. ;
   DO NOT USE THIS!  The `clear-registers!' call does not distinguish between
   registers containing objects and registers containing unboxed things, and
   as a result can write unboxed stuff to memory.
   (LAP ,@(clear-registers! a0 a1 d0)
	(MOV W (& ,number-pushed) (D 0))
	(JSR ,entry:compiler-enclose))
   |#
   ))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-define environment name value))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (assignment-call:default entry:compiler-set! environment name value))

(define (assignment-call:default entry environment name value)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((set-value (expression->machine-register! value a2)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-environment
	     ,@set-value
	     ,@clear-map
	     ,(load-constant name (INST-EA (A 1)))
	     (JSR ,entry)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:DEFINE (? environment) (? name)
			   (CONS-POINTER (CONSTANT (? type))
					 (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-define environment name type
				datum))

(define-rule statement
  (INTERPRETER-CALL:SET! (? environment) (? name)
			 (CONS-POINTER (CONSTANT (? type))
				       (REGISTER (? datum))))
  (assignment-call:cons-pointer entry:compiler-set! environment name type
				datum))

(define (assignment-call:cons-pointer entry environment name type datum)
  (let ((set-environment (expression->machine-register! environment a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-environment
	     (MOV L ,datum ,reg:temp)
	     (MOV B (& ,type) ,reg:temp)
	     ,@clear-map
	     (MOV L ,reg:temp (A 2))
	     ,(load-constant name (INST-EA (A 1)))
	     (JSR ,entry)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-REFERENCE (? extension) (? safe?))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((clear-map (clear-map!)))
      (LAP ,@set-extension
	   ,@clear-map
	   (JSR ,(if safe?
		     entry:compiler-safe-reference-trap
		     entry:compiler-reference-trap))
	   ,@(make-external-label (generate-label))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension) (? value))
  (QUALIFIER (not (eq? 'CONS-POINTER (car value))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((set-value (expression->machine-register! value a1)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-extension
	     ,@set-value
	     ,@clear-map
	     (JSR ,entry:compiler-assignment-trap)
	     ,@(make-external-label (generate-label)))))))

(define-rule statement
  (INTERPRETER-CALL:CACHE-ASSIGNMENT (? extension)
				     (CONS-POINTER (CONSTANT (? type))
						   (REGISTER (? datum))))
  (let ((set-extension (expression->machine-register! extension a0)))
    (let ((datum (coerce->any datum)))
      (let ((clear-map (clear-map!)))
	(LAP ,@set-extension
	     (MOV L ,datum ,reg:temp)
	     (MOV B (& ,type) ,reg:temp)
	     ,@clear-map
	     (MOV L ,reg:temp (A 1))
	     (JSR ,entry:compiler-assignment-trap)
	     ,@(make-external-label (generate-label)))))))

;;;; Poppers

(define-rule statement
  (MESSAGE-RECEIVER:CLOSURE (? frame-size))
  (record-push!
   (LAP (MOV L (& ,(* frame-size 4)) (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:STACK (? frame-size))
  (record-push!
   (LAP (MOV L
	     (& ,(+ #x00100000 (* frame-size 4)))
	     (@-A 7)))))

(define-rule statement
  (MESSAGE-RECEIVER:SUBPROBLEM (? label))
  (record-continuation-frame-pointer-offset! label)
  (increment-frame-pointer-offset!
   2
   (LAP (PEA (@PCR ,label))
	(MOV B (& ,type-code:return-address) (@A 7))
	(MOV L (& #x00200000) (@-A 7)))))

(define (apply-closure-sequence frame-size receiver-offset label)
  (LAP ,(load-dnw frame-size 1)
       (LEA (@AO 7 ,(* (+ receiver-offset (frame-pointer-offset)) 4))
	    (A 0))
       (LEA (@PCR ,label) (A 1))
       (JMP ,popper:apply-closure)))

(define (apply-stack-sequence frame-size receiver-offset n-levels label)
  (LAP (MOVEQ (& ,n-levels) (D 0))
       ,(load-dnw frame-size 1)
       (LEA (@AO 7 ,(* (+ receiver-offset (frame-pointer-offset)) 4))
	    (A 0))
       (LEA (@PCR ,label) (A 1))
       (JMP ,popper:apply-stack)))

(define-rule statement
  (MESSAGE-SENDER:VALUE (? receiver-offset))
  (disable-frame-pointer-offset!
   (LAP ,@(clear-map!)
	,@(increment-anl 7 (+ receiver-offset (frame-pointer-offset)))
	(JMP ,popper:value))))