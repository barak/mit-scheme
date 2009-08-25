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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; RTL rule utilities for SVM
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Register-allocator interface

(define (sort-machine-registers registers)
  registers)

(define (register-type register)
  (cond ((register-value-class=word? register) 'WORD)
	((register-value-class=float? register) 'FLOAT)
	(else (error:bad-range-argument register 'REGISTER-TYPE))))

(define-syntax define-fixed-register-references
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(* symbol) (cdr form))
	 `(BEGIN
	    ,@(map (lambda (name)
		     `(DEFINE-INTEGRABLE ,(symbol-append 'RREF: name)
			(REGISTER-REFERENCE ,(symbol-append 'REGNUM: name))))
		   (cdr form)))
	 (ill-formed-syntax form)))))

(define-fixed-register-references
  stack-pointer
  dynamic-link
  free-pointer
  value
  environment)

(define (pseudo-register-home register)
  (error "Attempt to access temporary register:" register))

(define (register->register-transfer source target)
  (if (= source target)
      (LAP)
      (begin
	(guarantee-registers-compatible source target)
	(inst:copy (register-reference target)
		   (register-reference source)))))

(define (reference->register-transfer source target)
  (cond ((register-reference? source)
	 (register->register-transfer (reference->register source) target))
	((memory-reference? source)
	 (inst:load 'WORD (register-reference target) source))
	(else
	 (error:bad-range-argument source #f))))

(define (home->register-transfer source target)
  (inst:load 'WORD (register-reference target) (pseudo-register-home source)))

(define (register->home-transfer source target)
  (inst:store 'WORD (register-reference target) (pseudo-register-home target)))

;;;; Linearizer interface

(define lap:make-label-statement
  inst:label)

(define (lap:make-unconditional-branch label)
  (inst:jump (ea:address label)))

(define (lap:make-entry-point label block-start-label)
  block-start-label
  (LAP ,@(inst:entry-point label)
       ,@(make-expression-label label)))

(define (make-expression-label label)
  (make-external-label label 'EXPRESSION))

(define (make-external-label label type-code)
  (set! *external-labels* (cons label *external-labels*))
  (LAP ,@(inst:datum-u16 type-code)
       ,@(inst:datum-u16 `(- ,label *START*))
       ,@(inst:label label)))

(define (make-expression-label label)
  (make-external-label label #xFFFF))

(define (make-internal-entry-label label)
  (make-external-label label #xFFFE))

(define (make-internal-continuation-label label)
  (make-external-label label #xFFFD))

(define (make-procedure-label n-required n-optional rest? label)
  (make-external-label label
		       (encode-procedure-type n-required n-optional rest?)))

(define (make-internal-procedure-label label)
  (make-external-label label (encode-continuation-offset label #xFFFE)))

(define (make-continuation-label entry-label label)
  (make-external-label label (encode-continuation-offset label #xFFFD)))

(define (encode-procedure-type n-required n-optional rest?)
  (guarantee-exact-nonnegative-integer n-required)
  (guarantee-exact-nonnegative-integer n-optional)
  (if (not (and (< n-required #x80) (< n-optional #x80)))
      (error "Can't encode procedure arity:" n-required n-optional))
  (fix:or n-required
	  (fix:or (fix:lsh n-optional 7)
		  (if rest? #x4000 0))))

(define (encode-continuation-offset label default)
  (let ((offset
	 (rtl-procedure/next-continuation-offset (label->object label))))
    (if offset
	(begin
	  (guarantee-exact-nonnegative-integer offset)
	  (if (not (< offset #x7FF8))
	      (error "Can't encode next-continuation offset:" offset))
	  (+ offset #x8000))
	default)))

;;;; Utilities for the rules

(define (load-constant target object)
  (cond ((object-pointer? object)
	 (inst:load 'WORD
		    target
		    (ea:address (constant->label object))))
	((object-non-pointer? object)
	 (inst:load-non-pointer target
				(object-type object)
				(object-datum object)))
	(else
	 (error:bad-range-argument object 'LOAD-CONSTANT))))

(define (simple-branches! condition source1 #!default source2)
  (if (default-object? source2)
      (set-current-branches!
       (lambda (label)
	 (inst:conditional-jump condition source1 (ea:address label)))
       (lambda (label)
	 (inst:conditional-jump (invert-condition condition)
				source1 (ea:address label))))
      (set-current-branches!
       (lambda (label)
	 (inst:conditional-jump condition source1 source2 (ea:address label)))
       (lambda (label)
	 (inst:conditional-jump (invert-condition condition)
				source1 source2 (ea:address label))))))

(define (invert-condition condition)
  (let loop
      ((conditions
	'((EQ NEQ)
	  (LT GE)
	  (GT LE)
	  (SLT SGE)
	  (SGT SLE)
	  (CMP NCMP)
	  (FIX NFIX)
	  (IFIX NIFIX))))
    (if (not (pair? conditions))
	(error:bad-range-argument condition 'INVERT-CONDITION))
    (cond ((eq? (caar conditions) condition) (cdar conditions))
	  ((eq? (cdar conditions) condition) (caar conditions))
	  (else (loop (cdr conditions))))))

(define (internal->external-label label)
  (rtl-procedure/external-label (label->object label)))

(define (word-source source)
  (register-reference (load-alias-register! source 'WORD)))

(define (word-target target)
  (delete-dead-registers!)
  (register-reference (or (register-alias target 'WORD)
			  (allocate-alias-register! target 'WORD))))

(define (word-temporary)
  (register-reference (allocate-temporary-register! 'WORD)))

(define (float-source source)
  (register-reference (load-alias-register! source 'FLOAT)))

(define (float-target target)
  (delete-dead-registers!)
  (register-reference (or (register-alias target 'FLOAT)
			  (allocate-alias-register! target 'FLOAT))))

(define (float-temporary)
  (register-reference (allocate-temporary-register! 'FLOAT)))

(define (parse-memory-ref expression)
  (pattern-lookup memory-ref-rules expression))

(define (parse-memory-address expression)
  (receive (scale ea) (pattern-lookup memory-address-rules expression)
    scale
    ea))

(define (make-memory-rules offset-operator?)
  (list (rule-matcher ((? scale offset-operator?)
		       (REGISTER (? base))
		       (REGISTER (? index)))
		      (values scale
			      (ea:indexed (word-source base)
					  0 scale
					  (word-source index) scale)))
	(rule-matcher ((? scale offset-operator?)
		       (REGISTER (? base))
		       (MACHINE-CONSTANT (? offset)))
		      (values scale
			      (ea:offset (word-source base) offset scale)))
	(rule-matcher ((? scale offset-operator?)
		       ((? scale* offset-address-operator?)
			(REGISTER (? base))
			(REGISTER (? index)))
		       (MACHINE-CONSTANT (? offset)))
		      (values scale
			      (ea:indexed (word-source base)
					  offset scale
					  (word-source index) scale*)))
	(rule-matcher ((? scale offset-operator?)
		       ((? scale* offset-address-operator?)
			(REGISTER (? base))
			(MACHINE-CONSTANT (? offset)))
		       (REGISTER (? index)))
		      (values scale
			      (ea:indexed (word-source base)
					  offset scale*
					  (word-source index) scale)))
	(rule-matcher (POST-INCREMENT (REGISTER (? base)) 1)
		      (values 'WORD
			      (ea:post-increment (word-source base) 'WORD)))
	(rule-matcher (PRE-INCREMENT (REGISTER (? base)) -1)
		      (values 'WORD
			      (ea:pre-decrement (word-source base) 'WORD)))))

(define memory-ref-rules
  (make-memory-rules
   (lambda (expression)
     (offset-operator? expression))))

(define memory-address-rules
  (make-memory-rules
   (lambda (expression)
     (offset-address-operator? expression))))

(define (offset-operator? expression)
  (case expression
    ((OFFSET) 'WORD)
    ((BYTE-OFFSET) 'BYTE)
    ((FLOAT-OFFSET) 'FLOAT)
    (else #f)))

(define (offset-address-operator? expression)
  (case expression
    ((OFFSET-ADDRESS) 'WORD)
    ((BYTE-OFFSET-ADDRESS) 'BYTE)
    ((FLOAT-OFFSET-ADDRESS) 'FLOAT)
    (else #f)))

(define (pre-lapgen-analysis rgraphs)
  (for-each (lambda (rgraph)
	      (for-each (lambda (edge)
			  (determine-interrupt-checks (edge-right-node edge)))
			(rgraph-entry-edges rgraph)))
	    rgraphs))