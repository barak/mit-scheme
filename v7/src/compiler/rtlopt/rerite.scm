#| -*-Scheme-*-

$Id: rerite.scm,v 1.5 2002/11/20 19:45:57 cph Exp $

Copyright (c) 1990-1999 Massachusetts Institute of Technology

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

;;;; RTL Rewriting
;;; package: (compiler rtl-optimizer rtl-rewriting)

(declare (usual-integrations))

(define-structure (rewriting-rules
		   (conc-name rewriting-rules/)
		   (constructor make-rewriting-rules ()))
  (assignment '())
  (statement '())
  (register '())
  (expression '())
  (generic '()))

(define rules:pre-cse (make-rewriting-rules))
(define rules:post-cse (make-rewriting-rules))

(define (rtl-rewriting:pre-cse rgraphs)
  (walk-rgraphs rules:pre-cse rgraphs))

(define (rtl-rewriting:post-cse rgraphs)
  (walk-rgraphs rules:post-cse rgraphs))

(define (add-rewriting-rule! pattern result-procedure)
  (new-rewriting-rule! rules:post-cse pattern result-procedure))

(define (add-pre-cse-rewriting-rule! pattern result-procedure)
  (new-rewriting-rule! rules:pre-cse pattern result-procedure))

(define (walk-rgraphs rules rgraphs)
  (if (not (and (null? (rewriting-rules/assignment rules))
		(null? (rewriting-rules/statement rules))
		(null? (rewriting-rules/register rules))
		(null? (rewriting-rules/expression rules))
		(null? (rewriting-rules/generic rules))))
      (for-each (lambda (rgraph)
		  (walk-rgraph rules rgraph))
		rgraphs)))

(define (walk-rgraph rules rgraph)
  (fluid-let ((*current-rgraph* rgraph))
    (for-each (lambda (bblock) (walk-bblock rules bblock))
	      (rgraph-bblocks rgraph))))

(define (walk-bblock rules bblock)
  (bblock-walk-forward bblock
    (lambda (rinst)
      (walk-rinst rules rinst))))

(define (walk-rinst rules rinst)
  (let ((rtl (rinst-rtl rinst)))
    ;; Typically there will be few rules, and few instructions that
    ;; match, so it is worth checking before rewriting anything.
    (if (or (match-rtl-statement rules rtl)
	    (rtl:any-subexpression? rtl
	      (letrec ((loop
			(lambda (expression)
			  (or (match-rtl-expression rules expression)
			      (rtl:any-subexpression? expression loop)))))
		loop)))
	(set-rinst-rtl!
	 rinst
	 (let loop
	     ((rtl
	       (rtl:map-subexpressions rtl
		 (letrec ((loop
			   (lambda (expression)
			     (let ((match-result
				    (match-rtl-expression rules expression)))
			       (if match-result
				   (loop (match-result))
				   expression)))))
		   loop))))
	   (let ((match-result (match-rtl-statement rules rtl)))
	     (if match-result
		 (loop (match-result))
		 rtl)))))))

(define (match-rtl-statement rules rtl)
  (or (if (rtl:assign? rtl)
	  (pattern-lookup (rewriting-rules/assignment rules) rtl)
	  (let ((entries
		 (assq (rtl:expression-type rtl)
		       (rewriting-rules/statement rules))))
	    (and entries
		 (pattern-lookup (cdr entries) rtl))))
      (pattern-lookup (rewriting-rules/generic rules) rtl)))

(define (match-rtl-expression rules expression)
  (or (if (rtl:register? expression)
	  (pattern-lookup (rewriting-rules/register rules) expression)
	  (let ((entries
		 (assq (rtl:expression-type expression)
		       (rewriting-rules/expression rules))))
	    (and entries
		 (pattern-lookup (cdr entries) expression))))
      (pattern-lookup (rewriting-rules/generic rules) expression)))

(define (new-rewriting-rule! rules pattern result-procedure)
  (let ((entry (cons pattern result-procedure)))
    (if (not (and (pair? pattern) (symbol? (car pattern))))
	(set-rewriting-rules/generic! rules
				      (cons entry
					    (rewriting-rules/generic rules)))
	(let ((keyword (car pattern)))
	  (cond ((eq? keyword 'ASSIGN)
		 (set-rewriting-rules/assignment!
		  rules
		  (cons entry (rewriting-rules/assignment rules))))
		((eq? keyword 'REGISTER)
		 (set-rewriting-rules/register!
		  rules
		  (cons entry (rewriting-rules/register rules))))
		((memq keyword rtl:expression-types)
		 (let ((entries
			(assq keyword (rewriting-rules/expression rules))))
		   (if entries
		       (set-cdr! entries (cons entry (cdr entries)))
		       (set-rewriting-rules/expression!
			rules
			(cons (list keyword entry)
			      (rewriting-rules/expression rules))))))
		((or (memq keyword rtl:statement-types)
		     (memq keyword rtl:predicate-types))
		 (let ((entries
			(assq keyword (rewriting-rules/statement rules))))
		   (if entries
		       (set-cdr! entries (cons entry (cdr entries)))
		       (set-rewriting-rules/statement!
			rules
			(cons (list keyword entry)
			      (rewriting-rules/statement rules))))))
		(else
		 (error "illegal RTL type" keyword))))))
  pattern)

(define-rule add-pre-cse-rewriting-rule!
  (OBJECT->ADDRESS (? source))
  (QUALIFIER (value-class=address? (rtl:expression-value-class source)))
  source)

;; KLUDGE!  This is unsafe, but currently works.
;; Probably closure bumping should not use byte-offset-address, and use
;; a new rtl type, but...

(define-rule add-pre-cse-rewriting-rule!
  (CONS-POINTER (MACHINE-CONSTANT (? type))
		(REGISTER (? datum register-known-value)))
  (QUALIFIER
   (and (= (ucode-type compiled-entry) type)
	(rtl:byte-offset-address? datum)
	(let ((v (let ((v (rtl:byte-offset-address-base datum)))
		   (if (rtl:register? v)
		       (register-known-value (rtl:register-number v))
		       v))))
	  (and v
	       (rtl:object->address? v)))))
  (rtl:make-byte-offset-address
   (rtl:object->address-expression
    (let ((v (rtl:byte-offset-address-base datum)))
      (if (rtl:register? v)
	  (register-known-value (rtl:register-number v))
	  v)))
   (rtl:byte-offset-address-offset datum)))