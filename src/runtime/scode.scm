#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; SCode Abstraction
;;; package: (runtime scode)

(declare (usual-integrations))

;;;; Constant

(define (scode-expression? object)
  (or (vector-ref scode-type-vector (object-type object))
      (and (compiled-code-address? object)
	   (eq? 'compiled-expression (compiled-entry-type object)))))
(register-predicate! scode-expression? 'scode-expression)

(define-deferred scode-type-vector
  (let ((type-vector (make-vector (microcode-type/code-limit) #f)))
    (for-each (lambda (name)
		(vector-set! type-vector (microcode-type name) #t))
	      '(access assignment combination comment conditional constant
		       definition delay disjunction extended-lambda lambda
		       lexpr quotation sequence the-environment variable))
    type-vector))

(define (scode-constant? object)
  (not (scode-expression? object)))
(register-predicate! scode-constant? 'scode-constant)

;;;; Quotation

(define (make-scode-quotation expression)
  (system-pair-cons (ucode-type quotation)
		    (unmap-reference-trap expression)
		    '()))

(define (scode-quotation? object)
  (object-type? (ucode-type quotation) object))
(register-predicate! scode-quotation? 'scode-quotation)

(define (scode-quotation-expression quotation)
  (guarantee scode-quotation? quotation 'scode-quotation-expression)
  (map-reference-trap (lambda () (system-pair-car quotation))))

;;;; Variable

(define (make-scode-variable name)
  (guarantee symbol? name 'make-scode-variable)
  (system-hunk3-cons (ucode-type variable) name #t '()))

(define (scode-variable? object)
  (object-type? (ucode-type variable) object))
(register-predicate! scode-variable? 'scode-variable)

(define (scode-variable-name variable)
  (guarantee scode-variable? variable 'scode-variable-name)
  (system-hunk3-cxr0 variable))

;;;; Definition

(define (make-scode-definition name value)
  (guarantee symbol? name 'make-scode-definition)
  (system-pair-cons (ucode-type definition)
		    (unmap-reference-trap name)
		    (unmap-reference-trap value)))

(define (scode-definition? object)
  (object-type? (ucode-type definition) object))
(register-predicate! scode-definition? 'scode-definition)

(define (scode-definition-name definition)
  (guarantee scode-definition? definition 'scode-definition-name)
  (system-pair-car definition))

(define (scode-definition-value definition)
  (guarantee scode-definition? definition 'scode-definition-value)
  (map-reference-trap (lambda () (system-pair-cdr definition))))

;;;; Assignment

(define (make-scode-assignment name value)
  (guarantee symbol? name 'make-scode-assignment)
  (system-pair-cons (ucode-type assignment)
		    (make-scode-variable name)
		    (unmap-reference-trap value)))

(define (scode-assignment? object)
  (object-type? (ucode-type assignment) object))
(register-predicate! scode-assignment? 'scode-assignment)

(define (scode-assignment-name assignment)
  (guarantee scode-assignment? assignment 'scode-assignment-name)
  (scode-variable-name (system-pair-car assignment)))

(define (scode-assignment-value assignment)
  (guarantee scode-assignment? assignment 'scode-assignment-value)
  (map-reference-trap (lambda () (system-pair-cdr assignment))))

;;;; Comment

(define (make-scode-comment text expression)
  (system-pair-cons (ucode-type comment)
		    (unmap-reference-trap expression)
		    text))

(define (scode-comment? object)
  (object-type? (ucode-type comment) object))
(register-predicate! scode-comment? 'scode-comment)

(define-print-method scode-comment?
  (standard-print-method
      (lambda (comment)
	(cond ((scode-library? comment) 'scode-library)
	      ((scode-declaration? comment) 'scode-declaration)
	      (else 'scode-comment)))
    (lambda (comment)
      (if (scode-library? comment)
	  (list (scode-library-name comment))
	  '()))))

(define (scode-comment-text comment)
  (guarantee scode-comment? comment 'scode-comment-text)
  (system-pair-cdr comment))

(define (scode-comment-expression comment)
  (guarantee scode-comment? comment 'scode-comment-expression)
  (map-reference-trap (lambda () (system-pair-car comment))))

(define (set-scode-comment-expression! comment expression)
  (guarantee scode-comment? comment 'set-scode-comment-expression!)
  (system-pair-set-car! comment (unmap-reference-trap expression)))

;;;; Declaration

(define (make-scode-declaration text expression)
  (make-scode-comment (cons declaration-tag text) expression))

(define (scode-declaration? object)
  (and (scode-comment? object)
       (let ((text (scode-comment-text object)))
	 (and (pair? text)
	      (eq? (car text) declaration-tag)))))
(register-predicate! scode-declaration? 'scode-declaration '<= scode-comment?)

(define-integrable declaration-tag
  '|#[declaration]|)

(define (scode-declaration-text declaration)
  (guarantee scode-declaration? declaration 'scode-declaration-text)
  (cdr (scode-comment-text declaration)))

(define (scode-declaration-expression declaration)
  (guarantee scode-declaration? declaration 'scode-declaration-expression)
  (scode-comment-expression declaration))

;;;; The-Environment

(define (make-scode-the-environment)
  (object-new-type (ucode-type the-environment) 0))

(define (scode-the-environment? object)
  (object-type? (ucode-type the-environment) object))
(register-predicate! scode-the-environment? 'scode-the-environment)

;;;; Access

(define (make-scode-access environment name)
  (guarantee symbol? name 'make-scode-access)
  (system-pair-cons (ucode-type access)
		    (unmap-reference-trap environment)
		    name))

(define (scode-access? object)
  (object-type? (ucode-type access) object))
(register-predicate! scode-access? 'scode-access)

(define (scode-access-environment access)
  (guarantee scode-access? access 'scode-access-environment)
  (map-reference-trap (lambda () (system-pair-car access))))

(define (scode-access-name access)
  (guarantee scode-access? access 'scode-access-name)
  (system-pair-cdr access))

;;;; Absolute Reference

(define (make-scode-absolute-reference name)
  (make-scode-access system-global-environment name))

(define (scode-absolute-reference? object)
  (and (scode-access? object)
       (system-global-environment? (scode-access-environment object))))
(register-predicate! scode-absolute-reference? 'scode-absolute-reference
		     '<= scode-access?)

(define (scode-absolute-reference-name reference)
  (guarantee scode-absolute-reference? reference 'scode-absolute-reference-name)
  (scode-access-name reference))

(define (scode-absolute-reference-to? object name)
  (and (scode-absolute-reference? object)
       (eq? name (scode-absolute-reference-name object))))

;;;; Delay

(define (make-scode-delay expression)
  (system-pair-cons (ucode-type delay)
		    (unmap-reference-trap expression)
		    '()))

(define (scode-delay? object)
  (object-type? (ucode-type delay) object))
(register-predicate! scode-delay? 'scode-delay)

(define (scode-delay-expression delay)
  (guarantee scode-delay? delay 'scode-delay-expression)
  (map-reference-trap (lambda () (system-pair-car delay))))

;;;; Sequence

(define (make-scode-sequence actions)
  (guarantee non-empty-list? actions 'make-sequence)
  (let loop ((actions actions))
    (if (pair? (cdr actions))
	(system-pair-cons (ucode-type sequence)
			  (unmap-reference-trap (car actions))
			  (unmap-reference-trap (loop (cdr actions))))
	(car actions))))

(define (scode-sequence? object)
  (object-type? (ucode-type sequence) object))
(register-predicate! scode-sequence? 'scode-sequence)

(define (scode-sequence-actions expression)
  (if (scode-sequence? expression)
      (append-map scode-sequence-actions
		  (list (map-reference-trap
			 (lambda ()
			   (system-pair-car expression)))
			(map-reference-trap
			 (lambda ()
			   (system-pair-cdr expression)))))
      (list expression)))

;;;; Combination

(define (make-scode-combination operator operands)
  (guarantee list? operands 'make-scode-combination)
  (system-list->vector (ucode-type combination)
		       (cons (unmap-reference-trap operator)
			     (let loop ((operands operands))
			       (if (pair? operands)
				   (cons (unmap-reference-trap (car operands))
					 (loop (cdr operands)))
				   '())))))

(define (scode-combination? object)
  (object-type? (ucode-type combination) object))
(register-predicate! scode-combination? 'scode-combination)

(define (scode-combination-operator combination)
  (guarantee scode-combination? combination 'scode-combination-operator)
  (map-reference-trap (lambda () (system-vector-ref combination 0))))

(define (scode-combination-operands combination)
  (guarantee scode-combination? combination 'scode-combination-operands)
  (let loop
      ((operands
	(system-subvector->list combination
				1
				(system-vector-length combination))))
    (if (pair? operands)
	(cons (map-reference-trap (lambda () (car operands)))
	      (loop (cdr operands)))
	'())))

;;;; Unassigned?

(define (make-scode-unassigned? name)
  (make-scode-combination (ucode-primitive lexical-unassigned?)
			  (list (make-scode-the-environment) name)))

(define (scode-unassigned?? object)
  (and (scode-combination? object)
       (eq? (scode-combination-operator object)
	    (ucode-primitive lexical-unassigned?))
       (let ((operands (scode-combination-operands object)))
	 (and (= 2 (length operands))
	      (scode-the-environment? (car operands))
	      (symbol? (cadr operands))))))
(register-predicate! scode-unassigned?? 'scode-unassigned?
		     '<= scode-combination?)

(define (scode-unassigned?-name expression)
  (guarantee scode-unassigned?? expression 'scode-unassigned?-name)
  (cadr (scode-combination-operands expression)))

;;;; Conditional

(define (make-scode-conditional predicate consequent alternative)
  (object-new-type (ucode-type conditional)
		   (hunk3-cons (unmap-reference-trap predicate)
			       (unmap-reference-trap consequent)
			       (unmap-reference-trap alternative))))

(define (scode-conditional? object)
  (object-type? (ucode-type conditional) object))
(register-predicate! scode-conditional? 'scode-conditional)

(define undefined-scode-conditional-branch unspecific)

(define (scode-conditional-predicate conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-predicate)
  (map-reference-trap (lambda () (system-hunk3-cxr0 conditional))))

(define (scode-conditional-consequent conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-consequent)
  (map-reference-trap (lambda () (system-hunk3-cxr1 conditional))))

(define (scode-conditional-alternative conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-alternative)
  (map-reference-trap (lambda () (system-hunk3-cxr2 conditional))))

;;;; Disjunction

(define (make-scode-disjunction predicate alternative)
  (system-pair-cons (ucode-type disjunction)
		    (unmap-reference-trap predicate)
		    (unmap-reference-trap alternative)))

(define (scode-disjunction? object)
  (object-type? (ucode-type disjunction) object))
(register-predicate! scode-disjunction? 'scode-disjunction)

(define (scode-disjunction-predicate disjunction)
  (guarantee scode-disjunction? disjunction 'scode-disjunction-predicate)
  (map-reference-trap (lambda () (system-pair-car disjunction))))

(define (scode-disjunction-alternative disjunction)
  (guarantee scode-disjunction? disjunction 'scode-disjunction-alternative)
  (map-reference-trap (lambda () (system-pair-cdr disjunction))))

;;;; Declaration

(define (make-scode-block-declaration text)
  (vector block-declaration-marker text))

(define (scode-block-declaration? object)
  (and (vector? object)
       (fix:= 2 (vector-length object))
       (eq? block-declaration-marker (vector-ref object 0))))

(define (scode-block-declaration-text declaration)
  (guarantee scode-block-declaration? declaration 'scode-block-declaration-text)
  (vector-ref declaration 1))

(define block-declaration-marker
  '|#[Block Declaration]|)

;;;; Lambda

(define (make-scode-lambda name required optional rest body)
  (guarantee symbol? name 'make-scode-lambda)
  (guarantee list-of-unique-symbols? required 'make-scode-lambda)
  (guarantee list-of-unique-symbols? optional 'make-scode-lambda)
  (if rest (guarantee symbol? rest 'make-scode-lambda))
  (cond ((and (null? optional)
	      (not rest))
	 (make-slambda name required body))
	((and (< (length required) 256)
	      (< (length optional) 256))
	 (make-xlambda name required optional rest body))
	(else
	 (error "Unable to encode these lambda parameters:"
		required optional))))

(define (scode-lambda? object)
  (or (slambda? object)
      (xlambda? object)))
(register-predicate! scode-lambda? 'scode-lambda)

(define (scode-lambda-name lambda)
  (cond ((slambda? lambda) (slambda-name lambda))
	((xlambda? lambda) (xlambda-name lambda))
	(else (error:not-a scode-lambda? lambda 'scode-lambda-name))))

(define (scode-lambda-required lambda)
  (cond ((slambda? lambda) (slambda-required lambda))
	((xlambda? lambda) (xlambda-required lambda))
	(else (error:not-a scode-lambda? lambda 'scode-lambda-required))))

(define (scode-lambda-optional lambda)
  (cond ((slambda? lambda) '())
	((xlambda? lambda) (xlambda-optional lambda))
	(else (error:not-a scode-lambda? lambda 'scode-lambda-optional))))

(define (scode-lambda-rest lambda)
  (cond ((slambda? lambda) #f)
	((xlambda? lambda) (xlambda-rest lambda))
	(else (error:not-a scode-lambda? lambda 'scode-lambda-rest))))

(define (scode-lambda-body lambda)
  (cond ((slambda? lambda) (slambda-body lambda))
	((xlambda? lambda) (xlambda-body lambda))
	(else (error:not-a scode-lambda? lambda 'scode-lambda-body))))

(define scode-lambda-name:unnamed '|#[unnamed-procedure]|)
(define scode-lambda-name:let '|#[let-procedure]|)
(define scode-lambda-name:fluid-let '|#[fluid-let-procedure]|)
(define scode-lambda-name:internal-lambda '|#[internal-lambda]|)

;;; Simple representation

(define (make-slambda name required body)
  (system-pair-cons (ucode-type lambda)
		    (unmap-reference-trap body)
		    (list->vector (cons name required))))

(define (slambda? object)
  (object-type? (ucode-type lambda) object))

(define (slambda-name slambda)
  (vector-ref (system-pair-cdr slambda) 0))

(define (slambda-required slambda)
  (let ((v (system-pair-cdr slambda)))
    (subvector->list v 1 (vector-length v))))

(define (slambda-body slambda)
  (map-reference-trap (lambda () (system-pair-car slambda))))

;;; Extended representation

(define (make-xlambda name required optional rest body)
  (let ((v
	 (list->vector
	  (cons name
		(append required optional (if rest (list rest) '())))))
	(arity
	 (let ((n-required (length required))
	       (n-optional (length optional)))
	   (fix:or (fix:or n-optional
			   (fix:lsh n-required 8))
		   (fix:lsh (if rest 1 0) 16)))))
    (object-new-type (ucode-type extended-lambda)
		     (hunk3-cons (unmap-reference-trap body)
				 v
				 arity))))

(define (xlambda? object)
  (object-type? (ucode-type extended-lambda) object))

(define (xlambda-name xlambda)
  (vector-ref (system-hunk3-cxr1 xlambda) 0))

(define (xlambda-required xlambda)
  (receive (optional-start optional-end rest?) (decode-xlambda-arity xlambda)
    (declare (ignore optional-end rest?))
    (subvector->list (system-hunk3-cxr1 xlambda) 1 optional-start)))

(define (xlambda-optional xlambda)
  (receive (optional-start optional-end rest?) (decode-xlambda-arity xlambda)
    (declare (ignore rest?))
    (subvector->list (system-hunk3-cxr1 xlambda) optional-start optional-end)))

(define (xlambda-rest xlambda)
  (receive (optional-start optional-end rest?) (decode-xlambda-arity xlambda)
    (declare (ignore optional-start))
    (and rest?
	 (vector-ref (system-hunk3-cxr1 xlambda) optional-end))))

(define (decode-xlambda-arity xlambda)
  (let ((arity (object-datum (system-hunk3-cxr2 xlambda))))
    (let ((optional-start (fix:+ 1 (fix:and (fix:lsh arity -8) #xff))))
      (values optional-start
	      (fix:+ optional-start (fix:and arity #xff))
	      (fix:= 1 (fix:lsh arity -16))))))

(define (xlambda-body xlambda)
  (map-reference-trap (lambda () (system-hunk3-cxr0 xlambda))))