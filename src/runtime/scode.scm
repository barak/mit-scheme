#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

(add-boot-deps! '(runtime microcode-tables))

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
	      '(access assignment combination comment conditional
		       definition delay disjunction extended-lambda lambda
		       lexpr quotation sequence the-environment variable))
    type-vector))

(define (scode-constant? object)
  (not (scode-expression? object)))
(register-predicate! scode-constant? 'scode-constant)

;;;; Quotation

(define (make-scode-quotation expression)
  (safe-system-pair-cons (ucode-type quotation) expression '()))

(define (scode-quotation? object)
  (object-type? (ucode-type quotation) object))
(register-predicate! scode-quotation? 'scode-quotation)

(define (scode-quotation-expression quotation)
  (guarantee scode-quotation? quotation 'scode-quotation-expression)
  (safe-system-pair-car quotation))

;;;; Variable

(define (make-scode-variable name #!optional safe?)
  (guarantee symbol? name 'make-scode-variable)
  (safe-system-triple-cons (ucode-type variable)
			   name
			   (or (default-object? safe?) (not safe?))
			   #f))

(define (scode-variable? object)
  (object-type? (ucode-type variable) object))
(register-predicate! scode-variable? 'scode-variable)

(define (scode-variable-name variable)
  (guarantee scode-variable? variable 'scode-variable-name)
  (safe-system-triple-first variable))

(define (scode-variable-safe? variable)
  (guarantee scode-variable? variable 'scode-variable-safe?)
  (not (safe-system-triple-second variable)))

;;;; Definition

(define (make-scode-definition name value)
  (guarantee symbol? name 'make-scode-definition)
  (safe-system-pair-cons (ucode-type definition) name value))

(define (scode-definition? object)
  (object-type? (ucode-type definition) object))
(register-predicate! scode-definition? 'scode-definition)

(define (scode-definition-name definition)
  (guarantee scode-definition? definition 'scode-definition-name)
  (safe-system-pair-car definition))

(define (scode-definition-value definition)
  (guarantee scode-definition? definition 'scode-definition-value)
  (safe-system-pair-cdr definition))

;;;; Syntax definition

(define (make-scode-macro-definition name transformer)
  (guarantee macro-transformer-expr? transformer 'make-scode-macro-definition)
  (make-scode-definition name
			 (make-macro-reference-trap-expression transformer)))

(define (scode-macro-definition? object)
  (and (scode-definition? object)
       (let ((value (scode-definition-value object)))
	 (and (macro-reference-trap-expression? value)
	      (macro-transformer-expr?
	       (macro-reference-trap-expression-transformer value))))))

(define (scode-macro-definition-name defn)
  (guarantee scode-macro-definition? defn 'scode-macro-definition-name)
  (scode-definition-name defn))

(define (scode-macro-definition-transformer defn)
  (guarantee scode-macro-definition? defn 'scode-macro-definition-transformer)
  (macro-reference-trap-expression-transformer (scode-definition-value defn)))

(define (make-macro-transformer-expr keyword transformer)
  (let ((procedure
	 (or (keyword->procedure keyword)
	     (error "Unknown macro-transformer keyword:" keyword))))
    (make-scode-combination (make-scode-absolute-reference procedure)
			    (list transformer
				  (make-scode-the-environment)))))

(define (macro-transformer-expr? object)
  (and (scode-combination? object)
       (let ((operator (scode-combination-operator object)))
	 (and (scode-access? operator)
	      (procedure->keyword (scode-access-name operator))
	      (eq? system-global-environment
		   (scode-access-environment operator))))
       (let ((operands (scode-combination-operands object)))
	 (and (= 2 (length operands))
	      (scode-lambda? (car operands))
	      (scode-the-environment? (cadr operands))))))

(define (macro-transformer-expr-keyword scode)
  (procedure->keyword (scode-access-name (scode-combination-operator scode))))

(define (macro-transformer-expr-lambda scode)
  (car (scode-combination-operands scode)))

(define (keyword->procedure keyword)
  (let ((p (assq keyword transformer-keywords)))
    (and p
	 (cdr p))))

(define (procedure->keyword procedure)
  (let ((p
	 (find (lambda (p) (eq? procedure (cdr p)))
	       transformer-keywords)))
    (and p
	 (car p))))

(define transformer-keywords
  '((sc-macro-transformer . sc-macro-transformer->expander)
    (rsc-macro-transformer . rsc-macro-transformer->expander)
    (er-macro-transformer . er-macro-transformer->expander)
    (spar-macro-transformer . spar-macro-transformer->expander)))

;;;; Assignment

(define (make-scode-assignment name value)
  (guarantee symbol? name 'make-scode-assignment)
  (safe-system-pair-cons (ucode-type assignment)
			 (make-scode-variable name)
			 value))

(define (scode-assignment? object)
  (object-type? (ucode-type assignment) object))
(register-predicate! scode-assignment? 'scode-assignment)

(define (scode-assignment-name assignment)
  (guarantee scode-assignment? assignment 'scode-assignment-name)
  (scode-variable-name (safe-system-pair-car assignment)))

(define (scode-assignment-value assignment)
  (guarantee scode-assignment? assignment 'scode-assignment-value)
  (safe-system-pair-cdr assignment))

;;;; Comment

(define (make-scode-comment text expression)
  (safe-system-pair-cons (ucode-type comment) expression text))

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
  (safe-system-pair-car comment))

(define (set-scode-comment-expression! comment expression)
  (guarantee scode-comment? comment 'set-scode-comment-expression!)
  (safe-system-pair-set-car! comment expression))

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
  (safe-system-pair-cons (ucode-type access) environment name))

(define (scode-access? object)
  (object-type? (ucode-type access) object))
(register-predicate! scode-access? 'scode-access)

(define (scode-access-environment access)
  (guarantee scode-access? access 'scode-access-environment)
  (safe-system-pair-car access))

(define (scode-access-name access)
  (guarantee scode-access? access 'scode-access-name)
  (safe-system-pair-cdr access))

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
  (safe-system-pair-cons (ucode-type delay) expression '()))

(define (scode-delay? object)
  (object-type? (ucode-type delay) object))
(register-predicate! scode-delay? 'scode-delay)

(define (scode-delay-expression delay)
  (guarantee scode-delay? delay 'scode-delay-expression)
  (safe-system-pair-car delay))

;;;; Sequence

(define (make-scode-sequence actions)
  (guarantee list? actions 'make-sequence)
  (let ((actions (append-map scode-sequence-actions actions)))
    (if (pair? actions)
	(let loop ((actions actions))
	  (if (pair? (cdr actions))
	      (%make-scode-sequence (car actions) (loop (cdr actions)))
	      (car actions)))
	(empty-sequence))))

(define (%make-scode-sequence first rest)
  (safe-system-pair-cons (ucode-type sequence) first rest))

(define (scode-sequence? object)
  (object-type? (ucode-type sequence) object))
(register-predicate! scode-sequence? 'scode-sequence)

(define (scode-sequence-first expression)
  (guarantee scode-sequence? expression 'scode-sequence-first)
  (safe-system-pair-car expression))

(define (scode-sequence-rest expression)
  (guarantee scode-sequence? expression 'scode-sequence-rest)
  (safe-system-pair-cdr expression))

(define (scode-sequence-actions expression)
  (if (scode-sequence? expression)
      (if (sequence-empty? expression)
	  '()
	  (append (scode-sequence-actions (safe-system-pair-car expression))
		  (scode-sequence-actions (safe-system-pair-cdr expression))))
      (list expression)))

(define (empty-sequence)
  (system-pair-cons (ucode-type sequence) #!unspecific #!unspecific))

(define (sequence-empty? sequence)
  (and (eq? #!unspecific (system-pair-car sequence))
       (eq? #!unspecific (system-pair-cdr sequence))))

;;;; Combination

(define (make-scode-combination operator operands)
  (let ((comb
	 (safe-system-vector-cons (ucode-type combination)
				  (fix:+ 1 (length operands)))))
    (safe-system-vector-set! comb 0 operator)
    (do ((operands operands (cdr operands))
	 (i 1 (fix:+ i 1)))
	((not (pair? operands)))
      (safe-system-vector-set! comb i (car operands)))
    comb))

(define (scode-combination? object)
  (object-type? (ucode-type combination) object))
(register-predicate! scode-combination? 'scode-combination)

(define (scode-combination-operator combination)
  (guarantee scode-combination? combination 'scode-combination-operator)
  (safe-system-vector-ref combination 0))

(define (scode-combination-operand combination index)
  (guarantee scode-combination? combination 'scode-combination-operand)
  (safe-system-vector-ref combination (fix:+ 1 index)))

(define (scode-combination-operands combination)
  (guarantee scode-combination? combination 'scode-combination-operands)
  (reverse
   (fold (lambda (index operands)
	   (cons (safe-system-vector-ref combination index) operands))
	 '()
	 (iota (fix:- (system-vector-length combination) 1) 1))))

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
  (safe-system-triple-cons (ucode-type conditional)
			   predicate
			   consequent
			   alternative))

(define (scode-conditional? object)
  (object-type? (ucode-type conditional) object))
(register-predicate! scode-conditional? 'scode-conditional)

(define undefined-scode-conditional-branch unspecific)

(define (scode-conditional-predicate conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-predicate)
  (safe-system-triple-first conditional))

(define (scode-conditional-consequent conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-consequent)
  (safe-system-triple-second conditional))

(define (scode-conditional-alternative conditional)
  (guarantee scode-conditional? conditional 'scode-conditional-alternative)
  (safe-system-triple-third conditional))

;;;; Disjunction

(define (make-scode-disjunction predicate alternative)
  (safe-system-pair-cons (ucode-type disjunction) predicate alternative))

(define (scode-disjunction? object)
  (object-type? (ucode-type disjunction) object))
(register-predicate! scode-disjunction? 'scode-disjunction)

(define (scode-disjunction-predicate disjunction)
  (guarantee scode-disjunction? disjunction 'scode-disjunction-predicate)
  (safe-system-pair-car disjunction))

(define (scode-disjunction-alternative disjunction)
  (guarantee scode-disjunction? disjunction 'scode-disjunction-alternative)
  (safe-system-pair-cdr disjunction))

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

(define (scode-lambda-name->syntax-name name)
  (let ((association (assq name special-form-procedure-names)))
    (and association
	 (cdr association))))

(define scode-lambda-name:unnamed '|#[unnamed-procedure]|)
(define scode-lambda-name:let '|#[let-procedure]|)
(define scode-lambda-name:fluid-let '|#[fluid-let-procedure]|)
(define scode-lambda-name:internal-lambda '|#[internal-lambda]|)

(define special-form-procedure-names
  `((,scode-lambda-name:unnamed . lambda)
    (,scode-lambda-name:internal-lambda . lambda)
    (,scode-lambda-name:let . let)
    (,scode-lambda-name:fluid-let . fluid-let)))

;;; Simple representation

(define (make-slambda name required body)
  (safe-system-pair-cons (ucode-type lambda)
			 body
			 (list->vector (cons name required))))

(define (slambda? object)
  (object-type? (ucode-type lambda) object))

(define (slambda-name slambda)
  (vector-ref (system-pair-cdr slambda) 0))

(define (slambda-required slambda)
  (let ((v (system-pair-cdr slambda)))
    (subvector->list v 1 (vector-length v))))

(define (slambda-body slambda)
  (safe-system-pair-car slambda))

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
    (safe-system-triple-cons (ucode-type extended-lambda) body v arity)))

(define (xlambda? object)
  (object-type? (ucode-type extended-lambda) object))

(define (xlambda-name xlambda)
  (vector-ref (safe-system-triple-second xlambda) 0))

(define (xlambda-required xlambda)
  (let-values (((optional-start optional-end rest?)
		(decode-xlambda-arity xlambda)))
    (declare (ignore optional-end rest?))
    (vector->list (safe-system-triple-second xlambda) 1 optional-start)))

(define (xlambda-optional xlambda)
  (let-values (((optional-start optional-end rest?)
		(decode-xlambda-arity xlambda)))
    (declare (ignore rest?))
    (vector->list (safe-system-triple-second xlambda)
		  optional-start
		  optional-end)))

(define (xlambda-rest xlambda)
  (let-values (((optional-start optional-end rest?)
		(decode-xlambda-arity xlambda)))
    (declare (ignore optional-start))
    (and rest?
	 (vector-ref (safe-system-triple-second xlambda) optional-end))))

(define (decode-xlambda-arity xlambda)
  (let ((arity (object-datum (safe-system-triple-third xlambda))))
    (let ((optional-start (fix:+ 1 (fix:and (fix:lsh arity -8) #xff))))
      (values optional-start
	      (fix:+ optional-start (fix:and arity #xff))
	      (fix:= 1 (fix:lsh arity -16))))))

(define (xlambda-body xlambda)
  (safe-system-triple-first xlambda))