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

;;;; SCode Optimizer: Data Types
;;; package: (scode-optimizer)

(declare (usual-integrations))

;;;; Enumerations

(define (enumeration/make names)
  (let ((enumerands
	 (let loop ((names names) (index 0))
	   (if (pair? names)
	       (cons (vector #f (car names) index)
		     (loop (cdr names) (1+ index)))
	       '()))))
    (let ((enumeration
	   (cons (list->vector enumerands)
		 (map (lambda (enumerand)
			(cons (enumerand/name enumerand) enumerand))
		      enumerands))))
      (for-each (lambda (enumerand)
		  (vector-set! enumerand 0 enumeration))
		enumerands)
      enumeration)))

(define-structure (enumerand (type vector)
			     (conc-name enumerand/))
  (enumeration #f read-only #t)
  (name #f read-only #t)
  (index #f read-only #t))

(define-integrable (enumeration/cardinality enumeration)
  (vector-length (car enumeration)))

(define-integrable (enumeration/index->enumerand enumeration index)
  (vector-ref (car enumeration) index))

(define (enumeration/name->enumerand enumeration name)
  (cdr (or (assq name (cdr enumeration))
	   (error "Unknown enumeration name:" name))))

(define-integrable (enumeration/name->index enumeration name)
  (enumerand/index (enumeration/name->enumerand enumeration name)))

(define-syntax define-enumeration
  (sc-macro-transformer
   (lambda (form environment)
     (let ((enumeration-name (cadr form))
	   (enumerand-names (caddr form)))
       `(BEGIN
	  (DEFINE ,enumeration-name
	    (ENUMERATION/MAKE ',enumerand-names))
	  ,@(map (lambda (enumerand-name)
		   `(DEFINE ,(symbol-append enumerand-name '/ENUMERAND)
		      (ENUMERATION/NAME->ENUMERAND
		       ,(close-syntax enumeration-name environment)
		       ',enumerand-name)))
		 enumerand-names))))))

(define-enumeration enumeration/random
  (block
   delayed-integration
   variable))

(define-enumeration enumeration/expression
  (access
   assignment
   combination
   conditional
   constant
   declaration
   delay
   disjunction
   open-block
   procedure
   quotation
   reference
   sequence
   the-environment))

;;;; Records

;;; The records used in SF are vectors that are tagged by an enumerand.

;;; NOTE: In most cases, there is the assumption that the second element
;;; in the vector is a piece of SCode that represents the original,
;;; unintegrated form.

(define-syntax define-simple-type
  (sc-macro-transformer
   (lambda (form environment)
     (let ((name (second form))
	   (constructor-name (third form))  ;; symbol or #F
	   (slots (fourth form)))
       `(BEGIN
	  (DEFINE-STRUCTURE
	      (,name
	       (TYPE VECTOR)
	       (NAMED
		,(close-syntax (symbol-append name '/ENUMERAND) environment))
	       (TYPE-DESCRIPTOR ,(symbol-append 'RTD: name))
	       (CONC-NAME ,(symbol-append name '/))
	       (CONSTRUCTOR ,(or constructor-name
				 (symbol-append name '/MAKE))))
	    (scode #f read-only #t)
	    ,@slots)
	 (DEFINE-GUARANTEE ,name ,(symbol->string name)))))))

;;; These accessors apply to all the record types.
(define-integrable (object/enumerand object)
  (vector-ref object 0))

(define (set-object/enumerand! object enumerand)
  (vector-set! object 0 enumerand))

(define-integrable (object/scode object)
  (vector-ref object 1))

(define (with-new-scode scode object)
  (let ((new (vector-copy object)))
    (vector-set! new 1 scode)
    new))

;;; BLOCK
(define-structure (block (type vector)
			 (named block/enumerand)
			 (conc-name block/)
			 (constructor block/%make
				      (parent safe? bound-variables)))
  parent
  (children '())
  safe?
  (declarations (declarations/make-null))
  bound-variables)

(define-guarantee block "block")

(define (block/make parent safe? bound-variables)
  (let ((block (block/%make parent safe? bound-variables)))
    (if parent
	(set-block/children! parent (cons block (block/children parent))))
    block))

;;; DELAYED-INTEGRATION
(define-structure (delayed-integration
		   (type vector)
		   (named delayed-integration/enumerand)
		   (conc-name delayed-integration/)
		   (constructor delayed-integration/make (operations value)))
  (state 'NOT-INTEGRATED)
  (environment #f)
  operations
  value)

(define-guarantee delayed-integration "delayed integration")

;;; VARIABLE
;; Done specially so we can tweak the print method.
;; This makes debugging an awful lot easier.
;; Note that there is no SCODE slot.
(define-structure (variable
		   (type vector)
		   (named variable/enumerand)
		   (type-descriptor rtd:variable)
		   (conc-name variable/)
		   (constructor variable/make (block name flags))
		   (print-procedure
		    (standard-unparser-method
		     'variable
		     (lambda (var port)
		       (write-string " " port)
		       (write (variable/name var) port)))))
  block
  name
  flags)

(define-guarantee variable "variable")

;;; Expressions
(define-simple-type access          #f                 (block environment name))
(define-simple-type assignment      #f                 (block variable value))
(define-simple-type combination     combination/%make  (block operator operands))
(define-simple-type conditional     conditional/%make  (predicate consequent alternative))
(define-simple-type constant        #f                 (value))
(define-simple-type declaration     #f                 (declarations expression))
(define-simple-type delay           #f                 (expression))
(define-simple-type disjunction     disjunction/%make  (predicate alternative))
(define-simple-type open-block      #f                 (block variables values actions))
(define-simple-type procedure       #f                 (block name required optional rest body))
(define-simple-type quotation       #f                 (block expression))
(define-simple-type sequence        sequence/%make     (actions))
(define-simple-type the-environment #f                 (block))

;;; Helpers for expressions

;; The primitive predicates that only return #T or #F.
(define primitive-boolean-predicates
  (map (lambda (name)
	 (make-primitive-procedure name #t))
       '(
	 %RECORD?
	 &<
	 &=
	 &>
	 BIT-STRING?
	 CELL?
	 CHAR-ASCII?
	 CHAR?
	 EQ?
	 EQUAL-FIXNUM?
	 FIXNUM?
	 FLONUM-EQUAL?
	 FLONUM-GREATER?
	 FLONUM-LESS?
	 FLONUM-NEGATIVE?
	 FLONUM-POSITIVE?
	 FLONUM-ZERO?
	 FLONUM?
	 GREATER-THAN-FIXNUM?
	 INDEX-FIXNUM?
	 INTEGER-EQUAL?
	 INTEGER-GREATER?
	 INTEGER-LESS?
	 INTEGER-NEGATIVE?
	 INTEGER-POSITIVE?
	 INTEGER-ZERO?
	 LESS-THAN-FIXNUM?
	 NEGATIVE-FIXNUM?
	 NEGATIVE?
	 NOT
	 NULL?
	 OBJECT-TYPE?
	 PAIR?
	 POSITIVE-FIXNUM?
	 POSITIVE?
	 STRING?
	 VECTOR?
	 ZERO-FIXNUM?
	 ZERO?
	 )))

;; True if expression is a call to one of the primitive-boolean-predicates.
(define (expression/call-to-boolean-predicate? expression)
  (and (combination? expression)
       (let ((operator (combination/operator expression)))
	 (and (constant? operator)
	      (let ((operator-value (constant/value operator)))
		(and (memq operator-value primitive-boolean-predicates)
		     (procedure-arity-valid?
		      operator-value
		      (length (combination/operands expression)))))))))

;; These primitives have no side effects.  We consider primitives
;; that check their arguments *have* a side effect. (Conservative)
(define effect-free-primitives
  (map (lambda (name)
	 (make-primitive-procedure name #t))
       '(
	 %RECORD?
	 BIT-STRING?
	 CELL?
	 CHAR?
	 EQ?
	 FIXNUM?
	 FLONUM?
	 NOT
	 NULL?
	 OBJECT-TYPE
	 OBJECT-TYPE?
	 PAIR?
	 STRING?
	 VECTOR?
	 )))

;; True if expression is a call to one of the effect-free-primitives.
(define (expression/call-to-effect-free-primitive? expression)
  (and (combination? expression)
       (let ((operator (combination/operator expression)))
	 (and (constant? operator)
	      (let ((operator-value (constant/value operator)))
		(and (memq operator-value effect-free-primitives)
		     (procedure-arity-valid?
		      operator-value
		      (length (combination/operands expression)))))))))

;; True if expression is a call to NOT.
;; Used in conditional simplification.
(define (expression/call-to-not? expression)
  (and (combination? expression)
       (let ((operator (combination/operator expression)))
	 (and (constant? operator)
	      (let ((operator-value (constant/value operator)))
		(and (eq? operator-value (ucode-primitive not))
		     (procedure-arity-valid?
		      operator-value
		      (length (combination/operands expression)))))))))

(define (expression/constant-eq? expression value)
  (cond ((constant? expression) (eq? (constant/value expression) value))
	((declaration? expression)
	 (expression/constant-eq? (declaration/expression expression) value))
	(else #f)))

(define-integrable (global-ref/make name)
  (access/make #f
	       #f
	       (constant/make #f system-global-environment)
	       name))

(define (global-ref? object)
  (and (access? object)
       (expression/constant-eq? (access/environment object) system-global-environment)
       (access/name object)))

;;; Constructors that need to do work.

;; When constucting a combination, we may discover that we
;; can reduce the combination through constant folding.
(define sf:enable-constant-folding? #t)

;; If we have a LET expression, and an argument has been integrated,
;; then we can remove it from the lambda binding and the argument
;; list.  This could lead to the combination disappearing altogether.
(define sf:enable-argument-deletion? #t)

;; If we apply a primitive to a conditional, rewrite such that
;; the primitive is applied to the arms of the conditional.
(define sf:enable-distribute-primitives? #t)

;; Foldable operators primitives that are members of
;; combination/constant-folding-operators

(define (foldable-combination? operator operands)
  (and (constant? operator)
       (let ((operator-value (constant/value operator)))
	 (and (primitive-procedure? operator-value)
	      (procedure-arity-valid? operator-value (length operands))
	      (memq operator-value combination/constant-folding-operators)))
	  ;; Check that the arguments are constant.
       (for-all? operands constant?)))

;; An operator is reducable if we can safely rewrite its argument list.
(define (reducable-operator? operator)
  (and (procedure? operator)
       ;; if the block is not safe, then random code can be
       ;; injected and it will expect to see all the arguments.
       (block/safe? (procedure/block operator))
       ;; if there are declarations we don't understand, we
       ;; should leave things alone.
       (for-all? (declarations/original
		  (block/declarations (procedure/block operator)))
		 declarations/known?)
       ;; Unintegrated optionals are tricky and rare.  Punt.
       (for-all? (procedure/optional operator) variable/integrated)
       ;; Unintegrated rest arguments are tricky and rare.  Punt.
       (let ((rest-arg (procedure/rest operator)))
	 (or (not rest-arg) (variable/integrated rest-arg)))))

(define (combination/make expression block operator operands)
  (cond ((and (foldable-combination? operator operands)
	      (noisy-test sf:enable-constant-folding? "Folding constants"))
	 (combination/fold-constant expression
				    (constant/value operator)
				    (map constant/value operands)))

	((and (constant? operator)
	      (primitive-procedure? (constant/value operator))
	      (not (eq? (constant/value operator) (ucode-primitive not)))
	      (length=? operands 1)
	      (conditional? (car operands))
	      (noisy-test sf:enable-distribute-primitives?
			  "Distribute primitives over conditionals"))
	 (conditional/make (and expression (object/scode expression))
			   (conditional/predicate (car operands))
			   (combination/make #f
					     block
					     (constant/make #f (constant/value operator))
					     (list (conditional/consequent (car operands))))
			   (combination/make #f
					     block
					     (constant/make #f (constant/value operator))
					     (list (conditional/alternative (car operands))))))

	((and (reducable-operator? operator)
	      (noisy-test sf:enable-argument-deletion? "argument deletion"))
	 (call-with-values (lambda () (partition-operands operator operands))
	   (lambda (new-argument-list new-operand-list other-operands)
	     ;; The new-argument-list has the remaining arguments
	     ;; after reduction.  The new-operand-list is the remaining
	     ;; operands after reduction.  The other-operands are a
	     ;; list of operands that must be evaluated (for effect)
	     ;; but whose value is discarded.
	     (let ((result-body
		    (if (and (null? new-argument-list)
			     ;; need to avoid things like this
			     ;; (foo bar (let () (define (baz) ..) ..))
			     ;; optimizing into
			     ;; (foo bar (define (baz) ..) ..)
			     (not (open-block? (procedure/body operator))))
			(procedure/body operator)
			(combination/%make
			 (and expression (object/scode expression))
			 block
			 (procedure/make
			  (procedure/scode operator)
			  (procedure/block operator)
			  (procedure/name operator)
			  new-argument-list
			  '()
			  #f
			  (procedure/body operator))
			 new-operand-list))))
	       (sequence/make
		(and expression (object/scode expression))
		(append other-operands (list result-body)))))))
	(else
	 (combination/%make (and expression (object/scode expression)) block operator operands))))

(define (combination/fold-constant expression operator operands)
  (let ((result (apply operator operands)))
  (if (not (eq? sf:enable-constant-folding? #t))
      (with-notification
       (lambda (port)
	 (display "Folding (" port)
	 (display operator port)
	 (for-each (lambda (operand) (display " " port) (write operand port)) operands)
	 (display ") => " port)
	 (write result port))))
  (constant/make (and expression (object/scode expression)) result)))

(define-integrable (partition-operands operator operands)
  (let ((free-in-body (expression/free-variables (procedure/body operator))))
    (let loop ((parameters 		(append (procedure/required operator)
						(procedure/optional operator)))
	       (operands   		operands)
	       (required-parameters	'())
	       (referenced-operands	'())
	       (unreferenced-operands	'()))
    (cond ((null? parameters)
	   (if (or (procedure/rest operator) (null? operands))
	       (values (reverse required-parameters) ; preserve order
			 (reverse referenced-operands)
			 (if (or (null? operands)
				 (variable/integrated (procedure/rest operator)))
			     unreferenced-operands
			     (append operands unreferenced-operands)))
	       (error "Argument mismatch" operands)))
	  ((null? operands)
	   (error "Argument mismatch" parameters))
	  (else
	   (let ((this-parameter (car parameters))
		 (this-operand   (car operands)))
	     (cond ((memq this-parameter free-in-body)
		    (loop (cdr parameters)
			  (cdr operands)
			  (cons this-parameter required-parameters)
			  (cons this-operand   referenced-operands)
			  unreferenced-operands))
		   ((variable/integrated this-parameter)
		    (loop (cdr parameters)
			  (cdr operands)
			  required-parameters
			  referenced-operands
			  unreferenced-operands))
		   (else
		    (loop (cdr parameters)
			  (cdr operands)
			  required-parameters
			  referenced-operands
			  (cons this-operand
				unreferenced-operands))))))))))

;;; Conditional

;; If the arms of a conditional are #T and #F, then
;; we're just canonicalizing the predicate value to a boolean.
;; If we already know the predicate is a boolean we can elide
;; this step.  Additionally, if the arms are #F and #T,
;; we're simply calling NOT.
(define sf:enable-elide-conditional-canonicalization? #t)

(define (conditional/make scode predicate consequent alternative)
  (cond ((and (expression/pure-false? consequent)
	      (expression/pure-true? alternative)
	      (noisy-test sf:enable-elide-conditional-canonicalization?
			  "Eliding inverse conditional canonicalization"))
	;; (if <exp> #f #t) => (not <exp>)
	;; We know that we're not making a double negative here
	;; because a call to NOT in the predicate would already
	;; have been inverted.
	 (combination/%make scode #f (constant/make #f (ucode-primitive not)) (list predicate)))

	;; If the consequent and alternative are the same, just make a sequence.
	((expressions/equal? consequent alternative)
	 (sequence/make scode (list predicate consequent)))

	(else
	 (conditional/%make scode predicate consequent alternative))))

;;; Disjunction

;; If the alternative of a disjunction is #F, we can elide the disjunction.
(define sf:enable-disjunction-simplification? #t)

(define (disjunction/make scode predicate alternative)
  (cond ((and (expression/pure-false? alternative)
	      (noisy-test sf:enable-disjunction-simplification? "Simplify disjunction"))
	 ;; (or (foo) #f) => (foo)
	 predicate)

	(else
	 (disjunction/%make scode predicate alternative))))

;;; Sequence

;;  Ensure that sequences are always flat.
(define (sequence/make scode actions)
  (define (sequence/collect-actions collected actions)
    (fold-left (lambda (reversed action)
		 (if (sequence? action)
		     (sequence/collect-actions reversed (sequence/actions action))
		     (cons action reversed)))
	       collected
	       actions))
  (let ((filtered-actions
	 (fold-left (lambda (filtered action)
		      (if (expression/effect-free? action)
			  (if (null? filtered)
			      (list action)
			      filtered)
			  (cons action filtered)))
		    '()
		    (sequence/collect-actions '() actions))))
    (if (null? (cdr filtered-actions))
	(car filtered-actions)
	(sequence/%make scode filtered-actions))))

;; Done specially so we can tweak the print method.
;; This makes debugging an awful lot easier.
(define-structure (reference
		   (type vector)
		   (named reference/enumerand)
		   (type-descriptor rtd:reference)
		   (conc-name reference/)
		   (constructor reference/make)
		   (print-procedure
		    (standard-unparser-method
		     'reference
		     (lambda (ref port)
		       (write-string " to " port)
		       (write (variable/name (reference/variable ref)) port)))))
  (scode #f read-only #t)
  block
  variable)

(define-guarantee reference "reference")

;;;; Miscellany

(define-syntax define-flag
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((name (cadr form))
	   (tester (caddr form))
	   (setter (cadddr form)))
       `(BEGIN
	  (DEFINE (,tester VARIABLE)
	    (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
	  (DEFINE (,setter VARIABLE)
	    (IF (NOT (MEMQ ',name (VARIABLE/FLAGS VARIABLE)))
		(SET-VARIABLE/FLAGS!
		 VARIABLE
		 (CONS ',name (VARIABLE/FLAGS VARIABLE))))))))))

(define-flag SIDE-EFFECTED variable/side-effected variable/side-effect!)
(define-flag REFERENCED    variable/referenced    variable/reference!)
(define-flag INTEGRATED    variable/integrated    variable/integrated!)
(define-flag MAY-IGNORE    variable/may-ignore?   variable/may-ignore!)
(define-flag MUST-IGNORE   variable/must-ignore?  variable/must-ignore!)

(define open-block/value-marker
  ;; This must be an interned object because we will fasdump it and
  ;; fasload it back in.
  (intern "#[(scode-optimizer)open-block/value-marker]"))

(define (expression/make-dispatch-vector)
  (make-vector (enumeration/cardinality enumeration/expression)))

(define (expression/make-method-definer dispatch-vector)
  (lambda (type-name method)
    (vector-set! dispatch-vector
		 (enumeration/name->index enumeration/expression type-name)
		 method)))

(define-integrable (expression/method dispatch-vector expression)
  (vector-ref dispatch-vector (enumerand/index (object/enumerand expression))))

(define-integrable (name->method dispatch-vector name)
  ;; Useful for debugging
  (vector-ref dispatch-vector
	      (enumeration/name->index enumeration/expression name)))

;;; Integration Info
(define integration-info-tag
  (string-copy "integration-info"))

(define-integrable (make-integration-info expression)
  (cons integration-info-tag expression))

(define-integrable (integration-info? object)
  (and (pair? object)
       (eq? integration-info-tag (car object))))

(define-guarantee integration-info "Integration info")

(define (integration-info/expression integration-info)
  (guarantee-integration-info integration-info 'integration-info/expression)
  (cdr integration-info))

(define-integrable (constant->integration-info constant)
  (make-integration-info (constant/make #f constant)))

;;; Returns #T if switch is not #F or 'warn.
;;; Additionally, prints text if switch is not #T.
;;; So set switch to #f to disable,
;;; set it to 'warn to disable, but issue a warning upon testing,
;;; set it to #t to enable,
;;; or set it to something like 'ok to enable *and* print noise.

;;; To use, make this the last clause in a test.
(define (noisy-test switch text)
  (and switch
       (cond ((eq? switch 'warn)
	      (warn "Not performing possible action:" text)
	      #f)
	     ((not (eq? switch #t))
	      (with-notification
	       (lambda (port) (write-string text port)))
	      #t)
	     (else #t))))
