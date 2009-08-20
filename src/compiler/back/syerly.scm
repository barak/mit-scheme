#| -*-Scheme-*-

$Id$

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

;;;; Syntax time instruction expansion
;;; package: (compiler lap-syntaxer)

(declare (usual-integrations))

;;;; Early instruction assembly

(define early-instructions '())
(define early-transformers '())

(define lap:syntax-instruction-expander
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     (let ((instruction (scode/unquasiquote (car operands))))
       (let ((ierror
	      (lambda (message)
		(error (string-append "LAP:SYNTAX-INSTRUCTION-EXPANDER: "
				      message)
		       instruction))))
	 (if (not (pair? instruction))
	     (ierror "bad instruction"))
	 (cond ((eq? (car instruction) 'UNQUOTE)
		(if-not-expanded))
	       ((memq (car instruction)
		      '(EQUATE SCHEME-OBJECT SCHEME-EVALUATION
			       ENTRY-POINT LABEL BLOCK-OFFSET))
		(if-expanded
		 (scode/make-combination
		  (scode/make-variable 'DIRECTIVE->INSTRUCTION-SEQUENCE)
		  operands)))
	       (else
		(let ((place (assq (car instruction) early-instructions)))
		  (if (not place)
		      (ierror "unknown opcode"))
		  (let ((opcode (car instruction))
			(body (cdr instruction))
			(rules (cdr place)))
		    (early-pattern-lookup
		     rules
		     body
		     early-transformers
		     (scode/make-constant opcode)
		     (lambda (mode result)
		       (if (false? mode)
			   (ierror "unknown instruction"))
		       (if (eq? mode 'TOO-MANY)
			   (if-not-expanded)
			   (if-expanded result)))
		     1))))))))))

;;;; Quasiquote unsyntaxing

(define (scode/unquasiquote exp)
  (cond ((scode/combination? exp)
	 (scode/combination-components
	  exp
	  (lambda (operator operands)
	    (define (kernel operator-name)
	      (case operator-name
		((CONS)
		 (cons (scode/unquasiquote (car operands))
		       (scode/unquasiquote (cadr operands))))
		((LIST)
		 (apply list (map scode/unquasiquote operands)))
		((CONS*)
		 (apply cons* (map scode/unquasiquote operands)))
		((APPEND)
		 (append-map (lambda (component)
			       (if (scode/constant? component)
				   (scode/constant-value component)
				   (list (list 'UNQUOTE-SPLICING component))))
			     operands))
		(else (list 'UNQUOTE exp))))
	    (cond ((eq? operator (ucode-primitive cons))
		   ;; integrations
		   (kernel 'CONS))
		  ((scode/absolute-reference? operator)
		   (kernel (scode/absolute-reference-name operator)))
		  (else (list 'UNQUOTE exp))))))
	((scode/constant? exp)
	 (scode/constant-value exp))
	(else (list 'UNQUOTE exp))))

;;;; Bit compression expanders

;;; SYNTAX-EVALUATION and OPTIMIZE-GROUP expanders

(define syntax-evaluation-expander
  (scode->scode-expander
   (let ((environment
	  (package/environment (find-package '(COMPILER LAP-SYNTAXER)))))
     (lambda (operands if-expanded if-not-expanded)
       (if (and (scode/constant? (car operands))
		(scode/variable? (cadr operands))
		(not (lexical-unreferenceable?
		      environment
		      (scode/variable-name (cadr operands)))))
	   (if-expanded
	    (scode/make-constant
	     ((lexical-reference environment
				 (scode/variable-name (cadr operands)))
	      (scode/constant-value (car operands)))))
	   (if-not-expanded))))))

;; This relies on the fact that scode/constant-value = identity-procedure.

(define optimize-group-expander
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     if-not-expanded
     (optimize-group-internal
      operands
      (lambda (result make-group?)
	(if make-group?
	    (if-expanded
	     (scode/make-combination (scode/make-variable 'OPTIMIZE-GROUP)
				     result))
	    (if-expanded
	     (scode/make-constant result))))))))

;;;; CONS-SYNTAX expander

(define (is-operator? expr name primitive)
  (or (and primitive
	   (scode/constant? expr)
	   (eq? (scode/constant-value expr) primitive))
      (and (scode/variable? expr)
	   (eq? (scode/variable-name expr) name))
      (and (scode/absolute-reference? expr)
	   (eq? (scode/absolute-reference-name expr) name))))

(define cons-syntax-expander
  (scode->scode-expander
   (lambda (operands if-expanded if-not-expanded)
     (let ((default
	     (lambda ()
	       (if (not (scode/constant? (cadr operands)))
		   (if-not-expanded)
		   (begin
		     (if (not (null? (scode/constant-value (cadr operands))))
			 (error "CONS-SYNTAX-EXPANDER: bad tail"
				(cadr operands)))
		     (if-expanded
		      (scode/make-combination (ucode-primitive cons)
					      operands)))))))
       (if (and (scode/constant? (car operands))
		(bit-string? (scode/constant-value (car operands)))
		(scode/combination? (cadr operands)))
	   (scode/combination-components (cadr operands)
	     (lambda (operator inner-operands)
	       (if (and (or (is-operator? operator 'CONS-SYNTAX false)
			    (is-operator? operator
					  'CONS
					  (ucode-primitive cons)))
			(scode/constant? (car inner-operands))
			(bit-string?
			 (scode/constant-value (car inner-operands))))
		   (if-expanded
		    (scode/make-combination
		     (if (scode/constant? (cadr inner-operands))
			 (ucode-primitive cons)
			 operator)
		     (cons (instruction-append
			    (scode/constant-value (car operands))
			    (scode/constant-value (car inner-operands)))
			   (cdr inner-operands))))
		   (default))))
	   (default))))))

;;;; INSTRUCTION->INSTRUCTION-SEQUENCE expander

(define instruction->instruction-sequence-expander
  (let ()
    (define (parse expression receiver)
      (if (not (scode/combination? expression))
	  (receiver false false false)
	  (scode/combination-components expression
	    (lambda (operator operands)
	      (cond ((and (not (is-operator? operator
					     'CONS
					     (ucode-primitive cons)))
			  (not (is-operator? operator 'CONS-SYNTAX false)))
		     (receiver false false false))
		    ((scode/constant? (cadr operands))
		     (if (not (null? (scode/constant-value (cadr operands))))
			 (error "INST->INST-SEQ-EXPANDER: bad CONS-SYNTAX tail"
				(scode/constant-value (cadr operands))))
		     (let ((name
			    (generate-uninterned-symbol 'INSTRUCTION-TAIL-)))
		       (receiver true
				 (cons name expression)
				 (scode/make-variable name))))
		    (else
		     (parse (cadr operands)
		       (lambda (mode info rest)
			 (if (not mode)
			     (receiver false false false)
			     (receiver true info
				       (scode/make-combination
					operator
					(list (car operands) rest))))))))))))
    (scode->scode-expander
     (lambda (operands if-expanded if-not-expanded)
       (if (not (scode/combination? (car operands)))
	   (if-not-expanded)
	   (parse (car operands)
	     (lambda (mode binding rest)
	       (if (not mode)
		   (if-not-expanded)
		   (if-expanded
		    (scode/make-let
		     (list (car binding))
		     (list (cdr binding))
		     (scode/make-combination
		      (ucode-primitive cons)
		      (list rest
			    (scode/make-variable (car binding))))))))))))))