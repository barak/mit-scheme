;;; -*-Scheme-*-
;;;
;;; $Id: syntax-output.scm,v 14.2 2002/03/01 03:09:58 cph Exp $
;;;
;;; Copyright (c) 1989-1991, 2001, 2002 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; Syntaxer Output Interface

(declare (usual-integrations))

(define (syntax-error history . rest)
  history				;ignore
  (apply error rest))

(define (transformer-eval expression environment)
  (eval expression environment))

(define (output/variable name)
  (make-variable name))

(define (output/constant datum)
  datum)

(define (output/assignment name value)
  (make-assignment name value))

(define (output/top-level-definition name value)
  (make-definition name
		   (if (lambda? value)
		       (lambda-components* value
			 (lambda (name* required optional rest body)
			   (if (eq? name* lambda-tag:unnamed)
			       (make-lambda* name required optional rest body)
			       value)))
		       value)))

(define (output/top-level-syntax-definition name value)
  (make-definition name (make-macro-reference-trap-expression value)))

(define (output/conditional predicate consequent alternative)
  (make-conditional predicate consequent alternative))

(define (output/sequence expressions)
  (make-sequence expressions))

(define (output/combination operator operands)
  (make-combination operator operands))

(define (output/lambda lambda-list body)
  (output/named-lambda lambda-tag:unnamed lambda-list body))

(define (output/named-lambda name lambda-list body)
  (output/lambda-internal name lambda-list '() body))

(define (output/lambda-internal name lambda-list declarations body)
  (call-with-values (lambda () (parse-mit-lambda-list lambda-list))
    (lambda (required optional rest)
      (make-lambda* name required optional rest
		    (let ((declarations (apply append declarations)))
		      (if (pair? declarations)
			  (make-sequence (make-block-declaration declarations)
					 body)
			  body))))))

(define (output/delay expression)
  (make-delay expression))

(define (output/unassigned-test name)
  (make-unassigned? name))

(define (output/unassigned)
  (make-unassigned-reference-trap))

(define (output/unspecific)
  unspecific)

(define (output/let names values body)
  (output/combination (output/named-lambda lambda-tag:let names body) values))

(define (output/letrec names values body)
  (output/let '() '()
	      (output/body '()
			   (make-sequence
			    (append! (map make-definition names values)
				     (list body))))))

(define (output/body declarations body)
  (scan-defines (let ((declarations (apply append declarations)))
		  (if (pair? declarations)
		      (make-sequence
		       (list (make-block-declaration declarations)
			     body))
		      body))
		make-open-block))

(define (output/definition name value)
  (make-definition name value))

(define (output/top-level-sequence declarations expressions)
  (let ((declarations (apply append declarations))
	(make-open-block
	 (lambda (expressions)
	   (scan-defines (make-sequence expressions) make-open-block))))
    (if (pair? declarations)
	(if (pair? expressions)
	    (make-open-block
	     (cons (make-block-declaration declarations)
		   expressions))
	    (make-block-declaration declarations))
	(if (pair? expressions)
	    (if (pair? (cdr expressions))
		(make-open-block expressions)
		(car expressions))
	    (output/unspecific)))))

(define (output/the-environment)
  (make-the-environment))

(define (output/access-reference name environment)
  (make-access environment name))

(define (output/access-assignment name environment value)
  (make-combination lexical-assignment (list environment name value)))

(define (output/local-declare declarations body)
  (make-declaration declarations body))

(define lambda-tag:unnamed
  ((ucode-primitive string->symbol) "#[unnamed-procedure]"))

(define lambda-tag:let
  ((ucode-primitive string->symbol) "#[let-procedure]"))

(define lambda-tag:fluid-let
  ((ucode-primitive string->symbol) "#[fluid-let-procedure]"))

;;;; Declarations

(define (define-declaration name pattern mapper)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (cons pattern mapper))
	(begin
	  (set! known-declarations
		(cons (cons name (cons pattern mapper))
		      known-declarations))
	  unspecific))))

(define (process-declaration declaration
			     selector
			     map-identifier
			     ill-formed-declaration)
  (if (pair? declaration)
      (let ((entry (assq (car declaration) known-declarations)))
	(if (and entry (syntax-match? (cadr entry) (cdr declaration)))
	    ((cddr entry) declaration selector map-identifier)
	    (begin
	      (warn "Unknown declaration:" declaration)
	      declaration)))
      (ill-formed-declaration declaration selector)))

(define known-declarations '())

(for-each (lambda (keyword)
	    (define-declaration keyword '()
	      (lambda (declaration selector map-identifier)
		selector map-identifier
		declaration)))
	  '(AUTOMAGIC-INTEGRATIONS
	    NO-AUTOMAGIC-INTEGRATIONS
	    ETA-SUBSTITUTION
	    NO-ETA-SUBSTITUTION
	    OPEN-BLOCK-OPTIMIZATIONS
	    NO-OPEN-BLOCK-OPTIMIZATIONS))

(for-each (lambda (keyword)
	    (define-declaration keyword '(* IDENTIFIER)
	      (lambda (declaration selector map-identifier)
		(cons (car declaration)
		      (select-map map-identifier
				  (cdr declaration)
				  (selector/add-cdr selector))))))
	  ;; The names in USUAL-INTEGRATIONS are always global.
	  '(USUAL-INTEGRATIONS
	    INTEGRATE
	    INTEGRATE-OPERATOR
	    INTEGRATE-SAFELY
	    IGNORE))

(define-declaration 'INTEGRATE-EXTERNAL
  `(* ,(lambda (object)
	 (or (string? object)
	     (pathname? object))))
  (lambda (declaration selector map-identifier)
    selector map-identifier
    declaration))

(for-each
 (lambda (keyword)
   (define-declaration keyword '(DATUM)
     (lambda (declaration selector map-identifier)
       (list (car declaration)
	     (let loop
		 ((varset (cadr declaration))
		  (selector (selector/add-cadr selector)))
	       (cond ((syntax-match? '('SET * IDENTIFIER) varset)
		      (cons (car varset)
			    (select-map map-identifier
					(cdr varset)
					(selector/add-cdr selector))))
		     ((or (syntax-match? '('UNION * DATUM) varset)
			  (syntax-match? '('INTERSECTION * DATUM) varset)
			  (syntax-match? '('DIFFERENCE DATUM DATUM) varset))
		      (cons (car varset)
			    (select-map loop
					(cdr varset)
					(selector/add-cdr selector))))
		     (else varset)))))))
 '(CONSTANT
   IGNORE-ASSIGNMENT-TRAPS
   IGNORE-REFERENCE-TRAPS
   PURE-FUNCTION
   SIDE-EFFECT-FREE
   USUAL-DEFINITION
   UUO-LINK))

(define-declaration 'REPLACE-OPERATOR '(* (IDENTIFIER * (DATUM DATUM)))
  (lambda (declaration selector map-identifier)
    (cons (car declaration)
	  (select-map
	   (lambda (rule selector)
	     (cons (map-identifier (car rule) (selector/add-car selector))
		   (select-map
		    (lambda (clause selector)
		      (list (car clause)
			    (if (identifier? (cadr clause))
				(map-identifier (cadr clause)
						(selector/add-cadr selector))
				(cadr clause))))
		    (cdr rule))))
	   (cdr declaration)
	   (selector/add-cdr selector)))))

(define-declaration 'REDUCE-OPERATOR '(* (IDENTIFIER DATUM * DATUM))
  (lambda (declaration selector map-identifier)
    (cons (car declaration)
	  (select-map
	   (lambda (rule selector)
	     (cons* (map-identifier (car rule) (selector/add-car selector))
		    (if (identifier? (cadr rule))
			(map-identifier (cadr rule)
					(selector/add-cadr selector))
			(cadr rule))
		    (select-map
		     (lambda (clause selector)
		       (if (or (syntax-match? '('NULL-VALUE IDENTIFIER DATUM)
					      clause)
			       (syntax-match? '('SINGLETON IDENTIFIER)
					      clause)
			       (syntax-match? '('WRAPPER IDENTIFIER ? DATUM)
					      clause))
			   (cons* (car clause)
				  (map-identifier (cadr clause)
						  (selector/add-cadr selector))
				  (cddr clause))
			   clause))
		     (cddr rule)
		     (selector/add-cddr selector))))
	   (cdr declaration)
	   (selector/add-cdr selector)))))