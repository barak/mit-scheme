#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Syntaxer Output Interface
;;; package: (runtime syntax output)

(declare (usual-integrations))

(define (transformer-eval output environment)
  (eval output (syntactic-environment->environment environment)))

(define (output/variable name)
  (make-scode-variable name))

(define (output/constant datum)
  datum)

(define (output/assignment name value)
  (make-scode-assignment name value))

(define (output/top-level-definition name value)
  (make-scode-definition name
    (if (scode-lambda? value)
	(lambda-components* value
	  (lambda (name* required optional rest body)
	    (if (eq? name* lambda-tag:unnamed)
		(make-lambda* name required optional rest body)
		value)))
	value)))

(define (output/top-level-syntax-definition name value)
  (make-scode-definition name (make-macro-reference-trap-expression value)))

(define (output/conditional predicate consequent alternative)
  (make-scode-conditional predicate consequent alternative))

(define (output/disjunction predicate alternative)
  (make-scode-disjunction predicate alternative))

(define (output/sequence expressions)
  (make-scode-sequence expressions))

(define (output/combination operator operands)
  (make-scode-combination operator operands))

(define (output/lambda lambda-list body)
  (output/named-lambda lambda-tag:unnamed lambda-list body))

(define (output/named-lambda name lambda-list body)
  (call-with-values (lambda () (parse-mit-lambda-list lambda-list))
    (lambda (required optional rest)
      (make-lambda* name required optional rest body))))

(define (output/delay expression)
  (make-scode-delay expression))

(define (output/unassigned-test name)
  (make-scode-unassigned? name))

(define (output/unassigned)
  (make-unassigned-reference-trap))

(define (output/unspecific)
  unspecific)

(define (output/let names values body)
  (output/combination (output/named-lambda lambda-tag:let names body) values))

(define (output/letrec names values body)
  (let ((temps (map (lambda (name)
		      (string->uninterned-symbol
		       (string-append (symbol->string (identifier->symbol name))
				      "-value"))) names)))
    (output/let
     names (map (lambda (name) name (output/unassigned)) names)
     (make-scode-sequence
      (cons (output/let
	     temps values
	     (make-scode-sequence
	      (map (lambda (name temp)
		     (make-scode-assignment name (make-scode-variable temp)))
		   names
		   temps)))
	    (list
	     (let ((body (scan-defines body make-scode-open-block)))
	       (if (scode-open-block? body)
		   (output/let '() '() body)
		   body))))))))

(define (output/body declarations body)
  (scan-defines (let ((declarations (apply append declarations)))
		  (if (pair? declarations)
		      (make-scode-sequence
		       (list (make-block-declaration declarations)
			     body))
		      body))
		make-scode-open-block))

(define (output/definition name value)
  (make-scode-definition name value))

(define (output/top-level-sequence declarations expressions)
  (let ((declarations (apply append declarations))
	(make-scode-open-block
	 (lambda (expressions)
	   (scan-defines (make-scode-sequence expressions)
			 make-scode-open-block))))
    (if (pair? declarations)
	(make-scode-open-block
	 (cons (make-block-declaration declarations)
	       (if (pair? expressions)
		   expressions
		   (list (output/unspecific)))))
	(if (pair? expressions)
	    (if (pair? (cdr expressions))
		(make-scode-open-block expressions)
		(car expressions))
	    (output/unspecific)))))

(define (output/the-environment)
  (make-scode-the-environment))

(define (output/access-reference name environment)
  (make-scode-access environment name))

(define (output/access-assignment name environment value)
  (make-scode-combination (ucode-primitive lexical-assignment)
		    (list environment name value)))

(define (output/runtime-reference name)
  (output/access-reference name system-global-environment))

(define lambda-tag:unnamed '|#[unnamed-procedure]|)
(define lambda-tag:let '|#[let-procedure]|)
(define lambda-tag:fluid-let '|#[fluid-let-procedure]|)