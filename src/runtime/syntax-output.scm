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

;;;; Syntaxer output interface
;;; package: (runtime syntax output)

(declare (usual-integrations))

(define (transformer-eval output environment)
  (eval output (senv->runtime environment)))

(define (output/variable name)
  (make-scode-variable name))

(define (output/constant datum)
  datum)

(define-record-type <quoted-identifier>
    (output/quoted-identifier identifier)
    quoted-identifier?
  (identifier quoted-identifier-identifier))

(define (output/assignment name value)
  (make-scode-assignment name value))

(define (output/definition name value)
  (make-scode-definition name
    (if (scode-lambda? value)
	(lambda-components* value
	  (lambda (name* required optional rest body)
	    (if (eq? name* scode-lambda-name:unnamed)
		(make-lambda* name required optional rest body)
		value)))
	value)))

(define (output/syntax-definition name value)
  (make-scode-definition name (make-macro-reference-trap-expression value)))

(define (output/top-level-syntax-expander procedure-name transformer)
  (output/combination (output/runtime-reference procedure-name)
		      (list transformer
			    (output/the-environment))))

(define (output/conditional predicate consequent alternative)
  (make-scode-conditional predicate consequent alternative))

(define (output/disjunction exprs)
  (reduce-right make-scode-disjunction '#f exprs))

(define (output/sequence exprs)
  (if (pair? exprs)
      (make-scode-sequence exprs)
      (output/unspecific)))

(define (output/combination operator operands)
  (make-scode-combination operator operands))

(define (output/lambda name lambda-list body)
  (receive (required optional rest) (parse-mit-lambda-list lambda-list)
    (make-lambda* name required optional rest body)))

(define (output/unassigned-test name)
  (make-scode-unassigned? name))

(define (output/unassigned)
  (make-unassigned-reference-trap))

(define (output/unspecific)
  unspecific)

(define (output/let names values body)
  (output/combination (output/lambda scode-lambda-name:let names body)
		      values))

(define (output/letrec names values body)
  (let ((temps
	 (map (lambda (name)
		(string->uninterned-symbol
		 (string-append (symbol->string (identifier->symbol name))
				"-value")))
	      names)))
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

(define (output/body exprs)
  (scan-defines (output/sequence exprs) make-scode-open-block))

(define (output/declaration text)
  (make-scode-block-declaration text))

(define (output/the-environment)
  (make-scode-the-environment))

(define (output/access-reference name environment)
  (make-scode-access environment name))

(define (output/access-assignment name environment value)
  (make-scode-combination (ucode-primitive lexical-assignment)
			  (list environment name value)))

(define (output/runtime-reference name)
  (output/access-reference name system-global-environment))