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

;;;; Syntax constructors
;;; package: (runtime syntax constructor)

(declare (usual-integrations))

(define (scons-rule patterns procedure)
  (spar-call-with-values
      (lambda (close . args)
	(close-part close (apply procedure args)))
    (spar-subform)
    (spar-push spar-arg:close)
    (top-level-patterns->spar patterns)))

(define-record-type <open-expr>
    (make-open-expr procedure)
    open-expr?
  (procedure open-expr-procedure))

(define (close-part close part)
  (if (open-expr? part)
      ((open-expr-procedure part) close)
      part))

(define (close-parts close parts)
  (map (lambda (part) (close-part close part))
       parts))

(define (scons-close identifier)
  (guarantee identifier? identifier 'scons-close)
  (make-open-expr (lambda (close) (close identifier))))

(define (scons-and . exprs)
  (make-open-expr
   (lambda (close)
     (let ((closed (close-parts close (delq #t exprs))))
       (case (length closed)
	 ((0) #t)
	 ((1) (car closed))
	 (else (cons (close 'and) closed)))))))

(define (scons-begin . exprs)
  (make-open-expr
   (lambda (close)
     (let ((closed (close-parts close (remove default-object? exprs))))
       (case (length closed)
	 ((1) (car closed))
	 (else (cons (close 'begin) closed)))))))

(define (scons-call operator . operands)
  (make-open-expr
   (lambda (close)
     (cons (close-part close operator)
	   (close-parts close operands)))))

(define (scons-declare . decls)
  (make-open-expr
   (lambda (close)
     (cons (close 'declare)
	   decls))))

(define (scons-define name value)
  (make-open-expr
   (lambda (close)
     (list (close 'define)
	   name
	   (close-part close value)))))

(define (scons-delay expr)
  (make-open-expr
   (lambda (close)
     (list (close 'delay)
	   (close-part close expr)))))

(define (scons-if predicate consequent alternative)
  (make-open-expr
   (lambda (close)
     (list (close 'if)
	   (close-part close predicate)
	   (close-part close consequent)
	   (close-part close alternative)))))

(define (scons-lambda bvl . body-forms)
  (make-open-expr
   (lambda (close)
     (cons* (close 'lambda)
	    bvl
	    (close-parts close body-forms)))))

(define (scons-named-lambda bvl . body-forms)
  (make-open-expr
   (lambda (close)
     (cons* (close 'named-lambda)
	    bvl
	    (close-parts close body-forms)))))

(define (scons-or . exprs)
  (make-open-expr
   (lambda (close)
     (let ((closed (close-parts close (delq #f exprs))))
       (case (length closed)
	 ((0) #f)
	 ((1) (car closed))
	 (else (cons (close 'or) closed)))))))

(define (scons-quote datum)
  (make-open-expr
   (lambda (close)
     (list (close 'quote) datum))))

(define (scons-quote-identifier id)
  (make-open-expr
   (lambda (close)
     (list (close 'quote-identifier) id))))

(define (scons-set! name #!optional value)
  (make-open-expr
   (lambda (close)
     (if (default-object? value)
	 (list (close 'set!) name)
	 (list (close 'set!)
	       name
	       (close-part close value))))))

(define (let-like keyword)
  (lambda (bindings . body-forms)
    (make-open-expr
     (lambda (close)
       (cons* (close keyword)
	      (close-bindings close bindings)
	      (close-parts close body-forms))))))

(define (close-bindings close bindings)
  (map (lambda (b)
	 (list (car b) (close-part close (cadr b))))
       bindings))

(define scons-let (let-like 'let))
(define scons-let-syntax (let-like 'let-syntax))
(define scons-letrec (let-like 'letrec))
(define scons-letrec* (let-like 'letrec*))

(define (scons-named-let name bindings . body-forms)
  (make-open-expr
   (lambda (close)
     (cons* (close 'let)
	    name
	    (close-bindings close bindings)
	    (close-parts close body-forms)))))