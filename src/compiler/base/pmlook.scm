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

;;;; Very Simple Pattern Matcher: Lookup
;;; package: (compiler pattern-matcher/lookup)

(declare (usual-integrations))

;;; PATTERN-LOOKUP returns either #F or a thunk that is the result of
;;; the matching rule result expression.

(define (pattern-lookup matchers instance)
  (let loop ((matchers matchers))
    (and (pair? matchers)
	 (or ((car matchers) instance)
	     (loop (cdr matchers))))))

(define (pattern-lookup-1 pattern body instance)
  (let loop
      ((pattern pattern)
       (instance instance)
       (vars '())
       (vals '())
       (k (lambda (vars vals) vars (apply body vals))))
    (cond ((pattern-variable? pattern)
	   (let ((var (pattern-variable-name pattern)))
	     (let find-var ((vars* vars) (vals* vals))
	       (if (pair? vars*)
		   (if (eq? (car vars*) var)
		       (and (eqv? (car vals*) instance)
			    (k vars vals))
		       (find-var (cdr vars*) (cdr vals*)))
		   (k (cons var vars) (cons instance vals))))))
	  ((pair? pattern)
	   (and (pair? instance)
		(loop (car pattern)
		      (car instance)
		      vars
		      vals
		      (lambda (vars vals)
			(loop (cdr pattern)
			      (cdr instance)
			      vars
			      vals
			      k)))))
	  (else
	   (and (eqv? pattern instance)
		(k vars vals))))))

(define (pattern-variables pattern)
  (let loop ((pattern pattern) (vars '()) (k (lambda (vars) vars)))
    (cond ((pattern-variable? pattern)
	   (k (let ((var (pattern-variable-name pattern)))
		(if (memq var vars)
		    vars
		    (cons var vars)))))
	  ((pair? pattern)
	   (loop (car pattern)
		 vars
		 (lambda (vars) (loop (cdr pattern) vars k))))
	  (else (k vars)))))

(define-integrable (make-pattern-variable name)
  (cons pattern-variable-tag name))

(define (pattern-variable? object)
  (and (pair? object)
       (eq? (car object) pattern-variable-tag)))

(define pattern-variable-tag
  '|#[(compiler pattern-matcher/lookup)pattern-variable]|)

(define-integrable (pattern-variable-name var)
  (cdr var))