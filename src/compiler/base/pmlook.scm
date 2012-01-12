#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011 Massachusetts Institute of
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

;;; PATTERN-LOOKUP-1 returns either #f or the result of applying
;;; <body> to the values matched by the pattern.  The values
;;; are in reverse order of variable occurrence in the pattern,
;;; repeated occurrences of a pattern variable must be eqv?,
;;; and only the first occurrence is used.

(define (pattern-lookup-1 pattern body instance)
  (let ((binding-alist
	 (let loop ((pattern pattern)
		    (instance instance)
		    (binding-alist '()))
	   ;; Cheat:  we know pattern variables are pairs
	   (cond ((pair? pattern)
		  (cond ((eq? (car pattern) pattern-variable-tag)
			 (let ((var (pattern-variable-name pattern)))
			   (let ((entry (assq var binding-alist)))
			     (if entry
				 (and (eqv? (cdr entry) instance)
				      binding-alist)
				 (cons (cons var instance) binding-alist)))))
			((pair? instance)
			 (let ((binding-alist*
				(loop (car pattern) (car instance) binding-alist)))
			   (and binding-alist*
				(loop (cdr pattern) (cdr instance) binding-alist*))))
			(else #f)))
		 ((eqv? pattern instance) binding-alist)
		 (else #f)))))
    (and binding-alist
	 (apply body (map cdr binding-alist)))))

;;; PATTERN-LOOKUP-2 returns either #f or the result of applying
;;; <body> to the values matched by the pattern.  The values
;;; are in reverse order of variable occurrence in the pattern.
;;; There must be no repeated occurrences of a pattern variable.

(define (pattern-lookup-2 pattern body instance)
  (let ((value-list
	 (let loop ((pattern pattern)
		    (instance instance)
		    (value-list '()))
	   ;; Cheat:  we know pattern variables are pairs
	   (cond ((pair? pattern)
		  (cond ((eq? (car pattern) pattern-variable-tag)
			 (cons instance value-list))
			((pair? instance)
			 (let ((value-list*
				(loop (car pattern) (car instance) value-list)))
			   (and value-list*
				(loop (cdr pattern) (cdr instance) value-list*))))
			(else #f)))
		 ((eqv? pattern instance) value-list)
		 (else #f)))))
    (and value-list
	 (apply body value-list))))

;; Stub definition for the moment.
(define (generate-pattern-matcher pattern body environment)
  (error "GENERATE-PATTERN-MATCHER: Stub not yet implemented."))

(define (pattern-variables pattern)
  (let loop ((pattern pattern) (vars '()))
    (cond ((pair? pattern)
	   ;; Cheat:  we know pattern variables are pairs
	   (if (eq? (car pattern) pattern-variable-tag)
	       (let ((var (pattern-variable-name pattern)))
		 (if (memq var vars)
		     vars
		     (cons var vars)))
	       (if (pair? pattern)
		   (let ((vars1 (loop (car pattern) vars)))
		     (and vars1
			  (loop (cdr pattern) vars1)))
		   vars)))
	  (else vars))))

(define (pattern-contains-duplicates? pattern)
  (not (let loop ((pattern pattern)
		  (vars '()))
	 (if (pair? pattern)
	     ;; Cheat:  we know pattern variables are pairs
	     (if (eq? (car pattern) pattern-variable-tag)
		 (if (memq (pattern-variable-name pattern) vars)
		     #f			; found a duplicate
		     (cons (pattern-variable-name pattern) vars))
		 (let ((vars1 (loop (car pattern) vars)))
		   (and vars1
			(loop (cdr pattern) vars1))))
	     vars))))

(define-integrable (make-pattern-variable name)
  (cons pattern-variable-tag name))

(define (pattern-variable? object)
  (and (pair? object)
       (eq? (car object) pattern-variable-tag)))

(define pattern-variable-tag
  '|#[(compiler pattern-matcher/lookup)pattern-variable]|)

(define-integrable (pattern-variable-name var)
  (cdr var))