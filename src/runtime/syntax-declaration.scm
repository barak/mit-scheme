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

;;;; Declarations

(declare (usual-integrations))

(define (define-declaration name pattern mapper)
  (let ((entry (assq name known-declarations)))
    (if entry
	(set-cdr! entry (cons pattern mapper))
	(begin
	  (set! known-declarations
		(cons (cons name (cons pattern mapper))
		      known-declarations))
	  unspecific))))

(define (map-declaration-identifiers procedure declaration)
  (if (not (pair? declaration))
      (error "Ill-formed declaration:" declaration))
  (let ((entry (assq (car declaration) known-declarations)))
    (if (and entry (syntax-match? (cadr entry) (cdr declaration)))
	((cddr entry) declaration procedure)
	(begin
	  (warn "Unknown declaration:" declaration)
	  declaration))))

(define known-declarations '())

(for-each (lambda (keyword)
	    (define-declaration keyword '()
	      (lambda (declaration procedure)
		procedure
		declaration)))
	  '(AUTOMAGIC-INTEGRATIONS
	    NO-AUTOMAGIC-INTEGRATIONS
	    ETA-SUBSTITUTION
	    NO-ETA-SUBSTITUTION
	    OPEN-BLOCK-OPTIMIZATIONS
	    NO-OPEN-BLOCK-OPTIMIZATIONS))

(for-each (lambda (keyword)
	    (define-declaration keyword '(* IDENTIFIER)
	      (lambda (declaration procedure)
		(cons (car declaration)
		      (map procedure (cdr declaration))))))
	  ;; The names in USUAL-INTEGRATIONS are always global.
	  '(
	    USUAL-INTEGRATIONS
	    INTEGRATE
	    INTEGRATE-OPERATOR
	    INTEGRATE-SAFELY
	    IGNORE
	    TYPE-CHECKS
	    NO-TYPE-CHECKS
	    RANGE-CHECKS
	    NO-RANGE-CHECKS
	    ))

(define-declaration 'INTEGRATE-EXTERNAL
  `(* ,(lambda (object)
	 (or (string? object)
	     (pathname? object))))
  (lambda (declaration procedure)
    procedure
    declaration))

(for-each
 (lambda (keyword)
   (define-declaration keyword '(DATUM)
     (lambda (declaration procedure)
       (list (car declaration)
	     (let loop ((varset (cadr declaration)))
	       (cond ((syntax-match? '('SET * IDENTIFIER) varset)
		      (cons (car varset)
			    (map procedure (cdr varset))))
		     ((syntax-match?* '(('UNION * DATUM)
					('INTERSECTION * DATUM)
					('DIFFERENCE DATUM DATUM))
				      varset)
		      (cons (car varset)
			    (map loop (cdr varset))))
		     (else varset)))))))
 '(CONSTANT
   IGNORE-ASSIGNMENT-TRAPS
   IGNORE-REFERENCE-TRAPS
   PURE-FUNCTION
   SIDE-EFFECT-FREE
   USUAL-DEFINITION
   UUO-LINK))

(define-declaration 'REPLACE-OPERATOR '(* (IDENTIFIER * (DATUM DATUM)))
  (lambda (declaration procedure)
    (cons (car declaration)
	  (map (lambda (rule)
		 (cons (procedure (car rule))
		       (map (lambda (clause)
			      (list (car clause)
				    (if (identifier? (cadr clause))
					(procedure (cadr clause))
					(cadr clause))))
			    (cdr rule))))
	       (cdr declaration)))))

(define-declaration 'REDUCE-OPERATOR '(* (IDENTIFIER DATUM * DATUM))
  (lambda (declaration procedure)
    (cons (car declaration)
	  (map (lambda (rule)
		 (cons* (procedure (car rule))
			(if (identifier? (cadr rule))
			    (procedure (cadr rule))
			    (cadr rule))
			(map (lambda (clause)
			       (if (syntax-match?*
				    '(('NULL-VALUE IDENTIFIER DATUM)
				      ('SINGLETON IDENTIFIER)
				      ('WRAPPER IDENTIFIER ? DATUM))
				    clause)
				   (cons* (car clause)
					  (procedure (cadr clause))
					  (cddr clause))
				   clause))
			     (cddr rule))))
	       (cdr declaration)))))