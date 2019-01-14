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

;;;; Declarations
;;; package: (runtime syntax declaration)

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

(define (map-decl-ids procedure declaration)
  (if (not (pair? declaration))
      (error "Ill-formed declaration:" declaration))
  (let* ((declaration
	  ;++ This is a kludge -- rather than strip syntactic closures,
	  ;++ it should be aware of the environment.
	  (if (symbol? (car declaration))
	      declaration
	      (cons (strip-syntactic-closures (car declaration))
		    (cdr declaration))))
	 (entry (assq (car declaration) known-declarations)))
    (if (and entry (syntax-match? (cadr entry) (cdr declaration)))
	((cddr entry) procedure declaration biselector:cr)
	(begin
	  (warn "Unknown declaration:" declaration)
	  declaration))))

(define (map+ procedure items selector)
  (map procedure
       items
       (biselect-list-elts items selector)))

(define known-declarations '())

(for-each (lambda (keyword)
	    (define-declaration keyword '(* identifier)
	      (lambda (procedure declaration selector)
		(cons (car declaration)
		      (map+ procedure
			    (cdr declaration)
			    (biselect-cdr selector))))))
	  ;; The names in USUAL-INTEGRATIONS are always global.
	  '(
	    usual-integrations
	    ignorable
	    ignore
	    integrate
	    integrate-operator
	    integrate-safely
	    type-checks
	    no-type-checks
	    range-checks
	    no-range-checks
	    ))

(define-declaration 'integrate-external
  `(* ,(lambda (object)
	 (or (string? object)
	     (pathname? object))))
  (lambda (procedure declaration selector)
    (declare (ignore procedure selector))
    declaration))

(define-declaration 'target-metadata
  `(* (symbol * datum))
  (lambda (procedure declaration selector)
    (declare (ignore procedure selector))
    declaration))

(for-each
 (lambda (keyword)
   (define-declaration keyword '(datum)
     (lambda (procedure declaration selector)
       (list (car declaration)
	     (let loop
		 ((varset (cadr declaration))
		  (selector (biselect-cadr selector)))
	       (cond ((syntax-match? '('set * identifier) varset)
		      (cons (car varset)
			    (map+ procedure
				  (cdr varset)
				  (biselect-cdr selector))))
		     ((syntax-match?* '(('union * datum)
					('intersection * datum)
					('difference datum datum))
				      varset)
		      (cons (car varset)
			    (map+ loop
				  (cdr varset)
				  (biselect-cdr selector))))
		     (else varset)))))))
 '(constant
   ignore-assignment-traps
   ignore-reference-traps
   pure-function
   side-effect-free
   usual-definition
   uuo-link))

(define-declaration 'replace-operator '(* (identifier * (datum datum)))
  (lambda (procedure declaration selector)
    (cons (car declaration)
	  (map+ (lambda (rule selector)
		  (cons (procedure (car rule) (biselect-car selector))
			(map+ (lambda (clause selector)
				(list (car clause)
				      (if (identifier? (cadr clause))
					  (procedure
					   (cadr clause)
					   (biselect-cadr selector))
					  (cadr clause))))
			      (cdr rule)
			      (biselect-cdr selector))))
		(cdr declaration)
		(biselect-cdr selector)))))

(define-declaration 'reduce-operator '(* (identifier datum * datum))
  (lambda (procedure declaration selector)
    (cons (car declaration)
	  (map+ (lambda (rule selector)
		  (cons* (procedure (car rule) (biselect-car selector))
			 (if (identifier? (cadr rule))
			     (procedure (cadr rule) (biselect-cadr selector))
			     (cadr rule))
			 (map+ (lambda (clause selector)
				 (if (syntax-match?*
				      '(('null-value identifier datum)
					('singleton identifier)
					('wrapper identifier ? datum))
				      clause)
				     (cons* (car clause)
					    (procedure (cadr clause)
						       (biselect-cadr selector))
					    (cddr clause))
				     clause))
			       (cddr rule)
			       (biselect-cddr selector))))
		(cdr declaration)
		(biselect-cdr selector)))))