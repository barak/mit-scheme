;;; -*-Scheme-*-
;;;
;;; $Id: synchk.scm,v 1.1 2001/06/26 18:03:24 cph Exp $
;;;
;;; Copyright (c) 1989 Massachusetts Institute of Technology
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

;;;; Syntax Checking
;;; written by Alan Bawden
;;; modified by Chris Hanson

(declare (usual-integrations))

(define (syntax-match? pattern object)
  (let ((match-error (lambda () (error "ill-formed pattern" pattern))))
    (cond ((symbol? pattern)
	   (case pattern
	     ((IDENTIFIER) (symbol? object))
	     ((ANYTHING EXPRESSION FORM) true)
	     ((BVL) (lambda-pattern? object))
	     (else (match-error))))
	  ((pair? pattern)
	   (case (car pattern)
	     ((QUOTE)
	      (if (and (pair? (cdr pattern))
		       (null? (cddr pattern)))
		  (eqv? (cadr pattern) object)
		  (match-error)))
	     ((*)
	      (if (pair? (cdr pattern))
		  (let ((head (cadr pattern))
			(tail (cddr pattern)))
		    (let loop ((object object))
		      (or (and (pair? object)
			       (syntax-match? head (car object))
			       (loop (cdr object)))
			  (syntax-match? tail object))))
		  (match-error)))
	     ((+)
	      (if (pair? (cdr pattern))
		  (let ((head (cadr pattern))
			(tail (cddr pattern)))
		    (and (pair? object)
			 (syntax-match? head (car object))
			 (let loop ((object (cdr object)))
			   (or (and (pair? object)
				    (syntax-match? head (car object))
				    (loop (cdr object)))
			       (syntax-match? tail object)))))
		  (match-error)))
	     ((?)
	      (if (pair? (cdr pattern))
		  (or (and (syntax-match? (cadr pattern) (car object))
			   (syntax-match? (cddr pattern) (cdr object)))
		      (syntax-match? (cddr pattern) object))
		  (match-error)))
	     (else
	      (and (pair? object)
		   (syntax-match? (car pattern) (car object))
		   (syntax-match? (cdr pattern) (cdr object))))))
	  (else
	   (eqv? pattern object)))))