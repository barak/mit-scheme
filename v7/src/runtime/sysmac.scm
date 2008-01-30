#| -*-Scheme-*-

$Id: sysmac.scm,v 14.17 2008/01/30 20:02:36 cph Exp $

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

;;;; System Internal Syntax
;;; package: (runtime system-macros)

(declare (usual-integrations))

(define-syntax define-primitives
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (let ((primitive-definition
	    (lambda (variable-name primitive-args)
	      `(DEFINE-INTEGRABLE ,variable-name
		 ,(apply make-primitive-procedure primitive-args)))))
       `(BEGIN ,@(map (lambda (name)
			(cond ((not (pair? name))
			       (primitive-definition name (list name)))
			      ((not (symbol? (cadr name)))
			       (primitive-definition (car name) name))
			      (else
			       (primitive-definition (car name) (cdr name)))))
		      (cdr form)))))))

(define-syntax ucode-type
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply microcode-type (cdr form)))))

(define-syntax ucode-primitive
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (apply make-primitive-procedure (cdr form)))))

(define-syntax ucode-return-address
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (make-return-address (apply microcode-return (cdr form))))))

(define-syntax define-guarantee
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(SYMBOL EXPRESSION) (cdr form))
	 (let ((root (cadr form))
	       (desc (close-syntax (caddr form) environment)))
	   (let ((p-name (symbol root '?))
		 (g-name (symbol 'guarantee- root))
		 (e-name (symbol 'error:not- root)))
	     `(BEGIN
		(DEFINE (,g-name OBJECT #!OPTIONAL CALLER)
		  (IF (NOT (,(close-syntax p-name environment) OBJECT))
		      (,(close-syntax e-name environment) OBJECT CALLER)))
		(DEFINE (,e-name OBJECT #!OPTIONAL CALLER)
		  (ERROR:WRONG-TYPE-ARGUMENT OBJECT
					     ,desc
					     (IF (DEFAULT-OBJECT? CALLER)
						 #F
						 CALLER))))))
	 (ill-formed-syntax form)))))