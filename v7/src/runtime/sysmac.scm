#| -*-Scheme-*-

$Id: sysmac.scm,v 14.8 2002/02/03 03:38:57 cph Exp $

Copyright (c) 1988, 1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
|#

;;;; System Internal Syntax
;;; package: (runtime system-macros)

(declare (usual-integrations))

(define-syntax define-primitives
  (sc-macro-transformer
   (lambda (form environment)
     (let ((primitive-definition
	    (lambda (variable-name primitive-args)
	      `(DEFINE-INTEGRABLE ,(close-syntax variable-name environment)
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