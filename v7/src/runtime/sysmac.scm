#| -*-Scheme-*-

$Id: sysmac.scm,v 14.5 2001/12/19 21:41:14 cph Exp $

Copyright (c) 1988, 1999, 2001 Massachusetts Institute of Technology

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

(define (initialize-package!)
  (let ((environment (->environment '(RUNTIME))))
    (set-environment-syntax-table! environment
				   (make-syntax-table (->environment '())))
    (for-each (lambda (entry)
		(syntax-table/define environment (car entry) (cadr entry)))
	      `((DEFINE-PRIMITIVES ,transform/define-primitives)
		(UCODE-PRIMITIVE ,transform/ucode-primitive)
		(UCODE-RETURN-ADDRESS ,transform/ucode-return-address)
		(UCODE-TYPE ,transform/ucode-type)))))

(define transform/define-primitives
  (let ((primitive-definition
	 (lambda (variable-name primitive-args)
	   `(DEFINE-INTEGRABLE ,variable-name
	      ,(apply make-primitive-procedure primitive-args)))))
    (lambda names
      `(BEGIN ,@(map (lambda (name)
		       (cond ((not (pair? name))
			      (primitive-definition name (list name)))
			     ((not (symbol? (cadr name)))
			      (primitive-definition (car name) name))
			     (else
			      (primitive-definition (car name) (cdr name)))))
		     names)))))

(define transform/ucode-type
  (lambda arguments
    (apply microcode-type arguments)))

(define transform/ucode-primitive
  (lambda arguments
    (apply make-primitive-procedure arguments)))

(define transform/ucode-return-address
  (lambda arguments
    (make-return-address (apply microcode-return arguments))))