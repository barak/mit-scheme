#| -*-Scheme-*-

$Id: sysmac.scm,v 14.3 1999/01/02 06:19:10 cph Exp $

Copyright (c) 1988, 1999 Massachusetts Institute of Technology

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
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; System Internal Syntax
;;; package: (runtime system-macros)

(declare (usual-integrations))

(define (initialize-package!)
  (set! syntax-table/system-internal (make-system-internal-syntax-table)))

(define syntax-table/system-internal)

(define (make-system-internal-syntax-table)
  (let ((table (make-syntax-table system-global-syntax-table)))
    (for-each (lambda (entry)
		(syntax-table-define table (car entry) (cadr entry)))
	      `((DEFINE-PRIMITIVES ,transform/define-primitives)
		(UCODE-PRIMITIVE ,transform/ucode-primitive)
		(UCODE-RETURN-ADDRESS ,transform/ucode-return-address)
		(UCODE-TYPE ,transform/ucode-type)))
    table))

(define transform/define-primitives
  (macro names
    `(BEGIN ,@(map (lambda (name)
		     (cond ((not (pair? name))
			    (primitive-definition name (list name)))
			   ((not (symbol? (cadr name)))
			    (primitive-definition (car name) name))
			   (else
			    (primitive-definition (car name) (cdr name)))))
		   names))))

(define (primitive-definition variable-name primitive-args)
  `(DEFINE-INTEGRABLE ,variable-name
     ,(apply make-primitive-procedure primitive-args)))

(define transform/ucode-type
  (macro arguments
    (apply microcode-type arguments)))

(define transform/ucode-primitive
  (macro arguments
    (apply make-primitive-procedure arguments)))

(define transform/ucode-return-address
  (macro arguments
    (make-return-address (apply microcode-return arguments))))