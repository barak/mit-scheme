#| -*-Scheme-*-

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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

;;;; RTL Generation: Special primitive combinations.  Spectrum version.

(declare (usual-integrations))

(define (define-special-primitive-handler name handler)
  (let ((primitive (make-primitive-procedure name true)))
    (let ((entry (assq primitive special-primitive-handlers)))
      (if entry
	  (set-cdr! entry handler)
	  (set! special-primitive-handlers
		(cons (cons primitive handler)
		      special-primitive-handlers)))))
  name)

(define (special-primitive-handler primitive)
  (let ((entry (assq primitive special-primitive-handlers)))
    (and entry
	 ((cdr entry)))))

(define special-primitive-handlers
  '())

(define (define-special-primitive/standard primitive)
  (define-special-primitive-handler primitive
    (lambda ()
      rtl:make-invocation:special-primitive)))

(define (define-special-primitive/if-open-coding primitive)
  (define-special-primitive-handler primitive
    (lambda ()
      (and compiler:open-code-primitives?
	   rtl:make-invocation:special-primitive))))

(define-special-primitive/standard '&+)
(define-special-primitive/standard '&-)
(define-special-primitive/standard '&*)
(define-special-primitive/standard '&/)
(define-special-primitive/standard '&=)
(define-special-primitive/standard '&<)
(define-special-primitive/standard '&>)
(define-special-primitive/standard '1+)
(define-special-primitive/standard '-1+)
(define-special-primitive/standard 'zero?)
(define-special-primitive/standard 'positive?)
(define-special-primitive/standard 'negative?)
(define-special-primitive/standard 'quotient)
(define-special-primitive/standard 'remainder)
(define-special-primitive/if-open-coding 'vector-cons)
(define-special-primitive/if-open-coding 'string-allocate)
(define-special-primitive/if-open-coding 'floating-vector-cons)