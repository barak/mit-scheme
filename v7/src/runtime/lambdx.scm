#| -*-Scheme-*-

$Id: lambdx.scm,v 14.9 2000/10/14 00:56:20 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

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

;;;; Alternative Components for Lambda
;;; package: ()

(declare (usual-integrations))

(define (make-lambda* name required optional rest body)
  (scan-defines
   body
   (lambda (auxiliary declarations body*)
     (make-lambda name required optional rest auxiliary declarations body*))))

(define (lambda-components* *lambda receiver)
  (lambda-components *lambda
    (lambda (name required optional rest auxiliary declarations body)
      (receiver name required optional rest
		(make-open-block auxiliary declarations body)))))

(define (lambda-components** *lambda receiver)
  (lambda-components* *lambda
    (lambda (name required optional rest body)
      (receiver (make-lambda-pattern name required optional rest)
		(append required optional (if (false? rest) '() (list rest)))
		body))))

(define-structure (lambda-pattern (conc-name lambda-pattern/))
  (name false read-only true)
  (required false read-only true)
  (optional false read-only true)
  (rest false read-only true))

(define (make-lambda** pattern bound body)

  (define (split pattern bound receiver)
    (cond ((null? pattern)
	   (receiver '() bound))
	  (else
	   (split (cdr pattern) (cdr bound)
	     (lambda (copy tail)
	       (receiver (cons (car bound) copy)
			 tail))))))

  (split (lambda-pattern/required pattern) bound
    (lambda (required tail)
      (split (lambda-pattern/optional pattern) tail
	(lambda (optional rest)
	  (make-lambda* (lambda-pattern/name pattern)
			required
			optional
			(if (null? rest) #F (car rest))
			body))))))