#| -*-Scheme-*-

$Id: lambdx.scm,v 14.13 2007/01/05 15:33:09 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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

;;;; Alternative Components for Lambda
;;; package: (runtime alternative-lambda)

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
		(append required optional (if rest (list rest) '()))
		body))))

(define-structure (lambda-pattern (conc-name lambda-pattern/))
  (name #f read-only #t)
  (required #f read-only #t)
  (optional #f read-only #t)
  (rest #f read-only #t))

(define (make-lambda** pattern bound body)

  (define (split pattern bound receiver)
    (if (pair? pattern)
	(split (cdr pattern) (cdr bound)
	  (lambda (copy tail)
	    (receiver (cons (car bound) copy)
		      tail)))
	(receiver '() bound)))

  (split (lambda-pattern/required pattern) bound
    (lambda (required tail)
      (split (lambda-pattern/optional pattern) tail
	(lambda (optional rest)
	  (make-lambda* (lambda-pattern/name pattern)
			required
			optional
			(if (pair? rest) (car rest) #f)
			body))))))