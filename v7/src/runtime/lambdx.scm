#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/lambdx.scm,v 14.3 1990/09/11 22:57:36 cph Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Alternative Components for Lambda
;;; package: ()

(declare (usual-integrations))

(define (make-lambda* name required optional rest body)
  (scan-defines body
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
		(append required optional (if (null? rest) '() (list rest)))
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
			(if (null? rest) rest (car rest))
			body))))))