#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/machines/i386/rgspcm.scm,v 1.1 1992/02/13 03:01:18 jinx Exp $
$MC68020-Header: rgspcm.scm,v 4.1 87/12/30 07:05:38 GMT cph Exp $

Copyright (c) 1992 Massachusetts Institute of Technology

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

;;;; RTL Generation: Special primitive combinations.  Intel i386 version.
;;; package: (compiler rtl-generator)

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
	 (cdr entry))))

(define special-primitive-handlers
  '())

(define (define-special-primitive/standard primitive)
  (define-special-primitive-handler primitive
    rtl:make-invocation:special-primitive))

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