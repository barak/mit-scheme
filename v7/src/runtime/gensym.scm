#| -*-Scheme-*-

$Id: gensym.scm,v 14.4 1994/06/19 18:27:10 cph Exp $

Copyright (c) 1988-94 Massachusetts Institute of Technology

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

;;;; Symbol Generation
;;; package: (runtime gensym)

(declare (usual-integrations))

(define (generate-uninterned-symbol #!optional argument)
  (let ((prefix
	 (cond ((or (default-object? argument) (not argument))
		name-prefix)
	       ((string? argument)
		argument)
	       ((symbol? argument)
		(symbol->string argument))
	       ((exact-nonnegative-integer? argument)
		(set! name-counter argument)
		name-prefix)
	       (else
		(error:wrong-type-argument argument "symbol or integer"
					   'GENERATE-UNINTERNED-SYMBOL)))))
    (string->uninterned-symbol
     (string-append prefix
		    (number->string
		     (let ((result name-counter))
		       (set! name-counter (1+ name-counter))
		       result))))))

(define name-counter)
(define name-prefix)

(define (initialize-package!)
  (set! name-counter 0)
  (set! name-prefix "G")
  unspecific)