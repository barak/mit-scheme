#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlty1.scm,v 1.12 1987/07/03 18:56:20 cph Exp $

Copyright (c) 1987 Massachusetts Institute of Technology

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

;;;; Register Transfer Language Type Definitions

(declare (usual-integrations))

(define-rtl-expression register % number)
(define-rtl-expression object->address rtl: register)
(define-rtl-expression object->datum rtl: register)
(define-rtl-expression object->type rtl: register)
(define-rtl-expression offset rtl: register number)
(define-rtl-expression pre-increment rtl: register number)
(define-rtl-expression post-increment rtl: register number)

(define-rtl-expression cons-pointer rtl: type datum)
(define-rtl-expression constant % value)
(define-rtl-expression variable-cache rtl: name)
(define-rtl-expression entry:continuation % continuation)
(define-rtl-expression entry:procedure % procedure)
(define-rtl-expression offset-address rtl: register number)
(define-rtl-expression unassigned rtl:)

(define-rtl-predicate eq-test % expression-1 expression-2)
(define-rtl-predicate true-test % expression)
(define-rtl-predicate type-test % expression type)
(define-rtl-predicate unassigned-test % expression)

(define-rtl-statement assign % address expression)
(define-rtl-statement continuation-heap-check % continuation)
(define-rtl-statement procedure-heap-check % procedure)
(define-rtl-statement return rtl:)
(define-rtl-statement setup-lexpr % procedure)

(define-rtl-statement interpreter-call:access % environment name)
(define-rtl-statement interpreter-call:cache-assignment % name value)
(define-rtl-statement interpreter-call:cache-reference % name safe?)
(define-rtl-statement interpreter-call:cache-unassigned? % name)
(define-rtl-statement interpreter-call:define % environment name value)
(define-rtl-statement interpreter-call:enclose rtl: size)
(define-rtl-statement interpreter-call:lookup % environment name safe?)
(define-rtl-statement interpreter-call:set! % environment name value)
(define-rtl-statement interpreter-call:unassigned? % environment name)
(define-rtl-statement interpreter-call:unbound? % environment name)

(define-rtl-statement invocation:apply % pushed prefix continuation)
(define-rtl-statement invocation:cache-reference % pushed prefix continuation
  name)
(define-rtl-statement invocation:jump % pushed prefix continuation procedure)
(define-rtl-statement invocation:lexpr % pushed prefix continuation procedure)
(define-rtl-statement invocation:lookup % pushed prefix continuation
  environment name)
(define-rtl-statement invocation:primitive % pushed prefix continuation
  procedure)
(define-rtl-statement invocation:uuo-link % pushed prefix continuation name)

(define-rtl-statement message-sender:value rtl: size)
(define-rtl-statement message-receiver:closure rtl: size)
(define-rtl-statement message-receiver:stack rtl: size)
(define-rtl-statement message-receiver:subproblem % continuation)