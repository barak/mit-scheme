#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/rtlbase/rtlty1.scm,v 4.12 1988/11/01 04:53:11 jinx Exp $

Copyright (c) 1987, 1988 Massachusetts Institute of Technology

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

(define-rtl-expression char->ascii rtl: expression)
(define-rtl-expression byte-offset rtl: register number)
(define-rtl-expression register % number)
(define-rtl-expression object->address rtl: expression)
(define-rtl-expression object->datum rtl: expression)
(define-rtl-expression object->type rtl: expression)
(define-rtl-expression object->fixnum rtl: expression)
(define-rtl-expression object->unsigned-fixnum rtl: expression)
(define-rtl-expression fixnum->object rtl: expression)
(define-rtl-expression fixnum->address rtl: expression)
(define-rtl-expression address->fixnum rtl: expression)
(define-rtl-expression offset rtl: register number)
(define-rtl-expression pre-increment rtl: register number)
(define-rtl-expression post-increment rtl: register number)

(define-rtl-expression cons-pointer rtl: type datum)
(define-rtl-expression constant % value)
(define-rtl-expression assignment-cache rtl: name)
(define-rtl-expression variable-cache rtl: name)
(define-rtl-expression entry:continuation rtl: continuation)
(define-rtl-expression entry:procedure rtl: procedure)
(define-rtl-expression offset-address rtl: register number)
(define-rtl-expression unassigned rtl:)

(define-rtl-expression fixnum-1-arg rtl: operator operand)
(define-rtl-expression fixnum-2-args rtl: operator operand-1 operand-2)

(define-rtl-predicate fixnum-pred-1-arg % predicate operand)
(define-rtl-predicate fixnum-pred-2-args % predicate operand-1 operand-2)
(define-rtl-expression generic-unary rtl: operator operand)
(define-rtl-expression generic-binary rtl: operator operand-1 operand-2)

(define-rtl-predicate eq-test % expression-1 expression-2)
(define-rtl-predicate true-test % expression)
(define-rtl-predicate type-test % expression type)
(define-rtl-predicate unassigned-test % expression)

(define-rtl-predicate overflow-test rtl:)

(define-rtl-statement assign % address expression)

(define-rtl-statement pop-return rtl:)

(define-rtl-statement continuation-entry rtl: continuation)
(define-rtl-statement continuation-header rtl: continuation)
(define-rtl-statement ic-procedure-header rtl: procedure)
(define-rtl-statement open-procedure-header rtl: procedure)
(define-rtl-statement procedure-header rtl: procedure min max)
(define-rtl-statement closure-header rtl: procedure)

(define-rtl-statement cons-closure rtl: procedure min max size)

(define-rtl-statement interpreter-call:access % environment name)
(define-rtl-statement interpreter-call:define % environment name value)
(define-rtl-statement interpreter-call:lookup % environment name safe?)
(define-rtl-statement interpreter-call:set! % environment name value)
(define-rtl-statement interpreter-call:unassigned? % environment name)
(define-rtl-statement interpreter-call:unbound? % environment name)

(define-rtl-statement interpreter-call:cache-assignment % name value)
(define-rtl-statement interpreter-call:cache-reference % name safe?)
(define-rtl-statement interpreter-call:cache-unassigned? % name)

(define-rtl-statement invocation:apply rtl: pushed continuation)
(define-rtl-statement invocation:jump rtl: pushed continuation procedure)
(define-rtl-statement invocation:computed-jump rtl: pushed continuation)
(define-rtl-statement invocation:lexpr rtl: pushed continuation procedure)
(define-rtl-statement invocation:computed-lexpr rtl: pushed continuation)
(define-rtl-statement invocation:uuo-link rtl: pushed continuation name)
(define-rtl-statement invocation:primitive rtl: pushed continuation procedure)
(define-rtl-statement invocation:special-primitive rtl: pushed continuation
  procedure)
(define-rtl-statement invocation:cache-reference rtl: pushed continuation name)
(define-rtl-statement invocation:lookup rtl: pushed continuation environment
  name)

(define-rtl-statement invocation-prefix:move-frame-up rtl: frame-size locative)
(define-rtl-statement invocation-prefix:dynamic-link rtl: frame-size locative
  register)