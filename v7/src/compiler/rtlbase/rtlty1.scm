#| -*-Scheme-*-

$Id: rtlty1.scm,v 4.25 2003/02/14 18:28:08 cph Exp $

Copyright (c) 1987-1999 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Register Transfer Language Type Definitions
;;; package: (compiler)

(declare (usual-integrations))

;;; These three lists will be filled in by the type definitions that
;;; follow.  See those macros for details.
(define rtl:expression-types '())
(define rtl:statement-types '())
(define rtl:predicate-types '())

(define-rtl-expression register % number)

;;; Scheme object
(define-rtl-expression constant % value)

;;; Memory references that return Scheme objects
(define-rtl-expression offset rtl: base offset)
(define-rtl-expression pre-increment rtl: register number)
(define-rtl-expression post-increment rtl: register number)

;;; Memory reference that returns ASCII integer
(define-rtl-expression byte-offset rtl: base offset)
;;; Memory reference that returns a floating-point number
(define-rtl-expression float-offset rtl: base offset)

;;; Generic arithmetic operations on Scheme number objects
;;; (define-rtl-expression generic-unary rtl: operator operand)
;;; (define-rtl-expression generic-binary rtl: operator operand-1 operand-2)

;;; Code addresses
(define-rtl-expression entry:continuation rtl: continuation)
(define-rtl-expression entry:procedure rtl: procedure)

;;; Allocating a closure object (returns its address)
(define-rtl-expression cons-closure rtl: entry min max size)
;;; Allocating a multi-closure object
;;; (returns the address of first entry point)
(define-rtl-expression cons-multiclosure rtl: nentries size entries)

;;; Cache addresses
(define-rtl-expression assignment-cache rtl: name)
(define-rtl-expression variable-cache rtl: name)

;;; Get the address of a Scheme object
(define-rtl-expression object->address rtl: expression)

;;; Convert between a datum and an address
;;; (define-rtl-expression datum->address rtl: expression)
;;; (define-rtl-expression address->datum rtl: expression)

;;; Add a constant offset to an address
(define-rtl-expression offset-address rtl: base offset)
(define-rtl-expression byte-offset-address rtl: base offset)
(define-rtl-expression float-offset-address rtl: base offset)

;;; A machine constant (an integer, usually unsigned)
(define-rtl-expression machine-constant rtl: value)

;;; Destructuring Scheme objects
(define-rtl-expression object->datum rtl: expression)
(define-rtl-expression object->type rtl: expression)
(define-rtl-expression cons-pointer rtl: type datum)
(define-rtl-expression cons-non-pointer rtl: type datum)

;;; Convert a character object to an ASCII machine integer
(define-rtl-expression char->ascii rtl: expression)

;;; Conversion between fixnum objects and machine integers
(define-rtl-expression object->fixnum rtl: expression)
(define-rtl-expression object->unsigned-fixnum rtl: expression)
(define-rtl-expression fixnum->object rtl: expression)

;;; Conversion between machine integers and addresses
(define-rtl-expression fixnum->address rtl: expression)
(define-rtl-expression address->fixnum rtl: expression)

;;; Machine integer arithmetic operations
(define-rtl-expression fixnum-1-arg rtl:
  operator operand overflow?)
(define-rtl-expression fixnum-2-args rtl:
  operator operand-1 operand-2 overflow?)

;;; Conversion between flonums and machine floats
(define-rtl-expression float->object rtl: expression)
(define-rtl-expression object->float rtl: expression)

;;; Floating-point arithmetic operations
(define-rtl-expression flonum-1-arg rtl:
  operator operand overflow?)
(define-rtl-expression flonum-2-args rtl:
  operator operand-1 operand-2 overflow?)

(define-rtl-predicate fixnum-pred-1-arg %
  predicate operand)
(define-rtl-predicate fixnum-pred-2-args %
  predicate operand-1 operand-2)

(define-rtl-predicate flonum-pred-1-arg %
  predicate operand)
(define-rtl-predicate flonum-pred-2-args %
  predicate operand-1 operand-2)

(define-rtl-predicate eq-test % expression-1 expression-2)
(define-rtl-predicate type-test % expression type)

;; General predicates
(define-rtl-predicate pred-1-arg % predicate operand)
(define-rtl-predicate pred-2-args % predicate operand-1 operand-2)

(define-rtl-predicate overflow-test rtl:)

(define-rtl-statement assign % address expression)

(define-rtl-statement pop-return rtl:)

(define-rtl-statement continuation-entry rtl: continuation)
(define-rtl-statement continuation-header rtl: continuation)
(define-rtl-statement ic-procedure-header rtl: procedure)
(define-rtl-statement open-procedure-header rtl: procedure)
(define-rtl-statement procedure-header rtl: procedure min max)
(define-rtl-statement closure-header rtl: procedure nentries entry)

(define-rtl-statement interpreter-call:access %
  continuation environment name)
(define-rtl-statement interpreter-call:define %
  continuation environment name value)
(define-rtl-statement interpreter-call:lookup %
  continuation environment name safe?)
(define-rtl-statement interpreter-call:set! %
  continuation environment name value)
(define-rtl-statement interpreter-call:unassigned? %
  continuation environment name)
(define-rtl-statement interpreter-call:unbound? %
  continuation environment name)

(define-rtl-statement interpreter-call:cache-assignment %
  continuation name value)
(define-rtl-statement interpreter-call:cache-reference %
  continuation name safe?)
(define-rtl-statement interpreter-call:cache-unassigned? %
  continuation name)

(define-rtl-statement invocation:apply rtl:
  pushed continuation)
(define-rtl-statement invocation:jump rtl:
  pushed continuation procedure)
(define-rtl-statement invocation:computed-jump rtl:
  pushed continuation)
(define-rtl-statement invocation:lexpr rtl:
  pushed continuation procedure)
(define-rtl-statement invocation:computed-lexpr rtl:
  pushed continuation)
(define-rtl-statement invocation:uuo-link rtl:
  pushed continuation name)
(define-rtl-statement invocation:global-link rtl:
  pushed continuation name)
(define-rtl-statement invocation:primitive rtl:
  pushed continuation procedure)
(define-rtl-statement invocation:special-primitive rtl:
  pushed continuation procedure)
(define-rtl-statement invocation:cache-reference rtl:
  pushed continuation name)
(define-rtl-statement invocation:lookup rtl:
  pushed continuation environment name)

(define-rtl-statement invocation-prefix:move-frame-up rtl:
  frame-size locative)
(define-rtl-statement invocation-prefix:dynamic-link rtl:
  frame-size locative register)