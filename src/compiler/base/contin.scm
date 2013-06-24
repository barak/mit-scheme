#| -*-Scheme-*-

$Id: contin.scm,v 4.14 2008/01/30 20:01:42 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Continuation datatype

(declare (usual-integrations))

;;; Continuations are a subtype of procedures, whose `type' is
;;; something other than PROCEDURE.

(define (make-continuation block continuation type)
  continuation
  (let ((block (make-block block 'CONTINUATION)))
    (let ((required (list (make-value-variable block))))
      (set-block-bound-variables! block required)
      (make-procedure type block 'CONTINUATION required '() false '() '()
		      (snode->scfg (make-fg-noop))))))

(define-enumeration continuation-type
  (effect predicate procedure push register value))

(define-integrable (procedure-continuation? procedure)
  (not (eq? (procedure-type procedure) continuation-type/procedure)))

(define (rvalue/continuation? rvalue)
  (and (rvalue/procedure? rvalue)
       (procedure-continuation? rvalue)))

(define-integrable continuation/type procedure-type)
(define-integrable set-continuation/type! set-procedure-type!)
(define-integrable continuation/block procedure-block)
(define-integrable continuation/closing-block procedure-closing-block)
(define-integrable continuation/entry-node procedure-entry-node)
(define-integrable set-continuation/entry-node! set-procedure-entry-node!)
(define-integrable continuation/combinations procedure-original-rest)
(define-integrable set-continuation/combinations! set-procedure-original-rest!)
(define-integrable continuation/label procedure-label)
(define-integrable continuation/returns procedure-applications)
(define-integrable set-continuation/returns! set-procedure-applications!)
(define-integrable continuation/ever-known-operator?
  procedure-always-known-operator?)
(define-integrable continuation/offset procedure-closure-offset)
(define-integrable set-continuation/offset! set-procedure-closure-offset!)
(define-integrable continuation/passed-out? procedure-passed-out?)
(define-integrable set-continuation/passed-out?! set-procedure-passed-out?!)
(define-integrable continuation/debugging-info procedure-debugging-info)
(define-integrable set-continuation/debugging-info!
  set-procedure-debugging-info!)

(define (continuation/register continuation)
  (or (procedure-register continuation)
      (let ((register (rtl:make-pseudo-register)))
	(set-procedure-register! continuation register)
	register)))

(define-integrable (continuation/always-known-operator? continuation)
  (eq? (continuation/ever-known-operator? continuation) 'ALWAYS))

(define-integrable (continuation/parameter continuation)
  (car (procedure-original-required continuation)))

(define-integrable return-operator/subproblem? rvalue/procedure?)
(define-integrable return-operator/reduction? rvalue/reference?)
(define-integrable reduction-continuation/lvalue reference-lvalue)

(define (continuation/frame-size continuation)
  (let ((closing-block (continuation/closing-block continuation)))
    (+ (if (ic-block? closing-block) 1 0)
       (if (and (stack-block? closing-block)
		(stack-block/dynamic-link? closing-block))
	   1
	   0)
       (if (continuation/always-known-operator? continuation)
	   0
	   1))))

(define (uni-continuation? rvalue)
  (and (rvalue/procedure? rvalue)
       (procedure-arity-correct? rvalue 1)))

(define-integrable (uni-continuation/parameter continuation)
  (car (procedure-original-required continuation)))

(define (delete-continuation/combination! continuation combination)
  (let ((combinations
	 (delq! combination (continuation/combinations continuation))))
    (set-continuation/combinations! continuation combinations)
    (if (and (null? combinations)
	     (null? (continuation/returns continuation)))
	(set-procedure-always-known-operator?! continuation false))))