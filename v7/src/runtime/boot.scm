#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/boot.scm,v 14.2 1988/08/05 20:16:26 cph Rel $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; Boot Time Definitions
;;; package: ()

(declare (usual-integrations))

(define (unparser/standard-method name #!optional unparser)
  (lambda (state object)
    (if (not (unparser-state? state)) (error "Bad unparser state" state))
    (let ((port (unparser-state/port state)))
      (write-string "#[" port)
      (if (string? name)
	  (write-string name port)
	  (unparse-object state name))
      (write-char #\Space port)
      (write-string (number->string (hash object)) port)
      (if (and (not (default-object? unparser)) unparser)
	  (begin (write-char #\Space port)
		 (unparser state object)))
      (write-char #\] port))))
(define-integrable interrupt-bit/stack     #x0001)
(define-integrable interrupt-bit/global-gc #x0002)
(define-integrable interrupt-bit/gc        #x0004)
(define-integrable interrupt-bit/global-1  #x0008)
(define-integrable interrupt-bit/kbd       #x0010)
(define-integrable interrupt-bit/global-2  #x0020)
(define-integrable interrupt-bit/timer     #x0040)
(define-integrable interrupt-bit/global-3  #x0080)
(define-integrable interrupt-bit/suspend   #x0100)

;; GC & stack overflow only
(define-integrable interrupt-mask/gc-ok    #x0007)

;; Absolutely everything off
(define-integrable interrupt-mask/none     #x0000)

;; Normal: all enabled
(define-integrable interrupt-mask/all      #xFFFF)

(define (with-absolutely-no-interrupts thunk)
  (with-interrupt-mask interrupt-mask/none
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define (without-interrupts thunk)
  (with-interrupt-mask interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define-primitives
  (object-pure? pure?)
  (object-constant? constant?)
  get-next-constant)