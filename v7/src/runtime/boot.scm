#| -*-Scheme-*-

$Id: boot.scm,v 14.11 1996/05/17 17:47:59 cph Exp $

Copyright (c) 1988-96 Massachusetts Institute of Technology

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

(define standard-unparser-method)
(define unparser/standard-method)
(let ((make-method
       (lambda (name unparser)
	 (lambda (state object)
	   (let ((port (unparser-state/port state))
		 (hash-string (number->string (hash object))))
	     (if *unparse-with-maximum-readability?*
		 (begin
		   (write-string "#@" port)
		   (write-string hash-string port))
		 (begin
		   (write-string "#[" port)
		   (if (string? name)
		       (write-string name port)
		       (with-current-unparser-state state
			 (lambda (port)
			   (write name port))))
		   (write-char #\space port)
		   (write-string hash-string port)
		   (if unparser (unparser state object))
		   (write-char #\] port))))))))
  (set! standard-unparser-method
	(lambda (name unparser)
	  (make-method name
		       (and unparser
			    (lambda (state object)
			      (with-current-unparser-state state
				(lambda (port)
				  (unparser object port))))))))
  (set! unparser/standard-method
	(lambda (name #!optional unparser)
	  (make-method name
		       (and (not (default-object? unparser))
			    unparser
			    (lambda (state object)
			      (unparse-char state #\space)
			      (unparser state object)))))))

(define (unparser-method? object)
  (and (procedure? object)
       (procedure-arity-valid? object 2)))

(define-integrable interrupt-bit/stack     #x0001)
(define-integrable interrupt-bit/global-gc #x0002)
(define-integrable interrupt-bit/gc        #x0004)
(define-integrable interrupt-bit/global-1  #x0008)
(define-integrable interrupt-bit/kbd       #x0010)
(define-integrable interrupt-bit/after-gc  #x0020)
(define-integrable interrupt-bit/timer     #x0040)
(define-integrable interrupt-bit/global-3  #x0080)
(define-integrable interrupt-bit/suspend   #x0100)
;; Interrupt bits #x0200 through #x4000 inclusive are reserved
;; for the Descartes PC sampler.

;; GC & stack overflow only
(define-integrable interrupt-mask/gc-ok    #x0007)

;; GC, stack overflow, and keyboard only
(define-integrable interrupt-mask/no-background #x0017)

;; GC, stack overflow, and timer only
(define-integrable interrupt-mask/timer-ok #x0047)

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

(define (without-background-interrupts thunk)
  (with-interrupt-mask interrupt-mask/no-background
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define-primitives
  (object-pure? pure?)
  (object-constant? constant?)
  get-next-constant)

(define-integrable (future? object)
  ((ucode-primitive object-type? 2) (ucode-type future) object))