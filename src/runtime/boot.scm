#| -*-Scheme-*-

$Id: boot.scm,v 14.21 2005/07/31 02:54:29 cph Exp $

Copyright 1986,1987,1988,1989,1990,1992 Massachusetts Institute of Technology
Copyright 1993,1996,2001,2004,2005 Massachusetts Institute of Technology

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

;;;; Boot Time Definitions
;;; package: (runtime boot-definitions)

(declare (usual-integrations))

(define (standard-unparser-method name unparser)
  (make-method name
	       (and unparser
		    (lambda (state object)
		      (with-current-unparser-state state
			(lambda (port)
			  (unparser object port)))))))

(define (unparser/standard-method name #!optional unparser)
  (make-method name
	       (and (not (default-object? unparser))
		    unparser
		    (lambda (state object)
		      (unparse-char state #\space)
		      (unparser state object)))))

(define (make-method name unparser)
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
	    (write-char #\] port))))))

(define (unparser-method? object)
  (and (procedure? object)
       (procedure-arity-valid? object 2)))

(define-guarantee unparser-method "unparser method")

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
  (with-limited-interrupts interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define (with-limited-interrupts limit-mask procedure)
  (with-interrupt-mask (fix:and limit-mask (get-interrupt-enables))
    procedure))

(define-primitives
  (object-pure? pure?)
  (object-constant? constant?)
  gc-space-status)

(define-integrable (future? object)
  ((ucode-primitive object-type? 2) (ucode-type future) object))

(define-integrable (default-object? object)
  (eq? object (default-object)))

(define-integrable (default-object)
  (object-new-type (ucode-type constant) 7))