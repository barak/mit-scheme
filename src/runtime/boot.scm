#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Boot-time definitions
;;; package: (runtime boot-definitions)

(declare (usual-integrations))

;;;; Interrupt control

(define interrupt-bit/stack     #x0001)
(define interrupt-bit/global-gc #x0002)
(define interrupt-bit/gc        #x0004)
(define interrupt-bit/global-1  #x0008)
(define interrupt-bit/kbd       #x0010)
(define interrupt-bit/after-gc  #x0020)
(define interrupt-bit/timer     #x0040)
(define interrupt-bit/global-3  #x0080)
(define interrupt-bit/suspend   #x0100)
;; Interrupt bits #x0200 through #x4000 inclusive are reserved
;; for the Descartes PC sampler.

;; GC & stack overflow only
(define interrupt-mask/gc-ok    #x0007)

;; GC, stack overflow, and timer only
(define interrupt-mask/timer-ok #x0047)

;; Absolutely everything off
(define interrupt-mask/none     #x0000)

;; Normal: all enabled
(define interrupt-mask/all      #xFFFF)

(define (with-absolutely-no-interrupts thunk)
  ((ucode-primitive with-interrupt-mask)
   interrupt-mask/none
   (lambda (interrupt-mask)
     interrupt-mask
     (thunk))))

(define (without-interrupts thunk)
  (with-limited-interrupts interrupt-mask/gc-ok
    (lambda (interrupt-mask)
      interrupt-mask
      (thunk))))

(define (with-limited-interrupts limit-mask procedure)
  ((ucode-primitive with-interrupt-mask)
   (fix:and limit-mask (get-interrupt-enables))
   procedure))

;;;; Boot-time initializers

(define (init-boot-inits!)
  (set! boot-inits '())
  unspecific)

(define (add-boot-init! thunk)
  (if (and booting? boot-inits)
      (set! boot-inits (cons thunk boot-inits))
      (thunk))
  unspecific)

(define (save-boot-inits! environment)
  (if (pair? boot-inits)
      (let ((inits (reverse! boot-inits)))
	(set! boot-inits #f)
	(let ((p (assq environment saved-boot-inits)))
	  (if p
	      (set-cdr! p (append! (cdr p) inits))
	      (begin
		(set! saved-boot-inits
		      (cons (cons environment inits)
			    saved-boot-inits))
		unspecific))))))

(define (get-boot-init-runner environment)
  (let ((p (assq environment saved-boot-inits)))
    (and p
	 (let ((inits (cdr p)))
	   (set! saved-boot-inits (delq! p saved-boot-inits))
	   (lambda ()
	     (for-each (lambda (init) (init))
		       inits))))))

(define (defer-boot-action group-name thunk)
  (if booting?
      (let ((group (%get-boot-action-group group-name)))
	(set-cdr! group
		  (cons thunk
			(cdr group))))
      (thunk)))

(define (run-deferred-boot-actions group-name)
  (let ((group (%find-boot-action-group group-name)))
    (if group
	(begin
	  (set! boot-action-groups (delq! group boot-action-groups))
	  (for-each (lambda (thunk) (thunk))
		    (reverse! (cdr group)))))))

(define (%get-boot-action-group group-name)
  (or (%find-boot-action-group group-name)
      (let ((group (cons group-name '())))
	(set! boot-action-groups (cons group boot-action-groups))
	group)))

(define (%find-boot-action-group group-name)
  (let loop ((groups boot-action-groups))
    (and (pair? groups)
	 (if (eq? group-name (caar groups))
	     (car groups)
	     (loop (cdr groups))))))

(define (finished-booting!)
  (set! booting? #f)
  (if (pair? boot-inits)
      (warn "boot-inits not saved:" boot-inits))
  (if (pair? saved-boot-inits)
      (warn "saved-boot-inits not run:" saved-boot-inits))
  (if (pair? boot-action-groups)
      (warn "boot-action-groups not run:" boot-action-groups)))

(define booting? #t)
(define boot-inits #f)
(define saved-boot-inits '())
(define boot-action-groups '())

;;;; Printing

(define (define-unparser-method predicate unparser)
  (defer-boot-action 'unparser-methods
    (lambda ()
      (define-unparser-method predicate unparser))))

(define (define-pp-describer predicate describer)
  (defer-boot-action 'pp-describers
    (lambda ()
      (define-pp-describer predicate describer))))

(define (unparser-method? object)
  (and (procedure? object)
       (procedure-arity-valid? object 2)))

(define (general-unparser-method procedure)
  (lambda (state object)
    (with-current-unparser-state state
      (lambda (port)
	(if (get-param:unparse-with-maximum-readability?)
	    (begin
	      (write-string "#@" port)
	      (write (object-hash object) port))
	    (procedure object port))))))

(define (bracketed-unparser-method procedure)
  (general-unparser-method
   (lambda (object port)
     (write-string "#[" port)
     (procedure object port)
     (write-char #\] port))))

(define (standard-unparser-method name procedure)
  (bracketed-unparser-method
   (lambda (object port)
     (display (if (procedure? name)
		  (name object)
		  name)
	      port)
     (write-char #\space port)
     (write (object-hash object) port)
     (if procedure (procedure object port)))))

(define (simple-unparser-method name get-parts)
  (standard-unparser-method name
    (and get-parts
	 (lambda (object port)
	   (for-each (lambda (object)
		       (write-char #\space port)
		       (write object port))
		     (get-parts object))))))

(define (simple-parser-method procedure)
  (lambda (objects lose)
    (or (and (pair? (cdr objects))
	     (procedure (cddr objects)))
	(lose))))

;;;; Miscellany

(define (object-constant? object)
  ((ucode-primitive constant?) object))

(define (object-pure? object)
  object
  #f)

(define (default-object? object)
  (eq? object (default-object)))

(define (default-object)
  ((ucode-primitive object-set-type) (ucode-type constant) 7))

(define (gc-space-status)
  ((ucode-primitive gc-space-status)))

(define (bytes-per-object)
  (vector-ref (gc-space-status) 0))