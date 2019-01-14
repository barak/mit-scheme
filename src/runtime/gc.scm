#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; Garbage Collector
;;; package: (runtime garbage-collector)

(declare (usual-integrations))

(define (initialize-package!)
  (set! gc-boot-loading? #t)
  (set! hook/gc-flip default/gc-flip)
  (set! hook/purify default/purify)
  (set! hook/stack-overflow default/stack-overflow)
  (set! hook/hardware-trap default/hardware-trap)
  (set! default-safety-margin 4500)
  (set! constant-space-queue '())
  (set! constant-space-queue-mutex (make-thread-mutex))
  (set! hook/gc-start default/gc-start)
  (set! hook/gc-finish default/gc-finish)
  (let ((fixed-objects ((ucode-primitive get-fixed-objects-vector))))
    (let ((interrupt-vector (vector-ref fixed-objects 1)))
      (vector-set! interrupt-vector 0 condition-handler/stack-overflow)
      (vector-set! interrupt-vector 2 condition-handler/gc))
    (vector-set! fixed-objects #x0C condition-handler/hardware-trap)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define (condition-handler/gc interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (hook/gc-flip default-safety-margin))

(define (condition-handler/stack-overflow interrupt-code interrupt-enables)
  interrupt-code
  (hook/stack-overflow)
  (set-interrupt-enables! interrupt-enables))

(define (condition-handler/hardware-trap escape-code)
  ((ucode-primitive set-trap-state!)
   ((ucode-primitive set-trap-state!) 2)) ; Ask.
  (hook/hardware-trap escape-code))

(define hook/gc-flip)
(define hook/purify)
(define hook/stack-overflow)
(define hook/hardware-trap)
(define default-safety-margin)

(define (default/gc-flip safety-margin)
  (if (and (not (eq? '() constant-space-queue))
	   (not (object-constant? constant-space-queue)))
      (purify-internal constant-space-queue safety-margin)
      (gc-flip-internal safety-margin)))

(define (default/purify item pure-space? queue?)
  pure-space?
  (if (and (not (eq? 'non-pointer (object-gc-type item)))
	   (not (object-constant? item)))
      (if queue?
	  (with-thread-mutex-lock constant-space-queue-mutex
	    (lambda ()
	      (set! constant-space-queue (cons item constant-space-queue))
	      unspecific))
	  (purify-internal item default-safety-margin))))

(define (default/stack-overflow)
  (abort->nearest "Aborting!: maximum recursion depth exceeded"))

(define (default/hardware-trap escape-code)
  escape-code
  (abort->nearest "Aborting!: the hardware trapped"))

(define constant-space-queue)
(define constant-space-queue-mutex)
(define hook/gc-start)
(define hook/gc-finish)

(define (gc-flip-internal safety-margin)
  (let ((start-value (hook/gc-start)))
    (let ((space-remaining ((ucode-primitive garbage-collect) safety-margin)))
      (gc-finish start-value space-remaining)
      space-remaining)))

(define (purify-internal item safety-margin)
  (let ((start-value (hook/gc-start)))
    (let ((result
	   ((ucode-primitive primitive-purify) item #f safety-margin)))
      (gc-finish start-value (cdr result))
      (cdr result))))

(define (default/gc-start)
  #f)

(define (default/gc-finish start-value space-remaining)
  start-value space-remaining
  #f)

(define (gc-finish start-value space-remaining)
  (hook/gc-finish start-value space-remaining)
  ((ucode-primitive request-interrupts! 1) interrupt-bit/after-gc))

(define (abort-heap-low)
  (if gc-boot-loading?
      (let ((console ((ucode-primitive tty-output-channel 0))))
	((ucode-primitive channel-write 4)
	 console
	 gc-boot-death-message
	 0
	 ((ucode-primitive string-length 1) gc-boot-death-message))
	((ucode-primitive exit-with-value 1) #x14))
      (abort->nearest
       (cmdl-message/append
	(cmdl-message/strings "Aborting!: out of memory")
	;; Clean up whatever possible to avoid a reoccurrence.
	(cmdl-message/active
	 (if (nearest-cmdl/batch-mode?)
	     (lambda (port)
	       (newline port)
	       (exit 'gc-out-of-space))
	     (lambda (port)
	       port
	       (with-gc-notification! #t gc-clean))))))))

(define gc-boot-loading?)

(define gc-boot-death-message
  "\n;; Aborting boot-load: Not enough memory to load -- Use -large option.\n")

;;;; User Primitives

(define (set-gc-safety-margin! #!optional safety-margin)
  (if (not (or (default-object? safety-margin) (not safety-margin)))
      (begin
	(set! default-safety-margin safety-margin)
	(gc-flip safety-margin)))
  default-safety-margin)

(define (gc-flip #!optional safety-margin)
  ;; Optionally overrides the GC safety margin for this flip only.
  (with-absolutely-no-interrupts
   (lambda ()
     (hook/gc-flip (if (default-object? safety-margin)
		       default-safety-margin
		       safety-margin)))))

(define (flush-purification-queue!)
  (if (and (not (eq? '() constant-space-queue))
	   (not (object-constant? constant-space-queue)))
      (gc-flip)))

(define (purify item #!optional pure-space? queue?)
  ;; Purify an item -- move it into pure space and clean everything by
  ;; doing a gc-flip.
  pure-space?
  (hook/purify item #f (if (default-object? queue?) #t queue?))
  item)

(define (constant-space/in-use)
  (let ((v (gc-space-status)))
    (- (vector-ref v 2)
       (vector-ref v 1))))