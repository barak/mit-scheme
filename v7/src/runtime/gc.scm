;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gc.scm,v 13.44 1988/05/05 08:39:12 cph Exp $
;;;
;;;	Copyright (c) 1988 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Garbage Collector

(declare (usual-integrations)
	 (integrate-primitive-procedures
	  garbage-collect primitive-purify primitive-impurify primitive-fasdump
	  set-interrupt-enables! enable-interrupts! primitive-gc-type pure?
	  get-next-constant call-with-current-continuation hunk3-cons
	  set-fixed-objects-vector! tty-write-char tty-write-string exit))

(define add-gc-daemon!)
(define gc-flip)
(define purify)
(define impurify)
(define fasdump)
(define suspend-world)
(define set-default-gc-safety-margin!)

(define garbage-collector-package
  (make-environment

(define default-safety-margin 4500)

;; SET-DEFAULT-GC-SAFETY-MARGIN! changes the amount of memory
;; saved from the heap to allow the GC handler to run.

(set! set-default-gc-safety-margin!
(named-lambda (set-default-gc-safety-margin! #!optional margin)
  (if (or (unassigned? margin) (null? margin))
      default-safety-margin
      (begin (set! default-safety-margin margin)
	     (gc-flip margin)))))

;;;; Cold Load GC

(define (reset)
  (enable-interrupts! interrupt-mask-none))

;;; User call -- optionally overrides the default GC safety
;;; margin for this flip only.

(set! gc-flip
(named-lambda (gc-flip #!optional new-safety-margin)
  (with-interrupts-reduced interrupt-mask-none
   (lambda (old-interrupt-mask)
     (garbage-collect
      (if (unassigned? new-safety-margin)
	  default-safety-margin
	  new-safety-margin))))))

(vector-set! (vector-ref (get-fixed-objects-vector) 1)
	     2				;Local Garbage Collection Interrupt
	     (named-lambda (gc-interrupt interrupt-code interrupt-enables)
	       (gc-flip Default-Safety-Margin)))

(vector-set! (vector-ref (get-fixed-objects-vector) 1)
	     0				;Local Stack Overflow Interrupt
	     (named-lambda (stack-overflow-interrupt interrupt-code
						     interrupt-enables)
	       (stack-overflow)
	       (set-interrupt-enables! interrupt-enables)))

;;; This variable is clobbered by GCSTAT.
(define (stack-overflow)
  (tty-write-char char:newline)
  (tty-write-string "Stack overflow!")
  (tty-write-char char:newline)
  (exit))

(vector-set! (get-fixed-objects-vector)
	     #x0C
	     (named-lambda (hardware-trap-handler escape-code)
	       (hardware-trap)))

;;; This is clobbered also by GCSTAT.
(define (hardware-trap)
  (tty-write-char char:newline)
  (tty-write-string "Hardware trap")
  (tty-write-char char:newline)
  (exit))

;;; The GC daemon is invoked by the microcode whenever there is a need.
;;; All we provide here is a trivial extension mechanism.

(vector-set! (get-fixed-objects-vector)
	     #x0B
	     (named-lambda (gc-daemon)
	       (trigger-daemons gc-daemons)))

(set-fixed-objects-vector! (get-fixed-objects-vector))

(define (trigger-daemons daemons . extra-args)
  (let loop ((daemons daemons))
    (if (not (null? daemons))
	(begin (apply (car daemons) extra-args)
	       (loop (cdr daemons))))))

(define gc-daemons '())

(set! add-gc-daemon!
(named-lambda (add-gc-daemon! daemon)
  (if (not (memq daemon gc-daemons))
      (set! gc-daemons (cons daemon gc-daemons)))))

(reset)

;;;; "GC-like" Primitives

;; Purify an item -- move it into pure space and clean everything
;; by doing a gc-flip

(set! purify
(named-lambda (purify item #!optional really-pure?)
  (if (not (car (primitive-purify item
				  (if (unassigned? really-pure?)
				      false
				      really-pure?)
				  default-safety-margin)))
      (error "Not enough room in constant space" purify item))
  item))
	      
(set! impurify
(named-lambda (impurify object)
  (if (or (zero? (primitive-gc-type object))
	  (not (pure? object)))
      object
      (primitive-impurify object))))

(set! fasdump
(named-lambda (fasdump object filename)
  (let ((filename (canonicalize-output-filename filename))
	(port (rep-output-port)))
    (newline port)
    (write-string "FASDumping " port)
    (write filename port)
    (if (not (primitive-fasdump object filename false))
	(error "Object is too large to be dumped" fasdump object))
    (write-string " -- done" port))
  object))

(set! suspend-world
(named-lambda (suspend-world suspender after-suspend after-restore)
  (with-interrupts-reduced interrupt-mask-gc-ok
    (lambda (ie)
      ((call-with-current-continuation
	(lambda (cont)
	  (let ((fixed-objects-vector (get-fixed-objects-vector))
		(dynamic-state (current-dynamic-state)))
	    (fluid-let ()
	      (call-with-current-continuation
	       (lambda (restart)
		 (gc-flip)
		 (suspender restart)
		 (cont after-suspend)))
	      (set-fixed-objects-vector! fixed-objects-vector)
	      (set-current-dynamic-state! dynamic-state)
	      (reset)
	      ((access snarf-version microcode-system))
	      (reset-keyboard-interrupt-dispatch-table!)
	      (set! *rep-keyboard-map* (keyboard-interrupt-dispatch-table))
	      ((access reset! primitive-io))
	      ((access reset! working-directory-package))
	      after-restore))))
	ie)))))

;;; end GARBAGE-COLLECTOR-PACKAGE.
))