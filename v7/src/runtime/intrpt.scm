#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/intrpt.scm,v 14.4 1990/06/22 01:04:36 cph Exp $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; Interrupt System
;;; package: (runtime interrupt-handler)

(declare (usual-integrations))

(define (initialize-package!)
  (set! index:interrupt-vector
	(fixed-objects-vector-slot 'SYSTEM-INTERRUPT-VECTOR))
  (set! index:termination-vector
	(fixed-objects-vector-slot 'MICROCODE-TERMINATIONS-PROCEDURES))
  (set! timer-interrupt default/timer-interrupt)
  (set! external-interrupt default/external-interrupt)
  (set! keyboard-interrupts
	(let ((table (make-vector 256 losing-keyboard-interrupt)))
	  (for-each (lambda (entry)
		      (vector-set! table
				   (char->ascii (car entry))
				   (cadr entry)))
		    `((#\B ,(keep-typeahead ^B-interrupt-handler))
		      (#\G ,(flush-typeahead ^G-interrupt-handler))
		      (#\U ,(flush-typeahead ^U-interrupt-handler))
		      (#\X ,(flush-typeahead ^X-interrupt-handler))
		      #| (#\S ,(keep-typeahead ^S-interrupt-handler)) |#
		      #| (#\Q ,(keep-typeahead ^Q-interrupt-handler)) |#
		      #| (#\P ,(flush-typeahead ^P-interrupt-handler)) |#
		      #| (#\Z ,(flush-typeahead ^Z-interrupt-handler)) |#))
	  table))
  (set! hook/clean-input/flush-typeahead default/clean-input)
  (set! hook/clean-input/keep-typeahead default/clean-input)
  (set! hook/^B-interrupt default/^B-interrupt)
  (set! hook/^G-interrupt default/^G-interrupt)
  (set! hook/^U-interrupt default/^U-interrupt)
  (set! hook/^X-interrupt default/^X-interrupt)
  #| (set! hook/^S-interrupt default/^S-interrupt) |#
  #| (set! hook/^Q-interrupt default/^Q-interrupt) |#
  #| (set! hook/^P-interrupt default/^P-interrupt) |#
  #| (set! hook/^Z-interrupt default/^Z-interrupt) |#
  (install))

(define-primitives
  (clear-interrupts! 1)
  (tty-next-interrupt-char 0)
  set-fixed-objects-vector!
  (setup-timer-interrupt 2))

(define-integrable stack-overflow-slot 0)
(define-integrable gc-slot 2)
(define-integrable character-slot 4)
(define-integrable timer-slot 6)
(define-integrable suspend-slot 8)
(define-integrable illegal-interrupt-slot 9)

(define index:interrupt-vector)
(define index:termination-vector)

;;;; Miscellaneous Interrupts

(define (timer-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/timer)
  (timer-interrupt))

(define timer-interrupt)
(define (default/timer-interrupt)
  (setup-timer-interrupt '() '())
  (error "Unhandled Timer interrupt received"))

(define (suspend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/suspend)
  (bind-condition-handler '() (lambda (condition) condition (%exit))
    (lambda ()
      (if (not (disk-save (merge-pathnames (string->pathname "scheme_suspend")
					   (home-directory-pathname))
			  true))
	  (%exit)))))

(define (gc-out-of-space-handler . args)
  args
  (abort-to-nearest-driver "Aborting! Out of memory"))

(define (illegal-interrupt-handler interrupt-code interrupt-enables)
  (error "Illegal interrupt" interrupt-code interrupt-enables))

(define (default-interrupt-handler interrupt-code interrupt-enables)
  (error "Anomalous interrupt" interrupt-code interrupt-enables))

;;;; Keyboard Interrupts

(define (external-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code
  (clear-interrupts! interrupt-bit/kbd)
  (external-interrupt (tty-next-interrupt-char) interrupt-enables))

(define (with-external-interrupts-handler handler thunk)
  (fluid-let ((external-interrupt (flush-typeahead handler)))
    (thunk)))

(define external-interrupt)
(define (default/external-interrupt character interrupt-enables)
  ((vector-ref keyboard-interrupts character) character interrupt-enables))

(define (losing-keyboard-interrupt character interrupt-enables)
  interrupt-enables
  (error "Bad interrupt character" character))

(define keyboard-interrupts)

(define ((flush-typeahead kernel) character interrupt-enables)
  (if (hook/clean-input/flush-typeahead character)
      (kernel character interrupt-enables)))

(define ((keep-typeahead kernel) character interrupt-enables)
  (if (hook/clean-input/keep-typeahead character)
      (kernel character interrupt-enables)))

(define hook/clean-input/flush-typeahead)
(define hook/clean-input/keep-typeahead)
(define (default/clean-input character) character true)

(define (^B-interrupt-handler character interrupt-enables)
  character
  (hook/^B-interrupt interrupt-enables))

(define (^G-interrupt-handler character interrupt-enables)
  character
  (hook/^G-interrupt interrupt-enables))

(define (^U-interrupt-handler character interrupt-enables)
  character
  (hook/^U-interrupt interrupt-enables))

(define (^X-interrupt-handler character interrupt-enables)
  character
  (hook/^X-interrupt interrupt-enables))

#|
(define (^S-interrupt-handler character interrupt-enables)
  character
  (hook/^S-interrupt interrupt-enables))

(define (^Q-interrupt-handler character interrupt-enables)
  character
  (hook/^Q-interrupt interrupt-enables))

(define (^P-interrupt-handler character interrupt-enables)
  character
  (hook/^P-interrupt interrupt-enables))

(define (^Z-interrupt-handler character interrupt-enables)
  character
  (hook/^Z-interrupt interrupt-enables))
|#

(define hook/^B-interrupt)
(define hook/^G-interrupt)
(define hook/^U-interrupt)
(define hook/^X-interrupt)
#| (define hook/^S-interrupt) |#
#| (define hook/^Q-interrupt) |#
#| (define hook/^P-interrupt) |#
#| (define hook/^Z-interrupt) |#

(define (default/^B-interrupt interrupt-enables)
  interrupt-enables
  (cmdl-interrupt/breakpoint))

(define (default/^G-interrupt interrupt-enables)
  interrupt-enables
  (cmdl-interrupt/abort-top-level))

(define (default/^U-interrupt interrupt-enables)
  interrupt-enables
  (cmdl-interrupt/abort-previous))

(define (default/^X-interrupt interrupt-enables)
  interrupt-enables
  (cmdl-interrupt/abort-nearest))

#|
(define (default/^S-interrupt interrupt-enables)
  (if (not busy-wait-continuation)
      (begin
	(set-interrupt-enables! interrupt-enables)
	(beep console-output-port)
	(call-with-current-continuation
	 (lambda (continuation)
	   (fluid-let ((busy-wait-continuation continuation))
	     (let busy-wait () (busy-wait))))))))

(define (default/^Q-interrupt interrupt-enables)
  (if busy-wait-continuation
      (begin (set-interrupt-enables! interrupt-enables)
	     (busy-wait-continuation false))))

(define busy-wait-continuation
  false)

(define (default/^P-interrupt interrupt-enables)
  (set-interrupt-enables! interrupt-enables)
  (proceed))

(define (default/^Z-interrupt interrupt-enables)
  (set-interrupt-enables! interrupt-enables)
  (edit))
|#

(define (install)
  (without-interrupts
   (lambda ()
     (let ((old-system-interrupt-vector
	    (vector-ref (get-fixed-objects-vector) index:interrupt-vector))
	   (old-termination-vector
	    (vector-ref (get-fixed-objects-vector) index:termination-vector)))
       (let ((previous-gc-interrupt
	      (vector-ref old-system-interrupt-vector gc-slot))
	     (previous-stack-interrupt
	      (vector-ref old-system-interrupt-vector stack-overflow-slot))
	     (system-interrupt-vector
	      (make-vector (vector-length old-system-interrupt-vector)
			   default-interrupt-handler))
	     (termination-vector
	      (let ((length (microcode-termination/code-limit)))
		(if old-termination-vector
		    (if (> length (vector-length old-termination-vector))
			(vector-grow old-termination-vector length)
			old-termination-vector)
		    (make-vector length false)))))

	 (vector-set! system-interrupt-vector gc-slot previous-gc-interrupt)
	 (vector-set! system-interrupt-vector stack-overflow-slot
		      previous-stack-interrupt)
	 (vector-set! system-interrupt-vector character-slot
		      external-interrupt-handler)
	 (vector-set! system-interrupt-vector timer-slot
		      timer-interrupt-handler)
	 (vector-set! system-interrupt-vector suspend-slot
		      suspend-interrupt-handler)
	 (vector-set! system-interrupt-vector illegal-interrupt-slot
		      illegal-interrupt-handler)

	 ;; install the new vector atomically
	 (vector-set! (get-fixed-objects-vector)
		      index:interrupt-vector
		      system-interrupt-vector)

	 (vector-set! termination-vector
		      (microcode-termination 'GC-OUT-OF-SPACE)
		      gc-out-of-space-handler)

	 (vector-set! (get-fixed-objects-vector)
		      index:termination-vector
		      termination-vector)

	 (set-fixed-objects-vector! (get-fixed-objects-vector)))))))