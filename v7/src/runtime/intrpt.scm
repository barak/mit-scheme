#| -*-Scheme-*-

$Id: intrpt.scm,v 14.23 2002/11/20 19:46:20 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

;;;; Interrupt System
;;; package: (runtime interrupt-handler)

(declare (usual-integrations))

(define (initialize-package!)
  (set! index:interrupt-vector
	(fixed-objects-vector-slot 'SYSTEM-INTERRUPT-VECTOR))
  (set! index:interrupt-mask-vector
	(fixed-objects-vector-slot 'INTERRUPT-MASK-VECTOR))
  (set! index:termination-vector
	(fixed-objects-vector-slot 'MICROCODE-TERMINATIONS-PROCEDURES))
  (set! hook/clean-input/flush-typeahead false)
  (set! hook/clean-input/keep-typeahead false)
  (set! hook/^B-interrupt false)
  (set! hook/^G-interrupt false)
  (set! hook/^U-interrupt false)
  (set! hook/^X-interrupt false)
  (set! keyboard-interrupt-vector
	(let ((table (make-vector 256 false)))
	  (for-each (lambda (entry)
		      (vector-set! table
				   (char->ascii (car entry))
				   (cadr entry)))
		    `((#\B ,^B-interrupt-handler)
		      (#\G ,^G-interrupt-handler)
		      (#\U ,^U-interrupt-handler)
		      (#\X ,^X-interrupt-handler)))
	  table))
  (install))

(define-primitives
  (clear-interrupts! 1)
  (tty-next-interrupt-char 0)
  set-fixed-objects-vector!
  (process-timer-clear 0)
  (real-timer-clear 0))

;; These interrupt bit positions must be allocated to bits that fit in
;; the datum field of a positive-fixnum.

(define-integrable stack-overflow-slot 0)
(define-integrable global-gc-slot 1)
(define-integrable gc-slot 2)
(define-integrable character-slot 4)
(define-integrable after-gc-slot 5)
(define-integrable timer-slot 6)
(define-integrable suspend-slot 8)
;; Room for Descartes profiler interrupt handlers
(define-integrable illegal-interrupt-slot 15)

(define index:interrupt-vector)
(define index:interrupt-mask-vector)
(define index:termination-vector)

;;;; Miscellaneous Interrupts

(define (timer-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/timer)
  (thread-timer-interrupt-handler))

;; This switch is set by the command-line initialization code.
(define generate-suspend-file?)

(define (suspend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/suspend)
  (if generate-suspend-file?
      (bind-condition-handler (list condition-type:serious-condition)
	  (lambda (condition)
	    condition
	    (%exit))
	(lambda ()
	  (bind-condition-handler (list condition-type:warning)
	      (lambda (condition)
		condition
		(muffle-warning))
	    (lambda ()
	      (if (not (disk-save (merge-pathnames "scheme_suspend"
						   (user-homedir-pathname))
				  true))
		  (%exit))))))
      (%exit)))

(define (gc-out-of-space-handler . args)
  args
  (abort->nearest "Aborting! Out of memory"))

(define (after-gc-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (trigger-gc-daemons!)
  ;; By clearing the interrupt after running the daemons we ignore an
  ;; GC that occurs while we are running the daemons.  This helps
  ;; prevent us from getting into a loop just running the daemons.
  (clear-interrupts! interrupt-bit/after-gc))

(define ((illegal-interrupt-handler interrupt-bit)
	 interrupt-code interrupt-enables)
  (clear-interrupts! interrupt-bit)
  (error "Illegal interrupt" interrupt-code interrupt-enables))

;;;; Keyboard Interrupts

(define keyboard-interrupt-vector)
(define hook/clean-input/flush-typeahead)
(define hook/clean-input/keep-typeahead)
(define hook/^B-interrupt)
(define hook/^G-interrupt)
(define hook/^U-interrupt)
(define hook/^X-interrupt)

(define (external-interrupt-handler interrupt-code interrupt-mask)
  interrupt-code interrupt-mask
  (clear-interrupts! interrupt-bit/kbd)
  (let ((char (tty-next-interrupt-char)))
    (let ((handler (vector-ref keyboard-interrupt-vector char)))
      (if (not handler)
	  (error "Bad interrupt character:" char))
      (handler char))))

(define (^B-interrupt-handler char)
  (signal-interrupt hook/^B-interrupt
		    hook/clean-input/keep-typeahead
		    char
		    cmdl-interrupt/breakpoint))

(define (^G-interrupt-handler char)
  (signal-interrupt hook/^G-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-top-level))

(define (^U-interrupt-handler char)
  (signal-interrupt hook/^U-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-previous))

(define (^X-interrupt-handler char)
  (signal-interrupt hook/^X-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-nearest))

(define (signal-interrupt hook/interrupt hook/clean-input char interrupt)
  (let ((thread (thread-mutex-owner (port/thread-mutex console-i/o-port))))
    (if thread
	(signal-thread-event thread
	  (lambda ()
	    (if hook/interrupt
		(hook/interrupt))
	    (if (or (not hook/clean-input)
		    (hook/clean-input char))
		(interrupt)))))))

(define (install)
  (without-interrupts
   (lambda ()
     (let ((system-interrupt-vector
	    (vector-ref (get-fixed-objects-vector) index:interrupt-vector))
	   (old-interrupt-mask-vector
	    (vector-ref (get-fixed-objects-vector)
			index:interrupt-mask-vector))
	   (old-termination-vector
	    (vector-ref (get-fixed-objects-vector) index:termination-vector)))
       (let ((interrupt-mask-vector
	      (let ((length (vector-length system-interrupt-vector)))
		(if (and (vector? old-interrupt-mask-vector)
			 (= (vector-length old-interrupt-mask-vector) length))
		    old-interrupt-mask-vector
		    (make-vector length))))
	     (termination-vector
	      (let ((length (microcode-termination/code-limit)))
		(if old-termination-vector
		    (if (> length (vector-length old-termination-vector))
			(vector-grow old-termination-vector length)
			old-termination-vector)
		    (make-vector length #f)))))

	 (let ((length (vector-length system-interrupt-vector)))
	   (do ((i 0 (fix:+ i 1)))
	       ((fix:= i length))
	     (if (not (vector-ref system-interrupt-vector i))
		 (let ((interrupt-bit (fix:lsh 1 i)))
		   (vector-set! interrupt-mask-vector i
				(fix:- interrupt-bit 1)) ; higher priority only
		   (vector-set! system-interrupt-vector i
				(illegal-interrupt-handler interrupt-bit))))))

	 (vector-set! interrupt-mask-vector stack-overflow-slot
		      interrupt-mask/none)

	 (vector-set! interrupt-mask-vector gc-slot
		      ;; interrupt-mask/none
		      (fix:lsh 1 global-gc-slot))

	 (vector-set! system-interrupt-vector timer-slot
		      timer-interrupt-handler)
	 (vector-set! interrupt-mask-vector timer-slot
		      interrupt-mask/gc-ok)

	 (vector-set! system-interrupt-vector character-slot
		      external-interrupt-handler)
	 (vector-set! interrupt-mask-vector character-slot
		      interrupt-mask/timer-ok)

	 (vector-set! system-interrupt-vector after-gc-slot
		      after-gc-interrupt-handler)
	 (vector-set! interrupt-mask-vector after-gc-slot
		      interrupt-mask/timer-ok)

	 (vector-set! system-interrupt-vector suspend-slot
		      suspend-interrupt-handler)
	 (vector-set! interrupt-mask-vector suspend-slot
		      interrupt-mask/timer-ok)

	 (vector-set! termination-vector
		      (microcode-termination 'GC-OUT-OF-SPACE)
		      gc-out-of-space-handler)

	 ;; Install the new tables atomically:

	 (vector-set! (get-fixed-objects-vector)
		      index:interrupt-mask-vector
		      interrupt-mask-vector)

	 (vector-set! (get-fixed-objects-vector)
		      index:termination-vector
		      termination-vector)

	 (set-fixed-objects-vector! (get-fixed-objects-vector)))))))