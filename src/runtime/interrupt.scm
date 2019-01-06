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

;;;; Interrupt System
;;; package: (runtime interrupt-handler)

(declare (usual-integrations))

(define (initialize-package!)
  (set! index:interrupt-vector
	(fixed-objects-vector-slot 'system-interrupt-vector))
  (set! index:interrupt-mask-vector
	(fixed-objects-vector-slot 'interrupt-mask-vector))
  (set! index:termination-vector
	(fixed-objects-vector-slot 'microcode-terminations-procedures))
  (set! event:console-resize (make-event-distributor))
  (set! hook/clean-input/flush-typeahead false)
  (set! hook/clean-input/keep-typeahead false)
  (set! hook/^b-interrupt false)
  (set! hook/^g-interrupt false)
  (set! hook/^u-interrupt false)
  (set! hook/^x-interrupt false)
  (set! keyboard-interrupt-vector
	(let ((table (make-vector 256 false)))
	  (for-each (lambda (entry)
		      (vector-set! table
				   (char->integer (car entry))
				   (cadr entry)))
		    `((#\B ,^b-interrupt-handler)
		      (#\G ,^g-interrupt-handler)
		      (#\U ,^u-interrupt-handler)
		      (#\X ,^x-interrupt-handler)))
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
(define-integrable console-resize-slot 7)
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
(define generate-suspend-file? #f)

(define (suspend-interrupt-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/suspend)
  (if generate-suspend-file?
      (bind-condition-handler (list condition-type:serious-condition)
	  (lambda (condition)
	    condition
	    (exit))
	(lambda ()
	  (bind-condition-handler (list condition-type:warning)
	      (lambda (condition)
		condition
		(muffle-warning))
	    (lambda ()
	      (if (not (disk-save (merge-pathnames "scheme_suspend"
						   (user-homedir-pathname))
				  true))
		  (exit))))))
      (exit)))

(define (gc-out-of-space-handler . args)
  args
  (abort->nearest "Aborting! Out of memory"))

(define after-gc-interrupt-handler
  (let ((running? #f))
    (named-lambda (after-gc-interrupt-handler interrupt-code interrupt-enables)
      (declare (ignore interrupt-code interrupt-enables))
      (clear-interrupts! interrupt-bit/after-gc)
      (set-interrupt-enables! interrupt-mask/timer-ok)
      ;; By checking that this handler is not still running we ignore
      ;; GCs that occur while we are running the daemons.  This helps
      ;; prevent us from getting into a loop just running the daemons.
      (if (not running?)
	  (begin
	    (set! running? #t)
	    (trigger-gc-daemons!)
	    (set! running? #f)
	    (handle-current-thread-events))))))

(define event:console-resize)
(define (console-resize-handler interrupt-code interrupt-enables)
  interrupt-code interrupt-enables
  (clear-interrupts! interrupt-bit/global-3)
  (cond ((console-thread)
         => (lambda (thread)
              (signal-thread-event thread
                (lambda ()
                  (event-distributor/invoke! event:console-resize)))))))

(define ((illegal-interrupt-handler interrupt-bit)
	 interrupt-code interrupt-enables)
  (clear-interrupts! interrupt-bit)
  (error "Illegal interrupt:" interrupt-bit interrupt-code interrupt-enables))

;;;; Keyboard Interrupts

(define keyboard-interrupt-vector)
(define hook/clean-input/flush-typeahead)
(define hook/clean-input/keep-typeahead)
(define hook/^b-interrupt)
(define hook/^g-interrupt)
(define hook/^u-interrupt)
(define hook/^x-interrupt)

(define (external-interrupt-handler interrupt-code interrupt-mask)
  interrupt-code interrupt-mask
  (clear-interrupts! interrupt-bit/kbd)
  (let ((char (tty-next-interrupt-char)))
    (let ((handler (vector-ref keyboard-interrupt-vector char)))
      (if (not handler)
	  (error "Bad interrupt character:" char))
      (handler char))))

(define (^b-interrupt-handler char)
  (signal-interrupt hook/^b-interrupt
		    hook/clean-input/keep-typeahead
		    char
		    cmdl-interrupt/breakpoint))

(define (^g-interrupt-handler char)
  (signal-interrupt hook/^g-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-top-level))

(define (^u-interrupt-handler char)
  (signal-interrupt hook/^u-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-previous))

(define (^x-interrupt-handler char)
  (signal-interrupt hook/^x-interrupt
		    hook/clean-input/flush-typeahead
		    char
		    cmdl-interrupt/abort-nearest))

(define (signal-interrupt hook/interrupt hook/clean-input char interrupt)
  (let ((thread
	 (thread-mutex-owner (textual-port-thread-mutex (console-i/o-port)))))
    (if thread
	(signal-thread-event thread
	  (lambda ()
	    (if hook/interrupt
		(hook/interrupt))
	    (if (or (not hook/clean-input)
		    (hook/clean-input char))
		(interrupt)))))))

(define (install)
  (let ((fov (get-fixed-objects-vector)))
    (let ((system-interrupt-vector (vector-ref fov index:interrupt-vector))
	  (old-interrupt-mask-vector (vector-ref fov
						 index:interrupt-mask-vector))
	  (old-termination-vector (vector-ref fov index:termination-vector)))
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
		     interrupt-mask/gc-ok)

	(vector-set! system-interrupt-vector suspend-slot
		     suspend-interrupt-handler)
	(vector-set! interrupt-mask-vector suspend-slot
		     interrupt-mask/timer-ok)

	(vector-set! system-interrupt-vector console-resize-slot
		     console-resize-handler)
	(vector-set! interrupt-mask-vector console-resize-slot
		     interrupt-mask/all)

	(vector-set! termination-vector
		     (microcode-termination 'gc-out-of-space)
		     gc-out-of-space-handler)

	(vector-set! fov index:interrupt-mask-vector interrupt-mask-vector)

	(vector-set! fov index:termination-vector termination-vector)

	(set-fixed-objects-vector! fov)))))