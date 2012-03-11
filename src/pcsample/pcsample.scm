#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

#|
TODO:
        Flonum in counts should be coerced into exacts straight away.
        Make profile tables hold their elements weakly again (?)
	Reset should preserve enable/disable state.
        Separate timing from sampling.
|#

;;;; PC Sampling
;;; package: (pc-sample)

(declare (usual-integrations))

(define (initialize-package!)
  (set! *pc-sample/state* 'UNINITIALIZED)
  (set! *pc-sample/sample-interval* pc-sample/default-sample-interval)
  (install))

(define-primitives
  (pc-sample/timer-clear 0)
  (pc-sample/timer-set   2)
  (%pc-sample/halted? 0)	; super secret state hook
  (pc-sample/spill-GC-samples-into-primitive-table 0)
  (        interp-proc-profile-buffer/install 1)
  (        interp-proc-profile-buffer/disable 0)
  (purified-code-block-profile-buffers/install 2)
  ( heathen-code-block-profile-buffers/install 2)
  (purified-code-block-profile-buffers/disable 0)
  ( heathen-code-block-profile-buffers/disable 0)
  ;; Following for runtime/microcode installation only
  (%pc-sample/install-microcode 0)
  (%pc-sample/disable-microcode 0)
  )

(define index:pc-sample/builtin-table)
(define index:pc-sample/utility-table)
(define index:pc-sample/primitive-table)
(define index:pc-sample/code-block-table)
(define index:pc-sample/purified-code-block-block-buffer)
(define index:pc-sample/purified-code-block-offset-buffer)
(define index:pc-sample/heathen-code-block-block-buffer)
(define index:pc-sample/heathen-code-block-offset-buffer)
(define index:pc-sample/interp-proc-buffer)
(define index:pc-sample/prob-comp-table)
(define index:pc-sample/UFO-table)

(define (install-indices)		; see utabmd.scm
  (set! index:pc-sample/builtin-table
	(fixed-objects-vector-slot 'PC-Sample/Builtin-Table))
  (set! index:pc-sample/utility-table
	(fixed-objects-vector-slot 'PC-Sample/Utility-Table))
  (set! index:pc-sample/primitive-table
	(fixed-objects-vector-slot 'PC-Sample/Primitive-Table))
  (set! index:pc-sample/code-block-table
	(fixed-objects-vector-slot 'PC-Sample/Code-Block-Table))
  (set! index:pc-sample/purified-code-block-block-buffer
	(fixed-objects-vector-slot 'PC-Sample/Purified-Code-Block-Block-Buffer))
  (set! index:pc-sample/purified-code-block-offset-buffer
	(fixed-objects-vector-slot 'PC-Sample/Purified-Code-Block-Offset-Buffer))
  (set! index:pc-sample/heathen-code-block-block-buffer
	(fixed-objects-vector-slot 'PC-Sample/Heathen-Code-Block-Block-Buffer))
  (set! index:pc-sample/heathen-code-block-offset-buffer
	(fixed-objects-vector-slot 'PC-Sample/Heathen-Code-Block-Offset-Buffer))
  (set! index:pc-sample/interp-proc-buffer
	(fixed-objects-vector-slot 'PC-Sample/Interp-Proc-Buffer))
  (set! index:pc-sample/prob-comp-table
	(fixed-objects-vector-slot 'PC-Sample/Prob-Comp-Table))
  (set! index:pc-sample/UFO-table
	(fixed-objects-vector-slot 'PC-Sample/UFO-Table))
  )

;; Sample while running pc-sample interrupt handling code?

(define *pc-sample/sample-sampler?* #F)	; Ziggy wants to, but nobody else...

;; Sample Interval

(define *pc-sample/sample-interval*)
(define  pc-sample/default-sample-interval 20) ; milliseconds (i.e. 50Hz-ish)

(define (pc-sample/sample-interval)
  "()\n\
  Returns the interval (in milliseconds) between the completion of one\n\
  PC sampling and the initiation of the next PC sampling.\n\
  This value may be changed by invoking:\n\
         (PC-SAMPLE/SET-SAMPLE-INTERVAL <interval>)\n\
  where <interval> is an exact positive integer expressing milliseconds.\n\
  The initial value for this implicit system state variable is determined\n\
  by the value returned by the expression: (PC-SAMPLE/DEFAULT-SAMPLE-INTERVAL)\
  "
        *pc-sample/sample-interval*)	; Fear not: package inits to default

(define (pc-sample/set-sample-interval #!optional interval)
  "(#!OPTIONAL interval)\n\
  Sets the interval between the completion of one PC sampling and the\n\
  initiation of the next PC sampling to be roughly INTERVAL milliseconds.\n\
  If no INTERVAL argument is supplied, it defaults to the value returned by\n\
  the expression (PC-SAMPLE/DEFAULT-SAMPLE-INTERVAL).\
  "
  (set! *pc-sample/sample-interval*
	(cond ((default-object? interval)
	       pc-sample/default-sample-interval)
	      ((zero? interval)
	       (cond (*pc-sample/noisy?*
		      (display
		       (string-append "\n;; PC Sampling has been disabled "
				      "via a  0 msec  sampling interval."))))
	       0)
	      ((negative? interval)	; Smart ass.
	       (display (string-append
			 "\n"
			 ";;-----------\n"
			 ";; WARNING --\n"
			 ";;-----------\n"
			 ";;\n"
			 ";; Your hardware configuration cannot "
			    "support negative PC sampling intervals.\n"
			 ";; Consult your local hardware distributor for an "
			    "FTL co-processor upgrade kit.\n"
			 ";;\n"
			 ";; In the meantime, a sample interval of  1 msec  "
			    "will be used instead.\n"
			 ";;\n"
			 ";; Have a nice day, " (current-user-name) ".\n"))
	       1)
	      ((not (integer? interval))
	       (error "PC Sampling interval must be a non-negative integer."
		      interval))
	      (else
	       interval)))
  unspecific)

(define *current-user-name-promise*)
(define (current-user-name) (force *current-user-name-promise*))

(define (install-current-user-name-promise)
  (cond (*pc-sample/install-verbosity?*
	 (newline)
	 (display "Installing current user name promise...")
	 (newline)))
  (set! *current-user-name-promise* (delay (unix/current-user-name)))
  unspecific)

;; Sample State Regulation

(define *pc-sample/state*)
(define (pc-sample/state)
        *pc-sample/state*)
(define (pc-sample/set-state! new-state)
  (set! *pc-sample/state*     new-state))

(define (pc-sample/uninitialized?)
  (eq?  (pc-sample/state) 'UNINITIALIZED))

(define (pc-sample/init #!optional start?)
  "(#!OPTIONAL start?)\n\
  Resets all PC sampling tables and sets the sampling interval to the\n\
  system default sampling interval.\n\
  This is the preferred way to initialize PC sampling in the system.\n\
  If the optional START? argument is supplied, PC sampling commences ASAP.\n\
  Otherwise, (PC-SAMPLE/START) may be invoked to commence sampling, whereupon\n\
  the evolving state of the PC sampling tables and counters may be monitored\n\
  by invoking: (PC-SAMPLE/STATUS).\
  "
  (pc-sample/reset)
  (pc-sample/set-state! 'INITIALIZED)
  (if (or (default-object? start?) (not start?))
      (pc-sample/set-sample-interval)
      (pc-sample/start))
  unspecific)

(define (pc-sample/initialized?)
  (not  (pc-sample/uninitialized?)))


(define *pc-sample/noisy?* #F)

(define (pc-sample/start #!optional interval)
  "(#!OPTIONAL interval)\n\
  Enables periodic sampling of the virtual Program Counter by starting the\n\
  PC sampling interrupt timer. Note that this does *not* initialize the PC\n\
  sampling tables into which the sampling profile information is gathered.\n\
  Unless/until these tables are initialized, no gathering of sampling info\n\
  will be recorded, although the PC sampling interrupts will be issued and\n\
  processed: the data will just not be recorded. To initiate sampling, refer\n\
  to (PC-SAMPLE/INIT) instead. By contrast, PC-SAMPLE/START serves two pur-\n\
  poses: 1) it is useful for unsuspending PC sampling after one has issued\n\
  a (PC-SAMPLE/STOP), and 2) it is useful for debuggering the interrupt/trap\n\
  mechanism for processing periodic PC sampling.\n\
  \n\
  The optional INTERVAL argument specifies how many milliseconds after a\n\
  PC sample completes should the next PC sample be attempted.\n\
  The evolving state of the PC sampling tables and counters may be monitored\n\
  by invoking: (PC-SAMPLE/STATUS).\
 "
  (cond ((not (default-object? interval))
	 (pc-sample/set-sample-interval interval)))
  (let ((real-interval (pc-sample/sample-interval)))
    (cond ((zero? real-interval)
	   (pc-sample/timer-clear)
	   (pc-sample/disable)
	   (cond (*pc-sample/noisy?*
		  (display
		   "\n;; PC Sampling DISABLED: by virtue of 0 msec interval")))
	   )
	  ((pc-sample/uninitialized?)
	   (pc-sample/init 'START))
	  (else
	   (cond (*pc-sample/noisy?*
		  (display (string-append "\n;; PC Sampling starting: "
					  (number->string real-interval)
					  " millisecond period."))))
	   (pc-sample/set-state! 'RUNNING)
	   (pc-sample/timer-set *ASAP* real-interval)))
    )
  unspecific)

(define *ASAP* 1)  ; cannot be 0... that would disable the timer.

(define-integrable (pc-sample/running?)
             (not  (%pc-sample/halted?)))

(define-integrable (pc-sample/started?)
                   (pc-sample/running?))


(define (pc-sample/stop)
  "()\n\
  Halts PC sampling by disabling the sampling interrupt timer.\n\
  No profiling state is reset so invoking (PC-SAMPLE/START <interval>)\n\
  afterward will re-start profiling by accumulating into the existing state.\n\
  By contrast, see PC-SAMPLE/ENABLE and PC-SAMPLE/DISABLE.\n\
  The state of the PC sampling tables and counters existent at the time when\n\
  the sampling was stopped may be monitored by invoking: (PC-SAMPLE/STATUS).\
  "
  (pc-sample/timer-clear)
  (pc-sample/set-state! 'STOPPED)
  (cond (*pc-sample/noisy?*
	 (display "\n;; PC Sampling stopped.")))
  unspecific)

(define-integrable (pc-sample/stopped?)
                   (%pc-sample/halted?))

;; Status/Accessors

;; Returns a structure of PC sampling profile information.
;; This is useful for monitoring the evolving histogram of PC sampling data.

(define-structure (pc-sample/status-record
		   (conc-name	pc-sample/status/)
		   (constructor pc-sample/status
				(#!optional builtin-table
					    utility-table
					    primitive-table
					    code-block-table
					    code-block-buffer/status
					    interp-proc-table
					    interp-proc-buffer/status
					    prob-comp-table
					    UFO-table)))
  (builtin-table		(pc-sample/builtin-table))
  (utility-table		(pc-sample/utility-table))
  (primitive-table		(pc-sample/primitive-table))
  (code-block-table		(pc-sample/code-block-table))
  (code-block-buffer/status	(pc-sample/code-block-buffer/status))
  (interp-proc-table		(pc-sample/interp-proc-table))
  (interp-proc-buffer/status	(pc-sample/interp-proc-buffer/status))
  (prob-comp-table		(pc-sample/prob-comp-table))
  (UFO-table			(pc-sample/UFO-table))
  )

(define pc-sample/builtin-table)
(define pc-sample/utility-table)
(define pc-sample/primitive-table)
(define pc-sample/purified-code-block-block-buffer)
(define pc-sample/purified-code-block-offset-buffer)
(define pc-sample/heathen-code-block-block-buffer)
(define pc-sample/heathen-code-block-offset-buffer)
(define pc-sample/interp-proc-buffer)
(define pc-sample/prob-comp-table)
(define pc-sample/UFO-table)

(define (pc-sample/code-block-table)          (code-block-profile-table))
(define (pc-sample/code-block-buffer/status)  (code-block-profile-buffer/status))
(define (pc-sample/interp-proc-table)        (interp-proc-profile-table))
(define (pc-sample/interp-proc-buffer/status)(interp-proc-profile-buffer/status))

;; Exportable naming scheme
(define (pc-sample/builtin/status)
        (pc-sample/builtin-table))
(define (pc-sample/utility/status)
        (pc-sample/utility-table))
(define (pc-sample/primitive/status)
        (pc-sample/primitive-table))
(define (pc-sample/code-block/status)
        (pc-sample/code-block-table))
(define (pc-sample/interp-proc/status)
        (pc-sample/interp-proc-table))
(define (pc-sample/prob-comp/status)
        (pc-sample/prob-comp-table))
(define (pc-sample/UFO/status)
        (pc-sample/UFO-table))

(define (generate:pc-sample/table-accessor index)
  (lambda ()
    (cond ((eq? index index:pc-sample/primitive-table)
	   (pc-sample/spill-GC-samples-into-primitive-table)))
    (vector-ref (get-fixed-objects-vector) index)))

(define (install-accessors)
  (set! pc-sample/builtin-table
	(generate:pc-sample/table-accessor index:pc-sample/builtin-table))
  (set! pc-sample/utility-table
	(generate:pc-sample/table-accessor index:pc-sample/utility-table))
  (set! pc-sample/primitive-table
	(generate:pc-sample/table-accessor index:pc-sample/primitive-table))
  (set! pc-sample/purified-code-block-block-buffer
	(generate:pc-sample/table-accessor index:pc-sample/purified-code-block-block-buffer))
  (set! pc-sample/purified-code-block-offset-buffer
	(generate:pc-sample/table-accessor index:pc-sample/purified-code-block-offset-buffer))
  (set! pc-sample/heathen-code-block-block-buffer
	(generate:pc-sample/table-accessor index:pc-sample/heathen-code-block-block-buffer))
  (set! pc-sample/heathen-code-block-offset-buffer
	(generate:pc-sample/table-accessor index:pc-sample/heathen-code-block-offset-buffer))
  (set! pc-sample/interp-proc-buffer
	(generate:pc-sample/table-accessor index:pc-sample/interp-proc-buffer))
  (set! pc-sample/prob-comp-table
	(generate:pc-sample/table-accessor index:pc-sample/prob-comp-table))
  (set! pc-sample/UFO-table
	(generate:pc-sample/table-accessor index:pc-sample/UFO-table))
  )

(define-structure (pc-sample/fixed-objects-record
		   (conc-name	pc-sample/fixed-objects/)
		   (constructor pc-sample/fixed-objects
				(#!optional builtin-table
					    utility-table
					    primitive-table
					    purified-cobl-block-buffer
					    purified-cobl-offset-buffer
					    heathen-cobl-block-buffer
					    heathen-cobl-offset-buffer
					    interp-proc-buffer
					    prob-comp-table
					    UFO-table)))
  (builtin-table	       (pc-sample/builtin-table))
  (utility-table	       (pc-sample/utility-table))
  (primitive-table	       (pc-sample/primitive-table))
  (purified-cobl-block-buffer  (pc-sample/purified-code-block-block-buffer))
  (purified-cobl-offset-buffer (pc-sample/purified-code-block-offset-buffer))
  (heathen-cobl-block-buffer   (pc-sample/heathen-code-block-block-buffer))
  (heathen-cobl-offset-buffer  (pc-sample/heathen-code-block-offset-buffer))
  (interp-proc-buffer	       (pc-sample/interp-proc-buffer))
  (prob-comp-table	       (pc-sample/prob-comp-table))
  (UFO-table		       (pc-sample/UFO-table))
  )

;; Makers

(define pc-sample/builtin-table/make)
(define pc-sample/utility-table/make)
(define pc-sample/primitive-table/make)
(define pc-sample/code-block-buffer/make/purified-blocks)
(define pc-sample/code-block-buffer/make/purified-offsets)
(define pc-sample/code-block-buffer/make/heathen-blocks)
(define pc-sample/code-block-buffer/make/heathen-offsets)
(define pc-sample/interp-proc-buffer/make)
(define pc-sample/prob-comp-table/make)
(define pc-sample/UFO-table/make)

(define (generate:pc-sample/table-maker length-thunk init-value-thunk)
  (lambda ()
    (make-initialized-vector (length-thunk)
			     (lambda (i) i (init-value-thunk)))))

(define (generate:pc-sample/buffer-maker length-thunk)
  (lambda ()
    (make-vector (length-thunk)
		 ;; interp-proc-buffer is a buffer of interp-procs, 
		 ;;   not a table of counters.
		 #F)))

(define (generate:pc-sample/counter-maker init-value-thunk)
  (lambda ()
    (vector (init-value-thunk)		; happy count
	    (init-value-thunk)		;   sad count
	    )))

(define (install-makers)
  (set! pc-sample/builtin-table/make
	(generate:pc-sample/table-maker get-builtin-count
					pc-sample/init-datum))
  (set! pc-sample/utility-table/make
	(generate:pc-sample/table-maker get-utility-count
					pc-sample/init-datum))
  (set! pc-sample/primitive-table/make
	(generate:pc-sample/table-maker get-primitive-count
					pc-sample/init-datum))
  (set! pc-sample/code-block-buffer/make/purified-blocks
	(generate:pc-sample/buffer-maker code-block-profile-buffer/purified/length))
  (set! pc-sample/code-block-buffer/make/purified-offsets
	(generate:pc-sample/buffer-maker code-block-profile-buffer/purified/length))
  (set! pc-sample/code-block-buffer/make/heathen-blocks
	(generate:pc-sample/buffer-maker code-block-profile-buffer/heathen/length))
  (set! pc-sample/code-block-buffer/make/heathen-offsets
	(generate:pc-sample/buffer-maker code-block-profile-buffer/heathen/length))
  (set! pc-sample/interp-proc-buffer/make
	(generate:pc-sample/buffer-maker interp-proc-profile-buffer/length))
  (set! pc-sample/prob-comp-table/make	
	(generate:pc-sample/counter-maker pc-sample/init-datum))
  (set! pc-sample/UFO-table/make	
	(generate:pc-sample/counter-maker pc-sample/init-datum))
  )

(define    (code-block-profile-buffer/purified/length) ; annoying alias
  (purified-code-block-profile-buffer/length))
(define    (code-block-profile-buffer/heathen/length)  ; disturbing alias
  ( heathen-code-block-profile-buffer/length))

(define (pc-sample/init-datum)
  "()\n\
   The initial PC sampling profile datum for each profiling table entry.\n\
   This is a convenient data abstraction for later extending profiling\n\
   data to be more than mere counts. More elaborate histograms are envisioned,\
   including gathering of timing and type statistics.\
  "
;------------------------------------------------------------------------------
; HORROR!  When I used a constant 0.0, I found it shared throughout the
;          profile data structures... I think maybe my C manipulation is
;          updating in place rather than storing back into the vector(s).
;          Dr.Adams assisted me in defining this adorable little work around
;          as a means of confusing the compiler into CONS-ing up a bunch o'
;          floating point 0.0's.
;------------------------------------------------------------------------------
  (massive-kludge *kludgey-constant*))	; for now, just a count

(define *kludgey-constant* (flo:+ 37. 42.))

(define (massive-kludge x)
  (flo:- x *kludgey-constant*))
;--------------------------------END-OF-HORROR---------------------------------

;; Profile hashtables (for interp-procs [pcsiproc] & code blocks [pcscobl])

(define make-profile-hash-table    )
(define      profile-hash-table-car)
(define      profile-hash-table-cdr)

(define (install-profile-hash-table)

;;;(set! make-profile-hash-table     make-weak-eq-hash-table);   weakly held
;;;(set!      profile-hash-table-car weak-car)
;;;(set!      profile-hash-table-cdr weak-cdr)

  (set! make-profile-hash-table				; strongly held
	(strong-hash-table/constructor (lambda (obj modulus)
					 (modulo (object-hash obj) modulus))
				       eq?
				       #T))
  (set! profile-hash-table-car car)
  (set! profile-hash-table-cdr cdr)
  )

;; Old value caches

;; Returns the profiling status in effect just before the last reset of any\n\
;; PC sampling profile table.\

(define-structure (pc-sample/status/previous-record
		   (conc-name	pc-sample/status/previous/)
		   (constructor pc-sample/status/previous
				(#!optional builtin-table
					    utility-table
					    primitive-table
					    code-block-table
					    code-block-buffer/status
					    interp-proc-table
					    interp-proc-buffer/status
					    prob-comp-table
					    UFO-table)))
  (builtin-table		(pc-sample/builtin-table/old))
  (utility-table		(pc-sample/utility-table/old))
  (primitive-table		(pc-sample/primitive-table/old))
  (code-block-table		(pc-sample/code-block-table/old))
  (code-block-buffer/status	(pc-sample/code-block-buffer/status/previous))
  (interp-proc-table		(pc-sample/interp-proc-table/old))
  (interp-proc-buffer/status	(pc-sample/interp-proc-buffer/status/previous))
  (prob-comp-table		(pc-sample/prob-comp-table/old))
  (UFO-table			(pc-sample/UFO-table/old))
  )

(define *pc-sample/builtin-table/old* #F)
(define (pc-sample/builtin-table/old)
        *pc-sample/builtin-table/old*)

(define *pc-sample/utility-table/old* #F)
(define (pc-sample/utility-table/old)
        *pc-sample/utility-table/old*)

(define *pc-sample/primitive-table/old* #F)
(define (pc-sample/primitive-table/old)
        *pc-sample/primitive-table/old*)

(define (pc-sample/code-block-table/old)
          (code-block-profile-table/old))

(define (pc-sample/code-block-buffer/status/previous)
          (code-block-profile-buffer/status/previous))

(define (pc-sample/interp-proc-table/old)
          (interp-proc-profile-table/old))

(define (pc-sample/interp-proc-buffer/status/previous)
          (interp-proc-profile-buffer/status/previous))

(define *pc-sample/prob-comp-table/old* #F)
(define (pc-sample/prob-comp-table/old)
        *pc-sample/prob-comp-table/old*)

(define *pc-sample/UFO-table/old* #F)
(define (pc-sample/UFO-table/old)
        *pc-sample/UFO-table/old*)

;; quirk... synchronize C buffer state w/ Scheme buffer state

(define-integrable (fixed-interp-proc-profile-buffer/disable)
                         (interp-proc-profile-buffer/disable))
(define-integrable (fixed-interp-proc-profile-buffer/install buffer)
                         (interp-proc-profile-buffer/install buffer))

;; quirks... for export to pcscobl.scm  [temporary kludges]

(define-integrable (fixed-purified-code-block-profile-buffers/disable)
                         (purified-code-block-profile-buffers/disable))
(define-integrable ( fixed-heathen-code-block-profile-buffers/disable)
                         ( heathen-code-block-profile-buffers/disable))

(define-integrable (fixed-purified-code-block-profile-buffers/install buff1
								      buff2)
                         (purified-code-block-profile-buffers/install buff1
								      buff2))
(define-integrable ( fixed-heathen-code-block-profile-buffers/install buff1
								      buff2)
                         ( heathen-code-block-profile-buffers/install buff1
								      buff2))

;; Resetters       TODO: Worry about disabling while copying? Not for now.
;;                       Maybe employ W/O-INTERRUPTS later. Maybe not.

(define (pc-sample/reset #!optional disable?)
  "(#!OPTIONAL disable?)\n\
  Resets all the PC Sampling profile tables and counters, initializing them\n\
  if they have never yet been initialized.\n\
  If the optional DISABLE? argument is supplied, PC Sampling is then\n\
  disabled by virtue of disabling the PC sampling timer interrupt.\n\
  PC sampling can be re-enabled by typing: (PC-SAMPLE/ENABLE)\n\
  \n\
  For more fine grained enabling/disabling of various kinds of sampling data\n\
  consider:\n\
	   \n\
     PC-SAMPLE/BUILTIN/ENABLE,		   PC-SAMPLE/BUILTIN/DISABLE,\n\
     PC-SAMPLE/UTILITY/ENABLE,		   PC-SAMPLE/UTILITY/DISABLE,\n\
     PC-SAMPLE/PRIMITIVE/ENABLE,	   PC-SAMPLE/PRIMITIVE/DISABLE,\n\
     PC-SAMPLE/CODE-BLOCK/ENABLE,	   PC-SAMPLE/CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/PURIFIED-CODE-BLOCK/ENABLE, PC-SAMPLE/PURIFIED-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/HEATHEN-CODE-BLOCK/ENABLE,  PC-SAMPLE/HEATHEN-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/INTERP-PROC/ENABLE,	   PC-SAMPLE/INTERP-PROC/DISABLE,\n\
     PC-SAMPLE/PROB-COMP/ENABLE,	   PC-SAMPLE/PROB-COMP/DISABLE,\n\
     PC-SAMPLE/UFO/ENABLE,		   PC-SAMPLE/UFO/DISABLE\
  "
  (cond ((or (default-object? disable?) (not disable?))
	 (pc-sample/builtin/reset)
	 (pc-sample/utility/reset)
	 (pc-sample/primitive/reset)
	 (pc-sample/code-block/reset)
	 (pc-sample/interp-proc/reset)
	 (pc-sample/prob-comp/reset)
	 (pc-sample/UFO/reset)
	 ;; resetting in itself does not alter the state of the pc-sampling...
	 'RESET)
	(else
	 (pc-sample/builtin/reset	disable?)
	 (pc-sample/utility/reset	disable?)
	 (pc-sample/primitive/reset	disable?)
	 (pc-sample/code-block/reset	disable?)
	 (pc-sample/interp-proc/reset	disable?)
	 (pc-sample/prob-comp/reset	disable?)
	 (pc-sample/UFO/reset		disable?)
	 (cond ((pc-sample/initialized?)
		(pc-sample/set-state! 'DISABLED)
		'RESET-AND-DISABLED)
	       (else
		'STILL-UNINITIALIZED)))))

(define  pc-sample/builtin/reset)
(define  pc-sample/utility/reset)
(define  pc-sample/primitive/reset)
(define (pc-sample/code-block/reset #!optional disable?) ; alias
  (if (or (default-object? disable?) (not disable?))
      (code-block-profile-tables/reset)
      (code-block-profile-tables/reset disable?)))
(define (pc-sample/purified-code-block/reset #!optional disable?) ; alias
  (if (or (default-object? disable?) (not disable?))
      (purified-code-block-profile-tables/reset)
      (purified-code-block-profile-tables/reset disable?)))
(define (pc-sample/heathen-code-block/reset #!optional disable?) ; alias
  (if (or (default-object? disable?) (not disable?))
      (heathen-code-block-profile-tables/reset)
      (heathen-code-block-profile-tables/reset disable?)))
(define (pc-sample/interp-proc/reset #!optional disable?) ; alias
  (if (or (default-object? disable?) (not disable?))
      (interp-proc-profile-table/reset)
      (interp-proc-profile-table/reset disable?)))
(define  pc-sample/prob-comp/reset)
(define  pc-sample/UFO/reset)

;; TODO: Would be very nice to maintain a bit-vector of the states of the
;;       sundry profiling tables: enabled/disabled

(define (generate:pc-sample/table-resetter index save-oldy default-table-maker)
  (lambda (#!optional disable?)
    (save-oldy)
    (let ((enabling? (or (default-object? disable?) (not disable?))))
      (vector-set! (get-fixed-objects-vector)
		   index
		   (if enabling?
		       (default-table-maker)
		       #F))
      (cond (enabling?
	     (cond ((pc-sample/uninitialized?)
		    (pc-sample/set-state! 'RESET)))
	      'RESET-AND-ENABLED)
	    ((pc-sample/uninitialized?)
	     'STILL-UNINITIALIZED)
	    (else
	     ;; TODO: should recognize when the last is disabled and mark
	     ;;       overall sampling state as disabled then.
	     'RESET-AND-DISABLED)))))

;; TODO: To avoid gratuitous cons-ing, really should always maintain two
;;       of each table (current and old) then flip the two on reset, re-
;;       initializing the new current (former old). [double buffer]

(define (install-resetters)
  (set! pc-sample/builtin/reset
	(generate:pc-sample/table-resetter
	    index:pc-sample/builtin-table
	    (lambda () (set! *pc-sample/builtin-table/old*
			     (pc-sample/builtin-table)))
	    pc-sample/builtin-table/make))
  (set! pc-sample/utility/reset
	(generate:pc-sample/table-resetter
	    index:pc-sample/utility-table
	    (lambda () (set! *pc-sample/utility-table/old*
			     (pc-sample/utility-table)))
	    pc-sample/utility-table/make))
  (set! pc-sample/primitive/reset
	(generate:pc-sample/table-resetter
	    index:pc-sample/primitive-table
	    (lambda () (set! *pc-sample/primitive-table/old*
			     (pc-sample/primitive-table)))
	    pc-sample/primitive-table/make))
  (set! pc-sample/prob-comp/reset
	(generate:pc-sample/table-resetter
	    index:pc-sample/prob-comp-table
	    (lambda () (set! *pc-sample/prob-comp-table/old*
			     (pc-sample/prob-comp-table)))
	    pc-sample/prob-comp-table/make))
  (set! pc-sample/UFO/reset
	(generate:pc-sample/table-resetter
	    index:pc-sample/UFO-table
	    (lambda () (set! *pc-sample/UFO-table/old*
			     (pc-sample/UFO-table)))
	    pc-sample/UFO-table/make))
  )

;; Enablers/Disablers

(define (pc-sample/enable)
  "()\n\
  Resets all PC sampling tables and counters and re-starts the PC\n\
  sampling periodic interrupt timer.\n\
  The old state/status of the PC sampling tables and counters can be\n\
  monitored by invoking: (PC-SAMPLE/STATUS/PREVIOUS).\n\
  The evolving state of the PC sampling tables and counters may be monitored\n\
  by invoking: (PC-SAMPLE/STATUS).\n\
  \n\
  For more fine grained enabling/disabling of various kinds of sampling data\n\
  consider:\n\
	   \n\
     PC-SAMPLE/BUILTIN/ENABLE,		   PC-SAMPLE/BUILTIN/DISABLE,\n\
     PC-SAMPLE/UTILITY/ENABLE,		   PC-SAMPLE/UTILITY/DISABLE,\n\
     PC-SAMPLE/PRIMITIVE/ENABLE,	   PC-SAMPLE/PRIMITIVE/DISABLE,\n\
     PC-SAMPLE/CODE-BLOCK/ENABLE,	   PC-SAMPLE/CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/PURIFIED-CODE-BLOCK/ENABLE, PC-SAMPLE/PURIFIED-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/HEATHEN-CODE-BLOCK/ENABLE,  PC-SAMPLE/HEATHEN-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/INTERP-PROC/ENABLE,	   PC-SAMPLE/INTERP-PROC/DISABLE,\n\
     PC-SAMPLE/PROB-COMP/ENABLE,	   PC-SAMPLE/PROB-COMP/DISABLE,\n\
     PC-SAMPLE/UFO/ENABLE,		   PC-SAMPLE/UFO/DISABLE\
  "
        (pc-sample/reset))

(define (pc-sample/disable)
  "()\n\
  Resets all the PC sampling tables and counters then disables the PC\n\
  sampling periodic interrupt timer.\n\
  The old state/status of the PC sampling tables and counters can be\n\
  monitored by invoking: (PC-SAMPLE/STATUS/PREVIOUS).\n\
  \n\
  For more fine grained enabling/disabling of various kinds of sampling data\n\
  consider:\n\
	   \n\
     PC-SAMPLE/BUILTIN/ENABLE,		   PC-SAMPLE/BUILTIN/DISABLE,\n\
     PC-SAMPLE/UTILITY/ENABLE,		   PC-SAMPLE/UTILITY/DISABLE,\n\
     PC-SAMPLE/PRIMITIVE/ENABLE,	   PC-SAMPLE/PRIMITIVE/DISABLE,\n\
     PC-SAMPLE/CODE-BLOCK/ENABLE,	   PC-SAMPLE/CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/PURIFIED-CODE-BLOCK/ENABLE, PC-SAMPLE/PURIFIED-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/HEATHEN-CODE-BLOCK/ENABLE,  PC-SAMPLE/HEATHEN-CODE-BLOCK/DISABLE,\n\
     PC-SAMPLE/INTERP-PROC/ENABLE,	   PC-SAMPLE/INTERP-PROC/DISABLE,\n\
     PC-SAMPLE/PROB-COMP/ENABLE,	   PC-SAMPLE/PROB-COMP/DISABLE,\n\
     PC-SAMPLE/UFO/ENABLE,		   PC-SAMPLE/UFO/DISABLE\
  "
        (pc-sample/reset 'DISABLE))


(define (pc-sample/builtin/enable)     (pc-sample/builtin/reset))
(define (pc-sample/builtin/disable)    (pc-sample/builtin/reset 'DISABLE))

(define (pc-sample/utility/enable)     (pc-sample/utility/reset))
(define (pc-sample/utility/disable)    (pc-sample/utility/reset 'DISABLE))

(define (pc-sample/primitive/enable)   (pc-sample/primitive/reset))
(define (pc-sample/primitive/disable)  (pc-sample/primitive/reset 'DISABLE))

(define (pc-sample/code-block/enable)  (code-block-profile-tables/enable)) ;cob
(define (pc-sample/code-block/disable) (code-block-profile-tables/disable));cob

(define (pc-sample/purified-code-block/enable) (purified-code-block-profile-tables/enable)) ;cob
(define (pc-sample/purified-code-block/disable)(purified-code-block-profile-tables/disable));cob

(define (pc-sample/heathen-code-block/enable)   (heathen-code-block-profile-tables/enable)) ;cob
(define (pc-sample/heathen-code-block/disable)  (heathen-code-block-profile-tables/disable));cob

(define (pc-sample/interp-proc/enable)  (interp-proc-profile-table/enable)) ;clo
(define (pc-sample/interp-proc/disable) (interp-proc-profile-table/disable)) ;clo

(define (pc-sample/prob-comp/enable)  (pc-sample/prob-comp/reset))
(define (pc-sample/prob-comp/disable) (pc-sample/prob-comp/reset 'DISABLE))

(define (pc-sample/UFO/enable)        (pc-sample/UFO/reset))
(define (pc-sample/UFO/disable)       (pc-sample/UFO/reset 'DISABLE))

#|
 |
 |	   --------------------------------------------------
 |	   --------------------------------------------------
 |
 |	     THIS PAGE INTENTIONALLY LEFT VERY NEARLY BLANK
 |
 |	   --------------------------------------------------
 |	   --------------------------------------------------
 |
 |  Seriously, though, user interface hacks moved to a separate file 'cause
 |  I could not decide on a stable set of basic display mechanisms... I leave
 |  it to the SWAT Team to deal with all that rot. For now, see PCDISP.SCM.
 |
 |#

;;; Call-with-pc-sampling

(define *pc-sample/top-level?*      #T)
(define *pc-sample/wan-sampling?*   #F)	; With-Absolutely-No-PC-Sampling
(define *pc-sample/timing?*         #F)
(define *pc-sample/timing-deficit?* #F)

(define *pc-sample/last-sampling-duration-deficit*       0 )
(define *pc-sample/last-sampling-duration-deficit/no-gc* 0.)
(define *pc-sample/last-sampling-duration-deficit/real*  0 )
		

(define (call-with-pc-sampling thunk #!optional untimed? displayer)
  (let ((restart? (and (pc-sample/running?)
		       (begin (pc-sample/stop) ; stop sampling until in d-wind
			      #T))))
    (dynamic-wind
     (lambda () 'restart-sampling-even-when-thunk-craps-out)
     (lambda ()
       (let* ((tople?  *pc-sample/top-level?*)
	      (defle?  *pc-sample/timing-deficit?*)
	      (timing? *pc-sample/timing?*)
	      (timing-up?  (and timing? (not defle?)))
	      (wanna-time? (or (default-object? untimed?) (not untimed?)))
	      (time-it? (and      wanna-time? (not timing?)))
	      (deficit? (and (not wanna-time?)     timing? ))
	      (neficit? (and time-it? defle?)) ; nix enclosing deficit charge
	      )
	 (cond (tople?			; tolerate nesting of cwpcs
		(pc-sample/reset)))	; start afresh inside thunk
	 (cond ((and tople? time-it?)	; erase deficit...
		;;... by first killing all the liberals
		'(for-each (lambda (x) (kill x)) *liberals*)
		(set! *pc-sample/last-sampling-duration-deficit*       0 )
		(set! *pc-sample/last-sampling-duration-deficit/no-gc* 0.)
		(set! *pc-sample/last-sampling-duration-deficit/real*  0 )))
	 (with-values 
	     (lambda ()
	       ;; Uhm... would wrap fluid-let around d-wind body but then it
	       ;;        would be included in the sample/timing: not desirable.
	       (fluid-let ((*pc-sample/top-level?* #F)
			   (*pc-sample/timing?*         (or time-it? timing?))
			   (*pc-sample/timing-deficit?* (or deficit?  defle?)))
		 (dynamic-wind (lambda () (or *pc-sample/wan-sampling?*
					      (pc-sample/start)))
			       (if (eq? wanna-time? timing-up?)   
				   (lambda () (values (thunk)
						      'runtime-fnord!
						      'process-time-fnord!
						      'real-time-fnord!))
				   (lambda ()
				     (let* ((start-rt  (     runtime      ))
					    (start-ptc (process-time-clock))
					    (start-rtc (   real-time-clock))
					    (result    (thunk))
					    (  end-rt  (     runtime      ))
					    (  end-ptc (process-time-clock))
					    (  end-rtc (   real-time-clock)))
				       (pc-sample/stop)	; dun sample following
				       (let ((p-s/no-gc (- end-rt  start-rt ))
					     (p-ticks   (- end-ptc start-ptc))
					     (r-ticks   (- end-rtc start-rtc)))
					 (values result
						 p-s/no-gc
						 p-ticks
						 r-ticks)))))
			       (lambda () (pc-sample/stop)))))
	   (lambda (result process-secs/no-gc process-ticks real-ticks)
	     ;; Probably not the best control paradigm in the world.
	     ;; If you know of a more elegant solution, I'd sure like
	     ;;  to hear it.   -ziggy@ai.mit.edu
	     (cond
	      ((or deficit? neficit?)
	       (let ((t:mixin (if deficit? int:+ int:-))
		     (s:mixin (if deficit? flo:+ flo:-)))
		 (set!          *pc-sample/last-sampling-duration-deficit*
		       (t:mixin *pc-sample/last-sampling-duration-deficit*
				process-ticks))
		 (set!          *pc-sample/last-sampling-duration-deficit/no-gc*
		       (s:mixin *pc-sample/last-sampling-duration-deficit/no-gc*
				process-secs/no-gc))
		 (set!          *pc-sample/last-sampling-duration-deficit/real*
		       (t:mixin *pc-sample/last-sampling-duration-deficit/real*
				real-ticks)))))
	     (cond ((and tople? time-it?)
		    (time-display thunk
				  process-ticks
				  process-secs/no-gc
				  real-ticks)))
	     (cond (tople?
		    (cond ((default-object? displayer)
			   (*pc-sample/default-status-displayer*))
			  (displayer
			   (displayer)))))
	     result))))
     (lambda ()
       (cond (restart?
	      (pc-sample/start)))))))

;;; Time Display

(define *pc-sample/time-display?*                  #T)
(define *pc-sample/time-display/running-time-too?* #T)
(define *pc-sample/time-display/non-gc-time-too?*  #T)

(define *pc-sample/time-display/real-time-too?*    #F)

(define (time-display thunk p-ticks p-secs/no-gc r-ticks)
  ;; not integrable so customizable
  (cond
   (*pc-sample/time-display?*
    (let ((stealth-t       *pc-sample/last-sampling-duration-deficit*      )
	  (stealth-s/no-gc *pc-sample/last-sampling-duration-deficit/no-gc*)
	  (stealth-t/real  *pc-sample/last-sampling-duration-deficit/real* ))
      (let ((  delta-t       (int:- p-ticks      stealth-t      ))
	    (  delta-s/no-gc (flo:- p-secs/no-gc stealth-s/no-gc))
	    (  delta-t/real  (int:- r-ticks      stealth-t/real )))
	(let ((delta-s
	       (flo:round-to-magnification
		(internal-time/ticks->seconds delta-t     )
		*flo:round-to-magnification/scale*))
	      (delta-s/real
	       (flo:round-to-magnification
		(internal-time/ticks->seconds delta-t/real)
		*flo:round-to-magnification/scale*)))
	  (let ((delta-s/gc-only (flo:- delta-s delta-s/no-gc)))
	    (for-each
	     display
	     `("\n;;;"
	       "\n;;; Timed funcall of " ,thunk
	       "\n;;;   took (in secs) " ,delta-s
	       ,@(if *pc-sample/time-display/running-time-too?*
		     `("\n;;;         running: " ,delta-s/no-gc)
		     '())
	       ,@(if *pc-sample/time-display/non-gc-time-too?*
		     `("\n;;;         GC time: " ,delta-s/gc-only)
		     '())
	       ,@(if *pc-sample/time-display/real-time-too?*
		     `("\n;;; wall clock time: " ,delta-s/real)
		     '())
	       "\n;;;\n"
	       ,@(if (fix:zero? stealth-t)
		     '()
		     (let ((stealth-s
			    (flo:round-to-magnification
			     (internal-time/ticks->seconds stealth-t     )
			     *flo:round-to-magnification/scale*))
			   (stealth-s/real
			    (flo:round-to-magnification
			     (internal-time/ticks->seconds stealth-t/real)
			     *flo:round-to-magnification/scale*)))
		       (let ((stealth-s/gc-only
			      (flo:- stealth-s stealth-s/no-gc)))
			 `("\n;;;      discounting " ,stealth-s
			   ,@(if *pc-sample/time-display/running-time-too?*
				 `("\n;;;         running: " ,stealth-s/no-gc)
				 '())
			   ,@(if *pc-sample/time-display/non-gc-time-too?*
				 `("\n;;;         GC time: " ,stealth-s/gc-only)
				 '())
			   ,@(if *pc-sample/time-display/real-time-too?*
				 `("\n;;; wall clock time: " ,stealth-s/real)
				 '())
			   "\n;;;      seconds spent in clandestine activities."
			   "\n;;;\n")))))
	     ))))))))

(define-integrable (flo:round-to-magnification num magnification)
  (flo:/ (flo:round (flo:* num magnification)) magnification))

(define *flo:round-to-magnification/scale* 1000000.)


(define (call-with-builtin-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/builtin/status/display))

(define (call-with-utility-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/utility/status/display))

(define (call-with-primitive-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/primitive/status/display))

(define (call-with-code-block-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/code-block/status/display))

(define (call-with-interp-proc-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/interp-proc/status/display))

(define (call-with-prob-comp-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/prob-comp/status/display))

(define (call-with-UFO-pc-sampling thunk)
  (call-with-pc-sampling thunk pc-sample/UFO/status/display))

;;; With-pc-sampling

(define (with-pc-sampling				  proc . args)
   (call-with-pc-sampling		(lambda () (apply proc	 args))))
(define (with-builtin-pc-sampling			  proc . args)
   (call-with-builtin-pc-sampling	(lambda () (apply proc	 args))))
(define (with-utility-pc-sampling			  proc . args)
   (call-with-utility-pc-sampling	(lambda () (apply proc	 args))))
(define (with-primitive-pc-sampling			  proc . args)
   (call-with-primitive-pc-sampling	(lambda () (apply proc	 args))))
(define (with-code-block-pc-sampling			  proc . args)
   (call-with-code-block-pc-sampling	(lambda () (apply proc	 args))))
(define (with-interp-proc-pc-sampling			  proc . args)
   (call-with-interp-proc-pc-sampling	(lambda () (apply proc	 args))))
(define (with-prob-comp-pc-sampling			  proc . args)
   (call-with-prob-comp-pc-sampling	(lambda () (apply proc	 args))))
(define (with-UFO-pc-sampling				  proc . args)
   (call-with-UFO-pc-sampling		(lambda () (apply proc	 args))))

;;; Call-without-pc-sampling

(define (call-without-pc-sampling thunk #!optional untimed?)
  ;; If UNTIMED? then subtract time in thunk from total time.
  (let ((restart? (and (pc-sample/running?)
		       (begin (pc-sample/stop) ; stop ASAP
			      #T))))
    (dynamic-wind
     (lambda () 'restart-sampling-even-when-thunk-craps-out)
     (lambda ()
       (let* ((tople?  *pc-sample/top-level?*)
	      (defle?  *pc-sample/timing-deficit?*)
	      (timing? *pc-sample/timing?*)
	      (timing-up?  (and timing? (not defle?)))
	      (wanna-time? (or (default-object? untimed?) (not untimed?)))
	      (time-it? (and      wanna-time? (not timing?)))
	      (deficit? (and (not wanna-time?)     timing? ))
	      (neficit? (and time-it? defle?)) ; nix enclosing deficit charge
	      )
	 (cond ((and tople? time-it?)	; erase deficit...
		;;... by first killing all the liberals
		'(for-each (lambda (x) (kill x)) *liberals*)
		(set! *pc-sample/last-sampling-duration-deficit*       0 )
		(set! *pc-sample/last-sampling-duration-deficit/no-gc* 0.)
		(set! *pc-sample/last-sampling-duration-deficit/real*  0 )))
	 ;; Really just want fluid-let around THUNK calls, but what the hay.
	 (fluid-let ((*pc-sample/top-level?*      #F)
		     (*pc-sample/timing?*         (or time-it? timing?))
		     (*pc-sample/timing-deficit?* (or deficit?  defle?)))
	   (if (eq? wanna-time? timing-up?)
	       (thunk)
	       (let* ((start-rt  (     runtime      ))
		      (start-ptc (process-time-clock))
		      (start-rtc (   real-time-clock))
		      (result    (thunk))
		      (  end-rt  (     runtime      ))
		      (  end-ptc (process-time-clock))
		      (  end-rtc (   real-time-clock)))
		 (let ((process-secs/no-gc (- end-rt  start-rt ))
		       (process-ticks      (- end-ptc start-ptc))
		       (real-ticks         (- end-rtc start-rtc)))
		   (cond
		    ((or deficit? neficit?)
		     (let ((t:mixin (if deficit? int:+ int:-))
			   (s:mixin (if deficit? flo:+ flo:-)))
		       (set!     *pc-sample/last-sampling-duration-deficit*
			(t:mixin *pc-sample/last-sampling-duration-deficit*
				 process-ticks))
		       (set!     *pc-sample/last-sampling-duration-deficit/no-gc*
			(s:mixin *pc-sample/last-sampling-duration-deficit/no-gc*
				 process-secs/no-gc))
		       (set!     *pc-sample/last-sampling-duration-deficit/real*
		        (t:mixin *pc-sample/last-sampling-duration-deficit/real*
				 real-ticks)))))
		   (cond ((and tople? time-it?)
			  (time-display thunk
					process-ticks
					process-secs/no-gc
					real-ticks))))
		 result)))))
     (lambda ()
       (cond (restart?
	      (pc-sample/start)))))))

(define (call-without-builtin-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/builtin/status/display))

(define (call-without-utility-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/utility/status/display))

(define (call-without-primitive-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/primitive/status/display))

(define (call-without-code-block-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/code-block/status/display))

(define (call-without-interp-proc-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/interp-proc/status/display))

(define (call-without-prob-comp-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/prob-comp/status/display))

(define (call-without-UFO-pc-sampling thunk)
  (call-without-pc-sampling thunk pc-sample/UFO/status/display))

;;; Without-pc-sampling

(define (without-pc-sampling				   proc . args)
   (call-without-pc-sampling		 (lambda () (apply proc	  args))))
(define (without-builtin-pc-sampling			   proc . args)
   (call-without-builtin-pc-sampling	 (lambda () (apply proc	  args))))
(define (without-utility-pc-sampling			   proc . args)
   (call-without-utility-pc-sampling	 (lambda () (apply proc	  args))))
(define (without-primitive-pc-sampling			   proc . args)
   (call-without-primitive-pc-sampling	 (lambda () (apply proc	  args))))
(define (without-code-block-pc-sampling			   proc . args)
   (call-without-code-block-pc-sampling	 (lambda () (apply proc	  args))))
(define (without-interp-proc-pc-sampling		   proc . args)
   (call-without-interp-proc-pc-sampling (lambda () (apply proc	  args))))
(define (without-prob-comp-pc-sampling			   proc . args)
   (call-without-prob-comp-pc-sampling	 (lambda () (apply proc	  args))))
(define (without-UFO-pc-sampling			   proc . args)
   (call-without-UFO-pc-sampling	 (lambda () (apply proc	  args))))

;;; Call-with-absolutely-no-pc-sampling

(define (call-with-absolutely-no-pc-sampling thunk #!optional untimed?)
  (let ((restart? (and (pc-sample/running?)
		       (begin (pc-sample/stop) ; stop ASAP
			      #T))))
    (dynamic-wind
     (lambda () 'restart-sampling-even-when-thunk-craps-out)
     (lambda () (let ((untimed-arg (and (not (default-object? untimed?))
					untimed?)))
		  (fluid-let ((*pc-sample/wan-sampling?* #T))
		    (call-without-pc-sampling thunk untimed-arg))))
     (lambda () (cond (restart?
		       (pc-sample/start)))))))

(define (call-with-absolutely-no-builtin-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk 
				       pc-sample/builtin/status/display))

(define (call-with-absolutely-no-utility-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/utility/status/display))

(define (call-with-absolutely-no-primitive-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/primitive/status/display))

(define (call-with-absolutely-no-code-block-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/code-block/status/display))

(define (call-with-absolutely-no-interp-proc-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/interp-proc/status/display))

(define (call-with-absolutely-no-prob-comp-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/prob-comp/status/display))

(define (call-with-absolutely-no-UFO-pc-sampling thunk)
  (call-with-absolutely-no-pc-sampling thunk
				       pc-sample/UFO/status/display))

;;; With-absolutely-no-pc-sampling

(define (with-absolutely-no-pc-sampling				      proc . args)
   (call-with-absolutely-no-pc-sampling		    (lambda () (apply proc   args))))
(define (with-absolutely-no-builtin-pc-sampling			      proc . args)
   (call-with-absolutely-no-builtin-pc-sampling	    (lambda () (apply proc   args))))
(define (with-absolutely-no-utility-pc-sampling			      proc . args)
   (call-with-absolutely-no-utility-pc-sampling	    (lambda () (apply proc   args))))
(define (with-absolutely-no-primitive-pc-sampling		      proc . args)
   (call-with-absolutely-no-primitive-pc-sampling   (lambda () (apply proc   args))))
(define (with-absolutely-no-code-block-pc-sampling		      proc . args)
   (call-with-absolutely-no-code-block-pc-sampling  (lambda () (apply proc   args))))
(define (with-absolutely-no-interp-proc-pc-sampling		      proc . args)
   (call-with-absolutely-no-interp-proc-pc-sampling (lambda () (apply proc   args))))
(define (with-absolutely-no-prob-comp-pc-sampling		      proc . args)
   (call-with-absolutely-no-prob-comp-pc-sampling   (lambda () (apply proc   args))))
(define (with-absolutely-no-UFO-pc-sampling			      proc . args)
   (call-with-absolutely-no-UFO-pc-sampling	    (lambda () (apply proc   args))))

;;; Install

(define *pc-sample/install-verbosity?* #F)

(define (install-dynamic-microcode)
  (let ((pcs-directory (system-library-directory-pathname "pcsample")))
    (cond (*pc-sample/install-verbosity?*
	   (newline)
	   (display "Installing dynamic microcode...")
	   (newline)))
    (cond ((not (implemented-primitive-procedure? ; avoid ucode re-loads
		 (make-primitive-procedure '%pc-sample/install-microcode 0)))
	   (let ((filename
		  (->namestring (merge-pathnames "pcsdld.sl" pcs-directory))))
	     (newline)
	     (write-string ";Loading ")
	     (write-string filename)
	     (let* ((handle ((make-primitive-procedure 'load-object-file)
			     filename))
		    (cth ((make-primitive-procedure 'object-lookup-symbol)
			  handle "initialize_pcsample_primitives" 0)))
	       (write-string " -- done")
	       ((make-primitive-procedure 'invoke-c-thunk) cth)))))))

(define (pc-sample/install-microcode-frobs)
  (cond (*pc-sample/install-verbosity?*
	 (newline)
	 (display "Installing microcode frobs...")
	 (newline)))
  (let ((win? (%pc-sample/install-microcode)))
    (cond ((not win?)
	   (error "\nCould not install PC Sample GC synch hooks.\
                   \nGame over."))))
  unspecific)

(define (pc-sample/disable-microcode-frobs)
  (cond (*pc-sample/install-verbosity?*
	 (newline)
	 (display "Disabling microcode frobs...")
	 (newline)))
  (let ((win? (%pc-sample/disable-microcode)))
    (cond ((not win?)
	   (error "\nCould not disable PC Sample GC synch hooks.\
                   \nGame over."))))
  unspecific)

(define (install)
  ;; Dynamically load microcode
  (install-dynamic-microcode)
  (add-event-receiver! event:after-restore install-dynamic-microcode)
  ;; Install runtime stuff...
  (install-indices)
  (install-accessors)
  (install-makers)
  (install-resetters)
  (install-profile-hash-table)
  ;; Install microcode structures
  (pc-sample/install-microcode-frobs)
  (add-event-receiver! event:after-restore pc-sample/install-microcode-frobs)
  (add-event-receiver! event:before-exit   pc-sample/disable-microcode-frobs)
  ;; HACK: reinitialize the variable when this code is disk-restored so
  ;;       we can post way-cool bands to the Internet News servers.
  (install-current-user-name-promise)
  (add-event-receiver! event:after-restore install-current-user-name-promise)
  ;; Stop sampling at inauspicious occassions...
  (add-event-receiver! event:after-restore pc-sample/stop)
  (add-event-receiver! event:before-exit   pc-sample/stop)
  )

;;; fini
