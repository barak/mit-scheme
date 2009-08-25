#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; System Clock
;;; package: (runtime system-clock)

(declare (usual-integrations))

(define (initialize-package!)
  (reset-system-clock!)
  (add-event-receiver! event:after-restore reset-system-clock!))

(define (reset-system-clock!)
  (set! offset-time (process-time-clock))
  (set! non-runtime 0))

(define offset-time)
(define non-runtime)

(define-integrable process-time-clock
  (ucode-primitive system-clock 0))

(define-integrable real-time-clock
  (ucode-primitive real-time-clock 0))

(define (system-clock)
  (process->system-time (process-time-clock)))

(define (runtime)
  (process->system-time (- (process-time-clock) non-runtime)))

(define (increment-non-runtime! ticks)
  (set! non-runtime (+ non-runtime ticks)))

(define (measure-interval runtime? thunk)
  (let ((start (process-time-clock)))
    (let ((receiver (thunk (process->system-time start))))
      (let ((end (process-time-clock)))
	(if (not runtime?)
	    (increment-non-runtime! (- end start)))
	(receiver (process->system-time end))))))

(define (process->system-time ticks)
  (internal-time/ticks->seconds (- ticks offset-time)))

(define (internal-time/ticks->seconds ticks)
  (/ (exact->inexact ticks) 1000))

(define (internal-time/seconds->ticks seconds)
  (round->exact (* seconds 1000)))

(define (with-timings thunk receiver)
  (let ((process-start  (process-time-clock))
	(gc-time-start  non-runtime)
	(real-start     (real-time-clock)))
    (let ((value (thunk)))
      (let ((process-end  (process-time-clock))
	    (gc-time-end  non-runtime)
	    (real-end     (real-time-clock)))
	(let ((process-time (- process-end process-start))
	      (gc-time      (- gc-time-end gc-time-start))
	      (real-time    (- real-end real-start)))
	  (receiver (- process-time gc-time)
		    gc-time
		    real-time)
	  value)))))