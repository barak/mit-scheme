#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcstat.scm,v 14.5 1991/11/26 07:06:07 cph Exp $

Copyright (c) 1988-1991 Massachusetts Institute of Technology

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

;;;; GC Statistics
;;; package: (runtime gc-statistics)

(declare (usual-integrations))

(define (initialize-package!)
  (set! hook/record-statistic! default/record-statistic!)
  (set! history-modes
	`((NONE . ,none:install-history!)
	  (BOUNDED . ,bounded:install-history!)
	  (UNBOUNDED . ,unbounded:install-history!)))
  (set-history-mode! 'BOUNDED)
  (set! timestamp (cons 0 0))
  (statistics-reset!)
  (add-event-receiver! event:after-restore statistics-reset!)
  (set! hook/gc-start recorder/gc-start)
  (set! hook/gc-finish recorder/gc-finish)
  unspecific)

(define (recorder/gc-start)
  (port/gc-start (nearest-cmdl/port))
  (set! this-gc-start-clock (real-time-clock))
  (set! this-gc-start (process-time-clock))
  unspecific)

(define (recorder/gc-finish ignored space-remaining)
  ignored
  (let* ((end-time (process-time-clock))
	 (end-time-clock (real-time-clock)))
    (increment-non-runtime! (- end-time this-gc-start))
    (statistics-flip this-gc-start end-time
		     space-remaining
		     this-gc-start-clock end-time-clock))
  (port/gc-finish (nearest-cmdl/port)))

(define timestamp)
(define total-gc-time)
(define last-gc-start)
(define last-gc-end)
(define this-gc-start)
(define this-gc-start-clock)
(define last-gc-start-clock)
(define last-gc-end-clock)

(define (gc-timestamp)
  timestamp)

(define (statistics-reset!)
  (set! timestamp (cons 1 (1+ (cdr timestamp))))
  (set! total-gc-time 0)
  (set! last-gc-start-clock false)
  (set! last-gc-end-clock (real-time-clock))
  (set! last-gc-start false)
  (set! last-gc-end (process-time-clock))
  (reset-recorder! '()))

(define-structure (gc-statistic (conc-name gc-statistic/))
  (timestamp false read-only true)
  (heap-left false read-only true)
  (this-gc-start false read-only true)
  (this-gc-end false read-only true)
  (last-gc-start false read-only true)
  (last-gc-end false read-only true)
  (this-gc-start-clock false read-only true)
  (this-gc-end-clock false read-only true)
  (last-gc-start-clock false read-only true)
  (last-gc-end-clock false read-only true))

(define (statistics-flip start-time end-time heap-left start-clock end-clock)
  (let ((statistic
	 (make-gc-statistic timestamp heap-left
			    start-time end-time
			    last-gc-start last-gc-end
			    start-clock end-clock
			    last-gc-start-clock last-gc-end-clock)))
    (set! timestamp (cons (1+ (car timestamp)) (cdr timestamp)))
    (set! total-gc-time (+ (- end-time start-time) total-gc-time))
    (set! last-gc-start start-time)
    (set! last-gc-end end-time)
    (set! last-gc-start-clock start-clock)
    (set! last-gc-end-clock end-clock)
    (record-statistic! statistic)
    (hook/record-statistic! statistic)))

(define (gc-statistic/meter stat)
  (car (gc-statistic/timestamp stat)))

(define hook/record-statistic!)

(define (default/record-statistic! statistic)
  statistic
  false)

(define (gctime)
  (internal-time/ticks->seconds total-gc-time))

;;;; Statistics Recorder

(define last-statistic)
(define history)

(define (reset-recorder! old)
  (set! last-statistic false)
  (reset-history! old))

(define (record-statistic! statistic)
  (set! last-statistic statistic)
  (record-in-history! statistic))

(define (gc-statistics)
  (let ((history (get-history)))
    (if (null? history)
	(if last-statistic
	    (list last-statistic)
	    '())
	history)))

;;;; History Modes

(define reset-history!)
(define record-in-history!)
(define get-history)
(define history-mode)

(define (gc-history-mode #!optional new-mode)
  (let ((old-mode history-mode))
    (if (not (default-object? new-mode))
	(let ((old-history (get-history)))
	  (set-history-mode! new-mode)
	  (reset-history! old-history)))
    old-mode))

(define (set-history-mode! mode)
  (let ((entry (assq mode history-modes)))
    (if (not entry)
	(error "Bad mode name" 'SET-HISTORY-MODE! mode))
    ((cdr entry))
    (set! history-mode (car entry))))

(define history-modes)

;;; NONE

(define (none:install-history!)
  (set! reset-history! none:reset-history!)
  (set! record-in-history! none:record-in-history!)
  (set! get-history none:get-history))

(define (none:reset-history! old)
  old
  (set! history '()))

(define (none:record-in-history! item)
  item
  'DONE)

(define (none:get-history)
  '())

;;; BOUNDED

(define history-size 8)

(define (copy-to-size l size)
  (let ((max (length l)))
    (if (>= max size)
	(list-head l size)
	(append (list-head l max)
		(make-list (- size max) '())))))

(define (bounded:install-history!)
  (set! reset-history! bounded:reset-history!)
  (set! record-in-history! bounded:record-in-history!)
  (set! get-history bounded:get-history))

(define (bounded:reset-history! old)
  (set! history (apply circular-list (copy-to-size old history-size))))

(define (bounded:record-in-history! item)
  (set-car! history item)
  (set! history (cdr history)))

(define (bounded:get-history)
  (let loop ((scan (cdr history)))
    (cond ((eq? scan history) '())
	  ((null? (car scan)) (loop (cdr scan)))
	  (else (cons (car scan) (loop (cdr scan)))))))

;;; UNBOUNDED

(define (unbounded:install-history!)
  (set! reset-history! unbounded:reset-history!)
  (set! record-in-history! unbounded:record-in-history!)
  (set! get-history unbounded:get-history))

(define (unbounded:reset-history! old)
  (set! history old))

(define (unbounded:record-in-history! item)
  (set! history (cons item history)))

(define (unbounded:get-history)
  (reverse history))