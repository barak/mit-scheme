;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcstat.scm,v 13.41 1987/01/23 00:13:34 jinx Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
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
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; GC Statistics

(declare (usual-integrations))

(define gctime)
(define gc-statistics)
(define gc-history-mode)

(define gc-statistics-package
  (make-package gc-statistics-package ()

;;;; Statistics Hooks

(define (gc-start-hook) 'DONE)
(define (gc-finish-hook state) 'DONE)

(define ((make-flip-hook old-flip) . More)
  (with-interrupts-reduced INTERRUPT-MASK-NONE
    (lambda (Old-Interrupt-Mask)
     (measure-interval
      #!FALSE		;i.e. do not count the interval in RUNTIME.
      (lambda (start-time)
	(let ((old-state (gc-start-hook)))
	  (let ((new-space-remaining (primitive-datum (apply old-flip more))))
	    (gc-finish-hook old-state)
	    (if (< new-space-remaining 4096)
		(abort->nearest
		 (standard-rep-message "Aborting: Out of memory!")))
	    (lambda (end-time)
	      (statistics-flip start-time
			       end-time
			       new-space-remaining)
	      new-space-remaining))))))))

;;;; Statistics Collector

(define meter)
(define total-gc-time)
(define last-gc-start)
(define last-gc-end)

(define (statistics-reset!)
  (set! meter 1)
  (set! total-gc-time 0)
  (set! last-gc-start #!FALSE)
  (set! last-gc-end (system-clock))
  (reset-recorder! '()))

(define (statistics-flip start-time end-time heap-left)
  (let ((statistic
	 (vector meter
		 start-time end-time
		 last-gc-start last-gc-end
		 heap-left)))
    (set! meter (1+ meter))
    (set! total-gc-time (+ (- end-time start-time) total-gc-time))
    (set! last-gc-start start-time)
    (set! last-gc-end end-time)
    (record-statistic! statistic)))

(set! gctime (named-lambda (gctime) total-gc-time))

;;;; Statistics Recorder

(define last-statistic)
(define history)

(define (reset-recorder! old)
  (set! last-statistic #!FALSE)
  (reset-history! old))

(define (record-statistic! statistic)
  (set! last-statistic statistic)
  (record-in-history! statistic))

(set! gc-statistics
      (named-lambda (gc-statistics)
	(let ((history (get-history)))
	  (if (null? history)
	      (if last-statistic
		  (list last-statistic)
		  '())
	      history))))

;;;; History Modes

(define reset-history!)
(define record-in-history!)
(define get-history)
(define history-mode)

(set! gc-history-mode
      (named-lambda (gc-history-mode #!optional new-mode)
	(let ((old-mode history-mode))
	  (if (not (unassigned? new-mode))
	      (let ((old-history (get-history)))
		(set-history-mode! new-mode)
		(reset-history! old-history)))
	  old-mode)))

(define (set-history-mode! mode)
  (let ((entry (assq mode history-modes)))
    (if (not entry)
	(error "Bad mode name" 'SET-HISTORY-MODE! mode))
    ((cdr entry))
    (set! history-mode (car entry))))

(define history-modes
  `((NONE . ,(named-lambda (none:install-history!)
	       (set! reset-history! none:reset-history!)
	       (set! record-in-history! none:record-in-history!)
	       (set! get-history none:get-history)))
    (BOUNDED . ,(named-lambda (bounded:install-history!)
		  (set! reset-history! bounded:reset-history!)
		  (set! record-in-history! bounded:record-in-history!)
		  (set! get-history bounded:get-history)))
    (UNBOUNDED . ,(named-lambda (unbounded:install-history!)
		    (set! reset-history! unbounded:reset-history!)
		    (set! record-in-history! unbounded:record-in-history!)
		    (set! get-history unbounded:get-history)))))

;;; NONE

(define (none:reset-history! old)
  (set! history '()))

(define (none:record-in-history! item)
  'DONE)

(define (none:get-history)
  '())

;;; BOUNDED

(define history-size 8)

(define (copy-to-size l size)
  (let ((max (length l)))
    (if (>= max size)
	(initial-segment l size)
	(append (initial-segment l max)
		(make-list (- size max) '())))))

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

(define (unbounded:reset-history! old)
  (set! history old))

(define (unbounded:record-in-history! item)
  (set! history (cons item history)))

(define (unbounded:get-history)
  (reverse history))

;;;; Initialization

(define (install!)
  (set-history-mode! 'BOUNDED)
  (statistics-reset!)
  (set! gc-flip (make-flip-hook gc-flip))
  (set! (access stack-overflow garbage-collector-package)
	(named-lambda (stack-overflow)
	  (abort->nearest
	   (standard-rep-message
	    "Aborting: Maximum recursion depth exceeded!"))))
  (set! (access hardware-trap garbage-collector-package)
	(named-lambda (hardware-trap)
	  (abort->nearest
	   (standard-rep-message
	    "Aborting: The hardware trapped!"))))
  (add-event-receiver! event:after-restore statistics-reset!))

;;; end GC-STATISTICS-PACKAGE.
))

;;;; GC Notification

(define toggle-gc-notification!)
(define print-gc-statistics)
(let ()

(define normal-recorder '())

(define (gc-notification statistic)
  (normal-recorder statistic)
  (with-output-to-port (rep-output-port)
    (lambda ()
      (print-statistic statistic))))

(set! toggle-gc-notification!
(named-lambda (toggle-gc-notification!)
  (if (null? normal-recorder)
      (begin (set! normal-recorder
		   (access record-statistic! gc-statistics-package))
	     (set! (access record-statistic! gc-statistics-package)
		   gc-notification))
      (begin (set! (access record-statistic! gc-statistics-package)
		   normal-recorder)
	     (set! normal-recorder '())))
  *the-non-printing-object*))

(set! print-gc-statistics
(named-lambda (print-gc-statistics)
  (for-each print-statistic (gc-statistics))))

(define (print-statistic statistic)
  (apply (lambda (meter
		  this-gc-start this-gc-end
		  last-gc-start last-gc-end
		  heap-left)
	   (let ((delta-time (- this-gc-end this-gc-start)))
	     (newline) (write-string "GC #") (write meter)
	     (write-string " took: ") (write delta-time)
	     (write-string " (")
	     (write (round (* (/ delta-time (- this-gc-end last-gc-end))
			      100)))
	     (write-string "%) free: ") (write heap-left)))
	 (vector->list statistic)))

)
)