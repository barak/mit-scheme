#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020 Massachusetts Institute of Technology

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

;;;; GC Notification
;;; package: (runtime gc-notification)

(declare (usual-integrations))

(add-boot-deps! '(runtime thread))

(define (toggle-gc-notification!)
  (let ((thread (current-thread)))
    (with-thread-mutex-lock threads-to-notify-mutex
      (lambda ()
	(if (registered-thread? thread)
	    (deregister-thread thread)
	    (register-thread thread))))))

(define (set-gc-notification! #!optional on?)
  (let ((on? (if (default-object? on?) #t on?))
	(thread (current-thread)))
    (with-thread-mutex-lock threads-to-notify-mutex
      (lambda ()
	(if on?
	    (register-thread thread)
	    (deregister-thread thread))))))

(define (with-gc-notification! notify? thunk)
  (let ((thread (current-thread))
	(outside))
    (dynamic-wind
     (lambda ()
       (with-thread-mutex-lock threads-to-notify-mutex
	 (lambda ()
	   (set! outside (registered-thread? thread))
	   (if notify?
	       (register-thread thread)
	       (deregister-thread thread)))))
     thunk
     (lambda ()
       (with-thread-mutex-lock threads-to-notify-mutex
	 (lambda ()
	   (if outside
	       (register-thread thread)
	       (deregister-thread thread))
	   (set! outside)
	   unspecific))))))

(define threads-to-notify (weak-list-set eq?))
(define threads-to-notify-mutex (make-thread-mutex))

(define-integrable (registered-thread? thread)
  (weak-list-set-contains? thread threads-to-notify))

(define-integrable (register-thread thread)
  (weak-list-set-add! thread threads-to-notify))

(define-integrable (deregister-thread thread)
  (weak-list-set-delete! thread threads-to-notify))

(define (signal-gc-events)
  (without-interrupts
   (lambda ()
     (let ((statistic last-statistic))

       (define (signal-event thread event)
	 (if (eq? 'dead (thread-execution-state thread))
	     #f
	     (begin
	       (%signal-thread-event thread event)
	       #t)))

       (define (gc-event)
	 (gc-notification statistic))

       (let ((signaled?
	      (if (< (gc-statistic/heap-left statistic) 4096)
		  (if first-running-thread
		      (signal-event first-running-thread abort-heap-low)
		      (let ((thread (console-thread)))
			(if thread
			    (signal-event thread abort-heap-low)
			    #f)))
		  (let ((signaled? #f))
		    (for-each (lambda (thread)
				(if (signal-event thread gc-event)
				    (begin
				      (set! signaled? #t)
				      unspecific)))
			      (weak-list-set->list threads-to-notify))
		    signaled?))))
	 (if signaled?
	     (%maybe-toggle-thread-timer)))))))

(add-boot-init! (lambda () (add-gc-daemon! signal-gc-events)))

;;;; Output

(define (gc-notification statistic)
  (print-statistic statistic (notification-output-port)))

(define (print-gc-statistics)
  (let ((status (gc-space-status)))
    (let ((granularity (vector-ref status 0))
	  (write-number
	   (lambda (n c)
	     (write-string (string-pad-left (number->string n) c)))))
      (let ((report-one
	     (lambda (label low high)
	       (let ((n-words (quotient (- high low) granularity)))
		 (newline)
		 (write-string
		  (string-pad-right (string-append label ": ") 17))
		 (write-number n-words 9)
		 (write-string " words = ")
		 (write-number (quotient n-words 1024) 6)
		 (write-string " blocks")
		 (let ((n-words (remainder n-words 1024)))
		   (write-string " + ")
		   (write-number n-words 4)
		   (write-string " words"))))))
	(let ((report-two
	       (lambda (label low free high)
		 (report-one (string-append label " in use") low free)
		 (report-one (string-append label " free") free high))))
	  (report-two "constant"
		      (vector-ref status 1)
		      (vector-ref status 2)
		      (vector-ref status 3))
	  (report-two "heap"
		      (vector-ref status 4)
		      (vector-ref status 5)
		      (vector-ref status 6))))))
  (for-each (let ((port (current-output-port)))
	      (lambda (statistic)
		(print-statistic statistic port)))
	    (gc-statistics)))

(define (print-statistic statistic port)
  (fresh-line port)
  (write-string (gc-statistic->string statistic) port)
  (newline port))

(define (gc-statistic->string statistic)
  (let* ((ticks/second 1000)
	 (intervals->string
	 (lambda (start end last-end)
	   (let ((gc-length (- end start))
		 (period (- end last-end)))
	     (string-append
	      (string-pad-left
	       (number->string (quotient gc-length ticks/second))
	       3)
	      "."
	      (string-pad-right
	       (number->string
		(round->exact (/ (remainder gc-length ticks/second)
				 10))
		#d10)
	       2
	       #\0)
	      (string-pad-left
	       (string-append
		"("
		(if (zero? period)
		    "100"
		    (number->string
		     (round->exact (* (/ gc-length period) 100))
		     #d10))
		"%)")
	       7)))))
	 (uctime-string	    ;; E.g. "13:15" for 1:15pm local time
	  (lambda (uctime)
	    (let* ((decoded-time
		    (universal-time->local-decoded-time uctime))
		   (hour (decoded-time/hour decoded-time))
		   (minute (decoded-time/minute decoded-time))
		   (second (decoded-time/second decoded-time)))
	      (string-append
	       (if (< hour 10) "0" "") (number->string hour)
	       (if (< minute 10) ":0" ":") (number->string minute)
	       (if (< second 10) ":0" ":") (number->string second))))))
    (string-append ";GC #"
		   (number->string (gc-statistic/meter statistic))
		   " "
		   (uctime-string (gc-statistic/this-gc-start-uctime statistic))
		   ": took: "
		   (intervals->string
		    (gc-statistic/this-gc-start statistic)
		    (gc-statistic/this-gc-end statistic)
		    (gc-statistic/last-gc-end statistic))
		   " CPU, "
		   (intervals->string
		    (gc-statistic/this-gc-start-clock statistic)
		    (gc-statistic/this-gc-end-clock statistic)
		    (gc-statistic/last-gc-end-clock statistic))
		   " real; free: "
		   (number->string (gc-statistic/heap-left statistic)))))