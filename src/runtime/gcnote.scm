#| -*-Scheme-*-

$Id: gcnote.scm,v 14.18 2005/07/31 02:58:39 cph Exp $

Copyright 1988,1989,1991,1993,1995,2000 Massachusetts Institute of Technology
Copyright 2005 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; GC Notification
;;; package: (runtime gc-notification)

(declare (usual-integrations))

(define (toggle-gc-notification!)
  (set! hook/record-statistic!
	(let ((current hook/record-statistic!))
	  (cond ((eq? current gc-notification) default/record-statistic!)
		((eq? current default/record-statistic!) gc-notification)
		(else (error "Can't grab GC statistics hook")))))
  unspecific)

(define (set-gc-notification! #!optional on?)
  (let ((on? (if (default-object? on?) #T on?)))
    (set! hook/record-statistic!
	  (let ((current hook/record-statistic!))
	    (if (or (eq? current gc-notification)
		    (eq? current default/record-statistic!))
		(if on?
		    gc-notification
		    default/record-statistic!)
		(error "Can't grab GC statistics hook"))))
    unspecific))
    
(define (with-gc-notification! notify? thunk)
  (fluid-let ((hook/record-statistic!
	       (if notify? gc-notification default/record-statistic!)))
    (thunk)))

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
		 (write-number n-words 8)
		 (write-string " words = ")
		 (write-number (quotient n-words 1024) 5)
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
	       7))))))
	     
    (string-append ";GC #"
		   (number->string (gc-statistic/meter statistic))
		   ": took: "
		   (intervals->string
		    (gc-statistic/this-gc-start statistic)
		    (gc-statistic/this-gc-end statistic)
		    (gc-statistic/last-gc-end statistic))
		   " CPU time, "
		   (intervals->string
		    (gc-statistic/this-gc-start-clock statistic)
		    (gc-statistic/this-gc-end-clock statistic)
		    (gc-statistic/last-gc-end-clock statistic))
		   " real time; free: "
		   (number->string (gc-statistic/heap-left statistic)))))