#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcnote.scm,v 14.6 1989/10/26 06:46:11 cph Rel $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

(define (with-gc-notification! notify? thunk)
  (fluid-let ((hook/record-statistic!
	       (if notify? gc-notification default/record-statistic!)))
    (thunk)))

(define (gc-notification statistic)
  (with-output-to-port (cmdl/output-port (nearest-cmdl))
    (lambda ()
      (print-statistic statistic))))

(define (print-gc-statistics)
  (let ((status ((ucode-primitive gc-space-status))))
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
  (for-each print-statistic (gc-statistics)))

(define (print-statistic statistic)
  (newline)
  (write-string (gc-statistic->string statistic)))

(define (gc-statistic->string statistic)
  (let ((delta-time
	 (- (gc-statistic/this-gc-end statistic)
	    (gc-statistic/this-gc-start statistic))))
    (string-append "GC #"
		   (number->string (gc-statistic/meter statistic))
		   " took: "
		   (number->string (internal-time/ticks->seconds delta-time))
		   " ("
		   (number->string
		    (round->exact
		     (* (/ delta-time 
			   (- (gc-statistic/this-gc-end statistic)
			      (gc-statistic/last-gc-end statistic)))
			100)))
		   "%) free: "
		   (number->string (gc-statistic/heap-left statistic)))))