#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/gcnote.scm,v 14.2 1988/06/13 11:45:12 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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
  *the-non-printing-object*)

(define (gc-notification statistic)
  (with-output-to-port (cmdl/output-port (nearest-cmdl))
    (lambda ()
      (print-statistic statistic))))

(define (print-gc-statistics)  (for-each print-statistic (gc-statistics)))

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
		    (round (* (/ delta-time 
				 (- (gc-statistic/this-gc-end statistic)
				    (gc-statistic/last-gc-end statistic)))
			      100)))		   "%) free: "
		   (number->string (gc-statistic/heap-left statistic)))))