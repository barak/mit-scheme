#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/datime.scm,v 14.3 1990/06/21 23:19:39 cph Rel $

Copyright (c) 1988, 1990 Massachusetts Institute of Technology

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

;;;; Date and Time Routines
;;; package: (runtime date/time)

(declare (usual-integrations))

;;;; Decoded Time

;;; Based on Common Lisp definition.  Needs time zone stuff, and
;;; handling of abbreviated year specifications.

(define decoded-time-structure-tag "decoded-time")

(define-structure (decoded-time
		   (type vector)
		   (named decoded-time-structure-tag)
		   (conc-name decoded-time/)
		   (constructor false))
  (second false read-only true)
  (minute false read-only true)
  (hour false read-only true)
  (day false read-only true)
  (month false read-only true)
  (year false read-only true)
  (day-of-week false read-only true))

(define (get-decoded-time)
  ((ucode-primitive get-decoded-time 1) decoded-time-structure-tag))

(define (decoded-time/date-string time)
  (string-append
   (vector-ref '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
			   "Saturday" "Sunday")
	       (decoded-time/day-of-week time))
   " "
   (vector-ref '#("January" "February" "March" "April" "May" "June"
			    "July" "August" "September" "October"
			    "November" "December")
	       (-1+ (decoded-time/month time)))
   " "
   (write-to-string (decoded-time/day time))
   ", "
   (write-to-string (decoded-time/year time))))

(define (decoded-time/time-string time)
  (let ((second (decoded-time/second time))
	(minute (decoded-time/minute time))
	(hour (decoded-time/hour time)))
    (string-append (write-to-string
		    (cond ((zero? hour) 12)
			  ((< hour 13) hour)
			  (else (- hour 12))))
		   (if (< minute 10) ":0" ":")
		   (write-to-string minute)
		   (if (< second 10) ":0" ":")
		   (write-to-string second)
		   " "
		   (if (< hour 12) "AM" "PM"))))