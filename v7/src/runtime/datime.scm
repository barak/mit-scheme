#| -*-Scheme-*-

$Id: datime.scm,v 14.10 1995/04/23 03:19:48 cph Exp $

Copyright (c) 1988-95 Massachusetts Institute of Technology

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
		   (constructor %make-decoded-time)
		   (constructor allocate-decoded-time ()))
  (second #f read-only #t)
  (minute #f read-only #t)
  (hour #f read-only #t)
  (day #f read-only #t)
  (month #f read-only #t)
  (year #f read-only #t)
  (day-of-week #f read-only #t)
  (daylight-savings-time #f read-only #t))

(define (make-decoded-time second minute hour day month year)
  (let ((dt
	 (let ((limit
		(lambda (low number high)
		  (cond ((< number low) low)
			((> number high) high)
			(else number)))))
	   (let ((month (limit 1 month 12)))
	     (%make-decoded-time (limit 0 second 59)
				 (limit 0 minute 59)
				 (limit 0 hour 23)
				 (limit 1 day (month/max-days month))
				 month
				 (if (< year 0) 0 year)
				 0
				 -1)))))
    ;; These calls fill in the other fields of the structure.
    ((ucode-primitive decode-time 2) dt ((ucode-primitive encode-time 1) dt))
    dt))

(define (month/max-days month)
  (guarantee-month month 'MONTH/MAX-DAYS)
  (vector-ref '#(31 29 31 30 31 30 31 31 30 31 30 31) (- month 1)))

(define (month/short-string month)
  (guarantee-month month 'MONTH/SHORT-STRING)
  (vector-ref '#("Jan" "Feb" "Mar" "Apr" "May" "Jun"
		       "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
	      (- month 1)))

(define (month/long-string month)
  (guarantee-month month 'MONTH/LONG-STRING)
  (vector-ref '#("January" "February" "March" "April" "May" "June"
			   "July" "August" "September" "October"
			   "November" "December")
	      (- month 1)))

(define (guarantee-month month name)
  (if (not (exact-integer? month))
      (error:wrong-type-argument month "month integer" name))
  (if (not (<= 1 month 12))
      (error:bad-range-argument month name)))

(define (decode-universal-time time)
  (let ((result (allocate-decoded-time)))
    ((ucode-primitive decode-time 2) result time)
    result))

(define (encode-universal-time dt)
  ((ucode-primitive encode-time 1) dt))

(define (get-universal-time)
  ((ucode-primitive encoded-time 0)))

(define (get-decoded-time)
  (decode-universal-time (get-universal-time)))

(define (decoded-time/date-string time)
  (string-append
   (if (decoded-time/day-of-week time)
       (string-append
	(vector-ref '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
				"Saturday" "Sunday")
		    (decoded-time/day-of-week time))
	" ")
       "")
   (month/long-string (decoded-time/month time))
   " "
   (number->string (decoded-time/day time))
   ", "
   (number->string (decoded-time/year time))))

(define (decoded-time/time-string time)
  (let ((second (decoded-time/second time))
	(minute (decoded-time/minute time))
	(hour (decoded-time/hour time)))
    (string-append (number->string
		    (cond ((zero? hour) 12)
			  ((< hour 13) hour)
			  (else (- hour 12))))
		   (if (< minute 10) ":0" ":")
		   (number->string minute)
		   (if (< second 10) ":0" ":")
		   (number->string second)
		   " "
		   (if (< hour 12) "AM" "PM"))))

(define (time-zone? object)
  (and (number? object)
       (exact? object)
       (<= -24 object 24)
       (integer? (* 3600 object))))

(define (time-zone->string tz)
  (if (not (time-zone? tz))
      (error:wrong-type-argument tz "time zone" 'TIME-ZONE->STRING))
  (let ((minutes (round (* 60 (- tz)))))
    (let ((qr (integer-divide (abs minutes) 60))
	  (d2 (lambda (n) (string-pad-left (number->string n) 2 #\0))))
      (string-append (if (< minutes 0) "-" "+")
		     (d2 (integer-divide-quotient qr))
		     (d2 (integer-divide-remainder qr))))))

(define (decoded-time/daylight-savings-time? dt)
  (> (decoded-time/daylight-savings-time dt) 0))