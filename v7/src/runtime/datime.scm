#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/datime.scm,v 14.2 1989/02/28 17:05:50 cph Rel $

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

;;;; Date and Time Routines
;;; package: (runtime date/time)

(declare (usual-integrations))

;;;; Decoded Time

;;; Based on Common Lisp definition.  Needs time zone stuff, and
;;; handling of abbreviated year specifications.

(define-structure (decoded-time (conc-name decoded-time/))
  (second false read-only true)
  (minute false read-only true)
  (hour false read-only true)
  (day false read-only true)
  (month false read-only true)
  (year false read-only true)
  (day-of-week false read-only true))

(define (get-decoded-time)
  ;; Can return false, indicating that we don't know the time.
  (let ((day ((ucode-primitive current-day)))
	(month ((ucode-primitive current-month)))
	(year ((ucode-primitive current-year))))
    (and year
	 (let ((year (+ year 1900)))
	   (make-decoded-time
	    ((ucode-primitive current-second))
	    ((ucode-primitive current-minute))
	    ((ucode-primitive current-hour))
	    day
	    month
	    year
	    (zellers-congruence day month year))))))

(define (zellers-congruence day month year)
  (let ((qr (integer-divide year 100)))
    (let ((month (1+ (modulo (- month 3) 12)))
	  (year (integer-divide-remainder qr))
	  (century (integer-divide-quotient qr)))
      (modulo (-1+ (- (+ day
			 (quotient (-1+ (* 13 month)) 5)
			 year
			 (quotient year 4)
			 (quotient century 4))
		      (+ (* 2 century)
			 (if (zero? (remainder year 4))
			     (* 2 (quotient month 11))
			     (quotient month 11)))))
	      7))))

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