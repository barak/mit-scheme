#| -*-Scheme-*-

$Id: datime.scm,v 14.17 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988-1999 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

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
		   (constructor allocate-decoded-time ())
		   (copier))
  (second #f read-only #t)
  (minute #f read-only #t)
  (hour #f read-only #t)
  (day #f read-only #t)
  (month #f read-only #t)
  (year #f read-only #t)
  (day-of-week #f)
  (daylight-savings-time #f read-only #t)
  (zone #f))

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
				 -1
				 #f)))))
    ;; These calls fill in the other fields of the structure.
    ;; ENCODE-TIME can easily signal an error, for example on unix
    ;; machines when the time is prior to 1970.
    (let ((t (ignore-errors (lambda () ((ucode-primitive encode-time 1) dt)))))
      (if (condition? t)
	  (set-decoded-time/day-of-week! dt #f)
	  ((ucode-primitive decode-time 2) dt t)))
    (if (decoded-time/zone dt)
	(set-decoded-time/zone! dt (/ (decoded-time/zone dt) 3600)))
    dt))

(define (decode-universal-time time)
  (let ((result (allocate-decoded-time)))
    ((ucode-primitive decode-time 2) result (- time epoch))
    (if (decoded-time/zone result)
	(set-decoded-time/zone! result (/ (decoded-time/zone result) 3600)))
    result))

(define (encode-universal-time dt)
  (+ ((ucode-primitive encode-time 1)
      (if (decoded-time/zone dt)
	  (let ((dt* (copy-decoded-time dt)))
	    (set-decoded-time/zone! dt* (* (decoded-time/zone dt*) 3600))
	    dt*)
	  dt))
     epoch))

(define (get-universal-time)
  (+ epoch ((ucode-primitive encoded-time 0))))

(define epoch 2208988800)

(define (get-decoded-time)
  (decode-universal-time (get-universal-time)))

(define (time-zone? object)
  (and (number? object)
       (exact? object)
       (<= -24 object 24)
       (integer? (* 3600 object))))

(define (decoded-time/daylight-savings-time? dt)
  (> (decoded-time/daylight-savings-time dt) 0))

(define (decoded-time/date-string time)
  (string-append (let ((day (decoded-time/day-of-week time)))
		   (if day
		       (string-append (day-of-week/long-string day) " ")
		       ""))
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

(define (universal-time->string time)
  (decoded-time->string (decode-universal-time time)))

(define (file-time->string time)
  (decoded-time->string (decode-file-time time)))

(define (decoded-time->string dt)
  ;; The returned string is in the format specified by RFC 822,
  ;; "Standard for the Format of ARPA Internet Text Messages",
  ;; provided that time-zone information is available from the C
  ;; library.
  (let ((d2 (lambda (n) (string-pad-left (number->string n) 2 #\0))))
    (string-append (let ((day (decoded-time/day-of-week dt)))
		     (if day
			 (string-append (day-of-week/short-string day) ", ")
			 ""))
		   (number->string (decoded-time/day dt))
		   " "
		   (month/short-string (decoded-time/month dt))
		   " "
		   (number->string (decoded-time/year dt))
		   " "
		   (d2 (decoded-time/hour dt))
		   ":"
		   (d2 (decoded-time/minute dt))
		   ":"
		   (d2 (decoded-time/second dt))
		   (let ((zone (decoded-time/zone dt)))
		     (if zone
			 (string-append
			  " "
			  (time-zone->string
			   (if (decoded-time/daylight-savings-time? dt)
			       (- zone 1)
			       zone)))
			 "")))))

(define (time-zone->string tz)
  (if (not (time-zone? tz))
      (error:wrong-type-argument tz "time zone" 'TIME-ZONE->STRING))
  (let ((minutes (round (* 60 (- tz)))))
    (let ((qr (integer-divide (abs minutes) 60))
	  (d2 (lambda (n) (string-pad-left (number->string n) 2 #\0))))
      (string-append (if (< minutes 0) "-" "+")
		     (d2 (integer-divide-quotient qr))
		     (d2 (integer-divide-remainder qr))))))

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

(define (day-of-week/short-string day)
  (guarantee-day-of-week day 'DAY-OF-WEEK/SHORT-STRING)
  (vector-ref '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day))

(define (day-of-week/long-string day)
  (guarantee-day-of-week day 'DAY-OF-WEEK/LONG-STRING)
  (vector-ref '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday"
			  "Saturday" "Sunday")
	      day))

(define (guarantee-day-of-week day name)
  (if (not (exact-integer? day))
      (error:wrong-type-argument day "day-of-week integer" name))
  (if (not (<= 0 day 6))
      (error:bad-range-argument day name)))