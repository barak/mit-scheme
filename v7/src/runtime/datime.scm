#| -*-Scheme-*-

$Id: datime.scm,v 14.40 2004/06/23 03:45:50 cph Exp $

Copyright 1986,1987,1988,1989,1990,1993 Massachusetts Institute of Technology
Copyright 1995,1996,1997,1999,2000,2003 Massachusetts Institute of Technology
Copyright 2004 Massachusetts Institute of Technology

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
		   (predicate decoded-time?)
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

(define (make-decoded-time second minute hour day month year #!optional zone)
  (check-decoded-time-args second minute hour day month year
			   'MAKE-DECODED-TIME)
  (let ((zone (if (default-object? zone) #f zone)))
    (if (and zone (not (time-zone? zone)))
	(error:wrong-type-argument zone "time zone" 'MAKE-DECODED-TIME))
    (if zone
	(%make-decoded-time second minute hour day month year
			    (compute-day-of-week day month year)
			    0
			    zone)
	(let ((dt
	       (%make-decoded-time second minute hour day month year 0 -1 #f)))
	  ;; These calls fill in the other fields of the structure.
	  ;; ENCODE-TIME can easily signal an error, for example on
	  ;; unix machines when the time is prior to 1970.
	  (let ((t (ignore-errors
		    (lambda () ((ucode-primitive encode-time 1) dt)))))
	    (if (condition? t)
		(set-decoded-time/day-of-week!
		 dt
		 (compute-day-of-week day month year))
		((ucode-primitive decode-time 2) dt t)))
	  (if (decoded-time/zone dt)
	      (set-decoded-time/zone! dt (/ (decoded-time/zone dt) 3600)))
	  dt))))

(define (check-decoded-time-args second minute hour day month year procedure)
  (let ((check-type
	 (lambda (object)
	   (if (not (exact-nonnegative-integer? object))
	       (error:wrong-type-argument object
					  "exact non-negative integer"
					  procedure)))))
    (let ((check-range
	   (lambda (object min max)
	     (check-type object)
	     (if (not (<= min object max))
		 (error:bad-range-argument object procedure)))))
      (check-type year)
      (check-range month 1 12)
      (check-range day 1 (month/max-days month))
      (check-range hour 0 23)
      (check-range minute 0 59)
      (check-range second 0 59))))

(define (compute-day-of-week day month year)
  ;; This implements Zeller's Congruence.
  (modulo (+ day
	     (let ((y (remainder year 100)))
	       (+ y
		  (floor (/ y 4))))
	     (let ((c (quotient year 100)))
	       (- (floor (/ c 4))
		  (* 2 c)))
	     (let ((m (modulo (- month 2) 12)))
	       (- (floor (/ (- (* 13 m) 1) 5))
		  (* (floor (/ m 11))
		     (if (and (= 0 (remainder year 4))
			      (or (not (= 0 (remainder year 100)))
				  (= 0 (remainder year 400))))
			 2
			 1))))
	     ;; This -1 adjusts so that 0 corresponds to Monday.
	     ;; Normally, 0 corresponds to Sunday.
	     -1)
	  7))

(define (universal-time->local-decoded-time time)
  (let ((result (allocate-decoded-time)))
    ((ucode-primitive decode-time 2) result (- time epoch))
    (if (decoded-time/zone result)
	(set-decoded-time/zone! result (/ (decoded-time/zone result) 3600)))
    result))

(define (universal-time->global-decoded-time time)
  (let ((result (allocate-decoded-time)))
    ((ucode-primitive decode-utc 2) result (- time epoch))
    (if (decoded-time/zone result)
	(set-decoded-time/zone! result (/ (decoded-time/zone result) 3600)))
    result))

(define (decoded-time->universal-time dt)
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

(define (local-decoded-time)
  (universal-time->local-decoded-time (get-universal-time)))

(define (global-decoded-time)
  (universal-time->global-decoded-time (get-universal-time)))

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

(define (universal-time->local-time-string time)
  (decoded-time->string (universal-time->local-decoded-time time)))

(define (universal-time->global-time-string time)
  (decoded-time->string (universal-time->global-decoded-time time)))

(define (file-time->local-time-string time)
  (decoded-time->string (file-time->local-decoded-time time)))

(define (file-time->global-time-string time)
  (decoded-time->string (file-time->global-decoded-time time)))

(define (decoded-time->string dt)
  ;; The returned string is in the format specified by RFC 822,
  ;; "Standard for the Format of ARPA Internet Text Messages",
  ;; provided that time-zone information is available from the C
  ;; library.
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
		       ""))))

(define (string->decoded-time string)
  ;; STRING must be in RFC-822 format.
  (let ((lose
	 (lambda ()
	   (error "Ill-formed RFC-822 time string:" string))))
    (let ((tokens
	   (let ((tokens (burst-string string char-set:whitespace #t)))
	     (case (length tokens)
	       ((4)
		;; Workaround for very old mail messages with dates in
		;; the following format: "24 September 1984 18:42-EDT".
		(let ((tokens* (burst-string (list-ref tokens 3) #\- #f)))
		  (if (fix:= 2 (length tokens*))
		      (list (car tokens)
			    (cadr tokens)
			    (caddr tokens)
			    (car tokens*)
			    (cadr tokens*))
		      (lose))))
	       ((5) tokens)
	       ((6)
		(if (and (fix:= 4 (string-length (car tokens)))
			 (char=? #\, (string-ref (car tokens) 3))
			 (string-ci->index days-of-week/short-strings
					   (substring (car tokens) 0 3)))
		    (cdr tokens)
		    (lose)))
	       (else (lose))))))
      (let ((time (burst-string (list-ref tokens 3) #\: #f)))
	(if (not (memv (length time) '(2 3)))
	    (error "Ill-formed RFC-822 time string:" string))
	(make-decoded-time (if (pair? (cddr time))
			       (string->number (caddr time))
			       0)
			   (string->number (cadr time))
			   (string->number (car time))
			   (string->number (list-ref tokens 0))
			   (string->month (list-ref tokens 1))
			   (string->year (list-ref tokens 2))
			   (string->time-zone (list-ref tokens 4)))))))

(define (string->universal-time string)
  (decoded-time->universal-time (string->decoded-time string)))

(define (string->file-time string)
  (decoded-time->file-time (string->decoded-time string)))

(define (time-zone->string tz)
  (if (not (time-zone? tz))
      (error:wrong-type-argument tz "time zone" 'TIME-ZONE->STRING))
  (let ((minutes (round (* 60 (- tz)))))
    (let ((qr (integer-divide (abs minutes) 60)))
      (string-append (if (< minutes 0) "-" "+")
		     (d2 (integer-divide-quotient qr))
		     (d2 (integer-divide-remainder qr))))))

(define (string->time-zone string)
  (let ((entry
	 (list-search-positive named-time-zones
	   (lambda (zone)
	     (string-ci=? string (car zone))))))
    (if entry
	(cadr entry)
	(let ((n (string->number string)))
	  (if (not (and (exact-integer? n)
			(<= -2400 n 2400)))
	      (error "Malformed time zone:" string))
	  (let ((qr (integer-divide (abs n) 100)))
	    (let ((hours (integer-divide-quotient qr))
		  (minutes (integer-divide-remainder qr)))
	      (if (not (<= 0 minutes 59))
		  (error "Malformed time zone:" string))
	      (let ((hours (+ hours (/ minutes 60))))
		(if (< n 0)
		    hours
		    (- hours)))))))))

(define named-time-zones
  '(("UT" 0)
    ("GMT" 0)
    ("EST" 5) ("EDT" 4) ("CST" 6) ("CDT" 5)
    ("MST" 7) ("MDT" 6) ("PST" 8) ("PDT" 7)
    ("A" 1) ("B" 2) ("C" 3) ("D" 4) ("E" 5) ("F" 6)
    ("G" 7) ("H" 8) ("I" 9) ("K" 10) ("L" 11) ("M" 12)
    ("N" -1) ("O" -2) ("P" -3) ("Q" -4) ("R" -5) ("S" -6)
    ("T" -7) ("U" -8) ("V" -9) ("W" -10) ("X" -11) ("Y" -12)
    ("Z" 0)))

;;;; ISO C ctime() strings

(define (decoded-time->ctime-string dt)
  (string-append
   (day-of-week/short-string (decoded-time/day-of-week dt))
   " "
   (month/short-string (decoded-time/month dt))
   " "
   (string-pad-left (number->string (decoded-time/day dt)) 2)
   " "
   (string-pad-left (number->string (decoded-time/hour dt)) 2 #\0)
   ":"
   (string-pad-left (number->string (decoded-time/minute dt)) 2 #\0)
   ":"
   (string-pad-left (number->string (decoded-time/second dt)) 2 #\0)
   " "
   (number->string (decoded-time/year dt))))

(define (ctime-string->decoded-time string #!optional zone)
  (let ((zone (if (default-object? zone) #f zone))
	(lose (lambda () (error "Ill-formed ctime() string:" string))))
    (if (not (or (not zone) (time-zone? zone)))
	(error:wrong-type-argument zone "time zone"
				   'CTIME-STRING->DECODED-TIME))
    (let ((tokens (burst-string string #\space #t)))
      (if (not (fix:= 5 (length tokens)))
	  (lose))
      (let ((time (burst-string (list-ref tokens 3) #\: #f)))
	(case (length time)
	  ((3)
	   (make-decoded-time (string->number (caddr time))
			      (string->number (cadr time))
			      (string->number (car time))
			      (string->number (list-ref tokens 2))
			      (string->month (list-ref tokens 1))
			      (string->year (list-ref tokens 4))
			      zone))
	  ((2)
	   (make-decoded-time 0
			      (string->number (cadr time))
			      (string->number (car time))
			      (string->number (list-ref tokens 2))
			      (string->month (list-ref tokens 1))
			      (string->year (list-ref tokens 4))
			      zone))
	  (else
	   (lose)))))))

(define (universal-time->local-ctime-string time)
  (decoded-time->ctime-string (universal-time->local-decoded-time time)))

(define (universal-time->global-ctime-string time)
  (decoded-time->ctime-string (universal-time->global-decoded-time time)))

(define (ctime-string->universal-time string #!optional zone)
  (decoded-time->universal-time
   (ctime-string->decoded-time string (if (default-object? zone) #f zone))))

(define (file-time->local-ctime-string time)
  (decoded-time->ctime-string (file-time->local-decoded-time time)))

(define (file-time->global-ctime-string time)
  (decoded-time->ctime-string (file-time->global-decoded-time time)))

(define (ctime-string->file-time string #!optional zone)
  (decoded-time->file-time
   (ctime-string->decoded-time string (if (default-object? zone) #f zone))))

;;;; ISO 8601 date/time strings

;;; This implements a subset of the ISO 8601 specification.  It
;;; accepts only complete date+time representations.  It does not
;;; support either truncation or expansion.  On output, it uses a
;;; single format.

(define (iso8601-string->decoded-time string)
  (let ((v (parse-8601-date/time (string->parser-buffer string))))
    (if (not v)
	(error:bad-range-argument string 'ISO8601-STRING->DECODED-TIME))
    (vector-ref v 0)))

(define (decoded-time->iso8601-string dt)
  (string-append (number->string (decoded-time/year dt))
		 "-"
		 (d2 (decoded-time/month dt))
		 "-"
		 (d2 (decoded-time/day dt))
		 " "
		 (d2 (decoded-time/hour dt))
		 ":"
		 (d2 (decoded-time/minute dt))
		 ":"
		 (d2 (decoded-time/second dt))
		 (let ((zone (decoded-time/zone dt)))
		   (if zone
		       (time-zone->string
			(if (decoded-time/daylight-savings-time? dt)
			    (- zone 1)
			    zone))
		       ""))))

(define (universal-time->local-iso8601-string time)
  (decoded-time->iso8601-string (universal-time->local-decoded-time time)))

(define (universal-time->global-iso8601-string time)
  (decoded-time->iso8601-string (universal-time->global-decoded-time time)))

(define (iso8601-string->universal-time string)
  (decoded-time->universal-time (iso8601-string->decoded-time string)))

(define (file-time->local-iso8601-string time)
  (decoded-time->iso8601-string (file-time->local-decoded-time time)))

(define (file-time->global-iso8601-string time)
  (decoded-time->iso8601-string (file-time->global-decoded-time time)))

(define (iso8601-string->file-time string)
  (decoded-time->file-time (iso8601-string->decoded-time string)))

(define parse-8601-date/time
  (*parser
   (encapsulate
       (lambda (v)
	 (let ((date (vector-ref v 0))
	       (time (vector-ref v 1))
	       (zone (vector-ref v 2)))
	   (make-decoded-time (vector-ref time 2)
			      (vector-ref time 1)
			      (vector-ref time 0)
			      (vector-ref date 2)
			      (vector-ref date 1)
			      (vector-ref date 0)
			      (and zone
				   (+ (* (- (vector-ref zone 0))
					 (vector-ref zone 1))
				      (/ (vector-ref zone 2) 60))))))
     (complete
      (seq parse-8601-date
	   (alt "T" " ")
	   parse-8601-time
	   (alt parse-8601-zone (values #f)))))))

(define parse-8601-date
  (*parser
   (alt (encapsulate (lambda (v) v)
	  (seq parse-8601-year
	       (alt (seq "-" parse-8601-month "-" parse-8601-day)
		    (seq parse-8601-month parse-8601-day))))
	(transform week-date->month-date
	  (seq parse-8601-year
	       (alt (seq "-W" parse-8601-week "-" parse-8601-week-day)
		    (seq "W" parse-8601-week parse-8601-week-day))))
	(transform ordinal-date->month-date
	  (seq parse-8601-year
	       (alt (seq "-" parse-8601-ordinal-day)
		    parse-8601-ordinal-day))))))

(define (week-date->month-date v)
  (let ((year (vector-ref v 0))
	(week (vector-ref v 1))
	(day (vector-ref v 2)))
    (let ((dt
	   (let ((dt (make-decoded-time 0 0 0 1 1 year 0)))
	     (universal-time->global-decoded-time
	      (+ (decoded-time->universal-time dt)
		 (* (+ (* 7 (- week 1))
		       (- day (+ (decoded-time/day-of-week dt) 1)))
		    86400))))))
      (and (fix:= (decoded-time/year dt) year)
	   (vector (vector (decoded-time/year dt)
			   (decoded-time/month dt)
			   (decoded-time/day dt)))))))

(define (ordinal-date->month-date v)
  (let ((year (vector-ref v 0))
	(day (vector-ref v 1)))
    (let ((dt
	   (let ((dt (make-decoded-time 0 0 0 1 1 year 0)))
	     (universal-time->global-decoded-time
	      (+ (decoded-time->universal-time dt)
		 (* (- day 1)
		    86400))))))
      (and (fix:= (decoded-time/year dt) year)
	   (vector (vector (decoded-time/year dt)
			   (decoded-time/month dt)
			   (decoded-time/day dt)))))))

(define parse-8601-zone
  (*parser
   (encapsulate (lambda (v) v)
     (alt (transform (lambda (v) v (vector 1 0 0))
		     (match "Z"))
	  (seq parse-8601-sign
	       parse-8601-zone-hour
	       (alt (seq (? ":") parse-8601-minute)
		    (values 0)))))))

(define parse-8601-time
  (*parser
   (transform (lambda (v)
		(if (fix:= (vector-ref v 0) 24)
		    (and (fix:= (vector-ref v 1) 0)
			 (fix:= (vector-ref v 2) 0)
			 (vector (vector 0 0 0)))
		    (vector v)))
     (seq parse-8601-hour
	  (alt (seq ":" parse-8601-minute
		    (alt (seq ":" parse-8601-second)
			 (values 0)))
	       (seq parse-8601-minute
		    (alt parse-8601-second
			 (values 0))))))))

(define (8601-number-parser n-digits low high)
  (let ((parse-digits
	 (case n-digits
	   ((1)
	    (*parser
	     (map string->number
		  (match (char-set char-set:numeric)))))
	   ((2)
	    (*parser
	     (map string->number
		  (match (seq (char-set char-set:numeric)
			      (char-set char-set:numeric))))))
	   ((3)
	    (*parser
	     (map string->number
		  (match (seq (char-set char-set:numeric)
			      (char-set char-set:numeric)
			      (char-set char-set:numeric))))))
	   ((4)
	    (*parser
	     (map string->number
		  (match (seq (char-set char-set:numeric)
			      (char-set char-set:numeric)
			      (char-set char-set:numeric)
			      (char-set char-set:numeric))))))
	   (else
	    (error:bad-range-argument n-digits '8601-NUMBER-PARSER)))))
    (lambda (b)
      (let ((v (parse-digits b)))
	(and v
	     (<= low (vector-ref v 0) high)
	     v)))))

(define parse-8601-year (8601-number-parser 4 1582 9999))
(define parse-8601-month (8601-number-parser 2 1 12))
(define parse-8601-week (8601-number-parser 2 1 53))
(define parse-8601-day (8601-number-parser 2 1 31))
(define parse-8601-week-day (8601-number-parser 1 1 7))
(define parse-8601-ordinal-day (8601-number-parser 3 1 366))
(define parse-8601-hour (8601-number-parser 2 0 24))
(define parse-8601-zone-hour (8601-number-parser 2 0 12))
(define parse-8601-minute (8601-number-parser 2 0 59))

(define parse-8601-second
  (*parser
   (transform (lambda (v)
		(let ((x (string->number (vector-ref v 0))))
		  (and (<= 0 x)
		       (< x 60)
		       (vector (min 59 (round->exact x))))))
	      (match (seq (char-set char-set:numeric)
			  (char-set char-set:numeric)
			  (? (seq "." (* (char-set char-set:numeric)))))))))

(define parse-8601-sign
  (*parser
   (alt (map (lambda (v) v 1) (match "+"))
	(map (lambda (v) v -1) (match "-")))))

;;;; Utilities

(define (month/max-days month)
  (guarantee-month month 'MONTH/MAX-DAYS)
  (vector-ref '#(31 29 31 30 31 30 31 31 30 31 30 31) (- month 1)))

(define (month/short-string month)
  (guarantee-month month 'MONTH/SHORT-STRING)
  (vector-ref month/short-strings (- month 1)))

(define (month/long-string month)
  (guarantee-month month 'MONTH/LONG-STRING)
  (vector-ref month/long-strings (- month 1)))

(define (guarantee-month month name)
  (if (not (exact-integer? month))
      (error:wrong-type-argument month "month integer" name))
  (if (not (<= 1 month 12))
      (error:bad-range-argument month name)))

(define (string->month string)
  (fix:+ 1
	 (or (string-ci->index month/short-strings string)
	     (string-ci->index month/long-strings string)
	     (error "Unknown month designation:" string))))

(define month/short-strings
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define month/long-strings
  '#("January" "February" "March" "April" "May" "June" "July" "August"
	       "September" "October" "November" "December"))

(define (day-of-week/short-string day)
  (guarantee-day-of-week day 'DAY-OF-WEEK/SHORT-STRING)
  (vector-ref days-of-week/short-strings day))

(define (day-of-week/long-string day)
  (guarantee-day-of-week day 'DAY-OF-WEEK/LONG-STRING)
  (vector-ref days-of-week/long-strings day))

(define (guarantee-day-of-week day name)
  (if (not (exact-integer? day))
      (error:wrong-type-argument day "day-of-week integer" name))
  (if (not (<= 0 day 6))
      (error:bad-range-argument day name)))

(define (string->day-of-week string)
  (or (string-ci->index days-of-week/short-strings string)
      (string-ci->index days-of-week/long-strings string)
      (error "Unknown day-of-week designation:" string)))

(define days-of-week/short-strings
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(define days-of-week/long-strings
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define (string-ci->index string-vector string)
  (let ((end (vector-length string-vector)))
    (let loop ((index 0))
      (cond ((fix:= index end) #f)
	    ((string-ci=? string (vector-ref string-vector index)) index)
	    (else (loop (fix:+ index 1)))))))

(define (string->year string)
  (let ((n (string->number string)))
    (if (not (exact-nonnegative-integer? n))
	(error:bad-range-argument string 'STRING->YEAR))
    (cond ((< n 70) (+ 2000 n))
	  ((< n 100) (+ 1900 n))
	  (else n))))

(define (d2 n)
  (string-pad-left (number->string n) 2 #\0))

;; Upwards compatibility
(define decode-universal-time universal-time->local-decoded-time)
(define encode-universal-time decoded-time->universal-time)
(define get-decoded-time local-decoded-time)
(define universal-time->string universal-time->local-time-string)
(define file-time->string file-time->local-time-string)