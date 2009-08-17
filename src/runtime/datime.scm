#| -*-Scheme-*-

$Id: 134fe2d2c237bd36490bd2c703d3407c0d327d56 $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Date and Time Routines
;;; package: (runtime date/time)

(declare (usual-integrations))

;;; Used extensively below.
(define (number-parser min-digits max-digits low high)
  (lambda (b)
    (let ((p (get-parser-buffer-pointer b)))
      (let ((done
	     (lambda ()
	       (let ((n (string->number (get-parser-buffer-tail b p))))
		 (and (<= low n high)
		      (vector n))))))
	(let loop ((n-digits 0))
	  (if (= n-digits max-digits)
	      (done)
	      (if (match-parser-buffer-char-in-set b char-set:numeric)
		  (loop (+ n-digits 1))
		  (if (>= n-digits min-digits)
		      (done)
		      (begin
			(set-parser-buffer-pointer! b p)
			#f)))))))))

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

(define-guarantee decoded-time "decoded time")

(define (make-decoded-time second minute hour day month year #!optional zone)
  (check-decoded-time-args second minute hour day month year
			   'MAKE-DECODED-TIME)
  (let ((zone (if (default-object? zone) #f zone)))
    (if zone
	(guarantee-time-zone zone 'MAKE-DECODED-TIME))
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

(define (check-decoded-time-args second minute hour day month year caller)
  (let ((check-range
	 (lambda (object min max)
	   (guarantee-exact-nonnegative-integer object caller)
	   (if (not (<= min object max))
	       (error:bad-range-argument object caller)))))
    (guarantee-exact-nonnegative-integer year caller)
    (check-range month 1 12)
    (check-range day 1 (month/max-days month))
    (check-range hour 0 23)
    (check-range minute 0 59)
    (check-range second 0 59)))

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

(define-guarantee time-zone "time zone")

(define (decoded-time/daylight-savings-time? dt)
  (> (decoded-time/daylight-savings-time dt) 0))

(define (decoded-time->utc dt)
  (if (let ((zone (decoded-time/zone dt)))
	(or (not zone)
	    (= zone 0)))
      dt
      (universal-time->global-decoded-time
       (decoded-time->universal-time dt))))

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

(define (universal-time->local-rfc2822-string time)
  (decoded-time->string (universal-time->local-decoded-time time)))

(define (universal-time->global-rfc2822-string time)
  (decoded-time->string (universal-time->global-decoded-time time)))

(define (file-time->local-rfc2822-string time)
  (decoded-time->string (file-time->local-decoded-time time)))

(define (file-time->global-rfc2822-string time)
  (decoded-time->string (file-time->global-decoded-time time)))

(define (decoded-time->rfc2822-string dt)
  ;; The returned string is in the format specified by RFC 2822,
  ;; provided that time-zone information is available from the C
  ;; library.
  (call-with-output-string
    (lambda (port)
      (write-decoded-time-as-rfc2822 dt port))))

(define (write-decoded-time-as-rfc2822 dt port)
  (%write-decoded-time-1 dt port)
  (let ((zone (decoded-time/zone dt)))
    (if zone
	(begin
	  (write-char #\space port)
	  (write-time-zone (if (decoded-time/daylight-savings-time? dt)
			       (- zone 1)
			       zone)
			   port)))))

(define (write-decoded-time-as-http dt port)
  (%write-decoded-time-1 (decoded-time->utc dt) port)
  (write-string " GMT" port))

(define (%write-decoded-time-1 dt port)
  (let ((day-of-week (decoded-time/day-of-week dt)))
    (if day-of-week
	(begin
	  (write-string (day-of-week/short-string day-of-week) port)
	  (write-string ", " port))))
  (write (decoded-time/day dt) port)
  (write-char #\space port)
  (write-string (month/short-string (decoded-time/month dt)) port)
  (write-char #\space port)
  (write (decoded-time/year dt) port)
  (write-char #\space port)
  (write-d2 (decoded-time/hour dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/minute dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/second dt) port))

(define (rfc2822-string->decoded-time string)
  (let ((v (*parse-string parser:rfc2822-time string)))
    (if (not v)
	(error:bad-range-argument string 'STRING->DECODED-TIME))
    (vector-ref v 0)))

(define (string->universal-time string)
  (decoded-time->universal-time (string->decoded-time string)))

(define (string->file-time string)
  (decoded-time->file-time (string->decoded-time string)))

(define parser:rfc2822-time
  (*parser
   (encapsulate (lambda (v)
		  (make-decoded-time (vector-ref v 6)
				     (vector-ref v 5)
				     (vector-ref v 4)
				     (vector-ref v 1)
				     (vector-ref v 2)
				     (vector-ref v 3)
				     (vector-ref v 7)))
     (seq (noise match-lws*)
	  (alt (seq parse-short-day-of-week
		    ","
		    (noise match-lws*))
	       (values #f))
	  parse-rfc2822-day
	  (noise match-lws)
	  parse-short-month
	  (noise match-lws)
	  (alt parse-rfc2822-year
	       parse-rfc2822-obs-year)
	  (noise match-lws)
	  parse-rfc2822-hour
	  (noise match-lws*)
	  ":"
	  (noise match-lws*)
	  parse-rfc2822-minute
	  (alt (seq (noise match-lws*)
		    ":"
		    parse-rfc2822-second)
	       (values 0))
	  (noise match-lws)
	  (alt parser:numeric-time-zone
	       parser:named-time-zone
	       ;; One-letter military zones are treated as zero; see RFC
	       ;; for rationale.
	       (map (lambda (n) n 0)
		    parser:military-time-zone))
	  (noise match-lws*)))))

(define parse-rfc2822-obs-year
  (*parser
   (map (lambda (s)
	  (let ((n (string->number s)))
	    (+ (if (< n 50) 2000 1900)
	       n)))
	(match (seq (char-set char-set:numeric)
		    (char-set char-set:numeric))))))

(define parse-rfc2822-day (number-parser 1 2 1 31))
(define parse-rfc2822-year (number-parser 4 4 1900 9999))
(define parse-rfc2822-hour (number-parser 2 2 0 23))
(define parse-rfc2822-minute (number-parser 2 2 0 59))
(define parse-rfc2822-second (number-parser 2 2 0 59))

(define match-lws
  (*matcher (+ (char-set char-set:wsp))))

(define match-lws*
  (*matcher (* (char-set char-set:wsp))))

(define (time-zone->string tz)
  (call-with-output-string
    (lambda (port)
      (write-time-zone tz port))))

(define (write-time-zone tz port)
  (guarantee-time-zone tz 'WRITE-TIME-ZONE)
  (let ((minutes (round (* 60 (- tz)))))
    (let ((qr (integer-divide (abs minutes) 60)))
      (write-char (if (< minutes 0) #\- #\+) port)
      (write-d2 (integer-divide-quotient qr) port)
      (write-d2 (integer-divide-remainder qr) port))))

(define (string->time-zone string)
  (let ((v (*parse-string parser:time-zone string)))
    (if (not v)
	(error:bad-range-argument string 'STRING->TIME-ZONE))
    (vector-ref v 0)))

(define parser:time-zone
  (*parser
   (alt parser:numeric-time-zone
	parser:named-time-zone
	parser:military-time-zone)))

(define parser:numeric-time-zone
  (*parser
   (encapsulate (lambda (v)
		  (let ((n
			 (+ (vector-ref v 1)
			    (/ (vector-ref v 2) 60))))
		    (if (string=? (vector-ref v 0) "+")
			(- n)
			n)))
     (seq (match (alt "+" "-"))
	  parse-time-zone-hour
	  parse-time-zone-minute))))

(define parse-time-zone-hour (number-parser 2 2 0 24))
(define parse-time-zone-minute (number-parser 2 2 0 59))

(define parser:named-time-zone
  (*parser
   (transform (lambda (v)
		(let ((entry
		       (let ((s (vector-ref v 0)))
			 (find (lambda (zone)
				 (string-ci=? (car zone) s))
			       named-time-zones))))
		  (and entry
		       (vector (cadr entry)))))
     (match (alt "UT"
		 (seq (char-set char-set:alphabetic)
		      (char-set char-set:alphabetic)
		      (char-set char-set:alphabetic)))))))

(define named-time-zones
  '(("UT" 0) ("GMT" 0)
    ("EST" 5) ("EDT" 4) ("CST" 6) ("CDT" 5)
    ("MST" 7) ("MDT" 6) ("PST" 8) ("PDT" 7)))

(define parser:military-time-zone
  (*parser
   (transform (lambda (v)
		(let ((c (char-upcase (string-ref (vector-ref v 0) 0))))
		  (cond ((char=? c #\Z)
			 (vector 0))
			((and (char>=? c #\A)
			      (char<=? c #\I))
			 (vector (- (+ (- (char->integer c)
					  (char->integer #\A))
				       1))))
			((and (char>=? c #\K)
			      (char<=? c #\M))
			 (vector (- (+ (- (char->integer c)
					  (char->integer #\K))
				       10))))
			((and (char>=? c #\N)
			      (char<=? c #\Y))
			 (vector (+ (- (char->integer c)
				       (char->integer #\N))
				    1)))
			(else #f))))
     (match (char-set char-set:alphabetic)))))

;;;; ISO C ctime() strings

(define (decoded-time->ctime-string dt)
  (call-with-output-string
    (lambda (port)
      (write-decoded-time-as-ctime dt port))))

(define (write-decoded-time-as-ctime dt port)
  (write-string (day-of-week/short-string (decoded-time/day-of-week dt)) port)
  (write-char #\space port)
  (write-string (month/short-string (decoded-time/month dt)) port)
  (write-char #\space port)
  (let ((day (decoded-time/day dt)))
    (if (< day 10)
	(write-char #\space port))
    (write day port))
  (write-char #\space port)
  (write-d2 (decoded-time/hour dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/minute dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/second dt) port)
  (write-char #\space port)
  (write (decoded-time/year dt) port))

(define (ctime-string->decoded-time string #!optional zone)
  (let ((v
	 (*parse-string (parser:ctime (if (default-object? zone) #f zone))
			string)))
    (if (not v)
	(error:bad-range-argument string 'CTIME-STRING->DECODED-TIME))
    (vector-ref v 0)))

(define (universal-time->local-ctime-string time)
  (decoded-time->ctime-string (universal-time->local-decoded-time time)))

(define (universal-time->global-ctime-string time)
  (decoded-time->ctime-string (universal-time->global-decoded-time time)))

(define (ctime-string->universal-time string #!optional zone)
  (decoded-time->universal-time (ctime-string->decoded-time string zone)))

(define (file-time->local-ctime-string time)
  (decoded-time->ctime-string (file-time->local-decoded-time time)))

(define (file-time->global-ctime-string time)
  (decoded-time->ctime-string (file-time->global-decoded-time time)))

(define (ctime-string->file-time string #!optional zone)
  (decoded-time->file-time (ctime-string->decoded-time string zone)))

(define (parser:ctime zone)
  (if zone
      (guarantee-time-zone zone 'PARSER:CTIME))
  (*parser
   (encapsulate (lambda (v)
		  (make-decoded-time (vector-ref v 5)
				     (vector-ref v 4)
				     (vector-ref v 3)
				     (vector-ref v 2)
				     (vector-ref v 1)
				     (vector-ref v 6)
				     zone))
     (seq parse-short-day-of-week
	  " "
	  parse-short-month
	  " "
	  (alt (seq " " parse-ctime-day1)
	       parse-ctime-day2)
	  " "
	  parse-ctime-hour
	  ":"
	  parse-ctime-minute
	  ":"
	  parse-ctime-second
	  " "
	  parse-ctime-year))))

(define parse-ctime-hour (number-parser 2 2 0 23))
(define parse-ctime-minute (number-parser 2 2 0 59))
(define parse-ctime-second (number-parser 2 2 0 59))
(define parse-ctime-day1 (number-parser 1 1 1 9))
(define parse-ctime-day2 (number-parser 2 2 10 31))
(define parse-ctime-year (number-parser 4 4 1900 9999))

;;;; ISO 8601 date/time strings

;;; This implements a subset of the ISO 8601 specification.  It
;;; accepts only complete date+time representations.  It does not
;;; support either truncation or expansion.  On output, it uses a
;;; single format.  The XML-RPC `specification' uses a bastard hybrid
;;; of the basic and extended formats -- maybe.

(define (iso8601-string->decoded-time string #!optional start end)
  (let ((v (*parse-string parser:iso8601-date/time string start end)))
    (if (not v)
	(error:bad-range-argument string 'ISO8601-STRING->DECODED-TIME))
    (vector-ref v 0)))

(define (xml-rpc-iso8601-string->decoded-time string #!optional start end)
  (let ((v (*parse-string parser:xml-rpc-iso8601-date/time string start end)))
    (if (not v)
	(error:bad-range-argument string
				  'XML-RPC-ISO8601-STRING->DECODED-TIME))
    (vector-ref v 0)))

(define (decoded-time->iso8601-string dt)
  (call-with-output-string
    (lambda (port)
      (write-decoded-time-as-iso8601-extended dt port))))

(define (decoded-time->xml-rpc-iso8601-string dt)
  (call-with-output-string
    (lambda (port)
      (write-decoded-time-as-xml-rpc-iso8601 dt port))))

(define (write-decoded-time-as-iso8601-basic dt port)
  (write-decoded-time-iso8601-basic-date dt port)
  (write-char #\T port)
  (write-decoded-time-iso8601-basic-time dt port)
  (write-decoded-time-iso8601-zone dt port))

(define (write-decoded-time-as-iso8601-extended dt port)
  (write-decoded-time-iso8601-extended-date dt port)
  (write-char #\T port)
  (write-decoded-time-iso8601-extended-time dt port)
  (write-decoded-time-iso8601-zone dt port))

(define (write-decoded-time-as-xml-rpc-iso8601 dt port)
  (write-decoded-time-iso8601-basic-date dt port)
  (write-char #\T port)
  (write-decoded-time-iso8601-extended-time dt port))

(define (write-decoded-time-iso8601-basic-date dt port)
  (write (decoded-time/year dt) port)
  (write-d2 (decoded-time/month dt) port)
  (write-d2 (decoded-time/day dt) port))

(define (write-decoded-time-iso8601-extended-date dt port)
  (write (decoded-time/year dt) port)
  (write-char #\- port)
  (write-d2 (decoded-time/month dt) port)
  (write-char #\- port)
  (write-d2 (decoded-time/day dt) port))

(define (write-decoded-time-iso8601-basic-time dt port)
  (write-d2 (decoded-time/hour dt) port)
  (write-d2 (decoded-time/minute dt) port)
  (write-d2 (decoded-time/second dt) port))

(define (write-decoded-time-iso8601-extended-time dt port)
  (write-d2 (decoded-time/hour dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/minute dt) port)
  (write-char #\: port)
  (write-d2 (decoded-time/second dt) port))

(define (write-decoded-time-iso8601-zone dt port)
  (let ((zone (decoded-time/zone dt)))
    (if zone
	(let ((minutes
	       (round (* 60
			 (- (if (decoded-time/daylight-savings-time? dt)
				(- zone 1)
				zone))))))
	  (if (= minutes 0)
	      (write-char #\Z port)
	      (let ((qr (integer-divide (abs minutes) 60)))
		(write-char (if (< minutes 0) #\- #\+) port)
		(write-d2 (integer-divide-quotient qr) port)
		(if (not (= (integer-divide-remainder qr) 0))
		    (begin
		      (write-char #\: port)
		      (write-d2 (integer-divide-remainder qr) port)))))))))

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

(define parser:iso8601-date/time
  ;; Use of the space separator isn't allowed, but we used to generate
  ;; strings with it, so don't barf if we see it.
  (*parser
   (encapsulate convert-8601-date/time
     (alt (seq parse-basic-8601-date
	       (alt "T" " ")
	       parse-basic-8601-time
	       parse-basic-8601-zone)
	  (seq parse-extended-8601-date
	       (alt "T" " ")
	       parse-extended-8601-time
	       parse-extended-8601-zone)))))

;;; A literal interpretation of the XML-RPC `specification' at
;;; <http://www.xmlrpc.com/spec> might suggest that the only valid
;;; date/time string is `19980717T14:08:55', which is the solitary
;;; example given for the `dateTime.iso8601' element.  Who knows what
;;; bizarre beasts pretending to be ISO 8601 strings might turn up in
;;; the wild?

(define parser:xml-rpc-iso8601-date/time
  (*parser
   (encapsulate convert-8601-date/time
     (seq (alt parse-extended-8601-date parse-basic-8601-date)
	  "T"
	  (alt parse-extended-8601-time parse-basic-8601-time)
	  (alt parse-extended-8601-zone parse-basic-8601-zone)))))

(define (convert-8601-date/time v)
  (let ((year (vector-ref v 0))
	(month (vector-ref v 1))
	(day (vector-ref v 2))
	(hour (vector-ref v 3))
	(minute (vector-ref v 4))
	(second (vector-ref v 5))
	(fraction (vector-ref v 6))
	(zone (vector-ref v 7)))
    (let ((adjust
	   (lambda (hour minute second offset)
	     (let ((dt
		    (universal-time->global-decoded-time
		     (+ (decoded-time->universal-time
			 (make-decoded-time second minute hour day month year
					    0))
			offset))))
	       (if (eqv? zone 0)
		   dt
		   (make-decoded-time (decoded-time/second dt)
				      (decoded-time/minute dt)
				      (decoded-time/hour dt)
				      (decoded-time/day dt)
				      (decoded-time/month dt)
				      (decoded-time/year dt)
				      zone))))))
      (if (< fraction 1/2)
	  (if (< hour 24)
	      (make-decoded-time second minute hour day month year zone)
	      (adjust 0 0 0 86400))
	  (adjust hour minute second 1)))))

(define parse-basic-8601-date
  (*parser
   (alt (seq parse-8601-year parse-8601-month parse-8601-day)
	(transform week-date->month-date
	  (seq parse-8601-year "W" parse-8601-week parse-8601-week-day))
	(transform ordinal-date->month-date
	  (seq parse-8601-year parse-8601-ordinal-day)))))

(define parse-extended-8601-date
  (*parser
   (alt (seq parse-8601-year "-" parse-8601-month "-" parse-8601-day)
	(transform week-date->month-date
	  (seq parse-8601-year "-W" parse-8601-week "-" parse-8601-week-day))
	(transform ordinal-date->month-date
	  (seq parse-8601-year "-" parse-8601-ordinal-day)))))

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
	   (vector (decoded-time/year dt)
		   (decoded-time/month dt)
		   (decoded-time/day dt))))))

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
	   (vector (decoded-time/year dt)
		   (decoded-time/month dt)
		   (decoded-time/day dt))))))

(define parse-basic-8601-zone
  (*parser
   (alt (encapsulate (lambda (v) v 0)
	  (noise "Z"))
	(transform transform-8601-zone
	  (seq parse-8601-sign
	       parse-8601-zone-hour
	       (alt parse-8601-minute
		    (values 0))))
	(values #f))))

(define parse-extended-8601-zone
  (*parser
   (alt (encapsulate (lambda (v) v 0)
	  (noise "Z"))
	(transform transform-8601-zone
	  (seq parse-8601-sign
	       parse-8601-zone-hour
	       ;; The colon isn't optional here, but we used to
	       ;; generate strings without it, so don't barf if it's
	       ;; missing.
	       (alt (seq (? ":") parse-8601-minute)
		    (values 0))))
	(values #f))))

(define (transform-8601-zone v)
  (let ((hour
	 (+ (vector-ref v 1)
	    (/ (vector-ref v 2) 60))))
    (and (<= hour 24)
	 (vector (* (- (vector-ref v 0))
		    hour)))))

(define parse-basic-8601-time
  (*parser
   (transform qualify-8601-time
     (seq parse-8601-hour
	  (alt (seq parse-8601-minute
		    (alt (seq parse-8601-second
			      (alt parse-8601-fraction
				   (values 0)))
			 (transform transform-8601-minute-fraction
			   parse-8601-fraction)
			 (values 0 0)))
	       (transform transform-8601-hour-fraction
		 parse-8601-fraction)
	       (values 0 0 0))))))

(define parse-extended-8601-time
  (*parser
   (transform qualify-8601-time
     (seq parse-8601-hour
	  (alt (seq ":" parse-8601-minute
		    (alt (seq ":" parse-8601-second
			      (alt parse-8601-fraction
				   (values 0)))
			 (transform transform-8601-minute-fraction
			   parse-8601-fraction)
			 (values 0 0)))
	       (transform transform-8601-hour-fraction
		 parse-8601-fraction)
	       (values 0 0 0))))))

(define parse-8601-fraction
  (*parser
   (map (lambda (s)
	  (/ (string->number s)
	     (expt 10 (string-length s))))
	(seq (alt "," ".")
	     (match (* (char-set char-set:numeric)))))))

(define (transform-8601-hour-fraction v)
  (let ((mx (* (vector-ref v 0) 60)))
    (let ((m (truncate mx)))
      (let ((sx (* (- mx m) 60)))
	(let ((s (truncate sx)))
	  (vector m s (- sx s)))))))

(define (transform-8601-minute-fraction v)
  (let ((sx (* (vector-ref v 0) 60)))
    (let ((s (truncate sx)))
      (vector s (- sx s)))))

(define (qualify-8601-time v)
  (let ((h (vector-ref v 0)))
    (and (or (< h 24)
	     (and (= (vector-ref v 1) 0)
		  (= (vector-ref v 2) 0)
		  (= (vector-ref v 3) 0)))
	 v)))

(define parse-8601-year (number-parser 4 4 1582 9999))
(define parse-8601-month (number-parser 2 2 1 12))
(define parse-8601-week (number-parser 2 2 1 53))
(define parse-8601-day (number-parser 2 2 1 31))
(define parse-8601-week-day (number-parser 1 1 1 7))
(define parse-8601-ordinal-day (number-parser 3 3 1 366))
(define parse-8601-hour (number-parser 2 2 0 24))
(define parse-8601-zone-hour (number-parser 2 2 0 24))
(define parse-8601-minute (number-parser 2 2 0 59))
(define parse-8601-second (number-parser 2 2 0 59))

(define parse-8601-sign
  (*parser
   (alt (map (lambda (v) v 1) (match "+"))
	(map (lambda (v) v -1) (match "-")))))

;;;; RFC 850 times (for HTTP only)

(define parser:rfc850-time
  (*parser
   (encapsulate (lambda (v)
		  (make-decoded-time (vector-ref v 6)
				     (vector-ref v 5)
				     (vector-ref v 4)
				     (vector-ref v 1)
				     (vector-ref v 2)
				     (vector-ref v 3)
				     0))
     (seq parse-long-day-of-week
	  ", "
	  parse-rfc850-day
	  " "
	  parse-short-month
	  " "
	  parse-rfc2822-obs-year
	  " "
	  parse-rfc850-hour
	  ":"
	  parse-rfc850-minute
	  ":"
	  parse-rfc850-second
	  " GMT"))))

(define parse-rfc850-day (number-parser 2 2 1 31))
(define parse-rfc850-hour (number-parser 2 2 0 23))
(define parse-rfc850-minute (number-parser 2 2 0 59))
(define parse-rfc850-second (number-parser 2 2 0 59))

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

(define (write-d2 n port)
  (if (< n 10)
      (write-char #\0 port))
  (write n port))

(define month/short-strings
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(define month/long-strings
  '#("January" "February" "March" "April" "May" "June" "July" "August"
	       "September" "October" "November" "December"))

(define days-of-week/short-strings
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(define days-of-week/long-strings
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"))

(define (string-vector-parser v offset)
  (let ((n (vector-length v)))
    (lambda (b)
      (let loop ((i 0))
	(and (< i n)
	     (if (match-parser-buffer-string b (vector-ref v i))
		 (vector (+ i offset))
		 (loop (+ i 1))))))))

(define parse-short-month
  (string-vector-parser month/short-strings 1))

(define parse-long-month
  (string-vector-parser month/long-strings 1))

(define parse-short-day-of-week
  (string-vector-parser days-of-week/short-strings 0))

(define parse-long-day-of-week
  (string-vector-parser days-of-week/long-strings 0))