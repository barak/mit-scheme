;;; -*-Scheme-*-
;;;
;;;	Copyright (c) 1985 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3.  All materials developed as a consequence of the use of
;;;	this software shall duly acknowledge such use, in accordance
;;;	with the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5.  In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Date and Time Routines

(declare (usual-integrations))

;;;; Date and Time

(define date
  (let ((year (make-primitive-procedure 'CURRENT-YEAR))
	(month (make-primitive-procedure 'CURRENT-MONTH))
	(day (make-primitive-procedure 'CURRENT-DAY)))
    (named-lambda (date #!optional receiver)
      ((if (unassigned? receiver) list receiver)
       (year) (month) (day)))))

(define time
  (let ((hour (make-primitive-procedure 'CURRENT-HOUR))
	(minute (make-primitive-procedure 'CURRENT-MINUTE))
	(second (make-primitive-procedure 'CURRENT-SECOND)))
    (named-lambda (time #!optional receiver)
      ((if (unassigned? receiver) list receiver)
       (hour) (minute) (second)))))

(define date->string)
(define time->string)
(let ()

(set! date->string
(named-lambda (date->string year month day)
  (if year
      (string-append
       (vector-ref days-of-the-week
		   (let ((qr (integer-divide year 4)))
		     (remainder (+ (* year 365)
				   (if (and (zero? (integer-divide-remainder qr))
					    (<= month 2))
				       (integer-divide-quotient qr)
				       (1+ (integer-divide-quotient qr)))
				   (vector-ref days-through-month (-1+ month))
				   day
				   6)
				7)))
       " "
       (vector-ref months-of-the-year (-1+ month))
       " "
       (write-to-string day)
       ", 19"
       (write-to-string year))
      "Date primitives not installed")))

(define months-of-the-year
  #("January" "February" "March" "April" "May" "June" "July"
    "August" "September" "October" "November" "December"))

(define days-of-the-week
  #("Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"))

(define days-through-month
  (let ()
    (define (month-loop months value)
      (if (null? months)
	  '()
	  (cons value
		(month-loop (cdr months) (+ value (car months))))))
    (list->vector (month-loop '(31 28 31 30 31 30 31 31 30 31 30 31) 0))))

(set! time->string
(named-lambda (time->string hour minute second)
  (if hour
      (string-append (write-to-string
		      (cond ((zero? hour) 12)
			    ((< hour 13) hour)
			    (else (- hour 12))))
		     (if (< minute 10) ":0" ":")
		     (write-to-string minute)
		     (if (< second 10) ":0" ":")
		     (write-to-string second)
		     " "
		     (if (< hour 12) "AM" "PM"))
      "Time primitives not installed")))

