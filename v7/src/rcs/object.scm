#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/rcs/object.scm,v 1.1 1991/01/18 19:08:04 cph Exp $

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

;;;; RCS Data Structures

(declare (usual-integrations))

(define-integrable (rcstext/make head access symbols locks strict? comment
				 description)
  (vector rcstext/tag head access symbols locks strict? comment description))

(define rcstext/tag
  "rcstext")

(define-integrable (rcstext/head rcstext) (vector-ref rcstext 1))
(define-integrable (rcstext/access rcstext) (vector-ref rcstext 2))
(define-integrable (rcstext/symbols rcstext) (vector-ref rcstext 3))
(define-integrable (rcstext/locks rcstext) (vector-ref rcstext 4))
(define-integrable (rcstext/strict? rcstext) (vector-ref rcstext 5))
(define-integrable (rcstext/comment rcstext) (vector-ref rcstext 6))
(define-integrable (rcstext/description rcstext) (vector-ref rcstext 7))

(define-integrable (rcstext/set-head! rcstext head)
  (vector-set! rcstext 1 head))

(define-integrable (rcstext/set-access! rcstext access)
  (vector-set! rcstext 2 access))

(define-integrable (rcstext/set-symbols! rcstext symbols)
  (vector-set! rcstext 3 symbols))

(define-integrable (rcstext/set-locks! rcstext locks)
  (vector-set! rcstext 4 locks))

(define-integrable (rcstext/set-strict?! rcstext strict?)
  (vector-set! rcstext 5 strict?))

(define-integrable (rcstext/set-comment! rcstext comment)
  (vector-set! rcstext 6 comment))

(define-integrable (rcstext/set-description! rcstext description)
  (vector-set! rcstext 7 description))

(define-integrable (delta/make number)
  (vector delta/tag number false false false false false false false))

(define delta/tag
  "delta")

(define-integrable (delta/number delta) (vector-ref delta 1))
(define-integrable (delta/date delta) (vector-ref delta 2))
(define-integrable (delta/author delta) (vector-ref delta 3))
(define-integrable (delta/state delta) (vector-ref delta 4))
(define-integrable (delta/branches delta) (vector-ref delta 5))
(define-integrable (delta/next delta) (vector-ref delta 6))
(define-integrable (delta/log delta) (vector-ref delta 7))
(define-integrable (delta/text delta) (vector-ref delta 8))

(define-integrable (delta/set-number! delta number)
  (vector-set! delta 1 number))

(define-integrable (delta/set-date! delta date)
  (vector-set! delta 2 date))

(define-integrable (delta/set-author! delta author)
  (vector-set! delta 3 author))

(define-integrable (delta/set-state! delta state)
  (vector-set! delta 4 state))

(define-integrable (delta/set-branches! delta branches)
  (vector-set! delta 5 branches))

(define-integrable (delta/set-next! delta next)
  (vector-set! delta 6 next))

(define-integrable (delta/set-log! delta log)
  (vector-set! delta 7 log))

(define-integrable (delta/set-text! delta text)
  (vector-set! delta 8 text))

(unparser/set-tagged-vector-method!
 delta/tag
 (unparser/standard-method "DELTA"
   (lambda (state delta)
     (unparse-string state (delta/number delta)))))

(define (date/make year month day hour minute second)
  (vector
   year month day hour minute second
   (+ second
      (* 60
	 (+ minute
	    (* 60
	       (+ hour
		  (* 24
		     (+ (-1+ day)
			(vector-ref
			 (if (zero? (remainder year 4))
			     '#(0 31 60 91 121 152 182 213 244 274 305 335)
			     '#(0 31 59 90 120 151 181 212 243 273 304 334))
			 (-1+ month))
			(* 365 year)
			(quotient year 4))))))))))

(define-integrable (date/year date) (vector-ref date 0))
(define-integrable (date/month date) (vector-ref date 1))
(define-integrable (date/day date) (vector-ref date 2))
(define-integrable (date/hour date) (vector-ref date 3))
(define-integrable (date/minute date) (vector-ref date 4))
(define-integrable (date/second date) (vector-ref date 5))
(define-integrable (date/total-seconds date) (vector-ref date 6))

(define (date->string date)
  (string-append (date-component->string (date/year date))
		 "/"
		 (date-component->string (date/month date))
		 "/"
		 (date-component->string (date/day date))
		 " "
		 (date-component->string (date/hour date))
		 ":"
		 (date-component->string (date/minute date))
		 ":"
		 (date-component->string (date/second date))
		 " GMT"))

(define (date-component->string number)
  (cond ((zero? number) "00")
	((< number 10) (string-append "0" (write-to-string number)))
	(else (write-to-string number))))

(define-integrable (date<? x y)
  (< (date/total-seconds x) (date/total-seconds y)))