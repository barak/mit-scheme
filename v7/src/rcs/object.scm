#| -*-Scheme-*-

$Id: object.scm,v 1.3 1999/01/02 06:11:34 cph Exp $

Copyright (c) 1988, 1991, 1999 Massachusetts Institute of Technology

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

;;;; RCS Data Structures

(declare (usual-integrations))

(define rcstext-rtd
  (make-record-type
   "rcstext"
   '(head branch access symbols locks strict? comment expand description)))

(define make-rcstext (record-constructor rcstext-rtd))
(define rcstext/head (record-accessor rcstext-rtd 'head))
(define rcstext/branch (record-accessor rcstext-rtd 'branch))
(define rcstext/access (record-accessor rcstext-rtd 'access))
(define rcstext/symbols (record-accessor rcstext-rtd 'symbols))
(define rcstext/locks (record-accessor rcstext-rtd 'locks))
(define rcstext/strict? (record-accessor rcstext-rtd 'strict?))
(define rcstext/comment (record-accessor rcstext-rtd 'comment))
(define rcstext/expand (record-accessor rcstext-rtd 'expand))
(define rcstext/description (record-accessor rcstext-rtd 'description))

(define delta-rtd
  (make-record-type "rcsdelta"
		    '(number date author state branches next log text)))

(define make-delta (record-constructor delta-rtd))
(define delta/number (record-accessor delta-rtd 'number))
(define delta/date (record-accessor delta-rtd 'date))
(define delta/author (record-accessor delta-rtd 'author))
(define delta/state (record-accessor delta-rtd 'state))
(define delta/branches (record-accessor delta-rtd 'branches))
(define delta/next (record-accessor delta-rtd 'next))
(define set-delta/next! (record-updater delta-rtd 'next))
(define delta/log (record-accessor delta-rtd 'log))
(define set-delta/log! (record-updater delta-rtd 'log))
(define delta/text (record-accessor delta-rtd 'text))
(define set-delta/text! (record-updater delta-rtd 'text))

(set-record-type-unparser-method! delta-rtd
  (unparser/standard-method 'rcsdelta
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