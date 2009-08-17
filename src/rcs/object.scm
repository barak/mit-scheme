#| -*-Scheme-*-

$Id: 0a507060c4b56773ef2eb620ee349a51f3b418cc $

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

;;;; RCS Data Structures

(declare (usual-integrations))

(define-record-type <rcs-text>
    (make-rcstext head branch access symbols locks strict? comment expand
		  description)
    rcstext?
  (head rcstext/head)
  (branch rcstext/branch)
  (access rcstext/access)
  (symbols rcstext/symbols)
  (locks rcstext/locks)
  (strict? rcstext/strict?)
  (comment rcstext/comment)
  (expand rcstext/expand)
  (description rcstext/description))

(define-record-type <rcs-delta>
    (make-delta number date author state branches next log text)
    delta?
  (number delta/number)
  (date delta/date)
  (author delta/author)
  (state delta/state)
  (branches delta/branches)
  (next delta/next set-delta/next!)
  (log delta/log set-delta/log!)
  (text delta/text set-delta/text!))

(set-record-type-unparser-method! <rcs-delta>
  (standard-unparser-method 'RCS-DELTA
    (lambda (delta port)
      (write-char #\space port)
      (write-string (delta/number delta) port))))

(define (date/make year month day hour minute second)
  (let ((year (if (< year 100) (+ 1900 year) year)))
    (let ((dt (make-decoded-time second minute hour day month year 0)))
      (vector dt
	      (decoded-time->universal-time dt)
	      (decoded-time->universal-time
	       (make-decoded-time 0 0 0 day month year 0))))))

(define (date/decoded date) (vector-ref date 0))
(define (date/universal date) (vector-ref date 1))
(define (date/day date) (vector-ref date 2))

(define (date->string date) (decoded-time->string (date/decoded date)))

(define (date<? x y) (< (date/universal x) (date/universal y)))
(define (date=? x y) (= (date/universal x) (date/universal y)))
(define (date>? x y) (> (date/universal x) (date/universal y)))

(define (day<? x y) (< (date/day x) (date/day y)))
(define (day=? x y) (= (date/day x) (date/day y)))
(define (day>? x y) (> (date/day x) (date/day y)))