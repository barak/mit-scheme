#| -*-Scheme-*-

$Id: object.scm,v 1.7 2003/02/14 18:28:32 cph Exp $

Copyright (c) 1988, 1991, 1999, 2000 Massachusetts Institute of Technology

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