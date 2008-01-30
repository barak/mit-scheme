#| -*-Scheme-*-

$Id: mvalue.scm,v 3.6 2008/01/30 20:01:43 cph Exp $

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

;;;; Multiple Value Support

(declare (usual-integrations))

(define (transmit-values transmitter receiver)
  (transmitter receiver))

(define (multiple-value-list transmitter)
  (transmitter list))

(define (return . values)
  (lambda (receiver)
    (apply receiver values)))

;;; For efficiency:

(define (return-2 v0 v1)
  (lambda (receiver)
    (receiver v0 v1)))

(define (return-3 v0 v1 v2)
  (lambda (receiver)
    (receiver v0 v1 v2)))

(define (return-4 v0 v1 v2 v3)
  (lambda (receiver)
    (receiver v0 v1 v2 v3)))

(define (return-5 v0 v1 v2 v3 v4)
  (lambda (receiver)
    (receiver v0 v1 v2 v3 v4)))

(define (return-6 v0 v1 v2 v3 v4 v5)
  (lambda (receiver)
    (receiver v0 v1 v2 v3 v4 v5)))

(define (list-multiple first . rest)
  (apply call-multiple list first rest))

(define (cons-multiple cars cdrs)
  (call-multiple cons cars cdrs))

(define (call-multiple procedure . transmitters)
  (apply return
	 (apply map
		procedure
		(map multiple-value-list transmitters))))