#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

;;;; RTL Generation: Special primitive combinations.  AMD x86-64 version.
;;; package: (compiler rtl-generator)

(declare (usual-integrations))

(define (define-special-primitive-handler name handler)
  (let ((primitive (make-primitive-procedure name true)))
    (let ((entry (assq primitive special-primitive-handlers)))
      (if entry
	  (set-cdr! entry handler)
	  (set! special-primitive-handlers
		(cons (cons primitive handler)
		      special-primitive-handlers)))))
  name)

(define (special-primitive-handler primitive)
  (let ((entry (assq primitive special-primitive-handlers)))
    (and entry
	 (cdr entry))))

(define special-primitive-handlers
  '())

(define (define-special-primitive/standard primitive)
  (define-special-primitive-handler primitive
    rtl:make-invocation:special-primitive))

(define-special-primitive/standard '&+)
(define-special-primitive/standard '&-)
(define-special-primitive/standard '&*)
(define-special-primitive/standard '&/)
(define-special-primitive/standard '&=)
(define-special-primitive/standard '&<)
(define-special-primitive/standard '&>)
(define-special-primitive/standard '1+)
(define-special-primitive/standard '-1+)
(define-special-primitive/standard 'zero?)
(define-special-primitive/standard 'positive?)
(define-special-primitive/standard 'negative?)
(define-special-primitive/standard 'quotient)
(define-special-primitive/standard 'remainder)