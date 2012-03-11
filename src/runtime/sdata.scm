#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Abstract Data Field
;;; package: (runtime scode-data)

(declare (usual-integrations))

(define (&typed-singleton-cons type element)
  ((ucode-primitive system-pair-cons) type (unmap-reference-trap element) '()))

(define (&singleton-element singleton)
  (map-reference-trap
   (lambda ()
     ((ucode-primitive system-pair-car) singleton))))

(define (&singleton-set-element! singleton new-element)
  ((ucode-primitive system-pair-set-car!)
   singleton
   (unmap-reference-trap new-element)))

(define (&typed-pair-cons type car cdr)
  ((ucode-primitive system-pair-cons)
   type
   (unmap-reference-trap car)
   (unmap-reference-trap cdr)))

(define (&pair-car pair)
  (map-reference-trap (lambda () ((ucode-primitive system-pair-car) pair))))

(define (&pair-set-car! pair new-car)
  ((ucode-primitive system-pair-set-car!) pair (unmap-reference-trap new-car)))

(define (&pair-cdr pair)
  (map-reference-trap (lambda () ((ucode-primitive system-pair-cdr) pair))))

(define (&pair-set-cdr! pair new-cdr)
  ((ucode-primitive system-pair-set-cdr!) pair (unmap-reference-trap new-cdr)))

(define (&typed-triple-cons type first second third)
  (object-new-type type
		   ((ucode-primitive hunk3-cons)
		    (unmap-reference-trap first)
		    (unmap-reference-trap second)
		    (unmap-reference-trap third))))

(define (&triple-first triple)
  (map-reference-trap (lambda () ((ucode-primitive system-hunk3-cxr0) triple))))

(define (&triple-set-first! triple new-first)
  ((ucode-primitive system-hunk3-set-cxr0!)
   triple
   (unmap-reference-trap new-first)))

(define (&triple-second triple)
  (map-reference-trap (lambda () ((ucode-primitive system-hunk3-cxr1) triple))))

(define (&triple-set-second! triple new-second)
  ((ucode-primitive system-hunk3-set-cxr1!)
   triple
   (unmap-reference-trap new-second)))

(define (&triple-third triple)
  (map-reference-trap (lambda () ((ucode-primitive system-hunk3-cxr2) triple))))

(define (&triple-set-third! triple new-third)
  ((ucode-primitive system-hunk3-set-cxr2!)
   triple
   (unmap-reference-trap new-third)))

(define (&typed-vector-cons type elements)
  ((ucode-primitive system-list-to-vector)
   type
   (let loop ((elements elements))
     (if (null? elements)
	 '()
	 (cons (unmap-reference-trap (car elements))
	       (loop (cdr elements)))))))

(define (&vector-length vector)
  ((ucode-primitive system-vector-size) vector))

(define (&vector-ref vector index)
  (map-reference-trap
   (lambda ()
     ((ucode-primitive system-vector-ref) vector index))))

(define (&subvector->list vector start stop)
  (let loop
      ((sublist
	((ucode-primitive system-subvector-to-list) vector start stop)))
    (if (null? sublist)
	'()
	(cons (map-reference-trap (lambda () (car sublist)))
	      (loop (cdr sublist))))))