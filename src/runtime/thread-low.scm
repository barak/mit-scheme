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

;;;; Some thread system structures needed during the early cold load.
;;; package: (runtime thread)

(declare (usual-integrations))

(define-integrable thread-mutex-tag
  '|#[(runtime thread)thread-mutex]|)

(define-integrable (thread-mutex? object)
  (and (vector? object)
       (fix:= 3 (vector-length object))
       (eq? (vector-ref object 0) thread-mutex-tag)))

(define-integrable (make-thread-mutex)
  (vector thread-mutex-tag (make-ring) #f))

(define-integrable (thread-mutex/waiting-threads t) (vector-ref t 1))

(define-integrable (thread-mutex/owner t) (vector-ref t 2))
(define-integrable (set-thread-mutex/owner! t o) (vector-set! t 2 o))

;;;; Circular Rings

#;(define-structure (link (conc-name link/))
  prev
  next
  item)

(define-integrable link-tag
  '|#[(runtime thread)link]|)

(define-integrable (link? object)
  (and (vector? object)
       (fix:= 4 (vector-length object))
       (eq? (vector-ref object 0) link-tag)))

(define-integrable (make-link prev next item)
  (vector link-tag prev next item))

(define-integrable (link/prev l) (vector-ref l 1))
(define-integrable (set-link/prev! l p) (vector-set! l 1 p))

(define-integrable (link/next l) (vector-ref l 2))
(define-integrable (set-link/next! l n) (vector-set! l 2 n))

(define-integrable (link/item l) (vector-ref l 3))
(define-integrable (set-link/item! l i) (vector-set! l 3 i))

(define (make-ring)
  (let ((link (make-link #f #f #f)))
    (set-link/prev! link link)
    (set-link/next! link link)
    link))

(define-integrable (ring/empty? ring)
  (eq? (link/next ring) ring))

(define (ring/enqueue ring item)
  (let ((prev (link/prev ring)))
    (let ((link (make-link prev ring item)))
      (set-link/next! prev link)
      (set-link/prev! ring link))))

(define (ring/dequeue ring default)
  (let ((link (link/next ring)))
    (if (eq? link ring)
	default
	(begin
	  (let ((next (link/next link)))
	    (set-link/next! ring next)
	    (set-link/prev! next ring))
	  (link/item link)))))

(define (ring/discard-all ring)
  (set-link/prev! ring ring)
  (set-link/next! ring ring))

(define (ring/remove-item ring item)
  (let loop ((link (link/next ring)))
    (if (not (eq? link ring))
	(if (eq? (link/item link) item)
	    (let ((prev (link/prev link))
		  (next (link/next link)))
	      (set-link/next! prev next)
	      (set-link/prev! next prev))
	    (loop (link/next link))))))

(define (ring/count-max-2 ring)
  (let ((link (link/next ring)))
    (cond ((eq? link ring) 0)
	  ((eq? (link/next link) ring) 1)
	  (else 2))))

(define (ring/first-item ring)
  (link/item (link/next ring)))

(define (ring/set-first-item! ring item)
  (set-link/item! (link/next ring) item))