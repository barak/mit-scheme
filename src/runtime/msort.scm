#| -*-Scheme-*-

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

;;;; Merge Sort
;;; package: (runtime merge-sort)

(declare (usual-integrations))

;; This merge sort is stable.

(define (merge-sort obj pred)
  (if (vector? obj)
      (merge-sort! (vector-copy obj) pred)
      (vector->list (merge-sort! (list->vector obj) pred))))

(define (merge-sort! v pred)
  (if (not (vector? v))
      (error:wrong-type-argument v "vector" 'MERGE-SORT!))
  (let sort-subvector
      ((v v)
       (temp (vector-copy v))
       (low 0)
       (high (vector-length v)))
    (if (fix:> (fix:- high low) 1)
	(let ((middle (fix:quotient (fix:+ low high) 2)))
	  ;; Sort each half of (V,LOW,HIGH) into the corresponding
	  ;; half of TEMP.
	  (sort-subvector temp v low middle)
	  (sort-subvector temp v middle high)
	  ;; Merge the two halves of TEMP back into V.
	  (let merge ((p low) (p1 low) (p2 middle))
	    (if (fix:< p high)
		(if (and (fix:< p1 middle)
			 (or (fix:= p2 high)
			     (not (pred (vector-ref temp p2)
					(vector-ref temp p1)))))
		    (begin
		      (vector-set! v p (vector-ref temp p1))
		      (merge (fix:+ p 1) (fix:+ p1 1) p2))
		    (begin
		      (vector-set! v p (vector-ref temp p2))
		      (merge (fix:+ p 1) p1 (fix:+ p2 1)))))))))
  v)

(define sort merge-sort)
(define sort! merge-sort!)