#| -*-Scheme-*-

$Id: msort.scm,v 14.8 2001/11/26 19:11:18 cph Exp $

Copyright (c) 1988-2001 Massachusetts Institute of Technology

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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.
|#

;;;; Merge Sort
;;; package: ()

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