#| -*-Scheme-*-

$Id: ordvec.scm,v 1.6 2007/01/05 15:33:10 cph Exp $

Copyright 1995, 1999 Massachusetts Institute of Technology

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

;;;; Ordered Vectors

(declare (usual-integrations))

;;; ORDER implements a total order.  It accepts two keys, returning
;;; one of LESS, GREATER, or EQUAL to indicate the relative position
;;; of the keys in the order.  It's assumed that the vector does not
;;; contain two distinct keys that ORDER finds EQUAL.

;;; MATCH accepts two keys, returning either #F or a real number.  The
;;; returned value indicates how well the keys match, with #F meaning
;;; "no match", and larger numbers indicating better matches.  It is
;;; assumed that MATCH is true for an open set around each argument,
;;; within the order implemented by ORDER, and false everywhere
;;; outside that set.

(define (ordered-vector-minimum-match vector key item-key order match
				      if-unique if-not-unique if-not-found)
  (ordered-subvector-minimum-match vector 0 (vector-length vector) key item-key
				   order match
				   if-unique if-not-unique if-not-found))

(define (ordered-subvector-minimum-match vector start end key item-key
					 order match
					 if-unique if-not-unique if-not-found)
  (call-with-values
      (lambda ()
	(match-ordered-subvector vector start end key item-key order match))
    (lambda (lower upper gcm closest)
      (cond ((not gcm)
	     (if-not-found))
	    ((fix:= lower (fix:- upper 1))
	     (if-unique (vector-ref vector closest)))
	    (else
	     (if-not-unique (vector-ref vector closest)
			    gcm
			    (lambda () (subvector vector lower upper))))))))

(define (ordered-vector-matches vector key item-key order match)
  (ordered-subvector-matches vector 0 (vector-length vector) key item-key
			     order match))

(define (ordered-subvector-matches vector start end key item-key order match)
  (call-with-values
      (lambda ()
	(match-ordered-subvector vector start end key item-key order match))
    (lambda (lower upper gcm closest)
      gcm closest
      (subvector vector lower upper))))

(define (match-ordered-vector vector key item-key order match)
  (match-ordered-subvector vector 0 (vector-length vector) key item-key
			   order match))

(define (match-ordered-subvector vector start end key item-key order match)
  (let ((perform-search
	 (lambda (index)
	   (letrec
	       ((scan-up
		 (lambda (upper gcm)
		   (if (fix:= upper end)
		       (values upper gcm)
		       (let ((m (mc upper)))
			 (if m
			     (scan-up (fix:+ upper 1) (min gcm m))
			     (values upper gcm))))))
		(scan-down
		 (lambda (lower gcm)
		   (if (fix:= lower start)
		       (values lower gcm)
		       (let* ((index (fix:- lower 1))
			      (m (mc index)))
			 (if m
			     (scan-down index (min gcm m))
			     (values lower gcm))))))
		(mc
		 (let ((close (item-key (vector-ref vector index))))
		   (lambda (index)
		     (match close (item-key (vector-ref vector index)))))))
	     (call-with-values (lambda () (scan-up (fix:+ index 1) (mc index)))
	       (lambda (upper gcm)
		 (call-with-values (lambda () (scan-down index gcm))
		   (lambda (lower gcm)
		     (values lower upper gcm index)))))))))
    (search-ordered-subvector vector start end key item-key order
      perform-search
      (lambda (index)
	(if (and (fix:< index end)
		 (match key (item-key (vector-ref vector index))))
	    (perform-search index)
	    (values index index #f index))))))

(define (search-ordered-vector vector key item-key order if-found if-not-found)
  (search-ordered-subvector vector 0 (vector-length vector) key item-key order
			    if-found if-not-found))

(define (search-ordered-subvector vector start end key item-key order
				  if-found if-not-found)
  (let loop ((low start) (high end))
    (if (fix:< low high)
	(let ((index (fix:quotient (fix:+ low high) 2)))
	  (case (order key (item-key (vector-ref vector index)))
	    ((LESS) (loop low index))
	    ((GREATER) (loop (fix:+ index 1) high))
	    (else (if-found index))))
	(if-not-found low))))