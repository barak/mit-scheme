#| -*-Scheme-*-

$Id: matcher.scm,v 1.1 2003/12/29 05:24:39 uid67408 Exp $

Copyright 2003 Massachusetts Institute of Technology

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

;;;; List matching

(declare (usual-integrations))

(define ((ml:matcher pattern) items)
  (ml:apply pattern items))

(define (ml:apply pattern items)
  (guarantee-list items 'ML:APPLY)
  (pattern (lambda (kf p r) kf p (r->v r))
	   (lambda () #f)
	   items))

(define ((ml:match predicate) ks kf p)
  (if (pair? p)
      (let ((item (car p)))
	(if (predicate item)
	    (ks kf (cdr p) (r1 item))
	    (kf)))
      (kf)))

(define ((ml:noise predicate) ks kf p)
  (if (and (pair? p) (predicate (car p)))
      (ks kf (cdr p) (r0))
      (kf)))

(define (ml:end ks kf p)
  (if (null? p)
      (ks kf p (r0))
      (kf)))

(define ((ml:* matcher) ks kf p)
  (let ks* ((kf kf) (p p) (r (r0)))
    (matcher (lambda (kf p r*) (ks* kf p (r+ r r*)))
	     (lambda () (ks kf p r))
	     p)))

(define (ml:seq . matchers)
  (let loop ((matchers matchers))
    (if (pair? matchers)
	(let ((m1 (car matchers))
	      (matchers (cdr matchers)))
	  (if (pair? matchers)
	      (let ((m2 (loop matchers)))
		(lambda (ks kf p)
		  (m1 (lambda (kf p r1)
			(m2 (lambda (kf p r2) (ks kf p (r+ r1 r2)))
			    kf
			    p))
		      kf
		      p)))
	      m1))
	(lambda (ks kf p) (ks kf p (r0))))))

(define (ml:alt . matchers)
  (if (pair? matchers)
      (let loop ((matchers matchers))
	(let ((m1 (car matchers))
	      (matchers (cdr matchers)))
	  (if (pair? matchers)
	      (let ((m2 (loop matchers)))
		(lambda (ks kf p)
		  (m1 ks
		      (lambda () (m2 ks kf p))
		      p)))
	      m1)))
      (lambda (ks kf p) ks p (kf))))

(define (ml:transform procedure matcher)
  (transformer (lambda (v) (v->r (procedure v))) matcher))

(define (ml:encapsulate procedure matcher)
  (transformer (lambda (v) (r1 (procedure v))) matcher))

(define (ml:map procedure matcher)
  (transformer (lambda (v) (v->r (vector-map procedure v))) matcher))

(define ((transformer transform matcher) ks kf p)
  (matcher (lambda (kf p r) (ks kf p (transform (r->v r))))
	   kf
	   p))

(define (ml:+ matcher)
  (ml:seq matcher (ml:* matcher)))

(define (ml:? matcher)
  (ml:alt matcher (ml:seq)))

(define ((ml:values . items) ks kf p)
  (ks kf p (l->r items)))

(define (ml:*-list matcher)
  (ml:encapsulate vector->list (ml:* matcher)))

(define-integrable (r0) '#())
(define-integrable (r1 item) (vector item))
(define-integrable (r->v r) r)
(define-integrable (v->r v) v)
(define-integrable (l->r l) (list->vector l))

(define (r+ r1 r2)
  (let ((n1 (vector-length r1))
	(n2 (vector-length r2)))
    (cond ((fix:= n1 0) r2)
	  ((fix:= n2 0) r1)
	  (else
	   (let ((r (make-vector (fix:+ n1 n2))))
	     (do ((i 0 (fix:+ i 1)))
		 ((fix:= i n1))
	       (vector-set! r i (vector-ref r1 i)))
	     (do ((i 0 (fix:+ i 1))
		  (j n1 (fix:+ j 1)))
		 ((fix:= i n2))
	       (vector-set! r j (vector-ref r2 i)))
	     r)))))

#|

;;; If the set of items doesn't include #F or pairs, this should be
;;; faster than the above.

(define-integrable (r0) #f)
(define-integrable (r1 item) item)

(define (r+ r1 r2)
  (cond ((not r1) r2)
	((not r2) r1)
	(else (cons r1 r2))))

(define (r->v r)
  (if r
      (let ((n
	     (let loop ((r r))
	       (if (pair? r)
		   (fix:+ (loop (car r))
			  (loop (cdr r)))
		   1))))
	(let ((v (make-vector n)))
	  (let loop ((r r) (i 0) (q '()))
	    (if (pair? r)
		(loop (car r)
		      i
		      (cons (cdr r) q))
		(begin
		  (vector-set! v i r)
		  (if (pair? q)
		      (loop (car q)
			    (fix:+ i 1)
			    (cdr q))))))
	  v))
      '#()))

(define (v->r v)
  ???)

(define (l->r l)
  ???)

|#