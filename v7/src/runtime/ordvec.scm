;;; -*-Scheme-*-
;;;
;;;	$Id: ordvec.scm,v 1.1 1995/05/03 07:35:56 cph Exp $
;;;
;;;	Copyright (c) 1995 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;
;;; NOTE: Parts of this program (Edwin) were created by translation
;;; from corresponding parts of GNU Emacs.  Users should be aware that
;;; the GNU GENERAL PUBLIC LICENSE may apply to these parts.  A copy
;;; of that license should have been included along with this file.
;;;

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