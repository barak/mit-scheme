;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/edwin/ring.scm,v 1.9 1989/04/28 22:52:56 cph Rel $
;;;
;;;	Copyright (c) 1984, 1989 Massachusetts Institute of Technology
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

;;;; Rings

(declare (usual-integrations))

(define (ring-list ring)
  (vector-ref ring 2))

(define make-ring)
(define ring-size)
(define ring-clear!)
(define ring-empty?)
(define ring-push!)
(define ring-pop!)
(define ring-ref)
(define ring-set!)
(let ()

(define (list-ref l i)
  (cond ((null? l) (error "Index too large" 'LIST-REF))
	((zero? i) (car l))
	(else (list-ref (cdr l) (-1+ i)))))

(define (list-set! l i o)
  (let loop ((l l) (i i))
    (cond ((null? l) (error "index too large" i))
	  ((zero? i) (set-car! l o))
	  (else (list-ref (cdr l) (-1+ i)))))
  unspecific)

(define (list-truncate! l i)
  (cond ((null? l) unspecific)
	((= i 1) (set-cdr! l '()))
	(else (list-truncate! (cdr l) (-1+ i))))
  unspecific)

(set! make-ring
(named-lambda (make-ring size)
  (if (< size 1)
      (error "Ring size too small" size)
      (vector "Ring" size '()))))

(set! ring-size
(named-lambda (ring-size ring)
  (length (vector-ref ring 2))))

(set! ring-clear!
(named-lambda (ring-clear! ring)
  (vector-set! ring 2 '())
  unspecific))

(set! ring-empty?
(named-lambda (ring-empty? ring)
  (null? (vector-ref ring 2))))

(set! ring-push!
(named-lambda (ring-push! ring object)
  (vector-set! ring 2 (cons object (vector-ref ring 2)))
  (list-truncate! (vector-ref ring 2) (vector-ref ring 1))))

(set! ring-pop!
(named-lambda (ring-pop! ring)
  (let ((l (vector-ref ring 2)))
    (if (null? l)
	(error "Ring empty" ring)
	(let ((object (car l)))
	  (vector-set! ring 2 (append! (cdr l) (list object)))
	  object)))))

(set! ring-ref
(named-lambda (ring-ref ring index)
  (list-ref (vector-ref ring 2) (remainder index (ring-size ring)))))

(set! ring-set!
(named-lambda (ring-set! ring index object)
  (list-set! (vector-ref ring 2) (remainder index (ring-size ring)) object)))

)