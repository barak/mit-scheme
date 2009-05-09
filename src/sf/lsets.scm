#| -*-Scheme-*-

$Id: lsets.scm,v 4.8 2008/01/30 20:02:38 cph Exp $

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

;;;; Unordered Set abstraction

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations))

#|

Each set has an ELEMENT-TYPE which is a predicate that all elements of
the set must satisfy.  Each set has a PREDICATE that is used to compare
identity of the elements.  An element appears in a set only once.

This code is bummed up the wazoo for speed.  It is derived from a SET
abstraction based on streams written by JRM.  I would not recommend trying
to figure out what is going on in this code.

;; User functions.

(define empty-set)
(define singleton-set)
(define list->set)
(define stream->set)
(define set-element-type)

(define set/member?)
(define set/adjoin)
(define set/adjoin*)
(define set/remove)
(define set->stream)
(define set->list)
(define set/for-each)
(define set/map)
(define set/empty?)
(define set/union)
(define set/union*)
(define set/intersection)
(define set/intersection*)

(define any-type?)

|#

(define-integrable (check-type element-type element)
  element-type element			;ignore
  true)

(define-integrable (member-procedure predicate) 
  predicate				;ignore
  memq)

(define-integrable (list-deletor predicate)
  (letrec ((list-deletor-loop
	    (lambda (list)
	      (if (pair? list)
		  (if (predicate (car list))
		      (list-deletor-loop (cdr list))
		      (cons (car list) (list-deletor-loop (cdr list))))
		  '()))))
    list-deletor-loop))

(define-integrable (set? object)
  object				;ignore
  true)

(define-integrable (%make-set element-type predicate elements)
  element-type predicate		;ignore
  elements)

(define-integrable (%unsafe-set-element-type set)
  set					;ignore
  (lambda (object) 
    (declare (integrate object))
    object				;ignore
    true))

(define-integrable (%unsafe-set-predicate set) 
  set					;ignore
  eq?)

(define-integrable (%unsafe-set-elements set)
  set)

(define-integrable (set-element-type set)
  (%unsafe-set-element-type set))

(define-integrable (adjoin-lists-without-duplicates predicate l1 l2)
  predicate				;ignore
  (let loop ((new-list l1) (old-list l2))
    (cond ((null? old-list) new-list)
	  ((memq (car old-list) new-list) (loop new-list (cdr old-list)))
	  (else (loop (cons (car old-list) new-list) (cdr old-list))))))

(define-integrable (invert-sense predicate)
  (lambda (object)
    (declare (integrate object))
    (not (predicate object))))

(define-integrable (%subset predicate list)
  ((list-deletor (invert-sense predicate)) list))

(define-integrable (remove-duplicates predicate list)
  (adjoin-lists-without-duplicates predicate '() list))

(define (empty-set element-type predicate)
  (%make-set element-type predicate '()))

(define (singleton-set element-type predicate element)
  (check-type element-type element)
  (%make-set element-type predicate (cons element '())))

(define (list->set element-type predicate elements)
  (%make-set element-type predicate
	     (let loop ((elements (apply list elements)))
	       (cond ((null? elements) '())
		     ((check-type element-type (car elements))
		      (remove-duplicates predicate 
				         (cons (car elements)
					       (loop (cdr elements)))))
		     (else (error "Can't happen"))))))

(define (stream->set element-type predicate stream)
  (%make-set element-type predicate
	     (let loop ((stream stream))
	       (cond ((empty-stream? stream) '())
		     ((check-type element-type (head stream))
		      (remove-duplicates predicate
					 (cons (head stream)
					       (loop (tail stream)))))
		     (else (error "Can't happen"))))))

;;; End of speed hack.

(declare (integrate-operator spread-set))
(define (spread-set set receiver)
  (declare (integrate receiver))
  (if (not (set? set))
      (error "Object not a set" set))
  (receiver (%unsafe-set-element-type set)
	    (%unsafe-set-predicate    set)
	    (%unsafe-set-elements     set)))

#|
(define (spread-2-sets set1 set2 receiver)
  (declare (integrate set1 set2 receiver))
  (spread-set set1
    (lambda (etype1 pred1 stream1)
      (spread-set set2
        (lambda (etype2 pred2 stream2)
	  (declare (integrate etype2 pred2))
	  (if (not (and (eq? etype1 etype2)
			(eq? pred1  pred2)))
	      (error "Set mismatch")
	      (receiver etype1 pred1 stream1 stream2)))))))
|#
(define-integrable (spread-2-sets set1 set2 receiver)
  (spread-set set1
    (lambda (etype1 pred1 stream1)
      (declare (integrate etype1 pred1))
      (spread-set set2
        (lambda (etype2 pred2 stream2)
	  etype2 pred2 ; are ignored
	  (receiver etype1 pred1 stream1 stream2))))))

(define (set/member? set element)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate element-type predicate stream))
      (check-type element-type element)
      ((member-procedure predicate) element list))))

(declare (integrate-operator adjoin-element))
(define (adjoin-element predicate element list)
  (declare (integrate list))
  predicate				;ignore
  (if (memq element list)
      list
      (cons element list)))

(define (set/adjoin set element)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      (check-type element-type element)
      (%make-set element-type predicate
		 (adjoin-element predicate element list)))))

(define (set/adjoin* set element-list)
  (if (null? element-list)
      set
      (set/adjoin (set/adjoin* set (cdr element-list)) (car element-list))))

(define (set/remove set element)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      (check-type element-type element)
      (%make-set element-type predicate (delq element list)))))

(define (set/subset set subset-predicate)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate element-type predicate list))
      (%make-set element-type predicate
		 (%subset subset-predicate list)))))

(define (set->stream set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type predicate		;ignore
      (list->stream list))))

(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list->stream (cdr list)))))

(define (set->list set)
  (spread-set set
    (lambda (element-type predicate l)
      (declare (integrate list))
      element-type predicate		;ignore
      (apply list l))))

(define (set/for-each function set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type predicate		;ignore
      (for-each function list))))

#|
(define (set/map new-element-type new-predicate function set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type predicate		;ignore
      (%make-set new-element-type new-predicate
		 (remove-duplicates
		  new-predicate
		  (map (lambda (element)
			 (let ((new-element (function element)))
			   (if (new-element-type new-element)
			       new-element
			       (error "Element of wrong type" new-element))))
		       list))))))
|#

(define (set/map new-element-type new-predicate function set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type predicate		;ignore
      (%make-set new-element-type new-predicate
		 (remove-duplicates eq? (map function list))))))

(define (set/empty? set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type predicate		;ignore
      (null? list))))

(define (interleave l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (interleave l2 (cdr l1)))))

(define (set/union s1 s2)
  (spread-2-sets s1 s2
    (lambda (etype pred list1 list2)
      (declare (integrate etype list1 list2))
      (%make-set
       etype pred
       (adjoin-lists-without-duplicates pred list1 list2)))))

(define (set/union* . sets)
  (cond ((null? sets) (error "Set/union* with no args"))
	((null? (cdr sets)) (car sets))
	(else (set/union (car sets) (apply set/union* (cdr sets))))))

(define (set/intersection s1 s2)
  (spread-2-sets s1 s2
    (lambda (etype pred l1 l2)
      (%make-set etype pred
		 (let loop ((elements l1))
		   (cond ((null? elements) '())
			 (((member-procedure pred) (car elements) l2)
			  (cons (car elements) (loop (cdr elements))))
			 (else (loop (cdr elements)))))))))

(define (set/intersection* . sets)
  (cond ((null? sets) (error "set/intersection* with no args"))
	((null? (cdr sets)) (car sets))
	(else (set/intersection (car sets)
				(apply set/intersection* (cdr sets))))))

(define (set/difference set1 set2)
  (spread-2-sets set1 set2
    (lambda (etype pred l1 l2)
      (declare (integrate etype l1 l2))
      (%make-set etype pred
		 (%subset (lambda (l1-element)
			    (not ((member-procedure pred) l1-element l2)))
			  l1)))))

(define (any-type? element)
  element				;ignore
  true)