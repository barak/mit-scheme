#| -*-Scheme-*-

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Unordered Set abstraction

(declare (usual-integrations))
(declare (automagic-integrations))
(declare (open-block-optimizations))


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

(using-syntax sf-syntax-table

(declare (integrate-operator list-deletor member-procedure))

(declare (integrate empty-set
		    singleton-set
		    set/member?
		    set/adjoin
		    set/remove
		    set->list
		    set/for-each
		    set/map
		    set/empty?
		    ))

#|

;;; Snarfed from "runtime/list.scm"

(define (member-procedure predicate)
  (lambda (element list)
    (let loop ((list list))
      (and (pair? list)
	   (if (predicate (car list) element)
	       list
	       (loop (cdr list)))))))

(define (list-deletor predicate)
  (define (list-deletor-loop list)
    (if (pair? list)
	(if (predicate (car list))
	    (list-deletor-loop (cdr list))
	    (cons (car list) (list-deletor-loop (cdr list))))
	'()))
  list-deletor-loop)

(define-named-structure set element-type predicate elements)

((access add-unparser-special-object! unparser-package)
 *set-tag
 (lambda (set)
   (unparse-with-brackets
    (lambda ()
      (write-string "Unordered Set ")
      (write (hash set))
      (write-string " of ")
      (display (%set-element-type set))))))

(define-integrable (check-type element-type element)
  (or (element-type element)
      (error "Element of wrong type -- CHECK-TYPE" element-type element)))
|#

(define-integrable (check-type element-type element)
  element-type element ;are ignored
  #t)

(define-integrable (member-procedure predicate) 
  predicate ; ignore
  memq)

(define (list-deletor predicate)
  (declare (integrate predicate))
  (define (list-deletor-loop list)
    (if (pair? list)
	(if (predicate (car list))
	    (list-deletor-loop (cdr list))
	    (cons (car list) (list-deletor-loop (cdr list))))
	'()))
  list-deletor-loop)

(define-integrable (set? object) object #t)

(define-integrable (%make-set element-type predicate elements)
  element-type ; ignore two
  predicate
  elements)

(define-integrable (%unsafe-set-element-type set)
  set	; ignore
  (lambda (object) 
    (declare (integrate object))
    object ; ignore
    #t))

(define-integrable (%unsafe-set-predicate set) 
  set ; ignore
  eq?)

(define-integrable (%unsafe-set-elements set) set)

(define-integrable (set-element-type set)
  (%unsafe-set-element-type set))

(declare (integrate-operator adjoin-lists-without-duplicates))

(define (adjoin-lists-without-duplicates predicate l1 l2)
  predicate ; is ignored
  (declare (integrate  l1 l2))
  (let ((member? memq))
    (declare (integrate member?))
    (define (loop new-list old-list)
      (cond ((null? old-list) new-list)
	    ((member? (car old-list) new-list) (loop new-list (cdr old-list)))
	    (else (loop (cons (car old-list) new-list) (cdr old-list)))))
    (loop l1 l2)))

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

(declare (integrate-operator spread-set spread-2-sets))

(define (spread-set set receiver)
  (declare (integrate receiver))
  (if (not (set? set))
      (error "Object not a set" set)
      (receiver (%unsafe-set-element-type set)
		(%unsafe-set-predicate    set)
		(%unsafe-set-elements     set))))

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
(define (spread-2-sets set1 set2 receiver)
  (declare (integrate set1 set2 receiver))
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
  (declare (integrate  list))
  predicate ; is ignored
  (cons element (delq element list)))

(define (set/adjoin set element)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate stream))
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
      element-type
      predicate
      (list->stream list))))

(define (list->stream list)
  (if (null? list)
      the-empty-stream
      (cons-stream (car list) (list->stream (cdr list)))))

(define (set->list set)
  (spread-set set
    (lambda (element-type predicate l)
      (declare (integrate list))
      element-type
      predicate
      (apply list l))))

(define (set/for-each function set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type
      predicate
      (for-each function list))))

#|
(define (set/map new-element-type new-predicate function set)
  (spread-set set
    (lambda (e p list)
      (declare (integrate list))
      e
      p
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
    (lambda (e p l)
      (declare (integrate list))
      e
      p
      (%make-set new-element-type new-predicate
		 (remove-duplicates eq? (map function l))))))

(define (set/empty? set)
  (spread-set set
    (lambda (element-type predicate list)
      (declare (integrate list))
      element-type
      predicate
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

(define (any-type? element) element true)

)

