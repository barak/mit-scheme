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

(declare (usual-integrations)
	 (automagic-integrations)
	 (open-block-optimizations)
	 (eta-substitution))

;;; simple table abstraction
;;;
;;; A table is a mutable mapping from key to value.  There is a
;;; comparison function to determine whether two keys are the same
 
;;; A table is a 4 tuple consisting of a get-function, a put-function,
;;; a remove-function, and a function to handle anything else.
;;;

;;; My big problem with this is that we have to go through the continuation
;;; passing style get function whether we want to or not.

(define-structure (table (conc-name %table-)
			 (constructor %make-table))
  (get-function false read-only true)
  (put!-function false read-only true)
  (remove!-function false read-only true)
  (anything-else false read-only true))

(define-integrable (table-get table key if-found if-not-found)
  ((%table-get-function table) key if-found if-not-found))

(define-integrable (table-put! table key value)
  ((%table-put!-function table) key value))

(define-integrable (table-remove! table key)
  ((%table-remove!-function table) key))

(define-integrable (table-function table operation arglist)
  ((%table-anything-else table) operation arglist))

(define (table-get-chain key1 if-found if-not-found . tables)
  (let loop ((table-list tables)
	     (key        key1))
    (if (null? table-list)
	(if-found key)
	(table-get (car table-list) key
	  (lambda (value) 
	    (loop (cdr table-list) value))
	  if-not-found))))

(define (table-get-list table keylist)
  (map (lambda (key)
	 (table-get table key 
		    identity-procedure
		    (lambda () #f)))
       keylist))

;;; Returns a table

(define (make-generic-eq?-table)
  (let ((the-table '()))

    (declare (integrate make-entry 
			entry-value
			set-entry-value!
			lookup 
			extend-table!))

    (define make-entry cons)
    (define entry-value cdr)
    (define set-entry-value! set-cdr!)

    (define (lookup key)
      (declare (integrate key))
      (assq key the-table))

    (define (extend-table! entry)
      (declare (integrate entry))
      (set! the-table (cons entry the-table)))

    ;; User functions

    (define (get key if-found if-not-found)
      (let ((entry (lookup key)))
	(if (null? entry)
	    (if-not-found)
	    (if-found (entry-value entry)))))

    (define (put! key value)
      (let ((entry (lookup key)))
	(if (null? entry)
	    (extend-table! (make-entry key value))
	    (set-entry-value! entry value))))

    (define (remove! key)
      (set! the-table (del-assq key the-table)))

    (define (dispatch message args)
      args
      (case message
	((predicate) eq?)
	(else (error "Don't understand that message"))))

    (%make-table get put! remove! dispatch)))