#| -*-Scheme-*-

$Id: emodel.scm,v 4.2 1993/01/02 07:33:35 cph Exp $

Copyright (c) 1987, 1993 Massachusetts Institute of Technology

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

;;;; SCode Optimizer: Environment Model

(declare (usual-integrations)
	 (integrate-external "object"))

(define (block/make parent safe? bound-variables)
  (let ((block
	 (%block/make parent
		      safe?
		      (let ((n-bound-variables (length bound-variables)))
			(if (fix:<= n-bound-variables block-hash-table-limit)
			    (cons n-bound-variables bound-variables)
			    (make-hash-table bound-variables))))))
    (if parent
	(set-block/children! parent (cons block (block/children parent))))
    block))

(define (variable/make&bind! block name)
  (let ((variable (variable/make block name '()))
	(bound-variables (block/bound-variables block)))
    (cond ((hash-table? bound-variables)
	   (hash-table-store! bound-variables variable))
	  ((fix:= (car bound-variables) block-hash-table-limit)
	   (set-block/bound-variables!
	    block
	    (make-hash-table (cons variable (cdr bound-variables)))))
	  (else
	   (set-car! bound-variables (fix:+ (car bound-variables) 1))
	   (set-cdr! bound-variables (cons variable (cdr bound-variables)))))
    variable))

(define-integrable block-hash-table-limit
  20)

(define (block/lookup-name block name intern?)
  (let search ((block block))
    (let ((bound-variables (block/bound-variables block)))
      (if (hash-table? bound-variables)
	  (or (hash-table-lookup bound-variables name)
	      (if (block/parent block)
		  (search (block/parent block))
		  (and intern? (variable/make&bind! block name))))
	  (let loop ((variables (cdr bound-variables)))
	    (cond ((null? variables)
		   (if (block/parent block)
		       (search (block/parent block))
		       (and intern? (variable/make&bind! block name))))
		  ((eq? name (variable/name (car variables)))
		   (car variables))
		  (else
		   (loop (cdr variables)))))))))

(define (block/limited-lookup block name limit)
  (let search ((block block))
    (and (not (eq? block limit))
	 (let ((bound-variables (block/bound-variables block)))
	   (if (hash-table? bound-variables)
	       (or (hash-table-lookup bound-variables name)
		   (and (block/parent block)
			(search (block/parent block))))
	       (let loop ((variables (cdr bound-variables)))
		 (cond ((null? variables)
			(and (block/parent block)
			     (search (block/parent block))))
		       ((eq? name (variable/name (car variables)))
			(car variables))
		       (else
			(loop (cdr variables))))))))))

(define-structure (hash-table
		   (type vector)
		   (named (string->symbol "#[(scode-optimizer)hash-table]"))
		   (constructor %make-hash-table))
  count
  buckets)

(define (make-hash-table variables)
  (let ((count (length variables)))
    (let ((buckets (make-hash-table-buckets (fix:+ count 1))))
      (let ((table (%make-hash-table count buckets)))
	(for-each (lambda (variable)
		    (%hash-table-store! buckets variable))
		  variables)
	table))))

(define (hash-table-store! table variable)
  (let ((count (fix:+ (hash-table-count table) 1)))
    (if (fix:= count (vector-length (hash-table-buckets table)))
	(let ((old-buckets (hash-table-buckets table)))
	  (let ((new-buckets (make-hash-table-buckets (fix:+ count count))))
	    (do ((h 0 (fix:+ h 1)))
		((fix:= h count))
	      (let ((variable (vector-ref old-buckets h)))
		(if variable
		    (%hash-table-store! new-buckets variable))))
	    (set-hash-table-buckets! table new-buckets))))
    (set-hash-table-count! table count))
  (%hash-table-store! (hash-table-buckets table) variable))

(define (%hash-table-store! buckets variable)
  (let ((k (symbol-hash (variable/name variable)))
	(m (vector-length buckets)))
    (let ((h1 (modulo k m)))
      (if (not (vector-ref buckets h1))
	  (vector-set! buckets h1 variable)
	  (let ((h2 (fix:+ (modulo k (fix:- m 1)) 1)))
	    (let loop ((h h1))
	      (let ((h
		     (let ((h (fix:+ h h2)))
		       (if (fix:< h m)
			   h
			   (fix:- h m)))))
		(if (not (vector-ref buckets h))
		    (vector-set! buckets h variable)
		    (loop h)))))))))

(define (make-hash-table-buckets n)
  (make-vector (let loop ((primes prime-numbers-stream))
		 (if (<= n (car primes))
		     (car primes)
		     (loop (force (cdr primes)))))
	       false))

(define (hash-table-lookup table name)
  (let ((buckets (hash-table-buckets table)))
    (let ((k (symbol-hash name))
	  (m (vector-length buckets)))
      (let ((h1 (modulo k m)))
	(let ((variable (vector-ref buckets h1)))
	  (and variable
	       (if (eq? name (variable/name variable))
		   variable
		   (let ((h2 (fix:+ (modulo k (fix:- m 1)) 1)))
		     (let loop ((h h1))
		       (let ((h
			      (let ((h (fix:+ h h2)))
				(if (fix:< h m)
				    h
				    (fix:- h m)))))
			 (let ((variable (vector-ref buckets h)))
			   (and variable
				(if (eq? name (variable/name variable))
				    variable
				    (loop h))))))))))))))

(define (block/lookup-names block names intern?)
  (map (lambda (name)
	 (block/lookup-name block name intern?))
       names))

(define (block/for-each-bound-variable block procedure)
  (let ((bound-variables (block/bound-variables block)))
    (if (hash-table? bound-variables)
	(let ((buckets (hash-table-buckets bound-variables)))
	  (let ((m (vector-length buckets)))
	    (do ((h 0 (fix:+ h 1)))
		((fix:= h m))
	      (if (vector-ref buckets h)
		  (procedure (vector-ref buckets h))))))
	(for-each procedure (cdr bound-variables)))))

(define (block/bound-variables-list block)
  (let ((bound-variables (block/bound-variables block)))
    (if (hash-table? bound-variables)
	(let ((buckets (hash-table-buckets bound-variables)))
	  (let ((m (vector-length buckets)))
	    (let loop ((h 0) (result '()))
	      (if (fix:= h m)
		  result
		  (loop (fix:+ h 1)
			(if (vector-ref buckets h)
			    (cons (vector-ref buckets h) result)
			    result))))))
	(cdr bound-variables))))

(define (block/unsafe! block)
  (if (block/safe? block)
      (begin
	(set-block/safe?! block false)
	(if (block/parent block)
	    (block/unsafe! (block/parent block))))))