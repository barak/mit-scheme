#| -*-Scheme-*-

$Id: stream.scm,v 14.15 2003/02/14 18:28:34 cph Exp $

Copyright (c) 1988-1999, 2002 Massachusetts Institute of Technology

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

;;;; Basic Stream Operations
;;; package: (runtime stream)

(declare (usual-integrations))

(define (stream-pair? stream)
  (and (pair? stream)
       (promise? (cdr stream))))

(define (stream-car stream)
  (if (not (stream-pair? stream))
      (error:wrong-type-argument stream "stream" 'STREAM-CAR))
  (car stream))

(define (stream-cdr stream)
  (if (not (stream-pair? stream))
      (error:wrong-type-argument stream "stream" 'STREAM-CDR))
  (force (cdr stream)))

(define the-empty-stream '())
(define stream-null? null?)
(define empty-stream? stream-null?)
(define stream-first stream-car)
(define stream-rest stream-cdr)
(define head stream-car)
(define tail stream-cdr)

(define (stream . list)
  (list->stream list))

(define stream-length
  (letrec
      ((loop
	(lambda (stream length)
	  (if (stream-pair? stream)
	      (loop (force (cdr stream)) (+ length 1))
	      (begin
		(if (not (null? stream))
		    (error:illegal-stream-element stream 'STREAM-LENGTH 0))
		length)))))
    (lambda (stream)
      (loop stream 0))))

(define (stream-ref stream index)
  (let ((tail (stream-tail stream index)))
    (if (not (stream-pair? tail))
	(error:bad-range-argument index 'STREAM-REF))
    (car tail)))

(define stream-head
  (letrec
      ((loop
	(lambda (stream index)
	  (if (= 0 index)
	      '()
	      (begin
		(if (not (stream-pair? stream))
		    (error:bad-range-argument index 'STREAM-HEAD))
		(cons (car stream)
		      (loop (force (cdr stream)) (- index 1))))))))
    (lambda (stream index)
      (if (not (exact-nonnegative-integer? index))
	  (error:wrong-type-argument index
				     "exact nonnegative integer"
				     'STREAM-HEAD))
      (loop stream index))))

(define stream-tail
  (letrec
      ((loop
	(lambda (stream index)
	  (if (= 0 index)
	      stream
	      (begin
		(if (not (stream-pair? stream))
		    (error:bad-range-argument index 'STREAM-TAIL))
		(loop (force (cdr stream)) (- index 1)))))))
    (lambda (stream index)
      (if (not (exact-nonnegative-integer? index))
	  (error:wrong-type-argument index
				     "exact nonnegative integer"
				     'STREAM-TAIL))
      (loop stream index))))

(define stream-map
  (letrec
      ((do-1
	(lambda (procedure stream)
	  (if (stream-pair? stream)
	      (cons-stream (procedure (car stream))
			   (do-1 procedure (force (cdr stream))))
	      (begin
		(if (not (null? stream))
		    (error:illegal-stream-element stream 'STREAM-MAP 1))
		'()))))
       (do-n
	(lambda (procedure streams)
	  (call-with-values (lambda () (split-streams streams 'STREAM-MAP))
	    (lambda (cars cdrs)
	      (if (null? cars)
		  '()
		  (cons (apply procedure cars)
			(delay (do-n procedure (map force cdrs))))))))))
    (lambda (procedure stream . streams)
      (if (null? streams)
	  ;; Kludge: accept arguments in old order.
	  (if (or (null? procedure) (stream-pair? procedure))
	      (do-1 stream procedure)
	      (do-1 procedure stream))
	  (do-n procedure (cons stream streams))))))

(define stream-for-each
  (letrec
      ((do-1
	(lambda (procedure stream)
	  (cond ((stream-pair? stream)
		 (procedure (car stream))
		 (do-1 procedure (force (cdr stream))))
		((not (null? stream))
		 (error:illegal-stream-element stream 'STREAM-FOR-EACH 1)))))
       (do-n
	(lambda (procedure streams)
	  (call-with-values
	      (lambda () (split-streams streams 'STREAM-FOR-EACH))
	    (lambda (cars cdrs)
	      (if (not (null? cars))
		  (begin
		    (apply procedure cars)
		    (do-n procedure (map force cdrs)))))))))
    (lambda (procedure stream . streams)
      (if (null? streams)
	  (do-1 procedure stream)
	  (do-n procedure (cons stream streams))))))

(define (split-streams streams operator)
  (let ((cars (list 'CARS))
	(cdrs (list 'CDRS)))
    (let loop ((streams streams) (cars-tail cars) (cdrs-tail cdrs) (n 0))
      (if (null? streams)
	  (values (cdr cars) (cdr cdrs))
	  (let ((stream (car streams)))
	    (if (stream-pair? stream)
		(let ((cars-tail* (list (car stream)))
		      (cdrs-tail* (list (cdr stream))))
		  (set-cdr! cars-tail cars-tail*)
		  (set-cdr! cdrs-tail cdrs-tail*)
		  (loop (cdr streams) cars-tail* cdrs-tail* (fix:+ n 1)))
		(begin
		  (if (not (null? stream))
		      (error:illegal-stream-element stream operator n))
		  (values '() '()))))))))

(define stream-append
  (letrec
      ((outer-loop
	(lambda (streams n)
	  (if (null? (cdr streams))
	      (car streams)
	      (inner-loop (car streams) (cdr streams) n))))
       (inner-loop
	(lambda (stream streams n)
	  (if (stream-pair? stream)
	      (cons-stream (car stream)
			   (inner-loop (force (cdr stream)) streams n))
	      (begin
		(if (not (null? stream))
		    (error:illegal-stream-element stream 'STREAM-APPEND n))
		(outer-loop streams (fix:+ n 1)))))))
    (lambda streams
      (if (null? streams)
	  '()
	  (outer-loop streams 0)))))

(define (stream-accumulate procedure initial stream)
  (if (stream-pair? stream)
      (procedure (car stream)
		 (stream-accumulate procedure initial (force (cdr stream))))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'STREAM-ACCUMULATE 2))
	initial)))

(define (stream-filter predicate stream)
  (if (stream-pair? stream)
      (if (predicate (car stream))
	  (cons-stream (car stream)
		       (stream-filter predicate (force (cdr stream))))
	  (stream-filter predicate (force (cdr stream))))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'STREAM-FILTER 1))
	'())))

(define stream-write
  (letrec
      ((loop
	(lambda (stream leader port)
	  (if (stream-pair? stream)
	      (begin
		(write-char leader port)
		(write (car stream) port)
		(loop (force (cdr stream)) #\space port))
	      (begin
		(if (not (null? stream))
		    (error:illegal-stream-element stream 'STREAM-WRITE 0))
		(write-char #\} port))))))
    (lambda (stream #!optional port)
      (loop stream
	    #\{
	    (if (default-object? port)
		(current-output-port)
		(guarantee-output-port port 'STREAM-WRITE))))))

(define (list->stream list)
  (if (pair? list)
      (cons-stream (car list) (list->stream (cdr list)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'LIST->STREAM))
	'())))

(define (stream->list stream)
  (if (stream-pair? stream)
      (cons (car stream)
	    (stream->list (force (cdr stream))))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'STREAM->LIST 0))
	'())))

(define prime-numbers-stream)

(define (make-prime-numbers-stream)
  (cons-stream
   2
   (letrec
       ((primes
	 (cons-stream
	  (cons 3 9)
	  (let filter ((integer 5))
	    (let loop ((primes primes))
	      (let ((prime (car primes)))
		(cond ((< integer (cdr prime))
		       (cons-stream (cons integer (* integer integer))
				    (filter (+ integer 2))))
		      ((= 0 (remainder integer (car prime)))
		       (filter (+ integer 2)))
		      (else
		       (loop (force (cdr primes)))))))))))
     (let loop ((primes primes))
       (cons-stream (car (car primes))
		    (loop (force (cdr primes))))))))

(define (initialize-package!)
  (let ((reset-primes!
	 (lambda ()
	   (set! prime-numbers-stream (make-prime-numbers-stream))
	   unspecific)))
    (reset-primes!)
    (add-secondary-gc-daemon! reset-primes!)))

(define (error:illegal-stream-element stream operator operand)
  (error (make-illegal-stream-element "stream" stream operator operand)))

(define make-illegal-stream-element)
(define condition-type:illegal-stream-element)

(define (initialize-conditions!)
  (set! condition-type:illegal-stream-element
	(make-condition-type 'ILLEGAL-STREAM-ELEMENT
	    condition-type:wrong-type-argument
	    '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'DATUM) port)
	    (write-string ", occurring in the " port)
	    (write-string (ordinal-number-string
			   (+ (access-condition condition 'OPERAND) 1))
			  port)
	    (write-string " argument to " port)
	    (write-operator (access-condition condition 'OPERATOR) port)
	    (write-string ", is not a stream." port))))
  (set! make-illegal-stream-element
	(condition-constructor condition-type:illegal-stream-element
			       '(TYPE DATUM OPERATOR OPERAND)))
  unspecific)