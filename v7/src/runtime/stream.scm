#| -*-Scheme-*-

$Id: stream.scm,v 14.11 1998/04/01 08:16:01 cph Exp $

Copyright (c) 1988-98 Massachusetts Institute of Technology

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
		(guarantee-output-port port))))))

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