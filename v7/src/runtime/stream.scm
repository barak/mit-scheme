#| -*-Scheme-*-

$Id: stream.scm,v 14.10 1998/03/31 20:04:18 cph Exp $

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

(define (stream-length stream)
  (let loop ((stream stream) (length 0))
    (if (stream-pair? stream)
	(loop (force (cdr stream)) (+ length 1))
	(begin
	  (if (not (null? stream))
	      (error:wrong-type-argument stream "stream" 'STREAM-LENGTH))
	  length))))

(define (stream-ref stream index)
  (let ((tail (stream-tail stream index)))
    (if (not (stream-pair? tail))
	(error:bad-range-argument index 'STREAM-REF))
    (car tail)))

(define (stream-head stream index)
  (if (not (exact-nonnegative-integer? index))
      (error:wrong-type-argument index
				 "exact nonnegative integer"
				 'STREAM-HEAD))
  (let loop ((stream stream) (index index))
    (if (= 0 index)
	'()
	(begin
	  (if (not (stream-pair? stream))
	      (error:bad-range-argument index 'STREAM-HEAD))
	  (cons (car stream)
		(loop (force (cdr stream)) (- index 1)))))))

(define (stream-tail stream index)
  (if (not (exact-nonnegative-integer? index))
      (error:wrong-type-argument index
				 "exact nonnegative integer"
				 'STREAM-TAIL))
  (let loop ((stream stream) (index index))
    (if (= 0 index)
	stream
	(begin
	  (if (not (stream-pair? stream))
	      (error:bad-range-argument index 'STREAM-TAIL))
	  (loop (force (cdr stream)) (- index 1))))))

(define (stream-map procedure stream . streams)
  (if (null? streams)
      (let ((stream-map-1
	     (lambda (procedure stream)
	       (let loop ((stream stream))
		 (if (stream-pair? stream)
		     (cons-stream (procedure (car stream))
				  (loop (force (cdr stream))))
		     (begin
		       (if (not (null? stream))
			   (error:wrong-type-argument stream
						      "stream"
						      'STREAM-MAP))
		       '()))))))
	;; Kludge: accept arguments in old order.
	(if (or (null? procedure) (stream-pair? procedure))
	    (stream-map-1 stream procedure)
	    (stream-map-1 procedure stream)))
      (let n-loop ((streams (cons stream streams)))
	(let parse-cars ((streams streams) (cars '()) (cdrs '()))
	  (cond ((null? streams)
		 (cons (apply procedure (reverse! cars))
		       (let ((cdrs (reverse! cdrs)))
			 (delay (n-loop (map force cdrs))))))
		((stream-pair? (car streams))
		 (parse-cars (cdr streams)
			     (cons (car (car streams)) cars)
			     (cons (cdr (car streams)) cdrs)))
		(else
		 (if (not (null? (car streams)))
		     (error:wrong-type-argument (car streams)
						"stream"
						'STREAM-MAP))
		 '()))))))

(define (stream-for-each procedure stream . streams)
  (if (null? streams)
      (let loop ((stream stream))
	(cond ((stream-pair? stream)
	       (procedure (car stream))
	       (loop (force (cdr stream))))
	      ((not (null? stream))
	       (error:wrong-type-argument stream "stream" 'STREAM-FOR-EACH))))
      (let n-loop ((streams (cons stream streams)))
	(let parse-cars ((streams streams) (cars '()) (cdrs '()))
	  (cond ((null? streams)
		 (apply procedure (reverse! cars))
		 (n-loop (map force (reverse! cdrs))))
		((stream-pair? (car streams))
		 (parse-cars (cdr streams)
			     (cons (car (car streams)) cars)
			     (cons (cdr (car streams)) cdrs)))
		((not (null? (car streams)))
		 (error:wrong-type-argument (car streams)
					    "stream"
					    'STREAM-FOR-EACH)))))))

(define (stream-append . streams)
  (if (null? streams)
      '()
      (let outer-loop ((streams streams))
	(if (null? (cdr streams))
	    (car streams)
	    (let inner-loop ((stream (car streams)))
	      (if (stream-pair? stream)
		  (cons-stream (car stream)
			       (inner-loop (force (cdr stream))))
		  (begin
		    (if (not (null? stream))
			(error:wrong-type-argument (car streams)
						   "stream"
						   'STREAM-APPEND))
		    (outer-loop (cdr streams)))))))))

(define (stream-accumulate procedure initial stream)
  (let loop ((stream stream))
    (if (stream-pair? stream)
	(procedure (car stream)
		   (loop (force (cdr stream))))
	(begin
	  (if (not (null? stream))
	      (error:wrong-type-argument stream "stream" 'STREAM-ACCUMULATE))
	  initial))))

(define (stream-filter predicate stream)
  (let loop ((stream stream))
    (if (stream-pair? stream)
	(if (predicate (car stream))
	    (cons-stream (car stream) (loop (force (cdr stream))))
	    (loop (force (cdr stream))))
	(begin
	  (if (not (null? stream))
	      (error:wrong-type-argument stream "stream" 'STREAM-FILTER))
	  '()))))

(define (stream-write stream #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee-output-port port))))
    (let loop ((stream stream) (leader #\{))
      (if (stream-pair? stream)
	  (begin
	    (write-char leader port)
	    (write (car stream) port)
	    (loop (force (cdr stream)) #\space))
	  (begin
	    (if (not (null? stream))
		(error:wrong-type-argument stream "stream" 'STREAM-WRITE))
	    (write-char #\} port))))))

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
	    (error:wrong-type-argument stream "stream" 'STREAM->LIST))
	'())))

(define prime-numbers-stream)

(define (make-prime-numbers-stream)
  (cons-stream
   2
   (letrec ((primes
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