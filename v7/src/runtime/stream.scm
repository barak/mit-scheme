#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/stream.scm,v 14.5 1989/09/20 15:06:47 cph Exp $

Copyright (c) 1988, 1989 Massachusetts Institute of Technology

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

(define-integrable (stream-null? stream)
  (null? stream))

(define-integrable (stream-car stream)
  (car stream))

(define-integrable (stream-cdr stream)
  (force (cdr stream)))

(define (stream . list)
  (list->stream list))

(define (list->stream list)
  (if (pair? list)
      (cons-stream (car list) (list->stream (cdr list)))
      (begin (if (not (null? list))
		 (error "LIST->STREAM: not a proper list" list))
	     '())))

(define (stream->list stream)
  (if (stream-pair? stream)
      (cons (stream-car stream) (stream->list (stream-cdr stream)))
      (begin (guarantee-stream-null stream 'STREAM->LIST) '())))

(define (stream-length stream)
  (let loop ((stream stream) (length 0))
    (if (stream-pair? stream)
	(loop (stream-cdr stream) (1+ length))
	(begin (guarantee-stream-null stream 'STREAM-LENGTH) length))))

(define (stream-ref stream index)
  (let ((tail (stream-tail stream index)))
    (if (not (stream-pair? tail))
	(error "STREAM-REF: index too large" index))
    (stream-car tail)))

(define (stream-head stream index)
  (if (not (and (integer? index) (not (negative? index))))
      (error "STREAM-HEAD: index must be nonnegative integer" index))
  (let loop ((stream stream) (index index))
    (if (zero? index)
	'()
	(begin
	  (if (not (stream-pair? stream))
	      (error "STREAM-HEAD: stream has too few elements" stream index))
	  (cons (stream-car stream) (loop (stream-cdr stream) (-1+ index)))))))

(define (stream-tail stream index)
  (if (not (and (integer? index) (not (negative? index))))
      (error "STREAM-TAIL: index must be nonnegative integer" index))  (let loop ((stream stream) (index index))
    (if (zero? index)
	stream
	(begin (if (not (stream-pair? stream))
		   (error "STREAM-TAIL: index too large" index))
	       (loop (stream-cdr stream) (-1+ index))))))

(define (stream-map stream procedure)
  (let loop ((stream stream))
    (if (stream-pair? stream)
	(cons-stream (procedure (stream-car stream))
		     (loop (stream-cdr stream)))
	(begin (guarantee-stream-null stream 'STREAM-MAP) '()))))

(define (guarantee-stream-null stream name)
  (if (not (null? stream))
      (error (string-append (symbol->string name) ": not a proper stream")
	     stream)))

(define-integrable the-empty-stream
  '())

(define-integrable (empty-stream? stream)
  (stream-null? stream))

(define (head stream)
  (if (stream-pair? stream)
      (stream-car stream)
      (error "head: not a proper stream" stream)))

(define (tail stream)
  (if (stream-pair? stream)
      (stream-cdr stream)
      (error "tail: not a proper stream" stream)))

(define prime-numbers-stream)

(define (make-prime-numbers-stream)
  (cons-stream
   2
   (letrec ((primes
	     (cons-stream
	      (cons 3 9)
	      (let filter ((integer 5))
		(let loop ((primes primes))
		  (let ((prime (stream-car primes)))
		    (cond ((< integer (cdr prime))
			   (cons-stream (cons integer (* integer integer))
					(filter (+ integer 2))))
			  ((zero? (remainder integer (car prime)))
			   (filter (+ integer 2)))
			  (else
			   (loop (stream-cdr primes))))))))))
     (let loop ((primes primes))
       (cons-stream (car (stream-car primes))
		    (loop (stream-cdr primes)))))))

(define (initialize-package!)
  (let ((reset-primes!
	 (lambda ()
	   (set! prime-numbers-stream (make-prime-numbers-stream))
	   unspecific)))
    (reset-primes!)
    (add-secondary-gc-daemon! reset-primes!)))