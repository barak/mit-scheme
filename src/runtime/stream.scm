#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Basic Stream Operations
;;; package: (runtime stream)

(declare (usual-integrations))

(add-boot-deps! '(runtime number))

(define (stream-pair? stream)
  (and (pair? stream)
       (promise? (cdr stream))))

(define-integrable (%stream-car stream)
  (car stream))

(define-integrable (%stream-cdr stream)
  (force (cdr stream)))

(define (stream-car stream)
  (guarantee stream-pair? stream 'stream-car)
  (%stream-car stream))

(define (stream-cdr stream)
  (guarantee stream-pair? stream 'stream-cdr)
  (%stream-cdr stream))

(define the-empty-stream '())
(define stream-null? null?)
(define empty-stream? stream-null?)
(define stream-first stream-car)
(define stream-rest stream-cdr)
(define head stream-car)
(define tail stream-cdr)

(define (null-stream? s #!optional caller)
  (cond ((null? s) #t)
	((stream-pair? s) #f)
	(else (error:illegal-stream-element stream caller 0))))

(define (stream . list)
  (list->stream list))

(define (stream-length stream)
  (let loop ((stream stream) (n 0))
    (if (null-stream? stream 'stream-length)
	n
	(loop (%stream-cdr stream) (+ n 1)))))

(define (stream-ref stream index)
  (let ((tail (stream-tail stream index)))
    (if (not (stream-pair? tail))
	(error:bad-range-argument index 'stream-ref))
    (car tail)))

(define (stream-head stream index)
  (guarantee exact-nonnegative-integer? index 'stream-head)
  (let loop ((stream stream) (index index))
    (if (> index 0)
	(begin
	  (if (not (stream-pair? stream))
	      (error:bad-range-argument index 'stream-head))
	  (cons (%stream-car stream)
		(loop (%stream-cdr stream) (- index 1))))
	'())))

(define (stream-tail stream index)
  (guarantee exact-nonnegative-integer? index 'stream-tail)
  (let loop ((stream stream) (index index))
    (if (> index 0)
	(begin
	  (if (not (stream-pair? stream))
	      (error:bad-range-argument index 'stream-tail))
	  (loop (%stream-cdr stream) (- index 1)))
	stream)))

(define (stream-last stream)
  (stream-car (stream-last-pair stream)))

(define (stream-last-pair stream)
  (if (null-stream? stream 'stream-last-pair)
      (error:bad-range-argument stream 'stream-last-pair))
  (let loop ((stream stream))
    (let ((next (%stream-cdr stream)))
      (if (null-stream? next 'stream-last-pair)
	  stream
	  (loop next)))))

(define (stream-drop-while predicate stream)
  (let loop ((stream stream))
    (if (and (not (null-stream? stream 'stream-drop-while))
	     (predicate (%stream-car stream)))
	(loop (%stream-cdr stream))
	stream)))

(define (stream-take-while predicate stream)
  (let loop ((stream stream))
    (cond ((null-stream? stream 'stream-take-while)
	   stream)
	  ((predicate (%stream-car stream))
	   (cons (%stream-car stream)
		 (loop (%stream-cdr stream))))
	  (else
	   the-empty-stream))))

(define (stream-map procedure stream . streams)
  (cond ((pair? streams)
	 (let loop ((streams (cons stream streams)))
	   (receive (cars cdrs) (split-streams streams 'stream-map)
	     (if (pair? cars)
		 (cons-stream (apply procedure cars)
			      (loop (map force cdrs)))
		 '()))))
	((and (procedure? procedure)
	      (or (null? stream) (stream-pair? stream)))
	 (let loop ((stream stream))
	   (if (null-stream? stream 'stream-map)
	       stream
	       (cons-stream (procedure (%stream-car stream))
			    (loop (%stream-cdr stream))))))
	((and (procedure? stream)
	      (or (null? procedure) (stream-pair? procedure)))
	 ;; Kludge: accept arguments in old order.
	 (stream-map stream procedure))
	(else
	 (error "Unknown arguments to STREAM-MAP."))))

(define (stream-for-each procedure stream . streams)
  (if (pair? streams)
      (let loop ((streams (cons stream streams)))
	(receive (cars cdrs) (split-streams streams 'stream-for-each)
	  (if (pair? cars)
	      (begin
		(apply procedure cars)
		(loop (map force cdrs))))))
      (let loop ((stream stream))
	(if (not (null-stream? stream 'stream-for-each))
	    (begin
	      (procedure (%stream-car stream))
	      (loop (%stream-cdr stream)))))))

(define (split-streams streams operator)
  (let ((cars (list 'cars))
	(cdrs (list 'cdrs)))
    (let loop ((streams streams) (cars-tail cars) (cdrs-tail cdrs))
      (if (pair? streams)
	  (let ((stream (car streams)))
	    (if (null-stream? stream operator)
		(values '() '())
		(let ((cars-tail* (list (car stream)))
		      (cdrs-tail* (list (cdr stream))))
		  (set-cdr! cars-tail cars-tail*)
		  (set-cdr! cdrs-tail cdrs-tail*)
		  (loop (cdr streams) cars-tail* cdrs-tail*))))
	  (values (cdr cars) (cdr cdrs))))))

(define (stream-append-map procedure stream . streams)
  (let ((sappend
	 (lambda (s1 s2)
	   (let loop ((s s1))
	     (if (null-stream? s 'stream-append-map)
		 (force s2)
		 (cons-stream (%stream-car s) (loop (%stream-cdr s))))))))
    (if (pair? streams)
	(let loop ((streams (cons stream streams)))
	  (receive (cars cdrs) (split-streams streams 'stream-append-map)
	    (if (pair? cars)
		(sappend (apply procedure cars)
			 (delay (loop (map force cdrs))))
		the-empty-stream)))
	(let loop ((stream stream))
	  (if (null-stream? stream 'stream-append-map)
	      stream
	      (sappend (procedure (%stream-car stream))
		       (delay (loop (%stream-cdr stream)))))))))

(define (stream-append . streams)
  (if (pair? streams)
      (let outer-loop ((streams streams))
	(if (pair? (cdr streams))
	    (let inner-loop ((stream (car streams)))
	      (if (null-stream? stream 'stream-append)
		  (outer-loop (cdr streams))
		  (cons-stream (%stream-car stream)
			       (inner-loop (%stream-cdr stream)))))
	    (car streams)))
      the-empty-stream))

(define (stream-accumulate procedure initial stream)
  (let loop ((stream stream))
    (if (null-stream? stream 'stream-accumulate)
	initial
	(procedure (%stream-car stream)
		   (loop (%stream-cdr stream))))))

(define (stream-filter predicate stream)
  (let loop ((stream stream))
    (cond ((null-stream? stream 'stream-filter)
	   stream)
	  ((predicate (%stream-car stream))
	   (cons-stream (%stream-car stream)
			(loop (%stream-cdr stream))))
	  (else
	   (loop (%stream-cdr stream))))))

(define (stream-truncate stream predicate)
  (stream-take-while predicate stream))

(define (stream-write stream #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee textual-output-port? port 'stream-write))))
    (if (null-stream? stream 'stream-write)
	(write-string "{}" port)
	(begin
	  (write-char #\{ port)
	  (write (%stream-car stream) port)
	  (stream-for-each (lambda (object)
			     (write-char #\space port)
			     (write object port))
			   (%stream-cdr stream))
	  (write-char #\} port)))))

(define (list->stream list)
  (if (pair? list)
      (cons-stream (car list) (list->stream (cdr list)))
      (begin
	(if (not (null? list))
	    (error:not-a list? list 'list->stream))
	'())))

(define (stream->list stream)
  (let loop ((s stream) (elements '()))
    (if (null-stream? s 'stream->list)
	(reverse elements)
	(loop (%stream-cdr s) (cons (%stream-car s) elements)))))

(define (make-prime-numbers-stream)
  (let ((limit (fix:- (fix:largest-value) 2)))
    (define odd-primes (cons-stream 3 (fixnum-filter 5)))
    (define (fixnum-filter n)
      (if (fix:<= n limit)
	  (let loop ((ps odd-primes))
	    (cond ((fix:< n (fix:* (%stream-car ps) (%stream-car ps)))
		   (cons-stream n (fixnum-filter (fix:+ n 2))))
		  ((fix:= 0 (fix:remainder n (%stream-car ps)))
		   (fixnum-filter (fix:+ n 2)))
		  (else
		   (loop (%stream-cdr ps)))))
	  (generic-filter n)))
    (define (generic-filter n)
      (let loop ((ps odd-primes))
	(cond ((< n (square (%stream-car ps)))
	       (cons-stream n (generic-filter (+ n 2))))
	      ((= 0 (remainder n (%stream-car ps)))
	       (generic-filter (+ n 2)))
	      (else
	       (loop (%stream-cdr ps))))))
    (cons-stream 2 odd-primes)))

(define prime-numbers-stream)
(define (reset-primes!)
  (set! prime-numbers-stream (make-prime-numbers-stream))
  unspecific)
(add-boot-init!
 (lambda ()
   (reset-primes!)
   (add-secondary-gc-daemon! reset-primes!)))

(define (generator->stream g)
  (define (stream-generator)
    (let ((object (g)))
      (if (eof-object? object)
	  the-empty-stream
	  (cons-stream object (stream-generator)))))
  (stream-generator))

(define condition-type:illegal-stream-element)
(define error:illegal-stream-element)
(seq:after-conditions 'add-action!
  (lambda ()
    (set! condition-type:illegal-stream-element
	  (make-condition-type 'illegal-stream-element
	      condition-type:wrong-type-argument
	      '()
	    #f))
    (set! error:illegal-stream-element
	  (let ((signaller
		 (condition-signaller condition-type:illegal-stream-element
				      '(type datum operator)
				      standard-error-handler)))
	    (named-lambda (error:illegal-stream-element stream operator)
	      (signaller "stream" stream operator))))
    unspecific))