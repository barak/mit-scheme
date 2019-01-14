#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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

(define (stream-pair? stream)
  (and (pair? stream)
       (promise? (cdr stream))))

(define-guarantee stream-pair "stream pair")

(define (stream-car stream)
  (guarantee stream-pair? stream 'stream-car)
  (car stream))

(define (stream-cdr stream)
  (guarantee stream-pair? stream 'stream-cdr)
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
  (let loop ((stream stream) (n 0))
    (if (stream-pair? stream)
	(loop (force (cdr stream)) (+ n 1))
	(begin
	  (if (not (null? stream))
	      (error:illegal-stream-element stream 'stream-length 0))
	  n))))

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
	  (cons (car stream)
		(loop (force (cdr stream)) (- index 1))))
	'())))

(define (stream-tail stream index)
  (guarantee exact-nonnegative-integer? index 'stream-tail)
  (let loop ((stream stream) (index index))
    (if (> index 0)
	(begin
	  (if (not (stream-pair? stream))
	      (error:bad-range-argument index 'stream-tail))
	  (loop (force (cdr stream)) (- index 1)))
	stream)))

(define (stream-last stream)
  (stream-car (stream-last-pair stream)))

(define (stream-last-pair stream)
  (if (not (stream-pair? stream))
      (if (null? stream)
	  (error:bad-range-argument stream 'stream-last-pair)
	  (error:illegal-stream-element stream 'stream-last-pair 0)))
  (let loop ((stream stream))
    (let ((next (force (cdr stream))))
      (if (stream-pair? next)
	  (loop next)
	  (begin
	    (if (not (null? next))
		(error:illegal-stream-element stream 'stream-last-pair 0))
	    stream)))))

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
	   (if (stream-pair? stream)
	       (cons-stream (procedure (car stream))
			    (loop (force (cdr stream))))
	       (begin
		 (if (not (null? stream))
		     (error:illegal-stream-element stream 'stream-map 1))
		 '()))))
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
	(cond ((stream-pair? stream)
	       (procedure (car stream))
	       (loop (force (cdr stream))))
	      ((not (null? stream))
	       (error:illegal-stream-element stream 'stream-for-each 1))))))

(define (split-streams streams operator)
  (let ((cars (list 'cars))
	(cdrs (list 'cdrs)))
    (let loop ((streams streams) (cars-tail cars) (cdrs-tail cdrs) (n 0))
      (if (pair? streams)
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
		  (values '() '()))))
	  (values (cdr cars) (cdr cdrs))))))

(define (stream-append-map procedure stream . streams)
  (let ((sappend
	 (lambda (s1 s2)
	   (let loop ((s s1))
	     (if (stream-pair? s)
		 (cons-stream (car s) (loop (force (cdr s))))
		 (begin
		   (if (not (null? s))
		       (error:illegal-stream-element s1 'stream-append 0))
		   (force s2)))))))
    (if (pair? streams)
	(let loop ((streams (cons stream streams)))
	  (receive (cars cdrs) (split-streams streams 'stream-append-map)
	    (if (pair? cars)
		(sappend (apply procedure cars)
			 (delay (loop (map force cdrs))))
		'())))
	(let loop ((stream stream))
	  (if (stream-pair? stream)
	      (sappend (procedure (car stream))
		       (delay (loop (force (cdr stream)))))
	      (begin
		(if (not (null? stream))
		    (error:illegal-stream-element stream 'stream-append-map 1))
		'()))))))

(define (stream-append . streams)
  (if (pair? streams)
      (let outer-loop ((streams streams) (n 0))
	(if (pair? (cdr streams))
	    (let inner-loop ((stream (car streams)))
	      (if (stream-pair? stream)
		  (cons-stream (car stream)
			       (inner-loop (force (cdr stream))))
		  (begin
		    (if (not (null? stream))
			(error:illegal-stream-element stream 'stream-append n))
		    (outer-loop (cdr streams) (fix:+ n 1)))))
	    (car streams)))
      '()))

(define (stream-accumulate procedure initial stream)
  (if (stream-pair? stream)
      (procedure (car stream)
		 (stream-accumulate procedure initial (force (cdr stream))))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'stream-accumulate 2))
	initial)))

(define (stream-filter predicate stream)
  (if (stream-pair? stream)
      (if (predicate (car stream))
	  (cons-stream (car stream)
		       (stream-filter predicate (force (cdr stream))))
	  (stream-filter predicate (force (cdr stream))))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'stream-filter 1))
	'())))

(define (stream-truncate stream predicate)
  (if (stream-pair? stream)
      (if (predicate (head stream))
	  the-empty-stream
	  (cons-stream (head stream)
		       (stream-truncate (tail stream) predicate)))
      (begin
	(if (not (null? stream))
	    (error:illegal-stream-element stream 'stream-truncate 1))
	'())))

(define (stream-write stream #!optional port)
  (let ((port
	 (if (default-object? port)
	     (current-output-port)
	     (guarantee textual-output-port? port 'stream-write))))
    (if (stream-pair? stream)
	(begin
	  (write-char #\{ port)
	  (write (car stream) port)
	  (stream-for-each (lambda (object)
			     (write-char #\space port)
			     (write object port))
			   (force (cdr stream)))
	  (write-char #\} port))
	(begin
	  (if (not (null? stream))
	      (error:illegal-stream-element stream 'stream-write 0))
	  (write-string "{}" port)))))

(define (list->stream list)
  (if (pair? list)
      (cons-stream (car list) (list->stream (cdr list)))
      (begin
	(if (not (null? list))
	    (error:not-a list? list 'list->stream))
	'())))

(define (stream->list stream)
  (let loop ((s stream)
	     (elements '()))
    (cond ((stream-pair? s) (loop (tail s) (cons (head s) elements)))
	  ((null? s) (reverse elements))
	  (else (error:illegal-stream-element s 'stream->list 0)))))

(define prime-numbers-stream)

(define (make-prime-numbers-stream)
  (cons-stream
   2
   (letrec
       ((primes (cons-stream 3 (fixnum-filter 5)))
	(fixnum-filter
	 (let ((limit (fix:- (fix:largest-value) 2)))
	   (lambda (n)
	     (if (fix:<= n limit)
		 (let loop ((ps primes))
		   (cond ((fix:< n (fix:* (car ps) (car ps)))
			  (cons-stream n (fixnum-filter (fix:+ n 2))))
			 ((fix:= 0 (fix:remainder n (car ps)))
			  (fixnum-filter (fix:+ n 2)))
			 (else
			  (loop (force (cdr ps))))))
		 (generic-filter n)))))
	(generic-filter
	 (lambda (n)
	   (let loop ((ps primes))
	     (cond ((< n (square (car ps)))
		    (cons-stream n (generic-filter (+ n 2))))
		   ((= 0 (remainder n (car ps)))
		    (generic-filter (+ n 2)))
		   (else
		    (loop (force (cdr ps)))))))))
     primes)))

(define (initialize-package!)
  (let ((reset-primes!
	 (lambda ()
	   (set! prime-numbers-stream (make-prime-numbers-stream))
	   unspecific)))
    (reset-primes!)
    (add-secondary-gc-daemon! reset-primes!)))

(define condition-type:illegal-stream-element)
(define error:illegal-stream-element)

(define (initialize-conditions!)
  (set! condition-type:illegal-stream-element
	(make-condition-type 'illegal-stream-element
	    condition-type:wrong-type-argument
	    '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'datum) port)
	    (write-string ", passed as the " port)
	    (write-string (ordinal-number-string
			   (+ (access-condition condition 'operand) 1))
			  port)
	    (write-string " argument to " port)
	    (write-operator (access-condition condition 'operator) port)
	    (write-string ", is not a stream." port))))
  (set! error:illegal-stream-element
	(let ((signaller
	       (condition-signaller condition-type:illegal-stream-element
				    '(type datum operator operand)
				    standard-error-handler)))
	  (named-lambda (error:illegal-stream-element stream operator operand)
	    (signaller "stream" stream operator operand))))
  unspecific)