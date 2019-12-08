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

;;;; SRFI 133: Vector Library

;;; Loosely based on the SRFI's sample implementation.

(define-library (srfi 133)
  (import (scheme base)
	  (scheme cxr)
	  (only (srfi 8)
		receive)
	  (only (mit legacy runtime)
		error:bad-range-argument
		fix:+
		fix:-
		fix:<
		fix:<=
		fix:=
		fix:>
		fix:>=
		fix:end-index
		fix:min
		fix:quotient
		fix:start-index
		guarantee
		index-fixnum?
		unspecific))
  (export reverse-list->vector
	  reverse-vector->list
	  vector-any
	  vector-append-subvectors
	  vector-cumulate
	  vector-empty?
	  vector-every
	  vector-fold
	  vector-fold-right
	  vector-index
	  vector-index-right
	  vector-map!
	  vector-partition
	  vector-reverse!
	  vector-reverse-copy
	  vector-skip
	  vector-skip-right
	  vector-swap!
	  vector-unfold
	  vector-unfold!
	  vector-unfold-right
	  vector-unfold-right!
	  vector=
          vector-binary-search
          vector-concatenate
          vector-count
          vector-reverse-copy!)
  (begin

;;; Constructors

(define (vector-unfold proc n . initial-seeds)
  (let ((v (make-vector n)))
    (apply vector-unfold! proc v 0 n initial-seeds)
    v))

(define (vector-unfold-right proc n . initial-seeds)
  (let ((v (make-vector n)))
    (apply vector-unfold-right! proc v 0 n initial-seeds)
    v))

(define (vector-copy v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'vector-copy))
	 (start (fix:start-index start end 'vector-copy))
	 (n (fix:- end start))
	 (result (make-vector n)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)) result)
      (vector-set! result i (vector-ref v (fix:+ start i))))))

(define (vector-reverse-copy v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'vector-reverse-copy))
	 (start (fix:start-index start end 'vector-reverse-copy))
	 (n (fix:- end start))
	 (last (fix:- end 1))
	 (result (make-vector n)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)) result)
      (vector-set! result i (vector-ref v (fix:- last i))))))

(define (vector-append . vs)
  (vector-concatenate vs))

(define (vector-concatenate vs)
  (let ((result
	 (make-vector
	  (let loop ((vs vs) (n 0))
	    (if (pair? vs)
		(loop (cdr vs)
		      (fix:+ (vector-length (car vs)) n))
		n)))))
    (let loop ((vs vs) (index 0))
      (if (pair? vs)
	  (let* ((v (car vs))
		 (n (vector-length v)))
	    (do ((i 0 (fix:+ i 1))
		 (j index (fix:+ j 1)))
		((not (fix:< i n))
		 (loop (cdr vs) j))
	      (vector-set! result j (vector-ref v i))))
	  result))))

(define (vector-append-subvectors . args)

  (define (parse-specs args)
    (let ((caller 'vector-append-subvectors))
      (let loop ((args args) (specs '()))
	(if (pair? args)
	    (loop (cdddr args)
		  (cons (let* ((v (car args))
			       (end
				(fix:end-index (caddr args)
					       (vector-length v)
					       caller)))
			  (vector v
				  (fix:start-index (cadr args) end caller)
				  end))
			specs))
	    (reverse specs)))))

  (define (compute-length specs)
    (let loop ((specs specs) (n 0))
      (if (pair? specs)
	  (loop (cdr specs)
		(fix:+ n
		       (fix:- (vector-ref (car specs) 2)
			      (vector-ref (car specs) 1))))
	  n)))

  (define (do-copies result specs)
    (let loop ((specs specs) (at 0))
      (if (pair? specs)
	  (let ((v (vector-ref (car specs) 0))
		(start (vector-ref (car specs) 1))
		(end (vector-ref (car specs) 2)))
	    (do ((i start (fix:+ i 1))
		 (at at (fix:+ at 1)))
		((not (fix:< i end))
		 (loop (cdr specs) at))
	      (vector-set! result at (vector-ref v i)))))))

  (let* ((specs (parse-specs args))
	 (result (make-vector (compute-length specs))))
    (do-copies result specs)
    result))

;;; Predicates

(define (vector-empty? v)
  (fix:= 0 (vector-length v)))

(define (vector= elt= . vs)
  (or (not (pair? vs))
      (let* ((v (car vs))
	     (n (vector-length v)))
	;; Check lengths first since that's fast.
	(and (let check-lengths ((vs* (cdr vs)))
	       (or (not (pair? vs*))
		   (and (fix:= n (vector-length (car vs*)))
			(check-lengths (cdr vs*)))))
	     (let check-elts ((vs (cdr vs)))
	       (or (not (pair? vs))
		   (let ((v* (car vs)))
		     (let loop ((i 0))
		       (if (fix:< i n)
			   (and (elt= (vector-ref v i)
				      (vector-ref v* i))
				(loop (fix:+ i 1)))
			   (check-elts (cdr vs)))))))))))

;;; Iteration

(define (vector-fold kons knil v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(do ((i 0 (fix:+ i 1))
	     (knil* knil (kons knil* (vector-ref v i))))
	    ((not (fix:< i n)) knil*)))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(do ((i 0 (fix:+ i 1))
	     (knil* knil (apply kons knil* (vectors-ref vs i))))
	    ((not (fix:< i n)) knil*)))))

(define (vector-fold-right kons knil v . vs)
  (if (null? vs)
      (do ((i (fix:- (vector-length v) 1) (fix:- i 1))
	   (knil* knil (kons knil* (vector-ref v i))))
	  ((not (fix:>= i 0)) knil*))
      (let ((vs (cons v vs)))
	(do ((i (fix:- (vectors-length vs) 1) (fix:- i 1))
	     (knil* knil (apply kons knil* (vectors-ref vs i))))
	    ((not (fix:>= i 0)) knil*)))))

(define (vector-map proc v . vs)
  (if (null? vs)
      (let* ((n (vector-length v))
	     (target (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) target)
	  (vector-set! target i (proc (vector-ref v i)))))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs))
	     (target (make-vector n)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) target)
	  (vector-set! target i (apply proc (vectors-ref v i)))))))

(define (vector-map! proc v . vs)
  (if (null? vs)
      (let* ((n (vector-length v)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (vector-set! v i (proc (vector-ref v i)))))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (vector-set! v i (apply proc (vectors-ref vs i)))))))

(define (vector-for-each proc v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (proc (vector-ref v i))))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(do ((i 0 (fix:+ i 1)))
	    ((not (fix:< i n)) unspecific)
	  (apply proc (vectors-ref vs i))))))

(define (vector-count pred v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(do ((i 0 (fix:+ i 1))
	     (j 0 (if (pred (vector-ref v i)) (fix:+ j 1) j)))
	    ((not (fix:< i n)) j)))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(do ((i 0 (fix:+ i 1))
	     (j 0 (if (apply pred (vectors-ref vs i)) (fix:+ j 1) j)))
	    ((not (fix:< i n)) j)))))

(define (vector-cumulate proc knil v)
  (let* ((n (vector-length v))
         (result (make-vector n)))
    (let loop ((i 0) (left knil))
      (if (fix:< i n)
          (let ((left* (proc left (vector-ref v i))))
            (vector-set! result i left*)
            (loop (fix:+ i 1) left*))
          result))))

;;; Searching

(define (vector-index pred v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (if (pred (vector-ref v i))
		   i
		   (loop (fix:+ i 1))))))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (if (apply pred (vectors-ref vs i))
		   i
		   (loop (fix:+ i 1))))))))

(define (vector-index-right pred v . vs)
  (if (null? v)
      (let loop ((i (fix:- (vector-length v) 1)))
	(and (fix:>= i 0)
	     (if (pred (vector-ref v i))
		 i
		 (loop (fix:- i 1)))))
      (let ((vs (cons v vs)))
	(let loop ((i (fix:- (vectors-length vs) 1)))
	  (and (fix:>= i 0)
	       (if (apply pred (vectors-ref vs i))
		   i
		   (loop (fix:- i 1))))))))

(define (vector-skip pred v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (if (pred (vector-ref v i))
		   (loop (fix:+ i 1))
		   i))))
      (let* ((vs (cons v vs))
	     (n (vectors-length vs)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (if (apply pred (vectors-ref vs i))
		   (loop (fix:+ i 1))
		   i))))))

(define (vector-skip-right pred v . vs)
  (if (null? v)
      (let loop ((i (fix:- (vector-length v) 1)))
	(and (fix:>= i 0)
	     (if (pred (vector-ref v i))
		 (loop (fix:- i 1))
		 i)))
      (let ((vs (cons v vs)))
	(let loop ((i (fix:- (vectors-length vs) 1)))
	  (and (fix:>= i 0)
	       (if (apply pred (vectors-ref vs i))
		   (loop (fix:- i 1))
		   i))))))

(define (vector-binary-search v value cmp)
  (let loop ((start 0) (end (vector-length v)))
    (and (fix:< start end)
	 (let ((midpoint (fix:quotient (fix:+ start end) 2)))
	   (let ((c (cmp (vector-ref v midpoint) value)))
	     (cond ((zero? c) midpoint)
		   ((positive? c) (loop start midpoint))
		   (else (loop (fix:+ midpoint 1) end))))))))

(define (vector-any pred v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (or (pred (vector-ref v i))
		   (loop (fix:+ i 1))))))
      (let ((vs (cons v vs))
	    (n (vectors-length vs)))
	(let loop ((i 0))
	  (and (fix:< i n)
	       (or (apply pred (vectors-ref vs i))
		   (loop (fix:+ i 1))))))))

(define (vector-every pred v . vs)
  (if (null? vs)
      (let ((n (vector-length v)))
	(let loop ((i 0))
	  (if (fix:< i n)
	      (and (pred (vector-ref v i))
		   (loop (fix:+ i 1)))
	      #t)))
      (let ((vs (cons v vs))
	    (n (vectors-length vs)))
	(let loop ((i 0))
	  (if (fix:< i n)
	      (and (apply pred (vectors-ref vs i))
		   (loop (fix:+ i 1)))
	      #t)))))

(define (vector-partition pred v)
  (let* ((n (vector-length v))
         (result (make-vector n)))
    (let loop ((i 0) (yes 0) (no (fix:- n 1)))
      (if (fix:< i n)
          (let ((elt (vector-ref v i)))
            (if (pred elt)
		(begin
		  (vector-set! result yes elt)
		  (loop (fix:+ i 1) (fix:+ yes 1) no))
		(begin
		  (vector-set! result no elt)
		  (loop (fix:+ i 1) yes (fix:- no 1)))))
	  (begin
	    (%vector-reverse! result yes n)
	    (values result yes))))))

;;; Mutators

(define (vector-swap! v i1 i2)
  (let ((x (vector-ref v i1)))
    (vector-set! v i1 (vector-ref v i2))
    (vector-set! v i2 x)))

(define (vector-reverse! v #!optional start end)
  (let ((end (fix:end-index end (vector-length v) 'vector-reverse!)))
    (%vector-reverse! v
		      (fix:start-index start end 'vector-reverse!)
		      end)))

(define (%vector-reverse! v start end)
  (let loop ((i start) (j (fix:- end 1)))
    (if (fix:< i j)
	(let ((elt (vector-ref v i)))
	  (vector-set! v i (vector-ref v j))
	  (vector-set! v j elt)
	  (loop (fix:+ i 1) (fix:- j 1))))))

(define (vector-copy! to at from #!optional start end)
  (let* ((end (fix:end-index end (vector-length from) 'vector-copy!))
	 (start (fix:start-index start end 'vector-copy!)))
    (let ((tend
	   (to-end-index at
			 (fix:- end start)
			 (vector-length to)
			 'vector-copy!)))
      (cond ((or (not (eq? to from)) (fix:< at start))
	     (do ((i start (fix:+ i 1))
		  (j at (fix:+ j 1)))
		 ((not (fix:< i end)) unspecific)
	       (vector-set! to j (vector-ref from i))))
	    ((fix:> at start)
	     (do ((i (fix:- end 1) (fix:- i 1))
		  (j (fix:- tend 1) (fix:- j 1)))
		 ((not (fix:>= i start)) unspecific)
	       (vector-set! to j (vector-ref from i))))))))

(define (vector-reverse-copy! to at from #!optional start end)
  (let* ((end (fix:end-index end (vector-length from) 'vector-reverse-copy!))
	 (start (fix:start-index start end 'vector-reverse-copy!)))
    (let ((tend
	   (to-end-index at
			 (fix:- end start)
			 (vector-length to)
			 'vector-copy!)))

      (define (do-copy tend start end)
	(do ((i start (fix:+ i 1))
	     (j (fix:- tend 1) (fix:- j 1)))
	    ((not (fix:< i end)) unspecific)
	  (vector-set! to j (vector-ref from i))))

      (cond ((or (not (eq? to from)) (fix:<= end at) (fix:<= tend start))
	     (do-copy tend start end))
	    ((fix:< at start)
	     (do-copy at tend end)
	     (%vector-reverse! to start tend))
	    ((fix:> at start)
	     (do-copy start end tend)
	     (%vector-reverse! to at end))))))

(define (vector-unfold! proc v start end . initial-seeds)
  (let* ((end (fix:end-index end (vector-length v) 'vector-unfold!))
	 (start (fix:start-index start end 'vector-unfold!)))
    (cond ((null? initial-seeds)
	   (do ((i start (fix:+ i 1)))
	       ((not (fix:< i end)) unspecific)
	     (vector-set! v i (proc i))))
	  ((null? (cdr initial-seeds))
	   (let loop ((i start) (seed (car initial-seeds)))
	     (if (fix:< i end)
		 (receive (elt seed*) (proc i seed)
		   (vector-set! v i elt)
		   (loop (fix:+ i 1) seed*)))))
	  (else
	   (let loop ((i start) (seeds initial-seeds))
	     (if (fix:< i end)
		 (receive (elt . seeds*) (apply proc i seeds)
		   (vector-set! v i elt)
		   (loop (fix:+ i 1) seeds*))))))))

(define (vector-unfold-right! proc v start end . initial-seeds)
  (let* ((end (fix:end-index end (vector-length v) 'vector-unfold!))
	 (start (fix:start-index start end 'vector-unfold!)))
    (cond ((null? initial-seeds)
	   (do ((i (fix:- end 1) (fix:- i 1)))
	       ((not (fix:>= i start)) unspecific)
	     (vector-set! v i (proc i))))
	  ((null? (cdr initial-seeds))
	   (let loop ((i (fix:- end 1)) (seed (car initial-seeds)))
	     (if (fix:>= i start)
		 (receive (elt seed*) (proc i seed)
		   (vector-set! v i elt)
		   (loop (fix:- i 1) seed*)))))
	  (else
	   (let loop ((i (fix:- end 1)) (seeds initial-seeds))
	     (if (fix:>= i start)
		 (receive (elt . seeds*) (apply proc i seeds)
		   (vector-set! v i elt)
		   (loop (fix:- i 1) seeds*))))))))

;;; Conversion

(define (vector->list v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'vector->list))
	 (start (fix:start-index start end 'vector->list)))
    (do ((i (fix:- end 1) (fix:- i 1))
	 (result '() (cons (vector-ref v i) result)))
	((not (fix:>= i start)) result))))

(define (reverse-vector->list v #!optional start end)
  (let* ((end (fix:end-index end (vector-length v) 'reverse-vector->list))
	 (start (fix:start-index start end 'reverse-vector->list)))
    (do ((i start (fix:+ i 1))
	 (result '() (cons (vector-ref v i) result)))
	((not (fix:< i end)) result))))

(define (list->vector l)
  (let* ((n (length l))
	 (result (make-vector n)))
    (do ((i 0 (fix:+ i 1))
	 (l l (cdr l)))
	((not (fix:< i n)) result)
      (vector-set! result i (car l)))))

(define (reverse-list->vector l)
  (let* ((n (length l))
	 (result (make-vector n)))
    (do ((i (fix:- n 1) (fix:- i 1))
	 (l l (cdr l)))
	((not (fix:>= i 0)) result)
      (vector-set! result i (car l)))))

;;; Internal

(define (vectors-ref vs i)
  (let loop ((vs vs))
    (if (pair? vs)
	(cons (vector-ref (car vs) i)
	      (loop (cdr vs)))
	'())))

(define (vectors-length vs)
  (let loop ((vs (cdr vs)) (n (vector-length (car vs))))
    (if (pair? vs)
	(loop (cdr vs) (fix:min n (vector-length (car vs))))
	n)))

(define (to-end-index start n limit caller)
  (guarantee index-fixnum? start caller)
  (let ((end (fix:+ start n)))
    (if (not (fix:<= n limit))
	(error:bad-range-argument start caller))
    end))

;; end of library
))