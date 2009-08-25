#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

;;;; Assembler utilities
;;; package: (compiler assembler)

(declare (usual-integrations))

(define-integrable (make-queue)
  (cons '() '()))

(define-integrable (queue->list queue)
  (car queue))

(define (add-to-queue! queue entry)
  (let ((new (cons entry '())))
    (if (null? (cdr queue))
	(set-car! queue new)
	(set-cdr! (cdr queue) new))
    (set-cdr! queue new)))

(define-integrable (set-label-value! name low high)
  (symbol-table-define! *the-symbol-table*
			name
			(label->machine-interval low high)))

(define (clear-symbol-table!)
  (set! *the-symbol-table* (make-symbol-table))
  unspecific)

(define (initialize-symbol-table!)
  (symbol-table-define! *the-symbol-table* *start-label* 0))

(define (finish-symbol-table!)
  (call-with-values
      (lambda ()
	(interval-values (symbol-table-value *the-symbol-table* *end-label*)))
    (lambda (low high)
      (do ((objects (queue->list *objects*) (cdr objects))
	   (pcmin (->bitstring-pc low) (+ pcmin scheme-object-width))
	   (pcmax (->bitstring-pc high) (+ pcmax scheme-object-width)))
	  ((null? objects))
	(set-label-value! (cadar objects) pcmin pcmax))))
  (for-each (lambda (equate)
	      (symbol-table-define! *the-symbol-table*
				    (car equate)
				    (evaluate (cadr equate) #f)))
	    (queue->list *equates*)))

(define (variable-width-lengths v)
  (let ((sel (vector-ref v 3)))
    (if (null? sel)
	(error "Bad variable width directive:" v))
    (let ((l (selector/length (car sel))))
      (let loop ((selectors (cdr sel)) (min l) (max l))
	(if (null? selectors)
	    (values min max)
	    (let ((this (selector/length (car selectors))))
	      (cond ((< this min) (loop (cdr selectors) this max))
		    ((> this max) (loop (cdr selectors) min this))
		    (else         (loop (cdr selectors) min max)))))))))

(define-integrable (selector/handler sel)
  (vector-ref sel 0))

(define-integrable (selector/length sel)
  (vector-ref sel 1))

(define-integrable (selector/low sel)
  (vector-ref sel 2))

(define-integrable (selector/high sel)
  (vector-ref sel 3))

;;;; Expression Evaluation

(define (evaluate expression pc-value)
  (define (inner exp)
    (cond ((pair? exp)
	   ((find-operator (car exp))
	    (inner (cadr exp))
	    (inner (caddr exp))))
	  ((number? exp) exp)
	  ((not (symbol? exp))
	   (error "evaluate: bad expression" exp))
	  ((eq? exp '*PC*)
	   (if (not pc-value)
	       (error "evaluate: *PC* found with no PC defined"))
	   pc-value)
	  (else
	   (symbol-table-value *the-symbol-table* exp))))
  (inner expression))

(define (find-operator keyword)
  (let ((place (assq keyword operators)))
    (if (not place)
	(error "evaluate: unknown operator:" keyword))
    ((cdr place))))

(define operators
  `((+ . ,(lambda () interval:+))
    (- . ,(lambda () interval:-))
    (* . ,(lambda () interval:*))
    (/ . ,(lambda () interval:/))
    (QUOTIENT . ,(lambda () interval:quotient))
    (REMAINDER . ,(lambda () interval:remainder))))

(define-integrable (->machine-pc pc)
  (paranoid-quotient pc addressing-granularity))

(define-integrable (->bitstring-pc pc)
  (* pc addressing-granularity))

(define (paranoid-quotient dividend divisor)
  (let ((result (integer-divide dividend divisor)))
    (if (not (zero? (integer-divide-remainder result)))
	(error "paranoid-quotient: not a multiple" dividend divisor))
    (integer-divide-quotient result)))

(define (final-pad pcvalue)
  (paddify pcvalue 0 scheme-object-width))

(define (paddify pc-val remdr divsr)
  (let ((aremdr (remainder pc-val divsr)))
    (+ pc-val
       (if (<= aremdr remdr)
	   (- remdr aremdr)
	   (+ remdr (- divsr aremdr))))))

;;;; Interval Arithmetic

(define-structure (interval (constructor %make-interval))
  (offset #f read-only #t)
  (segset #f read-only #t))

(define-integrable (label->machine-interval low high)
  (make-interval 0
		 (list (make-segment (make-point (->machine-pc low)
						 (->machine-pc high))
				     1))))

(define (make-interval offset segset)
  (if (null? segset)
      offset
      (%make-interval offset segset)))

(define (interval-values interval)
  (if (interval? interval)
      (let loop
	  ((result-1 (interval-offset interval))
	   (result-2 (interval-offset interval))
	   (base-1 0)
	   (base-2 0)
	   (segset (interval-segset interval)))
	(if (null? segset)
	    (if (<= result-1 result-2)
		(values result-1 result-2)
		(values result-2 result-1))
	    (let ((position-1 (segment-min (car segset)))
		  (position-2 (segment-max (car segset)))
		  (k (segment-coeff (car segset))))
	      (loop (+ result-1 (* (- position-1 base-1) k))
		    (+ result-2 (* (- position-2 base-2) k))
		    position-1
		    position-2
		    (cdr segset)))))
      (values interval interval)))

(define (interval-final-value interval)
  (if (interval? interval)
      (let loop
	  ((result (interval-offset interval))
	   (base 0)
	   (segset (interval-segset interval)))
	(if (null? segset)
	    result
	    (let ((position (segment-min (car segset)))
		  (k (segment-coeff (car segset))))
	      (loop (+ result (* (- position base) k))
		    position
		    (cdr segset)))))
      interval))

(define (interval:+ a b)
  (if (interval? a)
      (if (interval? b)
	  (make-interval (+ (interval-offset a) (interval-offset b))
			 (segset:+ (interval-segset a) (interval-segset b)))
	  (make-interval (+ (interval-offset a) b) (interval-segset a)))
      (if (interval? b)
	  (make-interval (+ a (interval-offset b)) (interval-segset b))
	  (+ a b))))

(define (interval:- a b)
  (if (interval? a)
      (if (interval? b)
	  (make-interval (- (interval-offset a) (interval-offset b))
			 (segset:- (interval-segset a) (interval-segset b)))
	  (make-interval (- (interval-offset a) b) (interval-segset a)))
      (if (interval? b)
	  (make-interval (- a (interval-offset b))
			 (segset:negate (interval-segset b)))
	  (- a b))))

(define (interval:* a b)
  (if (interval? a)
      (if (interval? b)
	  (error "Can't multiply two intervals:" a b)
	  (make-interval (* (interval-offset a) b)
			 (segset:scale (interval-segset a) b)))
      (if (interval? b)
	  (make-interval (* (interval-offset b) a)
			 (segset:scale (interval-segset b) a))
	  (* a b))))

;;; Integer division on intervals is hard because the numerator of the
;;; division is a summation.  For exact division we can just check the
;;; sum of the result to make sure it's an integer.  QUOTIENT and
;;; REMAINDER can't be done at all unless we constrain the remainder
;;; of the high and low values to be the same.

(define (interval:/ a b)
  (if (interval? b)
      (error "Can't divide by an interval:" b))
  (if (interval? a)
      (let ((result
	     (make-interval (/ (interval-offset a) b)
			    (segset:scale (interval-segset a) (/ 1 b)))))
	(if (not
	     (call-with-values (lambda () (interval-values result))
	       (lambda (low high)
		 (and (integer? low)
		      (integer? high)))))
	    (error "Interval division not exact:" a b))
	result)
      (paranoid-quotient a b)))

(define (interval:quotient a b)
  (if (or (interval? a) (interval? b))
      (error "QUOTIENT doesn't do intervals:" a b))
  (quotient a b))

(define (interval:remainder a b)
  (if (or (interval? a) (interval? b))
      (error "REMAINDER doesn't do intervals:" a b))
  (remainder a b))

;;; A segment consists of an ending point and a coefficient.
;;; The ending point has a minimum and maximum non-negative integer value.
;;; The coefficient is an integer.
;;; min(s1)=min(s2) iff max(s1)=max(s2)
;;; min(s1)<min(s2) iff max(s1)<max(s2)

(define-integrable make-segment cons)
(define-integrable segment-point car)
(define-integrable segment-coeff cdr)

(define-integrable make-point cons)
(define-integrable point-min car)
(define-integrable point-max cdr)

(define-integrable (segment-min segment)
  (point-min (segment-point segment)))

(define-integrable (segment-max segment)
  (point-max (segment-point segment)))

(define-integrable (segment:< s1 s2)
  (< (segment-min s1) (segment-min s2)))

(define-integrable (segment:= s1 s2)
  (= (segment-min s1) (segment-min s2)))

;;; A segset is a list of segments.
;;; The segments are sorted in order from least to greatest.
;;; There is an implicit starting point of zero.

(define (segset:+ a b)
  (cond ((null? a) b)
	((null? b) a)
	((segment:< (car a) (car b))
	 (cons-segset (segment-point (car a))
		      (+ (segment-coeff (car a)) (segment-coeff (car b)))
		      (segset:+ (cdr a) b)))
	(else
	 (cons-segset (segment-point (car b))
		      (+ (segment-coeff (car a)) (segment-coeff (car b)))
		      (segset:+ (if (segment:= (car a) (car b)) (cdr a) a)
				(cdr b))))))

(define (segset:- a b)
  (cond ((null? a) (segset:negate b))
	((null? b) a)
	((segment:< (car a) (car b))
	 (cons-segset (segment-point (car a))
		      (- (segment-coeff (car a)) (segment-coeff (car b)))
		      (segset:- (cdr a) b)))
	(else
	 (cons-segset (segment-point (car b))
		      (- (segment-coeff (car a)) (segment-coeff (car b)))
		      (segset:- (if (segment:= (car a) (car b)) (cdr a) a)
				(cdr b))))))

(define (segset:negate b)
  (segset:scale b -1))

(define (segset:scale b c)
  (if (null? b)
      b
      (cons (make-segment (segment-point (car b))
			  (* (segment-coeff (car b)) c))
	    (segset:scale (cdr b) c))))

(define (cons-segset point coeff segset)
  (if (= coeff (if (null? segset) 0 (segment-coeff (car segset))))
      segset
      (cons (make-segment point coeff) segset)))