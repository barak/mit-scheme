#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/compiler/back/bitutl.scm,v 1.4 1990/01/18 22:41:51 cph Rel $

Copyright (c) 1987, 1990 Massachusetts Institute of Technology

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

;;;; Assembler utilities

(declare (usual-integrations))

;;;; Extra symbol table operations

(define (clear-symbol-table!)
  (set! *the-symbol-table* (make-symbol-table)))

(define (initialize-symbol-table!)
  (symbol-table-define! *the-symbol-table* *start-label* 0))

(define (finish-symbol-table!)
  (define (process-objects obj pcmin pcmax)
    (if (null? obj)
	'DONE
	(begin
	  (symbol-table-define! *the-symbol-table*
				(caar obj)
				(make-machine-interval pcmin pcmax))
	  (process-objects (cdr obj)
			   (+ pcmin scheme-object-width)
			   (+ pcmax scheme-object-width)))))

  ;; Handle scheme objects
  (let ((val (symbol-table-value *the-symbol-table* *end-label*)))
    (process-objects (queue->list *objects*)
		     (* addressing-granularity (interval-low val))
		     (* addressing-granularity (interval-high val))))

  ;; Handle equates
  (for-each (lambda (equate)
	      (symbol-table-define!
	       *the-symbol-table*
	       (car equate)
	       (evaluate (cadr equate) false)))
	    (queue->list *equates*)))

;;;; Expression evaluation and intervals

(define (evaluate expression pc-value)
  (define (inner exp)
    (cond ((pair? exp)
	   ((find-operator (car exp))
	    (inner (cadr exp))
	    (inner (caddr exp))))
	  ((number? exp) exp)
	  ((not (symbol? exp))
	   (error "evaluate: bad expression" exp))
	  ((eq? exp '*PC*) pc-value)
	  (else
	   (symbol-table-value *the-symbol-table* exp))))
  (inner expression))

(declare (integrate-operator ->machine-pc make-machine-interval
			     make-interval interval?
			     interval-low interval-high))

(define (->machine-pc pc)
  (declare (integrate pc))
  (paranoid-quotient pc addressing-granularity))

;; Machine intervals are always in addressing units.

(define (make-machine-interval low high)
  (declare (integrate low high))
  (make-interval (->machine-pc low)
		 (->machine-pc high)))

(define (->interval value)
  (if (interval? value)
      value
      (make-interval value value)))

(define (make-interval low high)
  (declare (integrate low high))
  (vector 'INTERVAL low high))

(define (interval? obj)
  (declare (integrate obj))
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'INTERVAL)))

(define (interval-low obj)
  (declare (integrate obj))
  (vector-ref obj 1))

(define (interval-high obj)
  (declare (integrate obj))
  (vector-ref obj 2))

(define (paranoid-quotient dividend divisor)
  (let ((result (integer-divide dividend divisor)))
    (if (zero? (integer-divide-remainder result))
	(integer-divide-quotient result)
	(error "paranoid-quotient: not a multiple" dividend divisor))))

(define (pad pcvalue)
  (let ((r (remainder pcvalue scheme-object-width)))
    (if (zero? r)
	pcvalue
	(+ pcvalue (- scheme-object-width r)))))

;;;; Operators

(define operators '())

(define (define-operator! keyword procedure)
  (set! operators `((,keyword . ,procedure) ,@operators)))

(define (find-operator keyword)
  (let ((place (assq keyword operators)))
    (if (null? place)
	(error "evaluate: unknown operator" keyword)
	(cdr place))))

(define ((symmetric scalar) op1 op2)
  (if (interval? op1)
      (if (interval? op2)
	  (make-interval (scalar (interval-low op1) (interval-low op2))
			 (scalar (interval-high op1) (interval-high op2)))
	  (make-interval (scalar (interval-low op1) op2)
			 (scalar (interval-high op1) op2)))
      (if (interval? op2)
	  (make-interval (scalar op1 (interval-low op2))
			 (scalar op1 (interval-high op2)))
	  (scalar op1 op2))))

(define-operator! '+ (symmetric +))
(define-operator! '- (symmetric -))

;; Only one argument can be an interval.

(define-operator! '*
  (lambda (op1 op2)
    (cond ((interval? op1)
	   (make-interval (* (interval-low op1) op2)
			  (* (interval-high op1) op2)))
	  ((interval? op2)
	   (make-interval (* op1 (interval-low op2))
			  (* op1 (interval-high op2))))
	  (else (* op1 op2)))))

;; Only the first argument can be an interval

(define-operator! '/
  (lambda (op1 op2)
    (if (interval? op1)
	(make-interval (paranoid-quotient (interval-low op1) op2)
		       (paranoid-quotient (interval-high op1) op2))
	(paranoid-quotient op1 op2))))

;;;; Variable width expression utilities

(define (variable-width-lengths v receiver)
  (define (loop selectors min max)
    (cond ((null? selectors)
	   (receiver min max))
	  ((< (selector/length (car selectors)) min)
	   (loop (cdr selectors)
		 (selector/length (car selectors))
		 max))
	  ((> (selector/length (car selectors)) max)
	   (loop (cdr selectors)
		 min
		 (selector/length (car selectors))))
	  (else
	   (loop (cdr selectors) min max))))
  (let ((sel (vector-ref v 3)))
    (if (null? sel)
	(error "variable-width-lengths: Bad variable width directive" v)
	(loop (cdr sel)
	      (selector/length (car sel))
	      (selector/length (car sel))))))

(define (selector/fits? val sel)
  (let ((low (selector/low sel))
	(high (selector/high sel)))
    (and (or (false? low) (<= low val))
	 (or (false? high) (<= val high)))))

(declare (integrate-operator selector/high selector/low
			     selector/handler selector/length))

(define (selector/high sel)
  (declare (integrate sel))
  (vector-ref sel 3))

(define (selector/low sel)
  (declare (integrate sel))
  (vector-ref sel 2))

(define (selector/length sel)
  (declare (integrate sel))
  (vector-ref sel 1))

(define (selector/handler sel)
  (declare (integrate sel))
  (vector-ref sel 0))

;;;; Random utilities

;;; Queues

(declare (integrate-operator make-queue queue->list))

(define (make-queue)
  (cons '() '()))

(define (queue->list queue)
  (declare (integrate queue))
  (car queue))

(define (add-to-queue! queue entry)
  (let ((new (cons entry '())))
    (if (null? (cdr queue))
	(set-car! queue new)
	(set-cdr! (cdr queue) new))
    (set-cdr! queue new)))