#| -*-Scheme-*-

$Id: bitutl.scm,v 1.7 1993/12/08 17:43:16 gjr Exp $

Copyright (c) 1987-1993 Massachusetts Institute of Technology

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
;;; package: (compiler assembler)

(declare (usual-integrations))

;;;; Extra symbol table operations

(define (clear-symbol-table!)
  (set! *the-symbol-table* (make-symbol-table))
  unspecific)

(define (initialize-symbol-table!)
  (symbol-table-define! *the-symbol-table* *start-label* 0))

(define (finish-symbol-table!)
  (define (process-objects objs pcmin pcmax)
    (if (null? objs)
	'DONE
	(let ((object (car objs)))
	  (symbol-table-define! *the-symbol-table*
				(cadr object) ; label
				(make-machine-interval pcmin pcmax))
	  (process-objects (cdr objs)
			   (+ pcmin scheme-object-width)
			   (+ pcmax scheme-object-width)))))

  ;; Handle scheme objects
  (let ((val (symbol-table-value *the-symbol-table* *end-label*)))
    (process-objects (queue->list *objects*)
		     (* addressing-granularity (interval-low val))
		     (* addressing-granularity (interval-high val))))

  ;; Handle equates
  (for-each (lambda (equate)
	      (symbol-table-define! *the-symbol-table*
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
	  ((eq? exp '*PC*)
	   (if (not pc-value)
	       (error "evaluate: *PC* found with no PC defined"))
	   pc-value)
	  (else
	   (symbol-table-value *the-symbol-table* exp))))
  (inner expression))

(define-integrable (->machine-pc pc)
  (paranoid-quotient pc addressing-granularity))

(define-integrable (->bitstring-pc pc)
  (* pc addressing-granularity))

(define (paddify pc-val remdr divsr)
  (let ((aremdr (remainder pc-val divsr)))
    (+ pc-val
       (if (<= aremdr remdr)
	   (- remdr aremdr)
	   (+ remdr (- divsr aremdr))))))

;; Machine intervals are always in addressing units.

(define-integrable (make-machine-interval low high)
  (make-interval (->machine-pc low)
		 (->machine-pc high)))

(define (->interval value)
  (if (interval? value)
      value
      (make-interval value value)))

(define-integrable (make-interval low high)
  (vector 'INTERVAL low high))

(define-integrable (interval? obj)
  (and (vector? obj)
       (eq? (vector-ref obj 0) 'INTERVAL)))

(define-integrable (interval-low obj)
  (vector-ref obj 1))

(define-integrable (interval-high obj)
  (vector-ref obj 2))

(define (paranoid-quotient dividend divisor)
  (let ((result (integer-divide dividend divisor)))
    (if (zero? (integer-divide-remainder result))
	(integer-divide-quotient result)
	(error "paranoid-quotient: not a multiple" dividend divisor))))

(define (final-pad pcvalue)
  (paddify pcvalue 0 scheme-object-width))

;;;; Operators

(define operators '())

(define (define-operator! keyword procedure)
  (set! operators `((,keyword . ,procedure) ,@operators)))

(define (find-operator keyword)
  (let ((place (assq keyword operators)))
    (if (null? place)
	(error "evaluate: unknown operator" keyword)
	(cdr place))))

;; Either argument can be an interval

(define-operator! '+
  (lambda (op1 op2)
    (cond ((not (interval? op2))
	   (if (not (interval? op1))
	       (+ op1 op2)
	       (make-interval (+ (interval-low op1) op2)
			      (+ (interval-high op1) op2))))
	  ((not (interval? op1))
	   (make-interval (+ op1 (interval-low op2))
			  (+ op1 (interval-high op2))))
	  (else
	   (make-interval (+ (interval-low op1) (interval-low op2))
			  (+ (interval-high op1) (interval-high op2)))))))

(define-operator! '-
  (lambda (op1 op2)
    (cond ((not (interval? op2))
	   (if (not (interval? op1))
	       (- op1 op2)
	       (make-interval (- (interval-low op1) op2)
			      (- (interval-high op1) op2))))
	  ((not (interval? op1))
	   (make-interval (- op1 (interval-high op2))
			  (- op1 (interval-low op2))))
	  (else
	   (make-interval (- (interval-low op1) (interval-high op2))
			  (- (interval-high op1) (interval-low op2)))))))

;; Only one argument can be an interval, both assumed non-negative.

(define-operator! '*
  (lambda (op1 op2)
    (cond ((not (interval? op2))
	   (if (not (interval? op1))
	       (* op1 op2)
	       (make-interval (* (interval-low op1) op2)
			      (* (interval-high op1) op2))))
	  ((not (interval? op1))
	   (make-interval (* op1 (interval-low op2))
			  (* op1 (interval-high op2))))
	  (else
	   (error "evaluate: Both arguments are intervals" '* op1 op2)))))

;; Only the first argument can be an interval

(define ((asymmetric name op) op1 op2)
  (cond ((interval? op2)
	 (error "evaluate: Second operand is an interval" name op1 op2))
	((not (interval? op1))
	 (op op1 op2))
	(else
	 (make-interval (op (interval-low op1) op2)
			(op (interval-high op1) op2)))))

(define-operator! '/ (asymmetric '/ paranoid-quotient))
(define-operator! 'QUOTIENT (asymmetric 'QUOTIENT quotient))

(define-operator! 'REMAINDER
  (lambda (op1 op2)
    (cond ((interval? op2)
	   (error "evaluate: Second operand is an interval"
		  'REMAINDER op1 op2))
	  ((not (interval? op1))
	   (remainder op1 op2))
	  (else
	   (let ((rlow (remainder (interval-low op1) op2))
		 (rhigh (remainder (interval-high op1) op2)))
	     (if (> rlow rhigh)
		 (make-interval rhigh rlow)
		 (make-interval rlow rhigh)))))))

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