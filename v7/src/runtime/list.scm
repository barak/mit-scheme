#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/list.scm,v 14.1 1988/06/13 11:47:11 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

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

;;;; List Operations
;;; package: (runtime list)

(declare (usual-integrations))

(define-primitives
  cons pair? null? length car cdr set-car! set-cdr! general-car-cdr)

(define (list . items)
  items)

(define (cons* first-element . rest-elements)
  (let loop ((this-element first-element) (rest-elements rest-elements))
    (if (null? rest-elements)
	this-element
	(cons this-element
	      (loop (car rest-elements)
		    (cdr rest-elements))))))

(define (make-list length #!optional value)
  (if (not (and (integer? length) (not (negative? length))))
      (error "MAKE-LIST: length must be nonnegative integer" length))
  (let ((value (if (default-object? value) '() value)))
    (let loop ((n length) (result '()))
      (if (zero? n)
	  result
	  (loop (-1+ n) (cons value result))))))

(define (circular-list . items)
  (if (not (null? items))
      (let loop ((l items))
	(if (null? (cdr l))
	    (set-cdr! l items)
	    (loop (cdr l)))))
  items)

(define (make-circular-list length #!optional value)
  (if (not (and (integer? length) (not (negative? length))))
      (error "MAKE-CIRCULAR-LIST: length must be nonnegative integer" length))
  (if (positive? length)
      (let ((value (if (default-object? value) '() value)))
	(let ((last (cons value '())))
	  (let loop ((n (-1+ length)) (result last))
	    (if (zero? n)
		(begin
		  (set-cdr! last result)
		  result)
		(loop (-1+ n) (cons value result))))))
      '()))

(define (list-ref list index)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error "LIST-REF: index too large" index))
    (car tail)))

(define (list-tail list index)
  (if (not (and (integer? index) (not (negative? index))))
      (error "LIST-TAIL: index must be nonnegative integer" index))
  (let loop ((list list) (index index))
    (if (zero? index)
	list
	(begin (if (not (pair? list))
		   (error "LIST-TAIL: index too large" index))
	       (loop (cdr list) (-1+ index))))))

(define (list-head list index)
  (if (not (and (integer? index) (not (negative? index))))
      (error "LIST-HEAD: index must be nonnegative integer" index))
  (let loop ((list list) (index index))
    (if (zero? index)
	'()
	(begin
	  (if (not (pair? list))
	      (error "LIST-HEAD: list has too few elements" list index))
	  (cons (car list) (loop (cdr list) (-1+ index)))))))

(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

(define (list? object)
  (let loop ((object object))
    (if (null? object)
	true
	(and (pair? object)
	     (loop (cdr object))))))

(define (alist? object)
  (if (null? object)
      true
      (and (pair? object)
	   (pair? (car object))
	   (alist? (cdr object)))))

(define (list-copy items)
  (let loop ((items items))
    (if (pair? items)
	(cons (car items) (loop (cdr items)))
	(begin (if (not (null? items))
		   (error "LIST-COPY: argument not proper list" items))
	       '()))))

(define (alist-copy alist)
  (if (pair? alist)
      (begin (if (not (pair? (car alist)))
		 (error "ALIST-COPY: illegal alist element" (car alist)))
	     (cons (cons (caar alist) (cdar alist)) (alist-copy (cdr alist))))
      (begin (if (not (null? alist))
		 (error "ALIST-COPY: illegal alist" alist))
	     '())))

(define (tree-copy tree)
  (let loop ((tree tree))
    (if (pair? tree)
	(cons (loop (car tree)) (loop (cdr tree)))
	tree)))

;;;; Weak Pairs

(define-integrable (weak-cons car cdr)
  (system-pair-cons (ucode-type weak-cons) (or car weak-pair/false) cdr))

(define-integrable (weak-pair? object)
  (object-type? (ucode-type weak-cons) object))

(define-integrable (weak-pair/car? weak-pair)
  (system-pair-car weak-pair))

(define (weak-car weak-pair)
  (let ((car (system-pair-car weak-pair)))
    (and (not (eq? car weak-pair/false))
	 car)))

(define-integrable (weak-set-car! weak-pair object)
  (system-pair-set-car! weak-pair (or object weak-pair/false)))

(define-integrable (weak-cdr weak-pair)
  (system-pair-cdr weak-pair))

(define-integrable (weak-set-cdr! weak-pair object)
  (system-pair-set-cdr! weak-pair object))

(define weak-pair/false
  "weak-pair/false")

;;;; Standard Selectors

(define-integrable (caar x) (car (car x)))
(define-integrable (cadr x) (car (cdr x)))
(define-integrable (cdar x) (cdr (car x)))
(define-integrable (cddr x) (cdr (cdr x)))

(define-integrable (caaar x) (car (car (car x))))
(define-integrable (caadr x) (car (car (cdr x))))
(define-integrable (cadar x) (car (cdr (car x))))
(define-integrable (caddr x) (car (cdr (cdr x))))

(define-integrable (cdaar x) (cdr (car (car x))))
(define-integrable (cdadr x) (cdr (car (cdr x))))
(define-integrable (cddar x) (cdr (cdr (car x))))
(define-integrable (cdddr x) (cdr (cdr (cdr x))))

(define-integrable (caaaar x) (car (car (car (car x)))))
(define-integrable (caaadr x) (car (car (car (cdr x)))))
(define-integrable (caadar x) (car (car (cdr (car x)))))
(define-integrable (caaddr x) (car (car (cdr (cdr x)))))

(define-integrable (cadaar x) (car (cdr (car (car x)))))
(define-integrable (cadadr x) (car (cdr (car (cdr x)))))
(define-integrable (caddar x) (car (cdr (cdr (car x)))))
(define-integrable (cadddr x) (car (cdr (cdr (cdr x)))))

(define-integrable (cdaaar x) (cdr (car (car (car x)))))
(define-integrable (cdaadr x) (cdr (car (car (cdr x)))))
(define-integrable (cdadar x) (cdr (car (cdr (car x)))))
(define-integrable (cdaddr x) (cdr (car (cdr (cdr x)))))

(define-integrable (cddaar x) (cdr (cdr (car (car x)))))
(define-integrable (cddadr x) (cdr (cdr (car (cdr x)))))
(define-integrable (cdddar x) (cdr (cdr (cdr (car x)))))
(define-integrable (cddddr x) (cdr (cdr (cdr (cdr x)))))

(define-integrable (first x) (car x))
(define-integrable (second x) (car (cdr x)))
(define-integrable (third x) (car (cdr (cdr x))))
(define-integrable (fourth x) (car (cdr (cdr (cdr x)))))
(define-integrable (fifth x) (car (cdr (cdr (cdr (cdr x))))))
(define-integrable (sixth x) (car (cdr (cdr (cdr (cdr (cdr x)))))))
(define-integrable (seventh x) (car (cdr (cdr (cdr (cdr (cdr (cdr x))))))))

(define-integrable (eighth x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))

(define-integrable (ninth x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x))))))))))

(define-integrable (tenth x)
  (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr x)))))))))))

;;;; Sequence Operations

(define (append . lists)
  (if (null? lists)
      '()
      (let outer ((current (car lists)) (remaining (cdr lists)))
	(if (null? remaining)
	    current
	    (let inner ((list current))
	      (if (pair? list)
		  (cons (car list) (inner (cdr list)))
		  (begin (if (not (null? list))
			     (error "APPEND: Argument not a list" current))
			 (outer (car remaining) (cdr remaining)))))))))

(define (append! . lists)
  (if (null? lists)
      '()
      (let loop ((head (car lists)) (tail (cdr lists)))
	(cond ((null? tail)
	       head)
	      ((pair? head)
	       (set-cdr! (last-pair head) (loop (car tail) (cdr tail)))
	       head)
	      (else
	       (if (not (null? head))
		   (error "APPEND!: Argument not a list" head))
	       (loop (car tail) (cdr tail)))))))

(define (reverse l)
  (let loop ((rest l) (so-far '()))
    (if (pair? rest)
	(loop (cdr rest) (cons (car rest) so-far))
	(begin (if (not (null? rest))
		   (error "REVERSE: Argument not a list" l))
	       so-far))))

(define (reverse! l)
  (let loop ((current l) (new-cdr '()))
    (if (pair? current)
	(loop (set-cdr! current new-cdr) current)
	(begin (if (not (null? current))
		   (error "REVERSE!: Argument not a list" l))
	       new-cdr))))

;;;; Mapping Procedures

(define (map f . lists)
  ;; Compiler doesn't, but ought to, make this very fast.
  (apply map* '() f lists))

(define (map* initial-value f . lists)
  (if (null? lists)
      (error "MAP*: Too few arguments" f))
  (if (null? (cdr lists))
      (let 1-loop ((list (car lists)))
	(if (pair? list)
	    (cons (f (car list))
		  (1-loop (cdr list)))
	    (begin
	      (if (not (null? list))
		  (error "MAP*: Argument not a list" list))
	      initial-value)))
      (let n-loop ((lists lists))
	(let parse-cars
	    ((lists lists)
	     (receiver
	      (lambda (cars cdrs)
		(cons (apply f cars)
		      (n-loop cdrs)))))
	  (cond ((null? lists)
		 (receiver '() '()))
		((pair? (car lists))
		 (parse-cars (cdr lists)
			     (lambda (cars cdrs)
			       (receiver (cons (car (car lists)) cars)
					 (cons (cdr (car lists)) cdrs)))))
		(else
		 (if (not (null? (car lists)))
		     (error "MAP*: Argument not a list" (car lists)))
		 initial-value))))))

(define (reduce f initial list)
  (let loop ((value initial) (l list))
    (cond ((pair? l) (loop (f value (car l)) (cdr l)))
	  ((null? l) value)
	  (else (error "REDUCE: Argument not a list" list)))))

(define (for-each f . lists)
  (if (null? lists)
      (error "FOR-EACH: Too few arguments" f))
  (if (null? (cdr lists))
      (let 1-loop ((list (car lists)))
	(cond ((pair? list)
	       (f (car list))
	       (1-loop (cdr list)))
	      ((not (null? list))
	       (error "FOR-EACH: Argument not a list" list))))
      (let n-loop ((lists lists))
	(let parse-cars
	    ((lists lists)
	     (receiver
	      (lambda (cars cdrs)
		(apply f cars)
		(n-loop cdrs))))
	  (cond ((null? lists)
		 (receiver '() '()))
		((pair? (car lists))
		 (parse-cars (cdr lists)
			     (lambda (cars cdrs)
			       (receiver (cons (car (car lists)) cars)
					 (cons (cdr (car lists)) cdrs)))))
		((not (null? (car lists)))
		 (error "FOR-EACH: Argument not a list" (car lists)))))))
  *the-non-printing-object*)

(define (mapcan f . lists)
  ;; Compiler doesn't, but ought to, make this very fast.
  (apply mapcan* '() f lists))

(define (mapcan* initial-value f . lists)
  (if (null? lists)
      (error "MAPCAN*: Too few arguments" f))
  (let loop ((lists lists))
    (let scan
	((lists lists)
	 (c (lambda (cars cdrs)
	      (append! (apply f cars) (loop cdrs)))))
      (cond ((null? lists) (c '() '()))
	    ((null? (car lists)) initial-value)
	    (else
	     (scan (cdr lists)
		   (lambda (cars cdrs)
		     (c (cons (car (car lists)) cars)
			(cons (cdr (car lists)) cdrs)))))))))

;;;; Generalized List Operations

(define (list-transform-positive items predicate)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (cons (car items) (loop (cdr items)))
	    (loop (cdr items)))
	'())))

(define (list-transform-negative items predicate)
  (let loop ((items items))
    (if (pair? items)
	(if (predicate (car items))
	    (loop (cdr items))
	    (cons (car items) (loop (cdr items))))
	'())))

(define (list-search-positive items predicate)
  (let loop ((items items))
    (and (pair? items)
	 (if (predicate (car items))
	     (car items)
	     (loop (cdr items))))))

(define (list-search-negative items predicate)
  (let loop ((items items))
    (and (pair? items)
	 (if (predicate (car items))
	     (loop (cdr items))
	     (car items)))))

(define ((list-deletor predicate) items)
  (list-transform-negative items predicate))

(define (list-deletor! predicate)
  (letrec ((trim-initial-segment
	    (lambda (items)
	      (if (pair? items)
		  (if (predicate (car items))
		      (trim-initial-segment (cdr items))
		      (begin (locate-initial-segment items (cdr items))
			     items))
		  items)))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (pair? this)
		  (if (predicate (car this))
		      (set-cdr! last (trim-initial-segment (cdr this)))
		      (locate-initial-segment this (cdr this)))
		  this))))
    trim-initial-segment))

;;;; Membership/Association Lists

(define (initialize-package!)
  (set! memv (member-procedure eqv?))
  (set! member (member-procedure equal?))
  (set! delv (delete-member-procedure list-deletor eqv?))
  (set! delete (delete-member-procedure list-deletor equal?))
  (set! delv! (delete-member-procedure list-deletor! eqv?))
  (set! delete! (delete-member-procedure list-deletor! equal?))
  (set! assv (association-procedure eqv? car))
  (set! assoc (association-procedure equal? car))
  (set! del-assq (delete-association-procedure list-deletor eq? car))
  (set! del-assv (delete-association-procedure list-deletor eqv? car))
  (set! del-assoc (delete-association-procedure list-deletor equal? car))
  (set! del-assq! (delete-association-procedure list-deletor! eq? car))
  (set! del-assv! (delete-association-procedure list-deletor! eqv? car))
  (set! del-assoc! (delete-association-procedure list-deletor! equal? car)))

(define memv)
(define member)
(define delv)
(define delete)
(define delv!)
(define delete!)
(define assv)
(define assoc)
(define del-assq)
(define del-assv)
(define del-assoc)
(define del-assq!)
(define del-assv!)
(define del-assoc!)

(define (member-procedure predicate)
  (lambda (item items)
    (let loop ((items items))
      (and (pair? items)
	   (if (predicate (car items) item)
	       items
	       (loop (cdr items)))))))

(define ((delete-member-procedure deletor predicate) item items)
  ((deletor (lambda (match) (predicate match item))) items))

(define (association-procedure predicate selector)
  (lambda (key alist)
    (let loop ((alist alist))
      (and (pair? alist)
	   (if (predicate (selector (car alist)) key)
	       (car alist)
	       (loop (cdr alist)))))))

(define ((delete-association-procedure deletor predicate selector) key alist)
  ((deletor (lambda (entry) (predicate (selector entry) key))) alist))

;;; The following could be defined using the generic procedures above,
;;; but the compiler produces better code for them this way.  The only
;;; reason to use these procedures is speed, so we crank them up.

(define (memq item items)
  (let loop ((items items))
    (and (pair? items)
	 (if (eq? (car items) item)
	     items
	     (loop (cdr items))))))

(define (assq key alist)
  (let loop ((alist alist))
    (and (pair? alist)
	 (if (eq? (caar alist) key)
	     (car alist)
	     (loop (cdr alist))))))

(define (delq item items)
  (let loop ((items items))
    (if (pair? items)
	(if (eq? item (car items))
	    (loop (cdr items))
	    (cons (car items) (loop (cdr items))))
	'())))

(define (delq! item items)
  (letrec ((trim-initial-segment
	    (lambda (items)
	      (if (pair? items)
		  (if (eq? item (car items))
		      (trim-initial-segment (cdr items))
		      (begin (locate-initial-segment items (cdr items))
			     items))
		  items)))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (pair? this)
		  (if (eq? item (car this))
		      (set-cdr! last (trim-initial-segment (cdr this)))
		      (locate-initial-segment this (cdr this)))
		  this))))
    (trim-initial-segment items)))

;;;; Lastness and Segments

(define (last-pair list)
  (if (not (pair? list))
      (error "LAST-PAIR: Argument not a pair" list))
  (let loop ((list list))
    (if (pair? (cdr list))
	(loop (cdr list))
	list)))

(define (except-last-pair list)
  (if (not (pair? list))
      (error "EXCEPT-LAST-PAIR: Argument not a pair" list))
  (let loop ((list list))
    (if (pair? (cdr list))
	(cons (car list)
	      (loop (cdr list)))
	'())))

(define (except-last-pair! list)
  (if (not (pair? list))
      (error "EXCEPT-LAST-PAIR!: Argument not a pair" list))
  (if (pair? (cdr list))
      (begin (let loop ((list list))
	       (if (pair? (cddr list))
		   (loop (cdr list))
		   (set-cdr! list '())))
	     list)
      '()))