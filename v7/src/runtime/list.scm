#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/list.scm,v 14.9 1989/09/20 15:05:47 cph Exp $

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
	(begin
	  (if (not (pair? list))
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
	(begin
	  (if (not (null? items))
	      (error "LIST-COPY: argument not proper list" items))
	  '()))))

(define (alist-copy alist)
  (if (pair? alist)
      (begin
	(if (not (pair? (car alist)))
	    (error "ALIST-COPY: illegal alist element" (car alist)))
	(cons (cons (car (car alist)) (cdr (car alist)))
	      (alist-copy (cdr alist))))
      (begin
	(if (not (null? alist))
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

(define (weak-memq object weak-list)
  (let ((object (if object object weak-pair/false)))
    (let loop ((weak-list weak-list))
      (and (not (null? weak-list))
	   (if (eq? object (system-pair-car weak-list))
	       weak-list
	       (loop (system-pair-cdr weak-list)))))))

(define (weak-list->list weak-list)
  (if (weak-pair? weak-list)
      (let ((car (system-pair-car weak-list)))
	(if (not car)
	    (weak-list->list (system-pair-cdr weak-list))
	    (cons (if (eq? car weak-pair/false) false car)
		  (weak-list->list (system-pair-cdr weak-list)))))
      (begin
	(if (not (null? weak-list))
	    (error "improperly terminated weak list" weak-list))
	'())))

(define (list->weak-list list)
  (if (pair? list)
      (weak-cons (car list) (list->weak-list (cdr list)))
      (begin
	(if (not (null? list))
	    (error "improperly terminated list" list))
	'())))

(define weak-pair/false
  "weak-pair/false")

;;;; Standard Selectors

(declare (integrate-operator safe-car safe-cdr))

(define (safe-car x)
  (if (pair? x) (car x) (error "not a pair" x)))

(define (safe-cdr x)
  (if (pair? x) (cdr x) (error "not a pair" x)))

(define (caar x) (safe-car (safe-car x)))
(define (cadr x) (safe-car (safe-cdr x)))
(define (cdar x) (safe-cdr (safe-car x)))
(define (cddr x) (safe-cdr (safe-cdr x)))

(define (caaar x) (safe-car (safe-car (safe-car x))))
(define (caadr x) (safe-car (safe-car (safe-cdr x))))
(define (cadar x) (safe-car (safe-cdr (safe-car x))))
(define (caddr x) (safe-car (safe-cdr (safe-cdr x))))

(define (cdaar x) (safe-cdr (safe-car (safe-car x))))
(define (cdadr x) (safe-cdr (safe-car (safe-cdr x))))
(define (cddar x) (safe-cdr (safe-cdr (safe-car x))))
(define (cdddr x) (safe-cdr (safe-cdr (safe-cdr x))))

(define (caaaar x) (safe-car (safe-car (safe-car (safe-car x)))))
(define (caaadr x) (safe-car (safe-car (safe-car (safe-cdr x)))))
(define (caadar x) (safe-car (safe-car (safe-cdr (safe-car x)))))
(define (caaddr x) (safe-car (safe-car (safe-cdr (safe-cdr x)))))

(define (cadaar x) (safe-car (safe-cdr (safe-car (safe-car x)))))
(define (cadadr x) (safe-car (safe-cdr (safe-car (safe-cdr x)))))
(define (caddar x) (safe-car (safe-cdr (safe-cdr (safe-car x)))))
(define (cadddr x) (safe-car (safe-cdr (safe-cdr (safe-cdr x)))))

(define (cdaaar x) (safe-cdr (safe-car (safe-car (safe-car x)))))
(define (cdaadr x) (safe-cdr (safe-car (safe-car (safe-cdr x)))))
(define (cdadar x) (safe-cdr (safe-car (safe-cdr (safe-car x)))))
(define (cdaddr x) (safe-cdr (safe-car (safe-cdr (safe-cdr x)))))

(define (cddaar x) (safe-cdr (safe-cdr (safe-car (safe-car x)))))
(define (cddadr x) (safe-cdr (safe-cdr (safe-car (safe-cdr x)))))
(define (cdddar x) (safe-cdr (safe-cdr (safe-cdr (safe-car x)))))
(define (cddddr x) (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))

(define (first x) (safe-car x))
(define (second x) (safe-car (safe-cdr x)))
(define (third x) (safe-car (safe-cdr (safe-cdr x))))
(define (fourth x) (safe-car (safe-cdr (safe-cdr (safe-cdr x)))))
(define (fifth x) (safe-car (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))

(define (sixth x)
  (safe-car (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))

(define (seventh x)
  (safe-car
   (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))))

(define (eighth x)
  (safe-car
   (safe-cdr
    (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))))

(define (ninth x)
  (safe-car
   (safe-cdr
    (safe-cdr
     (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x))))))))))

(define (tenth x)
  (safe-car
   (safe-cdr
    (safe-cdr
     (safe-cdr
      (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr (safe-cdr x)))))))))))

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
		  (begin
		    (if (not (null? list))
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
	(begin
	  (if (not (null? rest))
	      (error "REVERSE: Argument not a list" l))
	  so-far))))

(define (reverse! l)
  (let loop ((current l) (new-cdr '()))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error "REVERSE!: Argument not a list" l))
	  new-cdr))))

;;;; Mapping Procedures

(let-syntax
    ((mapping-procedure
      (macro (name combiner initial-value procedure lists)
	(let ((name (string-upcase (symbol->string name))))
	  `(BEGIN
	     (IF (NULL? ,lists)
		 (ERROR ,(string-append name ": Too few arguments")
			,procedure))
	     (LET ((INITIAL-VALUE
		    (LAMBDA (LIST)
		      (IF (NOT (NULL? LIST))
			  (ERROR ,(string-append name ": Argument not a list")
				 LIST))
		      ,initial-value)))
	       (IF (NULL? (CDR ,lists))
		   (LET 1-LOOP ((LIST (CAR ,lists)))
		     (IF (PAIR? LIST)
			 (,combiner (,procedure (CAR LIST))
				    (1-LOOP (CDR LIST)))
			 (INITIAL-VALUE LIST)))
		   (LET N-LOOP ((LISTS ,lists))
		     (LET PARSE-CARS
			 ((LISTS LISTS)
			  (RECEIVER
			   (LAMBDA (CARS CDRS)
			     (,combiner (APPLY ,procedure CARS)
					(N-LOOP CDRS)))))
		       (COND ((NULL? LISTS)
			      (RECEIVER '() '()))
			     ((PAIR? (CAR LISTS))
			      (PARSE-CARS (CDR LISTS)
					  (LAMBDA (CARS CDRS)
					    (RECEIVER
					     (CONS (CAR (CAR LISTS)) CARS)
					     (CONS (CDR (CAR LISTS)) CDRS)))))
			     (ELSE
			      (INITIAL-VALUE (CAR LISTS)))))))))))))

(define (for-each procedure . lists)
  (mapping-procedure for-each begin unspecific procedure lists))

(define (map procedure . lists)
  (mapping-procedure map cons '() procedure lists))

(define (map* initial-value procedure . lists)
  (mapping-procedure map* cons initial-value procedure lists))

(define (append-map procedure . lists)
  (mapping-procedure append-map append '() procedure lists))

(define (append-map* initial-value procedure . lists)
  (mapping-procedure append-map* append initial-value procedure lists))

(define (append-map! procedure . lists)
  (mapping-procedure append-map! append! '() procedure lists))

(define (append-map*! initial-value procedure . lists)
  (mapping-procedure append-map*! append! initial-value procedure lists))

;;; end LET-SYNTAX
)

(define mapcan append-map!)
(define mapcan* append-map*!)

(define (reduce procedure initial list)
  (let ((result
	 (lambda (l value)
	   (if (not (null? l))
	       (error "REDUCE: Argument not a list" list))
	   value)))
    (if (pair? list)
	(let loop ((value (car list)) (l (cdr list)))
	  (if (pair? l)
	      (loop (procedure value (car l)) (cdr l))
	      (result l value)))
	(result list initial))))

(define (reduce-right procedure initial list)
  (let ((result
	 (lambda (l value)
	   (if (not (null? l))
	       (error "REDUCE-RIGHT: Argument not a list" list))
	   value)))
    (if (pair? list)
	(let loop ((value (car list)) (l (cdr list)))
	  (if (pair? l)
	      (procedure value (loop (car l) (cdr l)))
	      (result l value)))
	(result list initial))))

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
		      (begin
			(locate-initial-segment items (cdr items))
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
	 (if (eq? (car (car alist)) key)
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
		      (begin
			(locate-initial-segment items (cdr items))
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
      (begin
	(let loop ((list list))
	  (if (pair? (cdr (cdr list)))
	      (loop (cdr list))
	      (set-cdr! list '())))
	list)
      '()))