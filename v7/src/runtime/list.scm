#| -*-Scheme-*-

$Id: list.scm,v 14.18 1993/09/30 17:08:17 adams Exp $

Copyright (c) 1988-93 Massachusetts Institute of Technology

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
  (guarantee-index length 'MAKE-LIST)
  (let ((value (if (default-object? value) '() value)))
    (let loop ((n length) (result '()))
      (if (zero? n)
	  result
	  (loop (- n 1) (cons value result))))))

(define (circular-list . items)
  (if (not (null? items))
      (let loop ((l items))
	(if (null? (cdr l))
	    (set-cdr! l items)
	    (loop (cdr l)))))
  items)

(define (make-circular-list length #!optional value)
  (guarantee-index length 'MAKE-CIRCULAR-LIST)
  (if (positive? length)
      (let ((value (if (default-object? value) '() value)))
	(let ((last (cons value '())))
	  (let loop ((n (- length 1)) (result last))
	    (if (zero? n)
		(begin
		  (set-cdr! last result)
		  result)
		(loop (- n 1) (cons value result))))))
      '()))

(define (list-ref list index)
  (let ((tail (list-tail list index)))
    (if (not (pair? tail))
	(error:bad-range-argument index 'LIST-REF))
    (car tail)))

(define (list-tail list index)
  (guarantee-index index 'LIST-TAIL)
  (let loop ((list list) (index* index))
    (if (zero? index*)
	list
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'LIST-TAIL))
	  (loop (cdr list) (- index* 1))))))

(define (list-head list index)
  (guarantee-index index 'LIST-HEAD)
  (let loop ((list list) (index* index))
    (if (zero? index*)
	'()
	(begin
	  (if (not (pair? list))
	      (error:bad-range-argument index 'LIST-HEAD))
	  (cons (car list) (loop (cdr list) (- index* 1)))))))

(define (sublist list start end)
  (list-head (list-tail list start) (- end start)))

(define (list? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(let ((l1 (cdr l1)))
	  (and (not (eq? l1 l2))
	       (if (pair? l1)
		   (loop (cdr l1) (cdr l2))
		   (null? l1))))
	(null? l1))))

(define (alist? object)
  (let loop ((l1 object) (l2 object))
    (if (pair? l1)
	(and (pair? (car l1))
	     (let ((l1 (cdr l1)))
	       (and (not (eq? l1 l2))
		    (if (pair? l1)
			(and (pair? (car l1))
			     (loop (cdr l1) (cdr l2)))
			(null? l1)))))
	(null? l1))))

(define (list-copy items)
  (let loop ((items* items))
    (if (pair? items*)
	(cons (car items*) (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'LIST-COPY))
	  '()))))

(define (alist-copy alist)
  (let loop ((alist* alist))
    (if (pair? alist*)
	(begin
	  (if (not (pair? (car alist*)))
	      (error:wrong-type-argument alist "alist" 'ALIST-COPY))
	  (cons (cons (car (car alist*)) (cdr (car alist*)))
		(loop (cdr alist*))))
	(begin
	  (if (not (null? alist*))
	      (error:wrong-type-argument alist "alist" 'ALIST-COPY))
	  '()))))

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

(define (weak-list->list items)
  (let loop ((items* items))
    (if (weak-pair? items*)
	(let ((car (system-pair-car items*)))
	  (if (not car)
	      (loop (system-pair-cdr items*))
	      (cons (if (eq? car weak-pair/false) false car)
		    (loop (system-pair-cdr items*)))))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "weak list" 'WEAK-LIST->LIST))
	  '()))))

(define (list->weak-list items)
  (let loop ((items* items))
    (if (pair? items*)
	(weak-cons (car items*) (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'LIST->WEAK-LIST))
	  '()))))

(define weak-pair/false
  "weak-pair/false")

(define (weak-memq object items)
  (let ((object (or object weak-pair/false)))
    (let loop ((items* items))
      (if (weak-pair? items*)
	  (if (eq? object (system-pair-car items*))
	      items*
	      (loop (system-pair-cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "weak list" 'WEAK-MEMQ))
	    #f)))))

(define (weak-delq! item items)
  (letrec ((trim-initial-segment
	    (lambda (items*)
	      (if (weak-pair? items*)
		  (if (or (eq? item (system-pair-car items*))
			  (eq? #f (system-pair-car items*)))
		      (trim-initial-segment (system-pair-cdr items*))
		      (begin
			(locate-initial-segment items*
						(system-pair-cdr items*))
			items*))
		  (begin
		    (if (not (null? items*))
			(error:wrong-type-argument items "weak list"
						   'WEAK-MEMQ))
		    '()))))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (weak-pair? this)
		  (if (or (eq? item (system-pair-car this))
			  (eq? false (system-pair-car this)))
		      (set-cdr! last
				(trim-initial-segment (system-pair-cdr this)))
		      (locate-initial-segment this (system-pair-cdr this)))
		  (if (not (null? this))
		      (error:wrong-type-argument items "weak list"
						 'WEAK-MEMQ))))))
    (trim-initial-segment items)))

;;;; Standard Selectors

(declare (integrate-operator safe-car safe-cdr))

(define (safe-car x)
  (if (pair? x) (car x) (error:not-a-pair x)))

(define (safe-cdr x)
  (if (pair? x) (cdr x) (error:not-a-pair x)))

(define (error:not-a-pair x)
  (error:wrong-type-argument x "pair" #f))

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

;;; This algorithm uses a finite amount of stack and therefore half
;;; the memory of the simple recursive algorithm.  In addition, a
;;; clever compiler could optimize this into the obvious loop that
;;; everyone would write in assembly language.

(define (append . lists)
  (let ((lists (reverse! lists)))
    (if (null? lists)
	'()
	(let loop ((accum (car lists)) (rest (cdr lists)))
	  (if (null? rest)
	      accum
	      (loop (let ((l1 (car rest)))
		      (cond ((pair? l1)
			     (let ((root (cons (car l1) #f)))
			       (let loop ((cell root) (next (cdr l1)))
				 (cond ((pair? next)
					(let ((cell* (cons (car next) #f)))
					  (set-cdr! cell cell*)
					  (loop cell* (cdr next))))
				       ((null? next)
					(set-cdr! cell accum))
				       (else
					(error:wrong-type-argument (car rest)
								   "list"
								   'APPEND))))
			       root))
			    ((null? l1)
			     accum)
			    (else
			     (error:wrong-type-argument (car rest) "list"
							'APPEND))))
		    (cdr rest)))))))

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
		   (error:wrong-type-argument (car lists) "list" 'APPEND!))
	       (loop (car tail) (cdr tail)))))))

(define (reverse l)
  (let loop ((rest l) (so-far '()))
    (if (pair? rest)
	(loop (cdr rest) (cons (car rest) so-far))
	(begin
	  (if (not (null? rest))
	      (error:wrong-type-argument l "list" 'REVERSE))
	  so-far))))

(define (reverse! l)
  (let loop ((current l) (new-cdr '()))
    (if (pair? current)
	(let ((next (cdr current)))
	  (set-cdr! current new-cdr)
	  (loop next current))
	(begin
	  (if (not (null? current))
	      (error:wrong-type-argument l "list" 'REVERSE!))
	  new-cdr))))

;;;; Mapping Procedures

(let-syntax
    ((mapping-procedure
      (macro (name combiner initial-value procedure first rest)
	(let ((name (string-upcase (symbol->string name))))
	  `(IF (NULL? ,rest)
	       (LET 1-LOOP ((LIST ,first))
		 (IF (PAIR? LIST)
		     (,combiner (,procedure (CAR LIST))
				(1-LOOP (CDR LIST)))
		     (BEGIN
		       (IF (NOT (NULL? LIST))
			   (ERROR:WRONG-TYPE-ARGUMENT ,first "list" ',name))
		       ,initial-value)))
	       (LET ((LISTS (CONS ,first ,rest)))
		 (LET N-LOOP ((LISTS* LISTS))
		   (LET PARSE-CARS
		       ((LISTS LISTS)
			(LISTS* LISTS*)
			(CARS '())
			(CDRS '()))
		     (COND ((NULL? LISTS*)
			    (,combiner (APPLY ,procedure (REVERSE! CARS))
				       (N-LOOP (REVERSE! CDRS))))
			   ((PAIR? (CAR LISTS*))
			    (PARSE-CARS (CDR LISTS)
					(CDR LISTS*)
					(CONS (CAR (CAR LISTS*)) CARS)
					(CONS (CDR (CAR LISTS*)) CDRS)))
			   (ELSE
			    (IF (NOT (NULL? (CAR LISTS*)))
				(ERROR:WRONG-TYPE-ARGUMENT (CAR LISTS) "list"
							   ',name))
			    ,initial-value))))))))))

(define (for-each procedure first . rest)
  (mapping-procedure for-each begin unspecific procedure first rest))

(define (map procedure first . rest)
  (mapping-procedure map cons '() procedure first rest))

(define (map* initial-value procedure first . rest)
  (mapping-procedure map* cons initial-value procedure first rest))

(define (append-map procedure first . rest)
  (mapping-procedure append-map append '() procedure first rest))

(define (append-map* initial-value procedure first . rest)
  (mapping-procedure append-map* append initial-value procedure first rest))

(define (append-map! procedure first . rest)
  (mapping-procedure append-map! append! '() procedure first rest))

(define (append-map*! initial-value procedure first . rest)
  (mapping-procedure append-map*! append! initial-value procedure first rest))

;;; end LET-SYNTAX
)

(define mapcan append-map!)
(define mapcan* append-map*!)

(define (reduce procedure initial list)
  (if (pair? list)
      (let loop ((value (car list)) (l (cdr list)))
	(if (pair? l)
	    (loop (procedure value (car l)) (cdr l))
	    (begin
	      (if (not (null? l))
		  (error:wrong-type-argument list "list" 'REDUCE))
	      value)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'REDUCE))
	initial)))

(define (reduce-right procedure initial list)
  (if (pair? list)
      (let loop ((value (car list)) (l (cdr list)))
	(if (pair? l)
	    (procedure value (loop (car l) (cdr l)))
	    (begin
	      (if (not (null? l))
		  (error:wrong-type-argument list "list" 'REDUCE-RIGHT))
	      value)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'REDUCE-RIGHT))
	initial)))

(define (fold-left procedure initial list)
  (if (pair? list)
      (fold-left procedure (procedure initial (car list)) (cdr list))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'FOLD-LEFT))
	initial)))

(define (fold-right procedure initial list)
  (if (pair? list)
      (procedure (car list) (fold-right procedure initial (cdr list)))
      (begin
	(if (not (null? list))
	    (error:wrong-type-argument list "list" 'FOLD-RIGHT))
	initial)))


;;;; Generalized List Operations

(define (list-transform-positive items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (cons (car items*) (loop (cdr items*)))
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list"
					 'LIST-TRANSFORM-POSITIVE))
	  '()))))

(define (list-transform-negative items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (loop (cdr items*))
	    (cons (car items*) (loop (cdr items*))))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list"
					 'LIST-TRANSFORM-NEGATIVE))
	  '()))))

(define (list-search-positive items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (car items*)
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list"
					 'LIST-SEARCH-POSITIVE))
	  #f))))

(define (list-search-negative items predicate)
  (let loop ((items* items))
    (if (pair? items*)
	(if (predicate (car items*))
	    (loop (cdr items*))
	    (car items*))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list"
					 'LIST-SEARCH-NEGATIVE))
	  #f))))

(define ((list-deletor predicate) items)
  (list-transform-negative items predicate))

(define (list-deletor! predicate)
  (lambda (items)
    (letrec ((trim-initial-segment
	      (lambda (items*)
		(if (pair? items*)
		    (if (predicate (car items*))
			(trim-initial-segment (cdr items*))
			(begin
			  (locate-initial-segment items* (cdr items*))
			  items*))
		    (begin
		      (if (not (null? items*))
			  (error:wrong-type-argument items "list" #f))
		      '()))))
	     (locate-initial-segment
	      (lambda (last this)
		(if (pair? this)
		    (if (predicate (car this))
			(set-cdr! last (trim-initial-segment (cdr this)))
			(locate-initial-segment this (cdr this)))
		    (if (not (null? this))
			(error:wrong-type-argument items "list" #f))))))
      (trim-initial-segment items))))

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
  (set! del-assoc! (delete-association-procedure list-deletor! equal? car))
  unspecific)

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
    (let loop ((items* items))
      (if (pair? items*)
	  (if (predicate (car items*) item)
	      items*
	      (loop (cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "list" #f))
	    #f)))))

(define ((delete-member-procedure deletor predicate) item items)
  ((deletor (lambda (match) (predicate match item))) items))

(define (association-procedure predicate selector)
  (lambda (key items)
    (let loop ((items* items))
      (if (pair? items*)
	  (if (predicate (selector (car items*)) key)
	      (car items*)
	      (loop (cdr items*)))
	  (begin
	    (if (not (null? items*))
		(error:wrong-type-argument items "list" #f))
	    #f)))))

(define ((delete-association-procedure deletor predicate selector) key alist)
  ((deletor (lambda (entry) (predicate (selector entry) key))) alist))

;;; The following could be defined using the generic procedures above,
;;; but the compiler produces better code for them this way.  The only
;;; reason to use these procedures is speed, so we crank them up.

(define (memq item items)
  (let loop ((items* items))
    (if (pair? items*)
	(if (eq? (car items*) item)
	    items*
	    (loop (cdr items*)))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'MEMQ))
	  #f))))

(define (assq key alist)
  (let loop ((alist* alist))
    (if (pair? alist*)
	(begin
	  (if (not (pair? (car alist*)))
	      (error:wrong-type-argument alist "alist" 'ASSQ))
	  (if (eq? (car (car alist*)) key)
	      (car alist*)
	      (loop (cdr alist*))))
	(begin
	  (if (not (null? alist*))
	      (error:wrong-type-argument alist "alist" 'ASSQ))
	  #f))))

(define (delq item items)
  (let loop ((items* items))
    (if (pair? items*)
	(if (eq? item (car items*))
	    (loop (cdr items*))
	    (cons (car items*) (loop (cdr items*))))
	(begin
	  (if (not (null? items*))
	      (error:wrong-type-argument items "list" 'DELQ))
	  '()))))

(define (delq! item items)
  (letrec ((trim-initial-segment
	    (lambda (items*)
	      (if (pair? items*)
		  (if (eq? item (car items*))
		      (trim-initial-segment (cdr items*))
		      (begin
			(locate-initial-segment items* (cdr items*))
			items*))
		  (begin
		    (if (not (null? items*))
			(error:wrong-type-argument items "list" 'DELQ!))
		    '()))))
	   (locate-initial-segment
	    (lambda (last this)
	      (if (pair? this)
		  (if (eq? item (car this))
		      (set-cdr! last (trim-initial-segment (cdr this)))
		      (locate-initial-segment this (cdr this)))
		  (if (not (null? this))
		      (error:wrong-type-argument items "list" 'DELQ!))))))
    (trim-initial-segment items)))

;;;; Lastness and Segments

(define (last-pair list)
  (guarantee-pair list 'LAST-PAIR)
  (let loop ((list list))
    (if (pair? (cdr list))
	(loop (cdr list))
	list)))

(define (except-last-pair list)
  (guarantee-pair list 'EXCEPT-LAST-PAIR)
  (let loop ((list list))
    (if (pair? (cdr list))
	(cons (car list)
	      (loop (cdr list)))
	'())))

(define (except-last-pair! list)
  (guarantee-pair list 'EXCEPT-LAST-PAIR!)
  (if (pair? (cdr list))
      (begin
	(let loop ((list list))
	  (if (pair? (cdr (cdr list)))
	      (loop (cdr list))
	      (set-cdr! list '())))
	list)
      '()))

(define-integrable (guarantee-pair object procedure)
  (if (not (pair? object))
      (error:wrong-type-argument object "pair" procedure)))

(define-integrable (guarantee-index object procedure)
  (if (not (exact-nonnegative-integer? object))
      (error:wrong-type-argument object "exact nonnegative integer"
				 procedure)))