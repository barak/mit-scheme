#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/defstr.scm,v 14.5 1989/02/08 22:43:50 cph Exp $

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

;;;; Structure Definition Macro
;;; package: (runtime defstruct)

(declare (usual-integrations))

#| 

This macro works like the Common Lisp `defstruct' with the following
differences:

* The default constructor procedure takes positional arguments, in the
same order as specified in the definition of the structure.  A keyword
constructor may be specified by giving the option KEYWORD-CONSTRUCTOR.

* BOA constructors are described using Scheme lambda lists.  Since
there is nothing corresponding to &aux in Scheme lambda lists, this
functionality is not implemented.

* By default, no COPIER procedure is generated.

* The side effect procedure corresponding to the accessor "foo" is
given the name "set-foo!".

* Keywords are just ordinary symbols -- use "foo" instead of ":foo".

* The option values FALSE, NIL, TRUE, and T are treated as if the
appropriate boolean constant had been specified instead.

* The PRINT-FUNCTION option is named PRINT-PROCEDURE.  Its argument is
a procedure of two arguments (the unparser state and the structure
instance) rather than three as in Common Lisp.

* By default, named structures are tagged with a unique object of some
kind.  In Common Lisp, the structures are tagged with symbols, but
that depends on the Common Lisp package system to help generate unique
tags; Scheme has no such way of generating unique symbols.

* The NAMED option may optionally take an argument, which is normally
the name of a variable (any expression may be used, but it will be
evaluated whenever the tag name is needed).  If used, structure
instances will be tagged with that variable's value.  The variable
must be defined when the defstruct is evaluated.

* The TYPE option is restricted to the values VECTOR and LIST.

* The INCLUDE option is not implemented.

|#

(define (initialize-package!)
  (set! slot-assoc (association-procedure eq? slot/name))
  (syntax-table-define system-global-syntax-table 'DEFINE-STRUCTURE
    transform/define-structure))

(define transform/define-structure
  (macro (name-and-options . slot-descriptions)
    (let ((structure (parse/name-and-options name-and-options)))
      (structure/set-slots! structure
			    (parse/slot-descriptions structure
						     slot-descriptions))
      `(BEGIN ,@(type-definitions structure)
	      ,@(constructor-definitions structure)
	      ,@(accessor-definitions structure)
	      ,@(settor-definitions structure)
	      ,@(predicate-definitions structure)
	      ,@(copier-definitions structure)
	      ,@(print-procedure-definitions structure)
	      ',(structure/name structure)))))

;;;; Parse Name-and-Options

(define (parse/name-and-options name-and-options)
  (if (pair? name-and-options)
      (parse/options (car name-and-options) (cdr name-and-options))
      (parse/options name-and-options '())))

(define (parse/options name options)
  (if (not (symbol? name))
      (error "Structure name must be a symbol" name))
  (if (not (list? options))
      (error "Structure options must be a list" options))
  (let ((conc-name (symbol-append name '-))
	(constructor-seen? false)
	(keyword-constructor? false)
	(constructor-name (symbol-append 'make- name))
	(boa-constructors '())
	(copier-name false)
	(predicate-name (symbol-append name '?))
	(print-procedure default-value)
	(type-seen? false)
	(type 'STRUCTURE)
	(named-seen? false)
	(tag-name default-value)
	(offset 0)
	(include false))

    (define (parse/option keyword arguments)
      (let ((n-arguments (length arguments)))
	(define (check-arguments min max)
	  (if (or (< n-arguments min) (> n-arguments max))
	      (error "Structure option used with wrong number of arguments"
		     keyword
		     arguments)))

	(define (symbol-option default)
	  (parse/option-value symbol? keyword (car arguments) default))

	(case keyword
	  ((CONC-NAME)
	   (check-arguments 0 1)
	   (set! conc-name
		 (and (not (null? arguments))
		      (symbol-option (symbol-append name '-)))))
	  ((KEYWORD-CONSTRUCTOR)
	   (check-arguments 0 1)
	   (set! constructor-seen? true)
	   (set! keyword-constructor? true)
	   (if (not (null? (cdr arguments)))
	       (set! constructor-name
		     (symbol-option (symbol-append 'make- name)))))
	  ((CONSTRUCTOR)
	   (check-arguments 0 2)
	   (cond ((null? arguments)
		  (set! constructor-seen? true))
		 ((null? (cdr arguments))
		  (set! constructor-seen? true)
		  (set! constructor-name
			(symbol-option (symbol-append 'make- name))))
		 (else
		  (set! boa-constructors (cons arguments boa-constructors)))))
	  ((COPIER)
	   (check-arguments 0 1)
	   (if (not (null? arguments))
	       (set! copier-name (symbol-option (symbol-append 'copy- name)))))

	  ((PREDICATE)
	   (check-arguments 0 1)
	   (if (not (null? arguments))
	       (set! predicate-name (symbol-option (symbol-append name '?)))))
	  ((PRINT-PROCEDURE)
	   (check-arguments 1 1)
	   (set! print-procedure
		 (parse/option-value (lambda (x) x true)
				     keyword
				     (car arguments)
				     false)))
	  ((NAMED)
	   (check-arguments 0 1)
	   (set! named-seen? true)
	   (if (not (null? arguments))
	       (set! tag-name (car arguments))))
	  ((TYPE)
	   (check-arguments 1 1)
	   (set! type-seen? true)
	   (set! type (car arguments)))
	  ((INITIAL-OFFSET)
	   (check-arguments 1 1)
	   (set! offset (car arguments)))
	  #|
	  ((INCLUDE)
	   (check-arguments 1 1)
	   (set! include arguments))
	  |#
	  (else
	   (error "Unrecognized structure option" keyword)))))

    (for-each (lambda (option)
		(if (pair? option)
		    (parse/option (car option) (cdr option))
		    (parse/option option '())))
	      options)
    (vector structure
	    name
	    conc-name
	    keyword-constructor?
	    (and (or constructor-seen?
		     (null? boa-constructors))
		 constructor-name)
	    boa-constructors
	    copier-name
	    predicate-name
	    (if (eq? print-procedure default-value)
		`(,(absolute 'UNPARSER/STANDARD-METHOD) ',name)
		print-procedure)
	    type
	    (cond ((eq? type 'STRUCTURE) 'VECTOR)
		  ((eq? type 'VECTOR) 'VECTOR)
		  ((eq? type 'LIST) 'LIST)
		  (else (error "Unsupported structure type" type)))
	    (and (or (not type-seen?) named-seen?)
		 (if (eq? tag-name default-value) 'DEFAULT true))
	    (if (eq? tag-name default-value)
		name
		tag-name)
	    offset
	    include
	    '())))

(define default-value
  "default")

;;;; Parse Slot-Descriptions

(define (parse/slot-descriptions structure slot-descriptions)
  (define (loop slot-descriptions index)
    (if (null? slot-descriptions)
	'()
	(cons (parse/slot-description structure (car slot-descriptions) index)
	      (loop (cdr slot-descriptions) (1+ index)))))
  (loop slot-descriptions
	(if (structure/named? structure)
	    (1+ (structure/offset structure))
	    (structure/offset structure))))

(define (parse/slot-description structure slot-description index)
  structure
  (let ((kernel
	 (lambda (name default options)
	   (if (not (list? options))
	       (error "Structure slot options must be a list" options))
	   (let ((type #T)
		 (read-only? false))
	     (define (loop options)
	       (if (not (null? options))
		   (begin
		     (case (car options)
		       ((TYPE)
			(set! type
			      (parse/option-value symbol?
						  (car options)
						  (cadr options)
						  true)))
		       ((READ-ONLY)
			(set! read-only?
			      (parse/option-value boolean?
						  (car options)
						  (cadr options)
						  true)))
		       (else
			(error "Unrecognized structure slot option"
			       (car options))))
		     (loop (cddr options)))))
	     (loop options)
	     (vector name index default type read-only?)))))
    (if (pair? slot-description)
	(if (pair? (cdr slot-description))
	    (kernel (car slot-description)
		    (cadr slot-description)
		    (cddr slot-description))
	    (kernel (car slot-description) false '()))
	(kernel slot-description false '()))))

(define (parse/option-value predicate keyword option default)
  (case option
    ((FALSE NIL)
     #F)
    ((TRUE T)
     default)
    (else
     (if (not (or (predicate option)
		  (not option)
		  (eq? option default)))
	 (error "Structure option has incorrect type" keyword option))
     option)))

;;;; Descriptive Structure

(let-syntax
    ((define-structure-refs
       (macro (name reserved . slots)
	 (define (loop slots n)
	   (if (null? slots)
	       '()
	       (cons
		(let ((ref-name (symbol-append name '/ (car slots)))
		      (set-name (symbol-append name '/set- (car slots) '!)))
		  `(BEGIN
		     (DECLARE (INTEGRATE-OPERATOR ,ref-name ,set-name))
		     (DEFINE (,ref-name ,name)
		       (DECLARE (INTEGRATE ,name))
		       (VECTOR-REF ,name ,n))
		     (DEFINE (,set-name ,name ,(car slots))
		       (DECLARE (INTEGRATE ,name ,(car slots)))
		       (VECTOR-SET! ,name ,n ,(car slots))
		       ',unspecific)))
		(loop (cdr slots) (1+ n)))))
	 `(BEGIN ,@(loop slots reserved)))))

  (define-structure-refs structure 1
    name
    conc-name
    keyword-constructor?
    constructor-name
    boa-constructors
    copier-name
    predicate-name
    print-procedure
    type
    scheme-type
    named?
    tag-name
    offset
    include
    slots)

  (define-structure-refs slot 0
    name
    index
    default
    type
    read-only?))

(define-integrable structure
  (string->symbol "#[DEFSTRUCT-DESCRIPTION]"))

(define slot-assoc)

(define (structure? object)
  (and (vector? object)
       (not (zero? (vector-length object)))
       (eq? structure (vector-ref object 0))))

(define (tag->structure tag)
  (if (structure? tag)
      tag
      (let ((tag (named-structure/get-tag-description tag)))
	(and tag
	     (structure? tag)
	     tag))))

(define (named-structure? object)
  (cond ((vector? object)
	 (and (not (zero? (vector-length object)))
	      (tag->structure (vector-ref object 0))))
	((pair? object)
	 (tag->structure (car object)))
	(else false)))

(define (named-structure/description instance)
  (let ((structure
	 (tag->structure
	  (cond ((vector? instance) (vector-ref instance 0))
		((pair? instance) (car instance))
		(else (error "Illegal structure instance" instance))))))
    (if (not structure)
	(error "Illegal structure instance" instance))
    (let ((scheme-type (structure/scheme-type structure)))
      (if (not (case scheme-type
		 ((VECTOR) (vector? instance))
		 ((LIST) (list? instance))
		 (else (error "Illegal structure type" scheme-type))))
	  (error "Malformed structure instance" instance))
      (let ((accessor
	     (case scheme-type
	       ((VECTOR) vector-ref)
	       ((LIST) list-ref))))
	(map (lambda (slot)
	       `(,(slot/name slot) ,(accessor instance (slot/index slot))))
	     (structure/slots structure))))))

;;;; Code Generation

(define (absolute name)
  `(ACCESS ,name #F))

(define (accessor-definitions structure)
  (mapcan (lambda (slot)
	    (let ((accessor-name
		   (if (structure/conc-name structure)
		       (symbol-append (structure/conc-name structure)
				      (slot/name slot))
		       (slot/name slot))))
	      `((DECLARE (INTEGRATE-OPERATOR ,accessor-name))
		(DEFINE (,accessor-name STRUCTURE)
		  (DECLARE (INTEGRATE STRUCTURE))
		  ,(case (structure/scheme-type structure)
		     ((VECTOR)
		      `(,(absolute 'VECTOR-REF) STRUCTURE ,(slot/index slot)))
		     ((LIST)
		      `(,(absolute 'LIST-REF) STRUCTURE ,(slot/index slot)))
		     (else
		      (error "Unknown scheme type" structure)))))))
	  (structure/slots structure)))

(define (settor-definitions structure)
  (mapcan (lambda (slot)
	    (if (slot/read-only? slot)
		'()
		(let ((settor-name
		       (if (structure/conc-name structure)
			   (symbol-append 'SET-
					  (structure/conc-name structure)
					  (slot/name slot)
					  '!)
			   (symbol-append 'SET-
					  (slot/name slot)
					  '!))))
		  `((DECLARE (INTEGRATE-OPERATOR ,settor-name))
		    (DEFINE (,settor-name STRUCTURE VALUE)
		      (DECLARE (INTEGRATE STRUCTURE VALUE))
		      ,(case (structure/scheme-type structure)
			 ((VECTOR)
			  `(,(absolute 'VECTOR-SET!) STRUCTURE
						     ,(slot/index slot)
						     VALUE))
			 ((LIST)
			  `(,(absolute 'SET-CAR!)
			    (,(absolute 'LIST-TAIL) STRUCTURE
						    ,(slot/index slot))
			    VALUE))
			 (else
			  (error "Unknown scheme type" structure)))
		      ',unspecific)))))
	  (structure/slots structure)))

(define (constructor-definitions structure)
  `(,@(if (structure/constructor-name structure)
	  (list
	   ((if (structure/keyword-constructor? structure)
		constructor-definition/keyword
		constructor-definition/default)
	    structure
	    (structure/constructor-name structure)))
	  '())
    ,@(map (lambda (boa-constructor)
	     (constructor-definition/boa structure
					 (car boa-constructor)
					 (cadr boa-constructor)))
	   (structure/boa-constructors structure))))
(define (constructor-definition/default structure name)
  (let ((slot-names (map slot/name (structure/slots structure))))
    `(DEFINE (,name ,@slot-names)
       ;; *** Kludge -- SCHEME-TYPE happens to be same as constructor.
       (,(absolute (structure/scheme-type structure))
	,@(constructor-prefix-slots structure)
	,@slot-names))))

(define (constructor-definition/keyword structure name)
  (let ((keyword-list (string->uninterned-symbol "keyword-list")))
    `(DEFINE (,name . ,keyword-list)
       ,(let ((list-cons
	       `(,(absolute 'CONS*)
		 ,@(constructor-prefix-slots structure)
		 (,(absolute 'DEFINE-STRUCTURE/KEYWORD-PARSER)
		  ,keyword-list
		  (,(absolute 'LIST)
		   ,@(map (lambda (slot)
			    `(,(absolute 'CONS) ',(slot/name slot)
						,(slot/default slot)))
			  (structure/slots structure)))))))
	  (case (structure/scheme-type structure)
	    ((VECTOR)
	     `(,(absolute 'LIST->VECTOR) ,list-cons))
	    ((LIST)
	     list-cons)
	    (else
	     (error "Unknown scheme type" structure)))))))

(define (define-structure/keyword-parser argument-list default-alist)
  (if (null? argument-list)
      (map cdr default-alist)
      (let ((alist
	     (map (lambda (entry) (cons (car entry) (cdr entry)))
		  default-alist)))
	(let loop ((arguments argument-list))
	  (if (not (null? arguments))
	      (begin
		(if (null? (cdr arguments))
		    (error "Keyword list does not have even length"
			   argument-list))
		(set-cdr! (or (assq (car arguments) alist)
			      (error "Unknown keyword" (car arguments)))
			  (cadr arguments))
		(loop (cddr arguments)))))
	(map cdr alist))))

(define (constructor-definition/boa structure name lambda-list)
  `(DEFINE (,name . ,lambda-list)
     ;; *** Kludge -- SCHEME-TYPE happens to be same as constructor.
     (,(absolute (structure/scheme-type structure))
      ,@(constructor-prefix-slots structure)
      ,@(parse-lambda-list lambda-list
	  (lambda (required optional rest)
	    (let ((name->slot
		   (lambda (name)
		     (or (slot-assoc name (structure/slots structure))
			 (error "Not a defined structure slot" name)))))
	      (let ((required (map name->slot required))
		    (optional (map name->slot optional))
		    (rest (and rest (name->slot rest))))
		(map (lambda (slot)
		       (cond ((or (memq slot required)
				  (eq? slot rest))
			      (slot/name slot))
			     ((memq slot optional)
			      `(IF (DEFAULT-OBJECT? ,(slot/name slot))
				   ,(slot/default slot)
				   ,(slot/name slot)))
			     (else
			      (slot/default slot))))
		     (structure/slots structure)))))))))

(define (constructor-prefix-slots structure)
  (let ((offsets (make-list (structure/offset structure) false)))
    (if (structure/named? structure)
	(cons (structure/tag-name structure) offsets)
	offsets)))

(define (type-definitions structure)
  (cond ((not (structure/named? structure))
	 '())
	((eq? (structure/named? structure) 'DEFAULT)
	 `((DEFINE ,(structure/tag-name structure)
	     ',structure)))
	(else
	 `((NAMED-STRUCTURE/SET-TAG-DESCRIPTION!
	    ,(structure/tag-name structure)
	    ',structure)))))

(define (predicate-definitions structure)
  (if (and (structure/predicate-name structure)
	   (structure/named? structure))
      (case (structure/scheme-type structure)
	((VECTOR)
	 `((DEFINE (,(structure/predicate-name structure) OBJECT)
	     (AND (,(absolute 'VECTOR?) OBJECT)
		  (,(absolute 'NOT)
		   (,(absolute 'ZERO?) (,(absolute 'VECTOR-LENGTH) OBJECT)))
		  (,(absolute 'EQ?) (,(absolute 'VECTOR-REF) OBJECT 0)
				    ,(structure/tag-name structure))))))
	((LIST)
	 `((DEFINE (,(structure/predicate-name structure) OBJECT)
	     (AND (,(absolute 'PAIR?) OBJECT)
		  (,(absolute 'EQ?) (,(absolute 'CAR) OBJECT)
				    ,(structure/tag-name structure))))))
	(else
	 (error "Unknown scheme type" structure)))
      '()))

(define (copier-definitions structure)
  (let ((copier-name (structure/copier-name structure)))
    (if copier-name
	`((DECLARE (INTEGRATE-OPERATOR ,copier-name))
	  ,(case (structure/scheme-type structure)
	     ((VECTOR)
	      `(DEFINE (,copier-name OBJECT)
		 (DECLARE (INTEGRATE OBJECT))
		 (,(absolute 'VECTOR-COPY) OBJECT)))
	     ((LIST)
	      `(DEFINE (,copier-name OBJECT)
		 (DECLARE (INTEGRATE OBJECT))
		 (,(absolute 'LIST-COPY) OBJECT)))
	     (else
	      (error "Unknown scheme type" structure))))
	'())))

(define (print-procedure-definitions structure)
  (if (and (structure/print-procedure structure)
	   (structure/named? structure))
      `((,(absolute (case (structure/scheme-type structure)
		      ((VECTOR) 'UNPARSER/SET-TAGGED-VECTOR-METHOD!)
		      ((LIST) 'UNPARSER/SET-TAGGED-PAIR-METHOD!)
		      (else (error "Unknown scheme type" structure))))
	 ,(structure/tag-name structure)
	 ,(structure/print-procedure structure)))
      '()))