;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/defstr.scm,v 1.2 1987/08/11 05:41:01 cph Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Structure Definition Macro

(declare (usual-integrations))

#| 

This macro works like the Common Lisp `defstruct' with the following
differences:

* The default constructor procedure takes positional arguments, in the
same order as specified in the definition of the structure.  A keyword
constructor may be specified by giving the option KEYWORD-CONSTRUCTOR.

* The side effect procedure corresponding to the accessor "foo" is
given the name "set-foo!".

* Keywords are just ordinary symbols -- use "foo" instead of ":foo".

* The option values FALSE, NIL, TRUE, and T are treated as if the
appropriate boolean constant had been specified instead.

* After evaluating the structure definition, the name of the structure
is bound to a Scheme type object.  This works somewhat differently
from a Common Lisp type.

* The PRINT-FUNCTION option is named PRINT-PROCEDURE.  Its argument is
a procedure of one argument (the structure instance) rather than three
as in Common Lisp.

* By default, named structures are tagged with the Scheme type object.
In Common Lisp, the structures are tagged with symbols, but that
depends on the Common Lisp package system to help generate unique
tags; Scheme has no such way of generating unique symbols.

* The NAMED option may optionally take an argument, which should be
the name of a variable.  If used, structure instances will be tagged
with that variable's value rather than the Scheme type object.  The
variable must be defined when the defstruct is evaluated.

* The TYPE option is restricted to the values VECTOR and LIST.

* The INCLUDE option is not implemented.

* BOA constructors are described using Scheme lambda lists.  Since
there is nothing corresponding to &aux in Scheme lambda lists, this
functionality is not implemented.

|#

(define defstruct-package
  (make-environment

(syntax-table-define system-global-syntax-table 'DEFINE-STRUCTURE
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
	      ,@(print-procedure-definitions structure)))))

;;;; Parse Name-and-Options

(define (parse/name-and-options name-and-options)
  (if (pair? name-and-options)
      (parse/options (car name-and-options) (cdr name-and-options))
      (parse/options name-and-options '())))

(define (parse/options name options)
  (let ((conc-name (symbol-append name '-))
	(constructor-seen? false)
	(keyword-constructor? false)
	(constructor-name (symbol-append 'make- name))
	(boa-constructors '())
	(copier-name (symbol-append 'copy- name))
	(predicate-name (symbol-append name '?))
	(print-procedure false)
	(type-seen? false)
	(type 'STRUCTURE)
	(named-seen? false)
	(type-tagged? true)
	(tag-name name)
	(offset 0)
	(include false))

    (define (parse/option keyword arguments)
      (let ((n-arguments (length arguments)))
	(define (check-arguments min max)
	  (if (or (< n-arguments min) (> n-arguments max))
	      (error "Structure option used with wrong number of arguments"
		     keyword
		     arguments)))

	(case keyword
	  ((CONC-NAME)
	   (check-arguments 0 1)
	   (set! conc-name
		 (and (not (null? arguments))
		      (parse/option-value (car arguments)))))
	  ((KEYWORD-CONSTRUCTOR)
	   (check-arguments 0 1)
	   (set! constructor-seen? true)
	   (set! keyword-constructor? true)
	   (if (not (null? (cdr arguments)))
	       (set! constructor-name (parse/option-value (car arguments)))))
	  ((CONSTRUCTOR)
	   (check-arguments 0 2)
	   (cond ((null? arguments)
		  (set! constructor-seen? true))
		 ((null? (cdr arguments))
		  (set! constructor-seen? true)
		  (set! constructor-name (parse/option-value (car arguments))))
		 (else
		  (set! boa-constructors (cons arguments boa-constructors)))))
	  ((COPIER)
	   (check-arguments 0 1)
	   (if (not (null? arguments))
	       (set! copier-name (parse/option-value (car arguments)))))
	  ((PREDICATE)
	   (check-arguments 0 1)
	   (if (not (null? arguments))
	       (set! predicate-name (parse/option-value (car arguments)))))
	  ((PRINT-PROCEDURE)
	   (check-arguments 1 1)
	   (set! print-procedure (parse/option-value (car arguments))))
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
	  ((INCLUDE)
	   (check-arguments 1 1)
	   (set! include arguments))
	  (else
	   (error "Unrecognized structure option" keyword)))))

    (for-each (lambda (option)
		(if (pair? option)
		    (parse/option (car option) (cdr option))
		    (parse/option option '())))
	      options)
    (vector name
	    conc-name
	    keyword-constructor?
	    (and (or constructor-seen?
		     (null? boa-constructors))
		 constructor-name)
	    boa-constructors
	    copier-name
	    predicate-name
	    (or print-procedure
		(and (eq? tag-name name)
		     `(ACCESS DEFAULT-UNPARSER
			      DEFSTRUCT-PACKAGE
			      ,system-global-environment)))
	    type
	    (cond ((eq? type 'STRUCTURE) 'VECTOR)
		  ((eq? type 'VECTOR) 'VECTOR)
		  ((eq? type 'LIST) 'LIST)
		  (else (error "Unsupported structure type" type)))
	    (or (not type-seen?) named-seen?)
	    tag-name
	    offset
	    include
	    '())))

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
  (let ((kernel
	 (lambda (name default options)
	   (let ((type #T)
		 (read-only? false))
	     (define (loop options)
	       (if (not (null? options))
		   (begin (case (car options)
			    ((TYPE)
			     (set! type (parse/option-value (cadr options))))
			    ((READ-ONLY)
			     (set! read-only?
				   (parse/option-value (cadr options)))))
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

(define (parse/option-value name)
  (case name
    ((FALSE NIL) #F)
    ((TRUE T) #T)
    (else name)))

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
		       (VECTOR-SET! ,name ,n ,(car slots)))))
		(loop (cdr slots) (1+ n)))))
	 `(BEGIN ,@(loop slots reserved)))))

  (define-structure-refs structure 0
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

(define slot-assoc
  (association-procedure eq? slot/name))

;;;; Code Generation

(define (accessor-definitions structure)
  (mapcan (lambda (slot)
	    (let ((accessor-name
		   (if (structure/conc-name structure)
		       (symbol-append (structure/conc-name structure)
				      (slot/name slot))
		       (slot/name slot))))
	      `((DECLARE (INTEGRATE-OPERATOR ,accessor-name))
		(DEFINE (,accessor-name STRUCTURE)
		  ,(case (structure/scheme-type structure)
		     ((VECTOR)
		      `((ACCESS VECTOR-REF ,system-global-environment)
			STRUCTURE
			,(slot/index slot)))
		     ((LIST)
		      `((ACCESS LIST-REF ,system-global-environment)
			STRUCTURE
			,(slot/index slot)))
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
		      ,(case (structure/scheme-type structure)
			 ((VECTOR)
			  `((ACCESS VECTOR-SET! ,system-global-environment)
			    STRUCTURE
			    ,(slot/index slot)
			    VALUE))
			 ((LIST)
			  `((ACCESS SET-CAR! ,system-global-environment)
			    ((ACCESS LIST-TAIL ,system-global-environment)
			     STRUCTURE
			     ,(slot/index slot))
			    VALUE))
			 (else
			  (error "Unknown scheme type" structure))))))))
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
       ((ACCESS ,(structure/scheme-type structure) ,system-global-environment)
	,@(constructor-prefix-slots structure)
	,@slot-names))))

(define (constructor-definition/keyword structure name)
  (let ((keyword-list (string->uninterned-symbol "keyword-list")))
    `(DEFINE (,name . ,keyword-list)
       ,(let ((list-cons
	       `((ACCESS CONS* ,system-global-environment)
		 ,@(constructor-prefix-slots structure)
		 ((ACCESS KEYWORD-PARSER
			  DEFSTRUCT-PACKAGE
			  ,system-global-environment)
		  ,keyword-list
		  ((ACCESS LIST ,system-global-environment)
		   ,@(map (lambda (slot)
			    `((ACCESS CONS ,system-global-environment)
			      ',(slot/name slot)
			      ,(slot/default slot)))
			  (structure/slots structure)))))))
	  (case (structure/scheme-type structure)
	    ((VECTOR)
	     `((ACCESS LIST->VECTOR ,system-global-environment) ,list-cons))
	    ((LIST)
	     list-cons)
	    (else
	     (error "Unknown scheme type" structure)))))))

(define (constructor-definition/boa structure name lambda-list)
  `(DEFINE (,name . ,lambda-list)
     ;; *** Kludge -- SCHEME-TYPE happens to be same as constructor.
     ((ACCESS ,(structure/scheme-type structure) ,system-global-environment)
      ,@(constructor-prefix-slots structure)
      ,@((access parse-lambda-list syntaxer-package)
	 lambda-list
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
			     `(IF (UNASSIGNED? ,(slot/name slot))
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
  (if (structure/named? structure)
      `((DEFINE ,(structure/name structure)
	  ((ACCESS MAKE-STRUCTURE-TYPE
		   DEFSTRUCT-PACKAGE
		   ,system-global-environment)
	   ',structure
	   ,(and (not (eq? (structure/tag-name structure)
			   (structure/name structure)))
		 (structure/tag-name structure)))))
      '()))

(define (predicate-definitions structure)
  (if (and (structure/predicate-name structure)
	   (structure/named? structure))
      `((DEFINE ,(structure/predicate-name structure)
	  ((ACCESS TYPE-OBJECT-PREDICATE ,system-global-environment)
	   ,(structure/name structure))))
      '()))

(define (copier-definitions structure)
  (if (structure/copier-name structure)
      `((DEFINE ,(structure/copier-name structure)
	  ,(case (structure/scheme-type structure)
	     ((vector) `(ACCESS VECTOR-COPY ,system-global-environment))
	     ((list) `(ACCESS LIST-COPY ,system-global-environment))
	     (else (error "Unknown scheme type" structure)))))
      '()))

(define (print-procedure-definitions structure)
  (if (and (structure/print-procedure structure)
	   (structure/named? structure))
      `(((ACCESS ,(case (structure/scheme-type structure)
		    ((VECTOR) 'ADD-UNPARSER-SPECIAL-OBJECT!)
		    ((LIST) 'ADD-UNPARSER-SPECIAL-PAIR!)
		    (else (error "Unknown scheme type" structure)))
		 UNPARSER-PACKAGE
		 ,system-global-environment)
	 ,(structure/tag-name structure)
	 ,(structure/print-procedure structure)))
      '()))

;;;; Runtime Support

(define (keyword-parser argument-list default-alist)
  (if (null? argument-list)
      (map cdr default-alist)
      (let ((alist
	     (map (lambda (entry) (cons (car entry) (cdr entry)))
		  default-alist)))
	(define (loop arguments)
	  (if (not (null? arguments))
	      (begin
		(if (null? (cdr arguments))
		    (error "Keyword list does not have even length"
			   argument-list))
		(set-cdr! (or (assq (car arguments) alist)
			      (error "Unknown keyword" (car arguments)))
			  (cadr arguments))
		(loop (cddr arguments)))))
	(loop argument-list)
	(map cdr alist))))

(define (default-unparser structure-instance)
  ((access unparse-with-brackets unparser-package)
   (lambda ()
     (write
      (structure/name
       (or (structure-instance->description structure-instance)
	   (error "Not a named structure"))))
     (write-char #\Space)
     (write (hash structure-instance)))))

(define (make-structure-type structure tag)
  (let ((type
	 (make-sub-type
	  (structure/name structure)
	  (microcode-type-object 'vector)
	  (case (structure/scheme-type structure)
	    ((VECTOR)
	     (lambda (vector)
	       (and (not (zero? (vector-length vector)))
		    (eq? (vector-ref vector 0) tag))))
	    ((LIST)
	     (lambda (pair)
	       (eq? (car pair) tag)))
	    (else
	     (error "Unknown scheme type" structure))))))
    (if (not tag)
	(set! tag type))
    (2d-put! tag tag->structure structure)
    type))

(define (structure-instance->description structure)
  (2d-get (cond ((and (vector? structure)
		      (not (zero? (vector-length structure))))
		 (vector-ref structure 0))
		((pair? structure) (car structure))
		(else false))
	  tag->structure))

(define tag->structure
  "tag->structure")

;;; end DEFSTRUCT-PACKAGE
))