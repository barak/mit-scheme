#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/defstr.scm,v 14.15 1991/01/11 22:08:09 markf Exp $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

* COPIERS are not allowed for structures of type RECORD.

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

* The TYPE option is restricted to the values VECTOR, LIST and RECORD.

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
      (if (eq? (structure/scheme-type structure) 'RECORD)
	  (let ((tag-name (structure/tag-name structure)))
	    (structure/set-type! structure
				 (make-record-type
				  (make-record-type-name structure)
				  (map slot/name
				       (structure/slots structure))))))
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
	(default-constructor-disabled? false)
	(boa-constructors '())
	(keyword-constructors '())
	(copier-name false)
	(predicate-name (symbol-append name '?))
	(print-procedure default-value)
	(type-seen? false)
	(type 'STRUCTURE)
	(named-seen? false)
	(tag-name default-value)
	(offset 0)
	(include false))

    (define (parse/option option keyword arguments)
      (let ((n-arguments (length arguments)))

	(define (check-arguments min max)
	  (if (or (< n-arguments min) (> n-arguments max))
	      (error "Structure option used with wrong number of arguments"
		     option)))

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
	   (set! keyword-constructors
		 (cons (list option
			     (if (null? arguments)
				 (symbol-append 'make- name)
				 (car arguments)))
		       keyword-constructors)))
	  ((CONSTRUCTOR)
	   (check-arguments 0 2)
	   (if (null? arguments)
	       (set! boa-constructors
		     (cons (list option (symbol-append 'make- name))
			   boa-constructors))
	       (let ((name (car arguments)))
		 (if (memq name '(#F FALSE NIL))
		     (set! default-constructor-disabled? true)
		     (set! boa-constructors
			   (cons (cons option arguments)
				 boa-constructors))))))
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
		    (parse/option option (car option) (cdr option))
		    (parse/option option option '())))
	      options)
    (let loop ((constructors (append boa-constructors keyword-constructors)))
      (if (not (null? constructors))
	  (begin
	    (let ((name (cadar constructors)))
	      (for-each (lambda (constructor)
			  (if (eq? name (cadr constructor))
			      (error "Conflicting constructor definitions"
				     (caar constructors)
				     (car constructor))))
			(cdr constructors)))
	    (loop (cdr constructors)))))
    (vector structure
	    name
	    conc-name
	    false
	    (map cdr keyword-constructors)
	    (cond ((or (not (null? boa-constructors))
		       (not (null? keyword-constructors)))
		   (map cdr boa-constructors))
		  ((not default-constructor-disabled?)
		   (list (list (symbol-append 'make- name))))
		  (else
		   '()))
	    copier-name
	    predicate-name
	    (if (eq? print-procedure default-value)
		`(,(absolute 'UNPARSER/STANDARD-METHOD) ',name)
		print-procedure)
	    type 
	    (cond ((eq? type 'STRUCTURE) 'VECTOR)
		  ((eq? type 'VECTOR) 'VECTOR)
		  ((eq? type 'LIST) 'LIST)
		  ((eq? type 'RECORD) 'RECORD)
		  (else (error "Unsupported structure type" type)))
	    (and (or (not type-seen?) named-seen?)
		 (if (eq? tag-name default-value) 'DEFAULT true))
	    (if (eq? tag-name default-value)
		name
		tag-name)
	    (if (and (eq? type 'RECORD) (not (zero? offset)))
		(error "Offset not allowed for record type structures" offset)
		offset)
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
	   (let ((type #T) (read-only? false))
	     (define (with-option-type-and-argument options receiver)
	       (if (null? (cdr options))
		   (error "DEFINE-STRUCTURE -- Argument to option not given"
			  (car options))
		   (receiver (car options) (cadr options))))
	     (let loop ((options options))
	       (if (not (null? options))
		   (begin
		     (case (car options)
		       ((TYPE)
			(set! type
			      (with-option-type-and-argument options
                                (lambda (type arg)
				  (parse/option-value symbol?
						      type
						      arg
						      true)))))
		       ((READ-ONLY)
			(set! read-only?
			      (with-option-type-and-argument options
                                (lambda (type arg)
				  (parse/option-value boolean?
						      type
						      arg
						      true)))))
		       (else
			(error "Unrecognized structure slot option"
			       (car options))))
		     (loop (cddr options)))))
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
		       (VECTOR-SET! ,name ,n ,(car slots)))))
		(loop (cdr slots) (1+ n)))))
	 `(BEGIN ,@(loop slots reserved)))))

  (define-structure-refs structure 1
    name
    conc-name
    *dummy*
    keyword-constructors
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
  (string->symbol "#[defstruct-description]"))

(define slot-assoc)

(define (structure? object)
  (and (vector? object)
	   (not (zero? (vector-length object)))
	   (eq? structure (vector-ref object 0))))

(define (tag->structure tag)
  (if (structure? tag)
      tag
      (named-structure/get-tag-description tag)))

(define record-type-name-tag
  (string->symbol "#[defstruct-tag]"))

(unparser/set-tagged-vector-method! record-type-name-tag
  (lambda (state record-type-name)
    (unparse-object
     state
     (record-type-name->tag-name record-type-name))))

(define-integrable (make-record-type-name structure-descriptor)
  (vector
   record-type-name-tag
   (structure/tag-name structure-descriptor)
   structure-descriptor))

(define-integrable (record-type-name->tag-name type-name)
  (and (vector? type-name)
       (= 3 (vector-length type-name))
       (vector-second type-name)))

(define-integrable (record-type-name->structure-descriptor type-name)
  (and (vector? type-name)
       (= 3 (vector-length type-name))
       (vector-third type-name)))

(define-integrable (record-is-structure? record)
  (eq? (record-type-name->structure-descriptor record)
       record-type-name-tag))

(define (named-structure? object)
  (let ((object
	 (cond ((and (record? object) (record-is-structure? object))
		(tag->structure
		 (record-type-name->structure-descriptor
		  (record-type-name (record-type-descriptor object)))))
	       ((vector? object)
		(and (not (zero? (vector-length object)))
		     (tag->structure (vector-ref object 0))))
	       ((pair? object)
		(tag->structure (car object)))
	       (else false))))
    (or (structure? object)
	(procedure? object))))

(define (named-structure/description instance)
  (let ((structure
	 (tag->structure
	  (cond ((vector? instance) (vector-ref instance 0))
		((pair? instance) (car instance))
		((record? instance)
		 (record-type-name->structure-descriptor
		  (record-type-name (record-type-descriptor instance))))
		(else (error "Illegal structure instance" instance))))))
    (cond ((structure? structure)
	   (let ((scheme-type (structure/scheme-type structure)))
	     (if (not (case scheme-type
			((VECTOR) (vector? instance))
			((LIST) (list? instance))
			((RECORD) (record? instance))
			(else (error "Illegal structure type" scheme-type))))
		 (error "Malformed structure instance" instance))
	     (let ((accessor
		    (case scheme-type
		      ((VECTOR)
		       (lambda (instance slot)
			 (vector-ref instance (slot/index slot))))
		      ((LIST)
		       (lambda (instance slot)
			 (list-ref instance (slot/index slot))))
		      ((RECORD)
		       (lambda (instance slot)
			 ((record-accessor
			   (structure/type structure)
			   (slot/name slot))
			  instance))))))
	       (map (lambda (slot)
		      `(,(slot/name slot)
			,(accessor instance slot)))
		    (structure/slots structure)))))
	  ((procedure? structure)
	   (structure instance))
	  (else
	   (error "Illegal structure instance" instance)))))

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
	      (if (eq? (structure/scheme-type structure) 'RECORD)
		  `((DECLARE (INTEGRATE-OPERATOR ,accessor-name))
		    (DEFINE ,accessor-name
		      (,(absolute 'RECORD-ACCESSOR)
		       ,(structure/type structure)
		       ',(slot/name slot))))
		  `((DECLARE (INTEGRATE-OPERATOR ,accessor-name))
		    (DEFINE (,accessor-name STRUCTURE)
		      (DECLARE (INTEGRATE STRUCTURE))
		      ,(case (structure/scheme-type structure)
			 ((VECTOR)
			  `(,(absolute 'VECTOR-REF)
			    STRUCTURE
			    ,(slot/index slot)))
			 ((LIST)
			  `(,(absolute 'LIST-REF)
			    STRUCTURE
			    ,(slot/index slot)))
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
		  (if (eq? (structure/scheme-type structure) 'RECORD)
		      `((DECLARE (INTEGRATE-OPERATOR ,settor-name))
			(DEFINE ,settor-name
			  (,(absolute 'RECORD-UPDATER)
			   ,(structure/type structure)
			   ',(slot/name slot))))
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
			      (error "Unknown scheme type" structure)))))))))
	  (structure/slots structure)))

(define (constructor-definitions structure)
  `(,@(map (lambda (boa-constructor)
	     (if (null? (cdr boa-constructor))
		 (constructor-definition/default structure
						 (car boa-constructor))
		 (constructor-definition/boa structure
					     (car boa-constructor)
					     (cadr boa-constructor))))
	   (structure/boa-constructors structure))
    ,@(map (lambda (keyword-constructor)
	     (constructor-definition/keyword structure
					     (car keyword-constructor)))
	   (structure/keyword-constructors structure))))

(define (constructor-definition/default structure name)
  (let ((slot-names
	 (map (lambda (slot)
		(string->uninterned-symbol (symbol->string (slot/name slot))))
	      (structure/slots structure))))
    (if (eq? (structure/scheme-type structure) 'RECORD)
	`(DEFINE ,name
	   (,(absolute 'RECORD-CONSTRUCTOR)
	    ,(structure/type structure)
	    ',(map slot/name
		   (structure/slots structure))))
	`(DEFINE (,name ,@slot-names)
	   ;; *** Kludge -- SCHEME-TYPE happens to be same as constructor.
	   (,(absolute (structure/scheme-type structure))
	    ,@(constructor-prefix-slots structure)
	    ,@slot-names)))))

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
	    ((RECORD)
	     `((,(absolute 'RECORD-CONSTRUCTOR) (structure/type structure))
	       ,list-cons))
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
     (,(let ((scheme-type (structure/scheme-type structure)))
	 (if (eq? scheme-type 'RECORD)
	     ((absolute 'RECORD-CONSTRUCTOR)
	      (structure/type structure))
	     ;; *** Kludge -- SCHEME-TYPE happens to be same as constructor.
	     (absolute scheme-type)))
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
      (let ((variable (string->uninterned-symbol "object")))
	(case (structure/scheme-type structure)
	  ((VECTOR)
	   `((DEFINE (,(structure/predicate-name structure) ,variable)
	       (AND (,(absolute 'VECTOR?) ,variable)
		    (,(absolute 'NOT)
		     (,(absolute 'ZERO?)
		      (,(absolute 'VECTOR-LENGTH) ,variable)))
		    (,(absolute 'EQ?) (,(absolute 'VECTOR-REF) ,variable 0)
				      ,(structure/tag-name structure))))))
	  ((LIST)
	   `((DEFINE (,(structure/predicate-name structure) ,variable)
	       (AND (,(absolute 'PAIR?) ,variable)
		    (,(absolute 'EQ?) (,(absolute 'CAR) ,variable)
				      ,(structure/tag-name structure))))))
	  ((RECORD)
	   `((DEFINE ,(structure/predicate-name structure)
	       (,(absolute 'RECORD-PREDICATE)
		,(structure/type structure)))))
	  (else
	   (error "Unknown scheme type" structure))))
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
	     ((RECORD)
	      (error "No copiers for record type structures" structure))
	     (else
	      (error "Unknown scheme type" structure))))
	'())))

(define (print-procedure-definitions structure)
  (if (and (structure/print-procedure structure)
	   (structure/named? structure))
      (let ((scheme-type (structure/scheme-type structure)))
	`((,(absolute (case scheme-type
			((VECTOR) 'UNPARSER/SET-TAGGED-VECTOR-METHOD!)
			((LIST) 'UNPARSER/SET-TAGGED-PAIR-METHOD!)
			((RECORD) 'SET-RECORD-TYPE-UNPARSER-METHOD!)
			(else (error "Unknown scheme type" structure))))
	   ,((if (eq? scheme-type 'RECORD)
		 structure/type
		 structure/tag-name)
	     structure)
	   ,(structure/print-procedure structure))))
      '()))