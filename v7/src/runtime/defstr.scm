#| -*-Scheme-*-

$Id: defstr.scm,v 14.20 1992/12/07 19:06:41 cph Exp $

Copyright (c) 1988-1992 Massachusetts Institute of Technology

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
  same order as specified in the definition of the structure.  A
  keyword constructor may be specified by giving the option
  KEYWORD-CONSTRUCTOR.

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
  that depends on the Common Lisp package system to help generate
  unique tags; Scheme has no such way of generating unique symbols.

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
    (let ((structure
	   (with-values
	       (lambda ()
		 (if (pair? name-and-options)
		     (values (car name-and-options) (cdr name-and-options))
		     (values name-and-options '())))
	     (lambda (name options)
	       (parse/options name
			      options
			      (map parse/slot-description
				   slot-descriptions))))))
      (do ((slots (structure/slots structure) (cdr slots))
	   (index (if (structure/named? structure)
		      (+ (structure/offset structure) 1)
		      (structure/offset structure))
		  (+ index 1)))
	  ((null? slots))
	(set-slot/index! (car slots) index))
      `(BEGIN ,@(type-definitions structure)
	      ,@(constructor-definitions structure)
	      ,@(accessor-definitions structure)
	      ,@(modifier-definitions structure)
	      ,@(predicate-definitions structure)
	      ,@(copier-definitions structure)
	      ,@(print-procedure-definitions structure)
	      ',(structure/name structure)))))

;;;; Parse Options

(define (parse/options name options slots)
  (if (not (symbol? name))
      (error "Structure name must be a symbol:" name))
  (if (not (list? options))
      (error "Structure options must be a list:" options))
  (let ((conc-name (symbol-append name '-))
	(default-constructor-disabled? false)
	(boa-constructors '())
	(keyword-constructors '())
	(copier-name false)
	(predicate-name (symbol-append name '?))
	(print-procedure `(,(absolute 'UNPARSER/STANDARD-METHOD) ',name))
	(type 'RECORD)
	(type-name name)
	(tag-expression)
	(offset 0)
	(options-seen '()))
    (set! tag-expression type-name)
    (for-each
     (lambda (option)
       (if (not (or (symbol? option)
		    (and (pair? option)
			 (symbol? (car option))
			 (list? (cdr option)))))
	   (error "Ill-formed structure option:" option))
       (with-values
	   (lambda ()
	     (if (pair? option)
		 (values (car option) (cdr option))
		 (values option '())))
	 (lambda (keyword arguments)
	   (set! options-seen (cons (cons keyword option) options-seen))
	   (let ((n-arguments (length arguments))
		 (check-duplicate
		  (lambda ()
		    (let ((previous (assq keyword (cdr options-seen))))
		      (if previous
			  (error "Duplicate structure option:"
				 previous option)))))
		 (symbol-option
		  (lambda (argument)
		    (cond ((memq argument '(#F FALSE NIL)) false)
			  ((symbol? argument) argument)
			  (else (error "Illegal structure option:" option))))))
	     (let ((check-argument
		    (lambda ()
		      (if (not (= n-arguments 1))
			  (error
			   (if (= n-arguments 0)
			       "Structure option requires an argument:"
			       "Structure option accepts at most 1 argument:")
			   option))))
		   (check-arguments
		    (lambda (max)
		      (if (> n-arguments max)
			  (error (string-append
				  "Structure option accepts at most "
				  (number->string max)
				  " arguments:")
				 option)))))
	       (case keyword
		 ((CONC-NAME)
		  (check-duplicate)
		  (check-argument)
		  (set! conc-name (symbol-option (car arguments))))
		 ((CONSTRUCTOR)
		  (check-arguments 2)
		  (if (null? arguments)
		      (set! boa-constructors
			    (cons (list option (symbol-append 'MAKE- name))
				  boa-constructors))
		      (let ((name (car arguments)))
			(if (memq name '(#F FALSE NIL))
			    (set! default-constructor-disabled? true)
			    (set! boa-constructors
				  (cons (cons option arguments)
					boa-constructors))))))
		 ((KEYWORD-CONSTRUCTOR)
		  (check-arguments 1)
		  (set! keyword-constructors
			(cons (list option
				    (if (null? arguments)
					(symbol-append 'MAKE- name)
					(car arguments)))
			      keyword-constructors)))
		 ((COPIER)
		  (check-duplicate)
		  (check-arguments 1)
		  (set! copier-name
			(if (null? arguments)
			    (symbol-append 'COPY- name)
			    (symbol-option (car arguments)))))
		 ((PREDICATE)
		  (check-duplicate)
		  (check-arguments 1)
		  (set! predicate-name
			(if (null? arguments)
			    (symbol-append name '?)
			    (symbol-option (car arguments)))))
		 ((PRINT-PROCEDURE)
		  (check-duplicate)
		  (check-argument)
		  (set! print-procedure
			(and (not (memq (car arguments) '(#F FALSE NIL)))
			     (car arguments))))
		 ((TYPE)
		  (check-duplicate)
		  (check-argument)
		  (if (not (memq (car arguments) '(VECTOR LIST)))
		      (error "Illegal structure option:" option))
		  (set! type (car arguments)))
		 ((NAMED)
		  (check-duplicate)
		  (check-arguments 1)
		  (if (null? arguments)
		      (begin
			(set! type-name name)
			(set! tag-expression type-name))
		      (begin
			(set! type-name false)
			(set! tag-expression (car arguments)))))
		 ((INITIAL-OFFSET)
		  (check-duplicate)
		  (check-argument)
		  (if (not (exact-nonnegative-integer? (car arguments)))
		      (error "Illegal structure option:" option))
		  (set! offset (car arguments)))
		 (else
		  (error "Unknown structure option:" option))))))))
     options)
    (let loop ((constructors (append boa-constructors keyword-constructors)))
      (if (not (null? constructors))
	  (begin
	    (let ((name (cadar constructors)))
	      (for-each (lambda (constructor)
			  (if (eq? name (cadr constructor))
			      (error "Conflicting constructor definitions:"
				     (caar constructors)
				     (car constructor))))
			(cdr constructors)))
	    (loop (cdr constructors)))))
    (let ((type-seen? (assq 'TYPE options-seen))
	  (named-seen? (assq 'NAMED options-seen)))
      (let ((named? (or (not type-seen?) named-seen?)))
	(if (not type-seen?)
	    (begin
	      (if (and named-seen? (not type-name))
		  (error "Illegal structure option:" (cdr named-seen?)))
	      (let ((initial-offset-seen? (assq 'INITIAL-OFFSET options-seen)))
		(if initial-offset-seen?
		    (error "Structure option illegal without TYPE option:"
			   (cdr initial-offset-seen?))))))
	(if (not named?)
	    (let ((check
		   (lambda (option-seen)
		     (if option-seen
			 (error
			  "Structure option illegal for unnamed structure:"
			  (cdr option-seen))))))
	      (if predicate-name
		  (check (assq 'PREDICATE options-seen)))
	      (if print-procedure
		  (check (assq 'PRINT-PROCEDURE options-seen)))))
	(make-structure name
			conc-name
			(map cdr keyword-constructors)
			(cond ((or (not (null? boa-constructors))
				   (not (null? keyword-constructors)))
			       (map cdr boa-constructors))
			      ((not default-constructor-disabled?)
			       (list (list (symbol-append 'MAKE- name))))
			      (else
			       '()))
			copier-name
			(and named? predicate-name)
			(and named? print-procedure)
			type
			named?
			(and named? type-name)
			(and named? tag-expression)
			offset
			slots)))))

;;;; Parse Slot-Descriptions

(define (parse/slot-description slot-description)
  (with-values
      (lambda ()
	(if (pair? slot-description)
	    (if (pair? (cdr slot-description))
		(values (car slot-description)
			(cadr slot-description)
			(cddr slot-description))
		(values (car slot-description) false '()))
	    (values slot-description false '())))
    (lambda (name default options)
      (if (not (list? options))
	  (error "Structure slot options must be a list" options))
      (let ((type true)
	    (read-only? false)
	    (options-seen '()))
	(do ((options options (cddr options)))
	    ((null? options))
	  (if (null? (cdr options))
	      (error "Missing slot option argument:" (car options)))
	  (let ((previous (assq (car options) options-seen))
		(option (list (car options) (cadr options))))
	    (if previous
		(error "Duplicate slot option:" previous option))
	    (set! options-seen (cons option options-seen))
	    (case (car options)
	      ((TYPE)
	       (set! type
		     (let ((argument (cadr options)))
		       (cond ((memq argument '(#T TRUE T)) true)
			     ((symbol? argument) argument)
			     (else (error "Illegal slot option:" option))))))
	      ((READ-ONLY)
	       (set! read-only?
		     (let ((argument (cadr options)))
		       (cond ((memq argument '(#F FALSE NIL)) false)
			     ((memq argument '(#T TRUE T)) true)
			     (else (error "Illegal slot option:" option))))))
	      (else
	       (error "Unrecognized structure slot option:" option)))))
	(make-slot name default type read-only?)))))

;;;; Descriptive Structure

(define structure-rtd
  (make-record-type "structure"
		    '(NAME
		      CONC-NAME
		      KEYWORD-CONSTRUCTORS
		      BOA-CONSTRUCTORS
		      COPIER-NAME
		      PREDICATE-NAME
		      PRINT-PROCEDURE
		      TYPE
		      NAMED?
		      TYPE-NAME
		      TAG-EXPRESSION
		      OFFSET
		      SLOTS)))

(define make-structure
  (record-constructor structure-rtd))

(define structure?
  (record-predicate structure-rtd))

(define structure/name
  (record-accessor structure-rtd 'NAME))

(define structure/conc-name
  (record-accessor structure-rtd 'CONC-NAME))

(define structure/keyword-constructors
  (record-accessor structure-rtd 'KEYWORD-CONSTRUCTORS))

(define structure/boa-constructors
  (record-accessor structure-rtd 'BOA-CONSTRUCTORS))

(define structure/copier-name
  (record-accessor structure-rtd 'COPIER-NAME))

(define structure/predicate-name
  (record-accessor structure-rtd 'PREDICATE-NAME))

(define structure/print-procedure
  (record-accessor structure-rtd 'PRINT-PROCEDURE))

(define structure/type
  (record-accessor structure-rtd 'TYPE))

(define structure/named?
  (record-accessor structure-rtd 'NAMED?))

(define structure/type-name
  (record-accessor structure-rtd 'TYPE-NAME))

(define structure/tag-expression
  (record-accessor structure-rtd 'TAG-EXPRESSION))

(define structure/offset
  (record-accessor structure-rtd 'OFFSET))

(define structure/slots
  (record-accessor structure-rtd 'SLOTS))

(define slot-rtd
  (make-record-type "slot" '(NAME DEFAULT TYPE READ-ONLY? INDEX)))

(define make-slot
  (record-constructor slot-rtd '(NAME DEFAULT TYPE READ-ONLY?)))

(define slot/name (record-accessor slot-rtd 'NAME))
(define slot/default (record-accessor slot-rtd 'DEFAULT))
(define slot/type (record-accessor slot-rtd 'TYPE))
(define slot/read-only? (record-accessor slot-rtd 'READ-ONLY?))
(define slot/index (record-accessor slot-rtd 'INDEX))
(define set-slot/index! (record-modifier slot-rtd 'INDEX))

(define slot-assoc)

;;;; Code Generation

(define (absolute name)
  `(ACCESS ,name #F))

(define (accessor-definitions structure)
  (map (lambda (slot)
	 `(DEFINE-INTEGRABLE
	    (,(if (structure/conc-name structure)
		  (symbol-append (structure/conc-name structure)
				 (slot/name slot))
		  (slot/name slot))
	     STRUCTURE)
	    (,(absolute
	       (case (structure/type structure)
		 ((RECORD) '%RECORD-REF)
		 ((VECTOR) 'VECTOR-REF)
		 ((LIST) 'LIST-REF)))
	     STRUCTURE
	     ,(slot/index slot))))
       (structure/slots structure)))

(define (modifier-definitions structure)
  (append-map! (lambda (slot)
		 (if (slot/read-only? slot)
		     '()
		     `((DEFINE-INTEGRABLE
			 (,(if (structure/conc-name structure)
			       (symbol-append 'SET-
					      (structure/conc-name structure)
					      (slot/name slot)
					      '!)
			       (symbol-append 'SET- (slot/name slot) '!))
			  STRUCTURE
			  VALUE)
			 ,(case (structure/type structure)
			    ((RECORD)
			     `(,(absolute '%RECORD-SET!) STRUCTURE
							 ,(slot/index slot)
							 VALUE))
			    ((VECTOR)
			     `(,(absolute 'VECTOR-SET!) STRUCTURE
							,(slot/index slot)
							VALUE))
			    ((LIST)
			     `(,(absolute 'SET-CAR!)
			       (,(absolute 'LIST-TAIL) STRUCTURE
						       ,(slot/index slot))
			       VALUE)))))))
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
    `(DEFINE (,name ,@slot-names)
       (,(absolute
	  (case (structure/type structure)
	    ((RECORD) '%RECORD)
	    ((VECTOR) 'VECTOR)
	    ((LIST) 'LIST)))
	,@(constructor-prefix-slots structure)
	,@slot-names))))

(define (constructor-definition/keyword structure name)
  (let ((keyword-list (string->uninterned-symbol "keyword-list")))
    `(DEFINE (,name . ,keyword-list)
       ,(let ((list-cons
	       `(,@(constructor-prefix-slots structure)
		 (,(absolute 'DEFINE-STRUCTURE/KEYWORD-PARSER)
		  ,keyword-list
		  (,(absolute 'LIST)
		   ,@(map (lambda (slot)
			    `(,(absolute 'CONS) ',(slot/name slot)
						,(slot/default slot)))
			  (structure/slots structure)))))))
	  (case (structure/type structure)
	    ((RECORD)
	     `(,(absolute 'APPLY) ,(absolute '%RECORD) ,@list-cons))
	    ((VECTOR)
	     `(,(absolute 'APPLY) ,(absolute 'VECTOR) ,@list-cons))
	    ((LIST)
	     `(,(absolute 'CONS*) ,@list-cons)))))))

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
     (,(absolute
	(case (structure/type structure)
	  ((RECORD) '%RECORD)
	  ((VECTOR) 'VECTOR)
	  ((LIST) 'LIST)))
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
	(cons (structure/tag-expression structure) offsets)
	offsets)))

(define (copier-definitions structure)
  (let ((copier-name (structure/copier-name structure)))
    (if copier-name
	`((DEFINE ,copier-name
	    ,(absolute
	      (case (structure/type structure)
		((RECORD) 'RECORD-COPY)
		((VECTOR) 'VECTOR-COPY)
		((LIST) 'LIST-COPY)))))
	'())))

(define (predicate-definitions structure)
  (let ((predicate-name (structure/predicate-name structure)))
    (if predicate-name
	(let ((tag-expression (structure/tag-expression structure))
	      (variable (string->uninterned-symbol "object")))
	  `((DEFINE (,predicate-name ,variable)
	      ,(case (structure/type structure)
		 ((RECORD)
		  `(AND (,(absolute '%RECORD?) ,variable)
			(,(absolute 'EQ?)
			 (,(absolute '%RECORD-REF) ,variable 0)
			 ,tag-expression)))
		 ((VECTOR)
		  `(AND (,(absolute 'VECTOR?) ,variable)
			(,(absolute 'NOT)
			 (,(absolute 'ZERO?)
			  (,(absolute 'VECTOR-LENGTH) ,variable)))
			(,(absolute 'EQ?) (,(absolute 'VECTOR-REF) ,variable 0)
					  ,tag-expression)))
		 ((LIST)
		  `(AND (,(absolute 'PAIR?) ,variable)
			(,(absolute 'EQ?) (,(absolute 'CAR) ,variable)
					  ,tag-expression)))))))
	'())))

(define (print-procedure-definitions structure)
  (let ((print-procedure (structure/print-procedure structure)))
    (if (and print-procedure (eq? (structure/type structure) 'RECORD))
	`((,(absolute 'SET-RECORD-TYPE-UNPARSER-METHOD!)
	   ,(structure/type-name structure)
	   ,print-procedure))
	'())))

(define (type-definitions structure)
  (if (structure/named? structure)
      (let ((type (structure/type structure))
	    (type-name (structure/type-name structure))
	    (name (symbol->string (structure/name structure)))
	    (field-names (map slot/name (structure/slots structure))))
	(if (eq? type 'RECORD)
	    `((DEFINE ,type-name
		(,(absolute 'MAKE-RECORD-TYPE) ',name ',field-names)))
	    (let ((type-expression
		   `(,(absolute 'MAKE-DEFINE-STRUCTURE-TYPE)
		     ',type
		     ',name
		     ',field-names
		     ',(map slot/index (structure/slots structure))
		     ,(structure/print-procedure structure))))
	      (if type-name
		  `((DEFINE ,type-name ,type-expression))
		  `((NAMED-STRUCTURE/SET-TAG-DESCRIPTION!
		     ,(structure/tag-expression structure)
		     ,type-expression))))))
      '()))

(define structure-type-rtd
  (make-record-type "structure-type"
		    '(TYPE NAME FIELD-NAMES FIELD-INDEXES UNPARSER-METHOD)))

(define make-define-structure-type
  (record-constructor structure-type-rtd))

(define structure-type?
  (record-predicate structure-type-rtd))

(define structure-type/type
  (record-accessor structure-type-rtd 'TYPE))

(define structure-type/name
  (record-accessor structure-type-rtd 'NAME))

(define structure-type/field-names
  (record-accessor structure-type-rtd 'FIELD-NAMES))

(define structure-type/field-indexes
  (record-accessor structure-type-rtd 'FIELD-INDEXES))

(define structure-type/unparser-method
  (record-accessor structure-type-rtd 'UNPARSER-METHOD))

(define set-structure-type/unparser-method!
  (record-modifier structure-type-rtd 'UNPARSER-METHOD))

(define (structure-tag/unparser-method tag type)
  (let ((structure-type (tag->structure-type tag type)))
    (and structure-type
	 (structure-type/unparser-method structure-type))))

(define (named-structure? object)
  (cond ((record? object)
	 true)
	((vector? object)
	 (and (not (zero? (vector-length object)))
	      (tag->structure-type (vector-ref object 0) 'VECTOR)))
	((pair? object)
	 (tag->structure-type (car object) 'LIST))
	(else
	 false)))

(define (named-structure/description structure)
  (cond ((record? structure)
	 (record-description structure))
	((named-structure? structure)
	 =>
	 (lambda (type)
	   (let ((accessor (if (pair? structure) list-ref vector-ref)))
	     (map (lambda (field-name index)
		    `(,field-name ,(accessor structure index)))
		  (structure-type/field-names type)
		  (structure-type/field-indexes type)))))
	(else
	 (error:wrong-type-argument structure "named structure"
				    'NAMED-STRUCTURE/DESCRIPTION))))

(define (tag->structure-type tag type)
  (if (structure-type? tag)
      (and (eq? (structure-type/type tag) type)
	   tag)
      (and (symbol? tag)
	   (let ((structure-type (named-structure/get-tag-description tag)))
	     (and (structure-type? structure-type)
		  (eq? (structure-type/type structure-type) type)
		  structure-type)))))