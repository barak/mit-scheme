#| -*-Scheme-*-

$Id: defstr.scm,v 14.48 2003/03/08 04:53:58 cph Exp $

Copyright 1987,1988,1989,1990,1991,1992 Massachusetts Institute of Technology
Copyright 1993,1994,1995,1996,1997,2000 Massachusetts Institute of Technology
Copyright 2001,2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

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

(define-expander 'DEFINE-STRUCTURE system-global-environment
  (lambda (form environment closing-environment)
    (if (not (and (pair? (cdr form)) (list? (cddr form))))
	(error "Ill-formed special form:" form))
    (make-syntactic-closure closing-environment '()
      (let ((name-and-options (cadr form))
	    (slot-descriptions (cddr form)))
	(let ((structure
	       (call-with-values
		   (lambda ()
		     (if (pair? name-and-options)
			 (values (car name-and-options) (cdr name-and-options))
			 (values name-and-options '())))
		 (lambda (name options)
		   (if (not (symbol? name))
		       (error "Structure name must be a symbol:" name))
		   (if (not (list? options))
		       (error "Structure options must be a list:" options))
		   (let ((context
			  (make-parser-context name
					       environment
					       closing-environment)))
		     (parse/options options
				    (parse/slot-descriptions slot-descriptions)
				    context))))))
	  `(BEGIN ,@(type-definitions structure)
		  ,@(constructor-definitions structure)
		  ,@(accessor-definitions structure)
		  ,@(modifier-definitions structure)
		  ,@(predicate-definitions structure)
		  ,@(copier-definitions structure)))))))

;;;; Parse options

(define (parse/options options slots context)
  (let ((options (apply-option-transformers options context)))
    (let ((conc-name-option (find-option 'CONC-NAME options))
	  (constructor-options (find-options 'CONSTRUCTOR options))
	  (keyword-constructor-options
	   (find-options 'KEYWORD-CONSTRUCTOR options))
	  (copier-option (find-option 'COPIER options))
	  (predicate-option (find-option 'PREDICATE options))
	  (print-procedure-option (find-option 'PRINT-PROCEDURE options))
	  (type-option (find-option 'TYPE options))
	  (type-descriptor-option (find-option 'TYPE-DESCRIPTOR options))
	  (named-option (find-option 'NAMED options))
	  (safe-accessors-option (find-option 'SAFE-ACCESSORS options))
	  (initial-offset-option (find-option 'INITIAL-OFFSET options)))
      (check-for-duplicate-constructors constructor-options
					keyword-constructor-options)
      (if (and type-descriptor-option named-option)
	  (error "Conflicting structure options:"
		 (option/original type-descriptor-option)
		 (option/original named-option)))
      (let ((tagged?
	     (or (not type-option)
		 type-descriptor-option
		 named-option))
	    (offset
	     (if initial-offset-option
		 (option/argument initial-offset-option)
		 0)))
	(if (not type-option)
	    (check-for-illegal-untyped named-option initial-offset-option))
	(if (not tagged?)
	    (check-for-illegal-untagged predicate-option
					print-procedure-option))
	(do ((slots slots (cdr slots))
	     (index (if tagged? (+ offset 1) offset) (+ index 1)))
	    ((not (pair? slots)))
	  (set-slot/index! (car slots) index))
	(call-with-values
	    (lambda ()
	      (compute-tagging-info type-descriptor-option
				    named-option
				    context))
	  (lambda (type-name tag-expression)
	    (make-structure context
			    (if conc-name-option
				(option/argument conc-name-option)
				(default-conc-name context))
			    (compute-constructors constructor-options
						  keyword-constructor-options
						  context)
			    (map option/arguments keyword-constructor-options)
			    (and copier-option (option/argument copier-option))
			    (if predicate-option
				(option/argument predicate-option)
				(and tagged? (default-predicate-name context)))
			    (if print-procedure-option
				(option/argument print-procedure-option)
				(and type-option
				     (default-unparser-text context)))
			    (if type-option
				(option/argument type-option)
				'RECORD)
			    tagged?
			    (and tagged? type-name)
			    (and tagged? tag-expression)
			    (and safe-accessors-option
				 (option/argument safe-accessors-option))
			    offset
			    slots)))))))

(define (find-option keyword options)
  (find-matching-item options
    (lambda (option)
      (eq? (option/keyword option) keyword))))

(define (find-options keyword options)
  (keep-matching-items options
    (lambda (option)
      (eq? (option/keyword option) keyword))))

(define (check-for-duplicate-constructors constructor-options
					  keyword-constructor-options)
  (let loop
      ((options (append constructor-options keyword-constructor-options)))
    (if (pair? options)
	(let ((option (car options))
	      (options (cdr options)))
	  (let ((conflict
		 (let ((name (car (option/arguments option))))
		   (and name
			(find-matching-item options
			  (lambda (option*)
			    (eq? (car (option/arguments option*))
				 name)))))))
	    (if conflict
		(error "Conflicting constructor definitions:"
		       (option/original option)
		       (option/original conflict))))
	  (loop options)))))

(define (check-for-illegal-untyped named-option initial-offset-option)
  (let ((lose
	 (lambda (option)
	   (error "Structure option illegal without TYPE option:"
		  (option/original option)))))
    (if (and named-option
	     (let ((arguments (option/arguments named-option)))
	       (and (pair? arguments)
		    (not (car arguments)))))
	(lose named-option))
    (if initial-offset-option
	(lose initial-offset-option))))

(define (check-for-illegal-untagged predicate-option print-procedure-option)
  (let ((test
	 (lambda (option)
	   (if (and option
		    (let ((arguments (option/arguments option)))
		      (and (pair? arguments)
			   (car arguments))))
	       (error "Structure option illegal for unnamed structure:"
		      (option/original option))))))
    (test predicate-option)
    (test print-procedure-option)))

(define (compute-constructors constructor-options
			      keyword-constructor-options
			      context)
  (let* ((constructors (map option/arguments constructor-options))
	 (constructors* (delete '(#F) constructors)))
    (cond ((or (pair? keyword-constructor-options)
	       (pair? constructors*))
	   constructors*)
	  ((member '(#F) constructors) '())
	  (else (list (list (default-constructor-name context)))))))

(define (compute-tagging-info type-descriptor-option named-option context)
  (let ((single (lambda (name) (values name name))))
    (cond (type-descriptor-option
	   (single (option/argument type-descriptor-option)))
	  (named-option
	   (let ((arguments (option/arguments named-option)))
	     (if (pair? arguments)
		 (values #f (car arguments))
		 (single (default-type-name context)))))
	  (else
	   (single (default-type-name context))))))

(define (false-expression? object context)
  (or (let loop ((object object))
	(or (not object)
	    (and (syntactic-closure? object)
		 (loop (syntactic-closure/form object)))))
      (and (identifier? object)
	   (there-exists? false-expression-names
	     (lambda (name)
	       (identifier=? (parser-context/environment context)
			     object
			     (parser-context/closing-environment context)
			     name))))))

(define (false-marker? object)
  (or (not object)
      (memq object false-expression-names)))

(define false-expression-names
  '(FALSE NIL))

(define (true-marker? object)
  (or (eq? #t object)
      (memq object true-expression-names)))

(define true-expression-names
  '(TRUE T))

(define (option/argument option)
  (car (option/arguments option)))

(define (default-conc-name context)
  (symbol-append (parser-context/name context) '-))

(define (default-constructor-name context)
  (symbol-append 'MAKE- (parser-context/name context)))

(define (default-copier-name context)
  (symbol-append 'COPY- (parser-context/name context)))

(define (default-predicate-name context)
  (symbol-append (parser-context/name context) '?))

(define (default-unparser-text context)
  `(,(absolute 'STANDARD-UNPARSER-METHOD context)
    ',(parser-context/name context)
    #F))

(define (default-type-name context)
  (parser-context/name context))

(define (apply-option-transformers options context)
  (let loop ((options options))
    (if (pair? options)
	(let ((option (car options))
	      (options (cdr options)))
	  (let ((lose
		 (lambda () (error "Ill-formed structure option:" option))))
	    (let ((entry
		   (assq (cond ((and (pair? option)
				     (symbol? (car option))
				     (list? (cdr option)))
				(car option))
			       ((symbol? option)
				option)
			       (else
				(lose)))
			 known-options)))
	      (if (not entry)
		  (lose))
	      (let ((normal-option (if (pair? option) option (list option)))
		    (can-be-duplicated? (cadr entry))
		    (transformer (cddr entry)))
		(let ((option*
		       (and (not can-be-duplicated?)
			    (find-matching-item options
			      (let ((keyword (car normal-option)))
				(lambda (option*)
				  (eq? (if (pair? option*)
					   (car option*)
					   option*)
				       keyword)))))))
		  (if option*
		      (error "Duplicate structure option:" option option*)))
		(cons (let ((option* (transformer normal-option context)))
			(if (not option*)
			    (lose))
			(make-option (car option*)
				     (cdr option*)
				     option))
		      (loop options))))))
	'())))

(define (define-option keyword duplicates? transformer)
  (let ((entry (assq keyword known-options))
	(tail (cons duplicates? transformer)))
    (if entry
	(set-cdr! entry tail)
	(begin
	  (set! known-options (cons (cons keyword tail) known-options))
	  unspecific))))

(define known-options '())

(define (one-required-argument option if-1)
  (case (length (cdr option))
    ((1) (if-1 (cadr option)))
    (else #f)))

(define (one-optional-argument option if-0 if-1)
  (case (length (cdr option))
    ((0) (if-0))
    ((1) (if-1 (cadr option)))
    (else #f)))

(define (two-optional-arguments option if-0 if-1 if-2)
  (case (length (cdr option))
    ((0) (if-0))
    ((1) (if-1 (cadr option)))
    ((2) (if-2 (cadr option) (caddr option)))
    (else #f)))

(define-option 'CONC-NAME #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(cond ((false-marker? arg) `(CONC-NAME #F))
	      ((symbol? arg) `(CONC-NAME ,arg))
	      (else #f))))))

(define-option 'CONSTRUCTOR #t
  (lambda (option context)
    (two-optional-arguments option
      (lambda ()
	`(CONSTRUCTOR ,(default-constructor-name context)))
      (lambda (arg1)
	(cond ((false-expression? arg1 context) `(CONSTRUCTOR #F))
	      ((identifier? arg1) `(CONSTRUCTOR ,arg1))
	      (else #f)))
      (lambda (arg1 arg2)
	(if (and (identifier? arg1) (mit-lambda-list? arg2))
	    `(CONSTRUCTOR ,arg1 ,arg2)
	    #f)))))

(define-option 'KEYWORD-CONSTRUCTOR #t
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(KEYWORD-CONSTRUCTOR ,(default-constructor-name context)))
      (lambda (arg)
	(if (identifier? arg)
	    `(KEYWORD-CONSTRUCTOR ,arg)
	    #f)))))

(define-option 'COPIER #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(COPIER ,(default-copier-name context)))
      (lambda (arg)
	(cond ((false-expression? arg context) `(COPIER #F))
	      ((identifier? arg) `(COPIER ,arg))
	      (else #f))))))

(define-option 'PREDICATE #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(PREDICATE ,(default-predicate-name context)))
      (lambda (arg)
	(cond ((false-expression? arg context) `(PREDICATE #F))
	      ((identifier? arg) `(PREDICATE ,arg))
	      (else #f))))))

(define-option 'PRINT-PROCEDURE #f
  (lambda (option context)
    (one-required-argument option
      (lambda (arg)
	`(PRINT-PROCEDURE ,(if (false-expression? arg context) #f arg))))))

(define-option 'TYPE #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (memq arg '(VECTOR LIST))
	    `(TYPE ,arg)
	    #f)))))

(define-option 'TYPE-DESCRIPTOR #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (identifier? arg)
	    `(TYPE-DESCRIPTOR ,arg)
	    #f)))))

(define-option 'NAMED #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(NAMED))
      (lambda (arg)
	`(NAMED ,(if (false-expression? arg context) #f arg))))))

(define-option 'SAFE-ACCESSORS #f
  (lambda (option context)
    context
    (one-optional-argument option
      (lambda ()
	`(SAFE-ACCESSORS #T))
      (lambda (arg)
	(cond ((true-marker? arg) `(SAFE-ACCESSORS #T))
	      ((false-marker? arg) `(SAFE-ACCESSORS #F))
	      (else #f))))))

(define-option 'INITIAL-OFFSET #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (exact-nonnegative-integer? arg)
	    `(INITIAL-OFFSET ,arg)
	    #f)))))

;;;; Parse slot descriptions

(define (parse/slot-descriptions slot-descriptions)
  (let ((slots
	 (map (lambda (description)
		(cons (parse/slot-description description)
		      description))
	      slot-descriptions)))
    (do ((slots slots (cdr slots)))
	((not (pair? slots)))
      (let ((name (slot/name (caar slots))))
	(let ((slot*
	       (find-matching-item (cdr slots)
		 (lambda (slot)
		   (eq? (slot/name (car slot)) name)))))
	  (if slot*
	      (error "Structure slots must not have duplicate names:"
		     (cdar slots)
		     (cdr slot*))))))
    (map car slots)))

(define (parse/slot-description slot-description)
  (call-with-values
      (lambda ()
	(if (pair? slot-description)
	    (if (pair? (cdr slot-description))
		(values (car slot-description)
			(cadr slot-description)
			(cddr slot-description))
		(values (car slot-description) #f '()))
	    (values slot-description #f '())))
    (lambda (name default options)
      (if (not (list? options))
	  (error "Structure slot options must be a list:" options))
      (let ((type #t)
	    (read-only? #f)
	    (options-seen '()))
	(do ((options options (cddr options)))
	    ((not (pair? options)))
	  (if (not (pair? (cdr options)))
	      (error "Missing slot option argument:" (car options)))
	  (let ((keyword (car options))
		(argument (cadr options)))
	    (let ((option (list keyword argument)))
	      (let ((previous (assq keyword options-seen)))
		(if previous
		    (error "Duplicate slot option:" previous option)))
	      (set! options-seen (cons option options-seen))
	      (case keyword
		((TYPE)
		 (set! type
		       (cond ((true-marker? argument) #t)
			     ((symbol? argument) argument)
			     (else (error "Illegal slot option:" option)))))
		((READ-ONLY)
		 (set! read-only?
		       (cond ((false-marker? argument) #f)
			     ((true-marker? argument) #t)
			     (else (error "Illegal slot option:" option)))))
		(else
		 (error "Unrecognized structure slot option:" option))))))
	(make-slot name default type read-only?)))))

(define (get-slot-default slot structure)
  (make-syntactic-closure
      (parser-context/environment (structure/context structure))
      (map slot/name (structure/slots structure))
    (slot/default slot)))

;;;; Descriptive Structure

(define-record-type <structure>
    (make-structure context conc-name constructors keyword-constructors copier
		    predicate print-procedure type named? type-descriptor
		    tag-expression safe-accessors? offset slots)
    structure?
  (context structure/context)
  (conc-name structure/conc-name)
  (constructors structure/constructors)
  (keyword-constructors structure/keyword-constructors)
  (copier structure/copier)
  (predicate structure/predicate)
  (print-procedure structure/print-procedure)
  (type structure/type)
  (named? structure/tagged?)
  (type-descriptor structure/type-descriptor)
  (tag-expression structure/tag-expression)
  (safe-accessors? structure/safe-accessors?)
  (offset structure/offset)
  (slots structure/slots))

(define-record-type <parser-context>
    (make-parser-context name environment closing-environment)
    parser-context?
  (name parser-context/name)
  (environment parser-context/environment)
  (closing-environment parser-context/closing-environment))

(define-record-type <option>
    (make-option keyword arguments original)
    option?
  (keyword option/keyword)
  (arguments option/arguments)
  (original option/original))

(define-record-type <slot>
  (make-slot name default type read-only?)
    slot?
  (name slot/name)
  (default slot/default)
  (type slot/type)
  (read-only? slot/read-only?)
  (index slot/index set-slot/index!))

(define slot-assoc
  (association-procedure eq? slot/name))

;;;; Code Generation

(define (absolute name context)
  (close-syntax `(ACCESS ,name #F)
		(parser-context/closing-environment context)))

(define (close name context)
  (close-syntax name (parser-context/environment context)))

(define (accessor-definitions structure)
  (let ((context (structure/context structure)))
    (map (lambda (slot)
	   (let* ((name (slot/name slot))
		  (accessor-name
		   (let ((conc-name (structure/conc-name structure)))
		     (if conc-name
			 (symbol-append conc-name name)
			 name))))
	     (if (structure/safe-accessors? structure)
		 `(DEFINE ,accessor-name
		    (,(absolute (case (structure/type structure)
				  ((RECORD) 'RECORD-ACCESSOR)
				  ((VECTOR) 'DEFINE-STRUCTURE/VECTOR-ACCESSOR)
				  ((LIST) 'DEFINE-STRUCTURE/LIST-ACCESSOR))
				context)
		     ,(let ((tag (structure/tag-expression structure)))
			(if tag
			    (close tag context)
			    (slot/index slot)))
		     ',name))
		 `(DEFINE-INTEGRABLE (,accessor-name STRUCTURE)
		    (,(absolute (case (structure/type structure)
				  ((RECORD) '%RECORD-REF)
				  ((VECTOR) 'VECTOR-REF)
				  ((LIST) 'LIST-REF))
				context)
		     STRUCTURE
		     ,(slot/index slot))))))
	 (structure/slots structure))))

(define (modifier-definitions structure)
  (let ((context (structure/context structure)))
    (map (lambda (slot)
	   (let* ((name (slot/name slot))
		  (modifier-name
		   (let ((conc-name (structure/conc-name structure)))
		     (if conc-name
			 (symbol-append 'SET- conc-name name '!)
			 (symbol-append 'SET- name '!)))))
	     (if (structure/safe-accessors? structure)
		 `(DEFINE ,modifier-name
		    (,(absolute (case (structure/type structure)
				  ((RECORD) 'RECORD-MODIFIER)
				  ((VECTOR) 'DEFINE-STRUCTURE/VECTOR-MODIFIER)
				  ((LIST) 'DEFINE-STRUCTURE/LIST-MODIFIER))
				context)
		     ,(let ((tag (structure/tag-expression structure)))
			(if tag
			    (close tag context)
			    (slot/index slot)))
		     ',name))
		 `(DEFINE-INTEGRABLE (,modifier-name STRUCTURE VALUE)
		    ,(case (structure/type structure)
		       ((RECORD)
			`(,(absolute '%RECORD-SET! context) STRUCTURE
							    ,(slot/index slot)
							    VALUE))
		       ((VECTOR)
			`(,(absolute 'VECTOR-SET! context) STRUCTURE
							   ,(slot/index slot)
							   VALUE))
		       ((LIST)
			`(,(absolute 'SET-CAR! context)
			  (,(absolute 'LIST-TAIL context) STRUCTURE
							  ,(slot/index slot))
			  VALUE)))))))
	 (delete-matching-items (structure/slots structure) slot/read-only?))))

(define (constructor-definitions structure)
  `(,@(map (lambda (constructor)
	     (if (pair? (cdr constructor))
		 (constructor-definition/boa structure
					     (car constructor)
					     (cadr constructor))
		 (constructor-definition/default structure (car constructor))))
	   (structure/constructors structure))
    ,@(map (lambda (constructor)
	     (constructor-definition/keyword structure (car constructor)))
	   (structure/keyword-constructors structure))))

(define (constructor-definition/default structure name)
  (let ((slot-names (map slot/name (structure/slots structure))))
    (make-constructor structure name slot-names
      (lambda (tag-expression)
	`(,(absolute (case (structure/type structure)
		       ((RECORD) '%RECORD)
		       ((VECTOR) 'VECTOR)
		       ((LIST) 'LIST))
		     (structure/context structure))
	  ,@(constructor-prefix-slots structure tag-expression)
	  ,@slot-names)))))

(define (constructor-definition/keyword structure name)
  (make-constructor structure name 'KEYWORD-LIST
    (lambda (tag-expression)
      (let ((context (structure/context structure)))
	(let ((list-cons
	       `(,@(constructor-prefix-slots structure tag-expression)
		 (,(absolute 'DEFINE-STRUCTURE/KEYWORD-PARSER context)
		  KEYWORD-LIST
		  (,(absolute 'LIST context)
		   ,@(map (lambda (slot)
			    `(,(absolute 'CONS context)
			      ',(slot/name slot)
			      ,(get-slot-default slot structure)))
			  (structure/slots structure)))))))
	  (case (structure/type structure)
	    ((RECORD)
	     `(,(absolute 'APPLY context) ,(absolute '%RECORD context)
					  ,@list-cons))
	    ((VECTOR)
	     `(,(absolute 'APPLY context) ,(absolute 'VECTOR context)
					  ,@list-cons))
	    ((LIST)
	     `(,(absolute 'CONS* context) ,@list-cons))))))))

(define (constructor-definition/boa structure name lambda-list)
  (make-constructor structure name lambda-list
    (lambda (tag-expression)
      `(,(absolute (case (structure/type structure)
		     ((RECORD) '%RECORD)
		     ((VECTOR) 'VECTOR)
		     ((LIST) 'LIST))
		   (structure/context structure))
	,@(constructor-prefix-slots structure tag-expression)
	,@(call-with-values (lambda () (parse-mit-lambda-list lambda-list))
	    (lambda (required optional rest)
	      (let ((name->slot
		     (lambda (name)
		       (or (slot-assoc name (structure/slots structure))
			   (error "Not a defined structure slot:" name)))))
		(let ((required (map name->slot required))
		      (optional (map name->slot optional))
		      (rest (and rest (name->slot rest))))
		  (map (lambda (slot)
			 (cond ((or (memq slot required)
				    (eq? slot rest))
				(slot/name slot))
			       ((memq slot optional)
				`(IF (DEFAULT-OBJECT? ,(slot/name slot))
				     ,(get-slot-default slot structure)
				     ,(slot/name slot)))
			       (else
				(get-slot-default slot structure))))
		       (structure/slots structure))))))))))

(define (make-constructor structure name lambda-list generate-body)
  (let* ((context (structure/context structure))
	 (tag-expression (close (structure/tag-expression structure) context)))
    (if (eq? (structure/type structure) 'RECORD)
	(let ((tag (make-synthetic-identifier 'TAG)))
	  `(DEFINE ,name
	     (LET ((,tag (RECORD-TYPE-DISPATCH-TAG ,tag-expression)))
	       (NAMED-LAMBDA (,name ,@lambda-list)
		 ,(generate-body tag)))))
	`(DEFINE (,name ,@lambda-list)
	   ,(generate-body tag-expression)))))

(define (constructor-prefix-slots structure tag-expression)
  (let ((offsets (make-list (structure/offset structure) '#F)))
    (if (structure/tagged? structure)
	(cons tag-expression offsets)
	offsets)))

(define (copier-definitions structure)
  (let ((copier-name (structure/copier structure)))
    (if copier-name
	`((DEFINE ,copier-name
	    ,(absolute (case (structure/type structure)
			 ((RECORD) 'COPY-RECORD)
			 ((VECTOR) 'VECTOR-COPY)
			 ((LIST) 'LIST-COPY))
		       (structure/context structure))))
	'())))

(define (predicate-definitions structure)
  (let ((predicate-name (structure/predicate structure)))
    (if predicate-name
	(let* ((context (structure/context structure))
	       (tag-expression
		(close (structure/tag-expression structure) context)))
	  (case (structure/type structure)
	    ((RECORD)
	     `((DEFINE ,predicate-name
		 (LET ((TAG (RECORD-TYPE-DISPATCH-TAG ,tag-expression)))
		   (NAMED-LAMBDA (,predicate-name OBJECT)
		     (AND (,(absolute '%RECORD? context) OBJECT)
			  (,(absolute 'EQ? context)
			   (,(absolute '%RECORD-REF context) OBJECT 0)
			   TAG)))))))
	    ((VECTOR)
	     `((DEFINE (,predicate-name OBJECT)
		 (AND (,(absolute 'VECTOR? context) OBJECT)
		      (,(absolute 'NOT context)
		       (,(absolute 'ZERO? context)
			(,(absolute 'VECTOR-LENGTH context) OBJECT)))
		      (,(absolute 'EQ? context)
		       (,(absolute 'VECTOR-REF context) OBJECT 0)
		       ,tag-expression)))))
	    ((LIST)
	     `((DEFINE (,predicate-name OBJECT)
		 (AND (,(absolute 'PAIR? context) OBJECT)
		      (,(absolute 'EQ? context)
		       (,(absolute 'CAR context) OBJECT)
		       ,tag-expression)))))))
	'())))

(define (type-definitions structure)
  (if (structure/tagged? structure)
      (let ((type (structure/type structure))
	    (type-name (structure/type-descriptor structure))
	    (name
	     (symbol->string
	      (parser-context/name (structure/context structure))))
	    (field-names (map slot/name (structure/slots structure)))
	    (context (structure/context structure)))
	(if (eq? type 'RECORD)
	    `((DEFINE ,type-name
		(,(absolute 'MAKE-RECORD-TYPE context) ',name ',field-names))
	      ,@(let ((expression (structure/print-procedure structure)))
		  (if expression
		      `((,(absolute 'SET-RECORD-TYPE-UNPARSER-METHOD! context)
			 ,type-name
			 ,(close expression context)))
		      `())))
	    (let ((type-expression
		   `(,(absolute 'MAKE-DEFINE-STRUCTURE-TYPE context)
		     ',type
		     ',name
		     ',field-names
		     ',(map slot/index (structure/slots structure))
		     ,(close (structure/print-procedure structure) context))))
	      (if type-name
		  `((DEFINE ,type-name ,type-expression))
		  `((,(absolute 'NAMED-STRUCTURE/SET-TAG-DESCRIPTION! context)
		     ,(close (structure/tag-expression structure) context)
		     ,type-expression))))))
      '()))