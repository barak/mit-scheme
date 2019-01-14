#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019 Massachusetts Institute of Technology

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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Structure Definition Macro
;;; package: (runtime syntax defstruct)

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
  a procedure of two arguments (the structure instance and a textual output
  port) rather than three as in Common Lisp.

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

(define-syntax define-structure
  (sc-macro-transformer
   (lambda (form use-environment)
     (syntax-check '(_ + datum) form)
     (capture-syntactic-environment
      (lambda (closing-environment)
	(let ((structure
	       (receive (name options)
		   (let ((name-and-options (cadr form)))
		     (if (pair? name-and-options)
			 (values (car name-and-options) (cdr name-and-options))
			 (values name-and-options '())))
		 (if (not (symbol? name))
		     (error "Structure name must be a symbol:" name))
		 (if (not (list? options))
		     (error "Structure options must be a list:" options))
		 (let ((context
			(make-parser-context name
					     use-environment
					     closing-environment)))
		   (parse/options options
				  (parse/slot-descriptions (cddr form))
				  context)))))
	  `(begin ,@(type-definitions structure)
		  ,@(constructor-definitions structure)
		  ,@(accessor-definitions structure)
		  ,@(modifier-definitions structure)
		  ,@(predicate-definitions structure)
		  ,@(copier-definitions structure)
		  ,@(printer-definitions structure))))))))

;;;; Parse options

(define (parse/options options slots context)
  (let ((options (apply-option-transformers options context)))
    (let ((conc-name-option (find-option 'conc-name options))
	  (constructor-options (find-options 'constructor options))
	  (keyword-constructor-options
	   (find-options 'keyword-constructor options))
	  (copier-option (find-option 'copier options))
	  (predicate-option (find-option 'predicate options))
	  (print-procedure-option (find-option 'print-procedure options))
	  (type-option (find-option 'type options))
	  (type-descriptor-option (find-option 'type-descriptor options))
	  (named-option (find-option 'named options))
	  (safe-accessors-option (find-option 'safe-accessors options))
	  (initial-offset-option (find-option 'initial-offset options)))
      (check-for-duplicate-constructors constructor-options
					keyword-constructor-options)
      (let ((tagged?
	     (or (not type-option)
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
				     (default-print-method context)))
			    (if type-option
				(option/argument type-option)
				'record)
			    tagged?
			    type-name
			    (and tagged? tag-expression)
			    (and safe-accessors-option
				 (option/argument safe-accessors-option))
			    offset
			    slots)))))))

(define (find-option keyword options)
  (find (lambda (option)
	  (eq? (option/keyword option) keyword))
	options))

(define (find-options keyword options)
  (filter (lambda (option)
	    (eq? (option/keyword option) keyword))
	  options))

(define (check-for-duplicate-constructors constructor-options
					  keyword-constructor-options)
  (let loop
      ((options (append constructor-options keyword-constructor-options)))
    (if (pair? options)
	(let ((option (car options))
	      (options (cdr options)))
	  (let ((conflict
		 (let ((name (option/argument option)))
		   (and name
			(find (lambda (option*)
				(eq? (option/argument option*) name))
			      options)))))
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
	     (pair? (option/arguments named-option))
	     (not (option/argument named-option)))
	(lose named-option))
    (if initial-offset-option
	(lose initial-offset-option))))

(define (check-for-illegal-untagged predicate-option
				    print-procedure-option)
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
	 (constructors* (delete '(#f) constructors)))
    (cond ((or (pair? keyword-constructor-options)
	       (pair? constructors*))
	   constructors*)
	  ((member '(#f) constructors) '())
	  (else (list (list (default-constructor-name context)))))))

(define (compute-tagging-info type-descriptor-option named-option context)
  (let ((type-name
	 (if type-descriptor-option
	     (option/argument type-descriptor-option)
	     (default-type-name context))))
    (values type-name
	    (or (and named-option
		     (pair? (option/arguments named-option))
		     (option/argument named-option))
		type-name))))

(define (false-expression? object context)
  (or (let loop ((object object))
	(or (not object)
	    (and (syntactic-closure? object)
		 (loop (syntactic-closure-form object)))))
      (and (identifier? object)
	   (any (lambda (name)
		  (identifier=? (parser-context/use-environment context)
				object
				(parser-context/closing-environment context)
				name))
		false-expression-names))))

(define (false-marker? object)
  (or (not object)
      (memq object false-expression-names)))

(define false-expression-names
  '(false nil))

(define (true-marker? object)
  (or (eq? #t object)
      (memq object true-expression-names)))

(define true-expression-names
  '(true t))

(define (option/argument option)
  (car (option/arguments option)))

(define (default-conc-name context)
  (symbol (parser-context/name context) '-))

(define (default-constructor-name context)
  (symbol 'make- (parser-context/name context)))

(define (default-copier-name context)
  (symbol 'copy- (parser-context/name context)))

(define (default-predicate-name context)
  (symbol (parser-context/name context) '?))

(define (default-print-method context)
  `(,(absolute 'standard-print-method context)
    ',(parser-context/name context)))

(define (default-type-name context)
  (symbol 'rtd: (parser-context/name context)))

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
			    (find (let ((keyword (car normal-option)))
				    (lambda (option*)
				      (eq? (if (pair? option*)
					       (car option*)
					       option*)
					   keyword)))
				  options))))
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

(define-option 'conc-name #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(cond ((false-marker? arg) `(conc-name #f))
	      ((symbol? arg) `(conc-name ,arg))
	      (else #f))))))

(define-option 'constructor #t
  (lambda (option context)
    (two-optional-arguments option
      (lambda ()
	`(constructor ,(default-constructor-name context)))
      (lambda (arg1)
	(cond ((false-expression? arg1 context) `(constructor #f))
	      ((identifier? arg1) `(constructor ,arg1))
	      (else #f)))
      (lambda (arg1 arg2)
	(if (and (identifier? arg1) (mit-lambda-list? arg2))
	    `(constructor ,arg1 ,arg2)
	    #f)))))

(define-option 'keyword-constructor #t
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(keyword-constructor ,(default-constructor-name context)))
      (lambda (arg)
	(if (identifier? arg)
	    `(keyword-constructor ,arg)
	    #f)))))

(define-option 'copier #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(copier ,(default-copier-name context)))
      (lambda (arg)
	(cond ((false-expression? arg context) `(copier #f))
	      ((identifier? arg) `(copier ,arg))
	      (else #f))))))

(define-option 'predicate #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(predicate ,(default-predicate-name context)))
      (lambda (arg)
	(cond ((false-expression? arg context) `(predicate #f))
	      ((identifier? arg) `(predicate ,arg))
	      (else #f))))))

(define-option 'print-procedure #f
  (lambda (option context)
    (one-required-argument option
      (lambda (arg)
	`(print-procedure ,(if (false-expression? arg context) #f arg))))))

(define-option 'type #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (memq arg '(vector list))
	    `(type ,arg)
	    #f)))))

(define-option 'type-descriptor #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (identifier? arg)
	    `(type-descriptor ,arg)
	    #f)))))

(define-option 'named #f
  (lambda (option context)
    (one-optional-argument option
      (lambda ()
	`(named))
      (lambda (arg)
	`(named ,(if (false-expression? arg context) #f arg))))))

(define-option 'safe-accessors #f
  (lambda (option context)
    context
    (one-optional-argument option
      (lambda ()
	`(safe-accessors #t))
      (lambda (arg)
	(cond ((true-marker? arg) `(safe-accessors #t))
	      ((false-marker? arg) `(safe-accessors #f))
	      (else #f))))))

(define-option 'initial-offset #f
  (lambda (option context)
    context
    (one-required-argument option
      (lambda (arg)
	(if (exact-nonnegative-integer? arg)
	    `(initial-offset ,arg)
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
	       (find (lambda (slot)
		       (eq? (slot/name (car slot)) name))
		     (cdr slots))))
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
		((type)
		 (set! type
		       (cond ((true-marker? argument) #t)
			     ((symbol? argument) argument)
			     (else (error "Illegal slot option:" option)))))
		((read-only)
		 (set! read-only?
		       (cond ((false-marker? argument) #f)
			     ((true-marker? argument) #t)
			     (else (error "Illegal slot option:" option)))))
		(else
		 (error "Unrecognized structure slot option:" option))))))
	(make-slot name default type read-only?)))))

;;;; Descriptive Structure

(define-record-type <structure>
    (make-structure context conc-name constructors keyword-constructors copier
		    predicate print-procedure physical-type named?
		    type-descriptor tag-expression safe-accessors? offset slots)
    structure?
  (context structure/context)
  (conc-name structure/conc-name)
  (constructors structure/constructors)
  (keyword-constructors structure/keyword-constructors)
  (copier structure/copier)
  (predicate structure/predicate)
  (print-procedure structure/print-procedure)
  (physical-type structure/physical-type)
  (named? structure/tagged?)
  (type-descriptor structure/type-descriptor)
  (tag-expression structure/tag-expression)
  (safe-accessors? structure/safe-accessors?)
  (offset structure/offset)
  (slots structure/slots))

(define-integrable (structure/record-type? structure)
  (eq? (structure/physical-type structure) 'record))

(define-record-type <parser-context>
    (make-parser-context name use-environment closing-environment)
    parser-context?
  (name parser-context/name)
  (use-environment parser-context/use-environment)
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
  (close-syntax `(access ,name system-global-environment)
		(parser-context/closing-environment context)))

(define (close name context)
  (close-syntax name (parser-context/use-environment context)))

(define (accessor-definitions structure)
  (let ((context (structure/context structure)))
    (map (lambda (slot)
	   (let* ((name (slot/name slot))
		  (accessor-name
		   (let ((conc-name (structure/conc-name structure)))
		     (if conc-name
			 (symbol conc-name name)
			 name))))
	     (if (structure/safe-accessors? structure)
		 `(define ,accessor-name
		    (,(absolute (case (structure/physical-type structure)
				  ((record) 'record-accessor)
				  ((vector) 'define-structure/vector-accessor)
				  ((list) 'define-structure/list-accessor))
				context)
		     ,(close (structure/type-descriptor structure) context)
		     ',name))
		 `(define-integrable (,accessor-name structure)
		    (,(absolute (case (structure/physical-type structure)
				  ((record) '%record-ref)
				  ((vector) 'vector-ref)
				  ((list) 'list-ref))
				context)
		     structure
		     ,(slot/index slot))))))
	 (structure/slots structure))))

(define (modifier-definitions structure)
  (let ((context (structure/context structure)))
    (map (lambda (slot)
	   (let* ((name (slot/name slot))
		  (modifier-name
		   (let ((conc-name (structure/conc-name structure)))
		     (if conc-name
			 (symbol 'set- conc-name name '!)
			 (symbol 'set- name '!)))))
	     (if (structure/safe-accessors? structure)
		 `(define ,modifier-name
		    (,(absolute (case (structure/physical-type structure)
				  ((record) 'record-modifier)
				  ((vector) 'define-structure/vector-modifier)
				  ((list) 'define-structure/list-modifier))
				context)
		     ,(close (structure/type-descriptor structure) context)
		     ',name))
		 `(define-integrable (,modifier-name structure value)
		    ,(case (structure/physical-type structure)
		       ((record)
			`(,(absolute '%record-set! context) structure
							    ,(slot/index slot)
							    value))
		       ((vector)
			`(,(absolute 'vector-set! context) structure
							   ,(slot/index slot)
							   value))
		       ((list)
			`(,(absolute 'set-car! context)
			  (,(absolute 'list-tail context) structure
							  ,(slot/index slot))
			  value)))))))
	 (remove slot/read-only? (structure/slots structure)))))

(define (constructor-definitions structure)
  `(,@(map (lambda (constructor)
	     (constructor-definition/boa
	      structure
	      (car constructor)
	      (if (pair? (cdr constructor))
		  (cadr constructor)
		  (map slot/name (structure/slots structure)))))
	   (structure/constructors structure))
    ,@(let ((context (structure/context structure)))
	(let ((p (absolute (if (structure/record-type? structure)
			       'record-keyword-constructor
			       'define-structure/keyword-constructor)
			   context))
	      (t (close (structure/type-descriptor structure) context)))
	  (map (lambda (constructor) `(define ,(car constructor) (,p ,t)))
	       (structure/keyword-constructors structure))))))

(define (constructor-definition/boa structure name lambda-list)
  (make-constructor structure name lambda-list
    (lambda (tag-expression)
      (let ((context (structure/context structure)))
	`(,(absolute (case (structure/physical-type structure)
		       ((record) '%record)
		       ((vector) 'vector)
		       ((list) 'list))
		     context)
	  ,@(if (structure/tagged? structure) `(,tag-expression) '())
	  ,@(make-list (structure/offset structure) '#f)
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
			   (let ((name (slot/name slot))
				 (dv (default-value-expr structure slot)))
			     (cond ((or (memq slot required)
					(eq? slot rest))
				    name)
				   ((memq slot optional)
				    `(if (default-object? ,name) ,dv ,name))
				   (else dv))))
			 (structure/slots structure)))))))))))

(define (make-constructor structure name lambda-list generate-body)
  (let* ((context (structure/context structure))
	(tag-expression (close (structure/tag-expression structure) context)))
    (if (structure/record-type? structure)
	`(define ,name
	   (let ((tag ,tag-expression))
	     ,(capture-syntactic-environment
	       (lambda (environment)
		 `(named-lambda (,name ,@lambda-list)
		    ,(generate-body (close-syntax 'tag environment)))))))
	`(define (,name ,@lambda-list)
	   ,(generate-body tag-expression)))))

(define (default-value-expr structure slot)
  (let ((expression (slot/default slot)))
    ;; Scheme spends a remarkable amount of time fetching and calling
    ;; the default value procedures.  This hack eliminates that time
    ;; for many uses of DEFINE-STRUCTURE.
    (if (not (pair? (strip-syntactic-closures expression)))
	expression
	(let ((record? (structure/record-type? structure))
	      (context (structure/context structure)))
	  `(,(absolute (if record?
			   'record-type-default-value-by-index
			   'define-structure/default-value-by-index)
		       context)
	    ,(close (structure/type-descriptor structure) context)
	    ,(- (slot/index slot)
		(if record?
		    0
		    (+ (structure/offset structure)
		       (if (structure/tagged? structure) 1 0)))))))))

(define (copier-definitions structure)
  (let ((copier-name (structure/copier structure)))
    (if copier-name
	`((define ,copier-name
	    ,(absolute (case (structure/physical-type structure)
			 ((record) 'copy-record)
			 ((vector) 'vector-copy)
			 ((list) 'list-copy))
		       (structure/context structure))))
	'())))

(define (predicate-definitions structure)
  (let ((predicate-name (structure/predicate structure)))
    (if predicate-name
	(let* ((context (structure/context structure))
	       (tag-expression
		(close (structure/tag-expression structure) context))
	       (name (parser-context/name context)))
	  (case (structure/physical-type structure)
	    ((record)
	     `((define ,predicate-name
		 (,(absolute 'record-predicate context)
		  ,(close (structure/type-descriptor structure) context)))))
	    ((vector)
	     `((define (,predicate-name object)
		 (and (,(absolute 'vector? context) object)
		      (,(absolute 'not context)
		       (,(absolute 'zero? context)
			(,(absolute 'vector-length context) object)))
		      (,(absolute 'eq? context)
		       (,(absolute 'vector-ref context) object 0)
		       ,tag-expression)))
	       (,(absolute 'register-predicate! context)
		,predicate-name ',name
		'<= ,(absolute 'vector? context))))
	    ((list)
	     `((define (,predicate-name object)
		 (and (,(absolute 'pair? context) object)
		      (,(absolute 'eq? context)
		       (,(absolute 'car context) object)
		       ,tag-expression)))
	       (,(absolute 'register-predicate! context)
		,predicate-name ',name
		'<= ,(absolute 'pair? context))))))
	'())))

(define (type-definitions structure)
  (let ((type-name (structure/type-descriptor structure))
	(tag-expression (structure/tag-expression structure))
	(slots (structure/slots structure))
	(context (structure/context structure)))
    (let ((name (symbol->string (parser-context/name context)))
	  (field-names (map slot/name slots))
	  (inits
	   (map (lambda (slot)
		  (let ((default (slot/default slot)))
		    (if (false-marker? default)
			#f
			`(lambda () ,(close default context)))))
		slots)))
      `((define ,type-name
	  ,(if (structure/record-type? structure)
	       `(,(absolute 'make-record-type context)
		 ',name
		 (list ,@(map (lambda (name init)
				(if init
				    `(list ',name ,init)
				    `',name))
			      field-names
			      inits)))
	       `(,(absolute 'make-define-structure-type context)
		 ',(structure/physical-type structure)
		 ',name
		 '#(,@field-names)
		 '#(,@(map slot/index slots))
		 (vector ,@inits)
		 ;; This field was the print-procedure, no longer used.
		 ;; It should be removed after 9.3 is released.
		 #f
		 ,(if (and tag-expression
			   (not (eq? tag-expression type-name)))
		      (close tag-expression context)
		      '#f)
		 ',(+ (if (structure/tagged? structure) 1 0)
		      (structure/offset structure)
		      (length slots)))))
	,@(if (and tag-expression
		   (not (eq? tag-expression type-name)))
	      `((,(absolute 'named-structure/set-tag-description! context)
		 ,(close tag-expression context)
		 ,type-name))
	      '())))))

(define (printer-definitions structure)
  (if (and (structure/predicate structure)
	   (structure/print-procedure structure)
	   (or (structure/record-type? structure)
	       (structure/tagged? structure)))
      (let ((context (structure/context structure)))
	`((define-print-method
	    ,(close (structure/predicate structure) context)
	    ,(close (structure/print-procedure structure) context))))
      '()))