#| -*-Scheme-*-

$Id: defstr.scm,v 14.37 2002/01/12 02:56:14 cph Exp $

Copyright (c) 1988-1999, 2001, 2002 Massachusetts Institute of Technology

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.
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

(define-syntax define-structure
  (non-hygienic-macro-transformer
   (lambda (name-and-options . slot-descriptions)
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
	       ,@(copier-definitions structure))))))

;;;; Parse Options

;; These two names are separated to cross-syntaxing from #F=='() to
;; #F != '()

(define names-meaning-false
  '(#F FALSE NIL))

(define (make-default-defstruct-unparser-text name)
  `(,(absolute 'STANDARD-UNPARSER-METHOD)
    ',name
    #F))

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
	(print-procedure default)
	(type 'RECORD)
	(type-name name)
	(tag-expression name)
	(safe-accessors? #f)
	(offset 0)
	(options-seen '()))
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
		    (cond ((memq argument names-meaning-false) false)
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
			(if (memq name names-meaning-false)
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
			(and (not (memq (car arguments) names-meaning-false))
			     (car arguments))))
		 ((TYPE)
		  (check-duplicate)
		  (check-argument)
		  (if (not (memq (car arguments) '(VECTOR LIST)))
		      (error "Illegal structure option:" option))
		  (set! type (car arguments)))
		 ((TYPE-DESCRIPTOR)
		  (check-duplicate)
		  (check-argument)
		  (set! type-name (car arguments))
		  (set! tag-expression type-name))
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
		 ((SAFE-ACCESSORS)
		  (check-duplicate)
		  (check-arguments 1)
		  (set! safe-accessors?
			(if (null? arguments) #t (car arguments))))
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
	  (type-descriptor-seen? (assq 'TYPE-DESCRIPTOR options-seen))
	  (named-seen? (assq 'NAMED options-seen)))
      (if (and type-descriptor-seen? named-seen?)
	  (error "Conflicting options:" type-descriptor-seen? named-seen?))
      (let ((named? (or (not type-seen?) type-descriptor-seen? named-seen?)))
	(if (not type-seen?)
	    (let ((check-option
		   (lambda (seen?)
		     (if seen?
			 (error "Structure option illegal without TYPE option:"
				(cdr seen?))))))
	      (check-option (and (not type-name) named-seen?))
	      (check-option (assq 'INITIAL-OFFSET options-seen))))
	(if (not named?)
	    (let ((check
		   (lambda (option-seen)
		     (if option-seen
			 (error
			  "Structure option illegal for unnamed structure:"
			  (cdr option-seen))))))
	      (if predicate-name
		  (check (assq 'PREDICATE options-seen)))
	      (if (and (not (eq? print-procedure default)) print-procedure)
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
			(and named?
			     (cond ((not (eq? print-procedure default))
				    print-procedure)
				   ((eq? type 'RECORD)
				    false)
				   (else
				    (make-default-defstruct-unparser-text
				     name))))
			type
			named?
			(and named? type-name)
			(and named? tag-expression)
			safe-accessors?
			offset
			slots)))))

(define default
  (list 'DEFAULT))

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
	  (error "Structure slot options must be a list:" options))
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
		       (cond ((memq argument names-meaning-false) false)
			     ((memq argument '(#T TRUE T)) true)
			     (else (error "Illegal slot option:" option))))))
	      (else
	       (error "Unrecognized structure slot option:" option)))))
	(make-slot name default type read-only?)))))

;;;; Descriptive Structure

(define structure-rtd
  (make-record-type
   "structure"
   '(NAME CONC-NAME KEYWORD-CONSTRUCTORS BOA-CONSTRUCTORS COPIER-NAME
	  PREDICATE-NAME PRINT-PROCEDURE TYPE NAMED? TYPE-NAME
	  TAG-EXPRESSION SAFE-ACCESSORS? OFFSET SLOTS)))

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

(define structure/safe-accessors?
  (record-accessor structure-rtd 'SAFE-ACCESSORS?))

(define structure/offset
  (record-accessor structure-rtd 'OFFSET))

(define structure/slots
  (record-accessor structure-rtd 'SLOTS))

(define slot-rtd
  (make-record-type "slot" '(NAME DEFAULT TYPE READ-ONLY? INDEX)))

(define make-slot
  (record-constructor slot-rtd '(NAME DEFAULT TYPE READ-ONLY?)))

(define slot/name
  (record-accessor slot-rtd 'NAME))

(define slot/default
  (record-accessor slot-rtd 'DEFAULT))

(define slot/type
  (record-accessor slot-rtd 'TYPE))

(define slot/read-only?
  (record-accessor slot-rtd 'READ-ONLY?))

(define slot/index
  (record-accessor slot-rtd 'INDEX))

(define set-slot/index!
  (record-modifier slot-rtd 'INDEX))

(define slot-assoc
  (association-procedure eq? slot/name))

;;;; Code Generation

(define (absolute name)
  `(ACCESS ,name #F))

(define (accessor-definitions structure)
  (map (lambda (slot)
	 (let* ((name (slot/name slot))
		(accessor-name
		 (if (structure/conc-name structure)
		     (symbol-append (structure/conc-name structure) name)
		     name)))
	   (if (structure/safe-accessors? structure)
	       `(DEFINE ,accessor-name
		  (,(absolute
		     (case (structure/type structure)
		       ((RECORD) 'RECORD-ACCESSOR)
		       ((VECTOR) 'DEFINE-STRUCTURE/VECTOR-ACCESSOR)
		       ((LIST) 'DEFINE-STRUCTURE/LIST-ACCESSOR)))
		   ,(or (structure/tag-expression structure)
			(slot/index slot))
		   ',name))
	       `(DEFINE-INTEGRABLE (,accessor-name STRUCTURE)
		  (,(absolute
		     (case (structure/type structure)
		       ((RECORD) '%RECORD-REF)
		       ((VECTOR) 'VECTOR-REF)
		       ((LIST) 'LIST-REF)))
		   STRUCTURE
		   ,(slot/index slot))))))
       (structure/slots structure)))

(define (modifier-definitions structure)
  (append-map!
   (lambda (slot)
     (if (slot/read-only? slot)
	 '()
	 (list
	  (let* ((name (slot/name slot))
		 (modifier-name
		  (if (structure/conc-name structure)
		      (symbol-append 'SET-
				     (structure/conc-name structure)
				     name
				     '!)
		      (symbol-append 'SET- name '!))))
	    (if (structure/safe-accessors? structure)
		`(DEFINE ,modifier-name
		   (,(absolute
		      (case (structure/type structure)
			((RECORD) 'RECORD-MODIFIER)
			((VECTOR) 'DEFINE-STRUCTURE/VECTOR-MODIFIER)
			((LIST) 'DEFINE-STRUCTURE/LIST-MODIFIER)))
		    ,(or (structure/tag-expression structure)
			 (slot/index slot))
		    ',name))
		`(DEFINE-INTEGRABLE (,modifier-name STRUCTURE VALUE)
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
			 VALUE)))))))))
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
    (make-constructor structure name slot-names
      (lambda (tag-expression)
	`(,(absolute
	    (case (structure/type structure)
	      ((RECORD) '%RECORD)
	      ((VECTOR) 'VECTOR)
	      ((LIST) 'LIST)))
	  ,@(constructor-prefix-slots structure tag-expression)
	  ,@slot-names)))))

(define (constructor-definition/keyword structure name)
  (let ((keyword-list (string->uninterned-symbol "keyword-list")))
    (make-constructor structure name keyword-list
      (lambda (tag-expression)
	(let ((list-cons
	       `(,@(constructor-prefix-slots structure tag-expression)
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
	     `(,(absolute 'CONS*) ,@list-cons))))))))

(define (constructor-definition/boa structure name lambda-list)
  (make-constructor structure name lambda-list
    (lambda (tag-expression)
      `(,(absolute
	  (case (structure/type structure)
	    ((RECORD) '%RECORD)
	    ((VECTOR) 'VECTOR)
	    ((LIST) 'LIST)))
	,@(constructor-prefix-slots structure tag-expression)
	,@(parse-lambda-list lambda-list
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
				     ,(slot/default slot)
				     ,(slot/name slot)))
			       (else
				(slot/default slot))))
		       (structure/slots structure))))))))))

(define (make-constructor structure name arguments generate-body)
  (let ((tag-expression (structure/tag-expression structure)))
    (if (eq? (structure/type structure) 'RECORD)
	(let ((tag (generate-uninterned-symbol 'TAG-)))
	  `(DEFINE ,name
	     (LET ((,tag (RECORD-TYPE-DISPATCH-TAG ,tag-expression)))
	       (NAMED-LAMBDA (,name ,@arguments)
		 ,(generate-body tag)))))
	`(DEFINE (,name ,@arguments)
	   ,(generate-body tag-expression)))))

(define (constructor-prefix-slots structure tag-expression)
  (let ((offsets (make-list (structure/offset structure) false)))
    (if (structure/named? structure)
	(cons tag-expression offsets)
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
	  (case (structure/type structure)
	    ((RECORD)
	     (let ((tag (generate-uninterned-symbol 'TAG-)))
	       `((DEFINE ,predicate-name
		   (LET ((,tag (RECORD-TYPE-DISPATCH-TAG ,tag-expression)))
		     (NAMED-LAMBDA (,predicate-name ,variable)
		       (AND (,(absolute '%RECORD?) ,variable)
			    (,(absolute 'EQ?)
			     (,(absolute '%RECORD-REF) ,variable 0)
			     ,tag))))))))
	    ((VECTOR)
	     `((DEFINE (,predicate-name ,variable)
		 (AND (,(absolute 'VECTOR?) ,variable)
		      (,(absolute 'NOT)
		       (,(absolute 'ZERO?)
			(,(absolute 'VECTOR-LENGTH) ,variable)))
		      (,(absolute 'EQ?) (,(absolute 'VECTOR-REF) ,variable 0)
					,tag-expression)))))
	    ((LIST)
	     `((DEFINE (,predicate-name ,variable)
		 (AND (,(absolute 'PAIR?) ,variable)
		      (,(absolute 'EQ?) (,(absolute 'CAR) ,variable)
					,tag-expression)))))))
	'())))

(define (type-definitions structure)
  (if (structure/named? structure)
      (let ((type (structure/type structure))
	    (type-name (structure/type-name structure))
	    (name (symbol->string (structure/name structure)))
	    (field-names (map slot/name (structure/slots structure))))
	(if (eq? type 'RECORD)
	    `((DEFINE ,type-name
		(,(absolute 'MAKE-RECORD-TYPE)
		 ',name ',field-names
		 ,@(let ((print-procedure
			  (structure/print-procedure structure)))
		     (if (not print-procedure)
			 `()
			 `(,print-procedure))))))
	    (let ((type-expression
		   `(,(absolute 'MAKE-DEFINE-STRUCTURE-TYPE)
		     ',type
		     ',name
		     ',field-names
		     ',(map slot/index (structure/slots structure))
		     ,(structure/print-procedure structure))))
	      (if type-name
		  `((DEFINE ,type-name ,type-expression))
		  `((DEFINE ,(string->uninterned-symbol name)
		      (NAMED-STRUCTURE/SET-TAG-DESCRIPTION!
		       ,(structure/tag-expression structure)
		       ,type-expression)))))))
      '()))