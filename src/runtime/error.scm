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

;;;; Error System
;;; package: (runtime error-handler)

(declare (usual-integrations))

;;;; Condition Types

(define-structure (condition-type
		   (conc-name %condition-type/)
		   (constructor %make-condition-type
				(name field-indexes number-of-fields reporter))
		   (print-procedure
		    (standard-print-method 'condition-type
		      (lambda (type)
			(list (%condition-type/name type))))))
  (name #f read-only #t)
  generalizations
  (field-indexes #f read-only #t)
  (number-of-fields #f read-only #t)
  (reporter #f read-only #t)
  (properties (make-1d-table) read-only #t))

(define-guarantee condition-type "condition type")

(define-integrable (guarantee-condition-types object caller)
  (guarantee-list-of-type object
			  condition-type?
			  "list of condition types"
			  caller))

(define (make-condition-type name generalization field-names reporter)
  (if generalization
      (guarantee-condition-type generalization 'make-condition-type))
  (guarantee list-of-unique-symbols? field-names 'make-condition-type)
  (let ((type
	 (call-with-values
	     (lambda ()
	       (compute-field-indexes generalization field-names))
	   (lambda (n-fields field-indexes)
	     (%make-condition-type
	      (cond ((string? name) (string->immutable name))
		    ((symbol? name) (symbol->string name))
		    ((not name) "(anonymous)")
		    (else
		     (error:wrong-type-argument name "condition-type name"
						'make-condition-type)))
	      field-indexes
	      n-fields
	      (cond ((string? reporter)
		     (lambda (condition port)
		       condition
		       (write-string reporter port)))
		    ((procedure-of-arity? reporter 2)
		     reporter)
		    ((not reporter)
		     (if generalization
			 (%condition-type/reporter generalization)
			 (lambda (condition port)
			   (write-string "undocumented condition of type "
					 port)
			   (write (%condition/type condition) port))))
		    (else
		     (error:wrong-type-argument reporter
						"condition-type reporter"
						'make-condition-type))))))))
    (set-%condition-type/generalizations!
     type
     (cons type
	   (if generalization
	       (%condition-type/generalizations generalization)
	       '())))
    type))

(define (compute-field-indexes generalization field-names)
  (call-with-values
      (lambda ()
	(if generalization
	    (values (%condition-type/number-of-fields generalization)
		    (%condition-type/field-indexes generalization))
	    (values 0 '())))
    (lambda (old-n-fields old-indexes)
      (let loop
	  ((field-names field-names)
	   (index old-n-fields)
	   (indexes (let loop ((old-indexes old-indexes) (indexes '()))
		      (if (pair? old-indexes)
			  (loop (cdr old-indexes)
				(let ((entry (car old-indexes)))
				  (if (memq (car entry) field-names)
				      indexes
				      (cons entry indexes))))
			  indexes))))
	(if (pair? field-names)
	    (loop (cdr field-names)
		  (+ index 1)
		  (cons (cons (car field-names) index) indexes))
	    (values index (reverse! indexes)))))))

(define (%condition-type/field-index type field-name operator)
  (let ((association (assq field-name (%condition-type/field-indexes type))))
    (if (not association)
	(error:bad-range-argument field-name operator))
    (cdr association)))

(define (condition-type/name type)
  (guarantee-condition-type type 'condition-type/name)
  (%condition-type/name type))

(define (condition-type/field-names type)
  (guarantee-condition-type type 'condition-type/field-names)
  (map car (%condition-type/field-indexes type)))

(define (condition-type/generalizations type)
  (guarantee-condition-type type 'condition-type/generalizations)
  (list-copy (cdr (%condition-type/generalizations type))))

(define (condition-type/properties type)
  (guarantee-condition-type type 'condition-type/properties)
  (%condition-type/properties type))

(define (condition-type/put! type key datum)
  (1d-table/put! (condition-type/properties type) key datum))

(define (condition-type/get type key)
  (1d-table/get (condition-type/properties type) key #f))

;;;; Condition Instances

(define-structure (condition
		   (conc-name %condition/)
		   (constructor %%make-condition
				(type continuation restarts field-values))
		   (print-procedure
		    (standard-print-method 'condition
		      (lambda (condition)
			(list (%condition-type/name
			       (%condition/type condition)))))))
  (type #f read-only #t)
  (continuation #f read-only #t)
  (restarts #f read-only #t)
  (field-values #f read-only #t)
  (properties (make-1d-table) read-only #t))

(define-guarantee condition "condition")

(define (%make-condition type continuation restarts)
  (%%make-condition type continuation restarts
		    (make-vector (%condition-type/number-of-fields type) #f)))

(define (make-condition type continuation restarts field-alist)
  (guarantee-condition-type type 'make-condition)
  (guarantee continuation? continuation 'make-condition)
  (guarantee unique-keyword-list? field-alist 'make-condition)
  (let ((condition
	 (%make-condition type
			  continuation
			  (%restarts-argument restarts 'make-condition))))
    (let ((field-values (%condition/field-values condition)))
      (do ((alist field-alist (cddr alist)))
	  ((not (pair? alist)))
	(vector-set! field-values
		     (%condition-type/field-index type (car alist)
						  'make-condition)
		     (cadr alist))))
    condition))

(define (condition-constructor type #!optional field-names)
  (let ((caller 'condition-constructor))
    (guarantee-condition-type type caller)
    (let ((indexes
	   (map (lambda (field-name)
		  (%condition-type/field-index type field-name caller))
		(if (default-object? field-names)
		    (condition-type/field-names type)
		    (guarantee list-of-unique-symbols? field-names caller)))))
      (letrec
	  ((constructor
	    (lambda (continuation restarts . field-values)
	      (guarantee continuation? continuation constructor)
	      (let ((condition
		     (%make-condition type
				      continuation
				      (%restarts-argument restarts
							  constructor))))
		(let ((values (%condition/field-values condition)))
		  (do ((i indexes (cdr i))
		       (v field-values (cdr v)))
		      ((not (and (pair? i) (pair? v)))
		       (if (or (pair? i) (pair? v))
			   (error:wrong-number-of-arguments
			    constructor
			    (fix:+ (length indexes) 2)
			    (cons* continuation restarts field-values))))
		    (vector-set! values (car i) (car v))))
		condition))))
	constructor))))

(define-integrable (%restarts-argument restarts operator)
  (cond ((eq? 'bound-restarts restarts)
	 (param:bound-restarts))
	((condition? restarts)
	 (%condition/restarts restarts))
	(else
	 (guarantee-restarts restarts operator)
	 (list-copy restarts))))

(define (condition-of-type? object type)
  (guarantee-condition-type type 'condition-of-type?)
  (%condition-of-type? object type))

(define (condition-predicate type)
  (guarantee-condition-type type 'condition-predicate)
  (lambda (object) (%condition-of-type? object type)))

(define (%condition-of-type? object type)
  (and (condition? object)
       (%condition-has-type? object type)))

(define-integrable (%condition-has-type? condition type)
  (memq type (%condition-type/generalizations (%condition/type condition))))

(define (condition-accessor type field-name)
  (guarantee-condition-type type 'condition-accessor)
  (guarantee symbol? field-name 'condition-accessor)
  (let ((predicate (condition-predicate type))
	(index
	 (%condition-type/field-index type
				      field-name
				      'condition-accessor)))
    (lambda (condition)
      (if (not (predicate condition))
	  (error:wrong-type-argument condition
				     (string-append "condition of type "
						    (write-to-string type))
				     'condition-accessor))
      (vector-ref (%condition/field-values condition) index))))

(define (access-condition condition field-name)
  (guarantee-condition condition 'access-condition)
  ((condition-accessor (%condition/type condition) field-name) condition))

(define (condition/type condition)
  (guarantee-condition condition 'condition/type)
  (%condition/type condition))

(define (condition/continuation condition)
  (guarantee-condition condition 'condition/continuation)
  (%condition/continuation condition))

(define (condition/restarts condition)
  (guarantee-condition condition 'condition/restarts)
  (list-copy (%condition/restarts condition)))

(define (condition/properties condition)
  (guarantee-condition condition 'condition/properties)
  (%condition/properties condition))

(define (condition/put! condition key datum)
  (1d-table/put! (condition/properties condition) key datum))

(define (condition/get condition key)
  (1d-table/get (condition/properties condition) key #f))

(define (write-condition-report condition port)
  (guarantee-condition condition 'write-condition-report)
  (guarantee textual-output-port? port 'write-condition-report)
  (let ((reporter (%condition-type/reporter (%condition/type condition))))
    (if (%condition/error? condition)
	(ignore-errors (lambda () (reporter condition port)))
	(reporter condition port))))

(define (condition/report-string condition)
  (call-with-output-string
   (lambda (port)
     (write-condition-report condition port))))

;;;; Restarts

(define param:bound-restarts)

(define-structure (restart
		   (conc-name %restart/)
		   (constructor %make-restart
				(name reporter effector interactor))
		   (print-procedure
		    (standard-print-method 'restart
		      (lambda (restart)
			(let ((name (%restart/name restart)))
			  (if name
			      (list name)
			      '()))))))
  (name #f read-only #t)
  (reporter #f read-only #t)
  (effector #f read-only #t)
  (interactor #f)
  (properties (make-1d-table) read-only #t))

(define-guarantee restart "restart")

(define-integrable (guarantee-restarts object caller)
  (guarantee-list-of-type object restart? "list of restarts" caller))

(define (with-restart name reporter effector interactor thunk)
  (if name (guarantee symbol? name 'with-restart))
  (if (not (or (string? reporter) (procedure-of-arity? reporter 1)))
      (error:wrong-type-argument reporter "reporter" 'with-restart))
  (if (not (procedure? effector))
      (error:wrong-type-argument effector "effector" 'with-restart))
  (if (not (or (not interactor) (procedure? interactor)))
      (error:wrong-type-argument interactor "interactor" 'with-restart))
  (parameterize ((param:bound-restarts
		  (cons (%make-restart name reporter effector interactor)
			(param:bound-restarts))))
    (thunk)))

(define (with-simple-restart name reporter thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart name reporter (lambda () (continuation unspecific)) values
       thunk))))

(define (restart/name restart)
  (guarantee-restart restart 'restart/name)
  (%restart/name restart))

(define (write-restart-report restart port)
  (guarantee-restart restart 'write-restart-report)
  (guarantee textual-output-port? port 'write-restart-report)
  (let ((reporter (%restart/reporter restart)))
    (if (string? reporter)
	(write-string reporter port)
	(reporter port))))

(define (restart/effector restart)
  (guarantee-restart restart 'restart/effector)
  (%restart/effector restart))

(define (restart/interactor restart)
  (guarantee-restart restart 'restart/interactor)
  (%restart/interactor restart))

(define (restart/properties restart)
  (guarantee-restart restart 'restart/properties)
  (%restart/properties restart))

(define (restart/get restart key)
  (if (eq? key 'interactive)
      (restart/interactor restart)
      (1d-table/get (restart/properties restart) key #f)))

(define (restart/put! restart key datum)
  (if (eq? key 'interactive)
      (set-%restart/interactor! restart datum)
      (1d-table/put! (restart/properties restart) key datum)))

(define (bind-restart name reporter effector receiver)
  (with-restart name reporter effector #f
    (lambda ()
      (receiver (car (param:bound-restarts))))))

(define (invoke-restart restart . arguments)
  (guarantee-restart restart 'invoke-restart)
  (hook/invoke-restart (%restart/effector restart) arguments))

(define (invoke-restart-interactively restart #!optional condition)
  (guarantee-restart restart 'invoke-restart-interactively)
  (let ((effector (%restart/effector restart))
	(arguments
	 (let ((interactor (%restart/interactor restart)))
	   (if interactor
	       (call-with-values interactor list)
	       '())))
	(condition (if (default-object? condition) #f condition)))
    (let ((thread (and condition (condition/other-thread condition))))
      (if thread
	  (begin
	    (restart-thread thread 'ask
	      (lambda ()
		(hook/invoke-restart effector arguments)))
	    (continue-from-derived-thread-error condition))
	  (hook/invoke-restart effector arguments)))))

(define (condition/other-thread condition)
  (and (condition/derived-thread? condition)
       (let ((thread (access-condition condition 'thread)))
	 (and (not (eq? thread (current-thread)))
	      thread))))

(define (continue-from-derived-thread-error condition)
  (let loop ((restarts (bound-restarts)))
    (if (pair? restarts)
	(if (and (eq? 'continue (restart/name (car restarts)))
		 (eq? condition
		      (restart/get (car restarts) 'associated-condition)))
	    (invoke-restart (car restarts))
	    (loop (cdr restarts))))))

(define hook/invoke-restart)

(define (bound-restarts)
  (let loop ((restarts (param:bound-restarts)))
    (if (pair? restarts)
	(cons (car restarts) (loop (cdr restarts)))
	'())))

(define (first-bound-restart)
  (let ((restarts (param:bound-restarts)))
    (if (not (pair? restarts))
	(error:no-such-restart #f))
    (car restarts)))

(define (%find-restart name restarts)
  (let loop ((restarts restarts))
    (and (pair? restarts)
	 (if (eq? name (%restart/name (car restarts)))
	     (car restarts)
	     (loop (cdr restarts))))))

(define (find-restart name #!optional restarts)
  (guarantee symbol? name 'find-restart)
  (%find-restart name (restarts-default restarts 'find-restart)))

(define (abort #!optional restarts)
  (let ((restart (%find-restart 'abort (restarts-default restarts 'abort))))
    (if (not restart)
	(error:no-such-restart 'abort))
    ((%restart/effector restart))))

(define (continue #!optional restarts)
  (let ((restart
	 (%find-restart 'continue (restarts-default restarts 'continue))))
    (if restart
	((%restart/effector restart)))))

(define (muffle-warning #!optional restarts)
  (let ((restart
	 (%find-restart 'muffle-warning
			(restarts-default restarts 'muffle-warning))))
    (if (not restart)
	(error:no-such-restart 'muffle-warning))
    ((%restart/effector restart))))

(define (retry #!optional restarts)
  (let ((restart
	 (%find-restart 'retry (restarts-default restarts 'retry))))
    (if restart
	((%restart/effector restart)))))

(define (store-value datum #!optional restarts)
  (let ((restart
	 (%find-restart 'store-value
			(restarts-default restarts 'store-value))))
    (if restart
	((%restart/effector restart) datum))))

(define (use-value datum #!optional restarts)
  (let ((restart
	 (%find-restart 'use-value
			(restarts-default restarts 'use-value))))
    (if restart
	((%restart/effector restart) datum))))

(define (restarts-default restarts name)
  (cond ((or (default-object? restarts)
	     (eq? 'bound-restarts restarts))
	 (param:bound-restarts))
	((condition? restarts)
	 (%condition/restarts restarts))
	(else
	 (guarantee-restarts restarts name)
	 restarts)))

;;;; Condition Signalling and Handling

(define static-handler-frames)
(define dynamic-handler-frames)
(define break-on-signals-types)

(define (bind-default-condition-handler types handler)
  (guarantee-condition-types types 'bind-default-condition-handler)
  (guarantee-condition-handler handler 'bind-default-condition-handler)
  (static-handler-frames
   (cons (cons types handler)
	 (static-handler-frames)))
  unspecific)

(define (bind-condition-handler types handler thunk)
  (guarantee-condition-types types 'bind-condition-handler)
  (guarantee-condition-handler handler 'bind-condition-handler)
  (parameterize ((dynamic-handler-frames
		  (cons (cons types handler) (dynamic-handler-frames))))
    (thunk)))

(define-integrable (guarantee-condition-handler object caller)
  (guarantee unary-procedure? object caller))

(define (break-on-signals types)
  (guarantee-condition-types types 'break-on-signals)
  (break-on-signals-types types)
  unspecific)

(define hook/invoke-condition-handler)

(define (default/invoke-condition-handler handler condition)
  (handler condition))

(define (signal-condition condition)
  (guarantee-condition condition 'signal-condition)
  (let ((generalizations
	 (%condition-type/generalizations (%condition/type condition))))
    (let ((intersect-generalizations?
	   (lambda (types)
	     (let outer ((type (car types)) (types (cdr types)))
	       (let inner ((generalizations generalizations))
		 (if (pair? generalizations)
		     (or (eq? type (car generalizations))
			 (inner (cdr generalizations)))
		     (and (pair? types)
			  (outer (car types) (cdr types)))))))))
      (if (let ((types (break-on-signals-types)))
	    (and (pair? types)
		 (intersect-generalizations? types)))
	  (parameterize ((break-on-signals-types '()))
	    (breakpoint-procedure 'inherit
				  "BKPT entered because of BREAK-ON-SIGNALS:"
				  condition)))
      (do ((frames (dynamic-handler-frames) (cdr frames)))
	  ((not (pair? frames)))
	(if (let ((types (caar frames)))
	      (or (not (pair? types))
		  (intersect-generalizations? types)))
	    (parameterize ((dynamic-handler-frames (cdr frames)))
	      (hook/invoke-condition-handler (cdar frames) condition))))
      (do ((frames (static-handler-frames) (cdr frames)))
	  ((not (pair? frames)))
	(if (let ((types (caar frames)))
	      (or (not (pair? types))
		  (intersect-generalizations? types)))
	    (parameterize ((dynamic-handler-frames '())
			   (static-handler-frames (cdr frames)))
	      (hook/invoke-condition-handler (cdar frames) condition))))
      unspecific)))

;;;; Standard Condition Signallers

(define (error datum . arguments)
  (signal-simple datum arguments make-simple-error standard-error-handler))

(define (warn datum . arguments)
  (with-simple-restart 'muffle-warning "Ignore warning."
    (lambda ()
      (signal-simple datum arguments
		     make-simple-warning standard-warning-handler))))

(define (signal-simple datum arguments make-simple-condition default-handler)
  (let ((signal (signal-with-fallback default-handler)))
    (cond ((condition? datum)
	   (signal datum))
	  ((condition-type? datum)
	   (signal-standard* signal no-restarts
			     (condition-constructor datum)
			     arguments))
	  (else
	   (signal-standard signal no-restarts
			    make-simple-condition
			    datum
			    arguments)))))

(define (standard-error-handler condition)
  (let ((hook
	 (if (default-object? standard-error-hook)
	     (param:standard-error-hook)
	     standard-error-hook)))
    (if hook
	(fluid-let ((standard-error-hook #!default))
	  (parameterize ((param:standard-error-hook #f))
	    (hook condition)))))
  (repl/start (push-repl 'inherit condition '() "error>")))

(define (standard-warning-handler condition)
  (let ((hook
	 (if (default-object? standard-warning-hook)
	     (param:standard-warning-hook)
	     standard-warning-hook)))
    (if hook
	(fluid-let ((standard-warning-hook #!default))
	  (parameterize ((param:standard-warning-hook #f))
	    (hook condition)))
	(let ((port (notification-output-port)))
	  (fresh-line port)
	  (write-string ";Warning: " port)
	  (write-condition-report condition port)
	  (newline port)))))

(define standard-error-hook #!default)
(define standard-warning-hook #!default)
(define param:standard-error-hook)
(define param:standard-warning-hook)

(define (condition-signaller type field-names default-handler)
  (guarantee-condition-handler default-handler 'condition-signaller)
  (let ((signal (signal-with-fallback default-handler))
	(constructor (condition-constructor type field-names)))
    (lambda field-values
      (signal-standard* signal no-restarts constructor field-values))))

(define (signal-with-fallback default-handler)
  (lambda (condition)
    (signal-condition condition)
    (default-handler condition)))

(define (signal-standard signal bind-restarts constructor . args)
  (signal-standard* signal bind-restarts constructor args))

(define (signal-standard* signal bind-restarts constructor args)
  (call-with-current-continuation
    (lambda (continuation)
      (bind-restarts continuation
	(lambda ()
	  (signal (apply constructor continuation 'bound-restarts args)))))))

(define (no-restarts continuation thunk)
  (declare (ignore continuation))
  (thunk))

;;;; File operation errors

(define (error:file-operation index verb noun reason operator operands)
  (call-with-current-continuation
    (lambda (continuation)
      (signal-file-operation continuation index verb noun reason
			     operator operands))))

(define (signal-file-operation continuation index verb noun reason
			       operator operands)
  (with-restart 'use-value
      (string-append "Try to " verb " a different " noun ".")
      (lambda (operand)
	(within-continuation continuation
	  (lambda ()
	    (apply operator
		   (receive (head tail) (split-at operands index)
		     (append! head
			      (cons operand (cdr tail))))))))
      (let ((prompt
	     (string-append "New " noun
			    " name (an expression to be evaluated)")))
	(lambda ()
	  (values (prompt-for-evaluated-expression prompt))))
    (lambda ()
      (with-restart 'retry
	  (string-append "Try to " verb " the same " noun " again.")
	  (lambda ()
	    (within-continuation continuation
	      (lambda ()
		(apply operator operands))))
	  values
	(lambda ()
	  (let ((condition
		 (make-file-operation-error continuation 'bound-restarts
					    (list-ref operands index)
					    verb noun reason
					    operator operands)))
	    (signal-condition condition)
	    (standard-error-handler condition)))))))

;;;; R7RS adapter

(define (with-exception-handler handler thunk)
  (bind-condition-handler (list condition-type:error)
      (lambda (condition)
	(let ((value
	       (handler
		(if (r7rs-tunnel? condition)
		    (access-condition condition 'object)
		    condition)))
	      (restart (find-restart 'use-value condition)))
	  (if restart
	      (invoke-restart restart value))))
    thunk))

(define (raise object)
  (if (condition? object)
      (error object)
      (error condition-type:r7rs-tunnel object)))

(define (raise-continuable object)
  (if (condition? object)
      (error object)
      (signal-standard (signal-with-fallback standard-error-handler)
		       bind-raise-continuable-restarts
		       make-r7rs-tunnel
		       object)))

(define (bind-raise-continuable-restarts continuation thunk)
  (with-restart 'use-value
      "Continue with a different value."
      continuation
      (lambda ()
	(values (prompt-for-evaluated-expression
		 "Value to use (an expression to evaluate)")))
      thunk))

(define (error-object-message condition)
  (if (%condition-has-type? condition condition-type:simple-error)
      (access-condition condition 'message)
      (condition/report-string condition)))

(define (error-object-irritants condition)
  (if (%condition-has-type? condition condition-type:simple-error)
      (list-copy (access-condition condition 'irritants))
      '()))

;;;; Basic Condition Types

(define condition-type:arithmetic-error)
(define condition-type:bad-range-argument)
(define condition-type:cell-error)
(define condition-type:control-error)
(define condition-type:datum-out-of-range)
(define condition-type:derived-file-error)
(define condition-type:derived-port-error)
(define condition-type:derived-thread-error)
(define condition-type:divide-by-zero)
(define condition-type:error)
(define condition-type:file-error)
(define condition-type:file-operation-error)
(define condition-type:floating-point-divide-by-zero)
(define condition-type:floating-point-overflow)
(define condition-type:floating-point-underflow)
(define condition-type:illegal-datum)
(define condition-type:illegal-pathname-component)
(define condition-type:inexact-floating-point-result)
(define condition-type:integer-divide-by-zero)
(define condition-type:invalid-floating-point-operation)
(define condition-type:macro-binding)
(define condition-type:no-such-restart)
(define condition-type:port-error)
(define condition-type:r7rs-tunnel)
(define condition-type:serious-condition)
(define condition-type:simple-condition)
(define condition-type:simple-error)
(define condition-type:simple-warning)
(define condition-type:thread-error)
(define condition-type:unassigned-variable)
(define condition-type:unbound-variable)
(define condition-type:variable-error)
(define condition-type:warning)
(define condition-type:wrong-number-of-arguments)
(define condition-type:wrong-type-argument)
(define condition-type:wrong-type-datum)

(define make-simple-error)
(define make-simple-warning)
(define make-file-operation-error)
(define make-r7rs-tunnel)
(define error-object?)
(define file-error?)
(define r7rs-tunnel?)

(define error:bad-range-argument)
(define error:datum-out-of-range)
(define error:divide-by-zero)
(define error:no-such-restart)
(define error:derived-file)
(define error:derived-port)
(define error:derived-thread)
(define error:illegal-pathname-component)
(define error:macro-binding)
(define error:unassigned-variable)
(define error:unbound-variable)
(define error:wrong-number-of-arguments)
(define error:wrong-type-argument)
(define error:wrong-type-datum)

(define condition/derived-thread?)

(define (condition-type/error? type)
  (guarantee-condition-type type 'condition-type/error?)
  (%condition-type/error? type))

(define (condition/error? condition)
  (guarantee-condition condition 'condition/error?)
  (%condition/error? condition))

(define-integrable (%condition/error? condition)
  (%condition-type/error? (%condition/type condition)))

(define-integrable (%condition-type/error? type)
  (memq condition-type:error (%condition-type/generalizations type)))

(define (initialize-package!)
  (set! param:bound-restarts (make-unsettable-parameter '()))
  (set! static-handler-frames (make-settable-parameter '()))
  (set! dynamic-handler-frames (make-unsettable-parameter '()))
  (set! break-on-signals-types (make-settable-parameter '()))
  (set! param:standard-error-hook (make-settable-parameter #f))
  (set! param:standard-warning-hook (make-settable-parameter #f))
  (set! hook/invoke-condition-handler default/invoke-condition-handler)
  ;; No eta conversion for bootstrapping and efficiency reasons.
  (set! hook/invoke-restart
	(lambda (effector arguments)
	  (apply effector arguments)))
  (set! condition-type:serious-condition
	(make-condition-type 'serious-condition #f '() #f))
  (set! condition-type:warning
	(make-condition-type 'warning #f '() #f))

  (set! condition-type:error
	(make-condition-type 'error condition-type:serious-condition '() #f))
  (set! error-object?
	(condition-predicate condition-type:error))
  (set! condition-type:r7rs-tunnel
	(make-condition-type 'r7rs-tunnel condition-type:error '(object)
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'object) port)
	    (write-string " was raised." port))))
  (set! r7rs-tunnel?
	(condition-predicate condition-type:r7rs-tunnel))

  (let ((reporter/simple-condition
	 (lambda (condition port)
	   (format-error-message (access-condition condition 'message)
				 (access-condition condition 'irritants)
				 port))))
    (set! condition-type:simple-condition
	  (make-condition-type 'simple-condition #f '(message irritants)
	    reporter/simple-condition))
    (set! condition-type:simple-error
	  (make-condition-type 'simple-error condition-type:error
	      '(message irritants)
	    reporter/simple-condition))
    (set! condition-type:simple-warning
	  (make-condition-type 'simple-warning condition-type:warning
	      '(message irritants)
	    reporter/simple-condition)))

  (set! condition-type:illegal-datum
	(make-condition-type 'illegal-datum condition-type:error '(datum)
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'datum) port)
	    (write-string " has been found in an inappropriate context."
			  port))))

  (set! condition-type:datum-out-of-range
	(make-condition-type 'datum-out-of-range condition-type:illegal-datum
	    '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'datum) port)
	    (write-string " is not in the correct range." port))))

  (let ((write-type-description
	 (let ((char-set:vowels
		(char-set #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)))
	   (lambda (condition port)
	     (let ((type (access-condition condition 'type)))
	       (if (string? type)
		   (begin
		     (if (not (or (string-null? type)
				  (string-prefix-ci? "a " type)
				  (string-prefix-ci? "an " type)))
			 (write-string
			  (if (char-in-set? (string-ref type 0) char-set:vowels)
			      "an "
			      "a ")
			  port))
		     (write-string type port))
		   (write-string "the correct type" port))))))
	(write-operand-description
	 (lambda (condition port)
	   (let ((operator (access-condition condition 'operator))
		 (operand (access-condition condition 'operand)))
	     (if (or (symbol? operator)
		     (procedure? operator))
		 (begin
		   (write-string ", passed " port)
		   (cond ((symbol? operand)
			  (write-string "as the argument " port)
			  (write operand port))
			 ((exact-nonnegative-integer? operand)
			  (write-string "as the " port)
			  (write-string (ordinal-number-string (+ operand 1))
					port)
			  (write-string " argument" port))
			 (else
			  (write-string "as an argument" port)))
		   (write-string " to " port)
		   (write-operator operator port)
		   (write-string "," port)))))))
    (set! condition-type:wrong-type-datum
	  (make-condition-type 'wrong-type-datum condition-type:illegal-datum
	      '(type)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'datum) port)
	      (write-string " is not " port)
	      (write-type-description condition port)
	      (write-string "." port))))
    (set! condition-type:wrong-type-argument
	  (make-condition-type 'wrong-type-argument
	      condition-type:wrong-type-datum
	      '(operator operand)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'datum) port)
	      (write-operand-description condition port)
	      (write-string " is not " port)
	      (write-type-description condition port)
	      (write-string "." port))))
    (set! condition-type:bad-range-argument
	  (make-condition-type 'bad-range-argument
	      condition-type:datum-out-of-range
	      '(operator operand)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'datum) port)
	      (write-operand-description condition port)
	      (write-string " is not in the correct range." port)))))

  (set! condition-type:wrong-number-of-arguments
	(make-condition-type 'wrong-number-of-arguments
	    condition-type:wrong-type-datum
	    '(operands)
	  (lambda (condition port)
	    (let ((pluralize-argument
		   (lambda (number)
		     (write-string
		      (if (= number 1) " argument" " arguments")
		      port))))
	      (write-string "The procedure " port)
	      (write-operator (access-condition condition 'datum) port)
	      (write-string " has been called with " port)
	      (let ((count (length (access-condition condition 'operands))))
		(write count port)
		(pluralize-argument count))
	      (write-string "; it requires " port)
	      (let ((arity (access-condition condition 'type)))
		(let ((arity-min (procedure-arity-min arity))
		      (arity-max (procedure-arity-max arity)))
		  (cond ((eqv? arity-min arity-max)
			 (write-string "exactly " port)
			 (write arity-min port)
			 (pluralize-argument arity-min))
			((not arity-max)
			 (write-string "at least " port)
			 (write (car arity) port)
			 (pluralize-argument (car arity)))
			(else
			 (write-string "between " port)
			 (write arity-min port)
			 (write-string " and " port)
			 (write arity-max port)
			 (write-string " arguments" port)))))
	      (write-char #\. port)))))

  (set! condition-type:illegal-pathname-component
	(make-condition-type 'illegal-pathname-component
	    condition-type:wrong-type-datum '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'datum) port)
	    (write-string " is not a valid pathname " port)
	    (write-string (access-condition condition 'type) port)
	    (write-string "." port))))

  (set! condition-type:control-error
	(make-condition-type 'control-error condition-type:error '()
	  "Control error."))

  (set! condition-type:no-such-restart
	(make-condition-type 'no-such-restart condition-type:control-error
	    '(name)
	  (lambda (condition port)
	    (write-string "The restart named " port)
	    (write (access-condition condition 'name) port)
	    (write-string " is not bound." port))))

  (let ((anonymous-error
	 (lambda (type-name field-name)
	   (make-condition-type type-name condition-type:error
	       (list field-name)
	     (lambda (condition port)
	       (write-string "Anonymous error associated with " port)
	       (write (access-condition condition field-name) port)
	       (write-string "." port))))))
    (set! condition-type:port-error (anonymous-error 'port-error 'port))
    (set! condition-type:file-error (anonymous-error 'file-error 'filename))
    (set! condition-type:cell-error (anonymous-error 'cell-error 'location))
    (set! condition-type:thread-error (anonymous-error 'thread-error 'thread)))
  (set! file-error?
	(condition-predicate condition-type:file-error))

  (set! condition-type:derived-port-error
	(make-condition-type 'derived-port-error condition-type:port-error
	    '(condition)
	  (lambda (condition port)
	    (write-string "The port " port)
	    (write (access-condition condition 'port) port)
	    (write-string " signalled an error " port)
	    (write (access-condition condition 'condition) port)
	    (write-string ":" port)
	    (newline port)
	    (write-condition-report (access-condition condition 'condition)
				    port))))
  (set! error:derived-port
	(let ((make-condition
	       (condition-constructor condition-type:derived-port-error
				      '(port condition))))
	  (lambda (port condition)
	    (guarantee-condition condition 'error:derived-port)
	    (error (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   port
				   condition)))))

  (set! condition-type:derived-file-error
	(make-condition-type 'derived-file-error condition-type:file-error
	    '(condition)
	  (lambda (condition port)
	    (write-string "The file " port)
	    (write (access-condition condition 'filename) port)
	    (write-string " signalled an error " port)
	    (write (access-condition condition 'condition) port)
	    (write-string ":" port)
	    (newline port)
	    (write-condition-report (access-condition condition 'condition)
				    port))))
  (set! error:derived-file
	(let ((make-condition
	       (condition-constructor condition-type:derived-file-error
				      '(filename condition))))
	  (lambda (filename condition)
	    (guarantee-condition condition 'error:derived-file)
	    (error (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   filename
				   condition)))))

  (set! condition-type:derived-thread-error
	(make-condition-type 'derived-thread-error condition-type:thread-error
	    '(condition)
	  (lambda (condition port)
	    (write-string "The thread " port)
	    (write (access-condition condition 'thread) port)
	    (write-string " signalled " port)
	    (let ((condition (access-condition condition 'condition)))
	      (write-string (if (condition/error? condition)
				"an error "
				"a condition ")
			    port)
	      (write condition port)
	      (write-string ":" port)
	      (newline port)
	      (write-condition-report condition port)))))
  (set! error:derived-thread
	(let ((make-condition
	       (condition-constructor condition-type:derived-thread-error
				      '(thread condition))))
	  (lambda (thread condition)
	    (guarantee-condition condition 'error:derived-thread)
	    (let ((condition
		   (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   thread
				   condition)))
	      (with-simple-restart 'continue "Continue from error."
		(lambda ()
		  (restart/put! (first-bound-restart)
				'associated-condition
				condition)
		  (error condition)))))))
  (set! condition/derived-thread?
	(condition-predicate condition-type:derived-thread-error))

  (set! condition-type:file-operation-error
	(make-condition-type 'file-operation-error condition-type:file-error
	    '(verb noun reason operator operands)
	  (lambda (condition port)
	    (let ((noun (access-condition condition 'noun)))
	      (write-string "Unable to " port)
	      (write-string (access-condition condition 'verb) port)
	      (write-string " " port)
	      (write-string noun port)
	      (write-string " " port)
	      (write (->namestring (access-condition condition 'filename))
		     port)
	      (write-string " because: " port)
	      (let ((reason (access-condition condition 'reason)))
		(if reason
		    (write-string (reason-titlecase reason) port)
		    (begin
		      (write-string "No such " port)
		      (write-string noun port))))
	      (write-string "." port)))))
  (set! make-file-operation-error
	(condition-constructor condition-type:file-operation-error
			       '(filename verb noun reason operator operands)))

  (set! condition-type:variable-error
	(make-condition-type 'variable-error condition-type:cell-error
	    '(environment)
	  (lambda (condition port)
	    (write-string "Anonymous error associated with variable " port)
	    (write (access-condition condition 'location) port)
	    (write-string "." port))))

  (set! condition-type:unbound-variable
	(make-condition-type 'unbound-variable condition-type:variable-error
	    '()
	  (lambda (condition port)
	    (write-string "Unbound variable: " port)
	    (write (access-condition condition 'location) port))))

  (set! condition-type:unassigned-variable
	(make-condition-type 'unassigned-variable condition-type:variable-error
	    '()
	  (lambda (condition port)
	    (write-string "Unassigned variable: " port)
	    (write (access-condition condition 'location) port))))

  (set! condition-type:macro-binding
	(make-condition-type 'macro-binding condition-type:variable-error '()
	  (lambda (condition port)
	    (write-string "Variable reference to a syntactic keyword: " port)
	    (write (access-condition condition 'location) port))))

  (let ((arithmetic-error-report
	 (lambda (description)
	   (lambda (condition port)
	     (write-string description port)
	     (let ((operator (access-condition condition 'operator)))
	       (if operator
		   (begin
		     (write-string " signalled by " port)
		     (write-operator operator port)
		     (write-string "." port))))))))
    (set! condition-type:arithmetic-error
	  (make-condition-type 'arithmetic-error condition-type:error
	      '(operator operands)
	    (arithmetic-error-report "Anonymous arithmetic error")))
    (set! condition-type:divide-by-zero
	  (make-condition-type 'divide-by-zero condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Division by zero")))
    (set! condition-type:integer-divide-by-zero
	  (make-condition-type 'integer-divide-by-zero
	      condition-type:divide-by-zero
	      '()
	    (arithmetic-error-report "Integer division by zero")))
    (set! condition-type:floating-point-divide-by-zero
	  (make-condition-type 'floating-point-divide-by-zero
	      condition-type:divide-by-zero
	      '()
	    (arithmetic-error-report "Floating-point division by zero")))
    (set! condition-type:inexact-floating-point-result
	  (make-condition-type 'inexact-floating-point-result
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Inexact floating-point result")))
    (set! condition-type:invalid-floating-point-operation
	  (make-condition-type 'invalid-floating-point-operation
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Invalid floating-point operation")))
    (set! condition-type:floating-point-overflow
	  (make-condition-type 'floating-point-overflow
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Floating-point overflow")))
    (set! condition-type:floating-point-underflow
	  (make-condition-type 'floating-point-underflow
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Floating-point underflow"))))

  (set! make-simple-error
	(condition-constructor condition-type:simple-error
			       '(message irritants)))
  (set! make-simple-warning
	(condition-constructor condition-type:simple-warning
			       '(message irritants)))
  (set! make-r7rs-tunnel
	(condition-constructor condition-type:r7rs-tunnel
			       '(object)))

  (set! error:wrong-type-datum
	(condition-signaller condition-type:wrong-type-datum
			     '(datum type)
			     standard-error-handler))
  (set! error:datum-out-of-range
	(condition-signaller condition-type:datum-out-of-range
			     '(datum)
			     standard-error-handler))
  (set! error:wrong-type-argument
	(condition-signaller condition-type:wrong-type-argument
			     '(datum type operator)
			     standard-error-handler))
  (set! error:bad-range-argument
	(condition-signaller condition-type:bad-range-argument
			     '(datum operator)
			     standard-error-handler))
  (set! error:wrong-number-of-arguments
	(condition-signaller condition-type:wrong-number-of-arguments
			     '(datum type operands)
			     standard-error-handler))
  (set! error:illegal-pathname-component
	(condition-signaller condition-type:illegal-pathname-component
			     '(datum type)
			     standard-error-handler))
  (set! error:divide-by-zero
	(condition-signaller condition-type:divide-by-zero
			     '(operator operands)
			     standard-error-handler))
  (set! error:no-such-restart
	(condition-signaller condition-type:no-such-restart
			     '(name)
			     standard-error-handler))
  (set! error:unassigned-variable
	(condition-signaller condition-type:unassigned-variable
			     '(environment location)
			     standard-error-handler))
  (set! error:unbound-variable
	(condition-signaller condition-type:unbound-variable
			     '(environment location)
			     standard-error-handler))
  (set! error:macro-binding
	(condition-signaller condition-type:macro-binding
			     '(environment location)
			     standard-error-handler))
  unspecific)

;;;; Utilities

(define (ignore-errors thunk #!optional map-error)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:error)
	 (cond ((or (default-object? map-error)
		    (not map-error))
		k)
	       ((and (procedure? map-error)
		     (procedure-arity-valid? map-error 1))
		(lambda (condition)
		  (k (map-error condition))))
	       (else
		(error:wrong-type-argument map-error
					   "map-error procedure"
					   'ignore-errors)))
       thunk))))

(define warn-errors?
  (let ((ok "ok"))
    (lambda (thunk)
      (let ((v (ignore-errors (lambda () (thunk) ok))))
	(cond ((eq? v ok) #f)
	      ((condition? v) (warn v) #t)
	      (else (error "Unexpected value:" v)))))))

(define (format-error-message message irritants port)
  (parameterize ((param:printer-list-depth-limit 2)
		 (param:printer-list-breadth-limit 5))
    (for-each (lambda (irritant)
		(if (and (pair? irritant)
			 (eq? (car irritant) error-irritant/noise-tag))
		    (display (cdr irritant) port)
		    (begin
		      (write-char #\space port)
		      (write irritant port))))
	      (cons (if (string? message)
			(error-irritant/noise message)
			message)
		    irritants))))

(define-integrable (error-irritant/noise noise)
  (cons error-irritant/noise-tag noise))

(define error-irritant/noise-tag
  '(error-irritant/noise))

(define (ordinal-number-string n)
  (if (not (and (exact-nonnegative-integer? n) (< n 100)))
      (error:wrong-type-argument n "exact integer between 0 and 99"
				 'ordinal-number-string))
  (let ((ones-names
	 #("zeroth" "first" "second" "third" "fourth" "fifth" "sixth"
		    "seventh" "eighth" "ninth"))
	(tens-names #("twen" "thir" "for" "fif" "six" "seven" "eigh" "nine")))
    (cond ((< n 10) (vector-ref ones-names n))
	  ((< n 20)
	   (vector-ref #("tenth" "eleventh" "twelfth" "thirteenth" "fourteenth"
				 "fifteenth" "sixteenth" "seventeenth"
				 "eighteenth" "nineteenth")
		       (- n 10)))
	  (else
	   (let ((qr (integer-divide n 10)))
	     (string-append
	      (vector-ref tens-names (- (integer-divide-quotient qr) 2))
	      (let ((ones (integer-divide-remainder qr)))
		(if (zero? ones)
		    "tieth"
		    (string-append "ty-" (vector-ref ones-names ones))))))))))

(define (write-operator operator port)
  (write (if (primitive-procedure? operator)
	     (primitive-procedure-name operator)
	     operator)
	 port))

;; Not quite right: it should be using string-word-breaks to find the first
;; word.  Unfortunately even with the breaks it's still non-trivial to discover
;; what is a word and what isn't.  So for now we do this simple thing based on
;; whitespace.
(define (reason-titlecase reason)
  (let ((index (string-find-first-index char-whitespace? reason)))
    (if index
	(string-append (string-titlecase (string-slice reason 0 index))
		       (string-slice reason index))
	(string-titlecase reason))))