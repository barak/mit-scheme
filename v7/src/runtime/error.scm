#| -*-Scheme-*-

$Id: error.scm,v 14.60 2003/02/13 19:52:29 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1995,2000,2001,2002 Massachusetts Institute of Technology
Copyright 2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
		    (standard-unparser-method 'CONDITION-TYPE
		      (lambda (type port)
			(write-char #\space port)
			(write-string (%condition-type/name type) port)))))
  (name #f read-only #t)
  generalizations
  (field-indexes #f read-only #t)
  (number-of-fields #f read-only #t)
  (reporter #f read-only #t)
  (properties (make-1d-table) read-only #t))

(define (make-condition-type name generalization field-names reporter)
  (if generalization
      (guarantee-condition-type generalization 'MAKE-CONDITION-TYPE))
  (guarantee-list-of-symbols field-names 'MAKE-CONDITION-TYPE)
  (let ((type
	 (call-with-values
	     (lambda ()
	       (compute-field-indexes generalization field-names))
	   (lambda (n-fields field-indexes)
	     (%make-condition-type
	      (cond ((string? name) (string-copy name))
		    ((symbol? name) (symbol->string name))
		    ((not name) "(anonymous)")
		    (else
		     (error:wrong-type-argument name "condition-type name"
						'MAKE-CONDITION-TYPE)))
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
						'MAKE-CONDITION-TYPE))))))))
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

(define (condition-type/field-names type)
  (guarantee-condition-type type 'CONDITION-TYPE/FIELD-NAMES)
  (map car (%condition-type/field-indexes type)))

(define (condition-type/generalizations type)
  (guarantee-condition-type type 'CONDITION-TYPE/GENERALIZATIONS)
  (list-copy (cdr (%condition-type/generalizations type))))

(define (condition-type/properties type)
  (guarantee-condition-type type 'CONDITION-TYPE/PROPERTIES)
  (%condition-type/properties type))

(define (condition-type/put! type key datum)
  (1d-table/put! (condition-type/properties type) key datum))

(define (condition-type/get type key)
  (1d-table/get (condition-type/properties type) key #f))

;;;; Condition Instances

(define-structure (condition
		   (conc-name %condition/)
		   (constructor %make-condition (type continuation restarts))
		   (print-procedure
		    (standard-unparser-method 'CONDITION
		      (lambda (condition port)
			(write-char #\space port)
			(write-string
			 (%condition-type/name (%condition/type condition))
			 port)))))
  (type #f read-only #t)
  (continuation #f read-only #t)
  (restarts #f read-only #t)
  (field-values (make-vector (%condition-type/number-of-fields type) #f)
		read-only #t)
  (properties (make-1d-table) read-only #t))

(define (make-condition type continuation restarts field-alist)
  (guarantee-condition-type type 'MAKE-CONDITION)
  (guarantee-continuation continuation 'MAKE-CONDITION)
  (guarantee-keyword-association-list field-alist 'MAKE-CONDITION)
  (let ((condition
	 (%make-condition type
			  continuation
			  (%restarts-argument restarts 'MAKE-CONDITION))))
    (let ((field-values (%condition/field-values condition)))
      (do ((alist field-alist (cddr alist)))
	  ((not (pair? alist)))
	(vector-set! field-values
		     (%condition-type/field-index type (car alist)
						  'MAKE-CONDITION)
		     (cadr alist))))
    condition))

(define (condition-constructor type field-names)
  (guarantee-condition-type type 'CONDITION-CONSTRUCTOR)
  (guarantee-list-of-symbols field-names 'CONDITION-CONSTRUCTOR)
  (let ((indexes
	 (map (lambda (field-name)
		(%condition-type/field-index type field-name
					     'CONDITION-CONSTRUCTOR))
	      field-names)))
    (letrec
	((constructor
	  (lambda (continuation restarts . field-values)
	    (guarantee-continuation continuation constructor)
	    (let ((condition
		   (%make-condition type
				    continuation
				    (%restarts-argument restarts
							constructor))))
	      (let ((values (%condition/field-values condition)))
		(do ((i indexes (cdr i))
		     (v field-values (cdr v)))
		    ((or (not (pair? i))
			 (not (pair? v)))
		     (if (or (pair? i) (pair? v))
			 (error:wrong-number-of-arguments
			  constructor
			  (+ (length indexes) 1)
			  (cons continuation field-values))))
		  (vector-set! values (car i) (car v))))
	      condition))))
      constructor)))

(define-integrable (%restarts-argument restarts operator)
  (cond ((eq? 'BOUND-RESTARTS restarts)
	 *bound-restarts*)
	((condition? restarts)
	 (%condition/restarts restarts))
	(else
	 (guarantee-restarts restarts operator)
	 (list-copy restarts))))

(define (condition-predicate type)
  (guarantee-condition-type type 'CONDITION-PREDICATE)
  (lambda (object)
    (and (condition? object)
	 (memq type
	       (%condition-type/generalizations (%condition/type object))))))

(define (condition-accessor type field-name)
  (guarantee-condition-type type 'CONDITION-ACCESSOR)
  (guarantee-symbol field-name 'CONDITION-ACCESSOR)
  (let ((predicate (condition-predicate type))
	(index
	 (%condition-type/field-index type
				      field-name
				      'CONDITION-ACCESSOR)))
    (lambda (condition)
      (if (not (predicate condition))
	  (error:wrong-type-argument condition
				     (string-append "condition of type "
						    (write-to-string type))
				     'CONDITION-ACCESSOR))
      (vector-ref (%condition/field-values condition) index))))

(define (access-condition condition field-name)
  (guarantee-condition condition 'ACCESS-CONDITION)
  ((condition-accessor (%condition/type condition) field-name) condition))

(define (condition/type condition)
  (guarantee-condition condition 'CONDITION/TYPE)
  (%condition/type condition))

(define (condition/continuation condition)
  (guarantee-condition condition 'CONDITION/CONTINUATION)
  (%condition/continuation condition))

(define (condition/restarts condition)
  (guarantee-condition condition 'CONDITION/RESTARTS)
  (list-copy (%condition/restarts condition)))

(define (condition/properties condition)
  (guarantee-condition condition 'CONDITION/PROPERTIES)
  (%condition/properties condition))

(define (condition/put! condition key datum)
  (1d-table/put! (condition/properties condition) key datum))

(define (condition/get condition key)
  (1d-table/get (condition/properties condition) key #f))

(define (write-condition-report condition port)
  (guarantee-condition condition 'WRITE-CONDITION-REPORT)
  (guarantee-output-port port 'WRITE-CONDITION-REPORT)
  (let ((reporter (%condition-type/reporter (%condition/type condition))))
    (if (%condition/error? condition)
	(ignore-errors (lambda () (reporter condition port)))
	(reporter condition port))))

(define (condition/report-string condition)
  (call-with-output-string
   (lambda (port)
     (write-condition-report condition port))))

;;;; Restarts

(define *bound-restarts* '())

(define-structure (restart
		   (conc-name %restart/)
		   (constructor %make-restart
				(name reporter effector interactor))
		   (print-procedure
		    (standard-unparser-method 'RESTART
		      (lambda (restart port)
			(write-char #\space port)
			(let ((name (%restart/name restart)))
			  (if name
			      (write name port)
			      (write-string "(anonymous)" port)))))))
  (name #f read-only #t)
  (reporter #f read-only #t)
  (effector #f read-only #t)
  (interactor #f)
  (properties (make-1d-table) read-only #t))

(define (with-restart name reporter effector interactor thunk)
  (if name (guarantee-symbol name 'WITH-RESTART))
  (if (not (or (string? reporter) (procedure-of-arity? reporter 1)))
      (error:wrong-type-argument reporter "reporter" 'WITH-RESTART))
  (if (not (procedure? effector))
      (error:wrong-type-argument effector "effector" 'WITH-RESTART))
  (if (not (or (not interactor) (procedure? interactor)))
      (error:wrong-type-argument interactor "interactor" 'WITH-RESTART))
  (fluid-let ((*bound-restarts*
	       (cons (%make-restart name reporter effector interactor)
		     *bound-restarts*)))
    (thunk)))

(define (with-simple-restart name reporter thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (with-restart name reporter (lambda () (continuation unspecific)) values
       thunk))))

(define (restart/name restart)
  (guarantee-restart restart 'RESTART/NAME)
  (%restart/name restart))

(define (write-restart-report restart port)
  (guarantee-restart restart 'WRITE-RESTART-REPORT)
  (guarantee-output-port port 'WRITE-RESTART-REPORT)
  (let ((reporter (%restart/reporter restart)))
    (if (string? reporter)
	(write-string reporter port)
	(reporter port))))

(define (restart/effector restart)
  (guarantee-restart restart 'RESTART/EFFECTOR)
  (%restart/effector restart))

(define (restart/interactor restart)
  (guarantee-restart restart 'RESTART/INTERACTOR)
  (%restart/interactor restart))

(define (restart/properties restart)
  (guarantee-restart restart 'RESTART/PROPERTIES)
  (%restart/properties restart))

(define (restart/get restart key)
  (if (eq? key 'INTERACTIVE)
      (restart/interactor restart)
      (1d-table/get (restart/properties restart) key #f)))

(define (restart/put! restart key datum)
  (if (eq? key 'INTERACTIVE)
      (set-%restart/interactor! restart datum)
      (1d-table/put! (restart/properties restart) key datum)))

(define (bind-restart name reporter effector receiver)
  (with-restart name reporter effector #f
    (lambda ()
      (receiver (car *bound-restarts*)))))

(define (invoke-restart restart . arguments)
  (guarantee-restart restart 'INVOKE-RESTART)
  (hook/invoke-restart (%restart/effector restart) arguments))

(define (invoke-restart-interactively restart #!optional condition)
  (guarantee-restart restart 'INVOKE-RESTART-INTERACTIVELY)
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
	    (restart-thread thread 'ASK
	      (lambda ()
		(hook/invoke-restart effector arguments)))
	    (continue-from-derived-thread-error condition))
	  (hook/invoke-restart effector arguments)))))

(define (condition/other-thread condition)
  (and (condition/derived-thread? condition)
       (let ((thread (access-condition condition 'THREAD)))
	 (and (not (eq? thread (current-thread)))
	      thread))))

(define (continue-from-derived-thread-error condition)
  (let loop ((restarts (bound-restarts)))
    (if (pair? restarts)
	(if (and (eq? 'CONTINUE (restart/name (car restarts)))
		 (eq? condition
		      (restart/get (car restarts) 'ASSOCIATED-CONDITION)))
	    (invoke-restart (car restarts))
	    (loop (cdr restarts))))))

(define hook/invoke-restart)

(define (bound-restarts)
  (let loop ((restarts *bound-restarts*))
    (if (pair? restarts)
	(cons (car restarts) (loop (cdr restarts)))
	'())))

(define (first-bound-restart)
  (let ((restarts *bound-restarts*))
    (if (not (pair? restarts))
	(error:no-such-restart #f))
    (car restarts)))

(define (%find-restart name restarts)
  (let loop ((restarts restarts))
    (and (pair? restarts)
	 (if (eq? name (%restart/name (car restarts)))
	     (car restarts)
	     (loop (cdr restarts))))))

(define-syntax restarts-default
  (sc-macro-transformer
   (lambda (form environment)
     (let ((restarts (close-syntax (cadr form) environment))
	   (name (close-syntax (caddr form) environment)))
       ;; This is a macro because DEFAULT-OBJECT? is.
       `(COND ((OR (DEFAULT-OBJECT? ,restarts)
		   (EQ? 'BOUND-RESTARTS ,restarts))
	       *BOUND-RESTARTS*)
	      ((CONDITION? ,restarts)
	       (%CONDITION/RESTARTS ,restarts))
	      (ELSE
	       (GUARANTEE-RESTARTS ,restarts ,name)
	       ,restarts))))))

(define (find-restart name #!optional restarts)
  (guarantee-symbol name 'FIND-RESTART)
  (%find-restart name (restarts-default restarts 'FIND-RESTART)))

(define (abort #!optional restarts)
  (let ((restart (%find-restart 'ABORT (restarts-default restarts 'ABORT))))
    (if (not restart)
	(error:no-such-restart 'ABORT))
    ((%restart/effector restart))))

(define (continue #!optional restarts)
  (let ((restart
	 (%find-restart 'CONTINUE (restarts-default restarts 'CONTINUE))))
    (if restart
	((%restart/effector restart)))))

(define (muffle-warning #!optional restarts)
  (let ((restart
	 (%find-restart 'MUFFLE-WARNING
			(restarts-default restarts 'MUFFLE-WARNING))))
    (if (not restart)
	(error:no-such-restart 'MUFFLE-WARNING))
    ((%restart/effector restart))))

(define (retry #!optional restarts)
  (let ((restart
	 (%find-restart 'RETRY (restarts-default restarts 'RETRY))))
    (if restart
	((%restart/effector restart)))))

(define (store-value datum #!optional restarts)
  (let ((restart
	 (%find-restart 'STORE-VALUE
			(restarts-default restarts 'STORE-VALUE))))
    (if restart
	((%restart/effector restart) datum))))

(define (use-value datum #!optional restarts)
  (let ((restart
	 (%find-restart 'USE-VALUE
			(restarts-default restarts 'USE-VALUE))))
    (if restart
	((%restart/effector restart) datum))))

;;;; Condition Signalling and Handling

(define static-handler-frames '())
(define dynamic-handler-frames '())
(define break-on-signals-types '())

(define (bind-default-condition-handler types handler)
  (guarantee-condition-types types 'BIND-DEFAULT-CONDITION-HANDLER)
  (guarantee-condition-handler handler 'BIND-DEFAULT-CONDITION-HANDLER)
  (set! static-handler-frames
	(cons (cons types handler) static-handler-frames))
  unspecific)

(define (bind-condition-handler types handler thunk)
  (guarantee-condition-types types 'BIND-CONDITION-HANDLER)
  (guarantee-condition-handler handler 'BIND-CONDITION-HANDLER)
  (fluid-let ((dynamic-handler-frames
	       (cons (cons types handler) dynamic-handler-frames)))
    (thunk)))

(define (ignore-errors thunk)
  (call-with-current-continuation
   (lambda (continuation)
     (bind-condition-handler (list condition-type:error) continuation
       thunk))))

(define (break-on-signals types)
  (guarantee-condition-types types 'BREAK-ON-SIGNALS)
  (set! break-on-signals-types types)
  unspecific)

(define hook/invoke-condition-handler)

(define (default/invoke-condition-handler handler condition)
  (handler condition))

(define (signal-condition condition)
  (guarantee-condition condition 'SIGNAL-CONDITION)
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
      (if (let ((types break-on-signals-types))
	    (and (pair? types)
		 (intersect-generalizations? types)))
	  (fluid-let ((break-on-signals-types '()))
	    (breakpoint-procedure 'INHERIT
				  "BKPT entered because of BREAK-ON-SIGNALS:"
				  condition)))
      (do ((frames dynamic-handler-frames (cdr frames)))
	  ((not (pair? frames)))
	(if (let ((types (caar frames)))
	      (or (not (pair? types))
		  (intersect-generalizations? types)))
	    (fluid-let ((dynamic-handler-frames (cdr frames)))
	      (hook/invoke-condition-handler (cdar frames) condition))))
      (do ((frames static-handler-frames (cdr frames)))
	  ((not (pair? frames)))
	(if (let ((types (caar frames)))
	      (or (not (pair? types))
		  (intersect-generalizations? types)))
	    (fluid-let ((static-handler-frames (cdr frames))
			(dynamic-handler-frames '()))
	      (hook/invoke-condition-handler (cdar frames) condition))))
      unspecific)))

;;;; Standard Condition Signallers

(define (error datum . arguments)
  (signal-simple datum arguments make-simple-error standard-error-handler))

(define (warn datum . arguments)
  (with-simple-restart 'MUFFLE-WARNING "Ignore warning."
    (lambda ()
      (signal-simple datum arguments
		     make-simple-warning standard-warning-handler))))

(define (signal-simple datum arguments make-simple-condition default-handler)
  (if (condition? datum)
      (begin
	(signal-condition datum)
	(default-handler datum))
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((condition
		(if (condition-type? datum)
		    (make-condition datum
				    continuation
				    'BOUND-RESTARTS
				    arguments)
		    (make-simple-condition continuation
					   'BOUND-RESTARTS
					   datum
					   arguments))))
	   (begin
	     (signal-condition condition)
	     (default-handler condition)))))))

(define (standard-error-handler condition)
  (let ((hook standard-error-hook))
    (if hook
	(fluid-let ((standard-error-hook #f))
	  (hook condition))))
  (repl/start (push-repl 'INHERIT condition '() "error>")))

(define (standard-warning-handler condition)
  (let ((hook standard-warning-hook))
    (if hook
	(fluid-let ((standard-warning-hook #f))
	  (hook condition))
	(let ((port (notification-output-port)))
	  (fresh-line port)
	  (write-string ";Warning: " port)
	  (write-condition-report condition port)
	  (newline port)))))

(define standard-error-hook #f)
(define standard-warning-hook #f)

(define (condition-signaller type field-names default-handler)
  (guarantee-condition-handler default-handler 'CONDITION-SIGNALLER)
  (let ((make-condition (condition-constructor type field-names)))
    (lambda field-values
      (call-with-current-continuation
       (lambda (continuation)
	 (let ((condition
		(apply make-condition
		       (cons* continuation
			      'BOUND-RESTARTS
			      field-values))))
	   (signal-condition condition)
	   (default-handler condition)))))))

;; This is similar to condition-signaller, but error procedures
;; created with this allow substitution of the INDEXth argument by
;; using the USE-VALUE restart and allow retrying the operation by
;; using the RETRY restart.  The RETRY restart will return the
;; original irritant, while USE-VALUE will return a value prompted for.

(define (substitutable-value-condition-signaller
	 type field-names default-handler
	 index use-value-prompt use-value-message retry-message)
  (guarantee-condition-handler default-handler
			       'SUBSTITUTABLE-VALUE-CONDITION-SIGNALLER)
  (let ((make-condition (condition-constructor type field-names))
	(arity (length field-names)))
    (letrec
	((constructor
	  (lambda field-values
	    (if (not (= arity (length field-values)))
		(error:wrong-number-of-arguments constructor
						 arity
						 field-values))
	    (call-with-current-continuation
	     (lambda (continuation)
	       (let ((condition
		      (apply make-condition
			     (cons* continuation
				    'BOUND-RESTARTS
				    field-values))))
		 (with-restart 'USE-VALUE
		     (if (string? use-value-message)
			 use-value-message
			 (use-value-message condition))
		     continuation
		     (let ((prompt
			    (if (string? use-value-prompt)
				use-value-prompt
				(use-value-prompt condition))))
		       (lambda ()
			 (values (prompt-for-evaluated-expression prompt))))
		   (lambda ()
		     (with-restart 'RETRY
			 (if (string? retry-message)
			     retry-message
			     (retry-message condition))
			 (lambda ()
			   (continuation (list-ref field-values index)))
			 values
		       (lambda ()
			 (signal-condition condition)
			 (default-handler condition)))))))))))
      constructor)))

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
(define condition-type:floating-point-overflow)
(define condition-type:floating-point-underflow)
(define condition-type:illegal-datum)
(define condition-type:illegal-pathname-component)
(define condition-type:macro-binding)
(define condition-type:no-such-restart)
(define condition-type:port-error)
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

(define error:bad-range-argument)
(define error:datum-out-of-range)
(define error:divide-by-zero)
(define error:file-operation)
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
  (guarantee-condition-type type 'CONDITION-TYPE/ERROR?)
  (%condition-type/error? type))

(define (condition/error? condition)
  (guarantee-condition condition 'CONDITION/ERROR?)
  (%condition/error? condition))

(define-integrable (%condition/error? condition)
  (%condition-type/error? (%condition/type condition)))

(define-integrable (%condition-type/error? type)
  (memq condition-type:error (%condition-type/generalizations type)))

(define (initialize-package!)
  (set! hook/invoke-condition-handler default/invoke-condition-handler)
  ;; No eta conversion for bootstrapping and efficiency reasons.
  (set! hook/invoke-restart
	(lambda (effector arguments)
	  (apply effector arguments)))
  (set! condition-type:serious-condition
	(make-condition-type 'SERIOUS-CONDITION #f '() #f))
  (set! condition-type:warning
	(make-condition-type 'WARNING #f '() #f))

  (set! condition-type:error
	(make-condition-type 'ERROR condition-type:serious-condition '() #f))

  (let ((reporter/simple-condition
	 (lambda (condition port)
	   (format-error-message (access-condition condition 'MESSAGE)
				 (access-condition condition 'IRRITANTS)
				 port))))
    (set! condition-type:simple-condition
	  (make-condition-type 'SIMPLE-CONDITION #f '(MESSAGE IRRITANTS)
	    reporter/simple-condition))
    (set! condition-type:simple-error
	  (make-condition-type 'SIMPLE-ERROR condition-type:error
	      '(MESSAGE IRRITANTS)
	    reporter/simple-condition))
    (set! condition-type:simple-warning
	  (make-condition-type 'SIMPLE-WARNING condition-type:warning
	      '(MESSAGE IRRITANTS)
	    reporter/simple-condition)))

  (set! condition-type:illegal-datum
	(make-condition-type 'ILLEGAL-DATUM condition-type:error '(DATUM)
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'DATUM) port)
	    (write-string " has been found in an inappropriate context."
			  port))))

  (set! condition-type:datum-out-of-range
	(make-condition-type 'DATUM-OUT-OF-RANGE condition-type:illegal-datum
	    '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'DATUM) port)
	    (write-string " is not in the correct range." port))))

  (let ((write-type-description
	 (let ((char-set:vowels
		(char-set #\a #\e #\i #\o #\u #\A #\E #\I #\O #\U)))
	   (lambda (condition port)
	     (let ((type (access-condition condition 'TYPE)))
	       (if (string? type)
		   (begin
		     (if (not (or (string-null? type)
				  (string-prefix-ci? "a " type)
				  (string-prefix-ci? "an " type)))
			 (write-string
			  (if (char-set-member? char-set:vowels
						(string-ref type 0))
			      "an "
			      "a ")
			  port))
		     (write-string type port))
		   (write-string "the correct type" port))))))
	(write-operand-description
	 (lambda (condition port)
	   (let ((operator (access-condition condition 'OPERATOR))
		 (operand (access-condition condition 'OPERAND)))
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
	  (make-condition-type 'WRONG-TYPE-DATUM condition-type:illegal-datum
	      '(TYPE)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'DATUM) port)
	      (write-string " is not " port)
	      (write-type-description condition port)
	      (write-string "." port))))
    (set! condition-type:wrong-type-argument
	  (make-condition-type 'WRONG-TYPE-ARGUMENT
	      condition-type:wrong-type-datum
	      '(OPERATOR OPERAND)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'DATUM) port)
	      (write-operand-description condition port)
	      (write-string " is not " port)
	      (write-type-description condition port)
	      (write-string "." port))))
    (set! condition-type:bad-range-argument
	  (make-condition-type 'BAD-RANGE-ARGUMENT
	      condition-type:datum-out-of-range
	      '(OPERATOR OPERAND)
	    (lambda (condition port)
	      (write-string "The object " port)
	      (write (access-condition condition 'DATUM) port)
	      (write-operand-description condition port)
	      (write-string " is not in the correct range." port)))))

  (set! condition-type:wrong-number-of-arguments
	(make-condition-type 'WRONG-NUMBER-OF-ARGUMENTS
	    condition-type:wrong-type-datum
	    '(OPERANDS)
	  (lambda (condition port)
	    (let ((pluralize-argument
		   (lambda (number)
		     (write-string
		      (if (= number 1) " argument" " arguments")
		      port))))
	      (write-string "The procedure " port)
	      (write-operator (access-condition condition 'DATUM) port)
	      (write-string " has been called with " port)
	      (let ((count (length (access-condition condition 'OPERANDS))))
		(write count port)
		(pluralize-argument count))
	      (write-string "; it requires " port)
	      (let ((arity (access-condition condition 'TYPE)))
		(cond ((not (pair? arity))
		       (write-string "exactly " port)
		       (write arity port)
		       (pluralize-argument arity))
		      ((not (cdr arity))
		       (write-string "at least " port)
		       (write (car arity) port)
		       (pluralize-argument (car arity)))
		      ((= (car arity) (cdr arity))
		       (write-string "exactly " port)
		       (write (car arity) port)
		       (pluralize-argument (car arity)))
		      (else
		       (write-string "between " port)
		       (write (car arity) port)
		       (write-string " and " port)
		       (write (cdr arity) port)
		       (write-string " arguments" port))))
	      (write-char #\. port)))))

  (set! condition-type:illegal-pathname-component
	(make-condition-type 'ILLEGAL-PATHNAME-COMPONENT
	    condition-type:wrong-type-datum '()
	  (lambda (condition port)
	    (write-string "The object " port)
	    (write (access-condition condition 'DATUM) port)
	    (write-string " is not a valid pathname " port)
	    (write-string (access-condition condition 'TYPE) port)
	    (write-string "." port))))

  (set! condition-type:control-error
	(make-condition-type 'CONTROL-ERROR condition-type:error '()
	  "Control error."))

  (set! condition-type:no-such-restart
	(make-condition-type 'NO-SUCH-RESTART condition-type:control-error
	    '(NAME)
	  (lambda (condition port)
	    (write-string "The restart named " port)
	    (write (access-condition condition 'NAME) port)
	    (write-string " is not bound." port))))

  (let ((anonymous-error
	 (lambda (type-name field-name)
	   (make-condition-type type-name condition-type:error
	       (list field-name)
	     (lambda (condition port)
	       (write-string "Anonymous error associated with " port)
	       (write (access-condition condition field-name) port)
	       (write-string "." port))))))
    (set! condition-type:port-error (anonymous-error 'PORT-ERROR 'PORT))
    (set! condition-type:file-error (anonymous-error 'FILE-ERROR 'FILENAME))
    (set! condition-type:cell-error (anonymous-error 'CELL-ERROR 'LOCATION))
    (set! condition-type:thread-error (anonymous-error 'THREAD-ERROR 'THREAD)))

  (set! condition-type:derived-port-error
	(make-condition-type 'DERIVED-PORT-ERROR condition-type:port-error
	    '(CONDITION)
	  (lambda (condition port)
	    (write-string "The port " port)
	    (write (access-condition condition 'PORT) port)
	    (write-string " signalled an error:" port)
	    (newline port)
	    (write-condition-report (access-condition condition 'CONDITION)
				    port))))
  (set! error:derived-port
	(let ((make-condition
	       (condition-constructor condition-type:derived-port-error
				      '(PORT CONDITION))))
	  (lambda (port condition)
	    (guarantee-condition condition 'ERROR:DERIVED-PORT)
	    (error (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   port
				   condition)))))

  (set! condition-type:derived-file-error
	(make-condition-type 'DERIVED-FILE-ERROR condition-type:file-error
	    '(CONDITION)
	  (lambda (condition port)
	    (write-string "The file " port)
	    (write (access-condition condition 'FILENAME) port)
	    (write-string " signalled an error:" port)
	    (newline port)
	    (write-condition-report (access-condition condition 'CONDITION)
				    port))))
  (set! error:derived-file
	(let ((make-condition
	       (condition-constructor condition-type:derived-file-error
				      '(FILENAME CONDITION))))
	  (lambda (filename condition)
	    (guarantee-condition condition 'ERROR:DERIVED-FILE)
	    (error (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   filename
				   condition)))))

  (set! condition-type:derived-thread-error
	(make-condition-type 'DERIVED-THREAD-ERROR condition-type:thread-error
	    '(CONDITION)
	  (lambda (condition port)
	    (write-string "The thread " port)
	    (write (access-condition condition 'THREAD) port)
	    (write-string " signalled " port)
	    (let ((condition (access-condition condition 'CONDITION)))
	      (write-string (if (condition/error? condition)
				"an error"
				"a condition")
			    port)
	      (write-string ":" port)
	      (newline port)
	      (write-condition-report condition port)))))
  (set! error:derived-thread
	(let ((make-condition
	       (condition-constructor condition-type:derived-thread-error
				      '(THREAD CONDITION))))
	  (lambda (thread condition)
	    (guarantee-condition condition 'ERROR:DERIVED-THREAD)
	    (let ((condition
		   (make-condition (%condition/continuation condition)
				   (%condition/restarts condition)
				   thread
				   condition)))
	      (with-simple-restart 'CONTINUE "Continue from error."
		(lambda ()
		  (restart/put! (first-bound-restart)
				'ASSOCIATED-CONDITION
				condition)
		  (error condition)))))))
  (set! condition/derived-thread?
	(condition-predicate condition-type:derived-thread-error))

  (set! condition-type:file-operation-error
	(make-condition-type 'FILE-OPERATION-ERROR condition-type:file-error
	    '(VERB NOUN REASON OPERATOR OPERANDS)
	  (lambda (condition port)
	    (let ((noun (access-condition condition 'NOUN)))
	      (write-string "Unable to " port)
	      (write-string (access-condition condition 'VERB) port)
	      (write-string " " port)
	      (write-string noun port)
	      (write-string " " port)
	      (write (->namestring (access-condition condition 'FILENAME))
		     port)
	      (write-string " because: " port)
	      (let ((reason (access-condition condition 'REASON)))
		(if reason
		    (write-string (string-capitalize reason) port)
		    (begin
		      (write-string "No such " port)
		      (write-string noun port))))
	      (write-string "." port)))))
  (set! error:file-operation
	(let ((get-verb
	       (condition-accessor condition-type:file-operation-error 'VERB))
	      (get-noun
	       (condition-accessor condition-type:file-operation-error 'NOUN)))
	  (substitutable-value-condition-signaller
	   condition-type:file-operation-error
	   '(FILENAME VERB NOUN REASON OPERATOR OPERANDS)
	   standard-error-handler
	   0
	   (lambda (condition)
	     (string-append "New "
			    (get-noun condition)
			    " name (an expression to be evaluated)"))
	   (lambda (condition)
	     (string-append "Try to "
			    (get-verb condition)
			    " a different "
			    (get-noun condition)
			    "."))
	   (lambda (condition)
	     (string-append "Try to "
			    (get-verb condition)
			    " the same "
			    (get-noun condition)
			    " again.")))))

  (set! condition-type:variable-error
	(make-condition-type 'VARIABLE-ERROR condition-type:cell-error
	    '(ENVIRONMENT)
	  (lambda (condition port)
	    (write-string "Anonymous error associated with variable " port)
	    (write (access-condition condition 'LOCATION) port)
	    (write-string "." port))))

  (set! condition-type:unbound-variable
	(make-condition-type 'UNBOUND-VARIABLE condition-type:variable-error
	    '()
	  (lambda (condition port)
	    (write-string "Unbound variable: " port)
	    (write (access-condition condition 'LOCATION) port))))

  (set! condition-type:unassigned-variable
	(make-condition-type 'UNASSIGNED-VARIABLE condition-type:variable-error
	    '()
	  (lambda (condition port)
	    (write-string "Unassigned variable: " port)
	    (write (access-condition condition 'LOCATION) port))))

  (set! condition-type:macro-binding
	(make-condition-type 'MACRO-BINDING condition-type:variable-error '()
	  (lambda (condition port)
	    (write-string "Variable reference to a syntactic keyword: " port)
	    (write (access-condition condition 'LOCATION) port))))

  (let ((arithmetic-error-report
	 (lambda (description)
	   (lambda (condition port)
	     (write-string description port)
	     (let ((operator (access-condition condition 'OPERATOR)))
	       (if operator
		   (begin
		     (write-string " signalled by " port)
		     (write-operator operator port)
		     (write-string "." port))))))))
    (set! condition-type:arithmetic-error
	  (make-condition-type 'ARITHMETIC-ERROR condition-type:error
	      '(OPERATOR OPERANDS)
	    (arithmetic-error-report "Anonymous arithmetic error")))
    (set! condition-type:divide-by-zero
	  (make-condition-type 'DIVIDE-BY-ZERO condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Division by zero")))
    (set! condition-type:floating-point-overflow
	  (make-condition-type 'FLOATING-POINT-OVERFLOW
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Floating-point overflow")))
    (set! condition-type:floating-point-underflow
	  (make-condition-type 'FLOATING-POINT-UNDERFLOW
	      condition-type:arithmetic-error
	      '()
	    (arithmetic-error-report "Floating-point underflow"))))

  (set! make-simple-error
	(condition-constructor condition-type:simple-error
			       '(MESSAGE IRRITANTS)))
  (set! make-simple-warning
	(condition-constructor condition-type:simple-warning
			       '(MESSAGE IRRITANTS)))

  (set! error:wrong-type-datum
	(condition-signaller condition-type:wrong-type-datum
			     '(DATUM TYPE)
			     standard-error-handler))
  (set! error:datum-out-of-range
	(condition-signaller condition-type:datum-out-of-range
			     '(DATUM)
			     standard-error-handler))
  (set! error:wrong-type-argument
	(condition-signaller condition-type:wrong-type-argument
			     '(DATUM TYPE OPERATOR)
			     standard-error-handler))
  (set! error:bad-range-argument
	(condition-signaller condition-type:bad-range-argument
			     '(DATUM OPERATOR)
			     standard-error-handler))
  (set! error:wrong-number-of-arguments
	(condition-signaller condition-type:wrong-number-of-arguments
			     '(DATUM TYPE OPERANDS)
			     standard-error-handler))
  (set! error:illegal-pathname-component
	(condition-signaller condition-type:illegal-pathname-component
			     '(DATUM TYPE)
			     standard-error-handler))
  (set! error:divide-by-zero
	(condition-signaller condition-type:divide-by-zero
			     '(OPERATOR OPERANDS)
			     standard-error-handler))
  (set! error:no-such-restart
	(condition-signaller condition-type:no-such-restart
			     '(NAME)
			     standard-error-handler))
  (set! error:unassigned-variable
	(condition-signaller condition-type:unassigned-variable
			     '(ENVIRONMENT LOCATION)
			     standard-error-handler))
  (set! error:unbound-variable
	(condition-signaller condition-type:unbound-variable
			     '(ENVIRONMENT LOCATION)
			     standard-error-handler))
  (set! error:macro-binding
	(condition-signaller condition-type:macro-binding
			     '(ENVIRONMENT LOCATION)
			     standard-error-handler))

  unspecific)

;;;; Utilities

(define (format-error-message message irritants port)
  (fluid-let ((*unparser-list-depth-limit* 2)
	      (*unparser-list-breadth-limit* 5))
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
				 'ORDINAL-NUMBER-STRING))
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

(define-integrable (guarantee-list-of-symbols object operator)
  (if (not (list-of-symbols? object))
      (error:wrong-type-argument object "list of unique symbols" operator)))

(define (list-of-symbols? object)
  (and (list? object)
       (let loop ((field-names object))
	 (or (not (pair? field-names))
	     (and (symbol? (car field-names))
		  (not (memq (car field-names) (cdr field-names)))
		  (loop (cdr field-names)))))))

(define-integrable (guarantee-keyword-association-list object operator)
  (if (not (keyword-association-list? object))
      (error:wrong-type-argument object "keyword association list" operator)))

(define (keyword-association-list? object)
  (and (list? object)
       (let loop ((l object) (symbols '()))
	 (or (not (pair? l))
	     (and (symbol? (car l))
		  (not (memq (car l) symbols))
		  (pair? (cdr l))
		  (loop (cddr l) (cons (car l) symbols)))))))

(define-integrable (procedure-of-arity? object arity)
  (and (procedure? object)
       (procedure-arity-valid? object arity)))

(define-integrable (guarantee-symbol object operator)
  (if (not (symbol? object))
      (error:wrong-type-argument object "symbol" operator)))

(define-integrable (guarantee-continuation object operator)
  (if (not (continuation? object))
      (error:wrong-type-argument object "continuation" operator)))

(define-integrable (guarantee-condition-type object operator)
  (if (not (condition-type? object))
      (error:wrong-type-argument object "condition type" operator)))

(define-integrable (guarantee-condition-types object operator)
  (if (not (and (list? object) (for-all? object condition-type?)))
      (error:wrong-type-argument object "list of condition types" operator)))

(define-integrable (guarantee-condition object operator)
  (if (not (condition? object))
      (error:wrong-type-argument object "condition" operator)))

(define-integrable (guarantee-condition-handler object operator)
  (if (not (procedure-of-arity? object 1))
      (error:wrong-type-argument object "procedure of one argument" operator)))

(define-integrable (guarantee-restart object operator)
  (if (not (restart? object))
      (error:wrong-type-argument object "restart" operator)))

(define-integrable (guarantee-restarts object operator)
  (if (not (and (list? object) (for-all? object restart?)))
      (error:wrong-type-argument object "list of restarts" operator)))