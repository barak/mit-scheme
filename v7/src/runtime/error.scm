#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/error.scm,v 14.2 1988/06/21 05:48:19 cph Exp $

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

;;;; Error System
;;; package: (runtime error-handler)

(declare (usual-integrations))

(define (initialize-package!)
  (set! next-condition-type-index 0)
  (set! handler-frames false)
  (set! condition-type:error
	(let ((generalizations (list false)))
	  (let ((result
		 (%make-condition-type generalizations
				       true
				       condition-reporter/default)))
	    (set-car! generalizations result)
	    result)))
  (set! error-type:vanilla
	(make-condition-type (list condition-type:error)
			     condition-reporter/default))
  (set! hook/error-handler default/error-handler)
  (set! hook/error-decision default/error-decision)
  (let ((fixed-objects (get-fixed-objects-vector)))
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'ERROR-PROCEDURE)
		 error-procedure-handler)
    (vector-set! fixed-objects
		 (fixed-objects-vector-slot 'COMPILER-ERROR-PROCEDURE)
		 error-from-compiled-code)
    ((ucode-primitive set-fixed-objects-vector!) fixed-objects)))

(define (error-procedure-handler message irritants environment)
  (with-proceed-point proceed-value-filter
    (lambda ()
      (simple-error
       environment
       message
       ;; Kludge to support minimal upwards compatibility with `error'
       ;; forms syntaxed by older syntaxer.  Should be flushed after
       ;; new runtime system has been in use for a while.
       (cond ((eq? irritants *the-non-printing-object*) '())
	     ((or (null? irritants) (pair? irritants)) irritants)
	     (else (list irritants)))))))

(define (error-from-compiled-code message . irritants)
  (with-proceed-point proceed-value-filter
    (lambda ()
      (simple-error repl-environment message irritants))))

;;; (PROCEED) means retry error expression, (PROCEED value) means
;;; return VALUE as the value of the error subproblem.

(define (proceed-value-filter continuation values)
  (let ((next-subproblem
	 (and (not (null? values))
	      (continuation/first-subproblem continuation))))
    (if next-subproblem
	((stack-frame->continuation next-subproblem) (car values))
	(continuation *the-non-printing-object*))))

(define (simple-error environment message irritants)
  (signal-error
   (if (condition-type? message)
       (make-error-condition message irritants environment)
       ;; This handles old and "vanilla" errors.
       (let ((condition
	      (make-error-condition error-type:vanilla
				    irritants
				    environment)))
	 (if (string? message)
	     (1d-table/put! (condition/properties condition)
			    message-tag
			    message))
	 condition))))

(define (make-error-condition condition-type irritants environment)
  ;; Microcode errors also use this.
  (let ((condition
	 (make-condition condition-type
			 irritants
			 (current-proceed-continuation))))
    (1d-table/put! (condition/properties condition)
		   environment-tag
		   (if (eq? environment repl-environment)
		       (cons (standard-repl-environment) true)
		       (cons environment false)))
    condition))

(define message-tag
  "message-tag")

(define environment-tag
  "environment-tag")

(define repl-environment
  "repl-environment")

(define error-type:vanilla)

(define (condition-reporter/default condition port)
  (format-error-message (condition/message condition)
			(condition/irritants condition)
			port))

(define (condition/message condition)
  (or (1d-table/get (condition/properties condition) message-tag false)
      (1d-table/get (condition-type/properties (condition/type condition))
		    message-tag
		    "Anonymous error")))

(define-integrable (condition/environment condition)
  (car (1d-table/get (condition/properties condition) environment-tag false)))

(define-integrable (condition/substitute-environment? condition)
  (cdr (1d-table/get (condition/properties condition) environment-tag false)))

;;;; Standard Error Handler

(define (standard-error-handler condition)
  (fluid-let ((*error-condition* condition))
    (hook/error-handler condition)))

(define hook/error-handler)
(define (default/error-handler condition)
  (push-repl (condition/environment condition)
	     (let ((message
		    (cmdl-message/append
		     (cmdl-message/strings (condition/report-string condition))
		     (cmdl-message/active hook/error-decision))))
	       (if (condition/substitute-environment? condition)
		   (cmdl-message/append
		    message
		    (cmdl-message/strings
		     ""
		     "There is no environment available;"
		     "using the current REPL environment"))
		   message))
	     "Error->"))

(define hook/error-decision)
(define (default/error-decision)
  false)

(define *error-condition* false)

(define-integrable (error-condition)
  *error-condition*)

(define (error-continuation)
  (let ((condition (error-condition)))
    (and condition
	 (condition/continuation condition))))

(define-integrable (error-message)
  (condition/message (error-condition)))

(define-integrable (error-irritants)
  (condition/irritants (error-condition)))

;;;; Error Messages

(define (warn string . irritants)
  (let ((port (cmdl/output-port (nearest-cmdl))))
    (newline port)
    (write-string "Warning: " port)
    (format-error-message string irritants port)))

(define-integrable (error-irritants/sans-noise)
  (list-transform-negative (error-irritants)
    error-irritant/noise?))

(define (error-irritant)
  (let ((irritants (error-irritants/sans-noise)))
    (cond ((null? irritants) *the-non-printing-object*)
	  ((null? (cdr irritants)) (car irritants))
	  (else irritants))))

(define (cmdl-message/error string . irritants)
  (cmdl-message/strings
   (if (null? irritants)
       string
       (with-output-to-string
	 (lambda ()
	   (format-error-message string irritants (current-output-port)))))))

(define (format-error-message message irritants port)
  (fluid-let ((*unparser-list-depth-limit* 2)
	      (*unparser-list-breadth-limit* 5))
    (for-each (lambda (irritant)
		(if (error-irritant/noise? irritant)
		    (display (error-irritant/noise-value irritant) port)
		    (begin
		      (write-char #\Space port)
		      (write irritant port))))
	      (cons (if (string? message)
			(error-irritant/noise message)
			message)
		    irritants))))

(define-integrable (error-irritant/noise noise)
  (cons error-irritant/noise-tag noise))

(define (error-irritant/noise? irritant)
  (and (pair? irritant)
       (eq? (car irritant) error-irritant/noise-tag)))

(define-integrable (error-irritant/noise-value irritant)
  (cdr irritant))

(define error-irritant/noise-tag
  "error-irritant/noise")

;;;; Condition Types

(define-structure (condition-type
		   (constructor %make-condition-type
				(generalizations error? reporter))
		   (conc-name condition-type/))
  ;; `generalizations' is sorted in decreasing `index' order.
  (generalizations false read-only true)
  (error? false read-only true)
  (reporter false read-only true)
  (index (allocate-condition-type-index!) read-only true)
  (properties (make-1d-table) read-only true))

(define (make-condition-type generalizations reporter)
  (for-each guarantee-condition-type generalizations)
  (let ((generalizations
	 (cons false
	       (reduce generalizations/union
		       '()
		       (map condition-type/generalizations generalizations)))))
    (let ((result
	   (%make-condition-type
	    generalizations
	    (if (memq condition-type:error generalizations) true false)
	    (if (string? reporter) condition-reporter/default reporter))))
      (set-car! generalizations result)
      (if (string? reporter)
	  (1d-table/put! (condition-type/properties result)
			 message-tag
			 reporter))
      result)))

(define (allocate-condition-type-index!)
  (let ((index next-condition-type-index))
    (set! next-condition-type-index (1+ index))
    index))

(define next-condition-type-index)

(define (guarantee-condition-type object)
  (if (not (condition-type? object)) (error "Illegal condition-type" object))
  object)

(define-integrable (condition-type<? x y)
  (< (condition-type/index x) (condition-type/index y)))

(define (generalizations/union x y)
  ;; This takes advantage of (and preserves) the ordering of generalizations.
  (cond ((null? x) y)
	((null? y) x)
	((eq? (car x) (car y))
	 (cons (car x) (generalizations/union (cdr x) (cdr y))))
	((condition-type<? (car x) (car y))
	 (cons (car y) (generalizations/union x (cdr y))))
	(else
	 (cons (car x) (generalizations/union (cdr x) y)))))

(define (generalizations/intersect? x y)
  (cond ((or (null? x) (null? y)) false)
	((eq? (car x) (car y)) true)
	((condition-type<? (car x) (car y))
	 (generalizations/intersect? x (cdr y)))
	(else
	 (generalizations/intersect? (cdr x) y))))

(define (make-error-type generalizations reporter)
  (make-condition-type
   (if (there-exists? generalizations condition-type/error?)
       generalizations
       (cons condition-type:error generalizations))
   reporter))

(define (error-type? object)
  (and (condition-type? object)
       (condition-type/error? object)))

(define condition-type:error)

;;;; Condition Instances

(define-structure (condition
		   (constructor %make-condition (type irritants continuation))
		   (conc-name condition/))
  (type false read-only true)
  (irritants false read-only true)
  (continuation false read-only true)
  (properties (make-1d-table) read-only true))

(define (make-condition type irritants continuation)
  (guarantee-condition-type type)
  (if (not (list? irritants))
      (error "Illegal condition irritants" irritants))
  (guarantee-continuation continuation)
  (%make-condition type irritants continuation))

(define (guarantee-condition object)
  (if (not (condition? object)) (error "Illegal condition" object))
  object)

(define-integrable (condition/internal? condition)
  ;; For future expansion.
  false)

(define-integrable (condition/generalizations condition)
  (condition-type/generalizations (condition/type condition)))

(define-integrable (condition/error? condition)
  (condition-type/error? (condition/type condition)))

(define-integrable (condition/reporter condition)
  (condition-type/reporter (condition/type condition)))

(define (error? object)
  (and (condition? object)
       (condition/error? object)))

(define (condition/write-report condition #!optional port)
  ((condition/reporter condition)
   condition
   (if (default-object? port)
       (current-output-port)
       (guarantee-output-port port))))

(define (condition/report-string condition)
  (with-output-to-string
    (lambda ()
      ((condition/reporter condition) condition (current-output-port)))))

;;;; Condition Handling

(define handler-frames)

(define-structure (handler-frame (type structure)
				 (conc-name handler-frame/))
  (condition-types false read-only true)
  (handler false read-only true)
  (next false read-only true))

(define (bind-condition-handler condition-types handler thunk)
  (for-each guarantee-condition-type condition-types)
  (fluid-let ((handler-frames
	       (make-handler-frame condition-types
				   handler
				   handler-frames)))
    (thunk)))

(define-integrable (signal-error condition)
  (signal-condition condition standard-error-handler))

(define (signal-condition condition #!optional default-handler)
  (guarantee-condition condition)
  (let ((condition-type (condition/type condition)))
    (let ((generalizations (condition-type/generalizations condition-type)))
      (or (scan-handler-frames handler-frames generalizations
	    (lambda (frame)
	      (fluid-let ((handler-frames (handler-frame/next frame)))
		((handler-frame/handler frame) condition))))
	  (and (not (default-object? default-handler))
	       (fluid-let ((handler-frames false))
		 (default-handler condition)))))))

(define (scan-handler-frames frames generalizations try-frame)
  (let loop ((frame frames))
    (and frame
	 (or (and (let ((condition-types
			 (handler-frame/condition-types frame)))
		    (or (null? condition-types)
			(generalizations/intersect? generalizations
						    condition-types)))
		  (try-frame frame))
	     (loop (handler-frame/next frame))))))