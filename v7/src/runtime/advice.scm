#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/advice.scm,v 14.3 1988/12/30 06:41:58 cph Rel $

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

;;;; Advice package
;;; package: (runtime advice)

(declare (usual-integrations))

(define (initialize-package!)
  (set! entry-advice-population (make-population))
  (set! exit-advice-population (make-population))
  (set! particular-entry-advisor (particular-advisor primitive-advise-entry))
  (set! particular-exit-advisor (particular-advisor primitive-advise-exit))
  (set! particular-both-advisor primitive-advise-both)
  (set! particular-entry-unadvisor primitive-unadvise-entry)
  (set! particular-exit-unadvisor primitive-unadvise-exit)
  (set! particular-both-unadvisor primitive-unadvise-both)
  (set! primitive-trace-entry (particular-entry-advisor trace-entry-advice))
  (set! primitive-trace-exit (particular-exit-advisor trace-exit-advice))
  (set! primitive-trace-both
	(particular-both-advisor trace-entry-advice trace-exit-advice))
  (set! primitive-untrace
	(particular-both-unadvisor trace-entry-advice trace-exit-advice))
  (set! primitive-untrace-entry
	(particular-entry-unadvisor trace-entry-advice))
  (set! primitive-untrace-exit (particular-exit-unadvisor trace-exit-advice))
  (set! primitive-break-entry (particular-entry-advisor break-entry-advice))
  (set! primitive-break-exit (particular-exit-advisor break-exit-advice))
  (set! primitive-break-both
	(particular-both-advisor break-entry-advice break-exit-advice))
  (set! primitive-unbreak
	(particular-both-unadvisor break-entry-advice break-exit-advice))
  (set! primitive-unbreak-entry
	(particular-entry-unadvisor break-entry-advice))
  (set! primitive-unbreak-exit (particular-exit-unadvisor break-exit-advice))
  (set! advice (wrap-advice-extractor primitive-advice))
  (set! entry-advice (wrap-advice-extractor primitive-entry-advice))
  (set! exit-advice (wrap-advice-extractor primitive-exit-advice))
  (set! advise-entry (wrap-general-advisor primitive-advise-entry))
  (set! advise-exit (wrap-general-advisor primitive-advise-exit))
  (set! wrap-entry-unadvisor
	(wrap-unadvisor
	 (lambda (operation)
	   (map-over-population entry-advice-population operation))))
  (set! wrap-exit-unadvisor
	(wrap-unadvisor
	 (lambda (operation)
	   (map-over-population exit-advice-population operation))))
  (set! wrap-both-unadvisor
	(wrap-unadvisor
	 (lambda (operation)
	   (map-over-population entry-advice-population operation)
	   (map-over-population exit-advice-population operation))))
  (set! unadvise (wrap-both-unadvisor primitive-unadvise-entire-lambda))
  (set! unadvise-entry (wrap-entry-unadvisor primitive-unadvise-entire-entry))
  (set! unadvise-exit (wrap-exit-unadvisor primitive-unadvise-entire-exit))
  (set! untrace (wrap-both-unadvisor primitive-untrace))
  (set! untrace-entry (wrap-entry-unadvisor primitive-untrace-entry))
  (set! untrace-exit (wrap-exit-unadvisor primitive-untrace-exit))
  (set! unbreak (wrap-both-unadvisor primitive-unbreak))
  (set! unbreak-entry (wrap-entry-unadvisor primitive-unbreak-entry))
  (set! unbreak-exit (wrap-exit-unadvisor primitive-unbreak-exit))
  (set! trace-entry (wrap-advisor primitive-trace-entry))
  (set! trace-exit (wrap-advisor primitive-trace-exit))
  (set! trace-both (wrap-advisor primitive-trace-both))
  (set! trace trace-both)
  (set! break-entry (wrap-advisor primitive-break-entry))
  (set! break-exit (wrap-advisor primitive-break-exit))
  (set! break-both (wrap-advisor primitive-break-both))
  (set! break break-both))

;;;; Advice Wrappers

(define entry-advice-population)
(define exit-advice-population)

(define the-arguments)
(define the-procedure)
(define the-result)

(define (*args*)
  the-arguments)

(define (*proc*)
  the-procedure)

(define (*result*)
  the-result)

(define (add-lambda-advice! lambda advice-transformation)
  (lambda-wrap-body! lambda
    (lambda (body state receiver)
      (if (null? state)
	  (receiver (make-advice-hook)
		    (advice-transformation '() '() cons))
	  (receiver body
		    (advice-transformation (car state) (cdr state) cons))))))

(define (remove-lambda-advice! lambda advice-transformation)
  (lambda-advice lambda
    (lambda (entry-advice exit-advice)
      (advice-transformation entry-advice exit-advice
	(lambda (new-entry-advice new-exit-advice)
	  (if (and (null? new-entry-advice) (null? new-exit-advice))
	      (lambda-unwrap-body! lambda)
	      (lambda-wrap-body! lambda
		(lambda (body state receiver)
		  state
		  (receiver body
			    (cons new-entry-advice new-exit-advice))))))))))

(define (lambda-advice lambda receiver)
  (lambda-wrapper-components lambda
    (lambda (original-body state)
      original-body
      (if (null? state)
	  (error "Procedure has no advice -- LAMBDA-ADVICE" lambda))
      (receiver (car state) (cdr state)))))

(define (make-advice-hook)
  (make-combination syntaxed-advice-procedure
		    (list (make-the-environment))))

(define syntaxed-advice-procedure
  (scode-quote
   ((ACCESS PACKAGE/REFERENCE #F)
    ((ACCESS FIND-PACKAGE #F) '(RUNTIME ADVICE))
    'ADVISED-PROCEDURE-WRAPPER)))

;;;; The Advice Hook

;;; This procedure is called with the newly-created environment as its
;;; argument.

;;; Doing (PROCEED) from within entry or exit advice will cause that
;;; particular piece of advice to be terminated, but any remaining
;;; advice to be executed.  Doing (PROCEED value), however,
;;; immediately terminates all advice and returns VALUE as if the
;;; procedure called had generated the value.  Returning from a piece
;;; of exit advice is equivalent to doing (PROCEED value) from it.

(define (advised-procedure-wrapper environment)
  (let ((procedure (ic-environment/procedure environment))
	(arguments (ic-environment/arguments environment)))
    (lambda-wrapper-components (procedure-lambda procedure)
      (lambda (original-body state)
	(call-with-current-continuation
	 (lambda (continuation)

	   (define ((catching-proceeds receiver) advice)
	     (with-proceed-point
	      (lambda (proceed-continuation values)
		(if (null? values)
		    (proceed-continuation '())
		    (continuation (car values))))
	      (lambda ()
		(receiver advice))))

	   (for-each (catching-proceeds
		      (lambda (advice)
			(advice procedure arguments environment)))
		     (car state))
	   (let ((value (scode-eval original-body environment)))
	     (for-each (catching-proceeds
			(lambda (advice)
			  (set! value
				(advice procedure
					arguments
					value
					environment))))
		       (cdr state))
	     value)))))))

;;;; Primitive Advisors

(define (primitive-advice lambda)
  (lambda-advice lambda list))

(define (primitive-entry-advice lambda)
  (lambda-advice lambda
    (lambda (entry-advice exit-advice)
      exit-advice
      entry-advice)))

(define (primitive-exit-advice lambda)
  (lambda-advice lambda
    (lambda (entry-advice exit-advice)
      entry-advice
      exit-advice)))

(define (primitive-advise-entry lambda advice)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver (if (memq advice entry-advice)
		    entry-advice
		    (begin (add-to-population! entry-advice-population lambda)
			   (cons advice entry-advice)))
		exit-advice))))

(define (primitive-advise-exit lambda advice)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver entry-advice
		(if (memq advice exit-advice)
		    exit-advice
		    (begin (add-to-population! exit-advice-population lambda)
			   (append! exit-advice (list advice))))))))

(define ((primitive-advise-both new-entry-advice new-exit-advice) lambda)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver (if (memq new-entry-advice entry-advice)
		    entry-advice
		    (begin (add-to-population! entry-advice-population lambda)
			   (cons new-entry-advice entry-advice)))
		(if (memq new-exit-advice exit-advice)
		    exit-advice
		    (begin (add-to-population! exit-advice-population lambda)
			   (append! exit-advice (list new-exit-advice))))))))

(define (eq?-adjoin object list)
  (if (memq object list)
      list
      (cons object list)))

(define (primitive-unadvise-entire-entry lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      entry-advice
      (receiver '() exit-advice)))
  (remove-from-population! entry-advice-population lambda))

(define (primitive-unadvise-entire-exit lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      exit-advice
      (receiver entry-advice '())))
  (remove-from-population! exit-advice-population lambda))

(define (primitive-unadvise-entire-lambda lambda)
  (lambda-unwrap-body! lambda)
  (remove-from-population! entry-advice-population lambda)
  (remove-from-population! exit-advice-population lambda))

(define ((primitive-unadvise-entry advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-entry-advice (delq! advice entry-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population lambda))
	(receiver new-entry-advice exit-advice)))))

(define ((primitive-unadvise-exit advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-exit-advice (delq! advice exit-advice)))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population lambda))
	(receiver entry-advice new-exit-advice)))))

(define ((primitive-unadvise-both old-entry-advice old-exit-advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-entry-advice (delq! old-entry-advice entry-advice))
	    (new-exit-advice (delq! old-exit-advice exit-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population lambda))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population lambda))
	(receiver new-entry-advice new-exit-advice)))))

(define (((particular-advisor advisor) advice) lambda)
  (advisor lambda advice))

(define particular-entry-advisor)
(define particular-exit-advisor)
(define particular-both-advisor)
(define particular-entry-unadvisor)
(define particular-exit-unadvisor)
(define particular-both-unadvisor)

;;;; Trace

(define (trace-entry-advice procedure arguments environment)
  environment
  (trace-display procedure arguments))

(define (trace-exit-advice procedure arguments result environment)
  environment
  (trace-display procedure arguments result)
  result)

(define (trace-display procedure arguments #!optional result)
  (newline)
  (let ((width (- (output-port/x-size (current-output-port)) 3)))
    (let ((output
	   (with-output-to-truncated-string
	    width
	    (lambda ()
	      (if (default-object? result)
		  (write-string "[Entering ")
		  (begin (write-string "[")
			 (write result)
			 (write-string " <== ")))
	      (write-string "<")
	      (write procedure)
	      (for-each (lambda (arg) (write-char #\Space) (write arg))
			arguments)))))
      (if (car output)			; Too long?
	  (begin
	   (write-string (substring (cdr output) 0 (- width 5)))
	   (write-string " ... "))
	  (write-string (cdr output)))))
  (write-string ">]"))
(define primitive-trace-entry)
(define primitive-trace-exit)
(define primitive-trace-both)
(define primitive-untrace)
(define primitive-untrace-entry)
(define primitive-untrace-exit)

;;;; Break

(define (break-rep environment message . info)
  (breakpoint (cmdl-message/append
	       (cmdl-message/active (lambda () (apply trace-display info)))
	       (cmdl-message/standard message))
	      environment))

(define (break-entry-advice procedure arguments environment)
  (fluid-let ((the-procedure procedure)
	      (the-arguments arguments))
    (break-rep environment "Breakpoint on entry" procedure arguments)))

(define (break-exit-advice procedure arguments result environment)
  (fluid-let ((the-procedure procedure)
	      (the-arguments arguments)
	      (the-result result))
    (break-rep environment "Breakpoint on exit" procedure arguments result))
  result)

(define primitive-break-entry)
(define primitive-break-exit)
(define primitive-break-both)
(define primitive-unbreak)
(define primitive-unbreak-entry)
(define primitive-unbreak-exit)

;;;; Top Level Wrappers

(define (find-internal-lambda procedure path)
  (define (find-lambda lambda path)
    (define (loop elements)
      (cond ((null? elements)
	     (error "Couldn't find internal definition" path))
	    ((assignment? (car elements))
	     (assignment-components (car elements)
	       (lambda (name value)
		 (if (eq? name (car path))
		     (if (lambda? value)
			 (find-lambda value (cdr path))
			 (error "Internal definition not a procedure" path))
		     (loop (cdr elements))))))
	    (else
	     (loop (cdr elements)))))

    (if (null? path)
	lambda
	(lambda-components lambda
	  (lambda (name required optional rest auxiliary declarations body)
	    name required optional rest declarations
	    (if (memq (car path) auxiliary)
		(loop (sequence-actions body))
		(error "No internal definition by this name" (car path)))))))

  (if (null? path)
      (procedure-lambda procedure)
      (find-lambda (procedure-lambda procedure) (car path))))

;; The LIST-COPY will prevent any mutation problems.
(define ((wrap-advice-extractor extractor) procedure . path)
  (list-copy (extractor (find-internal-lambda procedure path))))

(define advice)
(define entry-advice)
(define exit-advice)

(define ((wrap-general-advisor advisor) procedure advice . path)
  (advisor (find-internal-lambda procedure path) advice)
  unspecific)

(define advise-entry)
(define advise-exit)

(define (((wrap-unadvisor map-over-population) unadvisor) . procedure&path)
  (if (null? procedure&path)
      (map-over-population unadvisor)
      (unadvisor (find-internal-lambda (car procedure&path)
				       (cdr procedure&path))))
  unspecific)

(define wrap-entry-unadvisor)
(define wrap-exit-unadvisor)
(define wrap-both-unadvisor)
(define unadvise)
(define unadvise-entry)
(define unadvise-exit)
(define untrace)
(define untrace-entry)
(define untrace-exit)
(define unbreak)
(define unbreak-entry)
(define unbreak-exit)

(define ((wrap-advisor advisor) procedure . path)
  (advisor (find-internal-lambda procedure path))
  unspecific)

(define trace-entry)
(define trace-exit)
(define trace-both)
(define trace)
(define break-entry)
(define break-exit)
(define break-both)
(define break)