;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/advice.scm,v 13.44 1987/06/30 20:58:10 cph Rel $
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

;;;; Advice package

(declare (usual-integrations))

(define advice-package
  (make-environment

(define the-args)
(define the-procedure)
(define the-result)

(define (*args*)
  the-args)

(define (*proc*)
  the-procedure)

(define (*result*)
  the-result)

(define entry-advice-population
  (make-population))

(define exit-advice-population
  (make-population))

;;;; Advice Wrappers

(define (add-lambda-advice! lambda advice-transformation)
  ((access lambda-wrap-body! lambda-package) lambda
    (lambda (body state cont)
      (if (null? state)
	  (cont (make-advice-hook)
		(advice-transformation '() '() cons))
	  (cont body
		(advice-transformation (car state) (cdr state) cons))))))

(define (remove-lambda-advice! lambda advice-transformation)
  (lambda-advice lambda
    (lambda (entry-advice exit-advice)
      (advice-transformation entry-advice exit-advice
	(lambda (new-entry-advice new-exit-advice)
	  (if (and (null? new-entry-advice)
		   (null? new-exit-advice))
	      ((access lambda-unwrap-body! lambda-package) lambda)
	      ((access lambda-wrap-body! lambda-package) lambda
		(lambda (body state cont)
		  (cont body (cons new-entry-advice new-exit-advice))))))))))

(define (lambda-advice lambda cont)
  ((access lambda-wrapper-components lambda-package) lambda
    (lambda (original-body state)
      (if (null? state)
	  (error "Procedure has no advice -- LAMBDA-ADVICE" lambda)
	  (cont (car state)
		(cdr state))))))

(define (make-advice-hook)
  (make-combination syntaxed-advice-procedure
		    (list (make-the-environment))))

(define syntaxed-advice-procedure
  (scode-quote
   (ACCESS ADVISED-PROCEDURE-WRAPPER ADVICE-PACKAGE '())))

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
  (let ((procedure (environment-procedure environment))
	(arguments (environment-arguments environment)))
    ((access lambda-wrapper-components lambda-package)
     (procedure-lambda procedure)
     (lambda (original-body state)
       (call-with-current-continuation
	 (lambda (continuation)

	   (define ((catching-proceeds receiver) advice)
	     (with-proceed-point
	      (lambda (value)
		(if (null? value)
		    '()
		    (continuation (car value))))
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
      entry-advice)))

(define (primitive-exit-advice lambda)
  (lambda-advice lambda
    (lambda (entry-advice exit-advice)
      exit-advice)))

(define (primitive-advise-entry lambda advice)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (cont (if (memq advice entry-advice)
		entry-advice
		(begin (add-to-population! entry-advice-population lambda)
		       (cons advice entry-advice)))
	    exit-advice))))

(define (primitive-advise-exit lambda advice)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (cont entry-advice
	    (if (memq advice exit-advice)
		exit-advice
		(begin (add-to-population! exit-advice-population lambda)
		       (append! exit-advice (list advice))))))))

(define ((primitive-advise-both new-entry-advice new-exit-advice) lambda)
  (add-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (cont (if (memq new-entry-advice entry-advice)
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
    (lambda (entry-advice exit-advice cont)
      (cont '() exit-advice)))
  (remove-from-population! entry-advice-population lambda))

(define (primitive-unadvise-entire-exit lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (cont entry-advice '())))
  (remove-from-population! exit-advice-population lambda))

(define (primitive-unadvise-entire-lambda lambda)
  ((access lambda-unwrap-body! lambda-package) lambda)
  (remove-from-population! entry-advice-population lambda)
  (remove-from-population! exit-advice-population lambda))

(define ((primitive-unadvise-entry advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (let ((new-entry-advice (delq! advice entry-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population lambda))
	(cont new-entry-advice exit-advice)))))

(define ((primitive-unadvise-exit advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (let ((new-exit-advice (delq! advice exit-advice)))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population lambda))
	(cont entry-advice new-exit-advice)))))

(define ((primitive-unadvise-both old-entry-advice old-exit-advice) lambda)
  (remove-lambda-advice! lambda
    (lambda (entry-advice exit-advice cont)
      (let ((new-entry-advice (delq! old-entry-advice entry-advice))
	    (new-exit-advice (delq! old-exit-advice exit-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population lambda))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population lambda))
	(cont new-entry-advice new-exit-advice)))))

(define (((particular-advisor advisor) advice) lambda)
  (advisor lambda advice))

(define particular-entry-advisor (particular-advisor primitive-advise-entry))
(define particular-exit-advisor (particular-advisor primitive-advise-exit))
(define particular-both-advisor primitive-advise-both)
(define particular-entry-unadvisor primitive-unadvise-entry)
(define particular-exit-unadvisor primitive-unadvise-exit)
(define particular-both-unadvisor primitive-unadvise-both)

;;;; Trace

(define (trace-entry-advice proc args env)
  (trace-display proc args))

(define (trace-exit-advice proc args result env)
  (trace-display proc args result)
  result)

(define (trace-display proc args #!optional result)
  (newline)
  (let ((width (- (access printer-width implementation-dependencies) 3)))
    (let ((output
	   (with-output-to-truncated-string
	    width
	    (lambda ()
	      (if (unassigned? result)
		  (write-string "[Entering ")
		  (begin (write-string "[")
			 (write result)
			 (write-string " <== ")))
	      (write-string "<")
	      (write proc)
	      (for-each (lambda (arg) (write-char #\Space) (write arg))
			args)))))
      (if (car output)			; Too long?
	  (begin
	   (write-string (substring (cdr output) 0 (- width 5)))
	   (write-string " ... "))
	  (write-string (cdr output)))))
  (write-string ">]"))

(define primitive-trace-entry
  (particular-entry-advisor trace-entry-advice))

(define primitive-trace-exit
  (particular-exit-advisor trace-exit-advice))

(define primitive-trace-both
  (particular-both-advisor trace-entry-advice trace-exit-advice))

(define primitive-untrace
  (particular-both-unadvisor trace-entry-advice trace-exit-advice))

(define primitive-untrace-entry
  (particular-entry-unadvisor trace-entry-advice))

(define primitive-untrace-exit
  (particular-exit-unadvisor trace-exit-advice))

;;;; Break

(define (break-rep env message . info)
  (push-rep env
	    (lambda ()
	      (apply trace-display info)
	      ((standard-rep-message message)))
	    (standard-rep-prompt breakpoint-prompt)))

(define (break-entry-advice proc args env)
  (fluid-let ((the-procedure proc)
	      (the-args args))
    (break-rep env "Breakpoint on entry" proc args)))

(define (break-exit-advice proc args result env)
  (fluid-let ((the-procedure proc)
	      (the-args args)
	      (the-result result))
    (break-rep env "Breakpoint on exit" proc args result))
  result)

(define primitive-break-entry
  (particular-entry-advisor break-entry-advice))

(define primitive-break-exit
  (particular-exit-advisor break-exit-advice))

(define primitive-break-both
  (particular-both-advisor break-entry-advice break-exit-advice))

(define primitive-unbreak
  (particular-both-unadvisor break-entry-advice break-exit-advice))

(define primitive-unbreak-entry
  (particular-entry-unadvisor break-entry-advice))

(define primitive-unbreak-exit
  (particular-exit-unadvisor break-exit-advice))

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
	    (if (memq (car path) auxiliary)
		(loop (sequence-actions body))
		(error "No internal definition by this name" (car path)))))))

  (if (null? path)
      (procedure-lambda procedure)
      (find-lambda (procedure-lambda procedure) (car path))))

;; The LIST-COPY will prevent any mutation problems.
(define ((wrap-advice-extractor extractor) procedure . path)
  (list-copy (extractor (find-internal-lambda procedure path))))

(define advice (wrap-advice-extractor primitive-advice))
(define entry-advice (wrap-advice-extractor primitive-entry-advice))
(define exit-advice (wrap-advice-extractor primitive-exit-advice))

(define ((wrap-general-advisor advisor) procedure advice . path)
  (advisor (find-internal-lambda procedure path) advice)
  *the-non-printing-object*)

(define advise-entry (wrap-general-advisor primitive-advise-entry))
(define advise-exit (wrap-general-advisor primitive-advise-exit))

(define (((wrap-unadvisor map-over-population) unadvisor) . procedure&path)
  (if (null? procedure&path)
      (map-over-population unadvisor)
      (unadvisor (find-internal-lambda (car procedure&path)
				       (cdr procedure&path))))
  *the-non-printing-object*)

(define wrap-entry-unadvisor
  (wrap-unadvisor
   (lambda (operation)
     (map-over-population entry-advice-population operation))))

(define wrap-exit-unadvisor
  (wrap-unadvisor
   (lambda (operation)
     (map-over-population exit-advice-population operation))))

(define wrap-both-unadvisor
  (wrap-unadvisor
   (lambda (operation)
     (map-over-population entry-advice-population operation)
     (map-over-population exit-advice-population operation))))

(define unadvise (wrap-both-unadvisor primitive-unadvise-entire-lambda))
(define unadvise-entry (wrap-entry-unadvisor primitive-unadvise-entire-entry))
(define unadvise-exit (wrap-exit-unadvisor primitive-unadvise-entire-exit))

(define untrace (wrap-both-unadvisor primitive-untrace))
(define untrace-entry (wrap-entry-unadvisor primitive-untrace-entry))
(define untrace-exit (wrap-exit-unadvisor primitive-untrace-exit))

(define unbreak (wrap-both-unadvisor primitive-unbreak))
(define unbreak-entry (wrap-entry-unadvisor primitive-unbreak-entry))
(define unbreak-exit (wrap-exit-unadvisor primitive-unbreak-exit))

(define ((wrap-advisor advisor) procedure . path)
  (advisor (find-internal-lambda procedure path))
  *the-non-printing-object*)

(define trace-entry (wrap-advisor primitive-trace-entry))
(define trace-exit (wrap-advisor primitive-trace-exit))
(define trace-both (wrap-advisor primitive-trace-both))

(define break-entry (wrap-advisor primitive-break-entry))
(define break-exit (wrap-advisor primitive-break-exit))
(define break-both (wrap-advisor primitive-break-both))

;;; end of ADVICE-PACKAGE.
))

;;;; Exports

(define advice (access advice advice-package))
(define entry-advice (access entry-advice advice-package))
(define exit-advice (access exit-advice advice-package))

(define advise-entry (access advise-entry advice-package))
(define advise-exit (access advise-exit advice-package))

(define unadvise (access unadvise advice-package))
(define unadvise-entry (access unadvise-entry advice-package))
(define unadvise-exit (access unadvise-exit advice-package))

(define trace (access trace-both advice-package))
(define trace-entry (access trace-entry advice-package))
(define trace-exit (access trace-exit advice-package))
(define trace-both (access trace-both advice-package))

(define untrace (access untrace advice-package))
(define untrace-entry (access untrace-entry advice-package))
(define untrace-exit (access untrace-exit advice-package))

(define break (access break-both advice-package))
(define break-entry (access break-entry advice-package))
(define break-exit (access break-exit advice-package))
(define break-both (access break-both advice-package))

(define unbreak (access unbreak advice-package))
(define unbreak-entry (access unbreak-entry advice-package))
(define unbreak-exit (access unbreak-exit advice-package))

(define *args*   (access *args* advice-package))
(define *proc*   (access *proc* advice-package))
(define *result* (access *result* advice-package))