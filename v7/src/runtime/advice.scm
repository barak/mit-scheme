#| -*-Scheme-*-

$Id: advice.scm,v 14.18 2002/11/20 19:46:18 cph Exp $

Copyright (c) 1988-2000 Massachusetts Institute of Technology

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

;;;; Advice package
;;; package: (runtime advice)

(declare (usual-integrations))

(define entry-advice-population)
(define exit-advice-population)

(define particular-entry-advisor)
(define particular-exit-advisor)
(define particular-both-advisor)
(define particular-entry-unadvisor)
(define particular-exit-unadvisor)
(define particular-both-unadvisor)

(define primitive-trace-entry)
(define primitive-trace-exit)
(define primitive-trace-both)

(define primitive-untrace)
(define primitive-untrace-entry)
(define primitive-untrace-exit)

(define primitive-break-entry)
(define primitive-break-exit)
(define primitive-break-both)

(define primitive-unbreak)
(define primitive-unbreak-entry)
(define primitive-unbreak-exit)

(define advice)
(define entry-advice)
(define exit-advice)

(define advise-entry)
(define advise-exit)

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

(define trace-entry)
(define trace-exit)
(define trace-both)
(define trace)

(define break-entry)
(define break-exit)
(define break-both)
(define break)

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
  (set! break break-both)
  unspecific)

;;;; Advice Wrappers

(define the-arguments)
(define the-procedure)
(define the-result)

(define (*args*)
  (list-copy the-arguments))

(define (*proc*)
  the-procedure)

(define (*result*)
  the-result)

(define (add-lambda-advice! *lambda advice-transformation)
  (lambda-wrap-body! *lambda
    (lambda (body state receiver)
      (if (null? state)
	  (receiver (make-advice-hook)
		    (advice-transformation '() '() cons))
	  (receiver body
		    (advice-transformation (car state) (cdr state) cons))))))

(define (remove-lambda-advice! *lambda advice-transformation)
  (lambda-advice *lambda
    (lambda (entry-advice exit-advice)
      (advice-transformation entry-advice exit-advice
	(lambda (new-entry-advice new-exit-advice)
	  (if (and (null? new-entry-advice) (null? new-exit-advice))
	      (lambda-unwrap-body! *lambda)
	      (lambda-wrap-body! *lambda
		(lambda (body state receiver)
		  state
		  (receiver body
			    (cons new-entry-advice new-exit-advice))))))))))

(define (lambda-advice *lambda receiver)
  (lambda-wrapper-components *lambda
    (lambda (original-body state)
      original-body
      (if (null? state)
	  (error "Procedure has no advice -- LAMBDA-ADVICE" *lambda))
      (receiver (car state) (cdr state)))))

(define (make-advice-hook)
  ;; This inserts the actual procedure in a constant list
  (make-combination
   (make-combination car
		     (list (list hook/advised-procedure-wrapper)))
   (list (make-the-environment))))

(define (hook/advised-procedure-wrapper environment)
  (advised-procedure-wrapper environment))

;;;; The Advice Hook

;;; This procedure is called with the newly-created environment as its
;;; argument.

(define (advised-procedure-wrapper environment)
  (let ((procedure (ic-environment/procedure environment))
	(arguments (ic-environment/arguments environment)))
    (lambda-wrapper-components (procedure-lambda procedure)
      (lambda (original-body state)
	(call-with-current-continuation
	 (lambda (continuation)
	   (fluid-let ((advice-continuation continuation))
	     (with-restart 'USE-VALUE
		 "Return a value from the advised procedure."
		 continuation
		 (lambda ()
		   (prompt-for-evaluated-expression "Procedure value"))
	       (lambda ()
		 (for-each (lambda (advice)
			     (with-simple-restart 'CONTINUE
				 "Continue with advised procedure."
			       (lambda ()
				 (advice procedure arguments environment))))
			   (car state))
		 (let ((value (scode-eval original-body environment)))
		   (for-each (lambda (advice)
			       (with-simple-restart 'CONTINUE
				   "Return from advised procedure."
				 (lambda ()
				   (advice procedure
					   arguments
					   value
					   environment))))
			     (cdr state))
		   value))))))))))

(define advice-continuation #f)

;;;; Primitive Advisors

(define (primitive-advice *lambda)
  (lambda-advice *lambda list))

(define (primitive-entry-advice *lambda)
  (lambda-advice *lambda
    (lambda (entry-advice exit-advice)
      exit-advice
      entry-advice)))

(define (primitive-exit-advice *lambda)
  (lambda-advice *lambda
    (lambda (entry-advice exit-advice)
      entry-advice
      exit-advice)))

(define (primitive-advise-entry *lambda advice)
  (add-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver (if (memq advice entry-advice)
		    entry-advice
		    (begin (add-to-population! entry-advice-population *lambda)
			   (cons advice entry-advice)))
		exit-advice))))

(define (primitive-advise-exit *lambda advice)
  (add-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver entry-advice
		(if (memq advice exit-advice)
		    exit-advice
		    (begin (add-to-population! exit-advice-population *lambda)
			   (append! exit-advice (list advice))))))))

(define ((primitive-advise-both new-entry-advice new-exit-advice) *lambda)
  (add-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (receiver (if (memq new-entry-advice entry-advice)
		    entry-advice
		    (begin (add-to-population! entry-advice-population *lambda)
			   (cons new-entry-advice entry-advice)))
		(if (memq new-exit-advice exit-advice)
		    exit-advice
		    (begin (add-to-population! exit-advice-population *lambda)
			   (append! exit-advice (list new-exit-advice))))))))

(define (eq?-adjoin object list)
  (if (memq object list)
      list
      (cons object list)))

(define (primitive-unadvise-entire-entry *lambda)
  (remove-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      entry-advice
      (receiver '() exit-advice)))
  (remove-from-population! entry-advice-population *lambda))

(define (primitive-unadvise-entire-exit *lambda)
  (remove-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      exit-advice
      (receiver entry-advice '())))
  (remove-from-population! exit-advice-population *lambda))

(define (primitive-unadvise-entire-lambda *lambda)
  (lambda-unwrap-body! *lambda)
  (remove-from-population! entry-advice-population *lambda)
  (remove-from-population! exit-advice-population *lambda))

(define ((primitive-unadvise-entry advice) *lambda)
  (remove-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-entry-advice (delq! advice entry-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population *lambda))
	(receiver new-entry-advice exit-advice)))))

(define ((primitive-unadvise-exit advice) *lambda)
  (remove-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-exit-advice (delq! advice exit-advice)))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population *lambda))
	(receiver entry-advice new-exit-advice)))))

(define ((primitive-unadvise-both old-entry-advice old-exit-advice) *lambda)
  (remove-lambda-advice! *lambda
    (lambda (entry-advice exit-advice receiver)
      (let ((new-entry-advice (delq! old-entry-advice entry-advice))
	    (new-exit-advice (delq! old-exit-advice exit-advice)))
	(if (null? new-entry-advice)
	    (remove-from-population! entry-advice-population *lambda))
	(if (null? new-exit-advice)
	    (remove-from-population! exit-advice-population *lambda))
	(receiver new-entry-advice new-exit-advice)))))

(define (((particular-advisor advisor) advice) *lambda)
  (advisor *lambda advice))

;;;; Trace and Break

(define (trace-entry-advice procedure arguments environment)
  environment
  (trace-display (trace-output-port) procedure arguments))

(define (trace-exit-advice procedure arguments result environment)
  environment
  (trace-display (trace-output-port) procedure arguments result)
  result)

(define (trace-display port procedure arguments #!optional result)
  (fresh-line port)
  (let ((width (- (max 40 (output-port/x-size port)) 1))
	(write-truncated
	 (lambda (object width)
	   (let ((output (write-to-string object width)))
	     (if (car output)
		 (substring-fill! (cdr output) (- width 3) width #\.))
	     (write-string (cdr output) port)))))
    (if (default-object? result)
	(write-string "[Entering " port)
	(begin
	  (write-string "[" port)
	  (write-truncated result (- width 2))
	  (newline port)
	  (write-string "      <== " port)))
    (write-truncated procedure (- width 11))
    (if (null? arguments)
	(write-string "]" port)
	(begin
	  (newline port)
	  (let ((write-args
		 (lambda (arguments)
		   (let loop ((prefix "    Args: ") (arguments arguments))
		     (write-string prefix port)
		     (write-truncated (car arguments) (- width 11))
		     (if (not (null? (cdr arguments)))
			 (begin
			   (newline port)
			   (loop "          " (cdr arguments))))))))
	    (if (<= (length arguments) 10)
		(begin
		  (write-args arguments)
		  (write-string "]" port))
		(begin
		  (write-args (list-head arguments 10))
		  (newline port)
		  (write-string "          ...]" port))))))
    (newline port)))

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

(define (break-rep environment message . info)
  (breakpoint (cmdl-message/append (cmdl-message/active
				    (lambda (port)
				      (apply trace-display port info)))
				   message)
	      environment
	      advice-continuation))

;;;; Top Level Wrappers

(define (find-internal-lambda procedure path)
  (if (not (compound-procedure? procedure))
      (error "only compound procedures may be advised" procedure))
  (if (null? path)
      (procedure-lambda procedure)
      (let find-lambda
	  ((*lambda (procedure-lambda procedure))
	   (path (car path)))
	(if (null? path)
	    *lambda
	    (let loop
		((elements
		  (lambda-components *lambda
		    (lambda (name required optional rest auxiliary declarations
				  body)
		      name required optional rest declarations
		      (if (not (memq (car path) auxiliary))
			  (error "no internal definition by this name"
				 (car path)))
		      (sequence-actions body)))))
	      (if (null? elements)
		  (error "Couldn't find internal definition" path))
	      (if (assignment? (car elements))
		  (assignment-components (car elements)
		    (lambda (name value)
		      (if (eq? name (car path))
			  (begin
			    (if (not (lambda? value))
				(error "internal definition not a procedure"
				       path))
			    (find-lambda value (cdr path)))
			  (loop (cdr elements)))))
		  (loop (cdr elements))))))))

;; The LIST-COPY will prevent any mutation problems.
(define ((wrap-advice-extractor extractor) procedure . path)
  (list-copy (extractor (find-internal-lambda procedure path))))

(define ((wrap-general-advisor advisor) procedure advice . path)
  (advisor (find-internal-lambda procedure path) advice)
  unspecific)

(define (((wrap-unadvisor map-over-population) unadvisor) . procedure&path)
  (if (null? procedure&path)
      (map-over-population unadvisor)
      (unadvisor (find-internal-lambda (car procedure&path)
				       (cdr procedure&path))))
  unspecific)

(define ((wrap-advisor advisor) procedure . path)
  (advisor (find-internal-lambda procedure path))
  unspecific)