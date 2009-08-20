#| -*-Scheme-*-

$Id$

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; Advice package
;;; package: (runtime advice)

(declare (usual-integrations))

(define entry-advice-population)
(define exit-advice-population)

(define (initialize-package!)
  (set! entry-advice-population (make-population))
  (set! exit-advice-population (make-population))
  unspecific)

(define the-arguments)
(define the-procedure)
(define the-result)

(define (*args*)
  (list-copy the-arguments))

(define (*proc*)
  the-procedure)

(define (*result*)
  the-result)

(define (get-advice procedure)
  (lambda-advice (procedure-lambda procedure)))

(define (lambda-advice *lambda)
  (lambda-wrapper-components *lambda
    (lambda (original-body state)
      original-body
      (if (not (pair? state))
	  (error:bad-range-argument *lambda 'LAMBDA-ADVICE))
      (values (car state) (cdr state)))))

(define (make-advice-hook)
  ;; This inserts the actual procedure in a constant list.
  (make-combination
   (make-combination (ucode-primitive car)
		     (list (list hook/advised-procedure-wrapper)))
   (list (make-the-environment))))

(define (hook/advised-procedure-wrapper environment)
  (advised-procedure-wrapper environment))

(define (advised-procedure-wrapper environment)
  ;; This procedure is called with the newly-created environment as
  ;; its argument.
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

;;;; Advisers

(define (advice procedure)
  (receive (entry-advice exit-advice) (get-advice procedure)
    (list (list-copy entry-advice)
	  (list-copy exit-advice))))

(define (entry-advice procedure)
  (receive (entry-advice exit-advice) (get-advice procedure)
    exit-advice
    (list-copy entry-advice)))

(define (exit-advice procedure)
  (receive (entry-advice exit-advice) (get-advice procedure)
    entry-advice
    (list-copy exit-advice)))

(define (advise-entry procedure entry-advice)
  (primitive-advise (procedure-lambda procedure)
		    (adjoiner entry-advice)
		    identity-procedure))

(define (advise-exit procedure exit-advice)
  (primitive-advise (procedure-lambda procedure)
		    identity-procedure
		    (adjoiner exit-advice)))

(define (advise-both procedure entry-advice exit-advice)
  (primitive-advise (procedure-lambda procedure)
		    (adjoiner entry-advice)
		    (adjoiner exit-advice)))

(define (primitive-advise *lambda edit-entry edit-exit)
  (let ((transform
	 (lambda (entry-advice exit-advice)
	   (let ((entry-advice* (edit-entry entry-advice))
		 (exit-advice* (edit-exit exit-advice)))
	     (if (not (eq? entry-advice* entry-advice))
		 (add-to-population! entry-advice-population *lambda))
	     (if (not (eq? exit-advice* exit-advice))
		 (add-to-population! exit-advice-population *lambda))
	     (cons entry-advice* exit-advice*)))))
    (lambda-wrap-body! *lambda
      (lambda (body state receiver)
	(if (pair? state)
	    (receiver body (transform (car state) (cdr state)))
	    (receiver (make-advice-hook) (transform '() '()))))))
  unspecific)

(define ((adjoiner advice) advice-list)
  (if (memq advice advice-list)
      advice-list
      (cons advice advice-list)))

;;;; Unadvisers

(define (entry-unadviser edit-entry-advice)
  (let ((unadvise
	 (lambda (*lambda)
	   (primitive-unadvise *lambda edit-entry-advice identity-procedure))))
    (unadviser unadvise
	       (lambda ()
		 (map-over-population entry-advice-population unadvise)))))

(define (exit-unadviser edit-exit-advice)
  (let ((unadvise
	 (lambda (*lambda)
	   (primitive-unadvise *lambda identity-procedure edit-exit-advice))))
    (unadviser unadvise
	       (lambda ()
		 (map-over-population exit-advice-population unadvise)))))

(define (both-unadviser edit-entry-advice edit-exit-advice)
  (unadviser
   (lambda (*lambda)
     (primitive-unadvise *lambda edit-entry-advice edit-exit-advice))
   (lambda ()
     (map-over-population entry-advice-population
       (lambda (*lambda)
	 (primitive-unadvise *lambda edit-entry-advice identity-procedure)))
     (map-over-population exit-advice-population
       (lambda (*lambda)
	 (primitive-unadvise *lambda identity-procedure edit-exit-advice))))))

(define ((unadviser unadvise-given unadvise-all) #!optional procedure)
  (if (or (default-object? procedure) (not procedure))
      (unadvise-all)
      (unadvise-given (procedure-lambda procedure)))
  unspecific)

(define (primitive-unadvise *lambda edit-entry edit-exit)
  (receive (entry-advice exit-advice) (lambda-advice *lambda)
    (let ((entry-advice (edit-entry entry-advice))
	  (exit-advice (edit-exit exit-advice)))
      (if (null? entry-advice)
	  (remove-from-population! entry-advice-population *lambda))
      (if (null? exit-advice)
	  (remove-from-population! exit-advice-population *lambda))
      (if (and (null? entry-advice) (null? exit-advice))
	  (lambda-unwrap-body! *lambda)
	  (lambda-wrap-body! *lambda
	    (lambda (body state receiver)
	      state
	      (receiver body (cons entry-advice exit-advice))))))))

(define (nullifier advice-list)
  advice-list
  '())

(define ((disjoiner advice) advice-list)
  (delq! advice advice-list))

(define unadvise-entry
  (entry-unadviser nullifier))

(define unadvise-exit
  (exit-unadviser nullifier))

(define unadvise
  (both-unadviser nullifier nullifier))

(define (specific-entry-unadviser entry-advice)
  (entry-unadviser (disjoiner entry-advice)))

(define (specific-exit-unadviser exit-advice)
  (exit-unadviser (disjoiner exit-advice)))

(define (specific-both-unadviser entry-advice exit-advice)
  (both-unadviser (disjoiner entry-advice) (disjoiner exit-advice)))

;;;; Trace

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

(define (trace-entry procedure)
  (advise-entry procedure trace-entry-advice))

(define (trace-exit procedure)
  (advise-exit procedure trace-exit-advice))

(define (trace-both procedure)
  (advise-both procedure trace-entry-advice trace-exit-advice))

(define trace trace-both)

(define untrace-entry
  (specific-entry-unadviser trace-entry-advice))

(define untrace-exit
  (specific-exit-unadviser trace-exit-advice))

(define untrace
  (specific-both-unadviser trace-entry-advice trace-exit-advice))

;;;; Break

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

(define (break-entry procedure)
  (advise-entry procedure break-entry-advice))

(define (break-exit procedure)
  (advise-exit procedure break-exit-advice))

(define (break-both procedure)
  (advise-both procedure break-entry-advice break-exit-advice))

(define unbreak-entry
  (specific-entry-unadviser break-entry-advice))

(define unbreak-exit
  (specific-exit-unadviser break-exit-advice))

(define unbreak
  (specific-both-unadviser break-entry-advice break-exit-advice))