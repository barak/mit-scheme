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

;;;; Simple in-line tests

;;; The idea behind this tester is that it executes a file of expressions, in
;;; order, except that some of the expressions will be annotated with
;;; "expectations" that must be satisfied by the evaluation of the corresponding
;;; expression.

;;; For example,
;;;
;;; (fib 20)
;;; 'expect eqv? 6765
;;;
;;; is a trivial example.  There are also expectations involving
;;; printed output, and others can be defined as needed.

;;; This style of testing closely resembles a transcript, and has the advantage
;;; that the code can just be loaded normally and it does the same thing without
;;; checking the expectations.

;;; This was developed as a response to GJS's dislike of the standard testing
;;; framework.  Gerry prefers to just type at the interpreter and keep a
;;; transcript of the results around; this closely mirrors his testing style.

(define (run-inline-tests filename . options)
  (let-values (((*eval *env *notify? *summarize?)
		(run-option-parser options 'run-inline-tests))
	       ((exprs imports) (read-test-file filename)))
    (let ((groups (parse-expression-groups exprs)))
      (parameterize ((test-eval *eval)
		     (test-env
		      (if imports
			  (make-environment-from-parsed-imports imports)
			  *env))
		     (notify? *notify?)
		     (summarize? *summarize?))
	(summarize-test-results
	 (notify-filename filename
	   (lambda ()
	     (execute-expression-groups groups))))))))

(define run-option-parser
  (keyword-option-parser
   (list (list 'eval procedure? (lambda () eval))
	 (list 'env environment? nearest-repl/environment)
	 (list 'notify? boolean? (lambda () #f))
	 (list 'summarize? boolean? (lambda () #t)))))

(define test-eval (make-parameter (default-object)))
(define test-env (make-parameter (default-object)))
(define notify? (make-parameter (default-object)))
(define summarize? (make-parameter (default-object)))

(define (notify-filename filename thunk)
  (if (notify?)
      (with-notification
	  (lambda (port)
	    (display "loading test: " port)
	    (write (->namestring filename) port))
	thunk)
      (thunk)))

(define (read-test-file filename)
  (let ((pn (pathname-default-type filename "scm")))
    (let ((source (read-r7rs-source pn)))
      (if source
	  (let ((program (r7rs-source-program source)))
	    (if (not program)
		(error "Inline testing requires a program:" source))
	    (if (not (null? (r7rs-source-libraries source)))
		(error "Inline testing doesn't support inline libraries:"
		       source))
	    (values (cdr (car (library-parsed-contents program)))
		    (library-parsed-imports program)))
	  (values (read-file pn) #f)))))

(define (parse-expression-groups exprs)

  (define (expectation? expr)
    (and (pair? expr)
	 (procedure? (car expr))))

  (let loop ((items (parse-expectations exprs)))
    (cond ((not (pair? items)) '())
	  ((expectation? (car items))
	   (cons (cons #f (take-while expectation? items))
		 (loop (drop-while expectation? items))))
	  ((and (pair? (cdr items))
		(expectation? (cadr items)))
	   (cons (cons (car items) (take-while expectation? (cdr items)))
		 (loop (drop-while expectation? (cdr items)))))
	  (else
	   (cons (list (car items))
		 (loop (cdr items)))))))

(define make-group cons)
(define group-expression car)
(define group-expectations cdr)

(define (parse-expectations exprs)
  (map (lambda (expr)
	 (or (parse-expectation expr)
	     expr))
       exprs))

(define (parse-expectation expr)
  (and (is-quotation? expr)
       (let ((text (quotation-text expr)))
	 (let loop ((rules expectation-rules))
	   (if (pair? rules)
	       (or (match-rule (car rules) text)
		   (loop (cdr rules)))
	       (begin
		 (warn "Unrecognized expectation:" text)
		 #f))))))

(define (match-rule rule text)
  (let ((keyword (expectation-rule-keyword rule))
        (n-args (expectation-rule-n-args rule))
        (handler (expectation-rule-handler rule)))
    (cond ((and (pair? text)
		(eq? (car text) keyword)
		(list? (cdr text))
		(= (length (cdr text)) n-args))
	   (cons handler (cdr text)))
	  ((and (eq? text keyword)
		(= n-args 0))
	   (list handler))
	  (else #f))))

(define (is-quotation? object)
  (and (pair? object)
       (eq? (car object) 'quote)
       (pair? (cdr object))
       (null? (cddr object))))

(define (quotation-text expr)
  (cadr expr))

;;; Lots or hair here to let the test driver deal with "interesting" uses of
;;; continuations.  In particular, the state of the driver is moved outside of
;;; the control structure, so that if there are multiple returns from evaluating
;;; an expression, the "current" expectations are used for each.

(define groups-to-test (make-settable-parameter (default-object)))
(define current-group (make-settable-parameter (default-object)))
(define test-results (make-settable-parameter (default-object)))

(define (execute-expression-groups groups)
  (parameterize ((groups-to-test groups)
		 (current-group #f)
		 (test-results '()))
    (let loop ()
      (if (pair? (groups-to-test))
          (begin
            (current-group (car (groups-to-test)))
            (groups-to-test (cdr (groups-to-test)))
	    (let ((result (execute-expression-group (current-group))))
	      (if result
		  (test-results (cons result (test-results)))))
            (loop))))
    (reverse (test-results))))

(define (execute-expression-group group)
  (let ((context (eval-to-context (car group))))
    (and (pair? (cdr group))
	 (cons (car group)
	       (filter-map (lambda (expectation)
			     (apply (car expectation)
				    context
				    (map (lambda (expr)
					   (eval expr (test-env)))
					 (cdr expectation))))
			   (cdr group))))))

(define (eval-to-context expr)
  (let ((output-port (open-output-string)))
    (call-with-current-continuation
      (lambda (k)
	(with-exception-handler
	    (lambda (condition)
	      (k
	       (make-error-expectation-context
		(get-output-string output-port)
		condition)))
	  (lambda ()
	    (let ((value
		   (parameterize ((current-output-port output-port))
		     ((test-eval) expr (test-env)))))
	      (make-value-expectation-context
	       (get-output-string output-port)
	       value))))))))

(define (make-error-expectation-context output condition)
  (define (is-error?) #t)
  (let ((port (open-input-string output)))
    (define (get-port) port)
    (define (get-condition) condition)
    (bundle expectation-context? is-error? get-port get-condition)))

(define (make-value-expectation-context output value)
  (define (is-error?) #f)
  (let ((port (open-input-string output)))
    (define (get-port) port)
    (define (get-value) value)
    (bundle expectation-context? is-error? get-port get-value)))

(define expectation-context?
  (make-bundle-predicate 'expectation-context))

(define (summarize-test-results results)
  (let ((failing-results (filter failing-test-result? results)))
    (for-each show-failing-result failing-results)
    (if (summarize?)
	(begin
	  (let ((failures (length failing-results))
		(all (length results)))
	    (fresh-line)
	    (display "Ran ")
	    (write all)
	    (display " test")
	    (if (not (= 1 all))
		(display "s"))
	    (display ": ")
	    (write failures)
	    (display " failure")
	    (if (not (= 1 failures))
		(display "s")))))
    (null? failing-results)))

(define (failing-test-result? result)
  (pair? (cdr result)))

(define (show-failing-result failure)
  (newline)
  (newline)
  (display "evaluating")
  (newline)
  (pp (car failure))
  (display "failed the following expectations:")
  (newline)
  (for-each (lambda (error)
              (display error)
              (newline))
            (cdr failure)))

(define (pp-to-string object)
  (call-with-output-string
    (lambda (port)
      (pp object port))))

;;;; Expectation rules

(define (define-expectation keyword n-args handler)
  (let ((rule (make-expectation-rule keyword n-args handler))
        (tail
         (find-tail (lambda (rule)
                      (eq? keyword
                           (expectation-rule-keyword rule)))
                    expectation-rules)))
    (if tail
        (set-car! tail rule)
        (set! expectation-rules
              (cons rule
                    expectation-rules)))))

(define (define-error-expectation keyword n-args handler)
  (define-expectation keyword n-args
    (lambda (context . args)
      (if (context 'is-error?)
	  (apply handler (context 'get-condition) args)
	  (string-append "expected error but instead got value\n"
			 (write-to-string (context 'get-value)))))))

(define (define-value-expectation keyword n-args handler)
  (define-expectation keyword n-args
    (lambda (context . args)
      (if (context 'is-error?)
	  (string-append "Expected non-error but instead got error: "
			 (condition/report-string (context 'get-condition)))
	  (apply handler (context 'get-value) args)))))

(define (define-output-expectation keyword n-args handler)
  (define-expectation keyword n-args
    (lambda (context . args)
      (if (context 'is-error?)
	  (string-append "Expected non-error but instead got error: "
			 (condition/report-string (context 'get-condition)))
	  (let ((objects (read-objects (context 'get-port))))
	    (if (condition? objects)
		"Error while reading output"
		(apply handler objects args)))))))

(define (read-objects port)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
	  (lambda (condition)
	    (k condition))
	(lambda ()
	  (let loop ((objects '()))
	    (let ((object (read port)))
	      (if (eof-object? object)
		  (reverse objects)
		  (loop (cons object objects))))))))))

(define expectation-rules
  '())

(define (make-expectation-rule keyword n-args handler)
  (list 'expectation-rule keyword n-args handler))

(define expectation-rule-keyword cadr)
(define expectation-rule-n-args caddr)
(define expectation-rule-handler cadddr)

(define-value-expectation 'expect 2
  (lambda (value pred expected)
    (if (pred expected value)
	#f
	(string-append "expected value\n"
		       (write-to-string expected)
		       "\nbut instead got value\n"
		       (write-to-string value)))))

(define-value-expectation 'expect-not 2
  (lambda (value pred expected)
    (if (pred expected value)
	(string-append "expected value different from\n"
		       (write-to-string expected)
		       "\nbut instead got value\n"
		       (write-to-string value))
	#f)))

(define-value-expectation 'expect-true 0
  (lambda (value)
    (if value
	#f
	(string-append "expected true value but got false"))))

(define-value-expectation 'expect-false 0
  (lambda (value)
    (if value
	(string-append "expected false value but got\n"
		       (write-to-string value))
	#f)))

(define-error-expectation 'expect-error 0
  (lambda (condition)
    (declare (ignore condition))
    #f))

;;; General written output expectation.
(define-output-expectation 'expect-output 2
  (lambda (objects pred expected)
    (let ((v (pred expected objects)))
      (cond ((string? v) v)
	    ((eq? #t v) #f)
	    ((eq? #f v)
	     (string-append "Output\n"
			    (pp-to-string objects)
			    "doesn't satisfy predicate\n"
			    (pp-to-string pred)
			    "with expected value\n"
			    (pp-to-string expected)))
	    (else
	     (error "illegal predicate value:" v))))))

(define-output-expectation 'expect-write 1
  (lambda (objects expected)
    (if (and (pair? objects)
	     (null? (cdr objects))
	     (equal? (car objects) expected))
	#f
	(string-append "expected to see output "
		       (write-to-string expected)
		       "\nbut instead saw "
		       (write-to-string (car objects))))))

(define-output-expectation 'expect-pp-description 1
  (lambda (objects expected)
    (if (and (pair? objects)
	     (equal? (cdr objects) expected))
	#f
	(string-append "expected to see pp description "
		       (write-to-string expected)
		       "\nbut instead saw "
		       (write-to-string objects)))))

(define-output-expectation 'expect-no-output 0
  (lambda (objects)
    (if (null? objects)
	#f
	(string-append "expected no output but found "
		       objects))))