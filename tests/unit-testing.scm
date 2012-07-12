#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012 Massachusetts Institute
    of Technology

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

;;;; Unit-test framework

(declare (usual-integrations))

(define (run-unit-test filename/s test-name #!optional environment)
  (let ((port (notification-output-port)))
    (let ((tests (load-unit-tests filename/s environment)))
      (let ((test (assq test-name tests)))
	(if (not test)
	    (error "Unknown test name:" test-name (map car tests)))
	(run-and-report test port)))))

(define (run-unit-tests filename/s #!optional environment)
  (let ((port (notification-output-port))
	(pass? #t))
    (for-each (lambda (test)
		(if (not (run-and-report test port))
		    (set! pass? #f)))
	      (load-unit-tests filename/s environment))
    pass?))

(define (load-unit-tests filename/s #!optional environment)
  (let ((test-environment (make-test-environment! environment)))
    (fluid-let ((*registered-tests* '()))
      (load filename/s test-environment)
      (reverse! *registered-tests*))))

(define (register-test name test)
  (guarantee-test test 'REGISTER-TEST)
  (set! *registered-tests* (cons (cons name test) *registered-tests*))
  unspecific)

(define *registered-tests*)

(define (test? object)
  (or (thunk? object)
      (list-of-type? object test?)))

(define-guarantee test "test")

(define (make-test-environment! #!optional parent)
  (let ((test-environment
	 (if (default-object? parent)
	     (make-top-level-environment)
	     (extend-top-level-environment parent))))
    (for-each (lambda (p)
		(environment-define test-environment (car p) (cdr p)))
	      test-definitions)
    test-environment))

(define-syntax define-for-tests
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (receive (name value)
	 (parse-define-form form rename)
       `(,(rename 'BEGIN)
	 (,(rename 'DEFINE) ,name ,value)
	 (,(rename 'ADD-TEST-DEFINITION) ',name ,name))))))

(define (add-test-definition name value)
  (let ((p (assq name test-definitions)))
    (if p
	(set-cdr! p value)
	(begin
	  (set! test-definitions (cons (cons name value) test-definitions))
	  unspecific))))

(define test-definitions '())

(define-for-tests (define-test name test . tests)
  (register-test name
		 (if (null? tests)
		     test
		     (cons test tests)))
  name)

;;;; Test runner

(define (run-and-report name.test port)
  (let ((start-time (process-time-clock)))
    (let ((results
	   (append-map! (lambda (named-sub-test)
			  (name-and-flatten (car named-sub-test)
					    (cdr named-sub-test)))
			(run-sub-tests (name-and-flatten "" (cdr name.test))))))
      (report-result (car name.test)
		     (internal-time/ticks->seconds
		      (- (process-time-clock) start-time))
		     results
		     port))))

(define (run-sub-tests named-sub-tests)
  ;; Runs sub-tests in left-to-right order.
  (let loop ((named-sub-tests named-sub-tests) (results '()))
    (if (pair? named-sub-tests)
	(loop (cdr named-sub-tests)
	      (cons (cons (caar named-sub-tests)
			  (run-sub-test (cdar named-sub-tests)))
		    results))
	(reverse! results))))

(define (name-and-flatten root-name item)
  (flatten (attach-names root-name item)))

(define (attach-names root-name item)
  (let loop ((item item) (name root-name))
    (if (list? item)
	(map (lambda (item index)
	       (loop item (string name "." index)))
	     item
	     (iota (length item)))
	(cons name item))))

(define (flatten items)
  (if (list? items)
      (append-map! flatten items)
      (list items)))

;;;; Reporting

(define (report-result test-name elapsed-time sub-test-results port)
  (let ((n-sub-test-results (length sub-test-results))
	(n-failed (count failing-sub-test? sub-test-results)))
    (fresh-line port)
    (write test-name port)
    (write-string ": " port)
    (if (> n-failed 0)
	(begin
	  (write-string "failed " port)
	  (write n-failed port)
	  (write-string " sub-tests out of " port)
	  (write n-sub-test-results port)
	  (report-test-time elapsed-time port)
	  (write-string ":" port)
	  (newline port)
	  (for-each
	   (lambda (sub-test-result)
	     (if (failing-sub-test? sub-test-result)
		 (report-test-failure "    "
				      (car sub-test-result)
				      (cdr sub-test-result)
				      port)))
	   sub-test-results))
	(begin
	  (write-string "passed " port)
	  (write n-sub-test-results port)
	  (write-string " sub-tests" port)
	  (report-test-time elapsed-time port)
	  (newline port))))
  (every passing-sub-test? sub-test-results))

(define (report-test-time elapsed-time port)
  (write-string " in " port)
  (write elapsed-time port)
  (write-string " seconds" port))

(define (report-test-failure prefix name failure port)
  (write-string prefix port)
  (write-string name port)
  (write-string ": " port)
  (cond ((not failure) (write-string "passed" port))
	((failure? failure) (report-failure failure port))
	(else (error "Ill-formed failure:" failure)))
  (newline port))

(define (failing-sub-test? sub-test-result)
  (cdr sub-test-result))

(define (passing-sub-test? sub-test-result)
  (not (cdr sub-test-result)))

(define condition-type:failure
  (make-condition-type 'FAILURE #f '(FAILURE) #f))

(define condition-failure
  (condition-accessor condition-type:failure 'FAILURE))

(define-record-type <failure>
    (%make-failure alist)
    failure?
  (alist failure-alist))

(define (make-failure . plist)
  (%make-failure (keyword-list->alist plist)))

(define (extend-failure failure plist)
  (%make-failure
   (append (failure-alist failure)
	   (keyword-list->alist plist))))

(define (failure-property key failure)
  (assq key (failure-alist failure)))

(define (write-string-property tag p port)
  (write-tag tag port)
  (write-string (cdr p) port))

(define (write-object-property tag p port)
  (write-tag tag port)
  (write (cdr p) port))

(define (write-expr-property tag p port)
  (write-tag tag port)
  (fluid-let ((*unparse-abbreviate-quotations?* #t))
    (write (cdr p) port)))

(define (write-tag tag port)
  (if tag
      (begin
	(write-string " " port)
	(write-string tag port)
	(write-string " " port))))

(define (failure-feature feature failure)
  (or (failure-property (symbol feature '-DESCRIPTION) failure)
      (failure-property (symbol feature '-OBJECT) failure)))

(define (write-feature tag p port)
  (if (string-suffix-ci? "-DESCRIPTION" (symbol-name (car p)))
      (write-string-property tag p port)
      (write-object-property tag p port)))

(define (report-failure failure port)
  (cond ((failure-property 'CONDITION failure)
	 => (lambda (p)
	      (let ((expr (failure-property 'EXPRESSION failure)))
		(if expr
		    (begin
		      (write-expr-property #f expr port)
		      (write-string " " port))))
	      (write-string "failed with error: " port)
	      (write-condition-report (cdr p) port)))
	((failure-feature 'RESULT failure)
	 => (lambda (result)
	      (write-string "value" port)
	      (let ((expr (failure-property 'EXPRESSION failure)))
		(if expr
		    (write-expr-property "of" expr port)))
	      (write-feature "was" result port)
	      (let ((expectation (failure-feature 'EXPECTATION failure)))
		(if expectation
		    (write-feature "but expected" expectation port)))))
	((failure-property 'DESCRIPTION failure)
	 => (lambda (p)
	      (write-string (cdr p) port)))
	(else
	 (error "Ill-formed failure:" failure))))

;;;; Assertions

(define-for-tests (run-sub-test thunk . properties)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handlers
      (list condition-type:failure
	    (lambda (condition)
	      (k (extend-failure (condition-failure condition)
				 properties)))
	    condition-type:error
	    (lambda (condition)
	      (if (not throw-test-errors?)
		  (k (apply make-failure
			    'CONDITION condition
			    properties)))))
      (lambda ()
	(thunk)
	#f)))))

(define-for-tests (with-test-properties thunk . properties)
  (bind-condition-handlers
   (list condition-type:failure
	 (lambda (condition)
	   (error
	    (remake-failure-condition
	     condition
	     (extend-failure (condition-failure condition)
			     properties))))
	 condition-type:error
	 (lambda (condition)
	   (if (not throw-test-errors?)
	       (apply fail 'CONDITION condition properties))))
   thunk))

(define throw-test-errors? #f)

(define (bind-condition-handlers bindings thunk)
  (if (pair? bindings)
      (bind-condition-handler (list (car bindings))
	  (cadr bindings)
	(lambda ()
	  (bind-condition-handlers (cddr bindings) thunk)))
      (thunk)))

(define-for-tests (fail . plist)
  (call-with-current-continuation
   (lambda (continuation)
     (error
      (make-failure-condition continuation
			      (apply make-failure plist))))))

(define (make-failure-condition continuation failure)
  (make-condition condition-type:failure
		  continuation
		  'BOUND-RESTARTS
		  (list 'FAILURE failure)))

(define (remake-failure-condition condition failure)
  (make-condition condition-type:failure
		  (condition/continuation condition)
		  (condition/restarts condition)
		  (list 'FAILURE failure)))

(define-for-tests (value-assert predicate description value . properties)
  (%assert predicate value description properties))

(define-for-tests (predicate-assertion predicate description)
  (lambda (value . properties)
    (%assert predicate value description properties)))

(define (%assert predicate value description properties)
  (if (not (predicate value))
      (apply fail
	     'RESULT-OBJECT value
	     'EXPECTATION-DESCRIPTION description
	     properties)))

(define-for-tests assert-true
  (predicate-assertion (lambda (x) x) "true"))

(define-for-tests assert-false
  (predicate-assertion not "false"))

(define-for-tests assert-null
  (predicate-assertion null? "an empty list"))

(define-for-tests (binary-assertion comparator)
  (lambda (value expected . properties)
    (if (not (comparator value expected))
	(apply fail
	       'RESULT-OBJECT value
	       'EXPECTATION-OBJECT expected
	       properties))))

(define-for-tests assert-eq (binary-assertion eq?))
(define-for-tests assert-eqv (binary-assertion eqv?))
(define-for-tests assert-equal (binary-assertion equal?))

(define-for-tests assert-= (binary-assertion =))
(define-for-tests assert-!= (binary-assertion (lambda (v e) (not (= v e)))))
(define-for-tests assert-< (binary-assertion <))
(define-for-tests assert-<= (binary-assertion <=))
(define-for-tests assert-> (binary-assertion >))
(define-for-tests assert->= (binary-assertion >=))

(define-for-tests assert-boolean-= (binary-assertion boolean=?))
(define-for-tests assert-boolean-!=
  (binary-assertion (lambda (x y) (not (boolean=? x y)))))

(define-for-tests assert-memv
  (binary-assertion (lambda (actual-value expected-list)
		      (and (memv actual-value expected-list) #t))))

(define-for-tests (assert-error thunk condition-types . properties)
  (call-with-current-continuation
   (lambda (k)
     (apply fail
	    'RESULT-OBJECT 
	    (bind-condition-handler condition-types
		(lambda (condition)
		  condition		;ignore
		  (k #f))
	      thunk)
	    properties))))