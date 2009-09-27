#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009 Massachusetts Institute of Technology

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

(define (run-unit-tests filename/s #!optional environment)
  (report-results
   (map run-test
	(load-unit-tests filename/s environment))))

(define (load-unit-tests filename/s #!optional environment)
  (let ((test-environment
	 (if (default-object? environment)
	     (make-top-level-environment)
	     (extend-top-level-environment environment))))
    (initialize-test-environment! test-environment)
    (fluid-let ((*registered-tests* '()))
      (load filename/s test-environment)
      (reverse! *registered-tests*))))

(define (register-test name test)
  (set! *registered-tests* (cons (cons name test) *registered-tests*))
  unspecific)

(define *registered-tests*)

(define (initialize-test-environment! test-environment)
  (for-each (lambda (p)
	      (environment-define test-environment (car p) (cdr p)))
	    test-definitions))

(define (add-test-definition name value)
  (let ((p (assq name test-definitions)))
    (if p
	(set-cdr! p value)
	(begin
	  (set! test-definitions (cons (cons name value) test-definitions))
	  unspecific))))

(define test-definitions '())

(define (report-results results)
  (let ((port (notification-output-port)))
    (for-each (lambda (result)
		(write (car result) port)
		(write-string ": " port)
		(cond ((not (cdr result))
		       (write-string "passed" port))
		      ((failure? (cdr result))
		       (report-failure (cdr result) port))
		      (else
		       (error "Ill-formed result:" result)))
		(newline port))
	      results))
  ;; Value is true iff all tests passed.
  (every (lambda (result)
	   (not (cdr result)))
	 results))

(define (run-test test)
  (cons (car test)
	(call-with-current-continuation
	 (lambda (k)
	   (bind-condition-handler (list condition-type:error)
	       (lambda (condition)
		 (k (make-failure 'CONDITION condition)))
	     (cdr test))))))

(define-record-type <failure>
    (%make-failure alist)
    failure?
  (alist failure-alist))

(define (make-failure . plist)
  (%make-failure (keyword-list->alist plist)))

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

(define-syntax define-for-tests
  (er-macro-transformer
   (lambda (form rename compare)
     compare
     (receive (name value)
	 (parse-define-form form rename)
       `(,(rename 'BEGIN)
	 (,(rename 'DEFINE) ,name ,value)
	 (,(rename 'ADD-TEST-DEFINITION) ',name ,name))))))

(define-for-tests (define-test name . tests)
  (let ((tests (flatten tests)))
    (if (pair? tests)
	(if (pair? (cdr tests))
	    (for-each (lambda (test index)
			(register-test (symbol name '/ index) test))
		      tests
		      (iota (length tests)))
	    (register-test name (car tests)))))
  name)

(define (flatten tests)
  (append-map! (lambda (test)
		 (if (list? test)
		     (flatten test)
		     (list test)))
	       tests))

(define-for-tests (predicate-assertion predicate description)
  (lambda (value . properties)
    (if (predicate value)
	#f
	(apply make-failure
	       'RESULT-OBJECT value
	       'EXPECTATION-DESCRIPTION description
	       properties))))

(define-for-tests (assert predicate description value . properties)
  (apply (predicate-assertion predicate description)
	 value
	 properties))

(define-for-tests (assert-true expr value)
  (if value
      #f
      (make-failure 'EXPRESSION expr
		    'RESULT-DESCRIPTION "false"
		    'EXPECTATION-DESCRIPTION "true")))

(define-for-tests (assert-false expr value)
  (if value
      (make-failure 'EXPRESSION expr
		    'RESULT-DESCRIPTION "true"
		    'EXPECTATION-DESCRIPTION "false")
      #f))

(define-for-tests assert-null
  (predicate-assertion null? "an empty list"))

(define-for-tests (binary-assertion comparator)
  (lambda (value expected . properties)
    (if (comparator value expected)
	#f
	(apply make-failure
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