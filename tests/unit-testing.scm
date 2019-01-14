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
    (parameterize ((registered-tests '()))
      (load filename/s test-environment)
      (reverse! (registered-tests)))))

(define (register-test name test)
  (guarantee-test test 'REGISTER-TEST)
  (registered-tests (cons (cons name test) (registered-tests)))
  unspecific)

(define registered-tests
  (make-settable-parameter '()))

(define (test? object)
  (or (thunk? object)
      (list-of-type? object test?)))

(define-guarantee test "test")

(define (make-test-environment! #!optional parent)
  (let ((test-environment
	 (if (default-object? parent)
	     (make-top-level-environment)
	     (extend-top-level-environment (->environment parent)))))
    (for-each (lambda (p)
		(environment-define test-environment (car p) (cdr p)))
	      test-definitions)
    test-environment))

(define-syntax define-for-tests
  (er-macro-transformer
   (lambda (form rename compare)
     (declare (ignore compare))
     (let ((name
	    (let loop ((p (cadr form)))
	      (cond ((pair? p) (loop (car p)))
		    ((identifier? p) p)
		    (else (ill-formed-syntax form))))))
       `(,(rename 'begin)
	 (,(rename 'define) ,@(cdr form))
	 (,(rename 'add-test-definition) ',name ,name))))))

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

(define show-passing-results? (make-settable-parameter #f))

(define (report-result test-name elapsed-time sub-test-results port)
  (let ((n-sub-test-results (length sub-test-results))
	(n-failed (count failing-sub-test? sub-test-results)))
    (cond ((> n-failed 0)
	   (fresh-line port)
	   (write-char #\; port)
	   (write test-name port)
	   (write-char #\space port)
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
	  ((show-passing-results?)
	   (fresh-line port)
	   (write-char #\; port)
	   (write test-name port)
	   (write-char #\space port)
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
  (write-char #\space port)
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

(define (failure-feature feature failure)
  (let ((variants
	 (cons (cons feature 'PATTERN)
	       (map (lambda (variant-type)
		      (cons (symbol feature '- variant-type)
			    variant-type))
		    '(DESCRIPTION OBJECT)))))
    ;; Return the first instance of any variant.
    ;; The result is tagged by the variant type.
    (find-map (lambda (p)
		(let ((p* (assq (car p) variants)))
		  (and p*
		       (cons (cdr p*) (cdr p)))))
	      (failure-alist failure))))

(define (report-failure failure port)
  (let ((p (failure-property 'assertion-index failure)))
    (if p
	(begin
	  (write-string "assertion " port)
	  (write (cdr p) port)
	  (write-string ": " port))))
  (cond ((failure-property 'CONDITION failure)
	 => (lambda (p)
	      (let ((expr (failure-property 'EXPRESSION failure)))
		(if expr
		    (begin
		      (write-expr-property #f expr port)
		      (write-char #\space port))))
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

(define (write-tag tag port)
  (if tag
      (begin
	(write-char #\space port)
	(display tag port))))

(define (write-expr-property tag p port)
  (write-tag tag port)
  (write-char #\space port)
  (parameterize ((param:printer-abbreviate-quotations? #t))
    (write (cdr p) port)))

(define (write-feature tag p port)
  (write-tag tag port)
  (receive (pattern objects) (decode-feature (car p) (cdr p))
    (guarantee list? pattern)
    (guarantee list? objects)
    (if (not (= (count marker? pattern) (length objects)))
	(error "Mismatch between pattern and objects:" pattern objects))
    (let loop ((pattern pattern) (objects objects))
      (if (pair? pattern)
	  (begin
	    (write-char #\space port)
	    (if (marker? (car pattern))
		(begin
		  (write (car objects) port)
		  (loop (cdr pattern) (cdr objects)))
		(begin
		  (display (car pattern) port)
		  (loop (cdr pattern) objects))))))))

(define (decode-feature variant-type value)
  (case variant-type
    ((PATTERN) (values (car value) (cdr value)))
    ((DESCRIPTION) (values (list value) '()))
    ((OBJECT) (values (list (marker)) (list value)))
    (else (error "Unknown variant type:" variant-type))))

(define-record-type <marker>
    (marker)
    marker?)

;;;; Assertions

(define-for-tests (run-sub-test thunk . properties)
  (call-with-current-continuation
   (lambda (k)
     (parameterize ((assertion-index 1))
       (bind-condition-handlers
	(list condition-type:failure
	      (lambda (condition)
		(k (extend-failure (condition-failure condition)
				   properties)))
	      condition-type:error
	      (lambda (condition)
		(if (not (throw-test-errors?))
		    (k (apply make-failure
			      'condition condition
			      'assertion-index (assertion-index)
			      properties)))))
	(lambda ()
	  (thunk)
	  #f))))))

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
	   (if (not (throw-test-errors?))
	       (apply fail 'CONDITION condition properties))))
   thunk))

(define throw-test-errors? (make-settable-parameter #f))

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
			      (apply make-failure
				     'assertion-index (assertion-index)
				     plist))))))

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

(define assertion-index (make-settable-parameter #f))

(define (%assert predicate value description properties)
  (if (not (predicate value))
      (apply fail
	     'result-object value
	     'expectation-description description
	     properties))
  (assertion-index (+ (assertion-index) 1)))

(define-for-tests assert-true
  (predicate-assertion (lambda (x) x) "true"))

(define-for-tests assert-false
  (predicate-assertion not "false"))

(define-for-tests assert-null
  (predicate-assertion null? "an empty list"))

(define-for-tests (assert-error thunk #!optional condition-types . properties)
  (let ((condition-types (if (default-object? condition-types)
			     (list condition-type:error)
			     condition-types)))
    (call-with-current-continuation
     (lambda (k)
       (apply fail
	      'RESULT-OBJECT
	      (bind-condition-handler
		  condition-types
		  (lambda (condition)
		    condition		;ignore
		    (k #f))
		thunk)
	      'EXPECTATION-OBJECT condition-types
	      properties)))))

(define-for-tests (error-assertion . condition-types)
  (lambda (thunk . properties)
    (apply assert-error thunk condition-types properties)))

(define-for-tests assert-simple-error
  (error-assertion condition-type:simple-error))

(define-for-tests assert-type-error
  (error-assertion condition-type:wrong-type-argument))

(define-for-tests assert-range-error
  (error-assertion condition-type:bad-range-argument))

(define-for-tests keep-it-fast!?
  (let ((v (get-environment-variable "FAST")))
    (if (or (eq? v #f) (string-null? v))
	(begin
	  (warn "To avoid long run times, export FAST=y.")
	  #f)
	#t)))

(define comparator?)
(define comparator-metadata)
(define set-comparator-metadata!)
(let ((table (make-hashed-metadata-table)))
  (set! comparator? (bundle-ref table 'has?))
  (set! comparator-metadata (bundle-ref table 'get))
  (set! set-comparator-metadata! (bundle-ref table 'put!))
  unspecific)

(define-for-tests (define-comparator comparator name)
  (guarantee binary-procedure? comparator 'define-comparator)
  (guarantee symbol? name 'define-comparator)
  (set-comparator-metadata! comparator (cons name (string name " to"))))

(define (name-of comparator)
  (car (comparator-metadata comparator)))

(define (text-of comparator)
  (cdr (comparator-metadata comparator)))

(define-comparator eq? 'eq?)
(define-comparator eqv? 'eqv?)
(define-comparator equal? 'equal?)
(define-comparator < '<)
(define-comparator <= '<=)
(define-comparator = '=)
(define-comparator > '>)
(define-comparator >= '>=)
(define-comparator boolean=? 'boolean=?)

(define-comparator char<=? 'char<=)
(define-comparator char<? 'char<)
(define-comparator char=? 'char=)
(define-comparator char>=? 'char>=)
(define-comparator char>? 'char>)

(define-comparator char-ci<=? 'char-ci<=)
(define-comparator char-ci<? 'char-ci<)
(define-comparator char-ci=? 'char-ci=)
(define-comparator char-ci>=? 'char-ci>=)
(define-comparator char-ci>? 'char-ci>)

(define-comparator string<=? 'string<=)
(define-comparator string<? 'string<)
(define-comparator string=? 'string=)
(define-comparator string>=? 'string>=)
(define-comparator string>? 'string>)

(define-comparator string-ci<=? 'string-ci<=)
(define-comparator string-ci<? 'string-ci<)
(define-comparator string-ci=? 'string-ci=)
(define-comparator string-ci>=? 'string-ci>=)
(define-comparator string-ci>? 'string-ci>)

(define (binary-assertion negate? test pattern)
  (let ((test (if negate? (negate-test test) test))
	(pattern (expand-pattern negate? pattern)))
    (lambda (value expected . properties)
      (if (not (test value expected))
	  (apply fail
		 'RESULT-OBJECT value
		 'EXPECTATION (list pattern expected)
		 properties)))))

(define (negate-test test)
  (lambda (value expected)
    (not (test value expected))))

(define (expand-pattern negate? pattern)
  (append-map (lambda (element)
		(if (pattern-if? element)
		    (if negate?
			(pattern-if-negative element)
			(pattern-if-positive element))
		    (list element)))
	      pattern))

(define (if+ positive #!optional negative)
  (pattern-if (list positive)
	      (if (default-object? negative)
		  '()
		  (list negative))))

(define (if- negative)
  (pattern-if '() (list negative)))

(define-record-type <pattern-if>
    (pattern-if positive negative)
    pattern-if?
  (positive pattern-if-positive)
  (negative pattern-if-negative))

(define-for-tests (simple-binary-assertion comparator negate?)
  (binary-assertion negate?
		    comparator
		    (list "an object" (if- "not")
			  (text-of comparator) (marker))))

(define-for-tests assert-eq (simple-binary-assertion eq? #f))
(define-for-tests assert-eqv (simple-binary-assertion eqv? #f))
(define-for-tests assert-equal (simple-binary-assertion equal? #f))
(define-for-tests assert-!eq (simple-binary-assertion eq? #t))
(define-for-tests assert-!eqv (simple-binary-assertion eqv? #t))
(define-for-tests assert-!equal (simple-binary-assertion equal? #t))

(define-for-tests assert-= (simple-binary-assertion = #f))
(define-for-tests assert-!= (simple-binary-assertion = #t))
(define-for-tests assert-< (simple-binary-assertion < #f))
(define-for-tests assert-<= (simple-binary-assertion <= #f))
(define-for-tests assert-> (simple-binary-assertion > #f))
(define-for-tests assert->= (simple-binary-assertion >= #f))

(define-for-tests assert-boolean= (simple-binary-assertion boolean=? #f))
(define-for-tests assert-boolean!= (simple-binary-assertion boolean=? #t))

(define-for-tests assert-char= (simple-binary-assertion char=? #f))
(define-for-tests assert-char!= (simple-binary-assertion char=? #t))
(define-for-tests assert-char< (simple-binary-assertion char<? #f))
(define-for-tests assert-char<= (simple-binary-assertion char<=? #f))
(define-for-tests assert-char> (simple-binary-assertion char>? #f))
(define-for-tests assert-char>= (simple-binary-assertion char>=? #f))

(define-for-tests assert-char-ci= (simple-binary-assertion char-ci=? #f))
(define-for-tests assert-char-ci!= (simple-binary-assertion char-ci=? #t))
(define-for-tests assert-char-ci< (simple-binary-assertion char-ci<? #f))
(define-for-tests assert-char-ci<= (simple-binary-assertion char-ci<=? #f))
(define-for-tests assert-char-ci> (simple-binary-assertion char-ci>? #f))
(define-for-tests assert-char-ci>= (simple-binary-assertion char-ci>=? #f))

(define-for-tests assert-string= (simple-binary-assertion string=? #f))
(define-for-tests assert-string!= (simple-binary-assertion string=? #t))
(define-for-tests assert-string< (simple-binary-assertion string<? #f))
(define-for-tests assert-string<= (simple-binary-assertion string<=? #f))
(define-for-tests assert-string> (simple-binary-assertion string>? #f))
(define-for-tests assert-string>= (simple-binary-assertion string>=? #f))

(define-for-tests assert-string-ci= (simple-binary-assertion string-ci=? #f))
(define-for-tests assert-string-ci!= (simple-binary-assertion string-ci=? #t))
(define-for-tests assert-string-ci< (simple-binary-assertion string-ci<? #f))
(define-for-tests assert-string-ci<= (simple-binary-assertion string-ci<=? #f))
(define-for-tests assert-string-ci> (simple-binary-assertion string-ci>? #f))
(define-for-tests assert-string-ci>= (simple-binary-assertion string-ci>=? #f))

(define-for-tests (member-assertion comparator negate?)
  (binary-assertion negate?
		    (lambda (value expected)
		      (any (lambda (x) (comparator value x)) expected))
		    (list "an object" (if- "not") "in"
			  (marker)
			  "compared using" (name-of comparator))))

(define-for-tests assert-memq (member-assertion eq? #f))
(define-for-tests assert-memv (member-assertion eqv? #f))
(define-for-tests assert-member (member-assertion equal? #f))
(define-for-tests assert-!memq (member-assertion eq? #t))
(define-for-tests assert-!memv (member-assertion eqv? #t))
(define-for-tests assert-!member (member-assertion equal? #t))

(define-for-tests (assert-list= comparator . args)
  (apply (list-assertion comparator #f) args))

(define-for-tests (assert-list!= comparator . args)
  (apply (list-assertion comparator #t) args))

(define (list-assertion comparator negate?)
  (binary-assertion negate?
		    (lambda (value expected)
		      (and (list? value)
			   (= (length value) (length expected))
			   (every comparator value expected)))
		    (list "a list with"
			  (if+ "the same elements as" "different elements from")
			  (marker)
			  "comparing elements with" (name-of comparator)
			  "in the same order")))

(define-for-tests (assert-lset= comparator . args)
  (apply (lset=-assertion comparator #f) args))

(define-for-tests (assert-lset!= comparator . args)
  (apply (lset=-assertion comparator #t) args))

(define (lset=-assertion comparator negate?)
  (binary-assertion negate?
		    (lambda (value expected)
		      (lset= comparator value expected))
		    (list "a list with"
			  (if+ "the same elements as" "different elements from")
			  (marker)
			  "comparing elements with" (name-of comparator)
			  "in any order")))