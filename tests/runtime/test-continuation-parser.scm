#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017, 2018, 2019, 2020, 2021, 2022 Massachusetts Institute of
    Technology

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

;;;; Tests of control points

;;; Frame types to test

;; Subproblems:

;; [x] access-continue
;; [x] assignment-continue
;; [ ] combination-apply
;; [ ] combination-save-value
;; [ ] compiler-assignment-trap-restart
;; [ ] compiler-error-restart
;; [ ] compiler-lookup-apply-trap-restart
;; [ ] compiler-operator-lookup-trap-restart
;; [ ] compiler-reference-trap-restart
;; [ ] compiler-safe-reference-trap-restart
;; [ ] compiler-unassigned?-trap-restart
;; [x] conditional-decide
;; [ ] definition-continue
;; [x] disjunction-decide
;; [ ] eval-error
;; [ ] hardware-trap
;; [ ] internal-apply
;; [ ] internal-apply-val
;; [x] sequence-continue
;; [ ] compiled-address (cc)
;; [ ] cc-bkpt (cc)

;; Non-subproblems:

;; [ ] compiler-interrupt-restart
;; [ ] compiler-link-caches-restart
;; [ ] reenter-compiled-code
;; [ ] pop-return-error
;; [ ] restore-interrupt-mask
;; [ ] restore-dont-copy-history
;; [ ] restore-history
;; [ ] restore-value
;; [ ] stack-marker
;; [ ] return-to-interpreter (cc)
;; [ ] cc-internal-apply (cc)
;; [ ] cc-restore-interrupt-mask (cc)
;; [ ] cc-stack-marker (cc)
;; [ ] cc-invocation (cc)

(define reference-cont)
(call-with-current-continuation
  (lambda (k)
    (set! reference-cont k)))

(define test-directory
  (directory-pathname (current-load-pathname)))

(define data-directory
  (merge-pathnames "test-continuation-parser-data/" test-directory))

(define (data-pathname name)
  (merge-pathnames name data-directory))

(define-test 'decoded-frames
  (lambda ()
    (let ((frames (continuation->cframe-stream reference-cont)))
      (stream-for-each
       (lambda (frame)
	 (let* ((raw (cframe-raw frame))
		(end (vector-length raw)))
	   (assert-eqv (cframe-length frame) end)
	   (for-each (lambda (i)
		       (assert-eqv (cframe-ref frame i)
				   (vector-ref raw i)))
		     (iota end)))
	 (let ((return (cframe-ref frame 0)))
	   (assert-eqv (cframe-return-address frame)
		       return)
	   (assert-eqv (cframe-compiled-code? frame)
		       (compiled-return-address? return))
	   (assert-eqv (cframe-return-code frame)
		       (and (interpreter-return-address? return)
			    (return-address/code return)))))
       frames)
      (let ((cp (cframe-stream->control-point frames)))
	(assert-true (control-point? cp))
	(let ((end (system-vector-length cp))
	      (reference-cp (continuation/control-point reference-cont)))
	  (assert-eqv end (system-vector-length reference-cp))
	  (for-each (lambda (i)
		      (assert-eqv (system-vector-ref cp i)
				  (system-vector-ref reference-cp i)))
		    (iota end)))))))

(define (get-continuation thunk)
  (call-with-current-continuation
    (lambda (k)
      (with-exception-handler
          (lambda (c)
            (k (condition/continuation c)))
        thunk))))

(define (get-cframe-stream thunk)
  (continuation->cframe-stream (get-continuation thunk)))

(define defined-frames
  (alist-table eq?))

(define (define-frame type . keylist)
  (alist-table-set! defined-frames type
		    (cons (cons 'type type)
			  (keyword-list->alist keylist))))

(define (frame-props)
  (alist-table-values defined-frames))

(define (prop-ref props name #!optional default)
  (let ((p (assq name props)))
    (if p
	(cdr p)
	(begin
	  (if (default-object? default)
	      (error "Unknown property:" name))
	  default))))

(define (frame-info props)
  (values (prop-ref props 'type)
	  (prop-ref props 'exp-field)
	  (prop-ref props 'env-field #f)
	  (prop-ref props 'exp-pred)
	  (frame-subexp props 'frame-subexp 'exp-subexp)))

(define (frame-cc-info props)
  (values (prop-ref props 'type 'compiled-address)
	  (prop-ref props 'cc-type 'compiled-address)
	  (prop-ref props 'cc-exp-pred (prop-ref props 'exp-pred))
	  (frame-subexp props 'cc-frame-subexp 'cc-exp-subexp 'exp-subexp)))

(define (frame-subexp props keyword . exp-subexps)
  (or (prop-ref props keyword #f)
      (let loop ((exp-subexps exp-subexps))
	(and (pair? exp-subexps)
	     (let ((exp-subexp (prop-ref props (car exp-subexps) #f)))
	       (if exp-subexp
		   (lambda (frame exp)
		     (declare (ignore frame))
		     (exp-subexp exp))
		   (loop (cdr exp-subexps))))))))

(define-frame 'access-continue
  'exp-field 'expression
  'exp-pred scode-access?
  'exp-subexp scode-access-environment
  'cc-exp-pred
  (lambda (exp)
    (and (scode-combination? exp)
	 (eq? (scode-combination-operator exp)
	      (make-primitive-procedure 'lexical-reference))))
  'cc-exp-subexp
  (lambda (exp)
    (scode-combination-operand exp 0)))

(define-frame 'assignment-continue
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-assignment?
  'exp-subexp scode-assignment-value)

(define-frame 'combination-apply
  'exp-field 'expression
  'exp-pred scode-combination?
  'exp-subexp scode-combination-operator)

(define-frame 'combination-save-value
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-combination?
  'frame-subexp
  (lambda (frame exp)
    (scode-combination-element exp
			       (cframe-field-value frame 'number-of-blanks))))

(define-frame 'conditional-decide
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-conditional?
  'exp-subexp scode-conditional-predicate)

#;
(define-frame 'definition-continue
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-definition?
  'exp-subexp scode-definition-value)

(define-frame 'disjunction-decide
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-disjunction?
  'exp-subexp scode-disjunction-predicate
  'cc-exp-pred
  (lambda (exp)
    (and (scode-combination? exp)
	 (let ((operator (scode-combination-operator exp)))
	   (and (scode-lambda? operator)
		(eq? (scode-lambda-name operator)
		     scode-lambda-name:let)
		(= 1 (length (scode-combination-operands exp)))))))
  'cc-exp-subexp
  (lambda (exp)
    (scode-combination-operand exp 0)))

(define-frame 'sequence-continue
  'exp-field 'expression
  'env-field 'environment
  'exp-pred scode-sequence?
  'exp-subexp scode-sequence-first)

(define-test 'simple-interpreted-subproblems
  (map (lambda (props)
	 (let-values (((type exp-field env-field exp-pred get-subexp)
		       (frame-info props)))
	   (lambda ()
	     (let ((env (make-top-level-environment)))
	       (define (do-one thunk)
		 (let ((cfs
			(cframe-stream-skip-non-subproblems
			 (get-cframe-stream thunk))))
		   (assert-true (stream-pair? cfs))
		   (let ((cf (stream-car cfs)))
		     (assert-eq (cframe-type cf) type)
		     (let ((exp (cframe-field-value cf exp-field))
			   (env* (cframe-field-value cf env-field #f)))
		       (assert-true (exp-pred exp))
		       (if env-field
			   (assert-eqv (environment-parent env*) env))
		       (let ((info (cframe-debugging-info cf)))
			 (assert-eqv (debugging-info*/expression info)
				     exp)
			 (if env-field
			     (assert-eqv (debugging-info*/environment info) env*)
			     (assert-true
			      (debugging-info*/undefined-environment?
			       (debugging-info*/environment info))))
			 (if get-subexp
			     (assert-eqv (debugging-info*/subexpression info)
					 (get-subexp cf exp))))))))
	       (map do-one
		    (load (data-pathname
			   (string-append (symbol->string type) ".scm"))
			  env))))))
       (frame-props)))

(define-test 'simple-compiled-subproblems
  (map (lambda (props)
	 (let-values (((type cc-type exp-pred get-subexp)
		       (frame-cc-info props)))
	   (lambda ()
	     (let ((env (make-top-level-environment)))
	       (define (do-one thunk)
		 (let ((cfs
			(cframe-stream-skip-non-subproblems
			 (get-cframe-stream thunk))))
		   (assert-true (stream-pair? cfs))
		   (let ((cf (stream-car cfs)))
		     (assert-eq (cframe-type cf) cc-type)
		     (let* ((info (cframe-debugging-info cf))
			    (exp (debugging-info*/expression info)))
		       (assert-true (exp-pred exp))
		       (if get-subexp
			   (assert-eqv (debugging-info*/subexpression info)
				       (get-subexp cf exp)))))))
	       (let ((pn (data-pathname (symbol->string type))))
		 (compile-file pn)
		 (map do-one (load pn env)))))))
       (frame-props)))