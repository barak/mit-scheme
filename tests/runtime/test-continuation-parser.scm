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

;;; [ ] untested; [x] tested; [?] can't test?

;; Subproblems:

;; [x] access-continue
;; [x] assignment-continue
;; [ ] cc-bkpt
;; [x] combination-apply
;; [x] combination-save-value
;; [x] compiled-address
;; [x] compiler-assignment-trap-restart
;; [?] compiler-error-restart
;; [?] compiler-lookup-apply-trap-restart
;; [x] compiler-operator-lookup-trap-restart
;; [x] compiler-reference-trap-restart
;; [x] compiler-safe-reference-trap-restart
;; [?] compiler-unassigned?-trap-restart
;; [x] conditional-decide
;; [?] definition-continue
;; [x] disjunction-decide
;; [ ] eval-error
;; [ ] hardware-trap
;; [x] internal-apply
;; [x] internal-apply-val
;; [x] sequence-continue

;; Non-subproblems:

;; [ ] cc-internal-apply
;; [ ] cc-invocation
;; [ ] cc-restore-interrupt-mask
;; [ ] cc-stack-marker
;; [ ] compiler-interrupt-restart
;; [ ] compiler-link-caches-restart
;; [ ] pop-return-error
;; [ ] reenter-compiled-code
;; [ ] restore-dont-copy-history
;; [ ] restore-history
;; [ ] restore-interrupt-mask
;; [ ] restore-value
;; [ ] return-to-interpreter
;; [ ] stack-marker

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
	   (compare-seqs end
			 (lambda (i) (cframe-ref frame i))
			 (lambda (i) (vector-ref raw i))))
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
	(let* ((reference-cp (continuation/control-point reference-cont))
	       (end (control-point-length reference-cp)))
	  (assert-eqv (control-point-length cp) (+ end 6))
	  (compare-seqs end
			(lambda (i)
			  (control-point-ref cp (+ i 6)))
			(lambda (i)
			  (control-point-ref reference-cp i))))))))

(define (compare-seqs end get1 get2)
  (let loop ((i 0))
    (if (< i end)
	(let ((elt (get1 i)))
	  (assert-eqv elt (get2 i))
	  (loop
	   (let ((i* (+ i 1)))
	     (if (manifest-nmv? elt)
		 (+ i* (manifest-nmv-datum elt))
		 i*)))))))

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
    (let ((alist (keyword-list->alist keylist)))
      (append (if (assq 'frame-type alist)
		  '()
		  (list (cons 'frame-type type)))
	      (if (assq 'test-file alist)
		  '()
		  (list (cons 'test-file (symbol->string type))))
	      alist))))

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

(define (frame-int-info props)
  (values (prop-ref props 'test-file)
	  (prop-ref props 'frame-type)
	  (prop-ref props 'exp-pred)
	  (frame-subexp props 'frame-subexp 'exp-subexp)))

(define (frame-int? props)
  (prop-ref props 'int? #t))

(define (frame-cc-info props)
  (values (prop-ref props 'test-file)
	  (prop-ref props 'cc-frame-type (prop-ref props 'frame-type))
	  (prop-ref props 'cc-exp-pred (prop-ref props 'exp-pred))
	  (frame-subexp props 'cc-frame-subexp 'cc-exp-subexp 'exp-subexp)))

(define (frame-cc? props)
  (prop-ref props 'cc? #t))

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
  'exp-pred scode-access?
  'exp-subexp scode-access-environment
  'cc-frame-type 'compiled-address
  'cc-exp-pred
  (lambda (exp)
    (and (scode-combination? exp)
	 (eq? (scode-combination-operator exp)
	      (make-primitive-procedure 'lexical-reference))))
  'cc-exp-subexp
  (lambda (exp)
    (scode-combination-operand exp 0)))

(define-frame 'internal-apply
  'exp-pred scode-combination?)

(define-frame 'internal-apply-val
  'exp-pred scode-combination?)

(define-frame 'assignment-continue
  'exp-pred scode-assignment?
  'exp-subexp scode-assignment-value
  'cc-frame-type 'compiled-address)

(define-frame 'combination-apply
  'exp-pred scode-combination?
  'exp-subexp scode-combination-operator
  'cc-frame-type 'compiled-address)

(define-frame 'combination-save-value
  'exp-pred scode-combination?
  'frame-subexp
  (lambda (frame exp)
    (scode-combination-element exp
			       (cframe-field-value frame 'number-of-blanks)))
  'cc-frame-type 'compiled-address)

(define-frame 'compiler-assignment-trap-restart
  'int? #f
  'exp-pred
  (lambda (exp)
    (and (scode-assignment? exp)
	 (eq? (scode-assignment-name exp) 'no-such-variable)
	 (eqv? (scode-assignment-value exp) 3))))

(define-frame 'compiler-operator-lookup-trap-restart
  'int? #f
  'exp-pred
  (lambda (exp)
    (and (scode-combination? exp)
	 (let ((op (scode-combination-operator exp)))
	   (and (scode-variable? op)
		(eq? (scode-variable-name op) 'no-such-variable)))
	 (equal? (scode-combination-operands exp) '(13)))))

(define-frame 'compiler-reference-trap-restart
  'int? #f
  'exp-pred
  (lambda (exp)
    (and (scode-variable? exp)
	 (eq? (scode-variable-name exp) 'no-such-variable)
	 (not (scode-variable-safe? exp)))))

(define-frame 'compiler-safe-reference-trap-restart
  'int? #f
  'exp-pred
  (lambda (exp)
    (and (scode-variable? exp)
	 (eq? (scode-variable-name exp) 'no-such-variable)
	 (scode-variable-safe? exp))))

(define-frame 'conditional-decide
  'exp-pred scode-conditional?
  'exp-subexp scode-conditional-predicate
  'cc-frame-type 'compiled-address)

(define-frame 'disjunction-decide
  'exp-pred scode-disjunction?
  'exp-subexp scode-disjunction-predicate
  'cc-frame-type 'compiled-address
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
  'exp-pred scode-sequence?
  'exp-subexp scode-sequence-first
  'cc-frame-type 'compiled-address)

(define-test 'simple-interpreted-subproblems
  (map (lambda (props)
	 (if (frame-int? props)
	     (let-values (((test-file frame-type exp-pred get-subexp)
			   (frame-int-info props)))
	       (lambda ()
		 (let ((env (make-top-level-environment)))
		   ((simple-subproblem-runner frame-type exp-pred get-subexp
					      env)
		    (load (data-pathname (pathname-new-type test-file "scm"))
			  env)))))
	     (lambda () unspecific)))
       (frame-props)))

(cond-expand
  ((not target-arch=none)
   (define-test 'simple-compiled-subproblems
     (map (lambda (props)
	    (if (frame-cc? props)
		(let-values (((test-file frame-type exp-pred get-subexp)
			      (frame-cc-info props)))
		  (lambda ()
		    (let ((env (make-top-level-environment))
			  (pn (data-pathname test-file)))
		      (compile-file pn)
		      ((simple-subproblem-runner frame-type exp-pred get-subexp
						 env)
		       (load pn env)))))
		(lambda () unspecific)))
	  (frame-props)))))

(define ((simple-subproblem-runner frame-type exp-pred get-subexp env) thunks)
  (for-each (lambda (thunk)
	      (let ((cfs
		     (stream-filter cframe-subproblem?
				    (get-cframe-stream thunk))))
		(assert-true (stream-pair? cfs))
		(let ((cf (stream-car cfs)))
		  (assert-eq (cframe-type cf) frame-type)
		  (let ((exp (cframe-dbg-expression cf))
			(env* (cframe-stream-dbg-environment cfs))
			(subexp (cframe-dbg-subexpression cf)))
		    (assert-true (exp-pred exp))
		    (if (not (or (cframe-dbg-environment-undefined? env*)
				 (eq? env* env)))
			(assert-eqv (environment-parent env*) env))
		    (if get-subexp
			(assert-eqv subexp (get-subexp cf exp)))))))
	    thunks))