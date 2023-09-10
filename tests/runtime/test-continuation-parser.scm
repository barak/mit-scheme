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

(define simple-subproblems
  (list (list 'access-continue
	      scode-access?
	      scode-access-environment
	      (lambda (exp)
		(and (scode-combination? exp)
		     (eq? (scode-combination-operator exp)
			  (make-primitive-procedure 'lexical-reference))))
	      (lambda (exp)
		(scode-combination-operand exp 0)))
	(list 'assignment-continue
	      scode-assignment?
	      scode-assignment-value)
	(list 'conditional-decide
	      scode-conditional?
	      scode-conditional-predicate)
	;; (list 'definition-continue
	;;       scode-definition?
	;;       scode-definition-value)
	(list 'disjunction-decide
	      scode-disjunction?
	      scode-disjunction-predicate
	      (lambda (exp)
		(and (scode-combination? exp)
		     (let ((operator (scode-combination-operator exp)))
		       (and (scode-lambda? operator)
			    (eq? (scode-lambda-name operator)
				 scode-lambda-name:let)
			    (= 1 (length (scode-combination-operands exp)))))))
	      (lambda (exp)
		(scode-combination-operand exp 0)))
	(list 'sequence-continue
	      scode-sequence?
	      scode-sequence-first)))

(define (interpreted-subproblem-args entry)
  (values (list-ref entry 0) (list-ref entry 1) (list-ref entry 2)))

(define (compiled-subproblem-args entry)
  (if (= (length entry) 5)
      (values (list-ref entry 0) (list-ref entry 3) (list-ref entry 4))
      (values (list-ref entry 0) (list-ref entry 1) (list-ref entry 2))))

(define-test 'simple-interpreted-subproblems
  (map (lambda (entry)
	 (let-values (((type pred get-subexp)
		       (interpreted-subproblem-args entry)))
	   (lambda ()
	     (let* ((env (make-top-level-environment))
		    (cfs
		     (cframe-stream-skip-non-subproblems
		      (get-cframe-stream
		       (load (data-pathname
			      (string-append (symbol->string type) ".scm"))
			     env)))))
	       (assert-true (stream-pair? cfs))
	       (let ((cf (stream-car cfs)))
		 (assert-eq (cframe-type cf) type)
		 (let ((exp (cframe-field-value cf 'expression)))
		   (assert-true (pred exp))
		   (let ((info (cframe-debugging-info cf)))
		     (assert-eqv (debugging-info*/expression info)
				 exp)
		     (if (eq? type 'access-continue)
			 (assert-true
			  (debugging-info*/undefined-environment?
			   (debugging-info*/environment info)))
			 (assert-eqv (environment-parent
				      (debugging-info*/environment info))
				     env))
		     (assert-eqv (debugging-info*/subexpression info)
				 (get-subexp exp)))))))))
       simple-subproblems))

(define-test 'simple-compiled-subproblems
  (map (lambda (entry)
	 (let-values (((type pred get-subexp)
		       (compiled-subproblem-args entry)))
	   (lambda ()
	     (let ((pn (data-pathname (symbol->string type))))
	       (compile-file pn)
	       (let* ((env (make-top-level-environment))
		      (cfs
		       (cframe-stream-skip-non-subproblems
			(get-cframe-stream
			 (load pn env)))))
		 (assert-true (stream-pair? cfs))
		 (let ((cf (stream-car cfs)))
		   (assert-eq (cframe-type cf) 'compiled-address)
		   (let* ((info (cframe-debugging-info cf))
			  (exp (debugging-info*/expression info)))
		     (assert-true (pred exp))
		     (assert-eqv (debugging-info*/subexpression info)
				 (get-subexp exp)))))))))
       simple-subproblems))