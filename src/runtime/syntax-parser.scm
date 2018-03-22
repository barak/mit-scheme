#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Syntax parser combinator language
;;; package: (runtime syntax parser)

(declare (usual-integrations))

;;; A "syntax parser" as defined here is a procedure with the following
;;; signature:
;;;
;;;     (lambda (input senv output success failure) ...)
;;;
;;; A parser is called with a bunch of arguments that are the parsing state.  A
;;; parser consumes none, some, or all of the input, and produces an arbitrary
;;; number of results.  A parser never returns; instead it calls either the
;;; SUCCESS or FAILURE continuation, depending on whether it was able to match
;;; the input.
;;;
;;; A parser consumes input by calling INPUT with an operation argument; see the
;;; code for details.  The SENV argument has its normal meaning.
;;;
;;; A parser produces output by calling %OUTPUT-PUSH with an arbitrary object
;;; <x>, which returns a new accumulator containing that object in addition to
;;; any other output previously saved.  The saved output objects can be obtained
;;; with %OUTPUT-ALL; they are returned in the order in which they were saved,
;;; so that the most recently saved object is the last element of the returned
;;; list.
;;;
;;; A successful match tail-recursively calls the SUCCESS continuation like
;;; this:
;;;
;;;     (success input* senv* output* failure*)
;;;
;;; where INPUT* is derived from INPUT by zero or more 'CDR operations; SENV*
;;; may be any syntactic environment; OUTPUT* must be a accumulator object
;;; derived from OUTPUT; and FAILURE* must be a thunk that never returns and
;;; eventually tail-recurses into SUCCESS or FAILURE.
;;;
;;; A failed match tail-recursively calls the FAILURE continuation like this:
;;;
;;;     (failure)

(define (spar-call spar form use-senv hist closing-senv)
  (spar (%new-input form hist closing-senv)
	use-senv
	(%new-output)
	(lambda (input senv output failure)
	  (declare (ignore senv failure))
	  (if (not (%input-null? input))
	      (error "Rule failed to match entire form."))
	  (output 'get-only))
	(lambda ()
	  (serror form use-senv hist "Ill-formed syntax:" form))))

;;;; Inputs and outputs

(define (%new-input form hist closing-senv)
  (let loop ((form form) (hist hist))
    (lambda (operator)
      (case operator
	((form) form)
	((hist) hist)
	((closing-senv) closing-senv)
	((car) (loop (car form) (hist-car hist)))
	((cdr) (loop (cdr form) (hist-cdr hist)))
	((discard) (loop '() (initial-hist '())))
	(else (error "Unknown operator:" operator))))))

(define (%input-form input) (input 'form))
(define (%input-hist input) (input 'hist))
(define (%input-closing-senv input) (input 'closing-senv))
(define (%input-car input) (input 'car))
(define (%input-cdr input) (input 'cdr))
(define (%input-discard input) (input 'discard))

(define (%input-pair? input) (pair? (%input-form input)))
(define (%input-null? input) (null? (%input-form input)))

(define (%new-output)
  (let loop ((objects '()))
    (lambda (op . args)
      (apply (case op
	       ((push)
		(lambda (object)
		  (loop (cons object objects))))
	       ((push-all)
		(lambda (objects*)
		  (guarantee list? objects*)
		  (let add ((objects* objects*) (objects objects))
		    (if (pair? objects*)
			(add (cdr objects*)
			     (cons (car objects*) objects))
			(loop objects)))))
	       ((top)
		(lambda ()
		  (car objects)))
	       ((pop)
		(lambda ()
		  (loop (cdr objects))))
	       ((pop-all)
		(lambda ()
		  (loop '())))
	       ((get-all)
		(lambda ()
		  (reverse objects)))
	       ((get-only)
		(lambda ()
		  (if (not (and (pair? objects)
				(null? (cdr objects))))
		      (error "Expected a single value:" objects))
		  (car objects)))
	       (else
		(error "Unknown operation:" op)))
	     args))))

(define (%output-top output) (output 'top))
(define (%output-all output) (output 'get-all))
(define (%output-pop output) (output 'pop))
(define (%output-pop-all output) (output 'pop-all))
(define (%output-push output object) (output 'push object))
(define (%output-push-all output objects) (output 'push-all objects))

;;;; Primitives

(define (%subst-args input senv output args)
  (map (lambda (arg)
	 (%subst-arg input senv output arg))
       args))

(define (%subst-arg input senv output arg)
  (cond ((eq? arg spar-arg:form) (%input-form input))
	((eq? arg spar-arg:hist) (%input-hist input))
	((eq? arg spar-arg:close) (make-closer (%input-closing-senv input)))
	((eq? arg spar-arg:senv) senv)
	((eq? arg spar-arg:value) (%output-top output))
	((eq? arg spar-arg:values) (%output-all output))
	(else arg)))

(define (make-closer senv)
  (lambda (expr)
    (close-syntax expr senv)))

(define-deferred spar-arg:form (string->uninterned-symbol ".form."))
(define-deferred spar-arg:hist (string->uninterned-symbol ".hist."))
(define-deferred spar-arg:close (string->uninterned-symbol ".close."))
(define-deferred spar-arg:senv (string->uninterned-symbol ".senv."))
(define-deferred spar-arg:value (string->uninterned-symbol ".value."))
(define-deferred spar-arg:values (string->uninterned-symbol ".values."))

(define (spar-match predicate . args)
  (lambda (input senv output success failure)
    (if (apply predicate (%subst-args input senv output args))
	(success input senv output failure)
	(failure))))

(define (spar-push . args)
  (lambda (input senv output success failure)
    (success input
	     senv
	     (%output-push-all output (%subst-args input senv output args))
	     failure)))

(define (spar-push-if predicate . args)
  (spar-seq (apply spar-match predicate args)
	    (apply spar-push args)))

(define (spar-push-value procedure . args)
  (lambda (input senv output success failure)
    (success input
	     senv
	     (%output-push output
			   (apply procedure
				  (%subst-args input senv output args)))
	     failure)))

(define (spar-error message . irritants)
  (lambda (input senv output success failure)
    (declare (ignore success failure))
    (apply serror
	   (%input-form input)
	   senv
	   (%input-hist input)
	   message
	   (%subst-args input senv output irritants))))

(define (spar-discard-form input senv output success failure)
  (success (%input-discard input) senv output failure))

;;;; Repeat combinators

(define (spar-opt . spars)
  (let ((spar (%seq spars)))
    (lambda (input senv output success failure)
      (spar input senv output success
	    (lambda ()
	      (success input senv output failure))))))

(define (spar* . spars)
  (let ((spar (%seq spars)))
    (lambda (input senv output success failure)
      (letrec
	  ((loop
	    (lambda (input senv output failure)
	      (spar input senv output loop
		    (lambda ()
		      (success input senv output failure))))))
	(loop input senv output failure)))))

(define (spar+ . spars)
  (let ((spar (%seq spars)))
    (spar-seq spar (spar* spar))))

(define (spar-repeat n-min n-max . spars)
  (guarantee exact-nonnegative-integer? n-min 'spar-repeat)
  (if n-max
      (begin
	(guarantee exact-nonnegative-integer? n-max 'spar-repeat)
	(if (not (>= n-max n-min))
	    (error:bad-range-argument n-max 'spar-repeat))))
  (let ((spar (%seq spars)))
    (let ((s1
	   (case n-min
	     ((0) #f)
	     ((1) spar)
	     (else (%repeat-exact spar n-min))))
	  (s2
	   (if n-max
	       (let ((delta (- n-max n-min)))
		 (case delta
		   ((0) #f)
		   ((1) spar)
		   (else (%repeat-up-to spar delta))))
	       (spar* spar))))
      (cond ((and s1 s2) (spar-seq s1 s2))
	    ((or s1 s2))
	    (else spar-succeed)))))

(define (%repeat-exact spar n)
  (lambda (input senv output success failure)
    (letrec
	((loop
	  (lambda (n input senv output failure)
	    (if (> n 0)
		(spar input senv output
		      (lambda (input* senv* output* failure*)
			(loop (- n 1) input* senv* output* failure*))
		      failure)
		(success input senv output failure)))))
      (loop n input senv output failure))))

(define (%repeat-up-to spar n)
  (lambda (input senv output success failure)
    (letrec
	((loop
	  (lambda (n senv input output failure)
	    (if (> n 0)
		(spar input senv output
		      (lambda (input* senv* output* failure*)
			(loop (- n 1) input* senv* output* failure*))
		      (lambda ()
			(success input senv output failure)))
		(success input senv output failure)))))
      (loop n input senv output failure))))

;;;; Sequence and alternative

(define (spar-seq . spars)
  (%seq spars))

(define (%seq spars)
  (cond ((not (pair? spars)) spar-succeed)
        ((not (pair? (cdr spars))) (car spars))
        (else (reduce-right %seq-combiner #f spars))))

(define (%seq-combiner s1 s2)
  (lambda (input senv output success failure)
    (s1 input senv output
	(lambda (input* senv* output* failure*)
	  (s2 input* senv* output* success failure*))
	failure)))

(define (spar-or . spars)
  (cond ((not (pair? spars)) spar-fail)
        ((not (pair? (cdr spars))) (car spars))
        (else (reduce-right %or-combiner #f spars))))

(define (%or-combiner s1 s2)
  (lambda (input senv output success failure)
    (s1 input senv output success
	(lambda ()
	  (s2 input senv output success failure)))))

(define (spar-succeed input senv output success failure)
  (success input senv output failure))

(define (spar-fail input senv output success failure)
  (declare (ignore input senv output success))
  (failure))

;;;; Element combinators

(define (spar-elt . spars)
  (let ((spar (%seq spars)))
    (lambda (input senv output success failure)
      (if (%input-pair? input)
	  (spar (%input-car input)
		senv
		output
		(lambda (input* senv* output* failure*)
		  (declare (ignore input*))
		  (success (%input-cdr input) senv* output* failure*))
		failure)
	  (failure)))))

(define (spar-match-elt predicate . args)
  (spar-elt (apply spar-match predicate args)))

(define (spar-push-elt . args)
  (spar-elt (apply spar-push args)))

(define (spar-push-elt-if predicate . args)
  (spar-elt (apply spar-push-if predicate args)))

(define (spar-match-null)
  (spar-match null? spar-arg:form))

;;;; Classifier support

(define (spar-with-mapped-senv procedure . spars)
  (let ((spar (%seq spars)))
    (lambda (input senv output success failure)
      (spar input
	    (procedure senv)
	    output
	    (lambda (input* senv* output* failure*)
	      (declare (ignore senv*))
	      (success input* senv output* failure*))
	    failure))))

(define-deferred spar-push-classified
  (spar-push-value classify-form
		   spar-arg:form
		   spar-arg:senv
		   spar-arg:hist))

(define-deferred spar-push-deferred-classified
  (spar-push-value (lambda (form senv hist)
		     (lambda ()
		       (classify-form form senv hist)))
		   spar-arg:form
		   spar-arg:senv
		   spar-arg:hist))

(define-deferred spar-push-open-classified
  (spar-push-value (lambda (form senv hist)
		     (declare (ignore senv))
		     (lambda (senv*)
		       (classify-form form senv* hist)))
		   spar-arg:form
		   spar-arg:senv
		   spar-arg:hist))

(define-deferred spar-push-body
  (spar-seq
    (spar-encapsulate-values
	(lambda (elts)
	  (lambda (frame-senv)
	    (let ((body-senv (make-internal-senv frame-senv)))
	      (map-in-order (lambda (elt) (elt body-senv))
			    elts))))
      (spar+ (spar-elt spar-push-open-classified))
      (spar-match-null))
    (spar-push spar-arg:senv)))

;;;; Value combinators

(define (spar-encapsulate-values procedure . spars)
  (%encapsulate procedure spars))

(define (spar-call-with-values procedure . spars)
  (%encapsulate (lambda (values) (apply procedure values)) spars))

(define (%encapsulate procedure spars)
  (%with-output (lambda (output output*)
		  (%output-push output (procedure (%output-all output*))))
		spars))

(define (spar-transform-values procedure . spars)
  (%transform procedure spars))

(define (spar-map-values procedure . spars)
  (%transform (lambda (values) (map procedure values)) spars))

(define (spar-append-map-values procedure . spars)
  (%transform (lambda (values) (append-map procedure values)) spars))

(define (spar-filter-map-values procedure . spars)
  (%transform (lambda (values) (filter-map procedure values)) spars))

(define (%transform procedure spars)
  (%with-output (lambda (output output*)
		  (%output-push-all output (procedure (%output-all output*))))
		spars))

(define (%with-output procedure spars)
  (let ((spar (%seq spars)))
    (lambda (input senv output success failure)
      (spar input
	    senv
	    (%output-pop-all output)
	    (lambda (input* senv* output* failure*)
	      (success input*
		       senv*
		       (procedure output output*)
		       failure*))
	    failure))))

;;;; Shorthand

(define (spar-top-level pattern procedure)
  (spar-call-with-values procedure
    (spar-elt)
    (spar-push spar-arg:close)
    (pattern->spar pattern)))

(define (make-pattern-compiler expr? caller)
  (call-with-constructors expr?
    (lambda (:* :+ :call :close :cons :elt :eqv? :form :hist :identifier? :list
		:match-elt :match-null :mit-bvl? :opt :or :push :push-elt
		:push-elt-if :push-value :r4rs-bvl? :senv :seq :symbol? :value)

      (define (loop pattern)
	(cond ((symbol? pattern)
	       (case pattern
		 ((symbol) (:push-elt-if (:symbol?) (:form)))
		 ((identifier id) (:push-elt-if (:identifier?) (:form)))
		 ((form expr) (:push-elt (:form)))
		 ((r4rs-bvl) (:push-elt-if (:r4rs-bvl?) (:form)))
		 ((mit-bvl) (:push-elt-if (:mit-bvl?) (:form)))
		 ((ignore) (:elt))
		 (else (bad-pattern pattern))))
	      ((procedure? pattern)
	       (:push-elt-if pattern (:form)))
	      ((and (pair? pattern)
		    (list? (cdr pattern)))
	       (case (car pattern)
		 ((*) (apply :* (map loop (cdr pattern))))
		 ((+) (apply :+ (map loop (cdr pattern))))
		 ((?) (apply :opt (map loop (cdr pattern))))
		 ((or) (apply :or (map loop (cdr pattern))))
		 ((seq) (apply :seq (map loop (cdr pattern))))
		 ((quote)
		  (if (not (and (pair? (cdr pattern))
				(null? (cddr pattern))))
		      (bad-pattern pattern))
		  (:match-elt (:eqv?) (cadr pattern) (:form)))
		 ((push) (apply :push (map convert-spar-arg (cdr pattern))))
		 ((push-value)
		  (apply :push-value
			 (cadr pattern)
			 (map convert-spar-arg (cddr pattern))))
		 ((list) (apply :call (:list) (map loop (cdr pattern))))
		 ((cons) (apply :call (:cons) (map loop (cdr pattern))))
		 ((call) (apply :call (cadr pattern) (map loop (cddr pattern))))
		 ((spar) (apply :seq (cdr pattern)))
		 ((elt)
		  (:elt (apply :seq (map loop (cdr pattern)))
			(:match-null)))
		 (else
		  (bad-pattern pattern))))
	      (else
	       (bad-pattern pattern))))

      (define (convert-spar-arg arg)
	(case arg
	  ((form) (:form))
	  ((hist) (:hist))
	  ((close) (:close))
	  ((senv) (:senv))
	  ((value) (:value))
	  (else arg)))

      (define (bad-pattern pattern)
	(error:wrong-type-argument pattern "syntax-parser pattern" caller))

      (lambda (pattern)
	(if (not (list? pattern))
	    (bad-pattern pattern))
	(:seq (apply :seq (map loop pattern))
	      (:match-null))))))

(define (call-with-constructors expr? procedure)

  (define (proc name procedure)
    (if expr?
	(lambda args (cons name args))
	(lambda args (apply procedure args))))

  (define (flat-proc name procedure)
    (if expr?
	(lambda args (cons name (elide-seqs args)))
	(lambda args (apply procedure args))))

  (define (elide-seqs exprs)
    (append-map (lambda (expr)
		  (if (and (pair? expr)
			   (eq? 'spar-seq (car expr)))
		      (cdr expr)
		      (list expr)))
		exprs))

  (define (const name value)
    (if expr?
	(lambda () name)
	(lambda () value)))

  (procedure (flat-proc 'spar* spar*)
	     (flat-proc 'spar+ spar+)
	     (flat-proc 'spar-call-with-values spar-call-with-values)
	     (const 'spar-arg:close spar-arg:close)
	     (const 'cons cons)
	     (flat-proc 'spar-elt spar-elt)
	     (const 'eqv? eqv?)
	     (const 'spar-arg:form spar-arg:form)
	     (const 'spar-arg:hist spar-arg:hist)
	     (const 'identifier? identifier?)
	     (const 'list list)
	     (proc 'spar-match-elt spar-match-elt)
	     (proc 'spar-match-null spar-match-null)
	     (const 'mit-lambda-list? mit-lambda-list?)
	     (flat-proc 'spar-opt spar-opt)
	     (proc 'spar-or spar-or)
	     (proc 'spar-push spar-push)
	     (proc 'spar-push-elt spar-push-elt)
	     (proc 'spar-push-elt-if spar-push-elt-if)
	     (proc 'spar-push-value spar-push-value)
	     (const 'r4rs-lambda-list? r4rs-lambda-list?)
	     (const 'spar-arg:senv spar-arg:senv)
	     (flat-proc 'spar-seq spar-seq)
	     (const 'symbol? symbol?)
	     (const 'spar-arg:value spar-arg:value)))

(define-deferred pattern->spar
  (make-pattern-compiler #f 'pattern->spar))

(define-deferred pattern->spar-expr
  (make-pattern-compiler #t 'pattern->spar-expr))