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

(define (spar->classifier spar)
  (lambda (form senv hist)
    (spar (%new-input form hist)
	  senv
	  (%new-output)
	  (lambda (input senv output failure)
	    (declare (ignore senv failure))
	    (if (not (%input-null? input))
		(error "Rule failed to match entire form."))
	    (output 'get-only))
	  (lambda ()
	    (serror form senv hist "Ill-formed syntax:" form)))))

;;;; Inputs and outputs

(define (%new-input form hist)
  (lambda (operator)
    (case operator
      ((form) form)
      ((hist) hist)
      ((car) (%new-input (car form) (hist-car hist)))
      ((cdr) (%new-input (cdr form) (hist-cdr hist)))
      (else (error "Unknown operator:" operator)))))

(define (%null-input)
  (%new-input '() (initial-hist '())))

(define (%input-form input) (input 'form))
(define (%input-hist input) (input 'hist))
(define (%input-car input) (input 'car))
(define (%input-cdr input) (input 'cdr))

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

(define (spar-require-form predicate)
  (lambda (input senv output success failure)
    (if (predicate (%input-form input))
	(success input senv output failure)
	(failure))))

(define (spar-require-senv predicate)
  (lambda (input senv output success failure)
    (if (predicate senv)
	(success input senv output failure)
	(failure))))

(define (spar-require-full predicate)
  (lambda (input senv output success failure)
    (if (predicate (%input-form input) senv)
	(success input senv output failure)
	(failure))))

(define (spar-require-value predicate)
  (lambda (input senv output success failure)
    (if (predicate (%output-top output))
	(success input senv output failure)
	(failure))))

(define (spar-discard-form input senv output success failure)
  (declare (ignore input))
  (success (%null-input) senv output failure))

(define (spar-push-form input senv output success failure)
  (success (%null-input)
	   senv
	   (%output-push output (%input-form input))
	   failure))

(define (spar-push-hist input senv output success failure)
  (success (%null-input)
	   senv
	   (%output-push output (%input-hist input))
	   failure))

(define (spar-push-senv input senv output success failure)
  (success input
	   senv
	   (%output-push output senv)
	   failure))

(define (spar-push-datum object)
  (lambda (input senv output success failure)
    (success input
	     senv
	     (%output-push output object)
	     failure)))

(define (spar-push-thunk-value procedure)
  (lambda (input senv output success failure)
    (success input
	     senv
	     (%output-push output (procedure))
	     failure)))

(define (spar-push-mapped-form procedure)
  (lambda (input senv output success failure)
    (success (%null-input)
	     senv
	     (%output-push output (procedure (%input-form input)))
	     failure)))

(define (spar-push-mapped-full procedure)
  (lambda (input senv output success failure)
    (success (%null-input)
	     senv
	     (%output-push output (procedure (%input-form input) senv))
	     failure)))

(define (%push-classified procedure)
  (lambda (input senv output success failure)
    (success (%null-input)
	     senv
	     (%output-push output
			   (procedure (%input-form input)
				      senv
				      (%input-hist input)))
	     failure)))

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

(define (spar-alt . spars)
  (cond ((not (pair? spars)) spar-fail)
        ((not (pair? (cdr spars))) (car spars))
        (else (reduce-right %alt-combiner #f spars))))

(define (%alt-combiner s1 s2)
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

(define spar-discard-elt
  (spar-elt spar-discard-form))

(define spar-require-null
  (spar-require-form null?))

(define spar-push-elt
  (spar-elt spar-push-form))

;;;; Environment combinators

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

(define spar-push-closed-form
  (spar-push-mapped-full
   (lambda (form senv)
     (make-syntactic-closure senv '() form))))

(define spar-push-closed-elt
  (spar-elt spar-push-closed-form))

(define spar-push-partially-closed-form
  (spar-push-mapped-full
   (lambda (form senv)
     (lambda (free)
       (make-syntactic-closure senv free form)))))

(define spar-push-partially-closed-elt
  (spar-elt spar-push-partially-closed-form))

(define-deferred spar-push-classified-form
  (%push-classified classify-form))

(define-deferred spar-push-classified-elt
  (spar-elt spar-push-classified-form))

(define spar-push-deferred-classified-form
  (%push-classified
   (lambda (form senv hist)
     (lambda ()
       (classify-form form senv hist)))))

(define spar-push-deferred-classified-elt
  (spar-elt spar-push-deferred-classified-form))

(define spar-push-open-classified-form
  (%push-classified
   (lambda (form senv hist)
     (declare (ignore senv))
     (lambda (senv*)
       (classify-form form senv* hist)))))

(define spar-push-open-classified-elt
  (spar-elt spar-push-open-classified-form))

(define-deferred spar-push-id-elt
  (spar-elt (spar-require-form identifier?)
	    spar-push-form))

(define (spar-push-id-elt= id)
  (spar-elt (spar-require-full
	     (lambda (form senv)
	       (and (identifier? form)
		    (identifier=? senv form senv id))))
	    spar-push-form))

;;;; Value combinators

(define (spar-push-values . spars)
  (%with-output (lambda (output output*)
		  (%output-push output (%output-all output*)))
		spars))

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

(define spar-push-body
  (spar-encapsulate-values
      (lambda (elts)
	(lambda (frame-senv)
	  (let ((body-senv (make-internal-senv frame-senv)))
	    (map-in-order (lambda (elt) (elt body-senv))
			  elts))))
    (spar+ spar-push-open-classified-elt)
    spar-require-null))