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

;;;; Tests of syntax-rules

(declare (usual-integrations))

(define param-options
  (keyword-option-parser
   `((ellipsis ,symbol? ,(lambda () '...))
     (literals ,(is-list-of symbol?) ,(lambda () '()))
     (underscore ,symbol? ,(lambda () '_)))))

(define (subst from to in)
  (let loop ((in in))
    (if (equal? from in)
	to
	(cond ((pair? in)
	       (let scan ((elts in))
		 (if (pair? elts)
		     (cons (loop (car elts))
			   (scan (cdr elts)))
		     (loop elts))))
	      ((vector? in)
	       (vector-map loop in))
	      (else in)))))

(define (subst-segment ellipsis in)
  (let loop ((in in))
    (case (car in)
      ((list dotted-list vector)
       (cons (car in)
	     (append-map (lambda (elt)
			   (if (eq? '* (car elt))
			       (list (loop (cadr elt))
				     (list 'literal ellipsis))
			       (list (loop elt))))
			 (cdr in))))
      (else in))))

(define rewriter-test-cases:simple-lists
  '((()
     (list))
    ((a)
     (list (var a)))
    ((a b)
     (list (var a) (var b)))
    ((a ... b)
     (list (* (var a)) (var b)))
    ((a ... b ...)
     (list (* (var a)) (* (var b))))
    ((a b ...)
     (list (var a) (* (var b))))
    ((a (_ ...) c)
     (list (var a) (list (* (anon-var))) (var c)))
    ((a (... ...) c)
     (list (var a) (literal ...) (var c)))
    ((a (b (... ...)) c)
     (list (var a) (list (var b) (literal ...)) (var c)))))

(define rewriter-test-cases:simple-vectors
  (map (lambda (tc)
	 (cons* (list->vector (car tc))
		(cons 'vector (cdadr tc))
		(cddr tc)))
       rewriter-test-cases:simple-lists))

(define rewriter-test-cases:dotted-lists
  '(((a b ... . 3)
     (dotted-list (var a) (* (var b)) (literal 3)))
    ((a b ... . c)
     (dotted-list (var a) (* (var b)) (var c)))
    ((a (b ...) . c)
     (dotted-list (var a) (list (* (var b))) (var c)))
    ((a (3 ...) . c)
     (dotted-list (var a) (list (* (literal 3))) (var c)))
    ((_ (3 ...) . c)
     (dotted-list (anon-var) (list (* (literal 3))) (var c)))
    ((a (_ ...) . c)
     (dotted-list (var a) (list (* (anon-var))) (var c)))))

(define rewriter-test-cases:change-ellipsis
  (map (lambda (tc)
	 (list (subst '... '::: (car tc))
	       (subst '(literal ...) '(literal :::) (cadr tc))
	       'ellipsis ':::))
       (append rewriter-test-cases:simple-lists
	       rewriter-test-cases:simple-vectors
	       rewriter-test-cases:dotted-lists)))

(define rewriter-test-cases:change-literals
  (append-map (lambda (tc)
		(list (list (car tc)
			    (subst '(var a) '(literal a) (cadr tc))
			    'literals '(a))
		      (list (car tc)
			    (subst '(anon-var) '(literal _) (cadr tc))
			    'literals '(_))
		      (list (car tc)
			    (subst-segment '...
					   (subst '(literal ...)
						  '(list (literal ...)
							 (literal ...))
						  (cadr tc)))
			    'literals '(...))))
	      (append rewriter-test-cases:simple-lists
		      rewriter-test-cases:simple-vectors
		      rewriter-test-cases:dotted-lists)))

(define rewriter-test-cases:complex-ellipsis-quoting
  (let ((tcs
	 '(((a (b (... (x y ...))) c)
	    (list (var a)
		  (list (var b) (list (var x) (var y) (literal ...)))
		  (var c)))
	   ((a (b (... (x ... y))) c)
	    (list (var a)
		  (list (var b) (list (var x) (literal ...) (var y)))
		  (var c))))))
    (append tcs
	    (map (lambda (tc)
		   (cons* (list->vector (car tc))
			  (cons 'vector (cdadr tc))
			  (cddr tc)))
		 tcs))))

(define rewriter-test-cases:complex-ellipsis-quoting-2
  '(((a (b (... (x y ...))) c)
     (list (var a)
	   (list (var b)
		 (list (literal ...) (list (var x) (var y) (literal ...))))
	   (var c))
     literals (...))
    ((a (b (... (x ... y))) c)
     (list (var a)
	   (list (var b)
		 (list (literal ...) (list (var x) (literal ...) (var y))))
	   (var c))
     literals (...))))

(define rewriter-test-cases
  (append rewriter-test-cases:simple-lists
	  rewriter-test-cases:simple-vectors
	  rewriter-test-cases:dotted-lists
	  rewriter-test-cases:change-ellipsis
	  rewriter-test-cases:change-literals
	  rewriter-test-cases:complex-ellipsis-quoting
	  rewriter-test-cases:complex-ellipsis-quoting-2))

(define-test 'rewriter
  (map (lambda (test-case)
	 (apply
	  (lambda (input expected . options)
	    (let-values
		(((ellipsis literals underscore)
		  (param-options options (default-object))))
	      (let ((rewriter (make-rewriter ellipsis literals underscore eq?)))
		(lambda ()
		  (assert-equal (rewriter input)
				expected)))))
	  test-case))
       rewriter-test-cases))

(define (clause-parser-test test-case)
  (apply
   (lambda (input expected . options)
     (let-values
	 (((ellipsis literals underscore)
	   (param-options options (default-object))))
       (lambda ()
	 (assert-equal (parse-clauses ellipsis literals input underscore eq?)
		       expected))))
   test-case))

(define clause-parser-test-cases
  '(((((foo) ()))
     (((list) (list))))
    ((((test)
       (lambda (y) y)))
     (((list)
       (list (var lambda) (list (var y)) (var y)))))
    ((((_ v r o s)
       (let ((index (vector-length v)))
         (subvector-move-left! v o index r (+ o s))
         r))
      ((_ v r o s i e)
       (let ((index i))
         (subvector-move-left! v o index r (+ o s))
         (vector-set! r (+ s index) e)
         (let ((skew (1+ s)))
           (vector-edit-code v r index skew)))))
     (((list (var v) (var r) (var o) (var s))
       (list
	(var let)
	(list (list (var index)
		    (list (var vector-length) (var v))))
	(list (var subvector-move-left!)
              (var v) (var o) (var index) (var r)
              (list (var +) (var o) (var s)))
	(var r)))
      ((list (var v) (var r) (var o) (var s) (var i) (var e))
       (list
	(var let)
	(list (list (var index) (var i)))
	(list (var subvector-move-left!)
              (var v) (var o) (var index) (var r)
              (list (var +) (var o) (var s)))
	(list (var vector-set!)
	      (var r)
	      (list (var +) (var s) (var index))
	      (var e))
	(list (var let)
	      (list (list (var skew) (list (var |1+|) (var s))))
	      (list (var vector-edit-code)
		    (var v) (var r) (var index) (var skew)))))))
    ((((_ (x y z))
       (letrec-syntax
	   ((bar (syntax-rules (q)
		   ((_ q w)
		    '()))))
	 (bar y z))))
     (((list (list (var x) (var y) (var z)))
       (list (var letrec-syntax)
	     (list (list (var bar)
			 (list (var syntax-rules)
			       (list (var q))
			       (list (list (anon-var) (var q) (var w))
				     (list (var quote) (list))))))
	     (list (var bar) (var y) (var z))))))
    ((((flatten f (a ...) ...)
       (f a ... ...)))
     (((list (var f) (* (list (* (var a)))))
       (list (var f) (* (* (var a)))))))))

(define-test 'clause-parser
  (map clause-parser-test
       clause-parser-test-cases))