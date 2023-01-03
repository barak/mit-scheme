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

;;;; Test of macro expander

(declare (usual-integrations))

(define (expand-expr expr)
  (unsyntax (syntax expr test-environment)))

(define test-environment
  (the-environment))

(define-test 'local-define-syntax/syntax
  (lambda ()
    (assert-matches
     (expand-expr '(let ()
		     (define-syntax test
		       (syntax-rules () ((test) (lambda (y) y))))
		     (list ((test) 1) ((test) 2))))
     '(let ()
	(list (let ((?y1 1)) ?y1)
	      (let ((?y2 2)) ?y2))))))

(define-test 'local-define-syntax/eval
  (lambda ()
    (assert-equal
     (eval '(let ()
              (define-syntax test
                (syntax-rules () ((test) (lambda (y) y))))
              (list ((test) 1) ((test) 2)))
           test-environment)
     '(1 2))))

(define-test 'bug-55090
  (lambda ()
    (assert-matches
     (unsyntax
      (cadr
       (scode-sequence-actions
        (syntax '(begin
                   (define-syntax vector-edit-code
                     (syntax-rules ()
                       ((_ v r o s)
                        (let ((index (vector-length v)))
                          (subvector-move-left! v o index r (+ o s))
                          r))
                       ((_ v r o s i e)
                        (let ((index i))
                          (subvector-move-left! v o index r (+ o s))
                          (vector-set! r (+ s index) e)
                          (let ((skew (1+ s)))
                            (vector-edit-code v r index skew))))))
                   (let ((input (vector 0 1 3)))
                     (let ((array (make-vector 4)))
                       (vector-edit-code input array 0 0 2 2))))
                test-environment))))
     '(let ((input (vector 0 1 3)))
        (let ((array (make-vector 4)))
          (let ((?index1 2))
            (subvector-move-left! input 0 ?index1 array (+ 0 0))
            (vector-set! array (+ 0 ?index1) 2)
            (let ((?skew (1+ 0)))
              (let ((?index2 (vector-length input)))
                (subvector-move-left! input ?index1 ?index2
                                      array (+ ?index1 ?skew))
                array))))))))

(define-test 'quoted-macro-name
  (lambda ()
    (assert-equal
     (expand-expr '(let ()
		     (define-syntax foo
		       (er-macro-transformer
			(lambda (f r c)
			  `(,(r 'quote) foo))))
		     (foo)))
     '(let () 'foo))))

(define-test 'ellipsis-ellipsis
  (lambda ()
    (assert-equal
     (expand-expr '(let ()
		     (define-syntax flatten
		       (syntax-rules ()
			 ((flatten f (a ...) ...)
			  (f a ... ...))))
		     (flatten list (0 1) (2 3) (4))))
     '(let () (list 0 1 2 3 4)))))

(define-test 'bug-57785
  (lambda ()
    (assert-matches
     (expand-expr '(lambda ()

		     (define-syntax bar
                       (sc-macro-transformer
			(lambda (exp env)
			  `(let ((,(cadr exp)
				  ,(close-syntax (cadr exp) env)))
			     (list ,(close-syntax (cadr exp) env)
				   'x)))))

		     (define-syntax bat
                       (syntax-rules ()
			 ((_ body ...)
			  ((lambda (md) (bar md)) 'quux))))

		     (bat x)))
     '(lambda ()
	(let ((?x1 'quux))
	  (let ((?x2 ?x1))
	    (list ?x1 'x)))))))

(define-test 'bug-57793
  (lambda ()
    (assert-equal
     (expand-expr '(lambda ()

		     (define-syntax foo
		       (syntax-rules ()
			 ((_ (x y z))
			  (letrec-syntax
			      ((bar (syntax-rules (q)
				      ((_ q w)
				       '()))))
			    (bar y z)))))

		     (foo (x1 q z1))))
     '(lambda ()
	'()))))

(define-test 'bug-57833
  (lambda ()
    (assert-equal
     (expand-expr '(lambda ()
		     (define-syntax foo
		       (syntax-rules ()
			 ((_ xy)
			  (letrec-syntax
			      ((bar1 (syntax-rules ()
				       ((_ (else* destination))
					(destination))))
			       (bar2 (syntax-rules ()
				       ((_ z)
					(bar1 z)))))
			    (bar2 xy)))))
		     (foo (else* start))))
     '(lambda ()
	(start)))))

(define-test 'bug-63438
  (lambda ()
    (assert-matches
     (expand-expr '(let ()
		     (define-syntax foo
		       (syntax-rules ()
			 ((foo 0)
			  (foo 1 x))
			 ((foo 1 y)
			  (lambda (x y)
			    (list (list x y)
				  (lambda (y) (list x y)))))))
		     (foo 0)))
     '(let ()
	(lambda (?x1 ?x2)
	  (list (list ?x1 ?x2) (lambda (?x3) (list ?x1 ?x3))))))
    (assert-matches
     (expand-expr '(let ((.x.1-0 123))
		     (define-syntax foo
                       (syntax-rules ()
			 ((foo y) (lambda (x) y))))
		     ((foo .x.1-0) 456)))
     '(let ((.x.1-0 123))
	(let ((?x1 456))
	  .x.1-0)))))

(define-test 'let-values
  (lambda ()
    (assert-equal
     (expand-expr '(let-values () unspecific))
     '(let () unspecific))
    (assert-equal
     (expand-expr '(let-values (((a) foo)) unspecific))
     '(let ((a foo)) unspecific))
    (assert-equal
     (expand-expr '(let-values (((a) foo)
				((b) bar))
		     unspecific))
     '(let ((a foo)
	    (b bar))
	unspecific))
    (assert-equal
     (expand-expr '(let-values ((() foo)) unspecific))
     '(call-with-values (lambda () foo)
	(lambda () unspecific)))
    (assert-equal
     (expand-expr '(let-values (((a b) foo)) unspecific))
     '(call-with-values (lambda () foo)
	(lambda (a b) unspecific)))
    (assert-matches
     (expand-expr '(let-values (((a) foo) ((b c) bar)) unspecific))
     '(let ((?x1 foo)
	    (?x2 (lambda () bar)))
	(call-with-values ?x2
	  (lambda (b c)
	    (let ((a ?x1))
	      unspecific)))))
    (assert-matches
     (expand-expr '(let-values (((a b) foo) ((c d) bar)) unspecific))
     '(let ((?x1 (lambda () foo))
	    (?x2 (lambda () bar)))
	(call-with-values ?x2
	  (lambda (c d)
	    (call-with-values ?x1
	      (lambda (a b)
		unspecific))))))))

;;;; Tests of syntax-rules, from Larceny:

(define-test 'be-like-begin
  (lambda ()

    (define-syntax be-like-begin
      (syntax-rules ()
	((be-like-begin name)
	 (define-syntax name
	   (syntax-rules ()
	     ((name expr (... ...))
	      (begin expr (... ...))))))))

    (be-like-begin sequence)

    (assert-equal (sequence 1 2 3 4)
		  4)))

(define-test 'be-like-begin-alt
  (lambda ()

    (define-syntax be-like-begin-alt
      (syntax-rules &etc ()
	((be-like-begin-alt name)
	 (define-syntax name
	   (syntax-rules ()
	     ((name expr (&etc ...))
	      (begin expr (&etc ...))))))))

    (be-like-begin-alt sequence)

    (assert-equal (sequence 1 2 3 4) 4)))

(define-test 'shadow-=>
  (lambda ()
    (assert-equal (let ((=> #f))
		    (cond (#t => 'ok)))
		  'ok)))

;; The next two macros were contributed by Alex Shinn, who reported them as bugs
;; in Larceny v0.98.

(define-test 'underscore-as-literal
  (lambda ()

    (define-syntax underscore-as-literal
      (syntax-rules (_)
	((underscore-as-literal _) 'under)
	((underscore-as-literal x) 'other)))

    (assert-eqv (underscore-as-literal _) 'under)
    (assert-eqv (underscore-as-literal 5) 'other)))

;; Fails: assertion 1: value was under but expected an object eqv? to other
(define-test 'ellipses-as-literal
  (lambda ()

    (define-syntax ellipses-as-literal
      (syntax-rules (...)
	((ellipses-as-literal ...) 'under)
	((ellipses-as-literal x) 'other)))

    (assert-eqv (ellipses-as-literal ...) 'under)
    (assert-eqv (ellipses-as-literal 6) 'other)))

(define-test 'override-ellipsis
  (lambda ()

    (assert-equal (let ((... 19))
		    (declare (ignore ...))
		    (define-syntax bar
		      (syntax-rules ()
			((bar x y ...)
			 (list y x ...))))
		    (bar 1 2 3))
		  '(2 1 3))

    (assert-equal (let ((... 19))
		    (define-syntax bar
		      (syntax-rules ()
			((bar x y)
			 (list y x ...))))
		    (bar 1 2))
		  '(2 1 19))))

(define-test 'strip-syntactic-closures
  (lambda ()
    (let ((x (eval (read-from-string "'(a #1=(b . #1#))") test-environment)))
      (assert-list x)
      (assert-= (length x) 2)
      (assert-eq (car x) 'a)
      (assert-pair (cadr x))
      (assert-eq (car (cadr x)) 'b)
      (assert-eq (cdr (cadr x)) (cadr x)))
    (let ((x
	   (eval (read-from-string "'(a #1=(b . #1#) #2=(c . #1#) #2#)")
		 test-environment)))
      (assert-list x)
      (assert-= (length x) 4)
      (assert-eq (car x) 'a)
      (assert-pair (cadr x))
      (assert-eq (car (cadr x)) 'b)
      (assert-eq (cdr (cadr x)) (cadr x))
      (assert-pair (caddr x))
      (assert-eq (car (caddr x)) 'c)
      (assert-eq (cdr (caddr x)) (cadr x))
      (assert-eq (cadddr x) (caddr x)))))

(define-test 'dotted-list
  (lambda ()

    ;; Dotted-list pattern without ellipsis acts like rest parameter of lambda:
    (define-syntax foo
      (syntax-rules ()
	((_ a b . c)
	 (quote (a b c)))))
    (assert-equal (foo 1 2 3 4) '(1 2 (3 4)))

    ;; Dotted-list pattern with ellipsis matches final cdr of input:
    (define-syntax bar
      (syntax-rules ()
	((_ a b ... . c)
	 (quote (a (b ...) c)))))
    (assert-equal (bar 1 2 3 4) '(1 (2 3 4) ()))
    (assert-equal (bar 1 2 3 . 4) '(1 (2 3) 4))))

(define-test 'bug-63503
  (lambda ()
    (define-syntax foo
      (syntax-rules (keyword)
	((foo keyword x) x)))
    (define-syntax bar
      (syntax-rules ()
	((bar x)
	 (foo keyword x))))
    (assert-equal (bar 123) 123)))

(define-test 'bug-63568
  (lambda ()
    (assert-equal
     (expand-expr '(lambda ()
		     (define-syntax define-foo
		       (syntax-rules ()
			 ((define-foo ((variable value ...)))
			  (begin
			    (add-foo! '(variable value))
			    ...))))
		     (define-foo ((a 0 1 2)))))
     '(lambda ()
	(add-foo! '(a 0))
	(add-foo! '(a 1))
	(add-foo! '(a 2))))))

;; See discussion in bug #63568 after it was closed.
(define-test 'extra-ellipses
  (lambda ()
    (assert-equal
     (expand-expr '(lambda ()
		     (define-syntax foo
                       (syntax-rules ()
			 ((foo (x (y z ...) ...))
			  (begin
			    (add-foo! '(x y z))
			    ... ...))))
		     (foo (x (y1 z1a z1b) (y2 z2) (y3) (y4 z4a z4b z4c z4d)))))
     '(lambda ()
	(add-foo! '(x y1 z1a))
	(add-foo! '(x y1 z1b))
	(add-foo! '(x y2 z2))
	(add-foo! '(x y4 z4a))
	(add-foo! '(x y4 z4b))
	(add-foo! '(x y4 z4c))
	(add-foo! '(x y4 z4d))))))