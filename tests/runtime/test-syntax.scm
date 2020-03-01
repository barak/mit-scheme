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

;;;; Test of macro expander

(declare (usual-integrations))

(define test-environment
  (the-environment))

(define-test 'local-define-syntax/syntax
  (lambda ()
    (assert-matches
     (unsyntax
      (syntax '(let ()
                 (define-syntax test
                   (syntax-rules () ((test) (lambda (y) y))))
                 (list ((test) 1) ((test) 2)))
              test-environment))
     '(let () (list (let ((?y1 1)) ?y1) (let ((?y2 2)) ?y2))))))

(define-test 'local-define-syntax/eval
  (lambda ()
    (assert-equal
     (eval '(let ()
              (define-syntax test
                (syntax-rules () ((test) (lambda (y) y))))
              (list ((test) 1) ((test) 2)))
           test-environment)
     '(1 2))))

(define-test 'bug55090
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
     (unsyntax
      (syntax '(let ()
                 (define-syntax foo
                   (er-macro-transformer
                    (lambda (f r c)
                      `(,(r 'quote) foo))))
                 (foo))
              test-environment))
     '(let () 'foo))))

(define-test 'ellipsis-ellipsis
  (lambda ()
    (expect-error
     (lambda ()
       (assert-equal
        (unsyntax
         (syntax '(let ()
                    (define-syntax flatten
                      (syntax-rules ()
                        ((flatten f (a ...) ...)
                         (f a ... ...))))
                    (flatten list (0 1) (2 3) (4)))
                 test-environment))
        '(list 0 1 2 3 4))))))

(define-test 'rename-of-compound-identifier
  (lambda ()
    (assert-equal
     (unsyntax
      (syntax '(lambda ()

		 (define-syntax bat
                   (syntax-rules ()
                     ((_ body ...)
                      ((lambda (md) (bar md)) 'quux))))

		 (define-syntax bar
                   (sc-macro-transformer
                    (lambda (exp env)
                      `(let ((,(cadr exp)
                              ,(close-syntax (cadr exp) env)))
			 (list ,(close-syntax (cadr exp) env)
                               'x)))))

		 (bat x))
              test-environment))
     '(lambda ()
	(let ((.md.1-0 'quux))
	  (let ((.md.2-1 .md.1-0))
	    (list .md.1-0 'x)))))))

(define-test 'syntax-rules-rename-of-compound-identifier
  (lambda ()
    (assert-equal
     (unsyntax
      (syntax '(lambda ()

		 (define-syntax foo
		   (syntax-rules ()
		     ((_ (x y z))
		      (letrec-syntax
			  ((bar (syntax-rules (q)
				  ((_ q w)
				   '()))))
			(bar y z)))))

		 (foo (x1 q z1)))
              test-environment))
     '(lambda ()
	'()))))

;;;; Tests of syntax-rules, from Larceny:

#|
;; Broken: "Missing ellipsis in expansion."
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
|#

#|
;; Feature not implemented
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
|#

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
