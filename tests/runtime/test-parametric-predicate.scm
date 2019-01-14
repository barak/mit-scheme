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

;;;; Tests for parametric predicates

(declare (usual-integrations))

(define-test 'parametric-predicate-one-parameter
  (lambda ()
    (let ((pattern '((? base))))
      (let* ((template
	      (make-predicate-template 'template pattern make-flat-test))
             (constructor (predicate-template-constructor template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list number?))
              (params2 (list boolean?)))
          (let ((tn (apply constructor params1))
                (tb (apply constructor params2)))
            (test-predicate-operations tn '(template number))
            (test-predicate-operations tb '(template boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)))))))

(define-test 'parametric-predicate-two-parameters
  (lambda ()
    (let ((pattern '((?* domains -) (? base))))
      (let* ((template
	      (make-predicate-template 'template pattern
				       make-function-like-test))
             (constructor (predicate-template-constructor template)))
        (test-template-operations template 'template pattern)

        (let ((params1 (list (list number? number?) number?))
              (params2 (list (list boolean? boolean?) boolean?)))
          (let ((tn (apply constructor params1))
                (tb (apply constructor params2)))
            (test-predicate-operations tn '(template (number number) number))
            (test-predicate-operations tb '(template (boolean boolean) boolean))
            (test-parametric-predicate-operations tn template params1)
            (test-parametric-predicate-operations tb template params2)))))))

(define-test 'covariant-ordering
  (lambda ()
    (let* ((template
	    (make-predicate-template 'template '((? a)) make-flat-test))
           (constructor (predicate-template-constructor template)))
      (let ((p1 (constructor (disjoin string? symbol?)))
            (p2 (constructor string?))
            (p3 (constructor symbol?)))

        (assert-true (predicate<= p1 p1))
        (assert-false (predicate<= p1 p2))
        (assert-false (predicate<= p1 p3))

        (assert-true (predicate<= p2 p1))
        (assert-true (predicate<= p2 p2))
        (assert-false (predicate<= p2 p3))

        (assert-true (predicate<= p3 p1))
        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p3 p3))

        ))))

(define-test 'contravariant-ordering
  (lambda ()
    (let* ((template
	    (make-predicate-template 'template '((? a -)) make-flat-test))
           (constructor (predicate-template-constructor template)))
      (let ((p1 (constructor (disjoin string? symbol?)))
            (p2 (constructor string?))
            (p3 (constructor symbol?)))

        (assert-true (predicate<= p1 p1))
        (assert-true (predicate<= p1 p2))
        (assert-true (predicate<= p1 p3))

        (assert-false (predicate<= p2 p1))
        (assert-true (predicate<= p2 p2))
        (assert-false (predicate<= p2 p3))

        (assert-false (predicate<= p3 p1))
        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p3 p3))

        ))))

(define-test 'mixed-ordering
  (lambda ()
    (let* ((template
	    (make-predicate-template 'template '((? a -) (? b)) make-flat-test))
           (constructor (predicate-template-constructor template)))
      (let ((p1 (constructor (disjoin string? symbol?)
			     (disjoin string? symbol?)))
            (p2 (constructor string? string?))
            (p3 (constructor string? (disjoin string? symbol?)))
            (p4 (constructor (disjoin string? symbol?) string?)))

        (for-each (lambda (predicate)
                    (assert-true (predicate<= predicate predicate)))
                  (list p1 p2 p3 p4))

        (assert-false (predicate<= p2 p1))
        (assert-false (predicate<= p3 p1))
        (assert-true (predicate<= p4 p1))

        (assert-false (predicate<= p3 p2))
        (assert-true (predicate<= p4 p2))

        (assert-true (predicate<= p2 p3))
        (assert-false (predicate<= p2 p4))

        ))))

(define-test 'template-patterns
  (lambda ()
    (let ((operators '(? ?* ?+))
          (names '(a b c))
          (polarities '(+ = -)))

      (assert-false (template-pattern? '())
		    'expression '(template-pattern? '()))
      (assert-false (template-pattern-element? '())
		    'expression '(template-pattern-element? '()))
      (assert-false (template-pattern? '(()))
		    'expression '(template-pattern? '(())))
      (for-each (lambda (symbol)
                  (assert-false (template-pattern? symbol)
				'expression `
				(template-pattern? ,symbol))
                  (assert-false (template-pattern-element? symbol)
				'expression
				`(template-pattern-element? ,symbol))
                  (assert-false (template-pattern? (list symbol))
				'expression
				`(template-pattern? ,(list symbol)))
                  (assert-false (template-pattern-element? (list symbol))
				'expression
				`(template-pattern-element? ,(list symbol)))
                  (assert-false (template-pattern? (list (list symbol)))
				'expression
				`(template-pattern? ,(list (list symbol)))))
                (append operators names polarities))

      (let ((elements (elementwise-lists-of (list operators names polarities))))
        (for-each
         (lambda (element)
           (assert-true (template-pattern? (list element))
			'expression
			`(template-pattern? ,(list element)))
           (assert-false (template-pattern? element)
			 'expression
			 `(template-pattern? ,element))
           (for-each
            (lambda (permutation)
              (let ((assertion
                     (if (equal? permutation element)
                         assert-true
                         assert-false)))
                (assertion (template-pattern-element? permutation)
			   'expression
			   `(template-pattern-element? ,permutation))
                (assertion (template-pattern? (list permutation))
			   'expression
			   `(template-pattern? ,(list permutation)))
                (assertion (template-pattern-element? (take permutation 2))
			   'expression
			   `(template-pattern-element? ,(take permutation 2)))
                (assertion (template-pattern? (list (take permutation 2)))
			   'expression
			   `(template-pattern? ,(list (take permutation 2))))))
            (all-permutations-of element)))
         elements)

        (for-each
         (lambda (elements)
           ((if (= (length elements)
                   (length (delete-duplicates (map cadr elements) eqv?)))
                assert-true
                assert-false)
            (template-pattern? elements)
	    'expression
	    `(template-pattern? ,elements)))
         (append
          (elementwise-lists-of (list elements elements))
          (elementwise-lists-of (list elements elements elements))))))))

(define-test 'match-template-pattern
  (lambda ()
    (assert-type-error (lambda () (match-numbers '((? a)) 1)))
    (assert-equal (match-numbers '((? a)) '(1))
                  '((a + 1)))
    (assert-equal (match-numbers '((? a -) (? b)) '(1 2))
                  '((a - 1)
                    (b + 2)))
    (assert-equal (match-numbers '((?* a) (? b -)) '((1 2 3) 2))
                  '((a + (1 2 3))
                    (b - 2)))
    (assert-equal (match-numbers '((?+ a -) (? b)) '((1 2 3) 2))
                  '((a - (1 2 3))
                    (b + 2)))
    (assert-equal (match-numbers '((?* a) (? b -)) '(() 2))
                  '((a + ())
                    (b - 2)))
    (assert-simple-error (lambda () (match-numbers '((?+ a -) (? b)) '(() 2))))
    (assert-simple-error (lambda () (match-numbers '((?* a) (? b -)) '(1 2))))
    (assert-simple-error (lambda () (match-numbers '((?+ a -) (? b)) '(1 2))))))

(define (make-flat-test . predicates)
  (lambda (object)
    (and (list? object)
	 (= (length predicates) (length object))
	 (every (lambda (predicate object)
		  (predicate object))
		predicates
		objects))))

(define (make-function-like-test domains codomain)
  (lambda (object)
    (and (pair? object)
	 (codomain (car object))
	 (list? (cdr object))
	 (= (length domains) (length (cdr object)))
	 (every (lambda (domain object)
		  (domain object))
		domains
		objects))))

(define (test-template-operations template name pattern)
  (assert-true (predicate-template? template)
	       'expression `(predicate-template? ,template))
  (assert-false (predicate? template)
	       'expression `(predicate? ,template))
  (assert-eqv (predicate-template-name template) name)
  (assert-equal (predicate-template-pattern template) pattern)
  (assert-lset= eq?
                (predicate-template-parameter-names template)
                (map template-pattern-element-name pattern))
  (let ((predicate (predicate-template-predicate template)))
    (assert-true (predicate? predicate)
		 'expression `(predicate? ,predicate))
    (assert-true (predicate<= predicate parametric-predicate?)
		 'expression `(predicate<= ,predicate ,parametric-predicate?))
    (assert-false (predicate<= parametric-predicate? predicate)
		  'expression
		  `(predicate<= ,parametric-predicate? ,predicate))))

(define (test-predicate-operations predicate name)
  (assert-true (predicate? predicate))
  (let ((tag (predicate->dispatch-tag predicate)))
    (assert-true (dispatch-tag? tag)
		 'expression `(dispatch-tag? ,tag))
    (assert-eqv (dispatch-tag->predicate tag) predicate)
    (assert-equal (predicate-name predicate) name)
    (assert-equal (dispatch-tag-name tag) name)))

(define (test-parametric-predicate-operations predicate template parameters)
  (assert-true (parametric-predicate? predicate)
	       'expression `(parametric-predicate? ,predicate))
  (assert-eqv (parametric-predicate-template predicate) template)
  (assert-lset= eq?
                (parametric-predicate-names predicate)
                (predicate-template-parameter-names template))
  (assert-lset= equal?
                (map (lambda (name)
                       ((predicate-template-accessor name template) predicate))
                     (predicate-template-parameter-names template))
                parameters))

(define (parametric-predicate-names predicate)
  (predicate-template-parameter-names
   (parametric-predicate-template predicate)))

(define (match-numbers pattern values)
  (parameter-bindings->alist
   (match-template-pattern pattern values number? 'match-numbers)))

(define (parameter-bindings->alist bindings)
  (map (lambda (binding)
         (list (parameter-binding-name binding)
               (parameter-binding-polarity binding)
               (parameter-binding-value binding)))
       bindings))

(define (all-permutations-of items)
  (let loop ((items items))
    (if (pair? items)
        (append-map (lambda (index)
                      (map (let ((head (list-ref items index)))
                             (lambda (tail)
                               (cons head tail)))
                           (loop (delete-item items index))))
                    (iota (length items)))
        '(()))))

(define (delete-item items index)
  (append (take items index)
          (cdr (drop items index))))

(define (elementwise-lists-of lists)
  (let loop ((lists lists))
    (if (pair? lists)
        (append-map (let ((tails (loop (cdr lists))))
                      (lambda (head)
                        (map (lambda (tail)
                               (cons head tail))
                             tails)))
                    (car lists))
        '(()))))