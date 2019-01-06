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

;;;; Predicates: parametric
;;; package: (runtime parametric-predicate)

(declare (usual-integrations))

(define parametric-tag-metatag (make-dispatch-metatag 'parametric-tag))
(define parametric-tag? (dispatch-tag->predicate parametric-tag-metatag))

(define %make-parametric-tag
  (dispatch-metatag-constructor parametric-tag-metatag 'make-parametric-tag))

(define (make-parametric-tag name predicate template bindings)
  (%make-parametric-tag name predicate template bindings))

(define-integrable (parametric-tag-template tag)
  (dispatch-tag-extra-ref tag 0))

(define-integrable (parametric-tag-bindings tag)
  (dispatch-tag-extra-ref tag 1))

(define (parametric-predicate? object)
  (and (predicate? object)
       (parametric-tag? (predicate->dispatch-tag object))))

(define (parametric-predicate-template predicate)
  (parametric-tag-template (predicate->dispatch-tag predicate)))

(define (parametric-predicate-bindings predicate)
  (parametric-tag-bindings (predicate->dispatch-tag predicate)))

;;;; Templates

(define (make-predicate-template name pattern make-data-test)
  (guarantee template-pattern? pattern 'make-predicate-template)
  (letrec*
      ((instantiator
        (make-instantiator name pattern make-data-test (lambda () template)))
       (template
        (%make-predicate-template name
				  pattern
				  (all-args-memoizer equal?
						     (lambda patterned-tags
						       patterned-tags)
						     instantiator)
				  (lambda (object)
				    (and (parametric-predicate? object)
					 (eqv? template
					       (parametric-predicate-template
						  object)))))))
    (register-predicate! (predicate-template-predicate template)
			 (symbol name '-predicate)
                         '<= parametric-predicate?)
    template))

(define-record-type <predicate-template>
    (%make-predicate-template name pattern instantiator predicate)
    predicate-template?
  (name predicate-template-name)
  (pattern predicate-template-pattern)
  (instantiator template-instantiator)
  (predicate predicate-template-predicate))

(define (make-instantiator name pattern make-data-test get-template)
  (lambda (patterned-tags caller)
    (letrec ((tag
	      (make-parametric-tag
	       (cons name
		     (map-template-pattern pattern
					   patterned-tags
					   dispatch-tag-name
					   caller))
	       (apply make-data-test
		      (map-template-pattern pattern
					    patterned-tags
					    dispatch-tag->predicate
					    caller))
	       (get-template)
	       (match-template-pattern pattern
				       patterned-tags
				       dispatch-tag?
				       caller))))
      tag)))

(define (predicate-template-constructor template #!optional caller)
  (let ((instantiator (template-instantiator template))
        (pattern (predicate-template-pattern template)))
    (lambda patterned-predicates
      (dispatch-tag->predicate
       (instantiator (map-template-pattern pattern
					   patterned-predicates
					   predicate->dispatch-tag
					   caller)
		     caller)))))

(define (predicate-template-parameter-names template)
  (template-pattern->names (predicate-template-pattern template)))

(define (predicate-template-accessor name template #!optional caller)
  (let ((elt
         (find (lambda (elt)
                 (eq? name (template-pattern-element-name elt)))
               (predicate-template-pattern template))))
    (if (not elt)
        (error:bad-range-argument name 'predicate-template-accessor))
    (let ((valid? (predicate-template-predicate template))
          (convert
           (if (template-pattern-element-single-valued? elt)
               dispatch-tag->predicate
	       (lambda (tags) (map dispatch-tag->predicate tags)))))
      (lambda (predicate)
	(guarantee valid? predicate caller)
        (convert
         (parameter-binding-value
          (find (lambda (binding)
                  (eqv? name (parameter-binding-name binding)))
                (parametric-tag-bindings
		 (predicate->dispatch-tag predicate)))))))))

;;;; Template patterns

(define (template-pattern? object)
  (and (non-empty-list? object)
       (every template-pattern-element? object)
       (list-of-unique-symbols? (template-pattern->names object))))

(define (template-pattern-element? object)
  (and (pair? object)
       (template-pattern-operator? (car object))
       (pair? (cdr object))
       (template-pattern-name? (cadr object))
       (or (null? (cddr object))
           (and (pair? (cddr object))
                (polarity? (caddr object))
                (null? (cdddr object))))))

(define (template-pattern-operator? object)
  (memq object '(? ?* ?+)))

(define (template-pattern-name? object)
  (and (symbol? object)
       (not (template-pattern-operator? object))
       (not (polarity? object))))

(define (polarity? object)
  (memq object '(+ = -)))

(define (template-pattern-element-operator element)
  (car element))

(define (template-pattern-element-name element)
  (cadr element))

(define (template-pattern-element-polarity element)
  (if (null? (cddr element))
      '+
      (caddr element)))

(define (template-pattern-element-single-valued? element)
  (eq? '? (template-pattern-element-operator element)))

(define (template-pattern->names pattern)
  (map template-pattern-element-name pattern))

(define (match-template-pattern pattern values value-predicate caller)
  (guarantee list? values caller)
  (if (not (= (length values) (length pattern)))
      (error:bad-range-argument values caller))
  (map (lambda (element value)
         (case (template-pattern-element-operator element)
           ((?)
            (if (not (value-predicate value))
                (error "Mismatch:" element value)))
           ((?*)
            (if (not (and (list? value)
                          (every value-predicate value)))
                (error "Mismatch:" element value)))
           ((?+)
            (if (not (and (pair? value)
                          (list? (cdr value))
                          (every value-predicate value)))
                (error "Mismatch:" element value)))
           (else (error:not-a template-pattern? pattern caller)))
         (make-parameter-binding element value))
       pattern
       values))

(define (map-template-pattern pattern object value-procedure caller)
  (map (lambda (element o)
         (case (template-pattern-element-operator element)
           ((?) (value-procedure o))
           ((?* ?+) (map value-procedure o))
           (else (error:not-a template-pattern? pattern caller))))
       pattern
       object))

(define-record-type <parameter-binding>
    (make-parameter-binding element value)
    parameter-binding?
  (element parameter-binding-element)
  (value parameter-binding-value))

(define (parameter-binding-name binding)
  (template-pattern-element-name
   (parameter-binding-element binding)))

(define (parameter-binding-polarity binding)
  (template-pattern-element-polarity
   (parameter-binding-element binding)))

(define (parameter-binding-values binding)
  (if (template-pattern-element-single-valued?
       (parameter-binding-element binding))
      (list (parameter-binding-value binding))
      (parameter-binding-value binding)))

(add-boot-init!
 (lambda ()
   (register-predicate! parametric-predicate? 'parametric-predicate
			'<= predicate?)
   (register-predicate! template-pattern? 'template-pattern
			'<= non-empty-list?)))

(add-boot-init!
 (lambda ()
   (define-dispatch-tag<= parametric-tag? parametric-tag?
     (lambda (tag1 tag2)
       (and (eqv? (parametric-tag-template tag1)
		  (parametric-tag-template tag2))
	    (every (lambda (bind1 bind2)
		     (let ((tags1 (parameter-binding-values bind1))
			   (tags2 (parameter-binding-values bind2)))
		       (and (= (length tags1) (length tags2))
			    (every (case (parameter-binding-polarity
					  bind1)
				     ((+) dispatch-tag<=)
				     ((-) dispatch-tag>=)
				     (else dispatch-tag=))
				   tags1
				   tags2))))
		   (parametric-tag-bindings tag1)
		   (parametric-tag-bindings tag2)))))))

(define is-list-of)
(define is-non-empty-list-of)
(define is-pair-of)
(add-boot-init!
 (lambda ()
   (set! is-list-of
	 (predicate-template-constructor
	  (make-predicate-template 'is-list-of
				   '((? elt-predicate))
				   (lambda (elt-predicate)
				     (lambda (object)
				       (list-of-type? object elt-predicate))))))
   (set! is-non-empty-list-of
	 (predicate-template-constructor
	  (make-predicate-template 'is-non-empty-list-of
				   '((? elt-predicate))
				   (lambda (elt-predicate)
				     (lambda (object)
				       (and (pair? object)
					    (list-of-type? object
							   elt-predicate)))))))
   (set! is-pair-of
	 (predicate-template-constructor
	  (make-predicate-template 'is-non-empty-list-of
				   '((? car-predicate) (? cdr-predicate))
				   (lambda (car-predicate cdr-predicate)
				     (lambda (object)
				       (and (pair? object)
					    (car-predicate (car object))
					    (cdr-predicate (cdr object))))))))
   unspecific))