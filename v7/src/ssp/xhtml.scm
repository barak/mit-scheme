#| -*-Scheme-*-

$Id: xhtml.scm,v 1.1 2003/12/29 05:24:55 uid67408 Exp $

Copyright 2002,2003 Massachusetts Institute of Technology

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; XHTML support

(declare (usual-integrations))

(define xhtml-external-dtd
  (make-xml-external-id "-//W3C//DTD XHTML 1.0 Strict//EN"
			"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))

(define xhtml-dtd
  (make-xml-dtd 'html xhtml-external-dtd '()))

(define xhtml-iri
  (make-xml-namespace-iri "http://www.w3.org/1999/xhtml"))

(define-syntax define-standard-element
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER) (cdr form))
	 (let ((name (cadr form)))
	   `(DEFINE ,name
	      (STANDARD-ELEMENT-CONSTRUCTOR ',name XHTML-IRI)))
	 (ill-formed-syntax form)))))

(define (standard-element-constructor simple iri)
  (let ((name (make-xml-name simple iri)))
    (lambda (attrs . items)
      (make-xml-element name
			(if (not attrs)
			    '()
			    attrs)
			(flatten-xml-element-contents items)))))

(define (flatten-xml-element-contents item)
  (letrec
      ((scan-item
	(lambda (item tail)
	  (cond ((xml-content-item? item) (cons item tail))
		((pair? item) (scan-list item tail))
		((or (not item) (null? item)) tail)
		(else (cons (convert-xhtml-string-value item) tail)))))
       (scan-list
	(lambda (items tail)
	  (if (pair? items)
	      (scan-item (car items)
			 (scan-list (cdr items) tail))
	      (begin
		(if (not (null? items))
		    (error:wrong-type-datum items "list"))
		tail)))))
    (scan-item item '())))

(define (convert-xhtml-string-value value)
  (cond ((symbol? value) (symbol-name value))
	((number? value) (number->string value))
	((xml-namespace-iri? value) (xml-namespace-iri-string value))
	(else (error:wrong-type-datum value "string value"))))

(define-standard-element a)
(define-standard-element abbr)
(define-standard-element acronym)
(define-standard-element address)
(define-standard-element b)
(define-standard-element big)
(define-standard-element blockquote)
(define-standard-element body)
(define-standard-element button)
(define-standard-element caption)
(define-standard-element cite)
(define-standard-element code)
(define-standard-element col)
(define-standard-element colgroup)
(define-standard-element dd)
(define-standard-element defn)
(define-standard-element del)
(define-standard-element dir)
(define-standard-element div)
(define-standard-element dl)
(define-standard-element dt)
(define-standard-element em)
(define-standard-element form)
(define-standard-element h1)
(define-standard-element h2)
(define-standard-element h3)
(define-standard-element h4)
(define-standard-element h5)
(define-standard-element head)
(define-standard-element html)
(define-standard-element i)
(define-standard-element ins)
(define-standard-element kbd)
(define-standard-element li)
(define-standard-element listing)
(define-standard-element menu)
(define-standard-element ol)
(define-standard-element optgroup)
(define-standard-element option)
(define-standard-element p)
(define-standard-element pre)
(define-standard-element q)
(define-standard-element s)
(define-standard-element samp)
(define-standard-element script)
(define-standard-element select)
(define-standard-element small)
(define-standard-element span)
(define-standard-element strike)
(define-standard-element strong)
(define-standard-element sub)
(define-standard-element sup)
(define-standard-element table)
(define-standard-element tbody)
(define-standard-element td)
(define-standard-element textarea)
(define-standard-element tfoot)
(define-standard-element th)
(define-standard-element thead)
(define-standard-element title)
(define-standard-element tr)
(define-standard-element tt)
(define-standard-element u)
(define-standard-element ul)
(define-standard-element var)

(define-syntax define-empty-element
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER) (cdr form))
	 (let ((name (cadr form)))
	   `(DEFINE ,name
	      (EMPTY-ELEMENT-CONSTRUCTOR ',name XHTML-IRI)))
	 (ill-formed-syntax form)))))

(define (empty-element-constructor simple iri)
  (let ((name (make-xml-name simple iri)))
    (lambda keyword-list
      (make-xml-element name
			(if (and (pair? keyword-list)
				 (list-of-type? (car keyword-list)
				   xml-attribute?)
				 (null? (cdr keyword-list)))
			    (car keyword-list)
			    (apply attributes keyword-list))
			'()))))

(define-empty-element br)
(define-empty-element hr)
(define-empty-element img)
(define-empty-element input)
(define-empty-element link)
(define-empty-element meta)

(define (attributes . keyword-list)
  (let loop ((bindings keyword-list))
    (if (and (pair? bindings)
	     (xml-name? (car bindings))
	     (pair? (cdr bindings)))
	(let ((value (cadr bindings))
	      (tail (loop (cddr bindings))))
	  (if value
	      (cons (make-xml-attribute
		     (car bindings)
		     (cond ((eq? value #t) (symbol-name (car bindings)))
			   ((xml-char-data? value) value)
			   (else (convert-xhtml-string-value value))))
		    tail)
	      tail))
	(begin
	  (if (not (null? bindings))
	      (error:wrong-type-argument keyword-list
					 "keyword list"
					 'ATTRIBUTES))
	  '()))))

(define (href iri . contents)
  (apply a
	 (attributes 'href iri)
	 contents))

(define (id-def tag . contents)
  (apply a
	 (attributes 'id tag
		     'name tag)
	 contents))

(define (id-ref tag . contents)
  (apply href (string-append "#" tag) contents))

(define (rel-link rel iri)
  (link 'rel rel
	'href iri))

(define (style-link iri)
  (link 'rel "stylesheet"
	'href iri
	'type "text/css"))

(define (http-equiv name value)
  (meta 'http-equiv name
	'content value))

(define (style . keyword-list)
  (let loop ((bindings keyword-list))
    (if (and (pair? bindings)
	     (symbol? (car bindings))
	     (pair? (cdr bindings))
	     (string? (cadr bindings)))
	(string-append (symbol-name (car bindings))
		       ": "
		       (cadr bindings)
		       (if (pair? (cddr bindings))
			   (string-append "; " (loop (cddr bindings)))
			   ";"))
	(begin
	  (if (not (null? bindings))
	      (error:wrong-type-argument keyword-list "keyword list" 'STYLE))
	  ""))))

(define (comment . strings)
  (make-xml-comment (string-append " " (apply string-append strings) " ")))