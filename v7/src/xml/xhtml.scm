#| -*-Scheme-*-

$Id: xhtml.scm,v 1.3 2004/07/15 18:25:07 cph Exp $

Copyright 2002,2003,2004 Massachusetts Institute of Technology

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

(define html-external-dtd
  (make-xml-external-id "-//W3C//DTD XHTML 1.0 Strict//EN"
			"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd"))

(define html-dtd
  (make-xml-dtd 'html html-external-dtd '()))

(define html-iri
  (make-xml-namespace-iri "http://www.w3.org/1999/xhtml"))

(define (html-element? object)
  (and (xml-element? object)
       (xml-name-iri=? (xml-element-name object) html-iri)))

(define-syntax define-standard-element
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(IDENTIFIER) (cdr form))
	 (let ((name (cadr form)))
	   `(BEGIN
	      (DEFINE ,(symbol-append 'HTML: name)
		(STANDARD-ELEMENT-CONSTRUCTOR ',name HTML-IRI))
	      (DEFINE ,(symbol-append 'HTML: name '?)
		(STANDARD-ELEMENT-PREDICATE ',name HTML-IRI))))
	 (ill-formed-syntax form)))))

(define (standard-element-constructor simple iri)
  (let ((name (make-xml-name simple iri)))
    (lambda (attrs . items)
      (make-xml-element name
			(if (not attrs)
			    '()
			    attrs)
			(flatten-xml-element-contents items)))))

(define (standard-element-predicate simple iri)
  (let ((name (make-xml-name simple iri)))
    (lambda (object)
      (and (xml-element? object)
	   (xml-name=? (xml-element-name object) name)))))

(define (flatten-xml-element-contents item)
  (letrec
      ((scan-item
	(lambda (item tail)
	  (cond ((xml-content-item? item) (cons item tail))
		((pair? item) (scan-list item tail))
		((or (not item) (null? item)) tail)
		(else (cons (convert-html-string-value item) tail)))))
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

(define (convert-html-string-value value)
  (cond ((xml-content-item? value) value)
	((symbol? value) (symbol-name value))
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
	   `(BEGIN
	      (DEFINE ,(symbol-append 'HTML: name)
		(EMPTY-ELEMENT-CONSTRUCTOR ',name HTML-IRI))
	      (DEFINE ,(symbol-append 'HTML: name '?)
		(STANDARD-ELEMENT-PREDICATE ',name HTML-IRI))))
	 (ill-formed-syntax form)))))

(define (empty-element-constructor simple iri)
  (let ((name (make-xml-name simple iri)))
    (lambda items
      (make-xml-element name (apply html-attrs items) '()))))

(define-empty-element br)
(define-empty-element hr)
(define-empty-element img)
(define-empty-element input)
(define-empty-element link)
(define-empty-element meta)

(define (html-attrs . items)
  (let loop ((items items))
    (if (pair? items)
	(let ((item (car items))
	      (items (cdr items)))
	  (cond ((and (xml-name? item)
		      (pair? items))
		 (let ((value (car items))
		       (attrs (loop (cdr items))))
		   (if value
		       (cons (make-xml-attribute
			      item
			      (if (eq? value #t)
				  (symbol-name item)
				  (convert-html-string-value value)))
			     attrs)
		       attrs)))
		((xml-attribute? item)
		 (cons item (loop items)))
		((list-of-type? item xml-attribute?)
		 (append item (loop items)))
		(else
		 (error "Unknown item passed to html-attrs:" item))))
	'())))

(define (html:href iri . contents)
  (apply html:a
	 (html-attrs 'href iri)
	 contents))

(define (html:id-def tag . contents)
  (apply html:a
	 (html-attrs 'id tag
		     'name tag)
	 contents))

(define (html:id-ref tag . contents)
  (apply html:href (string-append "#" tag) contents))

(define (html:rel-link rel iri)
  (html:link 'rel rel
	     'href iri))

(define (html:style-link iri)
  (html:link 'rel "stylesheet"
	     'href iri
	     'type "text/css"))

(define (html:http-equiv name value)
  (html:meta 'http-equiv name
	     'content value))

(define (html:style . keyword-list)
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

(define (html:comment . strings)
  (make-xml-comment (string-append " " (apply string-append strings) " ")))