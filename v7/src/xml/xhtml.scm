#| -*-Scheme-*-

$Id: xhtml.scm,v 1.6 2004/07/19 04:45:11 cph Exp $

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

(define (guarantee-html-element object caller)
  (if (not (html-element? object))
      (error:wrong-type-argument object "XHTML element" caller)))

(define (html-element-name? object)
  (and (xml-name? object)
       (xml-name-iri=? object html-iri)))

(define (guarantee-html-element-name object caller)
  (if (not (html-element-name? object))
      (error:wrong-type-argument object "XHTML element name" caller)))

(define-syntax define-html-element
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL SYMBOL ? 'EMPTY) (cdr form))
	 (let ((name (cadr form))
	       (context (caddr form))
	       (empty? (pair? (cdddr form))))
	   `(BEGIN
	      (DEFINE ,(symbol-append 'HTML: name)
		(STANDARD-HTML-CONSTRUCTOR ',name ',context ,empty?))
	      (DEFINE ,(symbol-append 'HTML: name '?)
		(STANDARD-HTML-PREDICATE ',name))
	      ',name))
	 (ill-formed-syntax form)))))

(define (standard-html-constructor simple context empty?)
  (let ((name (make-xml-name simple html-iri)))
    (hash-table/put! element-context-map name context)
    (if empty?
	(lambda items
	  (make-xml-element name (apply xml-attrs items) '()))
	(lambda (attrs . items)
	  (make-xml-element name
			    (if (not attrs) '() attrs)
			    (flatten-xml-element-contents items))))))

(define (standard-html-predicate simple)
  (let ((name (make-xml-name simple html-iri)))
    (lambda (object)
      (and (xml-element? object)
	   (xml-name=? (xml-element-name object) name)))))

(define (html-element-context elt)
  (guarantee-html-element elt 'HTML-ELEMENT-CONTEXT)
  (hash-table/get element-context-map (xml-element-name elt) #f))

(define (html-element-name-context name)
  (guarantee-html-element-name name 'HTML-ELEMENT-NAME-CONTEXT)
  (hash-table/get element-context-map name #f))

(define (html-element-names)
  (hash-table/key-list element-context-map))

(define element-context-map
  (make-eq-hash-table))

(define (xml-attrs . items)
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
				  (convert-xml-string-value value)))
			     attrs)
		       attrs)))
		((xml-attribute? item)
		 (cons item (loop items)))
		((list-of-type? item xml-attribute?)
		 (append item (loop items)))
		(else
		 (error "Unknown item passed to xml-attrs:" item))))
	'())))

(define (flatten-xml-element-contents item)
  (letrec
      ((scan-item
	(lambda (item tail)
	  (cond ((pair? item) (scan-list item tail))
		((or (not item) (null? item)) tail)
		(else (cons (convert-xml-string-value item) tail)))))
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

(define (convert-xml-string-value value)
  (cond ((xml-content-item? value) value)
	((symbol? value) (symbol-name value))
	((number? value) (number->string value))
	((xml-namespace-iri? value) (xml-namespace-iri-string value))
	((list-of-type? value xml-nmtoken?) (nmtokens->string value))
	(else (error:wrong-type-datum value "string value"))))

(define (nmtokens->string nmtokens)
  (if (pair? nmtokens)
      (let ((nmtoken-length
	     (lambda (nmtoken)
	       (string-length (symbol-name nmtoken)))))
	(let ((s
	       (make-string
		(let loop ((nmtokens nmtokens) (n 0))
		  (let ((n (fix:+ n (nmtoken-length (car nmtokens)))))
		    (if (pair? (cdr nmtokens))
			(loop (cdr nmtokens) (fix:+ n 1))
			n))))))
	  (let loop ((nmtokens nmtokens) (index 0))
	    (string-move! (symbol-name (car nmtokens)) s index)
	    (if (pair? (cdr nmtokens))
		(let ((index (fix:+ index (nmtoken-length (car nmtokens)))))
		  (string-set! s index #\space)
		  (loop (cdr nmtokens) (fix:+ index 1)))))
	  s))
      (make-string 0)))

(define-html-element a		inline)
(define-html-element abbr	inline)
(define-html-element acronym	inline)
(define-html-element address	block)
(define-html-element area	map empty)
(define-html-element b		inline)
(define-html-element base	head empty)
(define-html-element bdo	inline)
(define-html-element big	inline)
(define-html-element blockquote	block)
(define-html-element body	html)
(define-html-element br		inline empty)
(define-html-element button	inline)
(define-html-element caption	table)
(define-html-element cite	inline)
(define-html-element code	inline)
(define-html-element col	table empty)
(define-html-element colgroup	table)
(define-html-element dd		dl)
(define-html-element del	hybrid)
(define-html-element dfn	inline)
(define-html-element div	block)
(define-html-element dl		block)
(define-html-element dt		dl)
(define-html-element em		inline)
(define-html-element fieldset	block)
(define-html-element form	block)
(define-html-element h1		block)
(define-html-element h2		block)
(define-html-element h3		block)
(define-html-element h4		block)
(define-html-element h5		block)
(define-html-element h6		block)
(define-html-element head	html)
(define-html-element hr		block empty)
(define-html-element html	root)
(define-html-element i		inline)
(define-html-element img	inline empty)
(define-html-element input	inline empty)
(define-html-element ins	hybrid)
(define-html-element kbd	inline)
(define-html-element label	inline)
(define-html-element legend	fieldset)
(define-html-element li		list)
(define-html-element link	head empty)
(define-html-element map	inline)
(define-html-element meta	head empty)
(define-html-element noscript	block)
(define-html-element object	inline)
(define-html-element ol		block)
(define-html-element optgroup	select)
(define-html-element option	select)
(define-html-element p		block)
(define-html-element param	object empty)
(define-html-element pre	block)
(define-html-element q		inline)
(define-html-element samp	inline)
(define-html-element script	hybrid)
(define-html-element select	inline)
(define-html-element small	inline)
(define-html-element span	inline)
(define-html-element strong	inline)
(define-html-element style	head)
(define-html-element sub	inline)
(define-html-element sup	inline)
(define-html-element table	block)
(define-html-element tbody	table)
(define-html-element td		table)
(define-html-element textarea	inline)
(define-html-element tfoot	table)
(define-html-element th		table)
(define-html-element thead	table)
(define-html-element title	head)
(define-html-element tr		table)
(define-html-element tt		inline)
(define-html-element ul		block)
(define-html-element var	inline)

(define (html:href iri . contents)
  (apply html:a
	 (xml-attrs 'href iri)
	 contents))

(define (html:id-def tag . contents)
  (apply html:a
	 (xml-attrs 'id tag
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

(define (html:style-attr . keyword-list)
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