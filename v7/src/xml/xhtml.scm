#| -*-Scheme-*-

$Id: xhtml.scm,v 1.11 2004/07/24 03:19:18 cph Exp $

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

(define html-iri-string "http://www.w3.org/1999/xhtml")
(define html-iri (make-xml-namespace-iri html-iri-string))

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

(define (html-external-id? object)
  (and (xml-external-id? object)
       (let ((id (xml-external-id-id object))
	     (iri (xml-external-id-iri object)))
	 (and id
	      iri
	      (or (and (string=? id html-1.0-public-id)
		       (string=? iri html-1.0-system-id))
		  (and (string=? id html-1.1-public-id)
		       (string=? iri html-1.1-system-id)))))))

(define html-1.0-public-id "-//W3C//DTD XHTML 1.0 Strict//EN")
(define html-1.0-system-id "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")

(define html-1.0-external-id
  (make-xml-external-id html-1.0-public-id html-1.0-system-id))

(define html-1.1-public-id "-//W3C//DTD XHTML 1.1//EN")
(define html-1.1-system-id "http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd")

(define html-1.1-external-id
  (make-xml-external-id html-1.1-public-id html-1.1-system-id))

(define (html-dtd? object)
  (and (xml-dtd? object)
       (html:html? (xml-dtd-root object))
       (html-external-id? (xml-dtd-external object))
       (null? (xml-dtd-internal object))))

(define html-root-name
  (make-xml-name 'html html-iri))

(define html-1.0-dtd
  (make-xml-dtd html-root-name html-1.0-external-id '()))

(define html-1.1-dtd
  (make-xml-dtd html-root-name html-1.1-external-id '()))

(define (html-1.0-document attrs . items)
  (%make-document html-1.0-dtd attrs items))

(define (html-1.1-document attrs . items)
  (%make-document html-1.1-dtd attrs items))

(define (%make-document dtd attrs items)
  (let ((attr
	 (find-matching-item attrs
	   (lambda (attr)
	     (xml-name=? (xml-attribute-name attr) 'xmlns)))))
    (if (and attr (not (string=? (xml-attribute-value attr) html-iri-string)))
	(error "Default namespace must be HTML:" (xml-attribute-value attr)))
    (make-xml-document (make-xml-declaration "1.0" "UTF-8" #f)
		       '("\n")
		       dtd
		       '("\n")
		       (html:html (if attr
				      attrs
				      (xml-attrs 'xmlns html-iri attrs))
				  items)
		       '())))

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
		(STANDARD-XML-ELEMENT-CONSTRUCTOR ',name HTML-IRI ,empty?))
	      (DEFINE ,(symbol-append 'HTML: name '?)
		(STANDARD-XML-ELEMENT-PREDICATE ',name HTML-IRI))
	      (DEFINE-HTML-ELEMENT-CONTEXT ',name ',context)))
	 (ill-formed-syntax form)))))

(define (define-html-element-context qname context)
  (hash-table/put! element-context-map
		   (make-xml-name qname html-iri)
		   context)
  qname)

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
(define-html-element rb		ruby)
(define-html-element rbc	ruby)
(define-html-element rp		ruby)
(define-html-element rt		ruby)
(define-html-element rtc	ruby)
(define-html-element ruby	inline)
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