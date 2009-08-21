#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; XHTML support

(declare (usual-integrations))

(define html-uri-string "http://www.w3.org/1999/xhtml")
(define html-uri (->absolute-uri html-uri-string))

(define (html-element? object)
  (and (xml-element? object)
       (xml-name-uri=? (xml-element-name object) html-uri)))

(define (guarantee-html-element object caller)
  (if (not (html-element? object))
      (error:not-html-element object caller)))

(define (error:not-html-element object caller)
  (error:wrong-type-argument object "HTML element" caller))

(define (html-element-name? object)
  (and (xml-name? object)
       (xml-name-uri=? object html-uri)))

(define (guarantee-html-element-name object caller)
  (if (not (html-element-name? object))
      (error:not-html-element-name object caller)))

(define (error:not-html-element-name object caller)
  (error:wrong-type-argument object "HTML element name" caller))

(define-syntax define-html-id
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(DATUM DATUM DATUM) (cdr form))
	 (let ((version (cadr form))
	       (public-id (caddr form))
	       (system-id (cadddr form)))
	   (let ((pid-name (symbol 'HTML- version '-PUBLIC-ID))
		 (sid-name (symbol 'HTML- version '-SYSTEM-ID))
		 (eid-name (symbol 'HTML- version '-EXTERNAL-ID))
		 (dtd-name (symbol 'HTML- version '-DTD)))
	     `(BEGIN
		(DEFINE ,pid-name ,public-id)
		(DEFINE ,sid-name ,system-id)
		(DEFINE ,eid-name (MAKE-XML-EXTERNAL-ID ,pid-name ,sid-name))
		(DEFINE ,dtd-name (MAKE-XML-DTD 'html ,eid-name '())))))
	 (ill-formed-syntax form)))))

(define-html-id "1.0"
  "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/MarkUp/DTD/xhtml1-strict.dtd")

(define-html-id "1.1"
  "-//W3C//DTD XHTML 1.1//EN"
  "http://www.w3.org/MarkUp/DTD/xhtml11.dtd")

(define (html-public-id? id)
  (and (string? id)
       (string-prefix? "-//W3C//DTD XHTML " id)))

(define (html-external-id? object)
  (and (xml-external-id? object)
       (html-public-id? (xml-external-id-id object))))

(define (html-dtd? object)
  (and (xml-dtd? object)
       (eq? (xml-dtd-root object) 'html)
       (html-external-id? (xml-dtd-external object))
       (null? (xml-dtd-internal object))))

(define (html-1.0-document attrs . items)
  (%make-document html-1.0-dtd attrs items))

(define (html-1.1-document attrs . items)
  (%make-document html-1.1-dtd attrs items))

(define (%make-document dtd attrs items)
  (receive (decl items) (parse-decl items)
    (receive (styles items) (parse-styles items)
      (make-xml-document decl
			 '("\n")
			 dtd
			 (cons "\n"
			       (append-map! (lambda (style)
					      (list style "\n"))
					    styles))
			 (html:html (xml-attrs 'xmlns html-uri
					       attrs)
				    items)
			 '("\n")))))

(define (parse-decl items)
  (if (and (pair? items)
	   (xml-declaration? (car items)))
      (values (car items) (cdr items))
      (values (make-xml-declaration "1.0" "UTF-8" #f) items)))

(define (parse-styles items)
  (let loop ((items items) (styles '()))
    (if (and (pair? items)
	     (xml-processing-instructions? (car items))
	     (eq? (xml-processing-instructions-name (car items))
		  'xml-stylesheet))
	(loop (cdr items) (cons (car items) styles))
	(values (reverse! styles) items))))

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
		(STANDARD-XML-ELEMENT-CONSTRUCTOR ',name HTML-URI ,empty?))
	      (DEFINE ,(symbol-append 'HTML: name '?)
		(STANDARD-XML-ELEMENT-PREDICATE ',name HTML-URI))
	      (DEFINE-HTML-ELEMENT-CONTEXT ',name ',context)))
	 (ill-formed-syntax form)))))

(define (define-html-element-context qname context)
  (hash-table/put! element-context-map
		   (make-xml-name qname html-uri)
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

(define (html:href uri . contents)
  (apply html:a
	 (xml-attrs 'href uri)
	 contents))

(define (html:id-def tag . contents)
  (apply html:a
	 (xml-attrs 'id tag
		    'name tag)
	 contents))

(define (html:id-ref tag . contents)
  (apply html:href (string-append "#" tag) contents))

(define (html:rel-link rel uri)
  (html:link 'rel rel
	     'href uri))

(define (html:style-link uri)
  (html:link 'rel "stylesheet"
	     'href uri
	     'type "text/css"))

(define (html:http-equiv name value)
  (html:meta 'http-equiv name
	     'content value))

(define (html:style-attr . keyword-list)
  (guarantee-keyword-list keyword-list 'HTML:STYLE-ATTR)
  (if (pair? keyword-list)
      (let loop ((bindings keyword-list))
	(string-append (symbol-name (car bindings))
		       ": "
		       (cadr bindings)
		       (if (pair? (cddr bindings))
			   (string-append "; " (loop (cddr bindings)))
			   ";")))
      ""))