#| -*-Scheme-*-

$Id: xml-struct.scm,v 1.62 2008/09/24 00:26:39 cph Exp $

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

;;;; XML data structures

(declare (usual-integrations))

(define-syntax define-xml-type
  (sc-macro-transformer
   (lambda (form environment)
     (if (and (pair? (cdr form))
	      (identifier? (cadr form))
	      (list-of-type? (cddr form)
		(lambda (slot)
		  (or (syntax-match? '(IDENTIFIER EXPRESSION) slot)
		      (syntax-match? '(IDENTIFIER 'CANONICALIZE EXPRESSION)
				     slot)))))
	 (let ((root (symbol-append 'XML- (cadr form)))
	       (slots (cddr form)))
	   (let ((rtd (symbol-append '< root '>))
		 (%constructor (symbol-append '%MAKE- root))
		 (constructor (symbol-append 'MAKE- root))
		 (predicate (symbol-append root '?))
		 (error:not (symbol-append 'ERROR:NOT- root))
		 (slot-vars
		  (map (lambda (slot)
			 (close-syntax (car slot) environment))
		       slots)))
	     (let ((canonicalize
		    (lambda (slot var caller)
		      (if (eq? (cadr slot) 'CANONICALIZE)
			  `(,(close-syntax (caddr slot) environment) ,var)
			  `(BEGIN
			     (IF (NOT (,(close-syntax (cadr slot) environment)
				       ,var))
				 (ERROR:WRONG-TYPE-ARGUMENT
				  ,var
				  ,(symbol->string (car slot))
				  ',caller))
			     ,var)))))
	       `(BEGIN
		  (DEFINE ,rtd
		    (MAKE-RECORD-TYPE ',root '(,@(map car slots))))
		  (DEFINE ,predicate
		    (RECORD-PREDICATE ,rtd))
		  (DEFINE (,(symbol-append 'GUARANTEE- root) OBJECT CALLER)
		    (IF (NOT ,predicate)
			(,error:not OBJECT CALLER)))
		  (DEFINE (,error:not OBJECT CALLER)
		    (ERROR:WRONG-TYPE-ARGUMENT
		     OBJECT
		     ,(string-append "an XML "
				     (string-replace (symbol-name (cadr form))
						     #\-
						     #\space))
		     CALLER))
		  (DEFINE ,%constructor
		    (RECORD-CONSTRUCTOR ,rtd '(,@(map car slots))))
		  (DEFINE (,constructor ,@slot-vars)
		    (,%constructor
		     ,@(map (lambda (slot var)
			      (canonicalize slot var constructor))
			    slots
			    slot-vars)))
		  ,@(map (lambda (slot var)
			   (let* ((accessor (symbol-append root '- (car slot)))
				  (modifier (symbol-append 'SET- accessor '!)))
			     `(BEGIN
				(DEFINE ,accessor
				  (RECORD-ACCESSOR ,rtd ',(car slot)))
				(DEFINE ,modifier
				  (LET ((MODIFIER
					 (RECORD-MODIFIER ,rtd ',(car slot))))
				    (NAMED-LAMBDA (,modifier OBJECT ,var)
				      (MODIFIER OBJECT
						,(canonicalize slot
							       var
							       modifier))))))))
			 slots
			 slot-vars)))))
	 (ill-formed-syntax form)))))

(define-xml-type document
  (declaration (lambda (object) (or (not object) (xml-declaration? object))))
  (misc-1 xml-misc-content?)
  (dtd (lambda (object) (or (not object) (xml-dtd? object))))
  (misc-2 xml-misc-content?)
  (root xml-element?)
  (misc-3 xml-misc-content?))

(define (xml-misc-content? object)
  (list-of-type? object xml-misc-content-item?))

(define (xml-misc-content-item? object)
  (or (xml-comment? object)
      (xml-whitespace-string? object)
      (xml-processing-instructions? object)))

(define (xml-whitespace-string? object)
  (string-composed-of? object char-set:xml-whitespace))

(define-xml-type declaration
  (version xml-version?)
  (encoding xml-encoding?)
  (standalone (lambda (object) (member object '(#f "yes" "no")))))

(define (xml-version? object)
  (and (string-composed-of? object char-set:xml-version)
       (fix:> (string-length object) 0)))

(define char-set:xml-version
  (char-set-union char-set:alphanumeric
		  (string->char-set "_.:-")))

(define (xml-encoding? object)
  (or (not object)
      (and (string? object)
	   (let ((end (string-length object)))
	     (and (fix:> end 0)
		  (char-alphabetic? (string-ref object 0))
		  (substring-composed-of? object 1 end
					  char-set:xml-encoding))))))

(define char-set:xml-encoding
  (char-set-union char-set:alphanumeric
		  (string->char-set "_.-")))

(define-xml-type attribute
  (name xml-name?)
  (value canonicalize canonicalize-char-data))

(define (xml-char-data? object)
  (or (wide-char? object)
      (and (or (wide-string? object)
               (and (string? object)
                    (utf8-string-valid? object)))
           (string-of-xml-chars? object))))

(define (string-of-xml-chars? string)
  (for-all-chars-in-string? (alphabet-predicate alphabet:xml-char)
                            string
                            0
                            (string-length string)
                            'UTF-8))

(define (canonicalize-char-data object)
  (cond ((wide-char? object)
	 (call-with-utf8-output-string
	   (lambda (port)
	     (write-char object port))))
	((wide-string? object)
	 (wide-string->utf8-string object))
	((string? object)
	 (cond ((not (utf8-string-valid? object))
                (error:wrong-type-datum object "valid UTF-8 XML char data"))
               ((not (string-of-xml-chars? object))
                (error:wrong-type-datum object "well-formed XML char data"))
               (else object)))
	((uri? object)
	 (uri->string object))
	(else (error:wrong-type-datum object "an XML char data"))))

(define-xml-type element
  (name xml-name?)
  (attributes xml-attribute-list?)
  (content canonicalize canonicalize-content))

(define (xml-attribute-list? object)
  (and (list-of-type? object xml-attribute?)
       (let loop ((attrs object))
	 (if (pair? attrs)
	     (and (not (there-exists? (cdr attrs)
			 (let ((name (xml-attribute-name (car attrs))))
			   (lambda (attr)
			     (xml-name=? (xml-attribute-name attr) name)))))
		  (loop (cdr attrs)))
	     #t))))

(define (xml-content? object)
  (list-of-type? object xml-content-item?))

(define (xml-content-item? object)
  (or (xml-char-data? object)
      (xml-comment? object)
      (xml-element? object)
      (xml-processing-instructions? object)))

(define (canonicalize-content content)
  (letrec
      ((search
	(lambda (items)
	  (if (pair? items)
	      (let ((item (car items))
		    (items (cdr items)))
		(if (xml-char-data? item)
		    (join (canonicalize-char-data item) items)
		    (begin
		      (check-item item)
		      (cons item (search items)))))
	      '())))
       (join
	(lambda (s items)
	  (if (pair? items)
	      (let ((item (car items))
		    (items (cdr items)))
		(if (xml-char-data? item)
		    (join (string-append s (canonicalize-char-data item))
			  items)
		    (begin
		      (check-item item)
		      (cons* s item (search items)))))
	      (list s))))
       (check-item
	(lambda (item)
	  (if (not (or (xml-comment? item)
		       (xml-element? item)
		       (xml-processing-instructions? item)))
	      (error:wrong-type-datum content "an XML content")))))
    (search content)))

(define-xml-type comment
  (text canonicalize canonicalize-char-data))

(define-xml-type processing-instructions
  (name
   (lambda (object)
     (and (xml-qname? object)
	  (not (xml-name=? object 'xml)))))
  (text canonicalize canonicalize-char-data))

(define-xml-type dtd
  (root xml-qname?)
  (external (lambda (object)
	      (or (not object)
		  (xml-external-id? object))))
  (internal (lambda (object)
	      (list-of-type? object
		(lambda (object)
		  (or (xml-comment? object)
		      (xml-!element? object)
		      (xml-!attlist? object)
		      (xml-!entity? object)
		      (xml-unparsed-!entity? object)
		      (xml-parameter-!entity? object)
		      (xml-!notation? object)
		      (xml-parameter-entity-ref? object)))))))

(define-xml-type external-id
  (id (lambda (object)
	(or (not object)
	    (public-id? object))))
  (uri canonicalize
       (lambda (object)
	 (and object
	      (->uri (canonicalize-char-data object))))))

(define (public-id? object)
  (string-composed-of? object char-set:xml-public-id))

(define char-set:xml-public-id
  (char-set-union char-set:alphanumeric
		  (string->char-set " \r\n-'()+,./:=?;!*#@$_%")))

(define-xml-type !element
  (name xml-qname?)
  (content-type
   (lambda (object)
     (or (eq? object '|EMPTY|)
	 (eq? object '|ANY|)
	 (and (pair? object)
	      (eq? '|#PCDATA| (car object))
	      (list-of-type? (cdr object) xml-qname?))
	 (letrec
	     ((children?
	       (lambda (object)
		 (maybe-wrapped object
		   (lambda (object)
		     (and (pair? object)
			  (or (eq? 'alt (car object))
			      (eq? 'seq (car object)))
			  (list-of-type? (cdr object) cp?))))))
	      (cp?
	       (lambda (object)
		 (or (maybe-wrapped object xml-qname?)
		     (children? object))))
	      (maybe-wrapped
	       (lambda (object pred)
		 (or (pred object)
		     (and (pair? object)
			  (or (eq? #\? (car object))
			      (eq? #\* (car object))
			      (eq? #\+ (car object)))
			  (pair? (cdr object))
			  (pred (cadr object))
			  (null? (cddr object)))))))
	   (children? object))))))

(define-xml-type !attlist
  (name xml-qname?)
  (definitions canonicalize
    (lambda (object)
      (if (not (list-of-type? object
		 (lambda (item)
		   (and (pair? item)
			(xml-qname? (car item))
			(pair? (cdr item))
			(!attlist-type? (cadr item))
			(pair? (cddr item))
			(!attlist-default? (caddr item))
			(null? (cdddr item))))))
	  (error:wrong-type-datum object "an XML !ATTLIST definition"))
      (map (lambda (item)
	     (let ((d (caddr item)))
	       (if (pair? d)
		   (list (car item)
			 (cadr item)
			 (cons (car d) (canonicalize-char-data (cdr d))))
		   item)))
	   object))))

(define (!attlist-type? object)
  (or (eq? object '|CDATA|)
      (eq? object '|IDREFS|)
      (eq? object '|IDREF|)
      (eq? object '|ID|)
      (eq? object '|ENTITY|)
      (eq? object '|ENTITIES|)
      (eq? object '|NMTOKENS|)
      (eq? object '|NMTOKEN|)
      (and (pair? object)
	   (or (and (eq? (car object) '|NOTATION|)
		    (list-of-type? (cdr object) xml-qname?))
	       (and (eq? (car object) 'enumerated)
		    (list-of-type? (cdr object) xml-nmtoken?))))))

(define (!attlist-default? object)
  (or (eq? object '|#REQUIRED|)
      (eq? object '|#IMPLIED|)
      (and (pair? object)
	   (or (eq? (car object) '|#FIXED|)
	       (eq? (car object) 'default))
	   (xml-char-data? (cdr object)))))

(define-xml-type !entity
  (name xml-qname?)
  (value canonicalize canonicalize-entity-value))

(define-xml-type unparsed-!entity
  (name xml-qname?)
  (id xml-external-id?)
  (notation xml-qname?))

(define-xml-type parameter-!entity
  (name xml-qname?)
  (value canonicalize canonicalize-entity-value))

(define (canonicalize-entity-value object)
  (if (xml-external-id? object)
      object
      (begin
	(if (not (and (pair? object)
		      (list-of-type? object
			(lambda (object)
			  (or (xml-char-data? object)
			      (xml-entity-ref? object)
			      (xml-parameter-entity-ref? object))))))
	    (error:wrong-type-datum object "an XML !ENTITY value"))
	(canonicalize-content object))))

(define-xml-type !notation
  (name xml-qname?)
  (id xml-external-id?))

(define-xml-type entity-ref
  (name xml-qname?))

(define-xml-type parameter-entity-ref
  (name xml-qname?))

(define-syntax define-xml-printer
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER EXPRESSION) (cdr form))
	 (let ((name (cadr form))
	       (accessor (caddr form)))
	   (let ((root (symbol-append 'XML- name)))
	     `(SET-RECORD-TYPE-UNPARSER-METHOD!
	       ,(close-syntax (symbol-append '< root '>) environment)
	       (STANDARD-UNPARSER-METHOD ',root
		 (LAMBDA (,name PORT)
		   (WRITE-CHAR #\SPACE PORT)
		   (WRITE (,(close-syntax accessor environment) ,name)
			  PORT))))))
	 (ill-formed-syntax form)))))

(define-xml-printer processing-instructions xml-processing-instructions-name)
(define-xml-printer dtd xml-dtd-root)
(define-xml-printer !element xml-!element-name)
(define-xml-printer !attlist xml-!attlist-name)
(define-xml-printer !entity xml-!entity-name)
(define-xml-printer unparsed-!entity xml-unparsed-!entity-name)
(define-xml-printer parameter-!entity xml-parameter-!entity-name)
(define-xml-printer !notation xml-!notation-name)

(define-xml-printer element
  (lambda (elt)
    (xml-name-qname (xml-element-name elt))))

(define-xml-printer external-id
  (lambda (dtd)
    (or (xml-external-id-id dtd)
	(xml-external-id-uri dtd))))

(define (xml-attribute-namespace-decl? attr)
  (let ((name (xml-attribute-name attr)))
    (or (xml-name=? name 'xmlns)
	(xml-name-prefix=? name 'xmlns))))

(define (xml-element-namespace-decls elt)
  (filter xml-attribute-namespace-decl?
	  (xml-element-attributes elt)))

(define (xml-element-namespace-uri elt prefix)
  (let ((value
	 (find-xml-attr (if (null-xml-name-prefix? prefix)
			    'xmlns
			    (symbol-append 'xmlns: prefix))
			elt)))
    (and value
	 (begin
	   (string->uri value)		;signals error if not URI
	   value))))

(define (xml-element-namespace-prefix elt uri-string)
  (let ((attr
	 (find (lambda (attr)
		 (and (xml-attribute-namespace-decl? attr)
		      (string=? (xml-attribute-value attr) uri-string)))
	       (xml-element-attributes elt))))
    (and attr
	 (let ((name (xml-attribute-name attr)))
	   (if (xml-name=? name 'xmlns)
	       (null-xml-name-prefix)
	       (xml-name-local name))))))

;;;; Convenience procedures

(define (xml-comment . strings)
  (make-xml-comment
   (let* ((s (apply string-append (map canonicalize-char-data strings)))
	  (ws (utf8-string->wide-string s))
	  (n (wide-string-length ws)))
     (if (fix:> n 0)
	 (string-append
	  (if (char-whitespace? (wide-string-ref ws 0)) "" " ")
	  s
	  (if (char-whitespace? (wide-string-ref ws (fix:- n 1))) "" " "))
	 " "))))

(define (xml-stylesheet . items)
  (make-xml-processing-instructions
   'xml-stylesheet
   (call-with-utf8-output-string
     (lambda (port)
       (for-each (lambda (attr)
		   (write-char #\space port)
		   (write-string (xml-name-string (xml-attribute-name attr))
				 port)
		   (write-char #\= port)
		   (write-char #\" port)
		   (write-string (xml-attribute-value attr) port)
		   (write-char #\" port))
		 (apply xml-attrs items))))))

(define (standard-xml-element-constructor qname uri empty?)
  (let ((name (make-xml-name qname uri)))
    (if empty?
	(lambda items
	  (make-xml-element name (apply xml-attrs items) '()))
	(lambda (attrs . items)
	  (make-xml-element name
			    (if (not attrs) '() attrs)
			    (flatten-xml-element-content items))))))

(define (standard-xml-element-predicate qname uri)
  (let ((name (make-xml-name qname uri)))
    (lambda (object)
      (and (xml-element? object)
	   (xml-name=? (xml-element-name object) name)))))

(define (xml-attrs . items)
  (let ((flush
	 (lambda (name attrs)
	   (remove! (lambda (attr)
		      (eq? (xml-attribute-name attr) name))
		    attrs))))
    (let ((accum
	   (lambda (attr attrs)
	     (cons attr (flush (xml-attribute-name attr) attrs)))))
      (let loop ((items items))
	(if (pair? items)
	    (let ((item (car items))
		  (items (cdr items)))
	      (cond ((and (or (xml-name? item)
			      (string? item))
			  (pair? items))
		     (let ((name
			    (if (string? item)
				(make-xml-name item)
				item))
			   (value (car items))
			   (attrs (loop (cdr items))))
		       (if value
			   (accum (make-xml-attribute
				   name
				   (if (eq? value #t)
				       (xml-name-string name)
				       (convert-xml-string-value value)))
				  attrs)
			   (flush name attrs))))
		    ((xml-attribute? item)
		     (accum item (loop items)))
		    ((list-of-type? item xml-attribute?)
		     (do ((attrs item (cdr attrs))
			  (attrs* (loop items) (accum (car attrs) attrs*)))
			 ((not (pair? attrs)) attrs*)))
		    (else
		     (error "Unknown item passed to xml-attrs:" item))))
	    '())))))

(define (find-xml-attr name elt)
  (let ((name
	 (if (string? name)
	     (make-xml-name name)
	     (begin
	       (guarantee-xml-name name 'FIND-XML-ATTR)
	       name))))
    (let loop
	((attrs
	  (if (xml-element? elt)
	      (xml-element-attributes elt)
	      (begin
		(guarantee-list-of-type elt xml-attribute? "XML attributes"
					'FIND-XML-ATTR)
		elt))))
      (and (pair? attrs)
	   (if (xml-name=? (xml-attribute-name (car attrs)) name)
	       (xml-attribute-value (car attrs))
	       (loop (cdr attrs)))))))

(define (flatten-xml-element-content item)
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
	((uri? value) (uri->string value))
	((list-of-type? value xml-nmtoken?) (nmtokens->string value))
	(else (error:wrong-type-datum value "XML string value"))))

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