#| -*-Scheme-*-

$Id: xml-struct.scm,v 1.35 2003/09/26 03:56:58 cph Exp $

Copyright 2001,2002,2003 Massachusetts Institute of Technology

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

;;;; XML data structures

(declare (usual-integrations))

(define-syntax define-xml-type
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER * (IDENTIFIER EXPRESSION ? EXPRESSION))
			(cdr form))
	 (let ((root (symbol-append 'XML- (cadr form)))
	       (slots (cddr form)))
	   (let ((rtd (symbol-append '< root '>))
		 (constructor (symbol-append 'MAKE- root))
		 (predicate (symbol-append root '?))
		 (error:not (symbol-append 'ERROR:NOT- root))
		 (slot-vars
		  (map (lambda (slot)
			 (close-syntax (car slot) environment))
		       slots)))
	     (let ((test
		    (lambda (slot var name)
		      `(IF (NOT (,(close-syntax (cadr slot) environment) ,var))
			   (ERROR:WRONG-TYPE-ARGUMENT
			    ,var ,(symbol->string (car slot)) ',name)))))
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
		  (DEFINE ,constructor
		    (LET ((CONSTRUCTOR
			   (RECORD-CONSTRUCTOR ,rtd '(,@(map car slots)))))
		      (NAMED-LAMBDA (,constructor ,@slot-vars)
			,@(map (lambda (slot var) (test slot var constructor))
			       slots slot-vars)
			(CONSTRUCTOR
			 ,@(map (lambda (slot var)
				  (if (pair? (cddr slot))
				      `(,(caddr slot) ,var)
				      var))
				slots
				slot-vars)))))
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
				      ,(test slot var modifier)
				      (MODIFIER OBJECT ,var)))))))
			 slots
			 slot-vars)))))
	 (ill-formed-syntax form)))))

(define-xml-type document
  (declaration (lambda (object) (or (not object) (xml-declaration? object))))
  (misc-1 misc-arg?)
  (dtd (lambda (object) (or (not object) (xml-dtd? object))))
  (misc-2 misc-arg?)
  (root xml-element?)
  (misc-3 misc-arg?))

(define (misc-arg? object)
  (list-of-type? object
    (lambda (object)
      (or (xml-comment? object)
	  (xml-whitespace-string? object)
	  (xml-processing-instructions? object)))))

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

(define-xml-type element
  (name xml-name?)
  (attributes xml-attribute-list? canonicalize-attributes)
  (contents xml-content?))

(define (xml-attribute-list? object)
  (and (list-of-type? object xml-attribute?)
       (let loop ((attributes object))
	 (if (pair? attributes)
	     (and (not (there-exists? (cdr attributes)
			 (let ((name (caar attributes)))
			   (lambda (attribute)
			     (xml-name=? (car attribute) name)))))
		  (loop (cdr attributes)))
	     #t))))

(define (xml-attribute? object)
  (and (pair? object)
       (xml-name? (car object))
       (xml-attribute-value? (cdr object))))

(define (xml-attribute-value? object)
  (and (pair? object)
       (list-of-type? object xml-attribute-value-item?)))

(define (xml-attribute-value-item? object)
  (or (xml-char-data? object)
      (xml-entity-ref? object)))

(define (xml-content? object)
  (list-of-type? object xml-content-item?))

(define (xml-content-item? object)
  (or (xml-char-data? object)
      (xml-comment? object)
      (xml-element? object)
      (xml-processing-instructions? object)
      (xml-entity-ref? object)))

(define-xml-type comment
  (text xml-char-data? canonicalize-char-data))

(define-xml-type processing-instructions
  (name
   (lambda (object)
     (and (xml-qname? object)
	  (not (string-ci=? "xml" (symbol-name object))))))
  (text xml-char-data? canonicalize-char-data))

(define (xml-char-data? object)
  (or (string? object)
      (wide-char? object)
      (wide-string? object)))

(define (canonicalize-attributes attributes)
  (map (lambda (a)
	 (cons (car a)
	       (canonicalize-attribute-value (cdr a))))
       attributes))

(define (canonicalize-attribute-value v)
  (canonicalize-content v))

(define (canonicalize-entity-value v)
  (if (xml-external-id? v)
      v
      (canonicalize-attribute-value v)))

(define (canonicalize-content content)
  (letrec
      ((search
	(lambda (items)
	  (if (pair? items)
	      (let ((item (canonicalize-char-data (car items)))
		    (items (cdr items)))
		(if (string? item)
		    (join item items)
		    (cons item (search items))))
	      '())))
       (join
	(lambda (s items)
	  (if (pair? items)
	      (let ((item (canonicalize-char-data (car items)))
		    (items (cdr items)))
		(if (string? item)
		    (join (string-append s item) items)
		    (cons* s item (search items))))
	      (list s)))))
    (search content)))

(define (canonicalize-char-data object)
  (cond ((wide-char? object)
	 (call-with-output-string
	   (lambda (port)
	     (write-utf8-char object port))))
	((wide-string? object) (wide-string->utf8-string object))
	(else object)))

(define-xml-type dtd
  (root xml-name?)
  (external
   (lambda (object)
     (or (not object)
	 (xml-external-id? object))))
  (internal
   (lambda (object)
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
  (iri (lambda (object)
	 (or (not object)
	     (xml-char-data? object)))
       canonicalize-char-data))

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
  (definitions
    (lambda (object)
      (list-of-type? object
	(lambda (item)
	  (and (pair? item)
	       (xml-qname? (car item))
	       (pair? (cdr item))
	       (!attlist-type? (cadr item))
	       (pair? (cddr item))
	       (!attlist-default? (caddr item))
	       (null? (cdddr item))))))
    (lambda (object)
      (map (lambda (item)
	     (let ((d (caddr item)))
	       (if (pair? d)
		   (list (car item)
			 (cadr item)
			 (cons (car d) (canonicalize-attribute-value (cdr d))))
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
	   (eq? '|NOTATION| (car object))
	   (list-of-type? (cdr object) xml-qname?))
      (and (pair? object)
	   (eq? 'enumerated (car object))
	   (list-of-type? (cdr object) xml-nmtoken?))))

(define (!attlist-default? object)
  (or (eq? object '|#REQUIRED|)
      (eq? object '|#IMPLIED|)
      (and (pair? object)
	   (eq? '|#FIXED| (car object))
	   (xml-attribute-value? (cdr object)))
      (and (pair? object)
	   (eq? 'default (car object))
	   (xml-attribute-value? (cdr object)))))

(define-xml-type !entity
  (name xml-qname?)
  (value entity-value? canonicalize-entity-value))

(define-xml-type unparsed-!entity
  (name xml-qname?)
  (id xml-external-id?)
  (notation xml-qname?))

(define-xml-type parameter-!entity
  (name xml-qname?)
  (value entity-value? canonicalize-entity-value))

(define (entity-value? object)
  (or (and (pair? object)
	   (list-of-type? object
	     (lambda (object)
	       (or (xml-char-data? object)
		   (xml-entity-ref? object)
		   (xml-parameter-entity-ref? object)))))
      (xml-external-id? object)))

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
	(xml-external-id-iri dtd))))

(define (xml-attribute-value attr)
  (and (pair? (cdr attr))
       (string? (cadr attr))
       (null? (cddr attr))
       (cadr attr)))

(define (guarantee-xml-attribute-value object #!optional caller)
  (let ((v (xml-attribute-value object)))
    (if (not v)
	(error:not-xml-attribute-value object
				       (if (default-object? caller)
					   #f
					   caller)))
    v))

(define (error:not-xml-attribute-value object caller)
  (error:wrong-type-argument object "simple XML attribute value" caller))

(define (xml-attribute-namespace-decl? attr)
  (or (xml-name=? (car attr) 'xmlns)
      (xml-name-prefix=? (car attr) 'xmlns)))

(define (xml-element-namespace-decls elt)
  (keep-matching-items (xml-element-attributes elt)
    xml-attribute-namespace-decl?))

(define (xml-element-namespace-iri elt prefix)
  (let ((attr
	 (find-matching-item (xml-element-attributes elt)
	   (if (null-xml-name-prefix? prefix)
	       (lambda (attr)
		 (xml-name=? (car attr) 'xmlns))
	       (lambda (attr)
		 (and (xml-name-prefix=? (car attr) 'xmlns)
		      (xml-name-local=? (car attr) prefix)))))))
    (and attr
	 (make-xml-namespace-iri (guarantee-xml-attribute-value attr)))))

(define (xml-element-namespace-prefix elt iri)
  (let ((iri (xml-namespace-iri-string iri)))
    (let ((attr
	   (find-matching-item (xml-element-attributes elt)
	     (lambda (attr)
	       (and (xml-attribute-namespace-decl? attr)
		    (string=? (guarantee-xml-attribute-value attr) iri))))))
      (and attr
	   (if (xml-name=? (car attr) 'xmlns)
	       (null-xml-name-prefix)
	       (xml-name-local (car attr)))))))