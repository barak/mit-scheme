#| -*-Scheme-*-

$Id: xml-struct.scm,v 1.15 2003/07/13 03:45:04 cph Exp $

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

(define (xml-intern name)
  (if (not (and (string? name) (string-is-xml-nmtoken? name)))
      (error:wrong-type-argument name "XML nmtoken string" 'XML-INTERN))
  (string->symbol name))

(define (xml-name? object)
  (and (symbol? object)
       (string-is-xml-name? (symbol-name object))))

(define (xml-nmtoken? object)
  (and (symbol? object)
       (string-is-xml-nmtoken? (symbol-name object))))

(define (string-is-xml-name? string)
  (let ((buffer (string->parser-buffer string)))
    (and (match-utf8-char-in-alphabet buffer alphabet:name-initial)
	 (let loop ()
	   (if (peek-parser-buffer-char buffer)
	       (and (match-utf8-char-in-alphabet buffer
						 alphabet:name-subsequent)
		    (loop))
	       #t)))))

(define (string-is-xml-nmtoken? string)
  (let ((buffer (string->parser-buffer string)))
    (let loop ()
      (and (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
	   (if (peek-parser-buffer-char buffer)
	       (loop)
	       #t)))))

(define (xml-whitespace-string? object)
  (string-composed-of? object char-set:xml-whitespace))

(define (string-composed-of? string char-set)
  (and (string? string)
       (substring-composed-of? string 0 (string-length string) char-set)))

(define (substring-composed-of? string start end char-set)
  (let loop ((index start))
    (or (fix:= index end)
	(and (char-set-member? char-set (string-ref string index))
	     (loop (fix:+ index 1))))))

(define-syntax define-xml-type
  (sc-macro-transformer
   (lambda (form environment)
     (if (syntax-match? '(IDENTIFIER * (IDENTIFIER EXPRESSION)) (cdr form))
	 (let ((root (symbol-append 'XML- (cadr form)))
	       (slots (cddr form)))
	   (let ((rtd (symbol-append '< root '>))
		 (constructor (symbol-append 'MAKE- root))
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
		  (DEFINE ,(symbol-append root '?)
		    (RECORD-PREDICATE ,rtd))
		  (DEFINE ,constructor
		    (LET ((CONSTRUCTOR
			   (RECORD-CONSTRUCTOR ,rtd '(,@(map car slots)))))
		      (NAMED-LAMBDA (,constructor ,@slot-vars)
			,@(map (lambda (slot var) (test slot var constructor))
			       slots slot-vars)
			(CONSTRUCTOR ,@slot-vars))))
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
  (attributes xml-attribute-list?)
  (contents xml-content?))

(define (xml-attribute-list? object)
  (list-of-type? object xml-attribute?))

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

(define (xml-char-data? object)
  (or (string? object)
      (wide-string? object)))

(define-xml-type comment
  (text xml-char-data?))

(define-xml-type processing-instructions
  (name
   (lambda (object)
     (and (xml-name? object)
	  (not (string-ci=? "xml" (symbol-name object))))))
  (text xml-char-data?))

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
  (id
   (lambda (object)
     (or (not object)
	 (public-id? object))))
  (uri
   (lambda (object)
     (or (not object)
	 (xml-char-data? object)))))

(define (public-id? object)
  (string-composed-of? object char-set:xml-public-id))

(define char-set:xml-public-id
  (char-set-union char-set:alphanumeric
		  (string->char-set " \r\n-'()+,./:=?;!*#@$_%")))

(define-xml-type !element
  (name xml-name?)
  (content-type
   (lambda (object)
     (or (eq? object 'EMPTY)
	 (eq? object 'ANY)
	 (and (pair? object)
	      (eq? 'MIX (car object))
	      (list-of-type? (cdr object) xml-name?))
	 (letrec
	     ((children?
	       (lambda (object)
		 (maybe-wrapped object
		   (lambda (object)
		     (and (pair? object)
			  (or (eq? 'ALT (car object))
			      (eq? 'SEQ (car object)))
			  (list-of-type? (cdr object) cp?))))))
	      (cp?
	       (lambda (object)
		 (or (maybe-wrapped object xml-name?)
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
  (name xml-name?)
  (definitions
    (lambda (object)
      (list-of-type? object
	(lambda (item)
	  (and (pair? item)
	       (xml-name? (car item))
	       (pair? (cdr item))
	       (!attlist-type? (cadr item))
	       (pair? (cddr item))
	       (!attlist-default? (caddr item))
	       (null? (cdddr item))))))))

(define (!attlist-type? object)
  (or (eq? object 'CDATA)
      (eq? object 'IDREFS)
      (eq? object 'IDREF)
      (eq? object 'ID)
      (eq? object 'ENTITY)
      (eq? object 'ENTITIES)
      (eq? object 'NMTOKENS)
      (eq? object 'NMTOKEN)
      (and (pair? object)
	   (eq? 'NOTATION (car object))
	   (list-of-type? (cdr object) xml-name?))
      (and (pair? object)
	   (eq? 'ENUMERATED (car object))
	   (list-of-type? (cdr object) xml-nmtoken?))))

(define (!attlist-default? object)
  (or (eq? object 'REQUIRED)
      (eq? object 'IMPLIED)
      (and (pair? object)
	   (eq? 'FIXED (car object))
	   (xml-attribute-value? (cdr object)))
      (and (pair? object)
	   (eq? 'DEFAULT (car object))
	   (xml-attribute-value? (cdr object)))))

(define-xml-type !entity
  (name xml-name?)
  (value entity-value?))

(define-xml-type unparsed-!entity
  (name xml-name?)
  (id xml-external-id?)
  (notation xml-name?))

(define-xml-type parameter-!entity
  (name xml-name?)
  (value entity-value?))

(define (entity-value? object)
  (or (and (pair? object)
	   (list-of-type? object
	     (lambda (object)
	       (or (xml-char-data? object)
		   (xml-entity-ref? object)
		   (xml-parameter-entity-ref? object)))))
      (xml-external-id? object)))

(define-xml-type !notation
  (name xml-name?)
  (id xml-external-id?))

(define-xml-type entity-ref
  (name xml-name?))

(define-xml-type parameter-entity-ref
  (name xml-name?))

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

(define-xml-printer element xml-element-name)
(define-xml-printer processing-instructions xml-processing-instructions-name)
(define-xml-printer dtd xml-dtd-root)
(define-xml-printer external-id
  (lambda (dtd)
    (or (xml-external-id-id dtd)
	(xml-external-id-uri dtd))))
(define-xml-printer !element xml-!element-name)
(define-xml-printer !attlist xml-!attlist-name)
(define-xml-printer !entity xml-!entity-name)
(define-xml-printer unparsed-!entity xml-unparsed-!entity-name)
(define-xml-printer parameter-!entity xml-parameter-!entity-name)
(define-xml-printer !notation xml-!notation-name)