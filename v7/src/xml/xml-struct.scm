#| -*-Scheme-*-

$Id: xml-struct.scm,v 1.21 2003/08/03 06:20:40 cph Exp $

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

(define-record-type <combo-name>
    (make-combo-name simple universal)
    combo-name?
  (simple combo-name-simple)
  (universal combo-name-universal))

(set-record-type-unparser-method! <combo-name>
  (standard-unparser-method 'XML-NAME
    (lambda (name port)
      (write-char #\space port)
      (write (combo-name-simple name) port))))

(define-record-type <universal-name>
    (make-universal-name uri local combos)
    universal-name?
  (uri universal-name-uri)
  (local universal-name-local)
  (combos universal-name-combos))

(define (xml-name? object)
  (or (and (symbol? object)
	   (string-is-xml-name? (symbol-name object)))
      (combo-name? object)))

(define (guarantee-xml-name object caller)
  (if (not (xml-name? object))
      (error:not-xml-name object caller)))

(define (error:not-xml-name object caller)
  (error:wrong-type-argument object "an XML name" caller))

(define (xml-intern string #!optional uri)
  (guarantee-string string 'XML-INTERN)
  (cond ((and (string-is-xml-nmtoken? string)
	      (or (default-object? uri) (not uri)))
	 (string->symbol string))
	((string-is-xml-name? string)
	 (guarantee-string uri 'XML-INTERN)
	 (if (not (and (fix:> (string-length uri) 0)
		       (utf8-string-valid? uri)))
	     (error:wrong-type-argument uri "an XML name URI" 'XML-INTERN))
	 (let ((simple (string->symbol string)))
	   (%make-xml-name simple
			   uri
			   (let ((c (string-find-next-char string #\:)))
			     (if c
				 (string->symbol
				  (string-tail string (fix:+ c 1)))
				 simple)))))
	(else
	 (error:wrong-type-argument string "an XML name string" 'XML-INTERN))))

(define (%make-xml-name simple uri local)
  (let ((uname
	 (hash-table/intern! (hash-table/intern! universal-names
						 uri
						 make-eq-hash-table)
			     local
			     (lambda ()
			       (make-universal-name uri
						    local
						    (make-eq-hash-table))))))
    (hash-table/intern! (universal-name-combos uname)
			simple
			(lambda () (make-combo-name simple uname)))))

(define universal-names
  (make-string-hash-table))

(define (xml-name-string name)
  (cond ((xml-nmtoken? name) (symbol-name name))
	((combo-name? name) (symbol-name (combo-name-simple name)))
	(else (error:not-xml-name name 'XML-NAME-STRING))))

(define (xml-name-uri name)
  (cond ((xml-nmtoken? name) #f)
	((combo-name? name) (universal-name-uri (combo-name-universal name)))
	(else (error:not-xml-name name 'XML-NAME-URI))))

(define (xml-name-local name)
  (cond ((xml-nmtoken? name)
	 (let ((s (symbol-name name)))
	   (let ((c (string-find-next-char s #\:)))
	     (if c
		 (string->symbol (string-tail s (fix:+ c 1)))
		 name))))
	((combo-name? name) (universal-name-local (combo-name-universal name)))
	(else (error:not-xml-name name 'XML-NAME-STRING))))

(define (xml-name=? n1 n2)
  (let ((lose (lambda (n) (error:not-xml-name n 'XML-NAME=?))))
    (cond ((xml-nmtoken? n1)
	   (cond ((xml-nmtoken? n2) (eq? n1 n2))
		 ((combo-name? n2) (eq? n1 (combo-name-simple n2)))
		 (else (lose n2))))
	  ((combo-name? n1)
	   (cond ((xml-nmtoken? n2)
		  (eq? (combo-name-simple n1) n2))
		 ((combo-name? n2)
		  (eq? (combo-name-universal n1)
		       (combo-name-universal n2)))
		 (else (lose n2))))
	  (else (lose n1)))))

(define (xml-name-hash name modulus)
  (eq-hash-mod (xml-name-local name) modulus))

(define make-xml-name-hash-table
  (strong-hash-table/constructor xml-name-hash xml-name=? #t))

(define (xml-nmtoken? object)
  (and (symbol? object)
       (string-is-xml-nmtoken? (symbol-name object))))

(define (string-is-xml-name? string)
  (let ((buffer (string->parser-buffer string)))
    (and (match-utf8-char-in-alphabet buffer alphabet:name-initial)
	 (let loop ((nc 0))
	   (cond ((match-parser-buffer-char buffer #\:)
		  (loop (fix:+ nc 1)))
		 ((peek-parser-buffer-char buffer)
		  (and (match-utf8-char-in-alphabet buffer
						    alphabet:name-subsequent)
		       (loop nc)))
		 (else (fix:<= nc 1)))))))

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
     (if (syntax-match? '(IDENTIFIER * (IDENTIFIER EXPRESSION ? EXPRESSION))
			(cdr form))
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
     (and (xml-name? object)
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
  (uri (lambda (object)
	 (or (not object)
	     (xml-char-data? object)))
       canonicalize-char-data))

(define (public-id? object)
  (string-composed-of? object char-set:xml-public-id))

(define char-set:xml-public-id
  (char-set-union char-set:alphanumeric
		  (string->char-set " \r\n-'()+,./:=?;!*#@$_%")))

(define-xml-type !element
  (name xml-name?)
  (content-type
   (lambda (object)
     (or (eq? object '|EMPTY|)
	 (eq? object '|ANY|)
	 (and (pair? object)
	      (eq? '|#PCDATA| (car object))
	      (list-of-type? (cdr object) xml-name?))
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
	   (list-of-type? (cdr object) xml-name?))
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
  (name xml-name?)
  (value entity-value? canonicalize-entity-value))

(define-xml-type unparsed-!entity
  (name xml-name?)
  (id xml-external-id?)
  (notation xml-name?))

(define-xml-type parameter-!entity
  (name xml-name?)
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