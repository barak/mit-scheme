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

;;;; XML parser

;;; Comments of the form [N] refer to production rules in the XML 1.0
;;; standard, second edition, 6 October 2000.  Each such comment marks
;;; the code that corresponds to that rule.

(declare (usual-integrations))

;;;; Utilities

(define (perror ptr msg . irritants)
  (apply error
	 (string-append msg
			(if ptr
			    (string-append
			     " at "
			     (parser-buffer-position-string
			      ;; **** This isn't quite right.  ****
			      (if (pair? *entity-expansion-nesting*)
				  (cdar (last-pair *entity-expansion-nesting*))
				  ptr)))
			    "")
			(if (pair? irritants)
			    ":"
			    "."))
	 irritants))

(define (coalesce-elements v)
  (list->vector (coalesce-strings! (vector->list v))))

(define (coalesce-strings! elements)
  (do ((elements elements (cdr elements)))
      ((not (pair? elements)))
    (if (string? (car elements))
	(do ()
	    ((not (and (pair? (cdr elements))
		       (string? (cadr elements)))))
	  (set-car! elements
		    (string-append (car elements)
				   (cadr elements)))
	  (set-cdr! elements (cddr elements)))))
  elements)

(define (string-parser description char-set)
  (let ((a1 (char-set-difference char-set (string->char-set "\"")))
	(a2 (char-set-difference char-set (string->char-set "'"))))
    (*parser
     (alt (sbracket description "\"" "\"" (match (* (char-set a1))))
	  (sbracket description "'" "'" (match (* (char-set a2))))))))

;;;; Entry points

(define (read-xml-file pathname #!optional pi-handlers)
  (call-with-input-file pathname
    (lambda (port)
      (read-xml port (if (default-object? pi-handlers) '() pi-handlers)))))

(define (read-xml port #!optional pi-handlers)
  (receive (coding prefix) (determine-coding port)
    (parse-xml (textual-input-port->parser-buffer port prefix)
	       coding
	       (guarantee-pi-handlers pi-handlers 'read-xml))))

(define (bytevector->xml bv #!optional start end pi-handlers)
  (read-xml (binary->textual-port (open-input-bytevector bv start end))
	    pi-handlers))

(define (string->xml string #!optional start end pi-handlers)
  (parse-xml (string->parser-buffer string start end)
	     'any
	     (guarantee-pi-handlers pi-handlers 'string->xml)))

(define (guarantee-pi-handlers object caller)
  (if (default-object? object)
      '()
      (begin
	(if (not (list-of-type? object
		   (lambda (entry)
		     (and (pair? entry)
			  (symbol? (car entry))
			  (pair? (cdr entry))
			  (procedure? (cadr entry))
			  (procedure-arity-valid? (cadr entry) 1)
			  (null? (cddr entry))))))
	    (error:wrong-type-argument object "handler alist" caller))
	object)))

;;;; Character coding

(define (determine-coding port)
  (if (port/supports-coding? port)
      (begin
	(port/set-coding port 'BINARY)
	(port/set-line-ending port 'BINARY)
	(receive (coding name prefix) (determine-coding-1 port)
	  (port/set-coding port coding)
	  (port/set-line-ending port 'XML-1.0)
	  (values name prefix)))
      (values #f #f)))

(define (determine-coding-1 port)
  (let ((rb
	 (lambda ()
	   (let ((c (read-char port)))
	     (if (eof-object? c)
		 (error "EOF while determining char coding."))
	     (char->integer c))))
	(prefix
	 (lambda (n)
	   (string (integer->char n))))
	(lose
	 (lambda bytes
	   (error "Illegal starting bytes:" bytes))))
    (let ((b0 (rb)))
      (case b0
	((#x00)
	 (let* ((b1 (rb))
		(b2 (rb))
		(b3 (rb)))
	   (if (not (and (fix:= b1 #x00)
			 (fix:= b2 #xFE)
			 (fix:= b3 #xFF)))
	       (lose b0 b1 b2 b3)))
	 (values 'UTF-32BE 'UTF-32 #f))
	((#xEF)
	 (let* ((b1 (rb))
		(b2 (rb)))
	   (if (not (and (fix:= b1 #xBB)
			 (fix:= b2 #xBF)))
	       (lose b0 b1 b2)))
	 (values 'UTF-8 'UTF-8 #f))
	((#xFE)
	 (let ((b1 (rb)))
	   (if (not (fix:= b1 #xFF))
	       (lose b0 b1)))
	 (values 'UTF-16BE 'UTF-16 #f))
	((#xFF)
	 (let* ((b1 (rb))
		(b2 (rb))
		(b3 (rb)))
	   (if (not (fix:= b1 #xFE))
	       (lose b0 b1 b2 b3))
	   (if (and (fix:= b2 #x00) (fix:= b3 #x00))
	       (values 'UTF-32LE 'UTF-32 #f)
	       (values 'UTF-16LE 'UTF-16
		       (prefix (fix:or (fix:lsh b3 8) b2))))))
	(else
	 (values 'UTF-8
		 (if (fix:= b0 #x3C) 'NO-BOM 'UTF-8)
		 (prefix b0)))))))

(define (finish-coding buffer coding declaration)
  (if coding
      (let ((port (parser-buffer-port buffer)))
	(if port
	    (let* ((declared (normalize-coding port declaration))
		   (lose
		    (lambda ()
		      (error "Incorrect encoding declaration:" declared))))
	      (case coding
		((UTF-8 UTF-16)
		 (if (not (or (not declared) (eq? declared coding)))
		     (lose)))
		((UTF-32)
		 (if (not (eq? declared coding))
		     (lose)))
		((NO-BOM)
		 (if (coding-requires-bom? declared)
		     (lose))
		 (port/set-coding port (or declared 'UTF-8)))
		((ANY) unspecific)
		(else (error:bad-range-argument coding #f))))))))

(define (normalize-coding port declaration)
  (let ((coding
	 (and declaration
	      (let ((coding (xml-declaration-encoding declaration)))
		(and coding
		     (intern coding))))))
    (if (and coding
	     (not (port/known-coding? port coding)))
	(error:bad-range-argument coding #f))
    coding))

(define (coding-requires-bom? coding)
  (memq coding '(UTF-16 UTF-16BE UTF-16LE UTF-32 UTF-32BE UTF-32LE)))

;;;; Top level

(define (parse-xml buffer coding pi-handlers) ;[1,22]
  (let ((one-value (lambda (v) (and v (vector-ref v 0)))))
    (fluid-let ((*general-entities* (predefined-entities))
		(*standalone?*)
		(*internal-dtd?* #t)
		(*elt-decls* '())
		(*att-decls* '())
		(*pi-handlers* pi-handlers)
		(*in-dtd?* #f)
		(*prefix-bindings* '()))
      (let ((declaration (one-value (parse-declaration buffer))))
	(set! *standalone?*
	      (and declaration
		   (equal? (xml-declaration-standalone declaration)
			   "yes")))
	(finish-coding buffer coding declaration)
	(let* ((misc-1 (one-value (parse-misc buffer)))
	       (dtd
		(one-value
		 (fluid-let ((*in-dtd?* #t))
		   (parse-dtd buffer)))))
	  (if (html-dtd? dtd)
	      (add-html-entities!))
	  (let* ((misc-2 (if dtd (one-value (parse-misc buffer)) '()))
		 (element
		  (or (one-value (parse-element buffer))
		      (perror buffer "Missing root element")))
		 (misc-3 (one-value (parse-misc buffer))))
	    (if (peek-parser-buffer-char buffer)
		(perror buffer "Unparsed content in input"))
	    (make-xml-document declaration
			       misc-1
			       dtd
			       misc-2
			       element
			       misc-3)))))))

(define *standalone?*)
(define *internal-dtd?*)
(define *elt-decls*)
(define *att-decls*)
(define *pi-handlers* '())
(define *in-dtd?*)
(define *prefix-bindings*)

(define (xml-processing-instructions-handlers)
  *pi-handlers*)

(define parse-misc			;[27]
  (*parser
   (encapsulate vector->list
     (* (top-level
	 (alt parse-comment
	      parse-pi:misc
	      (map normalize-line-endings (match S))))))))

;;;; XML declaration

(define (xml-declaration-parser description text-decl?)
  description
  (*parser
   (top-level
    (with-pointer p
      (encapsulate
	  (lambda (v)
	    (transform-declaration (vector-ref v 0) text-decl? p))
	(seq "<?xml"
	     parse-declaration-attributes
	     "?>"))))))

(define parse-declaration		;[23,24,32,80]
  (xml-declaration-parser "XML declaration" #f))

(define parse-text-decl			;[77]
  (xml-declaration-parser "XML text declaration" #t))

(define (transform-declaration attributes text-decl? p)
  (let ((finish
	 (lambda (version encoding standalone)
	   (if (and (not text-decl?) (not version))
	       (perror p "Missing XML version"))
	   (if (and version
		    (not (*match-string match-xml-version version)))
	       (perror p "Malformed XML version" version))
	   (if (and version (not (string=? version "1.0")))
	       (perror p "Unsupported XML version" version))
	   (if (not (if encoding
			(*match-string match-encoding encoding)
			(not text-decl?)))
	       (perror p "Malformed encoding attribute" encoding))
	   (if standalone
	       (begin
		 (if (not (member standalone '("yes" "no")))
		     (perror p "Malformed standalone attribute" standalone))))
	   (make-xml-declaration version encoding standalone))))
    (let loop
	((attributes attributes)
	 (names
	  (if text-decl?
	      '(version encoding)
	      '(version encoding standalone)))
	 (results '()))
      (if (pair? names)
	  (if (pair? attributes)
	      (if (eq? (xml-attribute-name (car attributes)) (car names))
		  (loop (cdr attributes)
			(cdr names)
			(cons (xml-attribute-value (car attributes)) results))
		  (loop attributes
			(cdr names)
			(cons #f results)))
	      (let loop ((names names) (results results))
		(if (pair? names)
		    (loop (cdr names) (cons #f results))
		    (finish (caddr results) (cadr results) (car results)))))
	  (begin
	    (if (pair? attributes)
		(perror p "Extra attributes in XML declaration"
			(map xml-attribute-name attributes)))
	    (if text-decl?
		(finish (cadr results) (car results) #f)
		(finish (caddr results) (cadr results) (car results))))))))

(define match-xml-version		;[26]
  (let ((cs (char-set-union char-set:alphanumeric (string->char-set "_.:-"))))
    (*matcher (+ (char-set cs)))))

(define match-encoding			;[81]
  (let ((cs (char-set-union char-set:alphanumeric (string->char-set "_.-"))))
    (*matcher
     (seq (char-set char-set:alphabetic)
	  (* (char-set cs))))))

;;;; Elements

(define (parse-element b)		;[39]
  (let ((p (get-parser-buffer-pointer b)))
    (fluid-let ((*prefix-bindings* *prefix-bindings*))
      (let ((v (parse-start-tag b)))
	(and v
	     (begin
	       (namespace-processing! v p)
	       (vector (let ((name (vector-ref v 0)))
			 (make-xml-element name
					   (vector-ref v 1)
					   (if (string=? (vector-ref v 2) ">")
					       (parse-element-content b p name)
					       '()))))))))))

(define parse-start-tag			;[40,44]
  (*parser
   (top-level
    (bracket "start tag"
	(seq "<" parse-unexpanded-name)
	(match (alt ">" "/>"))
      parse-attribute-list))))

(define (namespace-processing! v p)
  (let* ((uname (vector-ref v 0))
	 (attrs (process-attr-decls (car uname) (vector-ref v 1) p)))
    (process-namespace-decls attrs)
    (vector-set! v 0 (expand-element-name uname))
    (for-each (lambda (attr)
		(set-xml-attribute-name! attr
					 (expand-attribute-name
					  (xml-attribute-name attr))))
	      attrs)
    (do ((attrs attrs (cdr attrs)))
	((not (pair? attrs)) unspecific)
      (let ((name (xml-attribute-name (car attrs))))
	(if (any (lambda (attr)
		   (xml-name=? (xml-attribute-name attr) name))
		 (cdr attrs))
	    (perror p "Attributes with same name" (xml-name->symbol name)))))))

(define (parse-element-content b p name)
  (let ((vc (parse-content b)))
    (if (not vc)
	(perror p "Unterminated start tag" name))
    (let ((ve (parse-end-tag b)))
      (if (not ve)
	  (if (peek-parser-buffer-char b)
	      (perror (get-parser-buffer-pointer b) "Unknown content")
	      (perror p "Unterminated start tag" name)))
      (if (not (eq? (xml-name->symbol (vector-ref ve 0))
		    (xml-name->symbol name)))
	  (perror p "Mismatched start tag" (vector-ref ve 0) name))
      (let ((content (coalesce-strings! (vector->list vc))))
	(if (null? content)
	    ;; Preserve fact that this element was formed by a
	    ;; start/end tag pair rather than by an empty
	    ;; element tag.
	    (list "")
	    content)))))

(define parse-end-tag			;[42]
  (*parser
   (top-level
    (sbracket "end tag" "</" ">"
      parse-required-element-name
      S?))))

(define parse-content			;[43]
  (*parser
   (seq parse-char-data
	(* (seq (alt parse-element
		     parse-reference
		     parse-cdata-section
		     parse-pi:element
		     parse-comment)
		parse-char-data)))))

;;;; Attribute defaulting

(define (process-attr-decls name attrs p)
  (let ((decl
	 (and (or *standalone?* *internal-dtd?*)
	      (find (lambda (decl)
		      (xml-name=? (xml-!attlist-name decl) name))
		    *att-decls*))))
    (if decl
	(do ((defns (xml-!attlist-definitions decl) (cdr defns))
	     (attrs attrs (process-attr-defn (car defns) attrs p)))
	    ((not (pair? defns)) attrs))
	attrs)))

(define (process-attr-defn defn attrs p)
  (let ((name (car defn))
	(type (cadr defn))
	(default (caddr defn)))
    (let ((attr
	   (find (lambda (attr)
		   (xml-name=? (car (xml-attribute-name attr)) name))
		 attrs)))
      (if attr
	  (let ((av (xml-attribute-value attr)))
	    (if (and (pair? default)
		     (eq? (car default) '|#FIXED|)
		     (not (string=? av (cdr default))))
		(perror (cdar attr) "Incorrect attribute value" name))
	    (if (not (eq? type '|CDATA|))
		(set-xml-attribute-value! attr (trim-attribute-whitespace av)))
	    attrs)
	  (begin
	    (if (eq? default '|#REQUIRED|)
		(perror p "Missing required attribute value" name))
	    (if (pair? default)
		(cons (%make-xml-attribute (cons name p) (cdr default)) attrs)
		attrs))))))

;;;; Other markup

(define (bracketed-region-parser description start end)
  (let ((parser
	 (terminated-region-parser description char-set:xml-char #t end)))
    (*parser (sbracket description start end parser))))

(define (terminated-region-parser description char-set must-match? end)
  (let ((matcher
	 (terminated-region-matcher description char-set must-match? end)))
    (*parser (map normalize-line-endings (match matcher)))))

(define (terminated-region-matcher description char-set must-match? . ends)
  description
  (lambda (buffer)
    (let loop ()
      (cond ((any (lambda (end)
		    (match-parser-buffer-string-no-advance buffer end))
		  ends)
	     #t)
	    ((match-parser-buffer-char-in-set buffer char-set)
	     (loop))
	    (must-match?
	     (let ((p (get-parser-buffer-pointer buffer))
		   (c (peek-parser-buffer-char buffer)))
	       (if c
		   (perror p "Illegal character" c)
		   (perror p "Unexpected EOF"))))
	    (else #t)))))

(define parse-char-data			;[14]
  (let ((parse-body
	 (terminated-region-parser "character data"
				   char-set:char-data
				   #f
				   "]]>")))
    (*parser
     (transform (lambda (v)
		  (if (fix:= 0 (string-length (vector-ref v 0)))
		      '#()
		      v))
       parse-body))))

(define parse-comment			;[15]
  (let ((parse-body
	 (terminated-region-parser "comment" char-set:xml-char #t "--")))
    (*parser
     (encapsulate
	 (lambda (v)
	   (make-xml-comment (vector-ref v 0)))
       (sbracket "comment" "<!--" "-->"
	 parse-body)))))

(define parse-cdata-section		;[18,19,20,21]
  (bracketed-region-parser "CDATA section" "<![CDATA[" "]]>"))

;;;; Names

(define parse-required-element-name
  (*parser (require-success "Malformed element name" parse-element-name)))

(define parse-element-name
  (*parser (map expand-element-name parse-unexpanded-name)))

(define parse-attribute-name
  (*parser (map expand-attribute-name parse-unexpanded-name)))

(define parse-unexpanded-name		;[5]
  (*parser
   (with-pointer p
     (map (lambda (s) (cons (make-xml-name s) p))
	  (match match:xml-name)))))

(define (simple-name-parser type)
  (let ((m (string-append "Malformed " type " name")))
    (*parser (require-success m (map make-xml-name (match match:xml-name))))))

(define parse-entity-name (simple-name-parser "entity"))
(define parse-pi-name (simple-name-parser "processing-instructions"))
(define parse-notation-name (simple-name-parser "notation"))

(define parse-required-nmtoken		;[7]
  (*parser
   (require-success "Malformed XML name token"
     (map make-xml-nmtoken (match match:xml-nmtoken)))))

;;;; Namespaces

(define (process-namespace-decls attrs)
  (set! *prefix-bindings*
	(let loop ((attrs attrs))
	  (if (pair? attrs)
	      (let ((uname (xml-attribute-name (car attrs)))
		    (value (xml-attribute-value (car attrs)))
		    (tail (loop (cdr attrs))))
		(let ((name (car uname))
		      (p (cdr uname)))
		  (let ((forbidden-uri
			 (lambda ()
			   (perror p "Forbidden namespace URI" value))))
		    (cond ((xml-name=? name 'xmlns)
			   (let ((uri (string->absolute-uri value)))
			     (if (or (uri=? value xml-uri)
				     (uri=? value xmlns-uri))
				 (forbidden-uri))
			     (cons (cons (null-xml-name-prefix) uri)
				   tail)))
			  ((and (xml-qname? name)
				(xml-name-prefix=? name 'xmlns))
			   (if (xml-name=? name 'xmlns:xmlns)
			       (perror p "Illegal namespace prefix" name))
			   (string->uri value) ;signals error if not URI
			   (if (if (xml-name=? name 'xmlns:xml)
				   (not (string=? value xml-uri-string))
				   (or (fix:= 0 (string-length value))
				       (string=? value xml-uri-string)
				       (string=? value xmlns-uri-string)))
			       (forbidden-uri))
			   (cons (cons (xml-name-local name) value) tail))
			  (else tail)))))
	      *prefix-bindings*)))
  unspecific)

(define (expand-element-name uname) (expand-name uname #f))
(define (expand-attribute-name uname) (expand-name uname #t))

(define (expand-name uname attribute-name?)
  (let ((name (car uname))
	(p (cdr uname)))
    (if (or *in-dtd?*
	    (not (xml-qname? name)))
	name
	(let ((uri (lookup-namespace-prefix name p attribute-name?)))
	  (if (null-xml-namespace-uri? uri)
	      name
	      (%make-xml-name name uri))))))

(define (lookup-namespace-prefix qname p attribute-name?)
  (let ((prefix (xml-qname-prefix qname)))
    (cond ((eq? prefix 'xmlns)
	   xmlns-uri)
	  ((eq? prefix 'xml)
	   xml-uri)
	  ((and attribute-name?
		(null-xml-name-prefix? prefix))
	   (null-xml-namespace-uri))
	  (else
	   (let ((entry (assq prefix *prefix-bindings*)))
	     (if entry
		 (cdr entry)
		 (begin
		   (if (not (null-xml-name-prefix? prefix))
		       (perror p "Undeclared XML prefix" prefix))
		   (null-xml-namespace-uri))))))))

;;;; Processing instructions

(define (pi-parser valid-content-item?) ;[16,17]
  (let ((description "processing instructions")
	(start "<?")
	(end "?>"))
    (let ((parse-body
	   (terminated-region-parser description char-set:xml-char #t end)))
      (*parser
       (with-pointer p
	 (transform
	     (lambda (v)
	       (let ((name (vector-ref v 0))
		     (text (vector-ref v 1)))
		 (if (string-ci=? (symbol->string name) "xml")
		     (perror p "Reserved XML processor name" name))
		 (let ((entry (assq name *pi-handlers*)))
		   (if entry
		       (let ((content ((cadr entry) text)))
			 (if (not (list-of-type? content valid-content-item?))
			     (perror p
				     "Illegal output from XML processor"
				     name))
			 (list->vector content))
		       (vector
			(make-xml-processing-instructions name text))))))
	   (sbracket description start end
	     parse-pi-name
	     (alt (seq S parse-body)
		  (values "")))))))))

(define parse-pi:misc
  (pi-parser xml-misc-content-item?))

(define parse-pi:element
  (pi-parser xml-content-item?))

(define parse-pi:internal-markup-decl
  (pi-parser
   (lambda (object)
     (or (xml-!element? object)
	 (xml-!attlist? object)
	 (xml-!entity? object)
	 (xml-!notation? object)
	 (xml-comment? object)
	 (xml-processing-instructions? object)))))

;;;; References

(define parse-char-reference		;[66]
  (let ((make-ref
	 (lambda (s r p)
	   (let ((n (string->number s r)))
	     (if (not (unicode-scalar-value? n))
		 (perror p "Invalid code point" n))
	     (let ((char (integer->char n)))
	       (if (not (char-in-set? char char-set:xml-char))
		   (perror p "Disallowed Unicode character" char))
	       (call-with-output-string
		 (lambda (port)
		   (write-char char port))))))))
    (*parser
     (with-pointer p
       (sbracket "character reference" "&#" ";"
	 (alt (map (lambda (s) (make-ref s 10 p))
		   (match match-decimal))
	      (seq "x"
		   (map (lambda (s) (make-ref s 16 p))
			(match match-hexadecimal)))))))))

(define match-decimal
  (*matcher (+ (char-set char-set:numeric))))

(define match-hexadecimal
  (*matcher (+ (char-set "0-9a-fA-f"))))

(define parse-reference			;[67]
  (*parser
   (alt parse-char-reference
	(with-pointer p
	  (transform
	      (lambda (v)
		(dereference-entity (vector-ref v 0) #f p))
	    parse-entity-reference-name)))))

(define parse-reference-deferred
  (*parser
   (match
    (seq "&"
	 (alt (seq "#"
		   (alt match-decimal
			(seq "x" match-hexadecimal)))
	      match:xml-name)
	 ";"))))

(define parse-entity-reference-name	;[68]
  (*parser
   (sbracket "entity reference" "&" ";"
     parse-entity-name)))

(define parse-entity-reference-deferred
  (*parser (match (seq "&" match:xml-name ";"))))

(define parse-parameter-entity-reference-name ;[69]
  (*parser
   (sbracket "parameter-entity reference" "%" ";"
     parse-entity-name)))

(define parse-parameter-entity-reference
  (*parser
   (map dereference-parameter-entity
	parse-parameter-entity-reference-name)))

;;;; Attributes

(define (attribute-list-parser parse-name ->name)
  (let ((parse-attribute (attribute-parser parse-name)))
    (*parser
     (with-pointer p
       (encapsulate
	   (lambda (v)
	     (let ((attrs (vector->list v)))
	       (do ((attrs attrs (cdr attrs)))
		   ((not (pair? attrs)))
		 (let ((name (->name (xml-attribute-name (car attrs)))))
		   (if (any (lambda (attr)
			      (xml-name=? (->name (xml-attribute-name attr))
					  name))
			    (cdr attrs))
		       (perror p "Attributes with same name" name))))
	       attrs))
	 (seq (* parse-attribute)
	      S?))))))

(define (attribute-parser parse-name)	;[41,25]
  (*parser
   (encapsulate (lambda (v)
		  (%make-xml-attribute (vector-ref v 0)
				       (vector-ref v 1)))
     (seq S
	  parse-name
	  S?
	  (require-success "Missing attribute separator" "=")
	  S?
	  parse-attribute-value))))

(define parse-attribute-list
  (attribute-list-parser parse-unexpanded-name
			 (lambda (a) (car a))))

(define parse-declaration-attributes
  (attribute-list-parser (*parser (map make-xml-name (match match:xml-name)))
			 (lambda (a) a)))

(define (attribute-value-parser char-set parse-reference)
  (let ((a1 (char-set-difference char-set (string->char-set "\"")))
	(a2 (char-set-difference char-set (string->char-set "'"))))
    (*parser
     (encapsulate (lambda (v)
		    (let ((elements (coalesce-strings! (vector->list v))))
		      (if (null? elements)
			  (list "")
			  elements)))
       (alt (sbracket "attribute value" "\"" "\""
	      (* (alt (match (+ (char-set a1)))
		      parse-reference)))
	    (sbracket "attribute value" "'" "'"
	      (* (alt (match (+ (char-set a2)))
		      parse-reference))))))))

(define parse-entity-value		;[9]
  (attribute-value-parser
   (char-set-difference char-set:xml-char (string->char-set "%&"))
   (*parser
    (alt parse-char-reference
	 parse-entity-reference-deferred
	 (with-pointer p
	   (sexp
	    (lambda (buffer)
	      (let ((v (parse-parameter-entity-reference-name buffer)))
		(and v
		     (let ((name (vector-ref v 0)))
		       (if (not *external-expansion?*)
			   (perror p "PE reference in internal subset" name))
		       (dereference-parameter-entity name)))))))))))

(define *external-expansion?* #f)

(define parse-attribute-value		;[10]
  (let ((parser
	 (attribute-value-parser char-set:char-data
				 parse-reference-deferred)))
    (*parser
     (map (lambda (elements)
	    (if (not (and (pair? elements)
			  (string? (car elements))
			  (null? (cdr elements))))
		(error "Uncoalesced attribute value:" elements))
	    (normalize-attribute-value (car elements)))
	  (require-success "Malformed attribute value" parser)))))

;;;; Normalization

(define (normalize-attribute-value string)
  (call-with-output-string
    (lambda (port)
      (let normalize-string ((string string))
	(let ((b (string->parser-buffer (normalize-line-endings string))))
	  (let loop ()
	    (let* ((p (get-parser-buffer-pointer b))
		   (char (read-parser-buffer-char b)))
	      (case char
		((#f)
		 unspecific)
		((#\tab #\newline #\return)
		 (write-char #\space port)
		 (loop))
		((#\&)
		 (set-parser-buffer-pointer! b p)
		 (let ((v (parse-char-reference b)))
		   (if v
		       (begin
			 (write-string (vector-ref v 0) port)
			 (loop))
		       (begin
			 (let ((name
				(vector-ref (parse-entity-reference-name b)
					    0)))
			   (let ((value (dereference-entity name #t p)))
			     (expand-entity-value name p
			       (lambda ()
				 (normalize-string value)))))
			 (loop)))))
		(else
		 (write-char char port)
		 (loop))))))))))

(define (trim-attribute-whitespace string)
  (let ((end (string-length string)))
    (call-with-output-string
     (lambda (port)

	 (define (skip-spaces start pending-space?)
	   (if (fix:< start end)
	       (let ((char (string-ref string start)))
		 (if (char-in-set? char-set:whitespace)
		     (skip-spaces (fix:+ start 1) pending-space?)
		     (begin
		       (if pending-space? (write-char #\space port))
		       (write-char char port)
		       (find-next-space (fix:+ start 1)))))))

	 (define (find-next-space start)
	   (if (fix:< start end)
	       (let ((char (string-ref string start)))
		 (if (char-in-set? char-set:whitespace)
		     (skip-spaces (fix:+ start 1) #t)
		     (begin
		       (write-char char port)
		       (find-next-space (fix:+ start 1)))))))

	 (skip-spaces 0 #f)))))

(define (normalize-line-endings string #!optional always-copy?)
  (if (string-find-next-char string #\return)
      (let ((end (string-length string)))
	(let ((step-over-eol
	       (lambda (index)
		 (fix:+ index
			(if (and (fix:< (fix:+ index 1) end)
				 (char=? (string-ref string (fix:+ index 1))
					 #\linefeed))
			    2
			    1)))))
	  (let ((builder (string-builder)))
	    (let loop ((start 0))
	      (let ((index
		     (string-find-next-char string #\return start end)))
		(if index
		    (begin
		      (builder #\newline)
		      (builder (string-slice string start index))
		      (loop (step-over-eol index)))
		    (builder (string-slice string start index)))))
	    (builder))))
      (if (if (default-object? always-copy?) #f always-copy?)
	  (string-copy string)
	  string)))

;;;; Parameter entities

(define (make-parameter-entity name value)
  (let ((entity (make-xml-parameter-!entity name value)))
    (if (and (not (eq? *parameter-entities* 'STOP))
	     (not (find-parameter-entity name)))
	(set! *parameter-entities* (cons entity *parameter-entities*)))
    entity))

(define (make-entity name value)
  (let ((entity (make-xml-!entity name value)))
    (if (and (not (eq? *general-entities* 'STOP))
	     (not (find-entity name)))
	(set! *general-entities* (cons entity *general-entities*)))
    entity))

(define (make-unparsed-entity name id notation)
  (let ((entity (make-xml-unparsed-!entity name id notation)))
    (if (and (not (eq? *general-entities* 'STOP))
	     (not (find-entity name)))
	(set! *general-entities* (cons entity *general-entities*)))
    entity))

(define (dereference-parameter-entity name)
  (let ((value
	 (and (not (eq? *parameter-entities* 'STOP))
	      (let ((entity (find-parameter-entity name)))
		(and entity
		     (xml-parameter-!entity-value entity))))))
    (if (and (pair? value)
	     (string? (car value))
	     (null? (cdr value)))
	(car value)
	(begin
	  (set! *parameter-entities* 'STOP)
	  (set! *general-entities* 'STOP)
	  (make-xml-parameter-entity-ref name)))))

(define (find-parameter-entity name)
  (find (lambda (entity)
	  (eq? name (xml-parameter-!entity-name entity)))
	*parameter-entities*))

(define *parameter-entities*)

;;;; General parsed entities

(define (dereference-entity name in-attribute? p)
  (if (eq? *general-entities* 'STOP)
      (perror p "Reference to externally-defined entity" name))
  (if (assq name *entity-expansion-nesting*)
      (perror p "Circular entity reference" name))
  (let ((entity (find-entity name)))
    (if (not entity)
	(perror p "Reference to undeclared entity" name))
    (if (xml-unparsed-!entity? entity)
	(perror p "Reference to unparsed entity" name))
    (let ((value (xml-!entity-value entity)))
      (if (xml-external-id? value)
	  (perror p "Reference to external entity" name))
      (if (not (and (pair? value)
		    (string? (car value))
		    (null? (cdr value))))
	  (perror p "Reference to partially-declared entity" name))
      (if in-attribute?
	  (car value)
	  (reparse-entity-value-string name (car value) p)))))

(define (reparse-entity-value-string name string p)
  (let ((v
	 (expand-entity-value name p
	   (lambda ()
	     (*parse-string parse-content string)))))
    (if (not v)
	(perror p "Malformed entity reference" string))
    v))

(define (expand-entity-value name p thunk)
  (fluid-let ((*entity-expansion-nesting*
	       (cons (cons name p) *entity-expansion-nesting*)))
    (thunk)))

(define (find-entity name)
  (let loop ((entities *general-entities*))
    (and (pair? entities)
	 (if (eq? (if (xml-!entity? (car entities))
		      (xml-!entity-name (car entities))
		      (xml-unparsed-!entity-name (car entities)))
		  name)
	     (car entities)
	     (loop (cdr entities))))))

(define (add-html-entities!)
  (if (pair? *general-entities*)
      (begin
	(set! *general-entities* (append *general-entities* html-entities))
	unspecific)))

(define (predefined-entities)
  (list (make-xml-!entity 'lt '("&#60;"))
	(make-xml-!entity 'gt '(">"))
	(make-xml-!entity 'amp '("&#38;"))
	(make-xml-!entity 'quot '("\""))
	(make-xml-!entity 'apos '("'"))))

(define *general-entities*)
(define *entity-expansion-nesting* '())

(define (make-external-id id uri p)
  (if *standalone?*
      (perror p "Illegal external reference in standalone document"))
  (make-xml-external-id id uri))

;;;; Document-type declarations

(define parse-dtd			;[28]
  (*parser
   (top-level
    (encapsulate
	(lambda (v)
	  (make-xml-dtd (vector-ref v 0)
			(vector-ref v 1)
			(vector-ref v 2)))
      (sbracket "document-type declaration" "<!DOCTYPE" ">"
	(require-success "Malformed document type"
	  (seq S
	       parse-required-element-name
	       (map (lambda (external)
		      (if external (set! *internal-dtd?* #f))
		      external)
		    (alt (seq S parse-external-id)
			 (values #f)))
	       S?
	       (alt (seq (sbracket "internal DTD" "[" "]"
			   parse-internal-subset)
			 S?)
		    (values '())))))))))

(define (parse-internal-subset buffer)
  (fluid-let ((*parameter-entities* '()))
    (let loop ((elements '#()))
      (let ((v
	     (or (parse-internal-markup-decl buffer)
		 (parse-decl-separator buffer))))
	(if v
	    (loop (vector-append elements v))
	    (vector (vector->list elements)))))))

(define parse-decl-separator		;[28a]
  (*parser
   (alt (with-pointer p
	  (transform
	      (lambda (v)
		(let ((value (vector-ref v 0)))
		  (if (string? value)
		      (reparse-text (vector (string-append " " value " "))
				    parse-external-subset-decl
				    "parameter-entity value"
				    p)
		      v)))
	    parse-parameter-entity-reference))
	S)))

(define parse-internal-markup-decl	;[29]
  (*parser
   (alt parse-!element
	parse-!attlist
	parse-!entity
	parse-!notation
	parse-pi:internal-markup-decl
	parse-comment)))

(define parse-!element			;[45]
  (letrec
      ((parse-children			;[47,49,50]
	(*parser
	 (encapsulate encapsulate-suffix
	   (seq (sbracket "!ELEMENT type" "(" ")"
		  S?
		  (alt (encapsulate (lambda (v) (cons 'alt (vector->list v)))
			 (seq parse-cp
			      (+ (seq S? "|" S? parse-cp))))
		       (encapsulate (lambda (v) (cons 'seq (vector->list v)))
			 (seq parse-cp
			      (* (seq S? "," S? parse-cp)))))
		  S?)
		(? (match (char-set "?*+")))))))

       (parse-cp			;[48]
	 (*parser
	  (alt (encapsulate encapsulate-suffix
		 (seq parse-element-name
		      (? (match (char-set "?*+")))))
	       parse-children)))

       (encapsulate-suffix
	(lambda (v)
	  (if (fix:= (vector-length v) 1)
	      (vector-ref v 0)
	      (list (string-ref (vector-ref v 1) 0)
		    (vector-ref v 0))))))

    (*parser
     (encapsulate
	 (lambda (v)
	   (let ((elt (make-xml-!element (vector-ref v 0) (vector-ref v 1))))
	     ;;(set! *elt-decls* (cons elt *elt-decls*))
	     elt))
       (sbracket "element declaration" "<!ELEMENT" ">"
	 S
	 parse-required-element-name
	 S
	 ;;[46]
	 (alt (map make-xml-name (match "EMPTY"))
	      (map make-xml-name (match "ANY"))
	      ;;[51]
	      (encapsulate vector->list
		(with-pointer p
		  (seq "("
		       S?
		       (map string->symbol (match "#PCDATA"))
		       (alt (seq S? ")")
			    (seq (* (seq S? "|" S? parse-element-name))
				 S?
				 ")*")

			    (sexp
			     (lambda (buffer)
			       buffer
			       (perror p "Ill-formed declaration value")))))))
	      parse-children))))))

(define parse-!attlist			;[52,53]
  (*parser
   (encapsulate
       (lambda (v)
	 (let ((attlist (make-xml-!attlist (vector-ref v 0) (vector-ref v 1))))
	   (set! *att-decls* (cons attlist *att-decls*))
	   attlist))
     (sbracket "attribute-list declaration" "<!ATTLIST" ">"
       S
       parse-required-element-name
       (encapsulate vector->list
	 (* (encapsulate
		(lambda (v)
		  (let ((name (vector-ref v 0))
			(type (vector-ref v 1))
			(default (vector-ref v 2)))
		    (list name type
			  (if (and (not (eq? type '|CDATA|))
				   (pair? default))
			      (cons (car default)
				    (trim-attribute-whitespace (cdr default)))
			      default))))
	      (seq S
		   parse-attribute-name
		   S
		   parse-!attlist-type
		   S
		   parse-!attlist-default))))
       S?))))

(define parse-!attlist-type		;[54,57]
  (*parser
   (alt (map make-xml-name
	     ;;[55,56]
	     (match (alt "CDATA" "IDREFS" "IDREF" "ID"
			 "ENTITY" "ENTITIES" "NMTOKENS" "NMTOKEN")))
	;;[58]
	(encapsulate vector->list
	  (bracket "notation type"
	      (seq (map make-xml-name (match "NOTATION"))
		   S
		   "(")
	      ")"
	    S?
	    parse-notation-name
	    (* (seq S? "|" S? parse-notation-name))
	    S?))
	;;[59]
	(encapsulate (lambda (v) (cons 'enumerated (vector->list v)))
	  (sbracket "enumerated type" "(" ")"
	    S?
	    parse-required-nmtoken
	    (* (seq S? "|" S? parse-required-nmtoken))
	    S?)))))

(define parse-!attlist-default		;[60]
  (*parser
   (alt (map string->symbol (match (alt "#REQUIRED" "#IMPLIED")))
	(encapsulate (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
	  (seq (map string->symbol (match "#FIXED"))
	       S
	       parse-attribute-value))
	(encapsulate (lambda (v) (cons 'default (vector-ref v 0)))
	  parse-attribute-value))))

(define parse-!entity			;[70,71,72,73,74,76]
  (*parser
   (sbracket "entity declaration" "<!ENTITY" ">"
     S
     (alt (encapsulate
	      (lambda (v)
		(make-parameter-entity (vector-ref v 0) (vector-ref v 1)))
	    (seq "%"
		 S
		 parse-entity-name
		 S
		 (alt parse-entity-value
		      parse-external-id)))
	  (encapsulate
	      (lambda (v)
		(if (fix:= (vector-length v) 2)
		    (make-entity (vector-ref v 0) (vector-ref v 1))
		    (make-unparsed-entity (vector-ref v 0)
					  (vector-ref v 1)
					  (vector-ref v 2))))
	    (seq parse-entity-name
		 S
		 (alt parse-entity-value
		      (seq parse-external-id
			   (? (seq S "NDATA" S
				   parse-notation-name)))))))
     S?)))

(define parse-!notation			;[82,83]
  (*parser
   (encapsulate
       (lambda (v) (make-xml-!notation (vector-ref v 0) (vector-ref v 1)))
     (sbracket "notation declaration" "<!NOTATION" ">"
       S
       parse-notation-name
       S
       (alt parse-external-id
	    (encapsulate
		(lambda (v) (make-xml-external-id (vector-ref v 0) #f))
	      (seq "PUBLIC"
		   S
		   parse-public-id-literal)))
       S?))))

(define parse-external-id		;[75]
  (*parser
   (with-pointer p
     (alt (encapsulate
	      (lambda (v)
		(make-external-id #f (vector-ref v 0) p))
	    (seq "SYSTEM"
		 S
		 parse-system-literal))
	  (encapsulate
	      (lambda (v)
		(make-external-id (vector-ref v 0) (vector-ref v 1) p))
	    (seq "PUBLIC"
		 S
		 parse-public-id-literal
		 S
		 parse-system-literal))))))

(define parse-system-literal		;[11]
  (string-parser "system literal" char-set:xml-char))

(define parse-public-id-literal		;[12,13]
  (string-parser "public-ID literal"
		 (char-set-union
		  char-set:alphanumeric
		  (string->char-set " \r\n-'()+,./:=?;!*#@$_%"))))

;;;; External subset

;; This is hairy because parameter-entity references can appear almost
;; anywhere within DTD declarations, when the declarations appear in
;; the external subset.  Rather than make the declaration parsing
;; rules overly complex, we do a pre-pass to expand references in the
;; interior of each declaration, and then reparse the expanded text.

(define parse-external-subset-decl	;[31]
  (*parser
   (* (alt parse-external-markup-decl
	   parse-conditional-section
	   parse-decl-separator))))

(define external-decl-parser
  (let ((a1 (char-set-difference char-set:xml-char (string->char-set "%\"'>")))
	(a2 (char-set-difference char-set:xml-char (string->char-set "\"")))
	(a3 (char-set-difference char-set:xml-char (string->char-set "'"))))
    (lambda (prefix parse-decl)
      (*parser
       (with-pointer p
	 (transform
	     (lambda (v)
	       (reparse-text v parse-decl "markup declaration" p))
	   (seq
	    (match prefix)
	    (require-success "Malformed markup declaration"
	      (seq
	       (* (alt (match
			(alt (+ (char-set a1))
			     (seq (char #\") (* (char-set a2)) (char #\"))
			     (seq (char #\') (* (char-set a3)) (char #\'))))
		       parse-parameter-entity-reference))
	       (match ">"))))))))))

(define (reparse-text v parser description ptr)
  (let ((v (coalesce-elements v)))
    (if (and (fix:= (vector-length v) 1)
	     (string? (vector-ref v 0)))
	(let ((v*
	       (fluid-let ((*external-expansion?* #t))
		 (*parse-string parser (vector-ref v 0)))))
	  (if (not v*)
	      (perror ptr
		      (string-append "Malformed " description)
		      (vector-ref v 0)))
	  v*)
	v)))

(define parse-external-markup-decl	;[29]
  (let ((parse-!element
	 (external-decl-parser (*matcher (seq "<!ELEMENT" S))
			       parse-!element))
	(parse-!attlist
	 (external-decl-parser (*matcher (seq "<!ATTLIST" S))
			       parse-!attlist))
	(parse-!entity
	 (external-decl-parser (*matcher (seq "<!ENTITY" S (? (seq "%" S))))
			       parse-!entity))
	(parse-!notation
	 (external-decl-parser (*matcher (seq "<!NOTATION" S))
			       parse-!notation)))
    (*parser
     (alt parse-internal-markup-decl
	  parse-!element
	  parse-!attlist
	  parse-!entity
	  parse-!notation))))

;;;; Conditional sections

(define parse-conditional-section	;[61]
  (*parser
   (alt parse-!ignore-section
	parse-!include-section
	parse-parameterized-conditional)))

(define-integrable conditional-start "<![")
(define-integrable conditional-end "]]>")

(define parse-!include-section		;[62]
  (*parser
   (bracket "!INCLUDE section"
       (noise (seq (string conditional-start)
		   S?
		   "INCLUDE"
		   S?
		   "["))
       (noise (string conditional-end))
     parse-external-subset-decl)))

(define parse-!ignore-section		;[63]
  (*parser
   (bracket "!IGNORE section"
       (noise (seq (string conditional-start)
		   S?
		   "IGNORE"
		   S?
		   "["))
       (noise (string conditional-end))
     (noise (* match-!ignore-contents)))))

(define match-!ignore-contents		;[64]
  (*matcher
   (seq match-!ignore
	(* (seq (string conditional-start)
		match-!ignore-contents
		(string conditional-end)
		match-!ignore)))))

(define match-!ignore			;[65]
  (terminated-region-matcher "ignore section" char-set:xml-char #t
			     conditional-start conditional-end))

(define parse-parameterized-conditional
  (*parser
   (with-pointer p
     (transform
	 (lambda (v)
	   (reparse-text v parse-conditional-section "conditional section" p))
       (bracket "parameterized conditional section"
	   (seq (match (string conditional-start))
		S?
		parse-parameter-entity-reference
		S?
		(match "["))
	   (match (string conditional-end))
	 (match (* match-!ignore-contents)))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'encapsulate 1)
;;; Eval: (scheme-indent-method 'transform 1)
;;; Eval: (scheme-indent-method 'require-success 1)
;;; Eval: (scheme-indent-method 'bracket 3)
;;; Eval: (scheme-indent-method 'sbracket 3)
;;; End:
