#| -*-Scheme-*-

$Id: xml-parser.scm,v 1.33 2003/08/03 06:14:19 cph Exp $

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

(define (string-parser description alphabet)
  (let ((a1 (alphabet- alphabet (string->alphabet "\"")))
	(a2 (alphabet- alphabet (string->alphabet "'"))))
    (*parser
     (alt (sbracket description "\"" "\"" (match (* (alphabet a1))))
	  (sbracket description "'" "'" (match (* (alphabet a2))))))))

(define (simple-attribute-value? v)
  (and (pair? v)
       (string? (car v))
       (null? (cdr v))))

(define (read-xml-file pathname #!optional pi-handlers)
  (call-with-input-file pathname
    (lambda (port)
      (read-xml port (if (default-object? pi-handlers) '() pi-handlers)))))

(define (read-xml port #!optional pi-handlers)
  (parse-xml-document (input-port->parser-buffer port)
		      (if (default-object? pi-handlers) '() pi-handlers)))

(define (string->xml string #!optional pi-handlers)
  (parse-xml-document (string->parser-buffer string)
		      (if (default-object? pi-handlers) '() pi-handlers)))

(define (substring->xml string start end #!optional pi-handlers)
  (parse-xml-document (substring->parser-buffer string start end)
		      (if (default-object? pi-handlers) '() pi-handlers)))

;;;; Top level

(define (parse-xml-document buffer #!optional pi-handlers) ;[1,22]
  (if (not (parser-buffer? buffer))
      (error:wrong-type-argument buffer "parser buffer" 'PARSE-XML-DOCUMENT))
  (let ((pi-handlers (if (default-object? pi-handlers) '() pi-handlers)))
    (if (not (list-of-type? pi-handlers
	       (lambda (entry)
		 (and (pair? entry)
		      (symbol? (car entry))
		      (pair? (cdr entry))
		      (procedure? (cadr entry))
		      (procedure-arity-valid? (cadr entry) 1)
		      (null? (cddr entry))))))
	(error:wrong-type-argument pi-handlers "handler alist"
				   'PARSE-XML-DOCUMENT))
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
	  (let* ((misc-1 (one-value (parse-misc buffer)))
		 (dtd
		  (one-value
		   (fluid-let ((*in-dtd?* #t))
		     (parse-dtd buffer))))
		 (misc-2 (if dtd (one-value (parse-misc buffer)) '()))
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
(define *pi-handlers*)
(define *in-dtd?*)
(define *prefix-bindings*)

(define parse-misc			;[27]
  (*parser
   (encapsulate vector->list
     (* (top-level
	 (alt parse-comment
	      parse-pi:misc
	      (map normalize-line-endings (match S))))))))

;;;; XML declaration

(define (xml-declaration-parser description text-decl?)
  (*parser
   (top-level
    (with-pointer p
      (encapsulate
	  (lambda (v)
	    (transform-declaration (vector-ref v 0) text-decl? p))
	(sbracket description "<?xml" "?>"
	  parse-declaration-attributes))))))

(define parse-declaration		;[23,24,32,80]
  (xml-declaration-parser "XML declaration" #f))

(define parse-text-decl			;[77]
  (xml-declaration-parser "XML text declaration" #t))

(define (transform-declaration attributes text-decl? p)
  (if (not (for-all? attributes
	     (lambda (attribute)
	       (simple-attribute-value? (cdr attribute)))))
      (perror p "XML declaration can't contain entity refs" attributes))
  (let ((finish
	 (lambda (version encoding standalone)
	   (if (and (not text-decl?) (not version))
	       (perror p "Missing XML version"))
	   (if (not (if version
			(match-xml-version (string->parser-buffer version))
			#t))
	       (perror p "Malformed XML version" version))
	   (if (not (if encoding
			(match-encoding (string->parser-buffer encoding))
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
	      (if (eq? (caar attributes) (car names))
		  (loop (cdr attributes)
			(cdr names)
			(cons (cadar attributes) results))
		  (loop attributes
			(cdr names)
			(cons #f results)))
	      (let loop ((names names) (results results))
		(if (pair? names)
		    (loop (cdr names) (cons #f results))
		    (finish (caddr results) (cadr results) (car results)))))
	  (begin
	    (if (pair? attributes)
		(perror p "Extra attributes in XML declaration" attributes))
	    (if text-decl?
		(finish (cadr results) (car results) #f)
		(finish (caddr results) (cadr results) (car results))))))))

(define match-xml-version		;[26]
  (let ((cs (char-set-union char-set:alphanumeric (string->char-set "_.:-"))))
    (*matcher (complete (+ (char-set cs))))))

(define match-encoding			;[81]
  (let ((cs (char-set-union char-set:alphanumeric (string->char-set "_.-"))))
    (*matcher
     (complete
      (seq (char-set char-set:alphabetic)
	   (* (char-set cs)))))))

;;;; Elements

(define (parse-element buffer)		;[39]
  (let ((p (get-parser-buffer-pointer buffer)))
    (fluid-let ((*prefix-bindings* *prefix-bindings*))
      (let ((v (parse-start-tag buffer)))
	(and v
	     (vector
	      (make-xml-element
	       (vector-ref v 0)
	       (vector-ref v 1)
	       (if (string=? (vector-ref v 2) ">")
		   (let loop ((elements '#()))
		     (let ((v* (parse-end-tag buffer)))
		       (if v*
			   (begin
			     (if (not (eq? (vector-ref v 0) (vector-ref v* 0)))
				 (perror p "Mismatched start tag"
					 (vector-ref v 0) (vector-ref v* 0)))
			     (let ((contents
				    (coalesce-strings!
				     (delete-matching-items!
					 (vector->list elements)
				       (lambda (element)
					 (and (string? element)
					      (string-null? element)))))))
			       (if (null? contents)
				   ;; Preserve fact that this element
				   ;; was formed by a start/end tag pair
				   ;; rather than by an empty-element
				   ;; tag.
				   (list "")
				   contents)))
			   (let ((v* (parse-content buffer)))
			     (if (not v*)
				 (perror p "Unterminated start tag"
					 (vector-ref v 0)))
			     (if (equal? v* '#(""))
				 (perror p "Unknown content"))
			     (loop (vector-append elements v*))))))
		   '()))))))))

(define parse-start-tag			;[40,44]
  (*parser
   (top-level
    (with-pointer p
      (transform (lambda (v)
		   (let* ((name (vector-ref v 0))
			  (attributes
			   (process-attr-decls name (vector-ref v 1) p)))
		     (process-namespace-decls attributes)
		     (vector (intern-element-name name)
			     (map (lambda (attr)
				    (cons (intern-attribute-name (car attr))
					  (cdr attr)))
				  attributes)
			     (vector-ref v 2))))
	(bracket "start tag"
	    (seq "<" parse-uninterned-name)
	    (match (alt ">" "/>"))
	  parse-attribute-list))))))

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

(define (process-attr-decls name attributes p)
  (let ((decl
	 (and (or *standalone?* *internal-dtd?*)
	      (find-matching-item *att-decls*
		(let ((name (string->symbol (car name))))
		  (lambda (decl)
		    (eq? name (xml-!attlist-name decl))))))))
    (if decl
	(let loop
	    ((definitions (xml-!attlist-definitions decl))
	     (attributes attributes))
	  (if (pair? definitions)
	      (loop (cdr definitions)
		    (process-attr-defn (car definitions) attributes p))
	      attributes))
	attributes)))

(define (process-attr-defn definition attributes p)
  (let ((name (symbol-name (car definition)))
	(type (cadr definition))
	(default (caddr definition)))
    (let ((attribute
	   (find-matching-item attributes
	     (lambda (attribute)
	       (string=? name (caar attribute))))))
      (if attribute
	  (let ((av (cdr attribute)))
	    (if (and (pair? default)
		     (eq? (car default) '|#FIXED|)
		     (not (attribute-value=? av (cdr default))))
		(perror (cdar attribute)
			"Incorrect attribute value"
			(string->symbol name)))
	    (if (and (not (eq? type '|CDATA|))
		     (simple-attribute-value? av))
		(set-car! av (trim-attribute-whitespace (car av))))
	    attributes)
	  (begin
	    (if (eq? default '|#REQUIRED|)
		(perror p
			"Missing required attribute value"
			(string->symbol name)))
	    (if (pair? default)
		(cons (cons (cons name p) (cdr default))
		      attributes)
		attributes))))))

(define (attribute-value=? v1 v2)
  (and (boolean=? (pair? v1) (pair? v2))
       (if (pair? v1)
	   (and (let ((i1 (car v1))
		      (i2 (car v2)))
		  (cond ((string? i1)
			 (and (string? i2)
			      (string=? i1 i2)))
			((xml-entity-ref? i1)
			 (and (xml-entity-ref? i2)
			      (eq? (xml-entity-ref-name i1)
				   (xml-entity-ref-name i2))))
			(else
			 (error "Unknown attribute value item:" i1))))
		(attribute-value=? (cdr v1) (cdr v2)))
	   #t)))

;;;; Other markup

(define (bracketed-region-parser description start end)
  (let ((parser (terminated-region-parser description alphabet:xml-char end)))
    (*parser (sbracket description start end parser))))

(define (terminated-region-parser description alphabet . ends)
  (let ((matcher (apply terminated-region-matcher description alphabet ends)))
    (*parser (map normalize-line-endings (match matcher)))))

(define (terminated-region-matcher description alphabet . ends)
  description
  (lambda (buffer)
    (let loop ()
      (if (and (not (there-exists? ends
		      (lambda (end)
			(match-parser-buffer-string-no-advance buffer
							       end))))
	       (match-utf8-char-in-alphabet buffer alphabet))
	  (loop)
	  #t))))

(define parse-char-data			;[14]
  (terminated-region-parser "character data" alphabet:char-data "]]>"))

(define parse-comment			;[15]
  (let ((parse-body
	 (terminated-region-parser "comment" alphabet:xml-char "--")))
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
  (*parser (map intern-element-name parse-uninterned-name)))

(define parse-attribute-name
  (*parser (map intern-attribute-name parse-uninterned-name)))

(define parse-uninterned-name		;[5]
  (*parser
   (with-pointer p
     (map (lambda (s) (cons s p))
	  (match (seq (? (seq match-name ":"))
		      match-name))))))

(define (simple-name-parser type)
  (let ((m (string-append "Malformed " type " name")))
    (*parser (require-success m (map xml-intern (match match-name))))))

(define parse-entity-name (simple-name-parser "entity"))
(define parse-pi-name (simple-name-parser "processing-instructions"))
(define parse-notation-name (simple-name-parser "notation"))

(define (match-name buffer)
  (and (match-utf8-char-in-alphabet buffer alphabet:name-initial)
       (let loop ()
	 (if (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
	     (loop)
	     #t))))

(define parse-required-name-token	;[7]
  (*parser
   (require-success "Malformed XML name token"
     (map xml-intern (match match-name-token)))))

(define (match-name-token buffer)
  (and (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
       (let loop ()
	 (if (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
	     (loop)
	     #t))))

(define (process-namespace-decls attributes)
  (set! *prefix-bindings*
	(let loop ((attributes attributes))
	  (if (pair? attributes)
	      (let ((name (caar attributes))
		    (value (cdar attributes))
		    (tail (loop (cdr attributes))))
		(let ((s (car name))
		      (pn (cdr name)))
		  (let ((uri
			 (lambda ()
			   (if (not (simple-attribute-value? value))
			       (perror pn "Illegal namespace URI" value))
			   (if (string-null? (car value))
			       #f	;xmlns=""
			       (car value))))
			(forbidden-uri
			 (lambda (uri)
			   (perror pn "Forbidden namespace URI" uri))))
		    (let ((guarantee-legal-uri
			   (lambda (uri)
			     (if (and uri
				      (or (string=? uri xml-uri)
					  (string=? uri xmlns-uri)))
				 (forbidden-uri uri)))))
		      (cond ((string=? "xmlns" s)
			     (let ((uri (uri)))
			       (guarantee-legal-uri uri)
			       (cons (cons #f uri) tail)))
			    ((string-prefix? "xmlns:" s)
			     (if (string=? "xmlns:xmlns" s)
				 (perror pn "Illegal namespace prefix" s))
			     (let ((uri (uri)))
			       (if (not uri) ;legal in XML 1.1
				   (forbidden-uri ""))
			       (if (string=? "xmlns:xml" s)
				   (if (not (and uri (string=? uri xml-uri)))
				       (forbidden-uri uri))
				   (guarantee-legal-uri uri))
			       (cons (cons (string-tail s 6) uri) tail)))
			    (else tail))))))
	      *prefix-bindings*)))
  unspecific)

(define (intern-element-name n) (intern-name n #t))
(define (intern-attribute-name n) (intern-name n #f))

(define (intern-name n element-name?)
  (let ((s (car n))
	(p (cdr n)))
    (let ((simple (string->symbol s))
	  (c (string-find-next-char s #\:)))
      (let ((uri
	     (and (not *in-dtd?*)
		  (or element-name? c)
		  (let ((prefix (and c (string->symbol (string-head s c)))))
		    (case prefix
		      ((xmlns) xmlns-uri)
		      ((xml) xml-uri)
		      (else
		       (let ((entry (assq prefix *prefix-bindings*)))
			 (if entry
			     (cdr entry)
			     (begin
			       (if prefix
				   (perror p "Unknown XML prefix" prefix))
			       #f)))))))))
	(if uri
	    (%make-xml-name simple
			    uri
			    (if c
				(string->symbol (string-head s (fix:+ c 1)))
				simple))
	    simple)))))

(define xml-uri "http://www.w3.org/XML/1998/namespace")
(define xmlns-uri "http://www.w3.org/2000/xmlns/")

;;;; Processing instructions

(define (pi-parser valid-content?) ;[16,17]
  (let ((description "processing instructions")
	(start "<?")
	(end "?>"))
    (let ((parse-body
	   (terminated-region-parser description alphabet:xml-char end)))
      (*parser
       (with-pointer p
	 (transform
	     (lambda (v)
	       (let ((name (vector-ref v 0))
		     (text (vector-ref v 1)))
		 (if (string-ci=? (symbol-name name) "xml")
		     (perror p "Reserved XML processor name" name))
		 (let ((entry (assq name *pi-handlers*)))
		   (if entry
		       (let ((content ((cadr entry) text)))
			 (if (not (list-of-type? content valid-content?))
			     (perror p
				     "Illegal output from XML processor"
				     name))
			 (list->vector content))
		       (vector
			(make-xml-processing-instructions name text))))))
	   (sbracket description start end
	     parse-pi-name
	     parse-body)))))))

(define parse-pi:misc
  (pi-parser
   (lambda (object)
     (or (string? object)
	 (xml-comment? object)
	 (xml-processing-instructions? object)))))

(define parse-pi:element
  (pi-parser
   (lambda (object)
     (or (string? object)
	 (xml-element? object)
	 (xml-comment? object)
	 (xml-processing-instructions? object)))))

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
	     (if (not (unicode-code-point? n))
		 (perror p "Invalid code point" n))
	     (let ((char (integer->char n)))
	       (if (not (char-in-alphabet? char alphabet:xml-char))
		   (perror p "Disallowed Unicode character" char))
	       (call-with-output-string
		 (lambda (port)
		   (write-utf8-char char port))))))))
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
		(let ((name (vector-ref v 0)))
		  (or (dereference-entity name #f p)
		      (vector (make-xml-entity-ref name)))))
	    parse-entity-reference-name)))))

(define parse-reference-deferred
  (*parser
   (match
    (seq "&"
	 (alt (seq "#"
		   (alt match-decimal
			(seq "x" match-hexadecimal)))
	      match-name)
	 ";"))))

(define parse-entity-reference-name	;[68]
  (*parser
   (sbracket "entity reference" "&" ";"
     parse-entity-name)))

(define parse-entity-reference-deferred
  (*parser (match (seq "&" match-name ";"))))

(define parse-parameter-entity-reference-name ;[69]
  (*parser
   (sbracket "parameter-entity reference" "%" ";"
     parse-entity-name)))

(define parse-parameter-entity-reference
  (*parser
   (map dereference-parameter-entity
	parse-parameter-entity-reference-name)))

;;;; Attributes

(define (attribute-list-parser parse-name)
  (let ((parse-attribute (attribute-parser parse-name)))
    (*parser
     (with-pointer p
       (encapsulate
	   (lambda (v)
	     (let ((alist (vector->list v)))
	       (do ((alist alist (cdr alist)))
		   ((not (pair? alist)))
		 (let ((entry (assq (caar alist) (cdr alist))))
		   (if entry
		       (perror p "Duplicate entry in attribute list"))))
	       alist))
	 (seq (* parse-attribute)
	      S?))))))

(define (attribute-parser parse-name)	;[41,25]
  (*parser
   (encapsulate (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
     (seq S
	  parse-name
	  S?
	  (require-success "Missing attribute separator" "=")
	  S?
	  parse-attribute-value))))

(define parse-declaration-attributes
  (attribute-list-parser (*parser (map xml-intern (match match-name)))))

(define parse-attribute-list
  (attribute-list-parser parse-uninterned-name))

(define (attribute-value-parser alphabet parse-reference)
  (let ((a1 (alphabet- alphabet (string->alphabet "\"")))
	(a2 (alphabet- alphabet (string->alphabet "'"))))
    (*parser
     (encapsulate (lambda (v)
		    (let ((elements (vector->list v)))
		      (if (null? elements)
			  (list "")
			  (coalesce-strings! elements))))
       (alt (sbracket "attribute value" "\"" "\""
	      (* (alt (match (+ (alphabet a1)))
		      parse-reference)))
	    (sbracket "attribute value" "'" "'"
	      (* (alt (match (+ (alphabet a2)))
		      parse-reference))))))))

(define parse-entity-value		;[9]
  (attribute-value-parser
   (alphabet- alphabet:xml-char (string->alphabet "%&"))
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
	 (attribute-value-parser alphabet:char-data
				 parse-reference-deferred)))
    (*parser
     (map normalize-attribute-value
	  (require-success "Malformed attribute value"
	    parser)))))

;;;; Normalization

(define (normalize-attribute-value elements)
  (coalesce-strings!
   (reverse!
    (let loop ((elements elements) (result '()))
      (if (pair? elements)
	  (let ((element (car elements))
		(elements (cdr elements)))
	    (if (string? element)
		(let ((buffer
		       (string->parser-buffer
			(normalize-line-endings element))))
		  (let normalize-string
		      ((port (open-output-string))
		       (result result))
		    (let* ((p (get-parser-buffer-pointer buffer))
			   (char (read-parser-buffer-char buffer)))
		      (case char
			((#f)
			 (loop elements
			       (cons (get-output-string port) result)))
			((#\tab #\newline #\return)
			 (write-char #\space port)
			 (normalize-string port result))
			((#\&)
			 (set-parser-buffer-pointer! buffer p)
			 (let ((v (parse-char-reference buffer)))
			   (if v
			       (begin
				 (write-string (vector-ref v 0) port)
				 (normalize-string port result))
			       (normalize-string
				(open-output-string)
				(let ((name
				       (vector-ref
					(parse-entity-reference-name buffer)
					0))
				      (result
				       (cons (get-output-string port) result)))
				  (let ((v (dereference-entity name #t p)))
				    (if v
					(expand-entity-value name p
					  (lambda ()
					    (loop v result)))
					(cons (make-xml-entity-ref name)
					      result))))))))
			(else
			 (write-char char port)
			 (normalize-string port result))))))
		(loop elements (cons element result))))
	  result)))))

(define (trim-attribute-whitespace string)
  (call-with-output-string
   (lambda (port)
     (let ((string (string-trim string)))
       (let ((end (string-length string)))
	 (let loop ((start 0))
	   (if (fix:< start end)
	       (let ((regs
		      (re-substring-search-forward "  +" string start end)))
		 (if regs
		     (begin
		       (write-substring string
					start
					(re-match-start-index 0 regs)
					port)
		       (write-char #\space port)
		       (loop (re-match-end-index 0 regs)))
		     (write-substring string start end port))))))))))

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
	  (let ((n
		 (let loop ((start 0) (n 0))
		   (let ((index
			  (substring-find-next-char string start end
						    #\return)))
		     (if index
			 (loop (step-over-eol index)
			       (fix:+ n (fix:+ (fix:- index start) 1)))
			 (fix:+ n (fix:- end start)))))))
	    (let ((string* (make-string n)))
	      (let loop ((start 0) (start* 0))
		(let ((index
		       (substring-find-next-char string start end
						 #\return)))
		  (if index
		      (let ((start*
			     (substring-move! string start index
					      string* start*)))
			(string-set! string* start* #\newline)
			(loop (step-over-eol index)
			      (fix:+ start* 1)))
		      (substring-move! string start end string* start*))))
	      string*))))
      (if (and (not (default-object? always-copy?))
	       always-copy?)
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
    (if (simple-attribute-value? value)
	(car value)
	(begin
	  (set! *parameter-entities* 'STOP)
	  (set! *general-entities* 'STOP)
	  (make-xml-parameter-entity-ref name)))))

(define (find-parameter-entity name)
  (let loop ((entities *parameter-entities*))
    (and (pair? entities)
	 (if (eq? (xml-parameter-!entity-name (car entities)) name)
	     (car entities)
	     (loop (cdr entities))))))

(define *parameter-entities*)

;;;; General parsed entities

(define (dereference-entity name in-attribute? p)
  (if (eq? *general-entities* 'STOP)
      #f
      (begin
	(if (assq name *entity-expansion-nesting*)
	    (perror p "Circular entity reference" name))
	(let ((entity (find-entity name)))
	  (if entity
	      (begin
		(if (xml-unparsed-!entity? entity)
		    (perror p "Reference to unparsed entity" name))
		(let ((value (xml-!entity-value entity)))
		  (cond ((xml-external-id? value) #f)
			(in-attribute? value)
			((simple-attribute-value? value)
			 (reparse-entity-value-string name (car value) p))
			(else
			 (if (or *standalone?* *internal-dtd?*)
			     (perror p "Reference to partially-defined entity"
				     name))
			 #f))))
	      (begin
		(if (or *standalone?* *internal-dtd?*)
		    (perror p "Reference to undefined entity" name))
		#f))))))

(define (reparse-entity-value-string name string p)
  (let ((v
	 (expand-entity-value name p
	   (lambda ()
	     ((*parser (complete parse-content))
	      (string->parser-buffer string))))))
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
	 (alt (map xml-intern (match "EMPTY"))
	      (map xml-intern (match "ANY"))
	      ;;[51]
	      (encapsulate vector->list
		(with-pointer p
		  (seq "("
		       S?
		       (map string->symbol (match "#PCDATA"))
		       (alt (seq S? ")")
			    (seq (* (seq S? "|" S?
					 parse-required-element-name))
				 S?
				 ")*")

			    (sexp
			     (lambda (buffer)
			       buffer
			       (perror p "Unterminated !ELEMENT type")))))))
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
				   (pair? default)
				   (simple-attribute-value? (cdr default)))
			      (list (car default)
				    (trim-attribute-whitespace (cadr default)))
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
   (alt (map xml-intern
	     ;;[55,56]
	     (match (alt "CDATA" "IDREFS" "IDREF" "ID"
			 "ENTITY" "ENTITIES" "NMTOKENS" "NMTOKEN")))
	;;[58]
	(encapsulate vector->list
	  (bracket "notation type"
	      (seq (map xml-intern (match "NOTATION"))
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
	    parse-required-name-token
	    (* (seq S? "|" S? parse-required-name-token))
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
  (string-parser "system literal" alphabet:xml-char))

(define parse-public-id-literal		;[12,13]
  (string-parser "public-ID literal"
		 (char-set->alphabet
		  (char-set-union
		   char-set:alphanumeric
		   (string->char-set " \r\n-'()+,./:=?;!*#@$_%")))))

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
  (let ((a1 (alphabet- alphabet:xml-char (string->alphabet "%\"'>")))
	(a2 (alphabet- alphabet:xml-char (string->alphabet "\"")))
	(a3 (alphabet- alphabet:xml-char (string->alphabet "'"))))
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
			(alt (* (alphabet a1))
			     (seq (char #\") (* (alphabet a2)) (char #\"))
			     (seq (char #\') (* (alphabet a3)) (char #\'))))
		       parse-parameter-entity-reference))
	       (match ">"))))))))))

(define (reparse-text v parser description ptr)
  (let ((v (coalesce-elements v)))
    (if (and (fix:= (vector-length v) 1)
	     (string? (vector-ref v 0)))
	(let ((v*
	       (fluid-let ((*external-expansion?* #t))
		 (parser (string->parser-buffer (vector-ref v 0))))))
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
	 (external-decl-parser (*matcher (seq "<!ENTITY"
					      S
					      (? (seq "%" S))))
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
  (terminated-region-matcher "ignore section" alphabet:xml-char
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
