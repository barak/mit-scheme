#| -*-Scheme-*-

$Id: xml-parser.scm,v 1.21 2003/02/14 18:28:38 cph Exp $

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

(define (coalesce-strings! elements)
  (do ((elements elements (cdr elements)))
      ((not (pair? elements)))
    (if (and (string? (car elements))
	     (pair? (cdr elements))
	     (string? (cadr elements)))
	(begin
	  (set-car! elements
		    (string-append (car elements)
				   (cadr elements)))
	  (set-cdr! elements (cddr elements)))))
  elements)

(define (coalesce-elements elements)
  (if (there-exists? elements xml-uninterpreted?)
      (make-xml-uninterpreted
       (apply string-append
	      (map (lambda (element)
		     (if (xml-uninterpreted? element)
			 (xml-uninterpreted-text element)
			 element))
		   elements)))
      (apply string-append elements)))

(define (parse-coalesced-element parser elements description ptr)
  (let ((value (coalesce-elements elements)))
    (if (string? value)
	(let ((v (parser (string->parser-buffer value))))
	  (if (not v)
	      (perror ptr (string-append "Malformed " description) value))
	  v)
	(vector value))))

(define (string-parser description alphabet)
  (let ((a1 (alphabet- alphabet (string->alphabet "\"")))
	(a2 (alphabet- alphabet (string->alphabet "'"))))
    (*parser
     (alt (sbracket description "\"" "\"" (match (* (alphabet a1))))
	  (sbracket description "'" "'" (match (* (alphabet a2))))))))

(define alphabet:alphabetic (char-set->alphabet char-set:alphabetic))
(define alphabet:numeric (char-set->alphabet char-set:numeric))
(define alphabet:alphanumeric (char-set->alphabet char-set:alphanumeric))

;;;; Top level

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
		  (*pi-handlers* pi-handlers))
	(let ((declaration (one-value (parse-declaration buffer))))
	  (set! *standalone?*
		(and declaration
		     (equal? (xml-declaration-standalone declaration)
			     "yes")))
	  (let* ((misc-1 (one-value (parse-misc buffer)))
		 (dtd (one-value (parse-dtd buffer)))
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
(define *pi-handlers*)

(define parse-misc			;[27]
  (*parser
   (encapsulate vector->list
     (* (top-level
	 (alt parse-comment
	      parse-pi:misc
	      (map normalize-line-endings (match S))))))))

(define (xml-declaration-parser description allow-standalone?)
  (*parser
   (top-level
    (with-pointer p
      (encapsulate
	  (lambda (v)
	    (transform-declaration (vector-ref v 0) allow-standalone? p))
	(sbracket description "<?xml" "?>"
	  parse-attribute-list))))))

(define parse-declaration		;[23,24,32,80]
  (xml-declaration-parser "XML declaration" #t))

(define parse-text-decl			;[77]
  (xml-declaration-parser "XML text declaration" #f))

(define (transform-declaration attributes allow-standalone? p)
  (let ((finish
	 (lambda (version encoding standalone)
	   (if (not (and (string? version)
			 (match-xml-version (string->parser-buffer version))))
	       (perror p "Malformed XML version" version))
	   (if (and encoding
		    (not (and (string? encoding)
			      (match-encoding
			       (string->parser-buffer encoding)))))
	       (perror p "Malformed encoding attribute" encoding))
	   (if standalone
	       (begin
		 (if (not allow-standalone?)
		     (perror
		      p
		      "Standalone attribute not allowed in text declaration"))
		 (if (not (member standalone '("yes" "no")))
		     (perror p "Malformed standalone attribute" standalone))))
	   (make-xml-declaration version encoding standalone))))
    (let loop
	((attributes attributes)
	 (names '("version" "encoding" "standalone"))
	 (results '()))
      (if (pair? names)
	  (if (pair? attributes)
	      (if (string=? (symbol-name (caar attributes)) (car names))
		  (loop (cdr attributes)
			(cdr names)
			(cons (cdar attributes) results))
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
	    (finish (caddr results) (cadr results) (car results)))))))

(define match-xml-version		;[26]
  (let ((a (alphabet+ alphabet:alphanumeric (string->alphabet "_.:-"))))
    (*matcher (complete (+ (alphabet a))))))

(define match-encoding			;[81]
  (let ((a (alphabet+ alphabet:alphanumeric (string->alphabet "_.-"))))
    (*matcher
     (complete
      (seq (alphabet alphabet:alphabetic)
	   (* (alphabet a)))))))

;;;; Elements

(define (parse-element buffer)		;[39]
  (let ((p (get-parser-buffer-pointer buffer)))
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
				   (list-transform-negative
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
		 '())))))))

(define parse-start-tag			;[40,44]
  (*parser
   (top-level
    (bracket "start tag"
	(seq (noise (string "<")) parse-name)
	(match (alt (string ">") (string "/>")))
      parse-attribute-list))))

(define parse-end-tag			;[42]
  (*parser
   (top-level
    (sbracket "end tag" "</" ">"
      parse-required-name
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

;;;; Processing instructions

(define (pi-parser valid-content?) ;[16,17]
  (let ((description "processing instructions")
	(start "<?")
	(end "?>"))
    (let ((parse-body
	   (terminated-region-parser description alphabet:xml-char end)))
      (*parser
       (transform
	   (lambda (v)
	     (let ((name (vector-ref v 0))
		   (text (vector-ref v 1)))
	       (let ((entry (assq name *pi-handlers*)))
		 (if entry
		     (let ((content ((cadr entry) text)))
		       (if (not (list-of-type? content valid-content?))
			   (error "Illegal output from XML processor:" name))
		       (list->vector content))
		     (vector (make-xml-processing-instructions name text))))))
	 (sbracket description start end
	   (with-pointer p
	     (map (lambda (name)
		    (if (string-ci=? (symbol-name name) "xml")
			(perror p "Illegal PI name" name))
		    name)
		  parse-required-name))
	   parse-body))))))

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

;;;; Names and references

(define parse-required-name
  (*parser (require-success "Malformed XML name" parse-name)))

(define parse-name			;[5]
  (*parser (map xml-intern (match match-name))))

(define (match-name buffer)
  (and (match-utf8-char-in-alphabet buffer alphabet:name-initial)
       (let loop ()
	 (if (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
	     (loop)
	     #t))))

(define parse-required-name-token
  (*parser (require-success "Malformed XML name token" parse-name-token)))

(define parse-name-token		;[7]
  (*parser (map xml-intern (match match-name-token))))

(define (match-name-token buffer)
  (and (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
       (let loop ()
	 (if (match-utf8-char-in-alphabet buffer alphabet:name-subsequent)
	     (loop)
	     #t))))

(define parse-char-reference		;[66]
  (let ((make-ref
	 (lambda (s r p)
	   (let ((n (string->number s r)))
	     (if (not (code-point-in-alphabet? n alphabet:xml-char))
		 (perror p "Disallowed Unicode code point" n))
	     (code-point->utf8-string n)))))
    (*parser
     (with-pointer p
       (sbracket "character reference" "&#" ";"
	 (alt (map (lambda (s) (make-ref s 10 p))
		   (match (+ (alphabet alphabet:numeric))))
	      (seq (noise (string "x"))
		   (map (lambda (s) (make-ref s 16 p))
			(match (+ (char-set "0-9a-fA-f")))))))))))

(define parse-reference			;[67]
  (*parser
   (alt parse-char-reference
	parse-entity-reference)))

(define parse-reference-deferred
  (*parser
   (match
    (seq (string "&")
	 (alt (seq (string "#")
		   (alt (+ (alphabet alphabet:numeric))
			(seq (string "x") (+ (char-set "0-9a-fA-f")))))
	      match-name)
	 (string ";")))))

(define parse-entity-reference		;[68]
  (*parser
   (with-pointer p
     (transform (lambda (v) (dereference-entity (vector-ref v 0) p))
       (sbracket "entity reference" "&" ";"
	 parse-required-name)))))

(define parse-entity-reference-deferred
  (*parser (match (seq (string "&") match-name (string ";")))))

(define parse-parameter-entity-reference ;[69]
  (*parser
   (map dereference-parameter-entity
	(sbracket "parameter-entity reference" "%" ";"
	  parse-required-name))))

;;;; Attributes

(define parse-attribute-list
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
	    S?)))))

(define parse-attribute			;[41,25]
  (*parser
   (encapsulate (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
     (seq S
	  parse-name
	  S?
	  (require-success "Missing attribute separator"
	    (noise (string "=")))
	  S?
	  parse-attribute-value))))

(define (attribute-value-parser alphabet parse-reference)
  (let ((a1 (alphabet- alphabet (string->alphabet "\"")))
	(a2 (alphabet- alphabet (string->alphabet "'"))))
    (*parser
     (encapsulate (lambda (v) (coalesce-elements (vector->list v)))
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
	 parse-parameter-entity-reference))))

(define parse-attribute-value		;[10]
  (let ((parser
	 (attribute-value-parser alphabet:char-data
				 parse-reference-deferred)))
    (*parser
     (with-pointer p
       (map (lambda (value) (normalize-attribute-value value p))
	    (require-success "Malformed attribute value"
	      parser))))))

;;;; Normalization

(define (normalize-attribute-value value p)
  (call-with-output-string
   (lambda (port)
     (let normalize-value ((value value))
       (if (string? value)
	   (let ((buffer
		  (string->parser-buffer (normalize-line-endings value))))
	     (let loop ()
	       (let ((char (peek-parser-buffer-char buffer)))
		 (cond ((not char)
			unspecific)
		       ((or (char=? char #\tab)
			    (char=? char #\newline))
			(write-char #\space port)
			(read-parser-buffer-char buffer)
			(loop))
		       ((char=? char #\&)
			(normalize-value
			 (vector-ref (parse-reference buffer)
				     0))
			(loop))
		       (else
			(write-char char port)
			(read-parser-buffer-char buffer)
			(loop))))))
	   (perror p "Reference to external entity in attribute"))))))

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
    (if (or (string? value)
	    (xml-uninterpreted? value))
	value
	(begin
	  (set! *parameter-entities* 'STOP)
	  (set! *general-entities* 'STOP)
	  (make-xml-uninterpreted
	   (string-append "%" (symbol-name name) ";"))))))

(define (find-parameter-entity name)
  (let loop ((entities *parameter-entities*))
    (and (pair? entities)
	 (if (eq? (xml-parameter-!entity-name (car entities)) name)
	     (car entities)
	     (loop (cdr entities))))))

(define *parameter-entities*)

;;;; General parsed entities

(define (dereference-entity name p)
  (if (eq? *general-entities* 'STOP)
      (uninterpreted-entity name)
      (begin
	(if (assq name *entity-expansion-nesting*)
	    (perror p "Circular entity reference" name))
	(let ((entity (find-entity name)))
	  (if entity
	      (begin
		(if (xml-unparsed-!entity? entity)
		    (perror p "Reference to unparsed entity" name))
		(let ((value (xml-!entity-value entity)))
		  (cond ((string? value) (expand-entity-value name value p))
			((xml-uninterpreted? value) (vector value))
			(else (uninterpreted-entity name)))))
	      (begin
		(if (or *standalone?* *internal-dtd?*)
		    (perror p "Reference to undefined entity" name))
		(uninterpreted-entity name)))))))

(define (expand-entity-value name value p)
  (let ((buffer (string->parser-buffer value)))
    (let ((v
	   (fluid-let ((*entity-expansion-nesting*
			(cons (cons name p) *entity-expansion-nesting*)))
	     (parse-content buffer))))
      (if (or (not v) (peek-parser-buffer-char buffer))
	  (perror p "Malformed entity reference" value))
      v)))

(define (find-entity name)
  (let loop ((entities *general-entities*))
    (and (pair? entities)
	 (if (eq? (if (xml-!entity? (car entities))
		      (xml-!entity-name (car entities))
		      (xml-unparsed-!entity-name (car entities)))
		  name)
	     (car entities)
	     (loop (cdr entities))))))

(define (uninterpreted-entity name)
  (vector (make-xml-uninterpreted (string-append "&" (symbol-name name) ";"))))

(define (predefined-entities)
  (list (make-xml-!entity (xml-intern "lt") "&#60;")
	(make-xml-!entity (xml-intern "gt") ">")
	(make-xml-!entity (xml-intern "amp") "&#38;")
	(make-xml-!entity (xml-intern "quot") "\"")
	(make-xml-!entity (xml-intern "apos") "'")))

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
	       parse-required-name
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
		(parse-coalesced-element parse-external-subset-decl
					 (list " " (vector-ref v 0) " ")
					 "parameter-entity value"
					 p))
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
		  (alt (encapsulate (lambda (v) (cons 'ALT (vector->list v)))
			 (seq parse-cp
			      (+ (seq S?
				      (noise (string "|"))
				      S?
				      parse-cp))))
		       (encapsulate (lambda (v) (cons 'SEQ (vector->list v)))
			 (seq parse-cp
			      (* (seq S?
				      (noise (string ","))
				      S?
				      parse-cp)))))
		  S?)
		(? (match (char-set "?*+")))))))

       (parse-cp			;[48]
	 (*parser
	  (alt (encapsulate encapsulate-suffix
		 (seq parse-name
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
	 (lambda (v) (make-xml-!element (vector-ref v 0) (vector-ref v 1)))
       (sbracket "element declaration" "<!ELEMENT" ">"
	 S
	 parse-required-name
	 S
	 ;;[46]
	 (alt (map xml-intern (match (string "EMPTY")))
	      (map xml-intern (match (string "ANY")))
	      ;;[51]
	      (encapsulate (lambda (v) (cons 'MIX (vector->list v)))
		(with-pointer p
		  (seq (noise (string "("))
		       S?
		       (noise (string "#PCDATA"))
		       (alt (seq S?
				 (noise (string ")")))
			    (seq (* (seq S?
					 (noise (string "|"))
					 S?
					 parse-required-name))
				 S?
				 (noise (string ")*")))

			    (sexp
			     (lambda (buffer)
			       buffer
			       (perror p "Unterminated !ELEMENT type")))))))
	      parse-children))))))

(define parse-!attlist			;[52,53]
  (*parser
   (encapsulate
       (lambda (v) (make-xml-!attlist (vector-ref v 0) (vector-ref v 1)))
     (sbracket "attribute-list declaration" "<!ATTLIST" ">"
       S
       parse-required-name
       (encapsulate vector->list
	 (* (encapsulate
		(lambda (v)
		  (let ((name (vector-ref v 0))
			(type (vector-ref v 1))
			(default (vector-ref v 2)))
		    (list name type
			  (if (and (not (eq? type (xml-intern "CDATA")))
				   (pair? default))
			      (list (car default)
				    (trim-attribute-whitespace (cadr default)))
			      default))))
	      (seq S
		   parse-name
		   S
		   ;;[54,57]
		   (alt (map xml-intern
			     ;;[55,56]
			     (alt (match (string "CDATA"))
				  (match (string "IDREFS"))
				  (match (string "IDREF"))
				  (match (string "ID"))
				  (match (string "ENTITY"))
				  (match (string "ENTITIES"))
				  (match (string "NMTOKENS"))
				  (match (string "NMTOKEN"))))
			;;[58]
			(encapsulate
			    (lambda (v)
			      (cons 'NOTATION (vector->list v)))
			  (bracket "notation type"
			      (noise (seq (string "NOTATION") S (string "(")))
			      (noise (string ")"))
			    S?
			    parse-required-name
			    (* (seq (noise (seq S? (string "|") S?))
				    parse-required-name))
			    S?))
			;;[59]
			(encapsulate
			    (lambda (v)
			      (cons 'ENUMERATED (vector->list v)))
			  (sbracket "enumerated type" "(" ")"
			    S?
			    parse-required-name-token
			    (* (seq S?
				    (noise (string "|"))
				    S?
				    parse-required-name-token))
			    S?)))
		   S
		   ;;[60]
		   (alt (map xml-intern
			     (alt (match (string "#REQUIRED"))
				  (match (string "#IMPLIED"))))
			(encapsulate vector->list
			  (seq (map xml-intern
				    (match (string "#FIXED")))
			       S
			       parse-attribute-value))
			(map (lambda (v) (list 'DEFAULT v))
			     parse-attribute-value))))))
       S?))))

(define parse-!entity			;[70,71,72,73,74,76]
  (*parser
   (sbracket "entity declaration" "<!ENTITY" ">"
     S
     (alt (encapsulate
	      (lambda (v)
		(make-parameter-entity (vector-ref v 0) (vector-ref v 1)))
	    (seq (noise (string "%"))
		 S
		 parse-required-name
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
	    (seq parse-required-name
		 S
		 (alt parse-entity-value
		      (seq parse-external-id
			   (? (seq S
				   (noise (string "NDATA"))
				   S
				   parse-required-name)))))))
     S?)))

(define parse-!notation			;[82,83]
  (*parser
   (encapsulate
       (lambda (v) (make-xml-!notation (vector-ref v 0) (vector-ref v 1)))
     (sbracket "notation declaration" "<!NOTATION" ">"
       S
       parse-required-name
       S
       (alt parse-external-id
	    (encapsulate
		(lambda (v) (make-xml-external-id (vector-ref v 0) #f))
	      (seq (noise (string "PUBLIC"))
		   S
		   parse-public-id-literal)))
       S?))))

(define parse-external-id		;[75]
  (*parser
   (with-pointer p
     (alt (encapsulate
	      (lambda (v)
		(make-external-id #f (vector-ref v 0) p))
	    (seq (noise (string "SYSTEM"))
		 S
		 parse-system-literal))
	  (encapsulate
	      (lambda (v)
		(make-external-id (vector-ref v 0) (vector-ref v 1) p))
	    (seq (noise (string "PUBLIC"))
		 S
		 parse-public-id-literal
		 S
		 parse-system-literal))))))

(define parse-system-literal		;[11]
  (string-parser "system literal" alphabet:xml-char))

(define parse-public-id-literal		;[12,13]
  (string-parser "public-ID literal"
		 (alphabet+ alphabet:alphanumeric
			    (string->alphabet " \r\n-'()+,./:=?;!*#@$_%"))))

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
	       (parse-coalesced-element parse-decl
					(vector->list v)
					"markup declaration"
					p))
	   (seq
	    (match prefix)
	    (require-success "Malformed markup declaration"
	      (seq
	       (* (alt (match
			(alt (* (alphabet a1))
			     (seq (char #\") (* (alphabet a2)) (char #\"))
			     (seq (char #\') (* (alphabet a3)) (char #\'))))
		       parse-parameter-entity-reference))
	       (match (string ">")))))))))))

(define parse-external-markup-decl	;[29]
  (let ((parse-!element
	 (external-decl-parser (*matcher (seq (string "<!ELEMENT") S))
			       parse-!element))
	(parse-!attlist
	 (external-decl-parser (*matcher (seq (string "<!ATTLIST") S))
			       parse-!attlist))
	(parse-!entity
	 (external-decl-parser (*matcher (seq (string "<!ENTITY")
					      S
					      (? (seq (string "%") S))))
			       parse-!entity))
	(parse-!notation
	 (external-decl-parser (*matcher (seq (string "<!NOTATION") S))
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
		   (string "INCLUDE")
		   S?
		   (string "[")))
       (noise (string conditional-end))
     parse-external-subset-decl)))

(define parse-!ignore-section		;[63]
  (*parser
   (bracket "!IGNORE section"
       (noise (seq (string conditional-start)
		   S?
		   (string "IGNORE")
		   S?
		   (string "[")))
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
	   (parse-coalesced-element parse-conditional-section
				    (vector->list v)
				    "conditional section"
				    p))
       (bracket "parameterized conditional section"
	   (seq (match (string conditional-start))
		S?
		parse-parameter-entity-reference
		S?
		(match (string "[")))
	   (match (string conditional-end))
	 (match (* match-!ignore-contents)))))))

;;; Edwin Variables:
;;; Eval: (scheme-indent-method 'encapsulate 1)
;;; Eval: (scheme-indent-method 'transform 1)
;;; Eval: (scheme-indent-method 'require-success 1)
;;; Eval: (scheme-indent-method 'bracket 3)
;;; Eval: (scheme-indent-method 'sbracket 3)
;;; End:
