#| -*-Scheme-*-

$Id: xml-output.scm,v 1.28 2003/09/26 19:39:01 cph Exp $

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

;;;; XML output

(declare (usual-integrations))

(define (write-xml xml port . options)
  (write-xml-1 xml port options))

(define (write-xml-file xml pathname . options)
  (call-with-output-file pathname
    (lambda (port)
      (write-xml-1 xml port options))))

(define (xml->string xml . options)
  (call-with-output-string
   (lambda (port)
     (write-xml-1 xml port options))))

(define (xml->wide-string xml . options)
  (call-with-wide-output-string
   (lambda (port)
     (write-xml-1 xml port options))))

(define (write-xml-1 xml port options)
  (%write-xml xml (make-ctx port options)))

(define-structure (ctx (type-descriptor <ctx>)
		       (keyword-constructor %make-ctx)
		       (print-procedure
			(standard-unparser-method 'XML-OUTPUT-CONTEXT #f)))
  (port #f read-only #t)
  (indent-attributes? #f read-only #t)
  (indent-dtd? #f read-only #t))

(define (make-ctx port options)
  (apply %make-ctx 'PORT port options))

(define (emit-char char ctx)
  (let ((port (ctx-port ctx)))
    (if (fix:< (char->integer char) #x80)
	(write-char char port)
	(begin
	  (write-string "&#x" port)
	  (write-string (number->string (char->integer char) 16) port)
	  (write-string ";" port)))))

(define (emit-string string ctx)
  (write-string string (ctx-port ctx)))

(define (emit-newline ctx)
  (newline (ctx-port ctx)))

(define (ctx-start-col ctx)
  (output-port/column (ctx-port ctx)))

(define (ctx-x-size ctx)
  (output-port/x-size (ctx-port ctx)))

(define (dtd-start-col ctx)
  (and (ctx-indent-dtd? ctx)
       (ctx-start-col ctx)))

(define-generic %write-xml (object ctx))

(define-method %write-xml ((document <xml-document>) ctx)
  (if (xml-document-declaration document)
      (%write-xml (xml-document-declaration document) ctx))
  (for-each (lambda (object) (%write-xml object ctx))
	    (xml-document-misc-1 document))
  (if (xml-document-dtd document)
      (%write-xml (xml-document-dtd document) ctx))
  (for-each (lambda (object) (%write-xml object ctx))
	    (xml-document-misc-2 document))
  (%write-xml (xml-document-root document) ctx)
  (for-each (lambda (object) (%write-xml object ctx))
	    (xml-document-misc-3 document)))

(define-method %write-xml ((declaration <xml-declaration>) ctx)
  (emit-string "<?xml version=\"" ctx)
  (emit-string (xml-declaration-version declaration) ctx)
  (emit-string "\"" ctx)
  (if (xml-declaration-encoding declaration)
      (begin
	(emit-string " encoding=\"" ctx)
	(emit-string (xml-declaration-encoding declaration) ctx)
	(emit-string "\"" ctx)))
  (if (xml-declaration-standalone declaration)
      (begin
	(emit-string " standalone=\"" ctx)
	(emit-string (xml-declaration-standalone declaration) ctx)
	(emit-string "\"" ctx)))
  (emit-string "?>" ctx))

(define-method %write-xml ((element <xml-element>) ctx)
  (let ((name (xml-element-name element))
	(contents (xml-element-contents element)))
    (emit-string "<" ctx)
    (write-xml-name name ctx)
    (write-xml-attributes (xml-element-attributes element)
			  (if (pair? contents) 1 3)
			  ctx)
    (if (pair? contents)
	(begin
	  (emit-string ">" ctx)
	  (for-each (lambda (content) (%write-xml content ctx))
		    contents)
	  (emit-string "</" ctx)
	  (write-xml-name name ctx)
	  (emit-string ">" ctx))
	(emit-string " />" ctx))))

(define-method %write-xml ((comment <xml-comment>) ctx)
  (emit-string "<!--" ctx)
  (emit-string (xml-comment-text comment) ctx)
  (emit-string "-->" ctx))

(define-method %write-xml ((pi <xml-processing-instructions>) ctx)
  (emit-string "<?" ctx)
  (write-xml-name (xml-processing-instructions-name pi) ctx)
  (emit-string " " ctx)
  (emit-string (xml-processing-instructions-text pi) ctx)
  (emit-string "?>" ctx))

(define-method %write-xml ((dtd <xml-dtd>) ctx)
  ;;root external internal
  (emit-string "<!DOCTYPE " ctx)
  (let ((col (dtd-start-col ctx)))
    (write-xml-name (xml-dtd-root dtd) ctx)
    (if (xml-dtd-external dtd)
	(write-xml-external-id (xml-dtd-external dtd) col ctx))
    (if (pair? (xml-dtd-internal dtd))
	(begin
	  (if (xml-dtd-external dtd)
	      (emit-newline ctx)
	      (emit-string " " ctx))
	  (emit-string "[" ctx)
	  (emit-newline ctx)
	  (for-each (lambda (element)
		      (%write-xml element ctx)
		      (emit-newline ctx))
		    (xml-dtd-internal dtd))
	  (emit-string "]" ctx)))
    (emit-string ">" ctx)))

(define-method %write-xml ((decl <xml-!element>) ctx)
  (emit-string "<!ELEMENT " ctx)
  (write-xml-name (xml-!element-name decl) ctx)
  (emit-string " " ctx)
  (let ((type (xml-!element-content-type decl)))
    (cond ((symbol? type)
	   (emit-string (string-upcase (symbol-name type)) ctx))
	  ((and (pair? type) (eq? (car type) '|#PCDATA|))
	   (emit-string "(#PCDATA" ctx)
	   (if (pair? (cdr type))
	       (begin
		 (for-each (lambda (name)
			     (emit-string "|" ctx)
			     (write-xml-name name ctx))
			   (cdr type))
		 (emit-string ")*" ctx))
	       (emit-string ")" ctx)))
	  (else
	   (letrec
	       ((write-children
		 (lambda (type)
		   (handle-iterator type
		     (lambda (type)
		       (if (not (and (pair? type)
				     (list? (cdr type))))
			   (lose))
		       (emit-string "(" ctx)
		       (write-cp (cadr type))
		       (for-each
			(let ((sep (if (eq? (car type) 'alt) "|" ",")))
			  (lambda (type)
			    (emit-string sep ctx)
			    (write-cp type)))
			(cddr type))
		       (emit-string ")" ctx)))))
		(write-cp
		 (lambda (type)
		   (handle-iterator type
		     (lambda (type)
		       (if (xml-name? type)
			   (write-xml-name type ctx)
			   (write-children type))))))
		(handle-iterator
		 (lambda (type procedure)
		   (if (and (pair? type)
			    (memv (car type) '(#\? #\* #\+))
			    (pair? (cdr type))
			    (null? (cddr type)))
		       (begin
			 (procedure (cadr type))
			 (emit-char (car type) ctx))
		       (procedure type))))
		(lose
		 (lambda () 
		   (error "Malformed !ELEMENT content type:" type))))
	     (write-children type)))))
  (emit-string ">" ctx))

(define-method %write-xml ((decl <xml-!attlist>) ctx)
  (emit-string "<!ATTLIST " ctx)
  (write-xml-name (xml-!attlist-name decl) ctx)
  (let ((definitions (xml-!attlist-definitions decl))
	(write-definition
	 (lambda (definition)
	   (write-xml-name (car definition) ctx)
	   (emit-string " " ctx)
	   (let ((type (cadr definition)))
	     (cond ((symbol? type)
		    (emit-string (string-upcase (symbol-name type)) ctx))
		   ((and (pair? type) (eq? (car type) '|NOTATION|))
		    (emit-string "NOTATION (" ctx)
		    (if (pair? (cdr type))
			(begin
			  (write-xml-name (cadr type) ctx)
			  (for-each (lambda (name)
				      (emit-string "|" ctx)
				      (write-xml-name name ctx))
				    (cddr type))))
		    (emit-string ")" ctx))
		   ((and (pair? type) (eq? (car type) 'enumerated))
		    (emit-string "(" ctx)
		    (if (pair? (cdr type))
			(begin
			  (write-xml-nmtoken (cadr type) ctx)
			  (for-each (lambda (nmtoken)
				      (emit-string "|" ctx)
				      (write-xml-nmtoken nmtoken ctx))
				    (cddr type))))
		    (emit-string ")" ctx))
		   (else
		    (error "Malformed !ATTLIST type:" type))))
	   (emit-string " " ctx)
	   (let ((default (caddr definition)))
	     (cond ((or (eq? default '|#REQUIRED|)
			(eq? default '|#IMPLIED|))
		    (emit-string (symbol-name default) ctx))
		   ((and (pair? default) (eq? (car default) '|#FIXED|))
		    (emit-string (symbol-name (car default)) ctx)
		    (emit-string " " ctx)
		    (write-xml-attribute-value (cdr default) ctx))
		   ((and (pair? default) (eq? (car default) 'default))
		    (write-xml-attribute-value (cdr default) ctx))
		   (else
		    (error "Malformed !ATTLIST default:" default)))))))
    (if (pair? definitions)
	(if (pair? (cdr definitions))
	    (for-each (lambda (definition)
			(emit-newline ctx)
			(emit-string "          " ctx)
			(write-definition definition))
		      definitions)
	    (begin
	      (emit-string " " ctx)
	      (write-definition (car definitions))))))
  (emit-string ">" ctx))

(define-method %write-xml ((decl <xml-!entity>) ctx)
  (emit-string "<!ENTITY " ctx)
  (let ((col (dtd-start-col ctx)))
    (write-xml-name (xml-!entity-name decl) ctx)
    (emit-string " " ctx)
    (write-entity-value (xml-!entity-value decl) col ctx)
    (emit-string ">" ctx)))

(define-method %write-xml ((decl <xml-unparsed-!entity>) ctx)
  (emit-string "<!ENTITY " ctx)
  (let ((col (dtd-start-col ctx)))
    (write-xml-name (xml-unparsed-!entity-name decl) ctx)
    (emit-string " " ctx)
    (write-xml-external-id (xml-unparsed-!entity-id decl) col ctx)
    (emit-string " NDATA " ctx)
    (write-xml-name (xml-unparsed-!entity-notation decl) ctx)
    (emit-string ">" ctx)))

(define-method %write-xml ((decl <xml-parameter-!entity>) ctx)
  (emit-string "<!ENTITY " ctx)
  (let ((col (dtd-start-col ctx)))
    (emit-string "% " ctx)
    (write-xml-name (xml-parameter-!entity-name decl) ctx)
    (emit-string " " ctx)
    (write-entity-value (xml-parameter-!entity-value decl) col ctx)
    (emit-string ">" ctx)))

(define-method %write-xml ((decl <xml-!notation>) ctx)
  (emit-string "<!NOTATION " ctx)
  (let ((col (dtd-start-col ctx)))
    (write-xml-name (xml-!notation-name decl) ctx)
    (emit-string " " ctx)
    (write-xml-external-id (xml-!notation-id decl) col ctx)
    (emit-string ">" ctx)))

(define-method %write-xml ((string <string>) ctx)
  (write-escaped-string string
			'((#\< . "&lt;")
			  (#\& . "&amp;"))
			ctx))

(define-method %write-xml ((ref <xml-entity-ref>) ctx)
  (emit-string "&" ctx)
  (write-xml-name (xml-entity-ref-name ref) ctx)
  (emit-string ";" ctx))

(define-method %write-xml ((ref <xml-parameter-entity-ref>) ctx)
  (emit-string "%" ctx)
  (write-xml-name (xml-parameter-entity-ref-name ref) ctx)
  (emit-string ";" ctx))

(define (write-xml-attributes attrs suffix-cols ctx)
  (let ((col
	 (and (ctx-indent-attributes? ctx)
	      (ctx-start-col ctx))))
    (if (and col
	     (pair? attrs)
	     (pair? (cdr attrs))
	     (>= (+ col
		    (xml-attributes-columns attrs)
		    suffix-cols)
		 (ctx-x-size ctx)))
	(begin
	  (emit-char #\space ctx)
	  (write-xml-attribute (car attrs) ctx)
	  (for-each (lambda (attr)
		      (write-indent (+ col 1) ctx)
		      (write-xml-attribute attr ctx))
		    (cdr attrs)))
	(for-each (lambda (attr)
		    (emit-char #\space ctx)
		    (write-xml-attribute attr ctx))
		  attrs))))

(define (xml-attributes-columns attrs)
  (do ((attrs attrs (cdr attrs))
       (n-cols 0 (+ n-cols 1 (xml-attribute-columns (car attrs)))))
      ((not (pair? attrs)) n-cols)))

(define (write-xml-attribute attr ctx)
  (write-xml-name (xml-attribute-name attr) ctx)
  (emit-char #\= ctx)
  (write-xml-attribute-value (xml-attribute-value attr) ctx))

(define (write-xml-attribute-value value ctx)
  (emit-char #\" ctx)
  (write-xml-string value ctx)
  (emit-char #\" ctx))

(define (xml-attribute-columns attr)
  (+ (xml-name-columns (xml-attribute-name attr))
     3
     (xml-string-columns (xml-attribute-value attr))))

(define (write-xml-string string ctx)
  (write-escaped-string string
			'((#\" . "&quot;")
			  (#\< . "&lt;")
			  (#\& . "&amp;"))
			ctx))

(define (xml-string-columns string)
  (let ((n 0))
    (for-each-wide-char string
      (lambda (char)
	(set! n
	      (fix:+ n
		     (case char
		       ((#\") 6)
		       ((#\<) 4)
		       ((#\&) 5)
		       (else 1))))
	unspecific))
    n))

(define (write-xml-name name ctx)
  (emit-string (xml-name-string name) ctx))

(define (xml-name-columns name)
  (utf8-string-length (xml-name-string name)))

(define (write-xml-nmtoken nmtoken ctx)
  (emit-string (xml-nmtoken-string nmtoken) ctx))

(define (write-entity-value value col ctx)
  (if (xml-external-id? value)
      (write-xml-external-id value col ctx)
      (begin
	(emit-char #\" ctx)
	(for-each
	 (lambda (item)
	   (if (string? item)
	       (write-escaped-string item
				     '((#\" . "&quot;")
				       (#\& . "&amp;")
				       (#\% . "&#37;"))
				     ctx)
	       (%write-xml item ctx)))
	 value)
	(emit-char #\" ctx))))

(define (write-xml-external-id id col ctx)
  (let ((quoted-string
	 (lambda (string)
	   (emit-char #\" ctx)
	   (write-xml-string string ctx)
	   (emit-char #\" ctx))))
    (if (xml-external-id-id id)
	(begin
	  (write-indent col ctx)
	  (emit-string "PUBLIC " ctx)
	  (quoted-string (xml-external-id-id id))
	  (if (xml-external-id-iri id)
	      (begin
		(write-indent col ctx)
		(quoted-string (xml-external-id-iri id)))))
	(begin
	  (write-indent col ctx)
	  (emit-string "SYSTEM" ctx)
	  (emit-string " " ctx)
	  (quoted-string (xml-external-id-iri id))))))

(define (write-indent col ctx)
  (if col
      (begin
	(emit-newline ctx)
	(let ((q.r (integer-divide col 8)))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i (car q.r)))
	    (emit-char #\tab ctx))
	  (do ((i 0 (fix:+ i 1)))
	      ((fix:= i (cdr q.r)))
	    (emit-char #\space ctx))))
      (emit-char #\space ctx)))

(define (write-escaped-string string escapes ctx)
  (for-each-wide-char string
    (lambda (char)
      (let ((e (assq char escapes)))
	(if e
	    (emit-string (cdr e) ctx)
	    (emit-char char ctx))))))

(define (for-each-wide-char string procedure)
  (let ((port (open-input-string string)))
    (let loop ()
      (let ((char (read-utf8-char port)))
	(if (not (eof-object? char))
	    (begin
	      (procedure char)
	      (loop)))))))