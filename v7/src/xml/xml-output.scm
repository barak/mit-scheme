;;; -*-Scheme-*-
;;;
;;; $Id: xml-output.scm,v 1.1 2001/07/16 20:40:30 cph Exp $
;;;
;;; Copyright (c) 2001 Massachusetts Institute of Technology
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;;; 02111-1307, USA.

;;;; XML output

(declare (usual-integrations))

(define-generic write-xml (object port))

(define-method write-xml ((document xml-document-rtd) port)
  (if (xml-document-declaration document)
      (write-xml (xml-document-declaration document) port))
  (for-each (lambda (object) (write-xml object port))
	    (xml-document-misc-1 document))
  (if (xml-document-dtd document)
      (write-xml (xml-document-dtd document) port))
  (for-each (lambda (object) (write-xml object port))
	    (xml-document-misc-2 document))
  (write-xml (xml-document-root document) port)
  (for-each (lambda (object) (write-xml object port))
	    (xml-document-misc-3 document)))

(define-method write-xml ((declaration xml-declaration-rtd) port)
  (write-string "<?xml" port)
  (write-xml-attributes
   (append (list (cons (xml-intern "version")
		       (xml-declaration-version declaration)))
	   (if (xml-declaration-encoding declaration)
	       (list (cons (xml-intern "encoding")
			   (xml-declaration-encoding declaration)))
	       '())
	   (if (xml-declaration-standalone declaration)
	       (list (cons (xml-intern "standalone")
			   (xml-declaration-standalone declaration)))
	       '()))
   port)
  (write-string "?>" port))

(define-method write-xml ((element xml-element-rtd) port)
  (write-string "<" port)
  (write-xml-name (xml-element-name element) port)
  (write-xml-attributes (xml-element-attributes element) port)
  (let ((contents (xml-element-contents element)))
    (if (pair? contents)
	(begin
	  (write-string ">" port)
	  (for-each (lambda (content) (write-xml content port))
		    contents)
	  (write-string "</" port)
	  (write-xml-name (xml-element-name element) port)
	  (write-string ">" port))
	(write-string "/>" port))))

(define-method write-xml ((pi xml-processing-instructions-rtd) port)
  (write-string "<?" port)
  (write-xml-name (xml-processing-instructions-name pi) port)
  (write-string (xml-processing-instructions-text pi) port)
  (write-string "?>" port))

(define-method write-xml ((element xml-uninterpreted-rtd) port)
  ;; **** There's a quoting problem here -- char data that gets
  ;; bundled into this must be quoted prior to combination with other
  ;; elements.
  (write-string (xml-uninterpreted-text element) port))

(define-method write-xml ((dtd xml-dtd-rtd) port)
  ;;root external internal
  (write-string "<!DOCTYPE " port)
  (write-xml-name (xml-dtd-root dtd) port)
  (if (xml-dtd-external dtd)
      (begin
	(write-string " " port)
	(write-xml-external-id (xml-dtd-external dtd) port)))
  (if (pair? (xml-dtd-internal dtd))
      (begin
	(write-string " [" port)
	(newline port)
	(for-each (lambda (element)
		    (write-xml element port)
		    (newline port))
		  (xml-dtd-internal dtd))
	(write-string "]" port)))
  (write-string ">" port))

(define-method write-xml ((decl xml-!element-rtd) port)
  (write-string "<!ELEMENT " port)
  (write-xml-name (xml-!element-name decl) port)
  (write-string " " port)
  (let ((type (xml-!element-content-type decl)))
    (cond ((symbol? type)
	   (write-xml-name type port))
	  ((and (pair? type) (eq? (car type) 'MIX))
	   (write-string "(#PCDATA" port)
	   (if (pair? (cdr type))
	       (begin
		 (for-each (lambda (name)
			     (write-string "|" port)
			     (write-xml-name name port))
			   (cdr type))
		 (write-string ")*" port))
	       (write-string ")" port)))
	  (else
	   (letrec
	       ((write-children
		 (lambda (type)
		   (handle-iterator type
		     (lambda (type)
		       (if (not (and (pair? type)
				     (list? (cdr type))))
			   (lose))
		       (write-string "(" port)
		       (write-cp (cadr type))
		       (for-each
			(let ((sep (if (eq? (car type) 'ALT) "|" ",")))
			  (lambda (type)
			    (write-string sep port)
			    (write-cp type)))
			(cddr type))
		       (write-string ")" port)))))
		(write-cp
		 (lambda (type)
		   (handle-iterator type
		     (lambda (type)
		       (if (symbol? type)
			   (write-xml-name type port)
			   (write-children type))))))
		(handle-iterator
		 (lambda (type procedure)
		   (if (and (pair? type)
			    (memv (car type) '(#\? #\* #\+))
			    (pair? (cdr type))
			    (null? (cddr type)))
		       (begin
			 (procedure (cadr type))
			 (write-char (car type) port))
		       (procedure type))))
		(lose
		 (lambda () 
		   (error "Malformed !ELEMENT content type:" type))))
	     (write-children type)))))
  (write-string ">" port))

(define-method write-xml ((decl xml-!attlist-rtd) port)
  (write-string "<!ATTLIST " port)
  (write-xml-name (xml-!attlist-name decl) port)
  (let ((definitions (xml-!attlist-definitions decl))
	(write-definition
	 (lambda (definition)
	   (write-xml-name (car definition) port)
	   (write-string " " port)
	   (let ((type (cadr definition)))
	     (cond ((symbol? type)
		    (write-xml-name type port))
		   ((and (pair? type) (eq? (car type) 'NOTATION))
		    (write-string "NOTATION (" port)
		    (if (pair? (cdr type))
			(begin
			  (write-xml-name (cadr type) port)
			  (for-each (lambda (name)
				      (write-string "|" port)
				      (write-xml-name name port))
				    (cddr type))))
		    (write-string ")" port))
		   ((and (pair? type) (eq? (car type) 'ENUMERATED))
		    (write-string "(" port)
		    (if (pair? (cdr type))
			(begin
			  (write-xml-name (cadr type) port)
			  (for-each (lambda (name)
				      (write-string "|" port)
				      (write-xml-name name port))
				    (cddr type))))
		    (write-string ")" port))
		   (else
		    (error "Malformed !ATTLIST type:" type))))
	   (write-string " " port)
	   (let ((default (caddr definition)))
	     (cond ((symbol? default)
		    (write-xml-name default port))
		   ((and (pair? default) (eq? (car default) 'DEFAULT))
		    (write-xml-string (cadr default) port))
		   ((and (pair? default) (symbol? (car default)))
		    (write-xml-name (car default) port)
		    (write-string " " port)
		    (write-xml-string (cadr default) port))
		   (else
		    (error "Malformed !ATTLIST default:" default)))))))
    (if (pair? definitions)
	(if (pair? (cdr definitions))
	    (for-each (lambda (definition)
			(newline port)
			(write-string "          " port)
			(write-definition definition))
		      definitions)
	    (begin
	      (write-string " " port)
	      (write-definition (car definitions))))))
  (write-string ">" port))

(define-method write-xml ((decl xml-!entity-rtd) port)
  (write-string "<!ENTITY " port)
  (write-xml-name (xml-!entity-name decl) port)
  (write-string " " port)
  (if (xml-external-id? (xml-!entity-value decl))
      (write-xml-external-id (xml-!entity-value decl) port)
      (write-entity-value (xml-!entity-value decl) port))
  (write-string ">" port))

(define-method write-xml ((decl xml-unparsed-!entity-rtd) port)
  (write-string "<!ENTITY " port)
  (write-xml-name (xml-unparsed-!entity-name decl) port)
  (write-string " " port)
  (write-xml-external-id (xml-unparsed-!entity-id decl) port)
  (write-string " NDATA " port)
  (write-xml-name (xml-unparsed-!entity-notation decl) port)
  (write-string ">" port))

(define-method write-xml ((decl xml-parameter-!entity-rtd) port)
  (write-string "<!ENTITY % " port)
  (write-xml-name (xml-parameter-!entity-name decl) port)
  (write-string " " port)
  (if (xml-external-id? (xml-parameter-!entity-value decl))
      (write-xml-external-id (xml-parameter-!entity-value decl) port)
      (write-entity-value (xml-parameter-!entity-value decl) port))
  (write-string ">" port))

(define-method write-xml ((decl xml-!notation-rtd) port)
  (write-string "<!NOTATION " port)
  (write-xml-name (xml-!notation-name decl) port)
  (write-string " " port)
  (write-xml-external-id (xml-!notation-id decl) port)
  (write-string ">" port))

(define-method write-xml ((string <string>) port)
  (let ((end (string-length string)))
    (do ((i 0 (fix:+ i 1)))
	((fix:= i end))
      (let ((char (string-ref string i)))
	(cond ((char=? char #\<)
	       (write-string "&lt;" port))
	      ((char=? char #\&)
	       (write-string "&amp;" port))
	      (else
	       (write-char char port)))))))

(define (write-xml-name name port)
  (write-string (symbol-name name) port))

(define (write-xml-attributes attributes port)
  (for-each (lambda (attribute)
	      (write-string " " port)
	      (write-xml-attribute attribute port))
	    attributes))

(define (write-xml-attribute attribute port)
  (write-xml-name (car attribute) port)
  (write-string "=" port)
  (write-xml-string (cdr attribute) port))

(define (write-xml-string string port)
  (let ((quote-char (if (string-find-next-char string #\") #\' #\"))
	(end (string-length string)))
    (write-char quote-char port)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i end))
      (let ((char (string-ref string i)))
	(cond ((char=? char quote-char)
	       (write-string (if (char=? char #\") "&quot;" "&apos;") port))
	      ((char=? char #\<)
	       (write-string "&lt;" port))
	      ((char=? char #\&)
	       (write-string "&amp;" port))
	      (else
	       (write-char char port)))))
    (write-char quote-char port)))

(define (write-entity-value string port)
  (let ((quote-char (if (string-find-next-char string #\") #\' #\"))
	(end (string-length string)))
    (write-char quote-char port)
    (do ((i 0 (fix:+ i 1)))
	((fix:= i end))
      (let ((char (string-ref string i)))
	(cond ((char=? char quote-char)
	       (write-string (if (char=? char #\") "&quot;" "&apos;") port))
	      ((char=? char #\%)
	       (write-string "&#37;" port))
	      ((char=? char #\&)
	       (write-string "&amp;" port))
	      (else
	       (write-char char port)))))
    (write-char quote-char port)))

(define (write-xml-external-id id port)
  (if (xml-external-id-id id)
      (begin
	(write-string "PUBLIC " port)
	(write-xml-string (xml-external-id-id id) port))
      (write-string "SYSTEM" port))
  (if (xml-external-id-uri id)
      (begin
	(write-string " " port)
	(write-xml-string (xml-external-id-uri id) port))))