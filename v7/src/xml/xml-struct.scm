;;; -*-Scheme-*-
;;;
;;; $Id: xml-struct.scm,v 1.1 2001/07/05 20:47:53 cph Exp $
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

;;;; XML data structures

(declare (usual-integrations))

(define-structure xml-document
  declaration
  misc-1
  dtd
  misc-2
  root
  misc-3)

(define-structure xml-declaration
  version
  encoding
  standalone?)

(define-structure (xml-element
		   (print-procedure
		    (standard-unparser-method 'XML-ELEMENT
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-element-name element) port)))))
  name
  attributes
  contents)

(define-structure (xml-processing-instructions
		   (print-procedure
		    (standard-unparser-method 'XML-PROCESSING-INSTRUCTIONS
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-processing-instructions-name element)
			       port)))))
  name
  text)

(define-structure xml-comment
  text)

(define-structure (xml-entity-reference
		   (print-procedure
		    (standard-unparser-method 'XML-ENTITY-REFERENCE
		      (lambda (reference port)
			(write-char #\space port)
			(write (xml-entity-reference-name reference) port)))))
  name)

(define-structure (xml-parameter-entity-reference
		   (print-procedure
		    (standard-unparser-method 'XML-PARAMETER-ENTITY-REFERENCE
		      (lambda (reference port)
			(write-char #\space port)
			(write (xml-parameter-entity-reference-name reference)
			       port)))))
  name)

(define (xml-intern string)
  ;; Prevents XML names from cluttering the symbol table.
  (or (hash-table/get xml-tokens string #f)
      (let ((symbol (string->uninterned-symbol string)))
	(hash-table/put! xml-tokens string symbol)
	symbol)))

(define xml-tokens
  (make-string-hash-table))

(define-structure (xml-dtd
		   (print-procedure
		    (standard-unparser-method 'XML-DTD
		      (lambda (dtd port)
			(write-char #\space port)
			(write (xml-dtd-root dtd) port)))))
  root
  external
  internal)

(define-structure (xml-external-id
		   (print-procedure
		    (standard-unparser-method 'XML-EXTERNAL-ID
		      (lambda (dtd port)
			(write-char #\space port)
			(write (or (xml-external-id-id dtd)
				   (xml-external-id-uri dtd))
			       port)))))
  id
  uri)

(define-structure (xml-element-declaration
		   (print-procedure
		    (standard-unparser-method 'XML-ELEMENT-DECLARATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-element-declaration-name element) port)))))
  name
  content-type)

(define-structure (xml-attribute-declaration
		   (print-procedure
		    (standard-unparser-method 'XML-ATTRIBUTE-DECLARATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-attribute-declaration-name element)
			       port)))))
  name
  definitions)

(define-structure xml-include-section
  contents)

(define-structure xml-ignore-section
  contents)

(define-structure (xml-entity-declaration
		   (print-procedure
		    (standard-unparser-method 'XML-ENTITY-DECLARATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-entity-declaration-name element) port)))))
  name
  value)

(define-structure (xml-parameter-entity-declaration
		   (print-procedure
		    (standard-unparser-method 'XML-PARAMETER-ENTITY-DECLARATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-parameter-entity-declaration-name element)
			       port)))))
  name
  value)

(define-structure (xml-notation-declaration
		   (print-procedure
		    (standard-unparser-method 'XML-NOTATION-DECLARATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-notation-declaration-name element)
			       port)))))
  name
  value)