#| -*-Scheme-*-

$Id: xml-struct.scm,v 1.9 2003/02/14 18:28:38 cph Exp $

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

(define-structure (xml-document
		   (type-descriptor xml-document-rtd))
  declaration
  misc-1
  dtd
  misc-2
  root
  misc-3)

(define-structure (xml-declaration
		   (type-descriptor xml-declaration-rtd))
  version
  encoding
  standalone)

(define-structure (xml-element
		   (type-descriptor xml-element-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-ELEMENT
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-element-name element) port)))))
  name
  attributes
  contents)

(define-structure (xml-comment
		   (type-descriptor xml-comment-rtd))
  text)

(define-structure (xml-processing-instructions
		   (type-descriptor xml-processing-instructions-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-PROCESSING-INSTRUCTIONS
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-processing-instructions-name element)
			       port)))))
  name
  text)

(define-structure (xml-uninterpreted
		   (type-descriptor xml-uninterpreted-rtd))
  text)

(define (xml-intern name)
  (string->symbol name))

(define-structure (xml-dtd
		   (type-descriptor xml-dtd-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-DTD
		      (lambda (dtd port)
			(write-char #\space port)
			(write (xml-dtd-root dtd) port)))))
  root
  external
  internal)

(define-structure (xml-external-id
		   (type-descriptor xml-external-id-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-EXTERNAL-ID
		      (lambda (dtd port)
			(write-char #\space port)
			(write (or (xml-external-id-id dtd)
				   (xml-external-id-uri dtd))
			       port)))))
  id
  uri)

(define-structure (xml-!element
		   (type-descriptor xml-!element-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-!ELEMENT
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-!element-name element) port)))))
  name
  content-type)

(define-structure (xml-!attlist
		   (type-descriptor xml-!attlist-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-!ATTLIST
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-!attlist-name element) port)))))
  name
  definitions)

(define-structure (xml-!entity
		   (type-descriptor xml-!entity-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-!ENTITY
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-!entity-name element) port)))))
  name
  value)

(define-structure (xml-unparsed-!entity
		   (type-descriptor xml-unparsed-!entity-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-UNPARSED-!ENTITY
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-unparsed-!entity-name element) port)))))
  name
  id
  notation)

(define-structure (xml-parameter-!entity
		   (type-descriptor xml-parameter-!entity-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-PARAMETER-!ENTITY
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-parameter-!entity-name element) port)))))
  name
  value)

(define-structure (xml-!notation
		   (type-descriptor xml-!notation-rtd)
		   (print-procedure
		    (standard-unparser-method 'XML-!NOTATION
		      (lambda (element port)
			(write-char #\space port)
			(write (xml-!notation-name element) port)))))
  name
  id)