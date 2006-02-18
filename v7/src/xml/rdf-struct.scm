#| -*-Scheme-*-

$Id: rdf-struct.scm,v 1.1 2006/02/18 04:31:51 cph Exp $

Copyright 2006 Massachusetts Institute of Technology

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

;;;; RDF data structures

(declare (usual-integrations))

(define-record-type <rdf-triple>
    (%make-rdf-triple subject predicate object)
    rdf-triple?
  (subject rdf-triple-subject)
  (predicate rdf-triple-predicate)
  (object rdf-triple-object))

(define-guarantee rdf-triple "RDF triple")

(define (make-rdf-triple subject predicate object)
  (%make-rdf-triple (if (rdf-bnode? subject)
			subject
			(->absolute-uri subject 'MAKE-RDF-TRIPLE))
		    (->absolute-uri predicate 'MAKE-RDF-TRIPLE)
		    (if (or (rdf-bnode? object)
			    (rdf-literal? object))
			object
			(->absolute-uri object 'MAKE-RDF-TRIPLE))))

(define-record-type <rdf-bnode>
    (%make-rdf-bnode name)
    rdf-bnode?
  (name rdf-bnode-name))

(define-guarantee rdf-bnode "RDF bnode")

(define (make-rdf-bnode name)
  (if (not (and (string? name)
		(complete-match match-bnode-name name)))
      (error:wrong-type-argument name "RDF bnode name" 'RDF-BNODE))
  (%make-rdf-bnode name))

(define-record-type <rdf-literal>
    (%make-rdf-literal text type)
    rdf-literal?
  (text rdf-literal-text)
  (type %rdf-literal-type))

(define-guarantee rdf-literal "RDF literal")

(define (make-rdf-literal text type)
  (guarantee-utf8-string text 'RDF-LITERAL)
  (%make-rdf-literal text
		     (if (or (not type)
			     (and (string? type)
				  (complete-match match-language type)))
			 type
			 (->absolute-uri type 'RDF-LITERAL))))

(define (rdf-literal-type literal)
  (let ((type (%rdf-literal-type literal)))
    (and (absolute-uri? type)
	 type)))

(define (rdf-literal-language literal)
  (let ((type (%rdf-literal-type literal)))
    (and (not (absolute-uri? type))
	 type)))

(define (complete-match matcher string #!optional start end)
  (let ((buffer (string->parser-buffer string start end)))
    (and (matcher buffer)
	 (not (peek-parser-buffer-char buffer)))))