#| -*-Scheme-*-

$Id: rdf-struct.scm,v 1.9 2006/06/22 19:17:27 cph Exp $

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
  (%make-rdf-triple (canonicalize-rdf-subject subject 'MAKE-RDF-TRIPLE)
		    (canonicalize-rdf-predicate predicate 'MAKE-RDF-TRIPLE)
		    (canonicalize-rdf-object object 'MAKE-RDF-TRIPLE)))

(define-record-type <rdf-bnode>
    (%make-rdf-bnode name)
    rdf-bnode?
  (name rdf-bnode-name))

(define-guarantee rdf-bnode "RDF bnode")

(set-record-type-unparser-method! <rdf-bnode>
  (standard-unparser-method 'RDF-BNODE
    (lambda (bnode port)
      (write-char #\space port)
      (write-rdf-bnode bnode port))))

(define (make-rdf-bnode #!optional name)
  (%make-rdf-bnode
   (cond ((default-object? name)
	  (generate-bnode-name))
	 ((and (string? name)
	       (complete-match match-bnode-name name))
	  name)
	 (else
	  (error:wrong-type-argument name "RDF bnode name" 'RDF-BNODE)))))

(define (generate-bnode-name)
  (string-append "B" (vector-8b->hexadecimal (random-byte-vector 8))))

(define-record-type <rdf-literal>
    (%make-rdf-literal text type)
    rdf-literal?
  (text rdf-literal-text)
  (type %rdf-literal-type))

(define-guarantee rdf-literal "RDF literal")

(set-record-type-unparser-method! <rdf-literal>
  (standard-unparser-method 'RDF-LITERAL
    (lambda (literal port)
      (write-char #\space port)
      (write-rdf-literal literal port))))

(define (make-rdf-literal text type)
  (guarantee-utf8-string text 'RDF-LITERAL)
  (%make-rdf-literal text
		     (if (or (not type)
			     (and (interned-symbol? type)
				  (complete-match match-language
						  (symbol-name type))))
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

(define (rdf-literal=? l1 l2)
  (and (string=? (rdf-literal-text l1) (rdf-literal-text l2))
       (eq? (%rdf-literal-type l1) (%rdf-literal-type l2))))

(define-record-type <rdf-index>
    (%make-rdf-index subjects predicates objects)
    rdf-index?
  (subjects rdf-index-subjects)
  (predicates rdf-index-predicates)
  (objects rdf-index-objects))

(define-guarantee rdf-index "RDF index")

(define (make-rdf-index)
  (%make-rdf-index (make-eq-hash-table)
		   (make-eq-hash-table)
		   (make-eq-hash-table)))

(define (add-to-rdf-index triple index)
  (let ((add
	 (lambda (key index)
	   (hash-table/put! index
			    key
			    (cons triple
				  (hash-table/get index
						  key
						  '()))))))
    (add (rdf-triple-subject triple) (rdf-index-subjects index))
    (add (rdf-triple-predicate triple) (rdf-index-predicates index))
    (let ((o (rdf-triple-object triple)))
      (if (not (rdf-literal? o))
	  (add o (rdf-index-objects index))))))

(define (canonicalize-rdf-subject subject #!optional caller)
  (if (rdf-bnode? subject)
      subject
      (->absolute-uri subject caller)))

(define (canonicalize-rdf-predicate predicate #!optional caller)
  (->absolute-uri predicate caller))

(define (canonicalize-rdf-object object #!optional caller)
  (cond ((or (rdf-bnode? object)
	     (rdf-literal? object))
	 object)
	((string? object) (make-rdf-literal object #f))
	(else (->absolute-uri object caller))))

(define match-bnode-name
  (let* ((name-head
	  (char-set-union (ascii-range->char-set #x41 #x5B)
			  (ascii-range->char-set #x61 #x7B)))
	 (name-tail
	  (char-set-union name-head
			  (ascii-range->char-set #x30 #x3A))))
    (*matcher
     (seq (char-set name-head)
	  (* (char-set name-tail))))))

(define match-language
  (let* ((language-head (ascii-range->char-set #x61 #x7B))
	 (language-tail
	  (char-set-union language-head
			  (ascii-range->char-set #x30 #x3A))))
    (*matcher
     (seq (+ (char-set language-head))
	  (* (seq #\- (+ (char-set language-tail))))))))

(define (complete-match matcher string)
  (let ((buffer (string->parser-buffer string)))
    (and (matcher buffer)
	 (not (peek-parser-buffer-char buffer)))))