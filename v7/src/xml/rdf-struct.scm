#| -*-Scheme-*-

$Id: rdf-struct.scm,v 1.19 2006/10/29 05:23:59 cph Exp $

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

(define (canonicalize-rdf-subject subject #!optional caller)
  (cond ((rdf-bnode? subject) subject)
	((%decode-bnode-uri subject caller))
	(else (canonicalize-rdf-uri subject caller))))

(define (canonicalize-rdf-predicate predicate #!optional caller)
  (canonicalize-rdf-uri predicate caller))

(define (canonicalize-rdf-object object #!optional caller)
  (cond ((rdf-literal? object) object)
	((string? object) (make-rdf-literal object #f))
	(else (canonicalize-rdf-subject object caller))))

(define (canonicalize-rdf-uri uri #!optional caller)
  (if (rdf-qname? uri)
      (rdf-qname->uri uri)
      (->absolute-uri uri caller)))

;;;; Blank nodes

(define-record-type <rdf-bnode>
    (%make-rdf-bnode name)
    rdf-bnode?
  (name rdf-bnode-name))

(define-guarantee rdf-bnode "RDF bnode")

(set-record-type-unparser-method! <rdf-bnode>
  (standard-unparser-method 'RDF-BNODE
    (lambda (bnode port)
      (write-char #\space port)
      (write-rdf/nt-bnode bnode port))))

(define (make-rdf-bnode #!optional name)
  (cond ((default-object? name)
	 (let ((name (generate-bnode-name)))
	   (let ((bnode (%make-rdf-bnode name)))
	     (hash-table/put! *rdf-bnode-registry* name bnode)
	     bnode)))
	((and (string? name)
	      (complete-match match-bnode-name name))
	 (hash-table/intern! *rdf-bnode-registry* name
	   (lambda ()
	     (%make-rdf-bnode name))))
	(else
	 (error:wrong-type-argument name "RDF bnode name" 'RDF-BNODE))))

(define (generate-bnode-name)
  (let loop ()
    (let ((name
	   (string-append "B"
			  (vector-8b->hexadecimal (random-byte-vector 8)))))
      (if (hash-table/get *rdf-bnode-registry* name #f)
	  (loop)
	  name))))

(define (%decode-bnode-uri uri caller)
  (let ((lookup
	 (lambda (uri)
	   (let ((bnode
		  (hash-table/get *rdf-bnode-registry*
				  (string-tail uri 2)
				  #f)))
	     (if (not bnode)
		 (error:bad-range-argument uri caller))
	     bnode))))
    (let ((handle-uri
	   (lambda (uri)
	     (cond ((complete-match match-bnode-uri uri) (lookup uri))
		   ((handle->rdf-bnode uri #f))
		   (else #f)))))
      (cond ((string? uri) (handle-uri uri))
	    ((symbol? uri) (handle-uri (symbol-name uri)))
	    (else #f)))))

(define (complete-match matcher string)
  (let ((buffer (string->parser-buffer string)))
    (and (matcher buffer)
	 (not (peek-parser-buffer-char buffer)))))

(define match-bnode-uri
  (*matcher (seq "_:" match-bnode-name)))

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

(define (make-rdf-bnode-registry)
  (make-string-hash-table))

(define *rdf-bnode-registry*
  (make-rdf-bnode-registry))

(define (port/bnode-registry port)
  (or (port/get-property port 'PORT/BNODE-REGISTRY #f)
      (let ((table (make-string-hash-table)))
	(port/set-property! port 'PORT/BNODE-REGISTRY table)
	table)))

(define (port/drop-bnode-registry port)
  (port/remove-property! port 'PORT/DROP-BNODE-REGISTRY))

(define (rdf-bnode->handle bnode)
  (string-append bnode-handle-prefix (number->string (hash bnode))))

(define (handle->rdf-bnode handle #!optional caller)
  (let ((v
	 (and (string? handle)
	      (parse-bnode-handle (string->parser-buffer handle)))))
    (if v
	(unhash (vector-ref v 0))
	(begin
	  (if caller
	      (error:wrong-type-argument handle "RDF bnode handle" caller))
	  #f))))

(define parse-bnode-handle
  (let ((prefix
	 (lambda (b) (match-parser-buffer-string b bnode-handle-prefix)))
	(digits (ascii-range->char-set #x30 #x3A)))
    (*parser
     (seq (noise prefix)
	  (map (lambda (s) (string->number s 10 #t))
	       (match (+ (char-set digits))))
	  (noise (end-of-input))))))

(define bnode-handle-prefix
  "_bnode_:")

;;;; Literals

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
      (write-rdf/nt-literal literal port))))

(define (make-rdf-literal text type)
  (guarantee-utf8-string text 'RDF-LITERAL)
  (%make-rdf-literal text
		     (if (or (not type)
			     (and (interned-symbol? type)
				  (complete-match match-language
						  (symbol-name type))))
			 type
			 (->absolute-uri type 'RDF-LITERAL))))

(define match-language
  (let* ((language-head (ascii-range->char-set #x61 #x7B))
	 (language-tail
	  (char-set-union language-head
			  (ascii-range->char-set #x30 #x3A))))
    (*matcher
     (seq (+ (char-set language-head))
	  (* (seq #\- (+ (char-set language-tail))))))))

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

;;;; Triples index (deprecated)

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

;;;; Qnames

(define (register-rdf-prefix prefix expansion #!optional registry)
  (guarantee-rdf-prefix prefix 'REGISTER-RDF-PREFIX)
  (let ((registry (check-registry registry 'REGISTER-RDF-PREFIX)))
    (let ((p (assq prefix (registry-bindings registry)))
	  (new
	   (uri->string
	    (->absolute-uri expansion 'REGISTER-RDF-PREFIX))))
      (if p
	  (if (not (string=? (cdr p) new))
	      (begin
		(warn "RDF prefix override:" prefix (cdr p) new)
		(set-cdr! p new)))
	  (set-registry-bindings! registry
				  (cons (cons prefix new)
					(registry-bindings registry))))))
  prefix)

(define (rdf-prefix-expansion prefix #!optional registry)
  (guarantee-rdf-prefix prefix 'RDF-PREFIX-EXPANSION)
  (let ((p
	 (assq prefix
	       (registry-bindings
		(check-registry registry 'RDF-PREFIX-EXPANSION)))))
    (and p
	 (cdr p))))

(define (uri->rdf-prefix uri #!optional registry error?)
  (let ((s (uri->string (->absolute-uri uri 'URI->RDF-PREFIX))))
    (let ((p
	   (let ((alist
		  (registry-bindings
		   (check-registry registry 'URI->RDF-PREFIX)))
		 (filter (lambda (p) (string-prefix? (cdr p) s))))
	     (or (find-matching-item alist
		   (lambda (p)
		     (and (not (eq? (car p) ':))
			  (filter p))))
		 (find-matching-item alist filter)))))
      (if p
	  (values (car p) (cdr p))
	  (begin
	    (if error? (error:bad-range-argument uri 'URI->RDF-PREFIX))
	    (values #f #f))))))

(define (uri->rdf-qname uri #!optional registry error?)
  (let ((uri (->absolute-uri uri 'URI->RDF-QNAME)))
    (receive (prefix expansion) (uri->rdf-prefix uri registry error?)
      (and prefix
	   (symbol prefix
		   (string-tail (uri->string uri)
				(string-length expansion)))))))

(define (rdf-qname->uri qname #!optional registry error?)
  (receive (prefix local) (split-rdf-qname qname)
    (let ((expansion (rdf-prefix-expansion prefix registry)))
      (if expansion
	  (->absolute-uri (string-append expansion local) 'RDF-QNAME->URI)
	  (begin
	    (if error? (error:bad-range-argument qname 'RDF-QNAME->URI))
	    #f)))))

(define (make-rdf-qname prefix local)
  (guarantee-rdf-prefix prefix 'MAKE-RDF-QNAME)
  (guarantee-string local 'MAKE-RDF-QNAME)
  (if (not (complete-match match:name local))
      (error:bad-range-argument local 'MAKE-RDF-QNAME))
  (symbol prefix local))

(define (rdf-qname-prefix qname)
  (guarantee-rdf-qname qname 'RDF-QNAME-PREFIX)
  (let ((s (symbol-name qname)))
    (symbol (string-head s (fix:+ (string-find-next-char s #\:) 1)))))

(define (rdf-qname-local qname)
  (guarantee-rdf-qname qname 'RDF-QNAME-LOCAL)
  (let ((s (symbol-name qname)))
    (string-tail s (fix:+ (string-find-next-char s #\:) 1))))

(define (split-rdf-qname qname)
  (guarantee-rdf-qname qname 'SPLIT-RDF-QNAME)
  (let ((s (symbol-name qname)))
    (let ((i (fix:+ (string-find-next-char s #\:) 1)))
      (values (symbol (string-head s i))
	      (string-tail s i)))))

(define (rdf-qname? object)
  (and (interned-symbol? object)
       (complete-match match-qname (symbol-name object))))

(define-guarantee rdf-qname "RDF QName")

(define (rdf-prefix? object)
  (and (interned-symbol? object)
       (complete-match match-prefix (symbol-name object))))

(define-guarantee rdf-prefix "RDF prefix")

(define match-qname
  (*matcher (seq match-prefix match:name)))

(define match-prefix
  (*matcher (seq (? match:prefix-name) ":")))

(define-record-type <rdf-prefix-registry>
    (make-rdf-prefix-registry bindings)
    rdf-prefix-registry?
  (bindings registry-bindings set-registry-bindings!))

(define-guarantee rdf-prefix-registry "RDF QName prefix registry")

(define (rdf-prefix-registry->alist #!optional registry)
  (alist-copy
   (registry-bindings
    (check-registry registry 'RDF-PREFIX-REGISTRY->ALIST))))

(define (copy-rdf-prefix-registry #!optional registry)
  (make-rdf-prefix-registry
   (alist-copy
    (registry-bindings (check-registry registry 'COPY-RDF-PREFIX-REGISTRY)))))

(define (check-registry registry caller)
  (if (default-object? registry)
      (let ((registry *default-rdf-prefix-registry*))
	(if (rdf-prefix-registry? registry)
	    registry
	    (let ((registry (new-rdf-prefix-registry)))
	      (warn "*default-rdf-prefix-registry* has illegal value.")
	      (set! *default-rdf-prefix-registry* registry)
	      registry)))
      (begin
	(guarantee-rdf-prefix-registry registry caller)
	registry)))

(define (new-rdf-prefix-registry)
  (make-rdf-prefix-registry (alist-copy default-prefixes)))

(define default-prefixes
  '((rdf: . "http://www.w3.org/1999/02/22-rdf-syntax-ns#")
    (rdfs: . "http://www.w3.org/2000/01/rdf-schema#")
    (owl: . "http://www.w3.org/2002/07/owl#")
    (xsd: . "http://www.w3.org/2001/XMLSchema#")))

(define *default-rdf-prefix-registry*
  (new-rdf-prefix-registry))