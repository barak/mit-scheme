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

;;;; RDF data structures

;;; Interning mechanisms used here hold onto the interned objects
;;; strongly.  They should be changed to hold them weakly.

(declare (usual-integrations))

;;;; Triples

(define-record-type <rdf-triple>
    (%make-rdf-triple subject predicate object index)
    rdf-triple?
  (subject rdf-triple-subject)
  (predicate rdf-triple-predicate)
  (object rdf-triple-object)
  (index rdf-triple-index))

(define-guarantee rdf-triple "RDF triple")

(define (make-rdf-triple subject predicate object)
  (let ((subject (canonicalize-rdf-subject subject 'MAKE-RDF-TRIPLE))
	(predicate (canonicalize-rdf-predicate predicate 'MAKE-RDF-TRIPLE))
	(object (canonicalize-rdf-object object 'MAKE-RDF-TRIPLE)))
    (hash-table-intern! rdf-triples (vector subject predicate object)
      (lambda ()
	(let ((triple
	       (%make-rdf-triple subject predicate object (next-index))))
	  (event-distributor/invoke! event:new-rdf-triple triple)
	  triple)))))

(define (for-each-rdf-triple procedure)
  (for-each procedure
	    (hash-table-values rdf-triples)))

(define next-index
  (let ((counter 0))
    (lambda ()
      (let ((index counter))
	(set! counter (+ counter 1))
	index))))

(define rdf-triples (make-equal-hash-table))
(define event:new-rdf-triple (make-event-distributor))

(define (rdf-subject? thing)
  (or (absolute-uri? thing)
      (rdf-bnode? thing)
      (rdf-graph? thing)))

(define (rdf-predicate? thing)
  (absolute-uri? thing))

(define (rdf-object? thing)
  (or (absolute-uri? thing)
      (rdf-bnode? thing)
      (rdf-graph? thing)
      (rdf-literal? thing)))

(define-guarantee rdf-subject "RDF subject")
(define-guarantee rdf-predicate "RDF predicate")
(define-guarantee rdf-object "RDF object")

(define (canonicalize-rdf-subject subject #!optional caller)
  (cond ((or (rdf-bnode? subject) (rdf-graph? subject)) subject)
	((%decode-bnode-uri subject))
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

;;;; Graphs

(define-record-type <rdf-graph>
    (%make-rdf-graph triples)
    rdf-graph?
  (triples rdf-graph-triples))

(define-guarantee rdf-graph "RDF graph")

(define (make-rdf-graph triples)
  (guarantee-list-of-type triples rdf-triple? "list of RDF triples"
			  'MAKE-RDF-GRAPH)
  (let ((triples
	 (if (pair? triples)
	     (let ((head
		    (cons 'DUMMY
			  (sort triples
			    (lambda (t1 t2)
			      (< (rdf-triple-index t1)
				 (rdf-triple-index t2)))))))
	       (let loop ((this (cdr head)) (prev head))
		 (let ((next (cdr this)))
		   (if (pair? next)
		       (if (eq? (car this) (car next))
			   (begin
			     (set-cdr! prev next)
			     (loop next prev))
			   (loop next this)))))
	       (cdr head))
	     '())))
    (hash-table-intern! rdf-graphs triples
      (lambda ()
	(let ((graph (%make-rdf-graph triples)))
	  (event-distributor/invoke! event:new-rdf-graph graph)
	  graph)))))

(define rdf-graphs (make-equal-hash-table))
(define event:new-rdf-graph (make-event-distributor))

;;;; Blank nodes

(define-record-type <rdf-bnode>
    (%make-rdf-bnode)
    rdf-bnode?)

(define-guarantee rdf-bnode "RDF bnode")

(define-print-method rdf-bnode?
  (standard-print-method 'rdf-bnode
    (lambda (bnode)
      (list (rdf-bnode-name bnode)))))

(define (make-rdf-bnode #!optional name)
  (if (default-object? name)
      (%make-rdf-bnode)
      (begin
	(guarantee string? name 'MAKE-RDF-BNODE)
	(hash-table-intern! *rdf-bnode-registry* name %make-rdf-bnode))))

(define (rdf-bnode-name bnode)
  (string-append "B" (number->string (hash-object bnode))))

(define (%decode-bnode-uri uri)
  (let ((v
	 (cond ((string? uri) (*parse-string parse-bnode uri))
	       ((symbol? uri) (*parse-symbol parse-bnode uri))
	       (else #f))))
    (and v
	 (unhash-object (vector-ref v 0)))))

(define parse-bnode
  (let ((digits (ascii-range->char-set #x30 #x3A)))
    (*parser
     (seq (noise "_:B")
	  (map (lambda (s) (string->number s 10 #t))
	       (match (+ (char-set digits))))
	  (noise (end-of-input))))))

(define (with-rdf-input-port port thunk)
  (fluid-let ((*rdf-bnode-registry*
	       (intern-port-property! port 'RDF-BNODE-REGISTRY
				      make-string-hash-table)))
    (thunk)))

(define *rdf-bnode-registry*)

;;;; Literals

(define-record-type <rdf-literal>
    (%make-rdf-literal text type)
    rdf-literal?
  (text rdf-literal-text)
  (type %rdf-literal-type))

(define-guarantee rdf-literal "RDF literal")

(define (make-rdf-literal text type)
  (guarantee string? text 'MAKE-RDF-LITERAL)
  (let ((type
	 (if (or (not type)
		 (language? type))
	     type
	     (->absolute-uri type 'MAKE-RDF-LITERAL))))
    (hash-table-intern! rdf-literals (cons text type)
      (lambda ()
	(%make-rdf-literal text type)))))

(define rdf-literals
  (make-equal-hash-table))

(define (rdf-literal-type literal)
  (let ((type (%rdf-literal-type literal)))
    (and (absolute-uri? type)
	 type)))

(define (rdf-literal-language literal)
  (let ((type (%rdf-literal-type literal)))
    (and (not (absolute-uri? type))
	 type)))

(define-print-method rdf-literal?
  (bracketed-print-method 'RDF-LITERAL
    (lambda (literal port)
      (write-char #\space port)
      (write-rdf/nt-literal literal port))))

(define (rdf-literal=? l1 l2)
  (eq? l1 l2))

(define (language? object)
  (and (interned-symbol? object)
       (*match-symbol match-language object)))

(define match-language
  (let* ((language-head (ascii-range->char-set #x61 #x7B))
	 (language-tail
	  (char-set-union language-head
			  (ascii-range->char-set #x30 #x3A))))
    (*matcher
     (seq (+ (char-set language-head))
	  (* (seq #\- (+ (char-set language-tail))))))))

;;;; Qnames

(define (register-rdf-prefix prefix expansion #!optional registry)
  (guarantee-rdf-prefix prefix 'REGISTER-RDF-PREFIX)
  (%register-rdf-prefix prefix
			(uri->string
			 (->absolute-uri expansion 'REGISTER-RDF-PREFIX))
			(check-registry registry 'REGISTER-RDF-PREFIX))
  prefix)

(define (merge-rdf-prefix-registry! from-registry #!optional to-registry)
  (guarantee-rdf-prefix-registry from-registry 'MERGE-RDF-PREFIX-REGISTRY!)
  (let ((to-registry (check-registry to-registry 'MERGE-RDF-PREFIX-REGISTRY!)))
    (for-each (lambda (p1)
		(if (not (eq? (car p1) ':))
		    (%register-rdf-prefix (car p1) (cdr p1) to-registry)))
	      (registry-bindings from-registry))))

(define (%register-rdf-prefix prefix expansion registry)
  (let ((p (assq prefix (registry-bindings registry))))
    (if p
	(if (not (string=? (cdr p) expansion))
	    (begin
	      (warn "RDF prefix override:" prefix (cdr p) expansion)
	      (set-cdr! p expansion)))
	(set-registry-bindings! registry
				(cons (cons prefix expansion)
				      (registry-bindings registry))))))

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
	     (or (find (lambda (p)
			 (and (not (eq? (car p) ':))
			      (filter p)))
		       alist)
		 (find filter alist)))))
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
  (guarantee string? local 'MAKE-RDF-QNAME)
  (if (not (*match-string match:name local))
      (error:bad-range-argument local 'MAKE-RDF-QNAME))
  (symbol prefix local))

(define (rdf-qname-prefix qname)
  (guarantee-rdf-qname qname 'RDF-QNAME-PREFIX)
  (let ((s (symbol->string qname)))
    (symbol (string-head s (fix:+ (string-find-next-char s #\:) 1)))))

(define (rdf-qname-local qname)
  (guarantee-rdf-qname qname 'RDF-QNAME-LOCAL)
  (let ((s (symbol->string qname)))
    (string-tail s (fix:+ (string-find-next-char s #\:) 1))))

(define (split-rdf-qname qname)
  (guarantee-rdf-qname qname 'SPLIT-RDF-QNAME)
  (let ((s (symbol->string qname)))
    (let ((i (fix:+ (string-find-next-char s #\:) 1)))
      (values (symbol (string-head s i))
	      (string-tail s i)))))

(define (rdf-qname? object)
  (and (interned-symbol? object)
       (*match-symbol match-qname object)))

(define-guarantee rdf-qname "RDF QName")

(define match-qname
  (*matcher (seq match-prefix match-tail)))

(define (match-tail buffer)
  (let loop ()
    (if (read-parser-buffer-char buffer)
	(loop)))
  #t)

(define (rdf-prefix? object)
  (and (interned-symbol? object)
       (*match-symbol match-prefix object)))

(define-guarantee rdf-prefix "RDF prefix")

(define match-prefix
  (*matcher (seq match:prefix-name ":")))

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

(define (port/set-rdf-prefix-registry port registry)
  (if registry
      (begin
	(guarantee-rdf-prefix-registry registry 'PORT/SET-RDF-PREFIX-REGISTRY!)
	(set-port-property! port 'RDF-PREFIX-REGISTRY registry))
      (remove-port-property! port 'RDF-PREFIX-REGISTRY)))

(define (port/rdf-prefix-registry port)
  (or (port-property port 'RDF-PREFIX-REGISTRY #f)
      *default-rdf-prefix-registry*))