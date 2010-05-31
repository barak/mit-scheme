#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010 Massachusetts Institute of Technology

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

;;;; XML names

(declare (usual-integrations))

(define (make-xml-name name #!optional uri)
  (let ((name-symbol (make-xml-name-symbol name))
	(uri
	 (if (default-object? uri)
	     (null-xml-namespace-uri)
	     (->absolute-uri uri 'MAKE-XML-NAME))))
    (if (null-xml-namespace-uri? uri)
	name-symbol
	(begin
	  (guarantee-xml-qname name-symbol 'MAKE-XML-NAME)
	  (if (not (case (xml-qname-prefix name-symbol)
		     ((xml) (uri=? uri xml-uri))
		     ((xmlns) (uri=? uri xmlns-uri))
		     (else #t)))
	      (error:bad-range-argument uri 'MAKE-XML-NAME))
	  (%make-xml-name name-symbol uri)))))

;;; EXPANDED-NAMES should be a key-weak hash table, but that has an
;;; effect only if the other two hash tables are datum-weak, because
;;; there is a strong reference from each datum to its associated key
;;; in all three hash tables involved.  And since we don't have datum-
;;; weak hash tables, for now these will all be key-strong, since
;;; there's some overhead and no value to using key-weak hash tables.

(define (%make-xml-name qname uri)
  (let ((uname
	 (let ((local (xml-qname-local qname)))
	   (hash-table/intern! (hash-table/intern! expanded-names uri
				 make-strong-eq-hash-table)
	       local
	     (lambda ()
	       (make-expanded-name uri local (make-strong-eq-hash-table)))))))
    (hash-table/intern! (expanded-name-combos uname) qname
      (lambda ()
	(make-combo-name qname uname)))))

(define expanded-names
  (make-strong-eq-hash-table))

(define (xml-name? object)
  (or (xml-name-symbol? object)
      (combo-name? object)))

(define-guarantee xml-name "an XML Name")

(define (xml-name-string name)
  (symbol-name (xml-name->symbol name)))

(define (xml-name->symbol name)
  (cond ((xml-name-symbol? name) name)
	((combo-name? name) (combo-name-qname name))
	(else (error:not-xml-name name 'XML-NAME->SYMBOL))))

(define (xml-name=? n1 n2)
  (if (and (combo-name? n1) (combo-name? n2))
      (eq? (combo-name-expanded n1) (combo-name-expanded n2))
      (eq? (xml-name->symbol n1) (xml-name->symbol n2))))

(define-record-type <combo-name>
    (make-combo-name qname expanded)
    combo-name?
  (qname combo-name-qname)
  (expanded combo-name-expanded))

(set-record-type-unparser-method! <combo-name>
  (standard-unparser-method 'XML-NAME
    (lambda (name port)
      (write-char #\space port)
      (write (combo-name-qname name) port))))

(define-record-type <expanded-name>
    (make-expanded-name uri local combos)
    expanded-name?
  (uri expanded-name-uri)
  (local expanded-name-local)
  (combos expanded-name-combos))

;;;; Symbolic names

(define (name-matcher initial subsequent)
  (lambda (buffer)
    (and (match-parser-buffer-char-in-set buffer initial)
	 (let loop ()
	   (if (match-parser-buffer-char-in-set buffer subsequent)
	       (loop)
	       #t)))))

(define match-ncname
  (name-matcher char-set:ncname-initial
		char-set:ncname-subsequent))

(define match:xml-name
  (name-matcher char-set:name-initial
		char-set:name-subsequent))

(define match:xml-nmtoken
  (name-matcher char-set:name-subsequent
		char-set:name-subsequent))

(define match:xml-qname
  (*matcher (seq match-ncname (? (seq ":" match-ncname)))))

(define (string-matcher matcher)
  (lambda (string #!optional start end)
    (matcher (utf8-string->parser-buffer string start end))))

(define string-is-xml-qname? (string-matcher match:xml-qname))
(define string-is-xml-name? (string-matcher match:xml-name))
(define string-is-xml-nmtoken? (string-matcher match:xml-nmtoken))

(define (name-constructor string-predicate constructor)
  (lambda (object)
    (if (string? object)
	(begin
	  (if (not (string-predicate object))
	      (error:bad-range-argument object constructor))
	  (utf8-string->symbol object))
	(begin
	  (guarantee-symbol object constructor)
	  (if (not (string-predicate (symbol-name object)))
	      (error:bad-range-argument object constructor))
	  object))))

(define make-xml-name-symbol
  (name-constructor string-is-xml-name? 'MAKE-XML-NAME-SYMBOL))

(define make-xml-nmtoken
  (name-constructor string-is-xml-nmtoken? 'MAKE-XML-NMTOKEN))

(define make-xml-qname
  (name-constructor string-is-xml-qname? 'MAKE-XML-QNAME))

(define (name-predicate string-predicate)
  (lambda (object)
    (and (symbol? object)
	 (string-predicate (symbol-name object)))))

(define xml-name-symbol? (name-predicate string-is-xml-name?))
(define xml-nmtoken? (name-predicate string-is-xml-nmtoken?))
(define xml-qname? (name-predicate string-is-xml-qname?))

(define-guarantee xml-name-symbol "an XML name symbol")
(define-guarantee xml-nmtoken "an XML name token")
(define-guarantee xml-qname "an XML QName")

;;;; Namespace support

(define (xml-namespace-conformant-name? object)
  (or (xml-qname? object)
      (combo-name? object)))

(define-guarantee xml-namespace-conformant-name
  "XML Namespaces conformant name")

(define (xml-name-uri name)
  (cond ((xml-qname? name) (null-xml-namespace-uri))
	((combo-name? name) (expanded-name-uri (combo-name-expanded name)))
	(else (error:not-xml-namespace-conformant-name name 'XML-NAME-URI))))

(define (xml-name-uri=? name uri)
  (uri=? (xml-name-uri name) uri))

(define (xml-name-prefix name)
  (%xml-qname-prefix
   (cond ((xml-qname? name) name)
	 ((combo-name? name) (combo-name-qname name))
	 (else
	  (error:not-xml-namespace-conformant-name name 'XML-NAME-PREFIX)))))

(define (null-xml-name-prefix? object)
  (eq? object '||))

(define (null-xml-name-prefix)
  '||)

(define (xml-name-prefix=? name prefix)
  (eq? (xml-name-prefix name) prefix))

(define (xml-name-local name)
  (cond ((xml-qname? name) (%xml-qname-local name))
	((combo-name? name) (expanded-name-local (combo-name-expanded name)))
	(else (error:not-xml-namespace-conformant-name name 'XML-NAME-LOCAL))))

(define (xml-name-local=? name local)
  (eq? (xml-name-local name) local))

(define (null-xml-namespace-uri? object)
  (and (uri? object)
       (uri=? object null-namespace-uri)))

(define (null-xml-namespace-uri)
  null-namespace-uri)

(define null-namespace-uri (->uri ""))
(define xml-uri-string "http://www.w3.org/XML/1998/namespace")
(define xml-uri (->uri xml-uri-string))
(define xmlns-uri-string "http://www.w3.org/2000/xmlns/")
(define xmlns-uri (->uri xmlns-uri-string))

(define (xml-qname-prefix qname)
  (guarantee-xml-qname qname 'XML-QNAME-PREFIX)
  (%xml-qname-prefix qname))

(define (%xml-qname-prefix qname)
  (let ((s (symbol-name qname)))
    (let ((c (string-find-next-char s #\:)))
      (if c
	  (utf8-string->symbol (string-head s c))
	  (null-xml-name-prefix)))))

(define (xml-qname-local qname)
  (guarantee-xml-qname qname 'XML-QNAME-LOCAL)
  (%xml-qname-local qname))

(define (%xml-qname-local qname)
  (let ((s (symbol-name qname)))
    (let ((c (string-find-next-char s #\:)))
      (if c
	  (utf8-string->symbol (string-tail s (fix:+ c 1)))
	  qname))))