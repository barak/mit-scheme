#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

;;;; XML name structures

(declare (usual-integrations))

(define (make-xml-name qname #!optional uri)
  (let ((qname (make-xml-qname qname))
	(uri-string
	 (cond ((default-object? uri) (null-xml-namespace-uri))
	       ((string? uri) uri)
	       ((wide-string? uri) (wide-string->utf8-string uri))
	       ((symbol? uri) (symbol-name uri))
	       ((uri? uri) (uri->string uri))
	       (else (error:not-uri uri 'MAKE-XML-NAME)))))
    (string->uri uri-string)		;signals error if not URI
    (if (string-null? uri-string)
	qname
	(begin
	  (if (not (case (xml-qname-prefix qname)
		     ((xml) (string=? uri-string xml-uri-string))
		     ((xmlns) (string=? uri-string xmlns-uri-string))
		     (else #t)))
	      (error:bad-range-argument uri-string 'MAKE-XML-NAME))
	  (%make-xml-name qname uri-string)))))

(define (%make-xml-name qname uri-string)
  (let ((uname
	 (let ((local (xml-qname-local qname)))
	   (hash-table/intern! (hash-table/intern! expanded-names
						   uri-string
						   make-eq-hash-table)
			       local
			       (lambda ()
				 (make-expanded-name uri-string
						     local
						     (make-eq-hash-table)))))))
    (hash-table/intern! (expanded-name-combos uname)
			qname
			(lambda () (make-combo-name qname uname)))))

(define expanded-names
  (make-string-hash-table))

(define (xml-name? object)
  (or (xml-qname? object)
      (combo-name? object)))

(define-guarantee xml-name "an XML Name")

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

(define (make-xml-nmtoken object)
  (if (string? object)
      (begin
	(if (not (string-is-xml-nmtoken? object))
	    (error:bad-range-argument object 'MAKE-XML-NMTOKEN))
	(utf8-string->symbol object))
      (begin
	(guarantee-xml-nmtoken object 'MAKE-XML-NMTOKEN)
	object)))

(define (xml-nmtoken? object)
  (and (symbol? object)
       (string-is-xml-nmtoken? (symbol-name object))))

(define-guarantee xml-nmtoken "an XML name token")

(define (xml-nmtoken-string nmtoken)
  (guarantee-xml-nmtoken nmtoken 'XML-NMTOKEN-STRING)
  (symbol-name nmtoken))

(define (string-is-xml-qname? string)
  (let ((end (string-length string)))
    (let ((c (substring-find-next-char string 0 end #\:)))
      (if c
	  (and (not (substring-find-next-char string (fix:+ c 1) end #\:))
	       (string-is-xml-name? string 0 c)
	       (string-is-xml-name? string (fix:+ c 1) end))
	  (string-is-xml-name? string 0 end)))))

(define (string-is-xml-name? string #!optional start end)
  (eq? (string-is-xml-nmtoken? string start end) 'NAME))

(define (string-is-xml-nmtoken? string #!optional start end)
  (let ((buffer (utf8-string->parser-buffer string start end)))
    (letrec
	((match-tail
	  (lambda ()
	    (if (peek-parser-buffer-char buffer)
		(and (match-parser-buffer-char-in-alphabet
		      buffer alphabet:name-subsequent)
		     (match-tail))
		#t))))
      (if (match-parser-buffer-char-in-alphabet buffer alphabet:name-initial)
	  (and (match-tail)
	       'NAME)
	  (and (match-parser-buffer-char-in-alphabet buffer
						     alphabet:name-subsequent)
	       (match-tail)
	       'NMTOKEN)))))

(define (string-composed-of? string char-set)
  (and (string? string)
       (substring-composed-of? string 0 (string-length string) char-set)))

(define (substring-composed-of? string start end char-set)
  (let loop ((index start))
    (or (fix:= index end)
	(and (char-set-member? char-set (string-ref string index))
	     (loop (fix:+ index 1))))))

(define (xml-name-string name)
  (symbol-name (xml-name-qname name)))

(define (xml-name-qname name)
  (cond ((xml-qname? name) name)
	((combo-name? name) (combo-name-qname name))
	(else (error:not-xml-name name 'XML-NAME-QNAME))))

(define (xml-name-qname=? name qname)
  (eq? (xml-name-qname name) qname))

(define (xml-name-uri name)
  (cond ((xml-qname? name) "")
	((combo-name? name) (expanded-name-uri (combo-name-expanded name)))
	(else (error:not-xml-name name 'XML-NAME-URI))))

(define (xml-name-uri=? name uri)
  (uri=? (xml-name-uri name) uri))

(define (xml-name-prefix name)
  (xml-qname-prefix
   (cond ((xml-qname? name) name)
	 ((combo-name? name) (combo-name-qname name))
	 (else (error:not-xml-name name 'XML-NAME-PREFIX)))))

(define (null-xml-name-prefix? object)
  (eq? object '||))

(define (null-xml-name-prefix)
  '||)

(define (xml-name-prefix=? name prefix)
  (eq? (xml-name-prefix name) prefix))

(define (xml-name-local name)
  (cond ((xml-qname? name) (xml-qname-local name))
	((combo-name? name) (expanded-name-local (combo-name-expanded name)))
	(else (error:not-xml-name name 'XML-NAME-LOCAL))))

(define (xml-name-local=? name local)
  (eq? (xml-name-local name) local))

(define (xml-name=? n1 n2)
  (let ((lose (lambda (n) (error:not-xml-name n 'XML-NAME=?))))
    (cond ((xml-qname? n1)
	   (cond ((xml-qname? n2) (eq? n1 n2))
		 ((combo-name? n2) (eq? n1 (combo-name-qname n2)))
		 (else (lose n2))))
	  ((combo-name? n1)
	   (cond ((xml-qname? n2)
		  (eq? (combo-name-qname n1) n2))
		 ((combo-name? n2)
		  (eq? (combo-name-expanded n1)
		       (combo-name-expanded n2)))
		 (else (lose n2))))
	  (else (lose n1)))))

(define (xml-name-hash name modulus)
  (eq-hash-mod (xml-name-local name) modulus))

(define make-xml-name-hash-table
  (strong-hash-table/constructor xml-name-hash xml-name=? #t))

(define (make-xml-qname object)
  (if (string? object)
      (begin
	(if (not (string-is-xml-qname? object))
	    (error:bad-range-argument object 'MAKE-XML-QNAME))
	(utf8-string->symbol object))
      (begin
	(guarantee-xml-qname object 'MAKE-XML-QNAME)
	object)))

(define (xml-qname? object)
  (and (interned-symbol? object)
       (string-is-xml-qname? (symbol-name object))))

(define-guarantee xml-qname "an XML QName")

(define (xml-qname-string qname)
  (guarantee-xml-qname qname 'XML-QNAME-STRING)
  (symbol->utf8-string qname))

(define (xml-qname-local qname)
  (guarantee-xml-qname qname 'XML-QNAME-LOCAL)
  (let ((s (symbol-name qname)))
    (let ((c (string-find-next-char s #\:)))
      (if c
	  (utf8-string->symbol (string-tail s (fix:+ c 1)))
	  qname))))

(define (xml-qname-prefix qname)
  (guarantee-xml-qname qname 'XML-QNAME-PREFIX)
  (let ((s (symbol-name qname)))
    (let ((c (string-find-next-char s #\:)))
      (if c
	  (utf8-string->symbol (string-head s c))
	  (null-xml-name-prefix)))))

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