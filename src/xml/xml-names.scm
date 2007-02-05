#| -*-Scheme-*-

$Id: xml-names.scm,v 1.15 2007/01/17 03:43:04 cph Exp $

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007 Massachusetts Institute of Technology

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

(define (make-xml-name qname uri)
  (let ((qname (make-xml-qname qname))
	(uri (->uri uri)))
    (if (null-xml-namespace-uri? uri)
	qname
	(begin
	  (check-prefix+uri qname uri)
	  (%make-xml-name qname uri)))))

(define (check-prefix+uri qname uri)
  (if (not (and (uri-absolute? uri)
		(let ((s (symbol-name qname)))
		  (let ((c (find-prefix-separator s)))
		    (case c
		      ((#f) #t)
		      ((ILLEGAL) #f)
		      (else
		       (case (utf8-string->symbol (string-head s c))
			 ((xml) (uri=? uri xml-uri))
			 ((xmlns) (uri=? uri xmlns-uri))
			 (else #t))))))))
      (error:bad-range-argument uri 'MAKE-XML-NAME)))

(define (%make-xml-name qname uri)
  (let ((uname
	 (let ((local (xml-qname-local qname)))
	   (hash-table/intern! (hash-table/intern! expanded-names
						   uri
						   make-eq-hash-table)
			       local
			       (lambda ()
				 (make-expanded-name uri
						     local
						     (make-eq-hash-table)))))))
    (hash-table/intern! (expanded-name-combos uname)
			qname
			(lambda () (make-combo-name qname uname)))))

(define expanded-names
  (make-eq-hash-table))

(define (xml-name? object)
  (or (xml-qname? object)
      (combo-name? object)))

(define (guarantee-xml-name object caller)
  (if (not (xml-name? object))
      (error:not-xml-name object caller)))

(define (error:not-xml-name object caller)
  (error:wrong-type-argument object "an XML Name" caller))

(define (null-xml-namespace-uri? object)
  (and (uri? object)
       (uri=? object null-namespace-uri)))

(define (null-xml-namespace-uri)
  null-namespace-uri)

(define null-namespace-uri (->relative-uri ""))
(define xml-uri (->absolute-uri "http://www.w3.org/XML/1998/namespace"))
(define xmlns-uri (->absolute-uri "http://www.w3.org/2000/xmlns/"))

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

(define (guarantee-xml-nmtoken object caller)
  (if (not (xml-nmtoken? object))
      (error:not-xml-nmtoken object caller)))

(define (error:not-xml-nmtoken object caller)
  (error:wrong-type-argument object "an XML name token" caller))

(define (xml-nmtoken-string nmtoken)
  (guarantee-xml-nmtoken nmtoken 'XML-NMTOKEN-STRING)
  (symbol-name nmtoken))

(define (string-is-xml-name? string)
  (eq? (string-is-xml-nmtoken? string) 'NAME))

(define (string-is-xml-nmtoken? string)
  (let ((buffer (utf8-string->parser-buffer string)))
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
  (cond ((xml-qname? name) (null-xml-namespace-uri))
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
	(if (not (string-is-xml-name? object))
	    (error:bad-range-argument object 'MAKE-XML-QNAME))
	(utf8-string->symbol object))
      (begin
	(guarantee-xml-qname object 'MAKE-XML-QNAME)
	object)))

(define (xml-qname? object)
  (and (interned-symbol? object)
       (string-is-xml-name? (symbol-name object))))

(define (guarantee-xml-qname object caller)
  (if (not (xml-qname? object))
      (error:not-xml-qname object caller)))

(define (error:not-xml-qname object caller)
  (error:wrong-type-argument object "an XML QName" caller))

(define (xml-qname-string qname)
  (guarantee-xml-qname qname 'XML-QNAME-STRING)
  (symbol->utf8-string qname))

(define (xml-qname-local qname)
  (let ((s (symbol-name qname)))
    (let ((c (find-prefix-separator s)))
      (if (or (not c) (eq? c 'ILLEGAL))
	  qname
	  (utf8-string->symbol (string-tail s (fix:+ c 1)))))))

(define (xml-qname-prefix qname)
  (let ((s (symbol-name qname)))
    (let ((c (find-prefix-separator s)))
      (if (or (not c) (eq? c 'ILLEGAL))
	  (null-xml-name-prefix)
	  (utf8-string->symbol (string-head s c))))))

(define (find-prefix-separator s)
  (let ((c (string-find-next-char s #\:)))
    (if (or (not c)
	    (let ((i (fix:+ c 1))
		  (e (string-length s)))
	      (and (let ((char
			  (let ((port (open-input-string s i e)))
			    (port/set-coding port 'UTF-8)
			    (read-char port))))
		     (and (not (eof-object? char))
			  (not (char=? char #\:))
			  (char-in-alphabet? char alphabet:name-initial)))
		   (not (substring-find-next-char s i e #\:)))))
	c
	'ILLEGAL)))

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