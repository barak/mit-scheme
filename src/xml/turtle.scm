#| -*-Scheme-*-

$Id: turtle.scm,v 1.7 2006/08/02 05:05:20 cph Exp $

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; Parser for RDF/Turtle

(declare (usual-integrations))

(define (read-rdf/turtle-file pathname #!optional base-uri)
  (let ((pathname (pathname-default-type pathname "ttl")))
    (call-with-input-file pathname
      (lambda (port)
	(fluid-let ((*rdf-bnode-registry* (make-rdf-bnode-registry)))
	  (post-process-parser-output
	   (parse-turtle-doc (input-port->parser-buffer port))
	   (if (default-object? base-uri)
	       (pathname->uri (merge-pathnames pathname))
	       (merge-uris (file-namestring pathname)
			   (->absolute-uri base-uri 'read-turtle-file)))))))))

(define (parse-turtle-doc buffer)
  (parse:ws* buffer)
  (discard-parser-buffer-head! buffer)
  (let loop ((items '()))
    (if (peek-parser-buffer-char buffer)
	(let ((v
	       (or (parse:directive buffer)
		   (parse:triples buffer)
		   (parser-buffer-error buffer "Expected subject"))))
	  (parse:ws* buffer)
	  (if (not (match-parser-buffer-char buffer #\.))
	      (parser-buffer-error buffer "Expected dot"))
	  (parse:ws* buffer)
	  (loop (cons (vector-ref v 0) items)))
	(reverse! items))))

(define parse:directive
  (*parser
   (encapsulate (lambda (v) (cons 'prefix (vector->list v)))
     (seq "@"
	  (alt (seq "prefix" parse:ws+)
	       (error #f "Unknown directive name"))
	  (alt (seq (alt parse:prefix-name (values #f))
		    ":"
		    parse:ws+)
	       (error #f "Expected prefix name"))
	  (alt parse:uriref
	       (error #f "Expected URI reference"))))))

(define parse:triples
  (*parser
   (encapsulate (lambda (v)
		  (cons* 'triples
			 (vector-ref v 0)
			 (vector-ref v 1)))
     (seq parse:subject
	  parse:ws+
	  parse:predicate-object-list))))

(define parse:predicate-object-list
  (*parser
   (encapsulate vector->list
     (seq parse:predicate-object-list-1
	  (* (seq parse:ws*
		  ";"
		  parse:ws*
		  parse:predicate-object-list-1))
	  (? (seq parse:ws*
		  ";"))))))

(define parse:predicate-object-list-1
  (let ((rdf:type (->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")))
    (*parser
     (encapsulate (lambda (v)
		    (cons (vector-ref v 0)
			  (vector-ref v 1)))
       (seq (alt parse:resource
		 (map (lambda (v) v rdf:type)
		      (match "a")))
	    (alt parse:ws+
		 (error #f "Expected whitespace"))
	    (encapsulate vector->list
	      (seq parse:object-required
		   (* (seq parse:ws*
			   ","
			   parse:ws*
			   parse:object-required)))))))))

(define parse:subject
  (*parser (alt parse:resource parse:blank)))

(define parse:object-required
  (*parser
   (alt parse:object
	(error #f "Expected object"))))

(define parse:object
  (*parser (alt parse:resource parse:blank parse:literal)))

(define parse:resource
  (*parser
   (alt (map string->uri parse:uriref)
	(encapsulate (lambda (v) (cons 'qname (vector->list v)))
	  (seq (alt parse:prefix-name (values #f))
	       ":"
	       (alt parse:name (values #f)))))))

(define parse:blank
  (*parser
   (with-pointer p
     (alt (map make-rdf-bnode
	       (seq "_:" parse:name))
	  ;; This notation should probably accept whitespace between the
	  ;; brackets, but the spec is written like this:
	  (encapsulate (lambda (v) v (make-rdf-bnode))
	    "[]")
	  (map (lambda (pols) (cons 'blank-node pols))
	       (seq "["
		    parse:ws*
		    parse:predicate-object-list
		    parse:ws*
		    (alt "]" (error p "Malformed blank node"))))
	  (encapsulate (lambda (v) (cons 'collection (vector->list v)))
	    (seq "("
		 parse:ws*
		 (alt ")"
		      (seq parse:object-required
			   (* (seq parse:ws+ parse:object))
			   parse:ws*
			   (alt ")"
				(error #f
				       "Expected close parenthesis"))))))))))

(define parse:name
  (*parser (match match:name)))

(define match:name
  (*matcher
   (seq (alphabet alphabet:name-start-char)
	(* (alphabet alphabet:name-char)))))

(define parse:prefix-name
  (*parser (match match:prefix-name)))

(define match:prefix-name
  (*matcher
   (seq (alphabet alphabet:prefix-name-start-char)
	(* (alphabet alphabet:name-char)))))

;;;; Literals

(define parse:literal
  (*parser
   (alt (encapsulate (lambda (v)
		       (let ((string (vector-ref v 0))
			     (type (vector-ref v 1)))
			 (if (or (not type)
				 (symbol? type)
				 (absolute-uri? type))
			     (make-rdf-literal string type)
			     (list 'typed-literal string type))))
	  (seq (alt parse:long-string parse:string)
	       (alt (seq "@"
			 (alt parse:language
			      (error #f "Malformed language")))
		    (seq "^^"
			 (alt parse:resource
			      (error #f "Malformed datatype")))
		    (values #f))))
	parse:double
	parse:decimal
	parse:integer
	parse:boolean)))

(define parse:language
  (*parser
   (map utf8-string->symbol
	(match (seq (+ (char-set char-set:turtle-lower))
		    (* (seq "-"
			    (+ (char-set char-set:turtle-lower+digit)))))))))

(define parse:integer
  (*parser
   (map (lambda (s)
	  (make-rdf-literal
	   (if (char=? (string-ref s 0) #\-)
	       s
	       (let ((end (string-length s)))
		 (let loop ((i (if (char=? (string-ref s 0) #\+) 1 0)))
		   (if (and (fix:< i end) (char=? (string-ref s i) #\0))
		       (loop (fix:+ i 1))
		       (if (fix:= i 0)
			   s
			   (string-tail s i))))))
	   xsd:integer))
	(match (seq (? (alt "-" "+"))
		    (+ (char-set char-set:turtle-digit)))))))

(define xsd:integer
  (string->uri "http://www.w3.org/2001/XMLSchema#integer"))

(define parse:double
  (let ((match:exponent
	 (*matcher
	  (seq (alt "e" "E")
	       (? (alt "-" "+"))
	       (+ (char-set char-set:turtle-digit))))))
    (*parser
     (map (lambda (s) (make-rdf-literal s xsd:double))
	  (match (seq (? (alt "-" "+"))
		      (alt (seq (+ (char-set char-set:turtle-digit))
				"."
				(* (char-set char-set:turtle-digit))
				match:exponent)
			   (seq "."
				(+ (char-set char-set:turtle-digit))
				match:exponent)
			   (seq (+ (char-set char-set:turtle-digit))
				match:exponent))))))))

(define xsd:double
  (string->uri "http://www.w3.org/2001/XMLSchema#double"))

(define parse:decimal
  (*parser
   (map (lambda (s) (make-rdf-literal s xsd:decimal))
	(match (seq (? (alt "-" "+"))
		    (alt (seq (+ (char-set char-set:turtle-digit))
			      "."
			      (* (char-set char-set:turtle-digit)))
			 (seq "."
			      (+ (char-set char-set:turtle-digit)))
			 ;;(+ (char-set char-set:turtle-digit))
			 ))))))

(define xsd:decimal
  (string->uri "http://www.w3.org/2001/XMLSchema#decimal"))

(define parse:boolean
  (*parser
   (map (lambda (s) (make-rdf-literal s xsd:boolean))
	(match (alt "true" "false")))))

(define xsd:boolean
  (string->uri "http://www.w3.org/2001/XMLSchema#boolean"))

;;;; Alphabets

(define char-set:turtle-hex
  (string->char-set "0123456789ABCDEF"))

(define char-set:turtle-digit
  (string->char-set "0123456789"))

(define char-set:turtle-ws
  (string->char-set "\t\n\r "))

(define char-set:turtle-lower
  (string->char-set "abcdefghijklmnopqrstuvwxyz"))

(define char-set:turtle-lower+digit
  (char-set-union char-set:turtle-lower
		  char-set:turtle-digit))

(define alphabet:name-start-char
  (code-points->alphabet
   '((#x0041 . #x005A)
     #x005F
     (#x0061 . #x007A)
     (#x00C0 . #x00D6)
     (#x00D8 . #x00F6)
     (#x00F8 . #x02FF)
     (#x0370 . #x037D)
     (#x037F . #x1FFF)
     (#x200C . #x200D)
     (#x2070 . #x218F)
     (#x2C00 . #x2FEF)
     (#x3001 . #xD7FF)
     (#xF900 . #xFDCF)
     (#xFDF0 . #xFFFD)
     (#x10000 . #xEFFFF))))

(define alphabet:name-char
  (alphabet+ alphabet:name-start-char
	     (code-points->alphabet
	      '(#x002d
		(#x0030 . #x0039)
		#x00B7
		(#x0300 . #x036F)
		(#x203F . #x2040)))))

(define alphabet:prefix-name-start-char
  (alphabet- alphabet:name-start-char (alphabet #\_)))

(define alphabet:character
  (code-points->alphabet '((#x20 . #x5B) (#x5D . #x10FFFF))))

(define alphabet:ucharacter
  (alphabet- alphabet:character (alphabet #\>)))

(define alphabet:scharacter
  (alphabet- alphabet:character (alphabet #\")))

(define alphabet:lcharacter
  (alphabet+ alphabet:character (alphabet #\tab #\newline #\return)))

;;;; Escaped strings

(define (delimited-region-parser name start-delim end-delim
				 alphabet parse-escapes)
  (lambda (buffer)
    (let ((output (open-output-string))
	  (start (get-parser-buffer-pointer buffer)))

      (define (read-head)
	(let ((p (get-parser-buffer-pointer buffer)))
	  (let loop ()
	    (cond ((match-parser-buffer-string-no-advance buffer end-delim)
		   (copy p)
		   (match-parser-buffer-string buffer end-delim)
		   (finish))
		  ((match-parser-buffer-char-in-alphabet buffer alphabet)
		   (loop))
		  ((match-parser-buffer-char-no-advance buffer #\\)
		   (copy p)
		   (let ((p (get-parser-buffer-pointer buffer)))
		     (read-parser-buffer-char buffer)
		     (read-escape p)))
		  (else
		   (let ((c (peek-parser-buffer-char buffer)))
		     (if c
			 (parser-buffer-error buffer
					      (emsg "Illegal character")
					      c)
			 (parser-buffer-error start
					      (emsg "Unexpected EOF")))))))))

      (define (read-escape p)
	(let ((v (parse-escapes buffer)))
	  (if v
	      (begin
		(write-char (vector-ref v 0) output)
		(read-head))
	      (parser-buffer-error p (emsg "Malformed string escape")))))

      (define (emsg msg)
	(string-append msg " in " name))

      (define (copy p)
	(call-with-parser-buffer-tail buffer p
	  (lambda (string start end)
	    (write-substring string start end output))))

      (define (finish)
	(vector (get-output-string output)))

      (port/set-coding output 'utf-8)
      (and (match-parser-buffer-string buffer start-delim)
	   (read-head)))))

(define (simple-escape-parser . chars)
  (lambda (buffer)
    (let loop ((chars chars))
      (and (pair? chars)
	   (if (match-parser-buffer-char buffer (car chars))
	       (vector (cadr chars))
	       (loop (cddr chars)))))))

(define parse:scharacter-escape
  (let ((parse-simple
	 (simple-escape-parser #\\ #\\
			       #\" #\"
			       #\t #\tab
			       #\n #\newline
			       #\r #\return)))
    (*parser
     (alt parse-simple
	  parse:unicode-escape))))

(define parse:ucharacter-escape
  (let ((parse-simple
	 (simple-escape-parser #\\ #\\
			       #\> #\>)))
    (*parser
     (alt parse-simple
	  parse:unicode-escape))))

(define parse:unicode-escape
  (*parser
   (map (lambda (s) (integer->char (string->number s 16 #t)))
	(alt (seq "u" (match (n*n 4 (char-set char-set:turtle-hex))))
	     (seq "U" (match (n*n 8 (char-set char-set:turtle-hex))))))))

(define parse:string
  (delimited-region-parser "string"
			   "\""
			   "\""
			   alphabet:scharacter
			   parse:scharacter-escape))

(define parse:long-string
  (delimited-region-parser "long string"
			   "\"\"\""
			   "\"\"\""
			   alphabet:lcharacter
			   parse:scharacter-escape))

(define parse:uriref
  (delimited-region-parser "URI reference"
			   "<"
			   ">"
			   alphabet:ucharacter
			   parse:ucharacter-escape))

;;;; Whitespace

(define parse:ws*
  (*parser (noise (* match:ws))))

(define parse:ws+
  (*parser (noise (+ match:ws))))

(define match:ws
  (*matcher
   (alt (char-set char-set:turtle-ws)
	match:comment)))

(define match:comment
  (*matcher
   (seq "#"
	(sexp (lambda (buffer)
		(let loop ()
		  (if (let ((char (read-parser-buffer-char buffer)))
			(and char
			     (not (char=? char #\newline))))
		      (loop)))
		#t)))))

;;;; Post-processing

;;; This code does prefix expansion and URI merging.

(define (post-process-parser-output stmts base-uri)
  (let ((prefixes
	 (map (lambda (p)
		(let ((prefix (cadr p))
		      (v (uri->string (merge-uris (caddr p) base-uri))))
		  (if prefix
		      (register-rdf-prefix (symbol prefix ':) v))
		  (cons prefix v)))
	      (keep-matching-items stmts
		(lambda (stmt)
		  (eq? (car stmt) 'prefix))))))
    (append-map! (lambda (stmt)
		   (case (car stmt)
		     ((triples)
		      (post-process-triples (cadr stmt)
					    (cddr stmt)
					    prefixes
					    base-uri))
		     ((prefix) '())
		     (else (error "Unknown statement:" stmt))))
		 stmts)))

(define (post-process-triples subject pols prefixes base-uri)
  (receive (subject triples) (post-process-resource subject prefixes base-uri)
    (append! triples
	     (post-process-pols subject
				pols
				prefixes
				base-uri))))

(define (post-process-pols subject pols prefixes base-uri)
  (append-map! (lambda (pol)
		 (receive (predicate triples)
		     (post-process-resource (car pol) prefixes base-uri)
		   (append! triples
			    (append-map! (lambda (object)
					   (receive (object triples)
					       (post-process-resource object
								      prefixes
								      base-uri)
					     (cons (make-rdf-triple subject
								    predicate
								    object)
						   triples)))
					 (cdr pol)))))
	       pols))

(define (post-process-resource resource prefixes base-uri)
  (cond ((or (absolute-uri? resource)
	     (rdf-bnode? resource)
	     (rdf-literal? resource))
	 (values resource '()))
	((relative-uri? resource)
	 (values (merge-uris resource base-uri) '()))
	((pair? resource)
	 (case (car resource)
	   ((qname)
	    (values (post-process-qname (cadr resource)
					(caddr resource)
					prefixes)
		    '()))
	   ((blank-node)
	    (let ((s (make-rdf-bnode)))
	      (values s
		      (post-process-pols s (cdr resource) prefixes base-uri))))
	   ((typed-literal)
	    (receive (uri triples)
		(post-process-resource (caddr resource) prefixes base-uri)
	      (values (make-rdf-literal (cadr resource) uri)
		      triples)))
	   ((collection)
	    (post-process-collection (cdr resource) prefixes base-uri))
	   (else
	    (error "Unknown resource:" resource))))
	(else
	 (error "Unknown resource:" resource))))

(define (post-process-qname prefix local prefixes)
  (string->uri
   (string-append (cdr
		   (or (find-matching-item prefixes
			 (if prefix
			     (lambda (p)
			       (and (string? (car p))
				    (string=? (car p) prefix)))
			     (lambda (p)
			       (not (car p)))))
		       (error "Unknown prefix:" prefix)))
		  (or local ""))))

(define (post-process-collection resources prefixes base-uri)
  (if (pair? resources)
      (receive (first triples)
	  (post-process-resource (car resources) prefixes base-uri)
	(receive (rest triples*)
	    (post-process-collection (cdr resources) prefixes base-uri)
	  (let ((p (make-rdf-bnode)))
	    (values p
		    (cons* (make-rdf-triple p rdf:first first)
			   (make-rdf-triple p rdf:rest rest)
			   (append! triples triples*))))))
      (values rdf:nil '())))

(define rdf:nil
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))

(define rdf:first
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))

(define rdf:rest
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))