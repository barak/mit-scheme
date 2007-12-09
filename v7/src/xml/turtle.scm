#| -*-Scheme-*-

$Id: turtle.scm,v 1.36 2007/12/09 04:42:03 cph Exp $

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

;;;; Codec for RDF/Turtle

(declare (usual-integrations))

;;;; Decoder

(define (read-rdf/turtle-file pathname #!optional base-uri)
  (let ((pathname (pathname-default-type pathname "ttl")))
    (call-with-input-file pathname
      (lambda (port)
	(read-rdf/turtle
	 port
	 (if (default-object? base-uri)
	     (pathname->uri (merge-pathnames pathname))
	     (merge-uris
	      (file-namestring pathname)
	      (->absolute-uri base-uri 'READ-RDF/TURTLE-FILE))))))))

(define (read-rdf/turtle port base-uri)
  (port/set-coding port 'UTF-8)
  (with-rdf-input-port port
    (lambda ()
      (post-process-parser-output
       (parse-turtle-doc (input-port->parser-buffer port))
       (->absolute-uri base-uri 'READ-RDF/TURTLE)))))

(define parse-turtle-doc
  (*parser
   (seq parse:ws*
	(encapsulate vector->list
	  (* (seq (alt parse:directive
		       parse:triples)
		  parse:ws*
		  "."
		  parse:ws*)))
	(alt (noise (end-of-input))
	     (error #f "Unable to parse")))))

(define parse:directive
  (*parser
   (encapsulate (lambda (v) (cons 'PREFIX (vector->list v)))
     (seq "@"
	  (alt (seq "prefix"
		    parse:ws+)
	       (error #f "Unknown directive name"))
	  (alt (seq parse:prefix
		    parse:ws+)
	       (error #f "Expected prefix name"))
	  (alt parse:uriref
	       (error #f "Expected URI reference"))))))

(define parse:triples
  (*parser
   (encapsulate (lambda (v)
		  (cons* 'TRIPLES
			 (vector-ref v 0)
			 (vector-ref v 1)))
     (seq parse:subject
	  parse:ws+
	  parse:predicate-object-list))))

(define parse:predicate-object-list
  (*parser
   (encapsulate vector->list
     (seq parse:predicate-object-list-1
	  (* (seq parse:ws* ";" parse:ws* parse:predicate-object-list-1))
	  (? (seq parse:ws* ";"))))))

(define parse:predicate-object-list-1
  (*parser
   (encapsulate (lambda (v) (cons (vector-ref v 0) (vector-ref v 1)))
     (seq (alt parse:resource
	       (map (lambda (v) v rdf:type)
		    (match "a")))
	  (alt parse:ws+
	       (error #f "Expected whitespace"))
	  (encapsulate vector->list
	    (seq parse:object-required
		 (* (seq parse:ws* "," parse:ws* parse:object-required))))))))

(define parse:subject
  (*parser (alt parse:resource parse:blank parse:graph)))

(define parse:object-required
  (*parser
   (alt parse:object
	(error #f "Expected object"))))

(define parse:object
  (*parser (alt parse:resource parse:blank parse:graph parse:literal)))

(define parse:resource
  (*parser
   (alt (map string->uri parse:uriref)
	(encapsulate (lambda (v) (cons 'QNAME (vector->list v)))
	  (seq parse:prefix
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
	  (map (lambda (pols) (cons 'BLANK-NODE pols))
	       (seq "["
		    parse:ws*
		    parse:predicate-object-list
		    parse:ws*
		    (alt "]" (error p "Malformed blank node"))))
	  (encapsulate (lambda (v) (cons 'COLLECTION (vector->list v)))
	    (seq "("
		 parse:ws*
		 (alt ")"
		      (seq parse:object-required
			   (* (seq parse:ws+
				   parse:object))
			   parse:ws*
			   (alt ")" (error p "Malformed list"))))))))))

(define parse:graph
  (*parser
   (encapsulate (lambda (v) (cons 'GRAPH (vector->list v)))
     (seq "{"
	  parse:ws*
	  (? (seq parse:triples
		  (* (seq parse:ws* "." parse:ws* parse:triples))
		  (? (seq parse:ws* "."))
		  parse:ws*))
	  "}"))))

(define parse:name
  (*parser (match match:name)))

(define match:name
  (*matcher
   (seq (alphabet alphabet:name-start-char)
	(* (alphabet alphabet:name-char)))))

(define parse:prefix
  (*parser
   (seq (match match:prefix-name)
	":")))

(define match:prefix-name
  (*matcher
   (? (seq (alphabet alphabet:prefix-name-start-char)
	   (* (alphabet alphabet:name-char))))))

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
			     (list 'TYPED-LITERAL string type))))
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

(define parse:boolean
  (*parser
   (map (lambda (s) (make-rdf-literal s xsd:boolean))
	(match (alt "true" "false")))))

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
	      '(#x002D
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

      (port/set-coding output 'UTF-8)
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

(define (post-process-parser-output v base-uri)
  (let ((stmts (vector-ref v 0))
	(registry (new-rdf-prefix-registry)))
    (values
     (make-rdf-graph
      (let ((prefixes
	     (map (lambda (p)
		    (let ((prefix (cadr p))
			  (v (uri->string (merge-uris (caddr p) base-uri))))
		      (register-rdf-prefix (symbol prefix ':) v registry)
		      (cons prefix v)))
		  (filter (lambda (stmt)
			    (eq? (car stmt) 'PREFIX))
			  stmts))))
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
     registry)))

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
	   ((graph)
	    (values (make-rdf-graph
		     (append-map! (lambda (stmt)
				    (case (car stmt)
				      ((triples)
				       (post-process-triples (cadr stmt)
							     (cddr stmt)
							     prefixes
							     base-uri))
				      (else
				       (error "Illegal statement:" stmt))))
				  (cdr resource)))
		    '()))
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
		   (or (find (lambda (p)
			       (string=? (car p) prefix))
			     prefixes)
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
		    (cons* (make-rdf-triple p rdf:type rdf:List)
			   (make-rdf-triple p rdf:first first)
			   (make-rdf-triple p rdf:rest rest)
			   (append! triples triples*))))))
      (values rdf:nil '())))

;;;; Encoder

(define (write-rdf/turtle-file graph registry pathname)
  (call-with-output-file pathname
    (lambda (port)
      (port/set-coding port 'UTF-8)
      (port/set-rdf-prefix-registry port registry)
      (write-rdf/turtle graph port))))

(define (write-rdf/turtle graph #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (write-prefixes graph port)
    (write-rdf/turtle-triples graph port)))

(define (write-prefixes graph port)
  (let ((table (make-eq-hash-table)))

    (define (check-graph g)
      (for-each check-triple (rdf-graph-triples g)))

    (define (check-triple t)
      (check-elt (rdf-triple-subject t))
      (check-elt (rdf-triple-predicate t))
      (check-elt (rdf-triple-object t)))

    (define (check-elt o)
      (cond ((uri? o)
	     (receive (prefix expansion)
		 (uri->rdf-prefix o (port/rdf-prefix-registry port) #f)
	       (if (and prefix (not (hash-table/get table prefix #f)))
		   (hash-table/put! table prefix expansion))))
	    ((rdf-graph? o)
	     (check-graph o))))

    (check-graph graph)
    (for-each (lambda (p)
		(write-rdf/turtle-prefix (car p) (cdr p) port))
	      (sort (hash-table->alist table)
		(lambda (a b)
		  (let ((a (symbol-name (car a)))
			(b (symbol-name (car b))))
		    (substring<? a 0 (fix:- (string-length a) 1)
				 b 0 (fix:- (string-length b) 1))))))))

(define (write-rdf/turtle-prefix prefix expansion #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (write-string "@prefix " port)
    (write-symbol prefix port)
    (write-string " <" port)
    (write-string expansion port)
    (write-string "> ." port)
    (newline port)))

(define (write-rdf/turtle-triples graph #!optional port)
  (write-triples (rdf-graph-triples graph)
		 0
		 (if (default-object? port) (current-output-port) port)))

(define (write-rdf/turtle-triple triple #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (write-group (list triple) 0 (lambda (s) s #f) port)
    (write-string "." port)))

(define (write-triples triples indentation port)
  (write-top-level triples
		   indentation
		   (let ((groups (inline-bnode-triples (all-triples triples))))
		     (lambda (subject)
		       (find (lambda (group)
			       (eq? (rdf-triple-subject (cadr group))
				    subject))
			     groups)))
		   port))

(define (inline-bnode-triples triples)
  (receive (no-refs one-ref)
      (classify-list triples
		     2
		     (lambda (t)
		       (let ((s (rdf-triple-subject t)))
			 (and (rdf-bnode? s)
			      (let ((n
				     (count (lambda (t)
					      (eq? (rdf-triple-object t) s))
					    triples)))
				(and (<= n 1)
				     n))))))
    (append! (map (lambda (ts) (cons 0 ts))
		  (group-triples-by-subject no-refs))
	     (map (lambda (ts) (cons 1 ts))
		  (group-triples-by-subject one-ref)))))

(define (all-triples triples)

  (define (run-queue q all)
    (if (pair? q)
	(let ((t (car q))
	      (q (cdr q)))
	  (let ((all (cons t all)))
	    (run-queue (check-elt (rdf-triple-object t)
				  (check-elt (rdf-triple-subject t)
					     q
					     all)
				  all)
		       all)))
	all))

  (define (check-elt elt q all)
    (if (rdf-graph? elt)
	(append (remove (lambda (t)
			  (or (memq t q)
			      (memq t all)))
			(rdf-graph-triples elt))
		q)
	q))

  (run-queue triples '()))

(define (group-triples-by-subject ts)
  (group-triples (sort-triples ts) rdf-triple-subject))

(define (write-top-level ts indentation inline-bnode port)
  (for-each (lambda (group)
	      (write-indentation indentation port)
	      (write-group group indentation inline-bnode port)
	      (write-string "." port)
	      (newline port))
	    (groups-to-write ts inline-bnode)))

(define (groups-to-write ts inline-bnode)
  (remove (lambda (group)
	    (let ((t (inline-bnode (rdf-triple-subject (car group)))))
	      (and t
		   (= (car t) 1))))
	  (group-triples-by-subject ts)))

(define (write-group ts indentation inline-bnode port)
  (let ((groups (group-triples ts rdf-triple-predicate)))
    (let ((subject-inline?
	   (write-subject (rdf-triple-subject (caar groups))
			  indentation
			  inline-bnode
			  port))
	(indentation (indent+ indentation)))
      (let ((writer
	     (and subject-inline?
		  (or (eq? (rdf-triple-predicate (caar groups)) rdf:type)
		      (null? (cdr groups)))
		  (null? (cdar groups))
		  (linear-object-writer (rdf-triple-object (caar groups))
					inline-bnode))))
	(if writer
	    (begin
	      (space port)
	      (write-predicate (rdf-triple-predicate (caar groups)) port)
	      (space port)
	      (writer port)
	      (write-pgroups-tail groups indentation inline-bnode port))
	    (write-pgroups groups indentation inline-bnode port))))))

(define (linear-object-writer o inline-bnode)
  (cond ((rdf-list->list o inline-bnode)
	 => (lambda (objects)
	      (cond ((null? objects)
		     (lambda (port)
		       (write-string "()" port)))
		    ((and (pair? objects)
			  (null? (cdr objects))
			  (linear-object-writer (car objects) inline-bnode))
		     => (lambda (write-elt)
			  (lambda (port)
			    (write-string "(" port)
			    (write-elt port)
			    (write-string ")" port))))
		    (else #f))))
	((rdf-bnode? o)
	 (and (not (inline-bnode o))
	      (lambda (port)
		(write-rdf/nt-bnode o port))))
	((uri? o)
	 (lambda (port)
	   (write-rdf/turtle-uri o port)))
	((rdf-graph? o)
	 (and (null? (rdf-graph-triples o))
	      (lambda (port)
		(write-string "{}" port))))
	((rdf-literal? o)
	 (lambda (port)
	   (write-rdf/turtle-literal o port)))
	(else
	 (error "Unknown RDF object:" o))))

(define (write-pgroups groups indentation inline-bnode port)
  (write-pgroup (car groups) indentation inline-bnode port)
  (write-pgroups-tail groups indentation inline-bnode port))

(define (write-pgroups-tail groups indentation inline-bnode port)
  (for-each (lambda (ts)
	      (write-string ";" port)
	      (write-pgroup ts indentation inline-bnode port))
	    (cdr groups)))

(define (write-pgroup ts indentation inline-bnode port)
  (write-indentation indentation port)
  (let ((p (rdf-triple-predicate (car ts)))
	(os (map rdf-triple-object ts)))
    (write-predicate p port)
    (let ((writer
	   (and (null? (cdr os))
		(linear-object-writer (car os) inline-bnode))))
      (if writer
	  (begin
	    (space port)
	    (writer port))
	  (let ((indentation (indent+ indentation)))
	    (write-object (car os) indentation inline-bnode port)
	    (for-each (lambda (o)
			(write-string "," port)
			(write-object o indentation inline-bnode port))
		      (cdr os)))))))

(define (write-object o indentation inline-bnode port)
  (write-indentation indentation port)
  (cond ((linear-object-writer o inline-bnode)
	 => (lambda (writer)
	      (writer port)))
	((rdf-graph? o)
	 (write-graph o indentation inline-bnode port))
	((rdf-list->list o inline-bnode)
	 => (lambda (os)
	      (write-string "(" port)
	      (let ((indentation (indent+ indentation)))
		(for-each (lambda (o)
			    (write-object o indentation inline-bnode port))
			  os))
	      (write-indentation indentation port)
	      (write-string ")" port)))
	((inline-bnode o)
	 => (lambda (ts)
	      (write-inline-bnode (cdr ts) indentation inline-bnode port)))
	(else
	 (error "Not an inline bnode:" o))))

(define (write-inline-bnode ts indentation inline-bnode port)
  (let ((groups (group-triples ts rdf-triple-predicate)))
    (write-string "[" port)
    (write-pgroups groups (indent+ indentation) inline-bnode port)
    (write-indentation indentation port)
    (write-string "]" port)))

(define (write-subject s indentation inline-bnode port)
  (cond ((uri? s)
	 (write-rdf/turtle-uri s port)
	 #t)
	((rdf-bnode? s)
	 (let ((ts (inline-bnode s)))
	   (if (and ts (= (car ts) 0))
	       (write-string "[]" port)
	       (write-rdf/nt-bnode s port)))
	 #t)
	((rdf-graph? s)
	 (if (null? (rdf-graph-triples s))
	     (begin
	       (write-string "{}" port)
	       #t)
	     (begin
	       (write-graph s indentation inline-bnode port)
	       #f)))
	(else
	 (error "Unknown RDF subject:" s))))

(define (write-graph graph indentation inline-bnode port)
  (write-string "{" port)
  (let ((indentation (indent+ indentation)))
    (do ((groups (groups-to-write (rdf-graph-triples graph) inline-bnode)
		 (cdr groups)))
	((not (pair? groups)))
      (write-indentation indentation port)
      (write-group (car groups) indentation inline-bnode port)
      (if (pair? (cdr groups))
	  (write-string "." port))))
  (write-indentation indentation port)
  (write-string "}" port))

(define (write-predicate p port)
  (if (eq? p rdf:type)
      (write-string "a" port)
      (write-rdf/turtle-uri p port)))

(define (write-rdf/turtle-literal literal #!optional port)
  (let ((text (rdf-literal-text literal))
	(port (if (default-object? port) (current-output-port) port)))
    (if (let ((type (rdf-literal-type literal)))
	  (or (eq? type xsd:boolean)
	      (eq? type xsd:decimal)
	      (eq? type xsd:double)
	      (eq? type xsd:integer)))
	(write-string text port)
	(begin
	  (write-literal-text text port)
	  (cond ((rdf-literal-type literal)
		 => (lambda (uri)
		      (write-string "^^" port)
		      (write-rdf/turtle-uri uri port)))
		((rdf-literal-language literal)
		 => (lambda (lang)
		      (write-string "@" port)
		      (write-symbol lang port))))))))

(define (write-literal-text text port)
  (if (string-find-next-char text #\newline)
      (let ((tport (open-input-string text)))
	(port/set-coding tport 'UTF-8)
	(write-string "\"\"\"" port)
	(let loop ()
	  (let ((char (read-char tport)))
	    (if (not (eof-object? char))
		(begin
		  (if (char=? char #\newline)
		      (newline port)
		      (write-literal-char char port))
		  (loop)))))
	(write-string "\"\"\"" port))
      (write-rdf/nt-literal-text text port)))

(define (write-rdf/turtle-uri uri #!optional port)
  (let ((port (if (default-object? port) (current-output-port) port)))
    (let* ((s (uri->string uri))
	   (end (string-length s)))
      (receive (prefix expansion)
	  (uri->rdf-prefix uri (port/rdf-prefix-registry port) #f)
	(if prefix
	    (let ((start (string-length expansion)))
	      (if (*match-string match:name s start end)
		  (begin
		    (write-string (symbol-name prefix) port)
		    (write-substring s start end port))
		  (write-rdf/nt-uri uri port)))
	    (write-rdf/nt-uri uri port))))))

(define (sort-triples triples)
  (sort triples
    (lambda (t1 t2)
      (let ((s1 (rdf-triple-subject t1))
	    (s2 (rdf-triple-subject t2)))
	(if (eq? s1 s2)
	    (let ((p1 (rdf-triple-predicate t1))
		  (p2 (rdf-triple-predicate t2)))
	      (if (eq? p1 p2)
		  #f			;???
		  (if (eq? p2 rdf:type)
		      #f
		      (if (eq? p1 rdf:type)
			  #t
			  (uri<? p1 p2)))))
	    (if (uri? s1)
		(if (uri? s2)
		    (uri<? s1 s2)
		    #t)
		(if (uri? s2)
		    #f
		    (rdf-bnode<? s1 s2))))))))

(define (group-triples ts accessor)
  (let loop ((ts ts) (groups '()))
    (if (pair? ts)
	(let grow ((ts (cdr ts)) (group (list (car ts))))
	  (if (and (pair? ts)
		   (eq? (accessor (car ts)) (accessor (car group))))
	      (grow (cdr ts) (cons (car ts) group))
	      (loop ts (cons (reverse! group) groups))))
	(reverse! groups))))

(define (uri<? a b)
  (string<? (uri->string a) (uri->string b)))

(define (rdf-bnode<? a b)
  (string<? (rdf-bnode-name a) (rdf-bnode-name b)))

(define (rdf-list->list node inline-bnode)
  (let loop ((node node))
    (if (eq? node rdf:nil)
	'()
	(let ((ts (inline-bnode node)))
	  (and ts
	       (eq? (rdf-triple-predicate (cadr ts)) rdf:type)
	       (eq? (rdf-triple-object (cadr ts)) rdf:List)
	       (pair? (cddr ts))
	       (eq? (rdf-triple-predicate (caddr ts)) rdf:first)
	       (pair? (cdddr ts))
	       (eq? (rdf-triple-predicate (cadddr ts)) rdf:rest)
	       (null? (cddddr ts))
	       (let ((rest (loop (rdf-triple-object (cadddr ts)))))
		 (and rest
		      (cons (rdf-triple-object (caddr ts)) rest))))))))

(define (space port)
  (write-char #\space port))

(define (write-symbol symbol port)
  (write-string (symbol-name symbol) port))

(define (write-indentation indentation port)
  (newline port)
  (let loop ((indentation indentation))
    (cond ((>= indentation 8)
	   (write-char #\tab port)
	   (loop (- indentation 8)))
	  ((>= indentation 1)
	   (write-char #\space port)
	   (loop (- indentation 1))))))

(define (indent+ indentation)
  (+ indentation 2))

(define (classify-list items n-classes classifier)
  (let ((classes (make-vector n-classes '())))
    (for-each (lambda (item)
		(let ((i (classifier item)))
		  (if i
		      (begin
			(if (not (and (exact-nonnegative-integer? i)
				      (< i n-classes)))
			    (error "Illegal classifier result:" i))
			(vector-set! classes i
				     (cons item (vector-ref classes i)))))))
	      items)
    (apply values (map reverse! (vector->list classes)))))

(define rdf:type
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))

(define rdf:List
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#List"))

(define rdf:first
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#first"))

(define rdf:rest
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"))

(define rdf:nil
  (string->uri "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil"))

(define xsd:integer
  (string->uri "http://www.w3.org/2001/XMLSchema#integer"))

(define xsd:double
  (string->uri "http://www.w3.org/2001/XMLSchema#double"))

(define xsd:decimal
  (string->uri "http://www.w3.org/2001/XMLSchema#decimal"))

(define xsd:boolean
  (string->uri "http://www.w3.org/2001/XMLSchema#boolean"))