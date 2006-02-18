#| -*-Scheme-*-

$Id: rdf-nt.scm,v 1.1 2006/02/18 04:31:47 cph Exp $

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

;;;; Codec for RDF N-triples syntax

(declare (usual-integrations))

;;;; Decoder

(define (read-rdf/nt port)
  (let loop ()
    (let ((line (read-line port)))
      (if (eof-object? line)
	  line
	  (let ((v
		 (or (parse-one-line (string->parser-buffer line))
		     (error "Failed to parse RDF/NT line:" line))))
	    (if (fix:= (vector-length v) 0)
		(loop)
		(vector-ref v 0)))))))

(define parse-one-line
  (*parser
   (complete
    (seq (noise match-ws*)
	 (? (alt parse-triple parse-comment))))))

(define parse-triple
  (*parser
   (encapsulate (lambda (v)
		  (make-rdf-triple (vector-ref v 0)
				   (vector-ref v 1)
				   (vector-ref v 2)))
     (seq (alt parse-uri-ref parse-node-id)
	  (noise match-ws+)
	  parse-uri-ref
	  (noise match-ws+)
	  (alt parse-uri-ref parse-node-id parse-literal)
	  (noise match-ws*)
	  "."
	  (noise match-ws*)))))

(define parse-comment
  (*parser (noise (seq "#" (* (char-set char-set:character))))))

(define parse-uri-ref
  (*parser (seq #\< parse-absolute-uri #\>)))

(define parse-node-id
  (*parser
   (encapsulate (lambda (v) (make-rdf-bnode (vector-ref v 0)))
     (seq "_:"
	  (match match-bnode-name)))))

(define match-bnode-name
  (*matcher
   (seq (char-set char-set:name-head)
	(* (char-set char-set:name-tail)))))

(define parse-literal
  (*parser
   (encapsulate (lambda (v)
		  (make-rdf-literal (vector-ref v 0) (vector-ref v 1)))
     (seq #\"
	  parse-string
	  #\"
	  (alt (seq #\@ (match match-language))
	       (seq "^^" parse-uri-ref)
	       (values #f))))))

(define (parse-string b)
  (let ((port (open-output-string)))

    (define (loop)
      (let ((p (get-parser-buffer-pointer b)))
	(if (match-parser-buffer-char-in-set b char-set:unescaped)
	    (begin
	      (let loop ()
		(if (match-parser-buffer-char-in-set b char-set:unescaped)
		    (loop)))
	      (call-with-parser-buffer-tail b p
		(lambda (string start end)
		  (write-substring string start end port))))))
      (let ((char
	     (let ((p (get-parser-buffer-pointer b)))
	       (and (match-parser-buffer-char b #\\)
		    (cond ((match-parser-buffer-char b #\t) #\tab)
			  ((match-parser-buffer-char b #\n) #\newline)
			  ((match-parser-buffer-char b #\r) #\return)
			  ((match-parser-buffer-char b #\") #\")
			  ((match-parser-buffer-char b #\\) #\\)
			  ((or (and (match-parser-buffer-char b #\u)
				    (match-hex 4))
			       (and (match-parser-buffer-char b #\U)
				    (match-hex 8)))
			   (integer->char
			    (call-with-parser-buffer-tail b p
			      (lambda (string start end)
				(substring->number string (+ start 2) end
						   16 #t)))))
			  (else #f))))))
	(if char
	    (begin
	      (write-char char port)
	      (loop))
	    (vector (get-output-string port)))))

    (define (match-hex n)
      (let loop ((i 0))
	(if (fix:< i n)
	    (and (match-parser-buffer-char-in-set b char-set:hex)
		 (loop (fix:+ i 1)))
	    #t)))

    (port/set-coding port 'UTF-8)
    (loop)))

(define match-language
  (*matcher
   (seq (+ (char-set char-set:language-head))
	(* (seq #\- (+ (char-set char-set:language-tail)))))))

(define match-ws*
  (*matcher (* (char-set char-set:ws))))

(define match-ws+
  (*matcher (+ (char-set char-set:ws))))

(define char-set:ws
  (char-set #\space #\tab))

(define char-set:character
  (ascii-range->char-set #x20 #x7F))

(define char-set:hex
  (char-set-union (ascii-range->char-set #x30 #x3A)
		  (ascii-range->char-set #x41 #x47)))

(define char-set:unescaped
  (char-set-difference char-set:character (char-set #\" #\\)))

(define char-set:name-head
  (char-set-union (ascii-range->char-set #x41 #x5B)
		  (ascii-range->char-set #x61 #x7B)))

(define char-set:name-tail
  (char-set-union char-set:name-head
		  (ascii-range->char-set #x30 #x3A)))

(define char-set:language-head
  (ascii-range->char-set #x61 #x7B))

(define char-set:language-tail
  (char-set-union char-set:language-head
		  (ascii-range->char-set #x30 #x3A)))

;;;; Encoder

(define (write-rdf/nt triple port)
  (let ((s (rdf-triple-subject triple)))
    (cond ((uri? s) (write-uri-ref s port))
	  ((rdf-bnode? s) (write-bnode s port))))
  (write-char #\space port)
  (write-uri-ref (rdf-triple-predicate triple) port)
  (write-char #\space port)
  (let ((o (rdf-triple-object triple)))
    (cond ((uri? o) (write-uri-ref o port))
	  ((rdf-bnode? o) (write-bnode o port))
	  ((rdf-literal? o) (write-literal o port))))
  (write-char #\space port)
  (write-char #\. port)
  (newline port))

(define (write-uri-ref uri port)
  (write-char #\< port)
  (write-uri uri port)
  (write-char #\> port))

(define (write-bnode bnode port)
  (write-string "_:" port)
  (write-string (rdf-bnode-name bnode) port))

(define (write-literal literal port)
  (write-char #\" port)
  (write-literal-text (rdf-literal-text literal) port)
  (write-char #\" port)
  (cond ((rdf-literal-type literal)
	 => (lambda (uri)
	      (write-string "^^" port)
	      (write-uri-ref uri port)))
	((rdf-literal-language literal)
	 => (lambda (lang)
	      (write-char #\@ port)
	      (write-string lang port)))))

(define (write-literal-text text port)
  (let ((text (open-input-string text)))
    (port/set-coding text 'UTF-8)
    (let loop ()
      (let ((char (read-char text)))
	(if (not (eof-object? char))
	    (begin
	      (write-literal-char char port)
	      (loop)))))))

(define (write-literal-char char port)
  (if (char-set-member? char-set:unescaped char)
      (write-char char port)
      (begin
	(write-char #\\ port)
	(if (or (char=? char #\")
		(char=? char #\\))
	    (write-char char port)
	    (let ((n (char->integer char)))
	      (cond ((fix:= n #x9) (write-char #\t port))
		    ((fix:= n #xA) (write-char #\n port))
		    ((fix:= n #xD) (write-char #\r port))
		    ((fix:< n #x10000) (write-hex n 4 port))
		    (else (write-hex n 8 port))))))))

(define (write-hex n digits port)
  (let loop ((n n) (m (expt 16 digits)))
    (if (> m 1)
	(begin
	  (write-char (string-ref "0123456789ABCDEF" (quotient n m)) port)
	  (loop (remainder n m) (quotient m 16))))))