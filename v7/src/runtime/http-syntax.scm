#| -*-Scheme-*-

$Id: http-syntax.scm,v 1.4 2008/09/21 22:20:14 cph Exp $

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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; HTTP syntax
;;; package: (runtime http-syntax)

(declare (usual-integrations))

;;;; Versions

(define (http-version? object)
  (and (pair? object)
       (exact-nonnegative-integer? (car object))
       (exact-nonnegative-integer? (cdr object))))

(define-guarantee http-version "HTTP version")

(define (make-http-version major minor) (cons major minor))
(define (http-version-major v) (car v))
(define (http-version-minor v) (cdr v))

(define (http-version=? v1 v2)
  (and (= (car v1) (car v2))
       (= (cdr v1) (cdr v2))))

(define (http-version<? v1 v2)
  (or (< (car v1) (car v2))
      (and (= (car v1) (car v2))
	   (< (cdr v1) (cdr v2)))))

(define parse-http-version
  (*parser
   (encapsulate* make-http-version
     (seq "HTTP/"
	  (map string->number
	       (match (+ (char-set char-set:numeric))))
	  "."
	  (map string->number
	       (match (+ (char-set char-set:numeric))))))))

(define (write-http-version version port)
  (write-string "HTTP/" port)
  (write (car version) port)
  (write-string "." port)
  (write (cdr version) port))

(define-deferred http-version:1.0 (make-http-version 1 0))
(define-deferred http-version:1.1 (make-http-version 1 1))

;;;; Status

(define (http-status? object)
  (and (exact-nonnegative-integer? object)
       (< object 1000)))

(define-guarantee http-status "HTTP status code")

(define (http-status-major status)
  (modulo status 100))

(define parse-http-status
  (*parser
   (map string->number
	(match (seq (char-set char-set:numeric)
		    (char-set char-set:numeric)
		    (char-set char-set:numeric))))))

(define (write-http-status object port)
  (write-string (string-pad-left (number->string object) 3 #\0) port))

;;;; Headers

(define-record-type <http-header>
    (%make-header name value parsed-value)
    http-header?
  (name http-header-name)
  (value http-header-value)
  (parsed-value http-header-parsed-value))

(define-guarantee http-header "HTTP header field")

(set-record-type-unparser-method! <http-header>
  (simple-unparser-method 'HTTP-HEADER
    (lambda (header)
      (list (http-header-name header)))))

(define (make-http-header name value)
  (guarantee-http-token name 'MAKE-HTTP-HEADER)
  (let ((defn (header-value-defn name)))
    (if defn
	(if ((hvdefn-predicate defn) value)
	    (%make-header name
			  (call-with-output-string
			    (lambda (port)
			      ((hvdefn-writer defn) value port)))
			  value)
	    (begin
	      (guarantee-http-text value 'MAKE-HTTP-HEADER)
	      (%make-header name value
			    (%call-parser (hvdefn-parser defn) value))))
	(begin
	  (guarantee-http-text value 'MAKE-HTTP-HEADER)
	  (%make-header name value (%unparsed-value))))))

(define (convert-http-headers headers #!optional caller)
  (guarantee-list headers caller)
  (map (lambda (header)
	 (cond ((http-header? header)
		header)
	       ((and (pair? header)
		     (http-token? (car header))
		     (string? (cdr header)))
		(make-http-header (car header) (cdr header)))
	       ((and (pair? header)
		     (http-token? (car header))
		     (pair? (cdr header))
		     (string? (cadr header))
		     (null? (cddr header)))
		(make-http-header (car header) (cadr header)))
	       (else
		(error:not-http-header header caller))))
       headers))

(define (guarantee-http-headers object #!optional caller)
  (guarantee-list-of-type object http-header? "HTTP headers" caller))

(define (http-header name headers error?)
  (let ((h
	 (find (lambda (header)
		 (eq? (http-header-name header) name))
	       headers)))
    (if (and (not h) error?)
	(error:bad-range-argument name 'HTTP-HEADER))
    h))

;;;; Tokens and text

(define (http-token? object)
  (and (interned-symbol? object)
       (string-is-http-token? (symbol-name object))))

(define-guarantee http-token "HTTP token")

(define (write-http-token token port)
  (write-string (symbol-name token) port))

(define (http-token-string? object)
  (and (string? object)
       (string-is-http-token? object)))

(define-guarantee http-token-string "HTTP token string")

(define (string-is-http-token? string)
  (*match-string match-http-token string))

(define parse-http-token
  (*parser (map intern (match match-http-token))))

(define match-http-token
  (*matcher (+ (char-set char-set:http-token))))

(define (http-text? object)
  (string? object))

(define-guarantee http-text "HTTP text")

(define (write-text string port)
  (if (string-is-http-token? string)
      (write-string string port)
      (write-quoted-string string port)))

(define (write-quoted-string string port)
  (write-char #\" port)
  (%write-with-quotations string char-set:http-qdtext port)
  (write-char #\" port))

(define (write-comment string port)
  (write-char #\( port)
  (%write-with-quotations string char-set:http-text port)
  (write-char #\) port))

(define (%write-with-quotations string unquoted port)
  (let ((n (string-length string)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (let ((char (string-ref string i)))
	(if (not (char-set-member? unquoted char))
	    (write-char #\\ port))
	(write-char char port)))))

(define (comment? string)
  (let ((port (open-input-string string)))
    (let loop ((level 0))
      (let ((char (read-char port)))
	(cond ((eof-object? char) (= level 0))
	      ((char=? char #\() (loop (+ level 1)))
	      ((char=? char #\)) (loop (- level 1)))
	      (else (loop level)))))))

;;;; Header I/O

(define (read-http-headers port)
  (let loop ((headers '()))
    (let ((string (read-rfc2822-folded-line port)))
      (if string
	  (loop (cons (parse-header string) headers))
	  (reverse! headers)))))

(define parse-header
  (let ((parser
	 (*parser
	  (seq (match (+ (char-set char-set:http-token)))
	       #\:
	       (noise (* (char-set char-set:wsp)))
	       (match (* (char-set char-set:http-text)))))))
    (lambda (string)
      (let ((v (*parse-string parser string)))
	(if (not v)
	    (error "Ill-formed HTTP header:" string))
	(let ((name (intern (vector-ref v 0)))
	      (value (vector-ref v 1)))
	  (%make-header name
			value
			(let ((defn (header-value-defn name)))
			  (if defn
			      (%call-parser (hvdefn-parser defn) value)
			      (%unparsed-value)))))))))

(define (%call-parser parser value)
  (parser value
	  (lambda (parsed-value)
	    parsed-value)
	  (lambda ()
	    (warn "Ill-formed HTTP header value:" value)
	    (%unparsed-value))))

(define (%unparsed-value)
  (default-object))

(define (write-http-headers headers port)
  (guarantee-http-headers headers 'WRITE-HTTP-HEADERS)
  (for-each (lambda (header)
	      (let ((name (http-header-name header)))
		(let ((defn (header-value-defn name)))
		  (if defn
		      (write-string (hvdefn-name defn) port)
		      (write-http-token name port))))
	      (write-string ": " port)
	      (write-string (http-header-value header) port)
	      (newline port))
	    headers)
  (newline port))

;;;; Header element types

(define (lp:comma-list min-length parse-item)
  (let ((ugh (list-parser (* (alt #\, lp:lws)))))
    (list-parser
     (qualify (lambda (items)
		(>= (length items) min-length))
       (encapsulate list
	 (alt ugh
	      (? parse-item
		 ugh
		 (* #\,
		    (? lp:lws)
		    parse-item
		    ugh))))))))

(define (write-comma-list write-elt elts port)
  (if (pair? elts)
      (begin
	(write-elt (car elts) port)
	(for-each (lambda (elt)
		    (write-string ", " port)
		    (write-elt elt port))
		  (cdr elts)))))

(define lp:token
  (list-parser (map intern lp:token-string)))

(define lp:token-cs
  (list-parser (map string->symbol lp:token-string)))

(define lp:token-string
  (list-parser (map token-token->string (match-if token-token?))))

(define lp:token+
  (lp:comma-list 1 lp:token))

(define (token+? object)
  (and (pair? object)
       (token*? object)))

(define lp:token-cs*
  (lp:comma-list 0 lp:token-cs))

(define (token*? object)
  (list-of-type? object http-token?))

(define (write-token* tokens port)
  (write-comma-list write-http-token tokens port))

(define lp:text
  (list-parser
   (alt lp:token-string
	lp:quoted-string)))

(define lp:quoted-string
  (list-parser
   (map quoted-string-token->string
	(match-if quoted-string-token?))))

(define lp:comment
  (list-parser
   (map comment-token->string
	(match-if comment-token?))))

(define lp:lws
  (list-parser (noise-if lws-token?)))

(define lp:*
  (list-parser (qualify (token= '*) lp:token)))

(define-integrable (token= token)
  (lambda (token*)
    (eq? token* token)))

;;;; Parameters

(define lp:parameter*
  (list-parser
   (encapsulate list
     (* lp:semicolon
	lp:parameter))))

(define lp:parameter
  (list-parser
   (encapsulate cons
     (seq lp:token
	  #\=
	  lp:text))))

(define lp:parameter%*
  (list-parser
   (encapsulate list
     (* lp:semicolon
	lp:parameter%))))

(define lp:parameter%
  (list-parser
   (encapsulate cons
     (seq lp:token
	  (alt (seq #\= lp:text)
	       (values #f))))))

(define lp:semicolon
  (list-parser
   (seq (? lp:lws)
	#\;
	(? lp:lws))))

(define (write-semicolon-sep port)
  (write-char #\; port)
  (write-char #\space port))

(define (http-parameters? object)
  (list-of-type? object parameter?))

(define (parameter? object)
  (pair-of-type? object
		 http-token?
		 http-text?))

(define (parameter%*? object)
  (list-of-type? object parameter%?))

(define (parameter%+? object)
  (list+-of-type? object parameter%?))

(define (parameter%? object)
  (pair-of-type? object
		 http-token?
		 (lambda (value)
		   (or (not value)
		       (http-text? value)))))

(define (write-parameter* parameters port)
  (for-each (lambda (param)
	      (write-semicolon-sep port)
	      (write-parameter param port))
	    parameters))

(define (write-parameter param port)
  (write-http-token (car param) port)
  (if (cdr param)
      (begin
	(write-char #\= port)
	(write-text (cdr param) port))))

(define lp:qparam
  (list-parser
   (qualify (lambda (p)
	      (eq? (car p) 'Q))
     lp:parameter)))

(define (qparam? object)
  (and (parameter? object)
       (eq? (car object) 'Q)))

(define lp:opt-qparam
  (list-parser
   (encapsulate list
     (? (seq lp:semicolon
	     lp:qparam)))))

(define (opt-qparam? object)
  (or (null? object)
      (and (pair? object)
	   (qparam? (car object))
	   (null? (cdr object)))))

;;; Slight misnomer here.  This "accept-params" represents the pattern
;;;     *( ";" parameter ) [accept-params]

(define lp:accept-params
  (list-parser
   (encapsulate list
     (seq (* (seq lp:semicolon
		  (disqualify qparam? lp:parameter)))
	  (? lp:semicolon
	     lp:qparam
	     (* lp:semicolon
		lp:parameter%))))))

(define (accept-params? value)
  (and (list? value)
       (let loop ((params value))
	 (if (pair? params)
	     (and (parameter? (car params))
		  (if (qparam? (car params))
		      (every parameter%? (cdr params))
		      (loop (cdr params))))
	     #t))))

(define (params-are-expectation? object)
  (and (pair? object)
       (or (cdar object)
	   (null? (cdr object)))))

(define lp:nonnegative-integer
  (list-parser
   (map string->number
	(qualify (lambda (string)
		   (*match-string (*matcher (+ (char-set char-set:numeric)))
				  string))
		 lp:token-string))))

(define lp:mime-type
  (list-parser
   (encapsulate make-mime-type
     (seq lp:token
	  #\/
	  lp:token))))

(define-deferred parser:http-date
  (let ((parser:gmttime (parser:ctime 0)))
    (*parser
     (map decoded-time->utc
	  (alt parser:rfc2822-time
	       parser:rfc850-time
	       parser:gmttime)))))

(define (http-date? value)
  (and (decoded-time? value)
       (eqv? (decoded-time/zone value) 0)))

(define (write-http-date value port)
  (write-decoded-time-as-http value port))

(define lp:hostport
  (list-parser
   (encapsulate (lambda (host port)
		  (*parse-string parse-hostport
				 (if port
				     (string-append host ":" port)
				     host)))
     (seq lp:token-string
	  (alt (seq #\: lp:token-string)
	       (values #f))))))

(define parse-hostport
  (*parser (encapsulate* cons url:parse:hostport)))

(define (hostport? value)
  (pair-of-type? value
		 string?
		 (lambda (port)
		   (or (not port)
		       (exact-nonnegative-integer? port)))))

(define (write-hostport value port)
  (write-string (car value) port)
  (if (cdr value)
      (begin
	(write-char #\: port)
	(write (cdr value) port))))

(define (language-range? object)
  (and (http-token? object)
       (token-is-language-range? object)))

(define (token-is-language-range? token)
  (or (eq? token '*)
      (token-is-language-tag? token)))

(define lp:language-tag
  (list-parser (qualify token-is-language-tag? lp:token)))

(define (language-tag? object)
  (and (http-token? object)
       (token-is-language-tag? object)))

(define (token-is-language-tag? token)
  (*match-string (let ((segment
			(*matcher
			 (n*m 1 8 (char-set char-set:alpha)))))
		   (*matcher
		    (seq segment
			 (* (seq #\- segment)))))
		 (symbol-name token)))

(define lp:entity-tag
  (list-parser
   (encapsulate cons
     (seq (alt (encapsulate (lambda () 'WEAK)
		 (seq (qualify (lambda (s)
				 (string=? s "W"))
			lp:token-string)
		      #\/))
	       (values 'STRONG))
	  lp:quoted-string))))

(define (entity-tag? value)
  (pair-of-type? value
		 (lambda (type)
		   (or (eq? type 'WEAK)
		       (eq? type 'STRONG)))
		 http-text?))

(define (write-entity-tag value port)
  (if (eq? (car value) 'WEAK)
      (write-string "W/" port))
  (write-quoted-string (cdr value) port))

(define lp:entity-tag+
  (lp:comma-list 1 lp:entity-tag))

(define (entity-tag+? value)
  (list+-of-type? value entity-tag?))

(define (write-entity-tag+ value port)
  (write-comma-list write-entity-tag value port))

(define lp:bytes-unit
  (list-parser (qualify (token= 'BYTES) lp:token)))

(define (bytes-unit? value)
  (eq? value 'BYTES))

(define lp:byte-range-set
  (lp:comma-list 1
    (list-parser
     (transform (lambda (string)
		  (let ((v
			 (*parse-string
			  (let ((match-num
				 (*matcher (+ (char-set char-set:numeric)))))
			    (*parser
			     (encapsulate* cons
			       (alt (seq (match match-num)
					 #\-
					 (alt (match match-num)
					      (values #f)))
				    (seq (values #f)
					 #\-
					 (match match-num))))))
			  string)))
		    (and v
			 (list (vector-ref v 0)))))
       lp:token-string))))

(define (byte-range-set? value)
  (list+-of-type? value
    (lambda (p)
      (and (pair? p)
	   (or (and (exact-nonnegative-integer? (car p))
		    (exact-nonnegative-integer? (cdr p)))
	       (and (exact-nonnegative-integer? (car p))
		    (not (cdr p)))
	       (and (not (car p))
		    (exact-nonnegative-integer? (cdr p))))))))

(define (write-byte-range-set value port)
  (write-comma-list (lambda (p port)
		      (if (car p)
			  (begin
			    (write (car p) port)
			    (write-char #\- port)
			    (if (cdr p)
				(write (cdr p) port)))
			  (begin
			    (write-char #\- port)
			    (write (cdr p) port))))
		    value
		    port))

(define lp:product
  (list-parser
   (encapsulate cons
     (seq lp:token-string
	  (alt (seq #\/
		    lp:token-string)
	       (values #f))))))

(define (product? value)
  (pair-of-type? value
		 http-token-string?
		 (lambda (x)
		   (or (not x)
		       (http-token-string? x)))))

(define (write-product value port)
  (write-string (car value) port)
  (if (cdr value)
      (begin
	(write-char #\/ port)
	(write-string (cdr value) port))))

(define lp:product/comment-list
  (list-parser
   (encapsulate list
     (seq (alt lp:product
	       lp:comment)
	  (* (seq (? lp:lws)
		  (alt lp:product
		       lp:comment)))))))

(define (product/comment-list? value)
  (list-of-type? value
    (lambda (elt)
      (or (product? elt)
	  (comment? elt)))))

(define (write-product/comment-list value port)
  (let ((write-elt
	 (lambda (elt port)
	   (if (product? elt)
	       (write-product elt port)
	       (write-comment elt port)))))
    (if (pair? value)
	(begin
	  (write-elt (car value) port)
	  (for-each (lambda (elt)
		      (write-char #\space port)
		      (write-elt elt port))
		    (cdr value))))))

;;;; Tokenization

(define (string->tokens string)
  (tokenizer-state:tokenize (open-input-string string)
			    (let ((head '())
				  (tail '()))
			      (lambda (#!optional token)
				(if (default-object? token)
				    (let ((tokens head))
				      (set! head '())
				      (set! tail '())
				      tokens)
				    (let ((tail* (list token)))
				      (if (pair? tail)
					  (set-cdr! tail tail*)
					  (set! head tail*))
				      (set! tail tail*)
				      unspecific))))
			    (let ((port (open-output-string)))
			      (lambda (#!optional char)
				(if (default-object? char)
				    (get-output-string! port)
				    (write-char char port))))))

(define (make-state eof-action else-action . bindings)
  (let ((table (make-vector #x100 else-action)))
    (do ((bindings bindings (cddr bindings)))
	((not (pair? bindings)))
      (let ((key (car bindings))
	    (handler (cadr bindings)))
	(cond ((char? key)
	       (vector-set! table (char->integer key) handler))
	      ((char-set? key)
	       (for-each (lambda (char)
			   (let ((i (char->integer char)))
			     (if (eq? (vector-ref table i) else-action)
				 (vector-set! table i handler))))
			 (char-set-members key)))
	      (else
	       (error:wrong-type-argument key "char or char-set")))))
    (lambda (port emit fifo)
      (let ((char (read-char port)))
	(if (eof-object? char)
	    (eof-action port emit fifo)
	    ((vector-ref table (char->integer char)) char port emit fifo))))))

(define-integrable (lws-token? object)
  (eqv? object #\space))

(define (separator-token? object)
  (and (char? object)
       (char-set-member? char-set:http-separators object)))

(define (separator-token->char token)
  token)

(define (token-token? object)
  (string? object))

(define (token-token->string token)
  token)

(define (quoted-string-token? object)
  (pair-of-type? object
		 (lambda (tag) (eq? tag 'QUOTED-STRING))
		 string?))

(define (quoted-string-token->string token)
  (cdr token))

(define (comment-token? object)
  (pair-of-type? object
		 (lambda (tag) (eq? tag 'COMMENT))
		 string?))

(define (comment-token->string token)
  (cdr token))

(define-syntax define-tokenizer-state
  (sc-macro-transformer
   (lambda (form env)
     env
     (if (and (syntax-match? '(SYMBOL ('EOF + DATUM)
				      + (EXPRESSION + DATUM))
			     (cdr form))
	      (let loop ((clauses (cddr form)))
		(and (pair? clauses)
		     (if (eq? (caar clauses) 'ELSE)
			 (null? (cdr clauses))
			 (loop (cdr clauses))))))
	 (let ((state (cadr form))
	       (eof-clause (caddr form))
	       (normal-clauses (except-last-pair (cdddr form)))
	       (else-clause (last (cdddr form))))

	   (define (compile-rhs clause vars)
	     (let ((rhs (cdr clause)))
	       `(LAMBDA (,@vars PORT EMIT FIFO)
		  ,@vars PORT EMIT FIFO
		  ,@(map compile-action (except-last-pair rhs))
		  ,(let ((ns (last rhs)))
		     (cond ((eq? ns 'DONE)
			    '(EMIT))
			   ((symbol? ns)
			    `(,(state->name ns) PORT EMIT FIFO))
			   (else ns))))))

	   (define (compile-action action)
	     (cond ((eq? action 'SAVE-CHAR) '(FIFO CHAR))
		   ((eq? action 'UNREAD-CHAR) '(UNREAD-CHAR CHAR PORT))
		   (else action)))

	   (define (state->name name)
	     (symbol 'TOKENIZER-STATE: name))

	   `(DEFINE-DEFERRED ,(state->name state)
	      (MAKE-STATE ,(if eof-clause
			       (compile-rhs eof-clause '())
			       `#F)
			  ,(compile-rhs else-clause '(CHAR))
			  ,@(append-map (lambda (clause)
					  `(,(car clause)
					    ,(compile-rhs clause '(CHAR))))
					normal-clauses))))
	 (ill-formed-syntax form)))))

(define-deferred char-set:http-separators
  (string->char-set "()<>@,;:\\\"/[]?={} \t"))

(define-deferred char-set:http-token
  (char-set-difference char-set:ascii
		       (char-set-union char-set:ctls
				       char-set:http-separators)))

(define-deferred char-set:http-text
  (char-set-invert char-set:ctls))

(define-deferred char-set:http-ctext
  (char-set-difference char-set:http-text (char-set #\( #\))))

(define-deferred char-set:http-qdtext
  (char-set-difference char-set:http-text (char-set #\")))

(define-deferred char-set:alpha
  (char-set-union (ascii-range->char-set #x41 #x5B)
		  (ascii-range->char-set #x61 #x7B)))

(define-tokenizer-state tokenize
  (eof done)
  (char-set:wsp in-wsp)
  (char-set:http-token save-char in-token)
  (#\" in-quoted-string)
  (#\( in-comment)
  (#\) (error "Illegal input char:" char))
  (char-set:http-separators (emit char) tokenize)
  (else (error "Illegal input char:" char)))

(define-tokenizer-state in-wsp
  (eof done)
  (char-set:wsp in-wsp)
  (else unread-char (emit #\space) tokenize))

(define-tokenizer-state in-token
  (eof (emit (fifo)) done)
  (char-set:http-token save-char in-token)
  (else (emit (fifo)) unread-char tokenize))

(define-tokenizer-state in-quoted-string
  (eof (error "Premature EOF in quoted string."))
  (char-set:http-qdtext save-char in-quoted-string)
  (#\\ in-quoted-string-quotation)
  (#\" (emit (cons 'QUOTED-STRING (fifo))) tokenize)
  (else (error "Illegal char in quoted string:" char)))

(define-tokenizer-state in-quoted-string-quotation
  (eof (error "Premature EOF in quoted string."))
  (else save-char in-quoted-string))

(define (tokenizer-state:in-comment port emit fifo)
  ;; Comments aren't context-free, so tokenize them more carefully.
  (let ((rc
	 (lambda ()
	   (let ((char (read-char port)))
	     (if (eof-object? char)
		 (error "Premature EOF while reading comment."))
	     char))))
    (let loop ((level 1))
      (let ((char (rc)))
	(cond ((char=? char #\()
	       (fifo char)
	       (loop (+ level 1)))
	      ((char=? char #\))
	       (if (= level 1)
		   (begin
		     (emit (cons 'COMMENT (fifo)))
		     (tokenizer-state:tokenize port emit fifo))
		   (begin
		     (fifo char)
		     (loop (- level 1)))))
	      ((char=? char #\\)
	       (fifo (rc))
	       (loop level))
	      ((char-set-member? char-set:http-text char)
	       (fifo char)
	       (loop level))
	      (else
	       (error "Illegal char in comment:" char)))))))

;;;; Header definitions

(define-syntax define-header
  (sc-macro-transformer
   (lambda (form env)
     (if (syntax-match? '(+ EXPRESSION) (cdr form))
	 `(ADD-BOOT-INIT!
	   (LAMBDA ()
	     (DEFINE-HEADER-1
	       ,@(map (lambda (expr)
			(close-syntax expr env))
		      (cdr form)))))
	 (ill-formed-syntax form)))))

(define (define-header-1 name parser predicate writer)
  (let ((key (intern name))
	(defn (make-hvdefn name parser predicate writer)))
    (let ((p (assq key header-value-defns)))
      (if p
	  (set-cdr! p defn)
	  (begin
	    (set! header-value-defns
		  (cons (cons key defn)
			header-value-defns))
	    unspecific)))))

(define (header-value-defn name)
  (let ((p (assq name header-value-defns)))
    (and p
	 (cdr p))))

(define header-value-defns '())

(define-structure hvdefn
  (name #f read-only #t)
  (parser #f read-only #t)
  (predicate #f read-only #t)
  (writer #f read-only #t))

(define ((tokenized-parser parser) string win lose)
  (parser (string->tokens string)
	  (lambda (items vals lose)
	    (if (null? items)
		(begin
		  (if (not (= (structure-parser-values-length vals) 1))
		      (error
		       "Wrong number of values from HTTP header parser."))
		  (win (structure-parser-values-ref vals 0)))
		(lose)))
	  lose))

(define ((direct-parser parser) string win lose)
  (let ((v (*parse-string parser string)))
    (if v
	(begin
	  (if (not (fix:= (vector-length v) 1))
	      (error "Wrong number of values from HTTP header parser."))
	  (win (vector-ref v 0)))
	(lose))))

;;;; General headers

(define-header "Cache-Control"
  (tokenized-parser (lp:comma-list 1 lp:parameter%))
  parameter%+?
  (lambda (value port) (write-comma-list write-parameter value port)))

(define-header "Connection"
  (tokenized-parser lp:token+)
  token+?
  write-token*)

(define-header "Date"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-header "Pragma"
  (tokenized-parser (lp:comma-list 1 lp:parameter%))
  parameter%+?
  (lambda (value port) (write-comma-list write-parameter value port)))

(define-header "Trailer"
  (tokenized-parser lp:token+)
  token+?
  write-token*)

(define-header "Transfer-Encoding"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate cons
	(seq lp:token
	     lp:parameter*)))))
  (lambda (value)
    (list-of-type? value
      (lambda (elt)
	(pair-of-type? elt
		       http-token?
		       http-parameters?))))
  (lambda (value port)
    (write-comma-list (lambda (elt port)
			(write-http-token (car elt) port)
			(write-parameter* (cdr elt) port))
		      value
		      port)))

(define-header "Upgrade"
  (tokenized-parser (lp:comma-list 1 lp:product))
  (lambda (value) (list+-of-type? value product?))
  (lambda (value port) (write-comma-list write-product value port)))

(define-header "Via"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate cons
	(seq (encapsulate cons
	       (seq (alt (seq lp:token #\/)
			 (values #f))
		    lp:token))
	     lp:lws
	     (alt lp:hostport
		  lp:token)
	     (? (noise (seq (? lp:lws)
			    lp:comment
			    (? lp:lws)))))))))
  (lambda (value)
    (pair-of-type? value
		   (lambda (received-protocol)
		     (pair-of-type? received-protocol
				    (lambda (name)
				      (or (not name)
					  (http-token? name)))
				    http-token?))
		   (lambda (received-by)
		     (or (hostport? received-by)
			 (http-token? received-by)))))
  (lambda (value port)
    (let ((received-protocol (car value)))
      (if (car received-protocol)
	  (begin
	    (write-http-token (car received-protocol) port)
	    (write-char #\/ port)))
      (write-http-token (cdr received-protocol) port))
    (let ((received-by (cdr value)))
      (if (hostport? received-by)
	  (write-hostport received-by port)
	  (write-http-token received-by port)))))

(define-header "Warning"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate vector
	(seq (qualify (lambda (n) (< n 1000)) lp:nonnegative-integer)
	     #\space
	     (alt lp:hostport
		  lp:token)
	     #\space
	     lp:quoted-string
	     (alt (seq #\space
		       (transform (lambda (string)
				    (let ((dt
					   (*parse-string parser:http-date
							  string)))
				      (and dt
					   (list dt))))
			 lp:quoted-string))
		  (values #f)))))))
  (lambda (value)
    (vector-of-types? value
		      (lambda (n)
			(and (exact-nonnegative-integer? n)
			     (< n 1000)))
		      (lambda (h)
			(or (hostport? h)
			    (http-token? h)))
		      http-text?
		      (lambda (dt)
			(or (not dt)
			    (decoded-time? dt)))))
  (lambda (value port)
    (receive (code agent text date) (vector->values value)
      (write-string (string-pad-left (number->string code) 3 #\0) port)
      (write-char #\space port)
      (if (hostport? agent)
	  (write-hostport agent port)
	  (write-http-token agent port))
      (write-char #\space port)
      (write-quoted-string text port)
      (if date
	  (begin
	    (write-char #\space port)
	    (write-quoted-string (call-with-output-string
				   (lambda (port)
				     (write-http-date date port)))
				 port))))))

;;;; Request headers

(define-header "Accept"
  (tokenized-parser
   (lp:comma-list 0
     (list-parser
      (encapsulate cons
	(seq (encapsulate (lambda (t1 t2)
			    (if (eq? t2 '*)
				(if (eq? t1 '*)
				    #t
				    t1)
				(make-mime-type t1 t2)))
	       (seq lp:token
		    #\/
		    lp:token))
	     lp:accept-params)))))
  (lambda (value)
    (list-of-type? value
      (lambda (elt)
	(pair-of-type? elt
		       (lambda (mt)
			 (or (mime-type? mt)
			     (http-token? mt)
			     (eq? mt #t)))
		       accept-params?))))
  (lambda (value port)
    (write-comma-list (lambda (elt port)
			(let ((mt (car elt)))
			  (cond ((mime-type? mt)
				 (write-mime-type mt port))
				((http-token? mt)
				 (write-http-token mt port)
				 (write-string "/*" port))
				(else
				 (write-string "*/*" port))))
			(write-parameter* (cdr elt) port))
		      value
		      port)))

(define-header "Accept-Charset"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate cons
	(seq lp:token
	     lp:opt-qparam)))))
  (lambda (value)
    (list+-of-type? value
      (lambda (elt)
	(pair-of-type? elt
		       http-token?
		       opt-qparam?))))
  (lambda (value port)
    (write-comma-list (lambda (elt port)
			(write-http-token (car elt) port)
			(write-parameter* (cdr elt) port))
		      value
		      port)))

(define-header "Accept-Encoding"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate cons
	(seq lp:token
	     lp:opt-qparam)))))
  (lambda (value)
    (list+-of-type? value
      (lambda (elt)
	(pair-of-type? elt
		       http-token?
		       opt-qparam?))))
  (lambda (value port)
    (write-comma-list (lambda (elt port)
			(write-http-token (car elt) port)
			(write-parameter* (cdr elt) port))
		      value
		      port)))

(define-header "Accept-Language"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (encapsulate cons
	(seq (qualify token-is-language-range? lp:token)
	     lp:opt-qparam)))))
  (lambda (value)
    (list+-of-type? value
      (lambda (elt)
	(pair-of-type? elt
		       language-range?
		       opt-qparam?))))
  (lambda (value port)
    (write-comma-list (lambda (elt port)
			(write-http-token (car elt) port)
			(write-parameter* (cdr elt) port))
		      value
		      port)))
#;
(define-header "Authorization"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

(define-header "Expect"
  (tokenized-parser
   (lp:comma-list 1
     (list-parser
      (qualify params-are-expectation?
	lp:parameter%*))))
  (lambda (value)
    (list+-of-type? value
      (lambda (expectation)
	(and (parameter%*? expectation)
	     (params-are-expectation? expectation)))))
  (lambda (value port)
    (write-comma-list (lambda (expectation)
			(write-parameter* expectation port))
		      value
		      port)))
#; 
(define-header "From"
  ;; parser is completely different -- it's a mail address.
  ...
  (lambda (value))
  (lambda (value port)))

(define-header "Host"
  (direct-parser parse-hostport)
  (lambda (value)
    (pair-of-type? value
		   string?
		   (lambda (port)
		     (or (not port)
			 (exact-nonnegative-integer? port)))))
  (lambda (value port)
    (write-string (car value) port)
    (if (cdr value)
	(begin
	  (write-char #\: port)
	  (write (cdr value) port)))))

(define-header "If-Match"
  (tokenized-parser
   (list-parser
    (alt (qualify (token= '*) lp:token)
	 lp:entity-tag+)))
  (lambda (value)
    (or (eq? value '*)
	(entity-tag+? value)))
  (lambda (value port)
    (if (eq? value '*)
	(write-http-token value port)
	(write-entity-tag+ value port))))

(define-header "If-Modified-Since"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-header "If-None-Match"
  (tokenized-parser
   (list-parser
    (alt (qualify (token= '*) lp:token)
	 lp:entity-tag+)))
  (lambda (value)
    (or (eq? value '*)
	(entity-tag+? value)))
  (lambda (value port)
    (if (eq? value '*)
	(write-http-token value port)
	(write-entity-tag+ value port))))

(define-header "If-Range"
  (let ((pe (tokenized-parser lp:entity-tag))
	(pd (direct-parser parser:http-date)))
    (lambda (string win lose)
      (pe string
	  win
	  (lambda ()
	    (pd string win lose)))))
  (lambda (value)
    (or (entity-tag? value)
	(http-date? value)))
  (lambda (value port)
    (if (entity-tag? value)
	(write-entity-tag value port)
	(write-http-date value port))))

(define-header "If-Unmodified-Since"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-header "Max-Forwards"
  (tokenized-parser lp:nonnegative-integer)
  exact-nonnegative-integer?
  write)
#;
(define-header "Proxy-Authorization"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

(define-header "Range"
  (tokenized-parser
   (list-parser
    (encapsulate cons
      (seq lp:bytes-unit
	   #\=
	   lp:byte-range-set))))
  (lambda (value)
    (pair-of-type? value
		   bytes-unit?
		   byte-range-set?))
  (lambda (value port)
    (write-http-token (car value) port)
    (write-char #\= port)
    (write-byte-range-set (cdr value) port)))

(define-header "Referer"
  (direct-parser
   (*parser
    (transform (lambda (v)
		 (if (uri-fragment (vector-ref v 0))
		     #f
		     v))
      (alt parse-absolute-uri
	   parse-relative-uri))))
  (lambda (value)
    (and (uri? value)
	 (not (uri-fragment value))))
  write-uri)

(define-header "TE"
  (tokenized-parser
   (lp:comma-list 0
     (list-parser
      (encapsulate cons
	(seq lp:token
	     lp:accept-params)))))
  (lambda (value)
    (pair-of-type? value
		   http-token?
		   accept-params?))
  (lambda (value port)
    (write-http-token (car value) port)
    (write-parameter* (cdr value) port)))

(define-header "User-Agent"
  (tokenized-parser lp:product/comment-list)
  product/comment-list?
  write-product/comment-list)

;;;; Response headers

(define-header "Accept-Ranges"
  (tokenized-parser
   (list-parser
    (alt (encapsulate (lambda (none) none '())
	   (qualify (token= 'NONE) lp:token))
	 lp:token+)))
  (lambda (value)
    (list+-of-type? value http-token?))
  (lambda (value port)
    (if (null? value)
	(write-http-token 'NONE port)
	(write-token* value port))))

(define-header "Age"
  (tokenized-parser
   lp:nonnegative-integer)
  exact-nonnegative-integer?
  write)

(define-header "ETag"
  (tokenized-parser lp:entity-tag)
  entity-tag?
  write-entity-tag)

(define-header "Location"
  (direct-parser parse-absolute-uri)
  absolute-uri?
  write-uri)
#;
(define-header "Proxy-Authenticate"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

(define-header "Retry-After"
  (direct-parser
   (*parser
    (alt parser:http-date
	 (map string->number
	      (match (+ (char-set char-set:numeric)))))))
  (lambda (value)
    (or (http-date? value)
	(exact-nonnegative-integer? value)))
  (lambda (value port)
    (if (exact-nonnegative-integer? value)
	(write value port)
	(write-http-date value port))))

(define-header "Server"
  (tokenized-parser lp:product/comment-list)
  product/comment-list?
  write-product/comment-list)

(define-header "Vary"
  (tokenized-parser
   (list-parser
    (alt (qualify (token= '*) lp:token)
	 lp:token+)))
  (lambda (value)
    (or (eq? value '*)
	(token+? value)))
  (lambda (value port)
    (if (eq? value '*)
	(write-http-token value port)
	(write-token* value port))))
#;
(define-header "WWW-Authenticate"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

;;;; Entity headers

(define-header "Allow"
  (tokenized-parser lp:token-cs*)
  token*?
  write-token*)

(define-header "Content-Encoding"
  (tokenized-parser lp:token+)
  token+?
  write-token*)

(define-header "Content-Language"
  (tokenized-parser (lp:comma-list 1 lp:language-tag))
  (lambda (value) (list+-of-type? value language-tag?))
  write-token*)

(define-header "Content-Length"
  (tokenized-parser lp:nonnegative-integer)
  exact-nonnegative-integer?
  write)

(define-header "Content-Location"
  (direct-parser
   (*parser
    (alt parse-absolute-uri
	 parse-relative-uri)))
  (lambda (value)
    (and (uri? value)
	 (not (uri-fragment value))))
  write-uri)

(define-header "Content-MD5"
  (lambda (string win lose)
    (let ((sum (decode-base64-octets string #f)))
      (if (and sum
	       (= (vector-8b-length sum) 16))
	  (win (structure-parser-values sum))
	  (lose))))
  (lambda (value)
    (and (vector-8b? value)
	 (= (vector-8b-length value) 16)))
  (lambda (value port)
    (write-string (string-trim-right (encode-base64-octets value)) port)))

(define-header "Content-Range"
  (tokenized-parser
   (list-parser
    (encapsulate vector
      (seq lp:bytes-unit
	   #\space
	   (alt (encapsulate cons
		  (seq lp:nonnegative-integer
		       #\-
		       lp:nonnegative-integer))
		lp:*)
	   #\/
	   (alt lp:nonnegative-integer
		lp:*)))))
  (lambda (value)
    (vector-of-types? value
		      bytes-unit?
		      (lambda (rs)
			(or (eq? rs '*)
			    (pair-of-type? rs
					   exact-nonnegative-integer?
					   exact-nonnegative-integer?)))
		      (lambda (il)
			(or (eq? il '*)
			    (exact-nonnegative-integer? il)))))
  (lambda (value port)
    (receive (unit rs il) (vector->values value)
      (write-http-token unit port)
      (write-char #\space port)
      (if (eq? rs '*)
	  (write-char #\* port)
	  (begin
	    (write (car rs) port)
	    (write-char #\= port)
	    (write (cdr rs) port)))
      (write-char #\/ port)
      (if (eq? il '*)
	  (write-char #\* port)
	  (write il port)))))

(define-header "Content-Type"
  (tokenized-parser
   (list-parser
    (encapsulate cons
      (seq lp:mime-type
	   lp:parameter*))))
  (lambda (value)
    (pair-of-type? value
		   mime-type?
		   http-parameters?))
  (lambda (value port)
    (write-mime-type (car value) port)
    (write-parameter* (cdr value) port)))

(define-header "Expires"
  (direct-parser
   (*parser
    (alt parser:http-date
	 (map (lambda (s) s #f)
	      (match (* (char-set char-set:http-text)))))))
  (lambda (value)
    (or (not value)
	(http-date? value)))
  (lambda (value port)
    (if (not value)
	(write-string "0" port)
	(write-http-date value port))))

(define-header "Last-Modified"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

;;;; Chunked encoding

(define (parse-http-chunk-leader string)
  (lp:chunk-leader (string->tokens string)
		   (lambda (tokens vals lose)
		     (if (null? tokens)
			 (structure-parser-values-ref vals 0)
			 (lose)))
		   (lambda ()
		     #f)))

(define lp:chunk-leader
  (list-parser
   (encapsulate cons
     (seq (transform (lambda (s)
		       (let ((n (string->number s 16 #f)))
			 (and n
			      (list n))))
	    lp:token-string)
	  (encapsulate list
	    (* lp:semicolon
	       lp:parameter%))))))

;;;; Utilities

(define initialize-package!
  (let ((environment (the-environment)))
    (lambda ()
      (run-boot-inits! environment))))

(define-deferred default-http-user-agent
  (list
   (cons "MIT-GNU-Scheme"
	 (let ((s (string-copy (get-subsystem-version-string "release"))))
	   (let ((end (string-length s)))
	     (do ((i 0 (+ i 1)))
		 ((not (< i end)))
	       (if (not (char-set-member? char-set:http-token
					  (string-ref s i)))
		   (string-set! s i #\_))))
	   s))))

(define (pair-of-type? object car-pred cdr-pred)
  (and (pair? object)
       (car-pred (car object))
       (cdr-pred (cdr object))))

(define (list+-of-type? object predicate)
  (and (pair? object)
       (list-of-type? object predicate)))

(define (vector-of-types? object . predicates)
  (and (vector? object)
       (= (vector-length object) (length predicates))
       (let loop ((predicates predicates) (i 0))
	 (if (pair? predicates)
	     (and ((car predicates) (vector-ref object i))
		  (loop (cdr predicates) (+ i 1)))
	     #t))))

(define (vector->values vector)
  (apply values (vector->list vector)))

(define (encode-base64-octets octets)
  (call-with-output-string
    (lambda (port)
      (let ((ctx (encode-base64:initialize port #f)))
	(encode-base64:update ctx octets 0 (vector-8b-length octets))
	(encode-base64:finalize ctx)))))

(define (decode-base64-octets string)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:decode-base64)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (call-with-output-octets
	   (lambda (port)
	     (port/set-coding port 'BINARY)
	     (port/set-line-ending port 'BINARY)
	     (let ((ctx (decode-base64:initialize port #f)))
	       (decode-base64:update ctx string 0 (string-length string))
	       (decode-base64:finalize ctx)))))))))

;;; Edwin Variables:
;;; lisp-indent/lp:comma-list: 1
;;; End:
