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
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
USA.

|#

;;;; HTTP syntax
;;; package: (runtime http-syntax)

(declare (usual-integrations))

;;;; Utility combinators

(define (lp:comma-list parse-item)
  (let ((ugh (list-parser (* (alt #\, lp:lws)))))
    (list-parser
     (encapsulate list
       (alt ugh
	    (? parse-item
	       ugh
	       (* #\,
		  (? lp:lws)
		  parse-item
		  ugh)))))))

(define (lp:comma-list+ parse-item)
  (let ((parser (lp:comma-list parse-item)))
    (list-parser
     (qualify pair?
       parser))))

(define ((token-predicate . data) object)
  (any (lambda (datum) (eq? object datum))
       data))

(define ((pair-predicate car-pred cdr-pred) object)
  (and (pair? object)
       (car-pred (car object))
       (cdr-pred (cdr object))))

(define ((list-predicate elt-pred) object)
  (list-of-type? object elt-pred))

(define ((list+-predicate elt-pred) object)
  (and (pair? object)
       (list-of-type? object elt-pred)))

(define (vector-predicate . preds)
  (let ((n (length preds)))
    (lambda (object)
      (and (vector? object)
	   (= (vector-length object) n)
	   (let loop ((preds preds) (i 0))
	     (if (pair? preds)
		 (and ((car preds) (vector-ref object i))
		      (loop (cdr preds) (+ i 1)))
		 #t))))))

(define ((opt-predicate pred) object)
  (or (not object)
      (pred object)))

(define ((alt-predicate . preds) object)
  (any (lambda (pred) (pred object))
       preds))

(define ((joined-predicate . preds) object)
  (every (lambda (pred) (pred object))
	 preds))

(define (->ascii string)
  (string->utf8 string))

(define (write-ascii string port)
  (write-bytevector (->ascii string) port))

(define (write-object value port)
  (write-ascii (write-to-string value) port))

(define ((sep-list-writer sep write-elt) value port)
  (if (pair? value)
      (let ((bytes (->ascii sep)))
	(write-elt (car value) port)
	(for-each (lambda (elt)
		    (write-bytevector bytes port)
		    (write-elt elt port))
		  (cdr value)))))

(define (comma-list-writer write-elt)
  (sep-list-writer ", " write-elt))

(define ((pair-writer write-car sep write-cdr) value port)
  (let ((write-car
	 (if (opt-writer? write-car)
	     (and (car value)
		  (cdr write-car))
	     write-car))
	(write-cdr
	 (if (opt-writer? write-cdr)
	     (and (cdr value)
		  (not (null? (cdr value)))
		  (cdr write-cdr))
	     write-cdr)))
    (if write-car
	(write-car (car value) port))
    (if (and sep write-car write-cdr)
	(if (char? sep)
	    (write-u8 (char->integer sep) port)
	    (write-ascii sep port)))
    (if write-cdr
	(write-cdr (cdr value) port))))

(define (vector-writer writer0 . args)
  (if (not (let loop ((args args))
	     (if (pair? args)
		 (and (or (not (car args))
			  (char? (car args))
			  (string? (car args)))
		      (pair? (cdr args))
		      (or (procedure? (cadr args))
			  (opt-writer? (cadr args)))
		      (loop (cddr args)))
		 (null? args))))
      (error "Ill-formed VECTOR-WRITER args:" (cons writer0 args)))
  (lambda (value port)
    (writer0 (vector-ref value 0) port)
    (let loop ((args args) (i 1))
      (if (pair? args)
	  (let ((sep (car args))
		(writer
		 (if (opt-writer? (cadr args))
		     (and (vector-ref value i)
			  (cdr (cadr args)))
		     (cadr args))))
	    (if writer
		(begin
		  (if sep (if (char? sep)
			      (write-u8 (char->integer sep) port)
			      (write-ascii sep port)))
		  (writer (vector-ref value i) port)))
	    (loop (cddr args) (+ i 1)))))))

(define (opt-writer elt-writer)
  (cons 'opt-writer elt-writer))

(define (opt-writer? object)
  (and (pair? object)
       (eq? (car object) 'opt-writer)))

(define ((alt-writer predicate consequent alternative) value port)
  ((if (predicate value) consequent alternative) value port))

(define ((token-writer token) value port)
  value
  (write-http-token token port))

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
  (write-ascii "HTTP/" port)
  (write-object (car version) port)
  (write-u8 (char->integer #\.) port)
  (write-object (cdr version) port))

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
  (write-ascii (string-pad-left (number->string object) 3 #\0) port))

;;;; Headers

(define-record-type <http-header>
    (%make-header name value parsed-value)
    http-header?
  (name http-header-name)
  (value http-header-value)
  (parsed-value http-header-parsed-value))

(define-guarantee http-header "HTTP header field")

(define-print-method http-header?
  (standard-print-method 'http-header
    (lambda (header)
      (list (http-header-name header)))))

(define (make-http-header name value)
  (guarantee http-token? name 'make-http-header)
  (let ((defn (header-value-defn name)))
    (if defn
	(if ((hvdefn-predicate defn) value)
	    (%make-header name
			  (utf8->string
			   (call-with-output-bytevector
			     (lambda (port)
			       ((hvdefn-writer defn) value port))))
			  value)
	    (begin
	      (guarantee http-text? value 'make-http-header)
	      (%make-header name value
			    (%call-parser (hvdefn-parser defn) value #t))))
	(begin
	  (guarantee http-text? value 'make-http-header)
	  (%make-header name value (%unparsed-value))))))

(define (convert-http-headers headers #!optional caller)
  (guarantee list? headers caller)
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

(define (http-header name headers error?)
  (let ((h
	 (find (lambda (header)
		 (eq? (http-header-name header) name))
	       headers)))
    (if (and (not h) error?)
	(error:bad-range-argument name 'http-header))
    h))

;;;; Tokens and text

(define (http-token? object)
  (and (interned-symbol? object)
       (string-is-http-token? (symbol->string object))))

(define-guarantee http-token "HTTP token")

(define (write-http-token token port)
  (write-ascii (symbol->string token) port))

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

(define (write-quoted-string string port)
  (write-u8 (char->integer #\") port)
  (%write-with-quotations string char-set:http-qdtext port)
  (write-u8 (char->integer #\") port))

(define (%write-with-quotations string unquoted port)
  (let ((n (string-length string)))
    (do ((i 0 (fix:+ i 1)))
	((not (fix:< i n)))
      (let ((char (string-ref string i)))
	(if (not (char-in-set? char unquoted))
	    (write-u8 (char->integer #\\) port))
	(write-u8 (char->integer char) port)))))

(define write-text
  (alt-writer string-is-http-token?
	      write-ascii
	      write-quoted-string))

(define (comment? string)
  (let ((port (open-input-string string)))
    (let loop ((level 0))
      (let ((char (read-char port)))
	(cond ((eof-object? char) (= level 0))
	      ((char=? char #\() (loop (+ level 1)))
	      ((char=? char #\)) (loop (- level 1)))
	      (else (loop level)))))))

(define (write-comment string port)
  (write-u8 (char->integer #\() port)
  (%write-with-quotations string char-set:http-text port)
  (write-u8 (char->integer #\)) port))

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
	       (noise (* (char-set char-set:wsp)))
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
			      (%call-parser (hvdefn-parser defn) value #f)
			      (%unparsed-value)))))))))

(define (%call-parser parser value error?)
  (parser value
	  (lambda (parsed-value)
	    parsed-value)
	  (lambda ()
	    (if error?
		(error "Ill-formed HTTP header value:" value)
		(warn "Ill-formed HTTP header value:" value))
	    (%unparsed-value))))

(define (%unparsed-value)
  (default-object))

(define (write-http-headers headers port)
  (guarantee-list-of http-header? headers 'write-http-headers)
  (for-each (lambda (header)
	      (let ((name (http-header-name header)))
		(let ((defn (header-value-defn name)))
		  (if defn
		      (write-ascii (hvdefn-name defn) port)
		      (write-http-token name port))))
	      (write-u8 (char->integer #\:) port)
	      (write-u8 (char->integer #\space) port)
	      (let ((value (http-header-value header)))
		(if (bytevector? value)
		    (write-bytevector value port)
		    (write-ascii value port)))
	      (write-u8 (char->integer #\return) port)
	      (write-u8 (char->integer #\linefeed) port))
	    headers)
  (write-u8 (char->integer #\return) port)
  (write-u8 (char->integer #\linefeed) port))

;;;; Header element types

(define lp:token
  (list-parser (map intern lp:token-string)))

(define lp:token-string
  (list-parser (map token-token->string (match-if token-token?))))

(define lp:token+
  (lp:comma-list+ lp:token))

(define write-tokens
  (comma-list-writer write-http-token))

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
  (list-parser (qualify *? lp:token)))

(define *?
  (token-predicate '*))

(define write-*
  (token-writer '*))

(define lp:parameters
  (list-parser
   (encapsulate list
     (* lp:semicolon
	lp:parameter))))

(define parameter?
  (pair-predicate http-token? http-text?))

(define lp:parameter
  (list-parser
   (encapsulate cons
     (seq lp:token
	  lp:=
	  lp:text))))

(define lp:parameter%
  (list-parser
   (encapsulate cons
     (seq lp:token
	  (alt (seq lp:= lp:text)
	       (values #f))))))

(define lp:=
  (list-parser
   (seq (? lp:lws)
	#\=
	(? lp:lws))))

(define parameter%?
  (pair-predicate http-token? (opt-predicate http-text?)))

(define write-parameter
  (pair-writer write-http-token
	       #\=
	       (opt-writer write-text)))

(define lp:semicolon
  (list-parser
   (seq (? lp:lws)
	#\;
	(? lp:lws))))

(define http-parameters?
  (list-predicate parameter?))

(define write-parameters
  (sep-list-writer "; " write-parameter))

(define (value+params-predicate pred)
  (pair-predicate pred http-parameters?))

(define (value+params-writer writer separator)
  (pair-writer writer
	       separator
	       (opt-writer (sep-list-writer separator write-parameter))))

(define lp:token+params
  (list-parser
   (encapsulate cons
     (seq lp:token
	  lp:parameters))))

(define token+params?
  (value+params-predicate http-token?))

(define write-token+params
  (value+params-writer write-http-token ";"))

(define (qparam? object)
  (and (parameter? object)
       (eq? (car object) 'q)))

(define lp:token+qparam
  (list-parser
   (encapsulate list
     (seq lp:token
	  (? lp:semicolon
	     (qualify qparam? lp:parameter))))))

(define token+qparam?
  (pair-predicate http-token?
		  (lambda (object)
		    (or (null? object)
			(and (pair? object)
			     (qparam? (car object))
			     (null? (cdr object)))))))

(define write-token+qparam
  (value+params-writer write-http-token ";"))

;;; Slight misnomer here.  This "accept-params" represents the pattern
;;;     *( ";" parameter ) [accept-params]

(define lp:accept-params
  (list-parser
   (encapsulate list
     (seq (* lp:semicolon
	     (disqualify qparam? lp:parameter))
	  (? lp:semicolon
	     (qualify qparam? lp:parameter)
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

(define range?
  (pair-predicate exact-nonnegative-integer?
		  exact-nonnegative-integer?))

(define write-range
  (pair-writer write-object #\- write-object))

(define (lp:numeric-token radix)
  (list-parser
   (transform (lambda (string)
		(let ((n (string->number string radix #f)))
		  (and n
		       (list n))))
     lp:token-string)))

(define lp:decimal (lp:numeric-token 10))
(define lp:hexadecimal (lp:numeric-token 16))

(define (write-opt-decimal n port)
  (if n
      (write-object n port)))

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
  (write-ascii (call-with-output-string
		 (lambda (out)
		   (write-decoded-time-as-http value out)))
	       port))

(define lp:hostport
  (list-parser
   (transform (lambda (host port)
		(let ((v
		       (*parse-string parse-hostport
				      (if port
					  (string-append host ":" port)
					  host))))
		  (and v
		       (list (vector-ref v 0)))))
     (seq lp:token-string
	  (alt (seq #\: lp:token-string)
	       (values #f))))))

(define parse-hostport
  (*parser (encapsulate* cons url:parse:hostport)))

(define hostport?
  (pair-predicate string?
		  (opt-predicate exact-nonnegative-integer?)))

(define write-hostport
  (pair-writer write-ascii
	       #\:
	       (opt-writer write)))

(define lp:hostport/token
  (list-parser (alt lp:hostport lp:token)))

(define hostport/token?
  (alt-predicate hostport? http-token?))

(define write-hostport/token
  (alt-writer hostport? write-hostport write-http-token))

(define (language-tag? object)
  (and (http-token? object)
       (*match-string (let ((segment
			     (*matcher
			      (n*m 1 8 (char-set char-set:alpha)))))
			(*matcher
			 (seq segment
			      (* (seq #\- segment)))))
		      (symbol->string object))))

(define language-range?
  (alt-predicate *? language-tag?))

(define lp:entity-tag
  (list-parser
   (encapsulate cons
     (seq (alt (map (lambda (s) s #t)
		    (seq (qualify (lambda (s) (string=? s "W"))
			   lp:token-string)
			 #\/))
	       (values #f))
	  lp:quoted-string))))

(define entity-tag?
  (pair-predicate boolean? http-text?))

(define write-entity-tag
  (pair-writer (lambda (weak? port)
		 (if weak?
		     (write-ascii "W/" port)))
	       #f
	       write-quoted-string))

(define lp:entity-tags
  (let ((lp:tags (lp:comma-list+ lp:entity-tag)))
    (list-parser
     (alt lp:*
	  lp:tags))))

(define entity-tags?
  (alt-predicate *?
		 (list+-predicate entity-tag?)))

(define write-entity-tags
  (alt-writer *?
	      write-*
	      (comma-list-writer write-entity-tag)))

(define lp:bytes-unit
  (list-parser (qualify bytes-unit? lp:token)))

(define bytes-unit?
  (token-predicate 'bytes))

(define write-bytes-unit
  (token-writer 'bytes))

(define byte-range-spec?
  (joined-predicate (pair-predicate (opt-predicate exact-nonnegative-integer?)
				    (opt-predicate exact-nonnegative-integer?))
		    (lambda (p)
		      (and (or (car p) (cdr p))
			   (if (and (car p) (cdr p))
			       (<= (car p) (cdr p))
			       #t)))))

(define lp:byte-range-set
  (lp:comma-list+
   (list-parser
    (qualify byte-range-spec?
      (transform (*parser-transform
		  (let ((match-num
			 (*matcher (+ (char-set char-set:numeric)))))
		    (*parser
		     (encapsulate* cons
		       (seq (alt (match match-num)
				 (values #f))
			    #\-
			    (alt (match match-num)
				 (values #f)))))))
	lp:token-string)))))

(define byte-range-set?
  (list+-predicate byte-range-spec?))

(define write-byte-range-set
  (comma-list-writer
   (pair-writer write-opt-decimal
		#\-
		write-opt-decimal)))

(define lp:product
  (list-parser
   (encapsulate cons
     (seq lp:token-string
	  (alt (seq lp:solidus
		    lp:token-string)
	       (values #f))))))

(define lp:solidus
  (list-parser
   (seq (? lp:lws)
	#\/
	(? lp:lws))))

(define product?
  (pair-predicate http-token-string?
		  (opt-predicate http-token-string?)))

(define write-product
  (pair-writer write-ascii
	       #\/
	       (opt-writer write-ascii)))

(define lp:product/comment-list
  (list-parser
   (encapsulate list
     (seq (alt lp:product
	       lp:comment)
	  (* (? lp:lws)
	     (alt lp:product
		  lp:comment))))))

(define product/comment-list?
  (list-predicate (alt-predicate product? comment?)))

(define (write-product/comment-list value port)
  (let ((write-elt (alt-writer product? write-product write-comment)))
    (if (pair? value)
	(begin
	  (write-elt (car value) port)
	  (for-each (lambda (elt)
		      (write-u8 (char->integer #\space) port)
		      (write-elt elt port))
		    (cdr value))))))

(define (write-ascii-uri value port)
  (write-ascii (call-with-output-string (lambda (out) (write-uri value out)))
	      port))

(define (write-ascii-mime-type mime-type port)
  (write-ascii (mime-type->string mime-type) port))

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
       (char-in-set? object char-set:http-separators)))

(define (separator-token->char token)
  token)

(define (token-token? object)
  (string? object))

(define (token-token->string token)
  token)

(define quoted-string-token?
  (pair-predicate (token-predicate 'quoted-string)
		  string?))

(define (quoted-string-token->string token)
  (cdr token))

(define comment-token?
  (pair-predicate (token-predicate 'comment)
		  string?))

(define (comment-token->string token)
  (cdr token))

(define-syntax define-tokenizer-state
  (sc-macro-transformer
   (lambda (form env)
     env
     (if (and (syntax-match? '(symbol ('eof + datum)
				      + (expression + datum))
			     (cdr form))
	      (let loop ((clauses (cddr form)))
		(and (pair? clauses)
		     (if (eq? (caar clauses) 'else)
			 (null? (cdr clauses))
			 (loop (cdr clauses))))))
	 (let ((state (cadr form))
	       (eof-clause (caddr form))
	       (normal-clauses (except-last-pair (cdddr form)))
	       (else-clause (last (cdddr form))))

	   (define (compile-rhs clause vars)
	     (let ((rhs (cdr clause)))
	       `(lambda (,@vars port emit fifo)
		  (declare (ignorable ,@vars port emit fifo))
		  ,@(map compile-action (except-last-pair rhs))
		  ,(let ((ns (last rhs)))
		     (cond ((eq? ns 'done)
			    '(emit))
			   ((symbol? ns)
			    `(,(state->name ns) port emit fifo))
			   (else ns))))))

	   (define (compile-action action)
	     (cond ((eq? action 'save-char) '(fifo char))
		   ((eq? action 'unread-char) '(unread-char char port))
		   (else action)))

	   (define (state->name name)
	     (symbol 'tokenizer-state: name))

	   `(define-deferred ,(state->name state)
	      (make-state ,(if eof-clause
			       (compile-rhs eof-clause '())
			       `#f)
			  ,(compile-rhs else-clause '(char))
			  ,@(append-map (lambda (clause)
					  `(,(car clause)
					    ,(compile-rhs clause '(char))))
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
  (#\" (emit (cons 'quoted-string (fifo))) tokenize)
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
		     (emit (cons 'comment (fifo)))
		     (tokenizer-state:tokenize port emit fifo))
		   (begin
		     (fifo char)
		     (loop (- level 1)))))
	      ((char=? char #\\)
	       (fifo (rc))
	       (loop level))
	      ((char-in-set? char char-set:http-text)
	       (fifo char)
	       (loop level))
	      (else
	       (error "Illegal char in comment:" char)))))))

;;;; Header definitions

(define (define-header name parser predicate writer)
  (hash-table-set! header-value-defns
		   (intern name)
		   (make-hvdefn name parser predicate writer)))

(define (header-value-defn name)
  (hash-table-ref/default header-value-defns name #f))

(define-deferred header-value-defns
  (make-strong-eq-hash-table))

(define-structure hvdefn
  (name #f read-only #t)
  (parser #f read-only #t)
  (predicate #f read-only #t)
  (writer #f read-only #t))

(define (define-comma-list-header name parser predicate writer)
  (define-header name
    (tokenized-parser (lp:comma-list parser))
    (list-predicate predicate)
    (comma-list-writer writer)))

(define (define-comma-list+-header name parser predicate writer)
  (define-header name
    (tokenized-parser (lp:comma-list+ parser))
    (list+-predicate predicate)
    (comma-list-writer writer)))

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

;; Header definitions are deferred at cold load...
(add-boot-init! (lambda ()

;;;; General headers

(define-comma-list+-header "Cache-Control"
  lp:parameter%
  parameter%?
  write-parameter)

(define-comma-list+-header "Connection"
  lp:token
  http-token?
  write-http-token)

(define-header "Date"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-comma-list+-header "Pragma"
  lp:parameter%
  parameter%?
  write-parameter)

(define-comma-list+-header "Trailer"
  lp:token
  http-token?
  write-http-token)

(define-comma-list+-header "Transfer-Encoding"
  lp:token+params
  token+params?
  write-token+params)

(define-comma-list+-header "Upgrade"
  lp:product
  product?
  write-product)

(define-comma-list+-header "Via"
  (list-parser
   (encapsulate vector
     (seq (encapsulate cons
	    (seq (alt (seq lp:token lp:solidus)
		      (values #f))
		 lp:token))
	  lp:lws
	  lp:hostport/token
	  (alt (seq (? lp:lws) lp:comment)
	       (values #f)))))
  (vector-predicate (pair-predicate (opt-predicate http-token?)
				    http-token?)
		    hostport/token?
		    (opt-predicate comment?))
  (vector-writer (pair-writer (opt-writer write-http-token)
			      #\/
			      write-http-token)
		 #\space
		 write-hostport/token
		 #\space
		 (opt-writer write-comment)))

(define-comma-list+-header "Warning"
  (list-parser
   (encapsulate vector
     (seq (qualify http-status? lp:decimal)
	  #\space
	  (alt lp:hostport
	       lp:token)
	  #\space
	  lp:quoted-string
	  (alt (seq #\space
		    (transform (*parser-transform parser:http-date)
		      lp:quoted-string))
	       (values #f)))))
  (vector-predicate http-status?
		    hostport/token?
		    http-text?
		    (opt-predicate decoded-time?))
  (vector-writer write-http-status
		 #\space
		 write-hostport/token
		 #\space
		 write-quoted-string
		 #\space
		 (opt-writer
		  (lambda (date port)
		    (write-quoted-string
		     (call-with-output-string
		       (lambda (port)
			 (write-http-date date port)))
		     port)))))

;;;; Request headers

(define-comma-list-header "Accept"
  (list-parser
   (encapsulate cons
     (seq (encapsulate (lambda (t1 t2)
			 (if (*? t2)
			     t1
			     (make-mime-type t1 t2)))
	    (seq lp:token
		 #\/
		 lp:token))
	  lp:accept-params)))
  (pair-predicate (alt-predicate mime-type? http-token?)
		  accept-params?)
  (value+params-writer (alt-writer mime-type?
				   write-ascii-mime-type
				   (lambda (value port)
				     (write-http-token value port)
				     (write-ascii "/*" port)))
		       ";"))

(define-comma-list+-header "Accept-Charset"
  lp:token+qparam
  token+qparam?
  write-token+qparam)

(define-comma-list+-header "Accept-Encoding"
  lp:token+qparam
  token+qparam?
  write-token+qparam)

(let ((qualifier (lambda (p) (language-range? (car p)))))
  (define-comma-list+-header "Accept-Language"
    (list-parser (qualify qualifier lp:token+qparam))
    (joined-predicate token+qparam? qualifier)
    write-token+qparam))
#;
(define-header "Authorization"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

(define-comma-list+-header "Expect"
  (list-parser
   (qualify params-are-expectation?
     (encapsulate list
       (* lp:semicolon
	  lp:parameter%))))
  (joined-predicate (list-predicate parameter%?)
		    params-are-expectation?)
  write-parameters)

#;
(define-header "From"
  ;; parser is completely different -- it's a mail address.
  ...
  (lambda (value))
  (lambda (value port)))

(define-header "Host"
  (direct-parser parse-hostport)
  hostport?
  write-hostport)

(define-header "If-Match"
  (tokenized-parser lp:entity-tags)
  entity-tags?
  write-entity-tags)

(define-header "If-Modified-Since"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-header "If-None-Match"
  (tokenized-parser lp:entity-tags)
  entity-tags?
  write-entity-tags)

(define-header "If-Range"
  (let ((pe (tokenized-parser lp:entity-tag))
	(pd (direct-parser parser:http-date)))
    (lambda (string win lose)
      (pe string
	  win
	  (lambda ()
	    (pd string win lose)))))
  (alt-predicate entity-tag? http-date?)
  (alt-writer entity-tag? write-entity-tag write-http-date))

(define-header "If-Unmodified-Since"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

(define-header "Max-Forwards"
  (tokenized-parser lp:decimal)
  exact-nonnegative-integer?
  write-object)

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
	   lp:=
	   lp:byte-range-set))))
  (pair-predicate bytes-unit? byte-range-set?)
  (pair-writer write-bytes-unit
	       #\=
	       write-byte-range-set))

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
  write-ascii-uri)

(define-comma-list-header "TE"
  (list-parser
   (encapsulate cons
     (seq lp:token
	  lp:accept-params)))
  (pair-predicate http-token?
		  accept-params?)
  write-token+params)

(define-header "User-Agent"
  (tokenized-parser lp:product/comment-list)
  product/comment-list?
  write-product/comment-list)

;;;; Response headers

(define-header "Accept-Ranges"
  (tokenized-parser
   (let ((none? (token-predicate 'none)))
     (list-parser
      (alt (encapsulate (lambda (none) none '())
	     (qualify none? lp:token))
	   lp:token+))))
  (list-predicate http-token?)
  (alt-writer null?
	      (token-writer 'none)
	      write-tokens))

(define-header "Age"
  (tokenized-parser lp:decimal)
  exact-nonnegative-integer?
  write-object)

(define-header "ETag"
  (tokenized-parser lp:entity-tag)
  entity-tag?
  write-entity-tag)

(define-header "Location"
  (direct-parser parse-absolute-uri)
  absolute-uri?
  write-ascii-uri)
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
	 lp:decimal)))
  (alt-predicate http-date? exact-nonnegative-integer?)
  (alt-writer http-date? write-http-date write-object))

(define-header "Server"
  (tokenized-parser lp:product/comment-list)
  product/comment-list?
  write-product/comment-list)

(define-header "Vary"
  (tokenized-parser
   (list-parser
    (alt lp:*
	 lp:token+)))
  (alt-predicate *? (list+-predicate http-token?))
  (alt-writer *? write-* write-tokens))
#;
(define-header "WWW-Authenticate"
  (tokenized-parser
   ...)
  (lambda (value))
  (lambda (value port)))

;;;; Entity headers

(define-comma-list-header "Allow"
  lp:token-string
  http-token-string?
  write-ascii)

(define-comma-list+-header "Content-Encoding"
  lp:token
  http-token?
  write-http-token)

(define-comma-list+-header "Content-Language"
  (list-parser (qualify language-tag? lp:token))
  language-tag?
  write-http-token)

(define-header "Content-Length"
  (tokenized-parser lp:decimal)
  exact-nonnegative-integer?
  write-object)

(define-header "Content-Location"
  (direct-parser
   (*parser
    (alt parse-absolute-uri
	 parse-relative-uri)))
  (lambda (value)
    (and (uri? value)
	 (not (uri-fragment value))))
  write-ascii-uri)

(define-header "Content-MD5"
  (lambda (string win lose)
    (let ((sum (decode-base64-bytes string #f)))
      (if (and sum
	       (= (bytevector-length sum) 16))
	  (win (structure-parser-values sum))
	  (lose))))
  (lambda (value)
    (and (bytevector? value)
	 (= (bytevector-length value) 16)))
  (lambda (value port)
    (write-ascii (string-trim-right (encode-base64-bytes value)) port)))

(define-header "Content-Range"
  (tokenized-parser
   (list-parser
    (encapsulate vector
      (seq lp:bytes-unit
	   #\space
	   (alt (encapsulate cons
		  (seq lp:decimal
		       #\-
		       lp:decimal))
		lp:*)
	   lp:solidus
	   (alt lp:decimal
		lp:*)))))
  (vector-predicate bytes-unit?
		    (alt-predicate range? *?)
		    (alt-predicate exact-nonnegative-integer? *?))
  (vector-writer write-bytes-unit
		 #\space
		 (alt-predicate range? write-range write-*)
		 #\/
		 (alt-predicate exact-nonnegative-integer?
				write-object write-*)))

(define-header "Content-Type"
  (tokenized-parser
   (list-parser
    (encapsulate cons
      (seq lp:mime-type
	   lp:parameters))))
  (value+params-predicate mime-type?)
  (value+params-writer write-ascii-mime-type "; "))

(define-header "Expires"
  (direct-parser
   (*parser
    (alt parser:http-date
	 (encapsulate (lambda (v) v #f)
	   (noise (+ (char-set char-set:http-text)))))))
  (opt-predicate http-date?)
  (alt-writer http-date?
	      write-http-date
	      (lambda (value port)
		value
		(write-ascii "-1" port))))

(define-header "Last-Modified"
  (direct-parser parser:http-date)
  http-date?
  write-http-date)

;; End of ADD-BOOT-INIT! wrapper.
))

;;;; Utilities

(define (parse-http-chunk-leader string)
  ((list-parser
    (encapsulate list
      (seq lp:hexadecimal
	   (* lp:semicolon
	      lp:parameter%))))
   (string->tokens string)
   (lambda (items vals lose)
     (if (null? items)
	 (structure-parser-values-ref vals 0)
	 (lose)))
   (lambda ()
     #f)))

(define-deferred default-http-user-agent
  (list
   (cons "MIT-GNU-Scheme"
	 (let ((s (string-copy (get-subsystem-version-string "release"))))
	   (let ((end (string-length s)))
	     (do ((i 0 (+ i 1)))
		 ((not (< i end)))
	       (if (not (char-in-set? (string-ref s i) char-set:http-token))
		   (string-set! s i #\_))))
	   s))))

(define (vector->values vector)
  (apply values (vector->list vector)))

(define (*parser-transform parser)
  (lambda (string)
    (let ((v (*parse-string parser string)))
      (and v
	   (list (vector-ref v 0))))))

(define (encode-base64-bytes bv)
  (call-with-output-string
    (lambda (port)
      (let ((ctx (encode-base64:initialize port #f)))
	(encode-base64:update ctx bv 0 (bytevector-length bv))
	(encode-base64:finalize ctx)))))

(define (decode-base64-bytes string)
  (call-with-current-continuation
   (lambda (k)
     (bind-condition-handler (list condition-type:decode-base64)
	 (lambda (condition)
	   condition
	   (k #f))
       (lambda ()
	 (call-with-output-bytevector
	   (lambda (port)
	     (let ((ctx (decode-base64:initialize port #f)))
	       (decode-base64:update ctx string 0 (string-length string))
	       (decode-base64:finalize ctx)))))))))