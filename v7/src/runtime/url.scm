#| -*-Scheme-*-

$Id: url.scm,v 1.38 2006/01/31 06:47:47 cph Exp $

Copyright 2000,2001,2003,2004,2005,2006 Massachusetts Institute of Technology

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

;;;; Uniform Resource Identifiers
;;; package: (runtime uri)

;;; RFC 3986 <http://ietf.org/rfc/rfc3986.txt>

(declare (usual-integrations))

(define-record-type <uri>
    (%make-uri scheme authority path query fragment)
    uri?
  (scheme uri-scheme set-uri-scheme!)
  (authority uri-authority)
  (path uri-path)
  (query uri-query)
  (fragment uri-fragment set-uri-fragment!))

(set-record-type-unparser-method! <uri>
  (standard-unparser-method 'URI
    (lambda (uri port)
      (write-char #\space port)
      (write (uri->string uri) port))))

(define (make-uri scheme authority path query fragment)
  (let ((path (if (equal? path '("")) '() path)))
    (if scheme (guarantee-uri-scheme scheme 'MAKE-URI))
    (if authority (guarantee-uri-authority authority 'MAKE-URI))
    (guarantee-uri-path path 'MAKE-URI)
    (if query (guarantee-utf8-string query 'MAKE-URI))
    (if fragment (guarantee-utf8-string fragment 'MAKE-URI))
    (if (and authority (pair? path) (path-relative? path))
	(error:bad-range-argument path 'MAKE-URI))
    (%make-uri scheme
	       authority
	       (if scheme
		   (remove-dot-segments path)
		   path)
	       query
	       fragment)))

(define (absolute-uri? object)
  (and (uri? object)
       (uri-absolute? object)))

(define (relative-uri? object)
  (and (uri? object)
       (uri-relative? object)))

(define-integrable (uri-absolute? uri)
  (if (uri-scheme uri) #t #f))

(define-integrable (uri-relative? uri)
  (if (uri-scheme uri) #f #t))

(define-guarantee uri "URI")
(define-guarantee relative-uri "relative URI")
(define-guarantee absolute-uri "absolute URI")

(define (uri-scheme? object)
  (and (interned-symbol? object)
       (complete-match matcher:scheme (symbol-name object))))

;;; A well-formed path is a list of N segments which is equivalent to
;;; a string in which there is a slash between each adjacent pair of
;;; segments.   In other words, there are N-1 slashes.  If the string
;;; begins with a slash, the internal form begins with an empty
;;; segment, and if it ends with a slash the internal form ends with
;;; an empty segment.

(define (uri-path? object)
  (list-of-type? object utf8-string?))

(define (uri-path-absolute? path)
  (guarantee-uri-path path 'URI-PATH-ABSOLUTE?)
  (path-absolute? path))

(define (path-absolute? path)
  (and (pair? path)
       (fix:= (string-length (car path)) 0)))

(define (uri-path-relative? path)
  (guarantee-uri-path path 'URI-PATH-RELATIVE?)
  (path-relative? path))

(define-integrable (path-relative? path)
  (not (path-absolute? path)))

(define-record-type <uri-authority>
    (%make-uri-authority userinfo host port)
    uri-authority?
  (userinfo uri-authority-userinfo)
  (host uri-authority-host)
  (port uri-authority-port))

(set-record-type-unparser-method! <uri-authority>
  (standard-unparser-method 'URI-AUTHORITY
    (lambda (authority port)
      (write-char #\space port)
      (write-authority authority port))))

(define (make-uri-authority userinfo host port)
  (if userinfo (guarantee-uri-userinfo userinfo 'MAKE-URI-AUTHORITY))
  (guarantee-uri-host host 'MAKE-URI-AUTHORITY)
  (if port (guarantee-uri-port port 'MAKE-URI-AUTHORITY))
  (%make-uri-authority userinfo host port))

(define (uri-userinfo? object)
  (and (string? object)
       (complete-match parser:userinfo object)))

(define (uri-host? object)
  (and (string? object)
       (complete-match matcher:host object)))

(define (uri-port? object)
  (exact-nonnegative-integer? object))

(define-guarantee uri-scheme "URI scheme")
(define-guarantee uri-path "URI path")
(define-guarantee uri-authority "URI authority")
(define-guarantee uri-userinfo "URI userinfo")
(define-guarantee uri-host "URI host")
(define-guarantee uri-port "URI port")

(define (complete-match matcher string #!optional start end)
  (let ((buffer (string->parser-buffer string start end)))
    (and (matcher buffer)
	 (not (peek-parser-buffer-char buffer)))))

(define (uri=? u1 u2)
  (let ((u1 (->uri u1 'URI=?))
	(u2 (->uri u2 'URI=?)))
    (and (eq? (uri-scheme u1) (uri-scheme u2))
	 (%component=? %uri-authority=? (uri-authority u1) (uri-authority u2))
	 (let loop ((p1 (uri-path u1)) (p2 (uri-path u2)))
	   (if (pair? p1)
	       (and (pair? p2)
		    (string=? (car p1) (car p2))
		    (loop (cdr p1) (cdr p2)))
	       (null? p2)))
	 (%component=? string=? (uri-query u1) (uri-query u2))
	 (%component=? string=? (uri-fragment u1) (uri-fragment u2)))))

(define (uri-authority=? a1 a2)
  (guarantee-uri-authority a1 'URI-AUTHORITY=?)
  (guarantee-uri-authority a2 'URI-AUTHORITY=?)
  (%uri-authority=? a1 a2))

(define (%uri-authority=? a1 a2)
  (and (%component=? string=?
		     (uri-authority-userinfo a1)
		     (uri-authority-userinfo a2))
       (string=? (uri-authority-host a1) (uri-authority-host a2))
       (%component=? = (uri-authority-port a1) (uri-authority-port a2))))

(define (%component=? predicate x1 x2)
  (if x1
      (and x2 (predicate x1 x2))
      (not x2)))

(define (uri->alist uri)
  `(,@(if (uri-scheme uri)
	  `((scheme ,(uri-scheme uri)))
	  '())
    ,@(if (uri-authority uri)
	  (let ((a (uri-authority uri)))
	    `(,@(if (uri-authority-userinfo a)
		    `((userinfo ,(uri-authority-userinfo a)))
		    '())
	      (host ,(uri-authority-host a))
	      ,@(if (uri-authority-port a)
		    `((port ,(uri-authority-port a)))
		    '())))
	  '())
    (path ,(uri-path uri))
    ,@(if (uri-query uri)
	  `((query ,(uri-query uri)))
	  '())
    ,@(if (uri-fragment uri)
	  `((fragment ,(uri-fragment uri)))
	  '())))

;;;; Merging

(define (merge-uris uri base-uri)
  (guarantee-absolute-uri base-uri 'MERGE-URIS)
  (let ((uri (->uri uri 'MERGE-URIS)))
    (cond ((uri-scheme uri)
	   (%make-uri (uri-scheme uri)
		      (uri-authority uri)
		      (remove-dot-segments (uri-path uri))
		      (uri-query uri)
		      (uri-fragment uri)))
	  ((uri-authority uri)
	   (%make-uri (uri-scheme base-uri)
		      (uri-authority uri)
		      (remove-dot-segments (uri-path uri))
		      (uri-query uri)
		      (uri-fragment uri)))
	  ((null? (uri-path uri))
	   (%make-uri (uri-scheme base-uri)
		      (uri-authority base-uri)
		      (uri-path base-uri)
		      (or (uri-query uri) (uri-query base-uri))
		      (uri-fragment uri)))
	  (else
	   (%make-uri (uri-scheme base-uri)
		      (uri-authority base-uri)
		      (remove-dot-segments
		       (merge-paths (uri-path uri) base-uri))
		      (uri-query uri)
		      (uri-fragment uri))))))

(define (merge-paths ref-path base-uri)
  (cond ((path-absolute? ref-path)
	 ref-path)
	((and (uri-authority base-uri)
	      (null? (uri-path base-uri)))
	 (cons "" ref-path))
	(else
	 (let ((path (uri-path base-uri)))
	   (if (and (pair? path)
		    (pair? (cdr path)))
	       (append (except-last-pair path) ref-path)
	       ref-path)))))

(define (remove-dot-segments path)
  ;; At all times, (APPEND INPUT (REVERSE OUTPUT)) must be well
  ;; formed.  If both INPUT and OUTPUT are non-null, the slash
  ;; separating them is assumed to be in INPUT.
  (letrec
      ((no-output
	(lambda (input)
	  (if (pair? input)
	      (let ((segment (car input))
		    (input (cdr input)))
		(if (or (string=? segment "..")
			(string=? segment "."))
		    ;; Rules A and D
		    (no-output input)
		    ;; Rule E
		    (some-output input (list segment))))
	      '())))
       (some-output
	(lambda (input output)
	  (if (pair? input)
	      (let ((segment (car input))
		    (input (cdr input)))
		(cond ((string=? segment ".")
		       ;; Rule B
		       (maybe-done input output))
		      ((string=? segment "..")
		       ;; Rule C
		       (maybe-done input
				   (if (pair? (cdr output))
				       (cdr output)
				       (list ""))))
		      (else
		       ;; Rule E
		       (some-output input (cons segment output)))))
	      output)))
       (maybe-done
	(lambda (input output)
	  (if (pair? input)
	      (some-output input output)
	      (cons "" output)))))
    (reverse! (no-output path))))

;;;; Parser

(define-syntax define-uri-coercion
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '(SYMBOL) (cdr form))
	 (let* ((root (cadr form)))
	   `(DEFINE (,(symbol '-> root) OBJECT #!OPTIONAL CALLER)
	      (IF (,(symbol root '?) OBJECT)
		  OBJECT
		  (OR (COMPLETE-PARSE
		       ,(symbol 'PARSE- root)
		       (OR (->PARSER-BUFFER OBJECT)
			   (,(symbol 'ERROR:NOT- root) OBJECT CALLER)))
		      (ERROR:BAD-RANGE-ARGUMENT OBJECT CALLER)))))
	 (ill-formed-syntax form)))))

(define-uri-coercion uri)
(define-uri-coercion absolute-uri)
(define-uri-coercion relative-uri)

(define (->parser-buffer object)
  (cond ((or (string? object) (wide-string? object))
	 (string->parser-buffer object))
	((input-port? object) (input-port->parser-buffer object))
	((symbol? object) (string->parser-buffer (symbol->wide-string object)))
	(else #f)))

(define (string->uri string #!optional start end)
  (%string->uri parse-uri string start end 'STRING->URI))

(define (string->absolute-uri string #!optional start end)
  (%string->uri parse-absolute-uri string start end 'STRING->ABSOLUTE-URI))

(define (string->relative-uri string #!optional start end)
  (%string->uri parse-relative-uri string start end 'STRING->RELATIVE-URI))

(define (%string->uri parser string start end caller)
  (or (complete-parse parser (string->parser-buffer string start end))
      (error:bad-range-argument string caller)))

(define (complete-parse parser buffer)
  (let ((v (parser buffer)))
    (and v
	 (not (peek-parser-buffer-char buffer))
	 (vector-ref v 0))))

(define parse-uri
  (*parser (top-level (encapsulate encapsulate-uri parser:uri-reference))))

(define parse-absolute-uri
  (*parser (top-level (encapsulate encapsulate-uri parser:uri))))

(define parse-relative-uri
  (*parser (top-level (encapsulate encapsulate-uri parser:relative-ref))))

(define (encapsulate-uri v)
  (%make-uri (vector-ref v 0)
	     (vector-ref v 1)
	     (vector-ref v 2)
	     (vector-ref v 3)
	     (vector-ref v 4)))

(define parser:uri
  (*parser
   (seq parser:scheme
	":"
	parser:hier-part
	(alt (seq "?" parser:query)
	     (values #f))
	(alt (seq "#" parser:fragment)
	     (values #f)))))

(define parser:hier-part
  (*parser
   (alt (seq "//" parser:authority parser:path-abempty)
	(seq (values #f) parser:path-absolute)
	(seq (values #f) parser:path-rootless)
	(seq (values #f) parser:path-empty))))

(define parser:uri-reference
  (*parser
   (alt parser:uri
	parser:relative-ref)))

(define parser:relative-ref
  (*parser
   (seq (values #f)
	parser:relative-part
	(alt (seq "?" parser:query)
	     (values #f))
	(alt (seq "#" parser:fragment)
	     (values #f)))))

(define parser:relative-part
  (*parser
   (alt (seq "//" parser:authority parser:path-abempty)
	(seq (values #f) parser:path-absolute)
	(seq (values #f) parser:path-noscheme)
	(seq (values #f) parser:path-empty))))

(define parser:scheme
  (*parser
   (map intern (match matcher:scheme))))

(define matcher:scheme
  (*matcher
   (seq (char-set char-set:uri-alpha)
	(* (char-set char-set:uri-scheme)))))

(define parser:authority
  (*parser
   (encapsulate (lambda (v)
		  (%make-uri-authority (vector-ref v 0)
				       (vector-ref v 1)
				       (vector-ref v 2)))
     (seq (alt (seq parser:userinfo "@")
	       (values #f))
	  parser:hostport))))

(define parser:hostport
  (*parser
   (seq (map uri-string-downcase (match matcher:host))
	(alt (seq ":"
		  (map (lambda (s)
			 (and (fix:> (string-length s) 0)
			      (string->number s)))
		       (match (* (char-set char-set:uri-digit)))))
	     (values #f)))))

;; This is a kludge to work around fact that STRING-DOWNCASE only
;; works on ISO 8859-1 strings, and we are using UTF-8 strings.

(define (uri-string-downcase string)
  (call-with-output-string
   (lambda (output)
     (port/set-coding output 'UTF-8)
     (let ((input (open-input-string string)))
       (port/set-coding input 'UTF-8)
       (let loop ()
	 (let ((char (read-char input)))
	   (if (not (eof-object? char))
	       (begin
		 (write-char (char-downcase char) output)
		 (loop)))))))))

(define matcher:host
  (*matcher
   (alt matcher:ip-literal
	;; subsumed by MATCHER:REG-NAME
	;;matcher:ipv4-address
	matcher:reg-name)))

(define matcher:ip-literal
  (*matcher
   (seq "["
	(alt matcher:ipv6-address
	     matcher:ipvfuture)
	"]")))

(define matcher:ipvfuture
  (*matcher
   (seq "v"
	(+ (char-set char-set:uri-hex))
	"."
	(+ (char-set char-set:uri-ipvfuture)))))

(define matcher:ipv6-address
  ;; This is artificially broken into separate clauses M1 ... M9 as a
  ;; work-around for a bug in the compiler.  The LET* is used so that
  ;; the clauses each fit on a single line.
  (let*
      ((h16 (*matcher (n*m 1 4 (char-set char-set:uri-hex))))
       (h16: (*matcher (seq h16 ":")))
       (ls32 (*matcher (alt (seq h16 ":" h16) matcher:ipv4-address)))
       (m1 (*matcher (seq                                (n*n 6 h16:) ls32)))
       (m2 (*matcher (seq                           "::" (n*n 5 h16:) ls32)))
       (m3 (*matcher (seq (? (seq             h16)) "::" (n*n 4 h16:) ls32)))
       (m4 (*matcher (seq (? (seq (*n 1 h16:) h16)) "::" (n*n 3 h16:) ls32)))
       (m5 (*matcher (seq (? (seq (*n 2 h16:) h16)) "::" (n*n 2 h16:) ls32)))
       (m6 (*matcher (seq (? (seq (*n 3 h16:) h16)) "::"        h16:  ls32)))
       (m7 (*matcher (seq (? (seq (*n 4 h16:) h16)) "::"              ls32)))
       (m8 (*matcher (seq (? (seq (*n 5 h16:) h16)) "::"              h16 )))
       (m9 (*matcher (seq (? (seq (*n 6 h16:) h16)) "::"                  ))))
    (*matcher (alt m1 m2 m3 m4 m5 m6 m7 m8 m9))))

(define matcher:ipv4-address
  (*matcher
   (seq matcher:dec-octet
	"."
	matcher:dec-octet
	"."
	matcher:dec-octet
	"."
	matcher:dec-octet)))

(define matcher:dec-octet
  (*matcher
   (alt "0"
	(seq "1"
	     (? (seq (char-set char-set:uri-digit)
		     (? (char-set char-set:uri-digit)))))
	(seq "2"
	     (? (alt (seq (char-set (string->char-set "01234"))
			  (? (char-set char-set:uri-digit)))
		     (seq "5"
			  (? (char-set (string->char-set "012345"))))
		     (char-set (string->char-set "6789")))))
	(seq (char-set (string->char-set "3456789"))
	     (? (char-set char-set:uri-digit))))))

(define parser:path-abempty
  (*parser
   (encapsulate (lambda (v)
		  (let ((segments (vector->list v)))
		    (if (pair? segments)
			(cons "" segments)
			segments)))
     (* (seq "/" parser:segment)))))

(define parser:path-absolute
  (*parser
   (encapsulate (lambda (v)
		  (let ((segments (vector->list v)))
		    (if (pair? segments)
			(cons "" segments)
			(list "" ""))))
     (seq "/"
	  (? (seq parser:segment-nz
		  (* (seq "/" parser:segment))))))))

(define parser:path-noscheme
  (*parser
   (encapsulate vector->list
     (seq parser:segment-nz-nc
	  (* (seq "/" parser:segment))))))

(define parser:path-rootless
  (*parser
   (encapsulate vector->list
     (seq parser:segment-nz
	  (* (seq "/" parser:segment))))))

(define (parser:path-empty buffer)
  buffer
  (vector '()))

;;;; Output

(define (uri->string uri)
  (guarantee-uri uri 'URI->STRING)
  (call-with-output-string
    (lambda (port)
      (%write-uri uri port))))

(define (uri->symbol uri)
  (utf8-string->symbol (uri->string uri)))

(define (write-uri uri port)
  (guarantee-uri uri 'WRITE-URI)
  (guarantee-port port 'WRITE-URI)
  (%write-uri uri port))

(define (%write-uri uri port)
  (if (uri-scheme uri)
      (begin
	(write (uri-scheme uri) port)
	(write-char #\: port)))
  (if (uri-authority uri)
      (write-authority (uri-authority uri) port))
  (let ((path (uri-path uri)))
    (if (pair? path)
	(begin
	  (if (uri-scheme uri)
	      (write-segment (car path) port)
	      (write-encoded (car path) char-set:uri-segment-nc port))
	  (for-each (lambda (segment)
		      (write-char #\/ port)
		      (write-segment segment port))
		    (cdr path)))))
  (if (uri-query uri)
      (begin
	(write-char #\? port)
	(write-encoded (uri-query uri) char-set:uri-query port)))
  (if (uri-fragment uri)
      (begin
	(write-char #\# port)
	(write-encoded (uri-fragment uri) char-set:uri-fragment port))))

(define (write-authority authority port)
  (write-string "//" port)
  (if (uri-authority-userinfo authority)
      (begin
	(write-encoded (uri-authority-userinfo authority)
		       char-set:uri-userinfo
		       port)
	(write-char #\@ port)))
  (if (uri-authority-host authority)
      (write-encoded (uri-authority-host authority)
		     char-set:uri-opaque-auth
		     port))
  (if (uri-authority-port authority)
      (begin
	(write-char #\: port)
	(write (uri-authority-port authority) port))))

(define (write-segment segment port)
  (write-encoded segment char-set:uri-segment port))

;;;; Escape codecs

(define (component-parser-* cs)
  (let ((matcher (component-matcher-* cs)))
    (*parser (map decode-component (match matcher)))))

(define (component-parser-+ cs)
  (let ((matcher (component-matcher-+ cs)))
    (*parser (map decode-component (match matcher)))))

(define (component-matcher-* cs)
  (*matcher (* (alt (char-set cs) matcher:pct-encoded))))

(define (component-matcher-+ cs)
  (*matcher (+ (alt (char-set cs) matcher:pct-encoded))))

(define matcher:pct-encoded
  (*matcher
   (seq "%"
	(char-set char-set:uri-hex)
	(char-set char-set:uri-hex))))

(define (decode-component string)
  (if (string-find-next-char string #\%)
      (call-with-output-string
	(lambda (port)
	  (let ((end (string-length string)))
	    (let loop ((i 0))
	      (if (fix:< i end)
		  (if (char=? (string-ref string i) #\%)
		      (begin
			(write-char (integer->char
				     (substring->number string
							(fix:+ i 1)
							(fix:+ i 3)
							16
							#t))
				    port)
			(loop (fix:+ i 3)))
		      (begin
			(write-char (string-ref string i) port)
			(loop (fix:+ i 1)))))))))
      string))

(define (write-encoded string unescaped port)
  (write-encoded-substring string 0 (string-length string) unescaped port))

(define (write-encoded-substring string start end unescaped port)
  (do ((i start (fix:+ i 1)))
      ((not (fix:< i end)))
    (let ((char (string-ref string i)))
      (if (char-set-member? unescaped char)
	  (write-char char port)
	  (begin
	    (write-char #\% port)
	    (write-string (string-pad-left
			   (string-upcase (number->string (char->integer char)
							  16))
			   2
			   #\0)
			  port))))))

;; backwards compatibility:
(define (url:encode-string string)
  (call-with-output-string
    (lambda (port)
      (write-encoded string url:char-set:unescaped port))))

;;;; Regular expressions

(define (uri-rexp:uri)
  (rexp-sequence (uri-rexp:scheme)
		 ":"
		 (uri-rexp:hier-part)
		 (rexp-optional "?" (uri-rexp:query))
		 (rexp-optional "#" (uri-rexp:fragment))))

(define (uri-rexp:hier-part)
  (rexp-alternatives (rexp-sequence "//"
				    (uri-rexp:authority)
				    (uri-rexp:path-abempty))
		     (uri-rexp:path-absolute)
		     (uri-rexp:path-rootless)
		     (uri-rexp:path-empty)))

(define (uri-rexp:uri-reference)
  (rexp-alternatives (uri-rexp:uri)
		     (uri-rexp:relative-ref)))

(define (uri-rexp:absolute-uri)
  (rexp-sequence (uri-rexp:scheme)
		 ":"
		 (uri-rexp:hier-part)
		 (rexp-optional "?" (uri-rexp:query))))

(define (uri-rexp:relative-ref)
  (rexp-sequence (uri-rexp:relative-part)
		 (rexp-optional "?" (uri-rexp:query))
		 (rexp-optional "#" (uri-rexp:fragment))))

(define (uri-rexp:relative-part)
  (rexp-alternatives (rexp-sequence "//"
				    (uri-rexp:authority)
				    (uri-rexp:path-abempty))
		     (uri-rexp:path-absolute)
		     (uri-rexp:path-noscheme)
		     (uri-rexp:path-empty)))

(define (uri-rexp:scheme)
  (rexp-sequence char-set:uri-alpha
		 (rexp* char-set:uri-scheme)))

(define (uri-rexp:opaque-auth)
  (rexp* char-set:uri-opaque-auth))

(define (uri-rexp:authority)
  (rexp-sequence (rexp-optional (uri-rexp:userinfo) "@")
		 (uri-rexp:host)
		 (rexp-optional ":" (uri-rexp:port))))

(define (uri-rexp:userinfo)
  (rexp* (uri-rexp:pct-encoded char-set:uri-userinfo)))

(define (uri-rexp:host)
  (rexp-alternatives (uri-rexp:ip-literal)
		     (uri-rexp:ipv4-address)
		     (uri-rexp:reg-name)))

(define (uri-rexp:port)
  (rexp* char-set:uri-digit))

(define (uri-rexp:ip-literal)
  (rexp-sequence "["
		 (rexp-alternatives (uri-rexp:ipv6-address)
				    (uri-rexp:ipvfuture))
		 "]"))

(define (uri-rexp:ipvfuture)
  (rexp-sequence "v"
		 (rexp+ char-set:uri-hex)
		 "."
		 (rexp+ char-set:uri-ipvfuture)))

(define (uri-rexp:ipv6-address)
  (let ((h16 (uri-rexp:h16))
	(ls32 (uri-rexp:ls32))
	(alt rexp-alternatives)
	(seq rexp-sequence)
	(? rexp-optional))
    (alt (seq                                  (rexp-n*n 6 h16 ":") ls32)
	 (seq                             "::" (rexp-n*n 5 h16 ":") ls32)
	 (seq (?                     h16) "::" (rexp-n*n 4 h16 ":") ls32)
	 (seq (? (rexp-*n 1 h16 ":") h16) "::" (rexp-n*n 3 h16 ":") ls32)
	 (seq (? (rexp-*n 2 h16 ":") h16) "::" (rexp-n*n 2 h16 ":") ls32)
	 (seq (? (rexp-*n 3 h16 ":") h16) "::" (rexp-n*n 1 h16 ":") ls32)
	 (seq (? (rexp-*n 4 h16 ":") h16) "::"                      ls32)
	 (seq (? (rexp-*n 5 h16 ":") h16) "::"                      h16 )
	 (seq (? (rexp-*n 6 h16 ":") h16) "::"                          ))))

(define (uri-rexp:h16)
  (rexp-n*m 1 4 char-set:uri-hex))

(define (uri-rexp:ls32)
  (rexp-alternatives (rexp-sequence (uri-rexp:h16)
				    ":"
				    (uri-rexp:h16))
		     (uri-rexp:ipv4-address)))

(define (uri-rexp:ipv4-address)
  (rexp-sequence (uri-rexp:dec-octet)
		 "."
		 (uri-rexp:dec-octet)
		 "."
		 (uri-rexp:dec-octet)
		 "."
		 (uri-rexp:dec-octet)))

(define (uri-rexp:dec-octet)
  (rexp-alternatives (rexp-sequence char-set:uri-digit)
		     (rexp-sequence (string->char-set "123456789")
				    char-set:uri-digit)
		     (rexp-sequence "1"
				    char-set:uri-digit
				    char-set:uri-digit)
		     (rexp-sequence "2"
				    (string->char-set "01234")
				    char-set:uri-digit)
		     (rexp-sequence "25"
				    (string->char-set "012345"))))

(define (uri-rexp:reg-name)
  (rexp* (uri-rexp:pct-encoded char-set:uri-reg-name)))

(define (uri-rexp:path)
  (rexp-alternatives (uri-rexp:path-abempty)
		     (uri-rexp:path-absolute)
		     (uri-rexp:path-noscheme)
		     (uri-rexp:path-rootless)
		     (uri-rexp:path-empty)))

(define (uri-rexp:path-abempty)
  (rexp* "/" (uri-rexp:segment)))

(define (uri-rexp:path-absolute)
  (rexp-sequence "/"
		 (rexp-optional (uri-rexp:segment-nz)
				(rexp* "/" (uri-rexp:segment)))))

(define (uri-rexp:path-noscheme)
  (rexp-sequence (uri-rexp:segment-nz-nc)
		 (rexp* "/" (uri-rexp:segment))))

(define (uri-rexp:path-rootless)
  (rexp-sequence (uri-rexp:segment-nz)
		 (rexp* "/" (uri-rexp:segment))))

(define (uri-rexp:path-empty)
  (rexp-sequence))

(define (uri-rexp:segment)
  (rexp* (uri-rexp:pct-encoded char-set:uri-segment)))

(define (uri-rexp:segment-nz)
  (rexp+ (uri-rexp:pct-encoded char-set:uri-segment)))

(define (uri-rexp:segment-nz-nc)
  (rexp+ (uri-rexp:pct-encoded char-set:uri-segment-nc)))

(define (uri-rexp:query)
  (rexp* char-set:uri-query))

(define (uri-rexp:fragment)
  (rexp* char-set:uri-fragment))

(define (uri-rexp:pct-encoded cs)
  (rexp-alternatives cs
		     (rexp-sequence "%"
				    char-set:uri-hex
				    char-set:uri-hex)))

(define char-set:uri-alpha)
(define char-set:uri-digit)
(define char-set:uri-hex)
(define char-set:uri-scheme)
(define char-set:uri-userinfo)
(define char-set:uri-opaque-auth)
(define char-set:uri-ipvfuture)
(define char-set:uri-reg-name)
(define char-set:uri-segment)
(define char-set:uri-segment-nc)
(define char-set:uri-query)
(define char-set:uri-fragment)

(define parser:userinfo)
(define matcher:reg-name)
(define parser:segment)
(define parser:segment-nz)
(define parser:segment-nz-nc)
(define parser:query)
(define parser:fragment)

(define url:char-set:unreserved)
(define url:char-set:unescaped)

(define (initialize-package!)
  (set! char-set:uri-alpha
	(string->char-set
	 "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  (set! char-set:uri-digit (string->char-set "0123456789"))
  (set! char-set:uri-hex (string->char-set "0123456789abcdefABCDEF"))
  (set! char-set:uri-scheme
	(char-set-union char-set:uri-alpha
			char-set:uri-digit
			(string->char-set "+-.")))
  (let* ((uri-char
	  (char-set-union char-set:uri-alpha
			  char-set:uri-digit
			  (string->char-set "!$&'()*+,-./:;=?@_~")))
	 (component-chars
	  (lambda (free)
	    (char-set-difference uri-char (string->char-set free)))))
    (set! char-set:uri-userinfo		(component-chars "/?@"))
    (set! char-set:uri-ipvfuture	char-set:uri-userinfo)
    (set! char-set:uri-reg-name		(component-chars "/:?@"))
    (set! char-set:uri-segment		(component-chars "/?"))
    (set! char-set:uri-segment-nc	(component-chars "/:?"))
    (set! char-set:uri-query		uri-char)
    (set! char-set:uri-fragment		uri-char)

    (set! char-set:uri-opaque-auth
	  (char-set-union (component-chars "/?")
			  (string->char-set "[]"))))

  (set! parser:userinfo		(component-parser-* char-set:uri-userinfo))
  (set! matcher:reg-name	(component-matcher-* char-set:uri-reg-name))
  (set! parser:segment		(component-parser-* char-set:uri-segment))
  (set! parser:segment-nz	(component-parser-+ char-set:uri-segment))
  (set! parser:segment-nz-nc	(component-parser-+ char-set:uri-segment-nc))
  (set! parser:query		(component-parser-* char-set:uri-query))
  (set! parser:fragment		(component-parser-* char-set:uri-fragment))

  ;; backwards compatibility:
  (set! url:char-set:unreserved
	(char-set-union char-set:uri-alpha
			char-set:uri-digit
			(string->char-set "!$'()*+,-._")))
  (set! url:char-set:unescaped
	(char-set-union url:char-set:unreserved
			(string->char-set ";/?:@&=")))
  unspecific)

;;;; Testing

(define (test-merge-uris #!optional verbose?)
  (let ((verbose? (if (default-object? verbose?) #f verbose?))
	(base-uri
	 (string->uri "http://a/b/c/d;p?q"))
	(normal-examples
	 '(("g:h"           "g:h")
	   ("g"             "http://a/b/c/g")
	   ("./g"           "http://a/b/c/g")
	   ("g/"            "http://a/b/c/g/")
	   ("/g"            "http://a/g")
	   ("//g"           "http://g")
	   ("?y"            "http://a/b/c/d;p?y")
	   ("g?y"           "http://a/b/c/g?y")
	   ("#s"            "http://a/b/c/d;p?q#s")
	   ("g#s"           "http://a/b/c/g#s")
	   ("g?y#s"         "http://a/b/c/g?y#s")
	   (";x"            "http://a/b/c/;x")
	   ("g;x"           "http://a/b/c/g;x")
	   ("g;x?y#s"       "http://a/b/c/g;x?y#s")
	   (""              "http://a/b/c/d;p?q")
	   ("."             "http://a/b/c/")
	   ("./"            "http://a/b/c/")
	   (".."            "http://a/b/")
	   ("../"           "http://a/b/")
	   ("../g"          "http://a/b/g")
	   ("../.."         "http://a/")
	   ("../../"        "http://a/")
	   ("../../g"       "http://a/g")))
	(abnormal-examples
	 '(("../../../g"    "http://a/g")
	   ("../../../../g" "http://a/g")
	   ("/./g"          "http://a/g")
	   ("/../g"         "http://a/g")
	   ("g."            "http://a/b/c/g.")
	   (".g"            "http://a/b/c/.g")
	   ("g.."           "http://a/b/c/g..")
	   ("..g"           "http://a/b/c/..g")
	   ("./../g"        "http://a/b/g")
	   ("./g/."         "http://a/b/c/g/")
	   ("g/./h"         "http://a/b/c/g/h")
	   ("g/../h"        "http://a/b/c/h")
	   ("g;x=1/./y"     "http://a/b/c/g;x=1/y")
	   ("g;x=1/../y"    "http://a/b/c/y")
	   ("g?y/./x"       "http://a/b/c/g?y/./x")
	   ("g?y/../x"      "http://a/b/c/g?y/../x")
	   ("g#s/./x"       "http://a/b/c/g#s/./x")
	   ("g#s/../x"      "http://a/b/c/g#s/../x")
	   ("http:g"        "http:g")))
	(n-errors 0))
    (let ((run-examples
	   (lambda (examples)
	     (for-each (lambda (p)
			 (let ((reference (car p))
			       (result (cadr p)))
			   (let ((s
				  (uri->string
				   (merge-uris reference base-uri))))
			     (cond ((not (string=? s result))
				    (set! n-errors (+ n-errors 1))
				    (write-line (list reference result s)))
				   (verbose?
				    (write-line (list reference result s)))))))
		       examples))))
      (if verbose? (write-string "Normal examples:\n"))
      (run-examples normal-examples)
      (if verbose? (write-string "\nAbnormal examples:\n"))
      (run-examples abnormal-examples)
      (if verbose? (newline))
      (if (> n-errors 0)
	  (write n-errors)
	  (write-string "No"))
      (write-string " errors found")
      (newline))))

;;;; Partial URIs

(define (string->partial-uri string #!optional start end)
  (parse-partial-uri (open-input-string string start end)))

(define (string->partial-absolute-uri string #!optional start end)
  (parse-partial-absolute-uri (open-input-string string start end)))

(define (parse-partial-uri port)
  (%parse-partial-uri port ppu:start-reference))

(define (parse-partial-absolute-uri port)
  (%parse-partial-uri port ppu:start-absolute))

(define (%parse-partial-uri port initial-state)
  (initial-state port  (open-output-string) (make-partial-uri initial-state)))

(define (partial-uri->string puri)
  (call-with-output-string
    (lambda (port)
      (write-partial-uri puri port))))

(define (write-partial-uri puri port)
  (guarantee-partial-uri puri 'WRITE-PARTIAL-URI)
  (let ((write-component
	 (lambda (component prefix suffix)
	   (if component
	       (begin
		 (write-string prefix port)
		 (write-string component port)
		 (write-string suffix port))))))
    (write-component (partial-uri-scheme puri) "" ":")
    (write-component (partial-uri-authority puri) "//" "")
    (write-component (partial-uri-path puri) "" "")
    (write-component (partial-uri-query puri) "?" "")
    (write-component (partial-uri-fragment puri) "#" "")
    (write-component (partial-uri-extra puri) "" "")))

(define-record-type <partial-uri>
    (%make-partial-uri state scheme authority path query fragment extra)
    partial-uri?
  (state partial-uri-state set-partial-uri-state!)
  (scheme partial-uri-scheme set-partial-uri-scheme!)
  (authority partial-uri-authority set-partial-uri-authority!)
  (path partial-uri-path set-partial-uri-path!)
  (query partial-uri-query set-partial-uri-query!)
  (fragment partial-uri-fragment set-partial-uri-fragment!)
  (extra partial-uri-extra set-partial-uri-extra!))

(set-record-type-unparser-method! <partial-uri>
  (standard-unparser-method 'PARTIAL-URI
    (lambda (puri port)
      (write-char #\space port)
      (write-partial-uri puri port))))

(define-guarantee partial-uri "partial URI")

(define (make-partial-uri state)
  (%make-partial-uri state #f #f #f #f #f #f))

(define (partial-uri-state-name puri)
  (let ((name (%partial-uri-state-name puri)))
    (case name
      ((START-REFERENCE START-ABSOLUTE) 'START)
      ((SCHEME-REFERENCE SCHEME-ABSOLUTE) 'SCHEME)
      ((SEGMENT-NZ-NC) 'PATH)
      ((HIER-PART INIT-SLASH)
       (if (partial-uri-scheme puri) 'HIER-PART 'RELATIVE-PART))
      (else name))))

(define (%partial-uri-state-name puri)
  (let ((state (partial-uri-state puri)))
    (let loop ((ps state-names))
      (if (not (pair? ps))
	  (error "Unknown partial-URI state:" state))
      (if (eq? (cdar ps) state)
	  (caar ps)
	  (loop (cdr ps))))))

(define (define-state-name name state)
  (let loop ((ps state-names))
    (if (pair? ps)
	(if (eq? (caar ps) name)
	    (set-cdr! (car ps) state)
	    (loop (cdr ps)))
	(begin
	  (set! state-names (cons (cons name state) state-names))
	  unspecific))))

(define state-names '())

(define (buffer->scheme buffer puri)
  (set-partial-uri-scheme! puri (get-output-string! buffer)))

(define (buffer->authority buffer puri)
  (set-partial-uri-authority! puri (get-output-string! buffer)))

(define (buffer->path buffer puri)
  (set-partial-uri-path! puri (get-output-string! buffer)))

(define (buffer->query buffer puri)
  (set-partial-uri-query! puri (get-output-string! buffer)))

(define (buffer->fragment buffer puri)
  (set-partial-uri-fragment! puri (get-output-string! buffer)))

(define (ppu-finish buffer puri error?)
  (set-partial-uri-extra! puri (get-output-string! buffer))
  (values puri error?))

(define-syntax define-ppu-state
  (sc-macro-transformer
   (lambda (form environment)
     environment

     (define (reorder-clauses clauses)
       (let ((eof (assq 'EOF clauses)))
	 (if eof
	     (cons eof (delq eof clauses))
	     (cons '(EOF) clauses))))

     (define (expand-clause clause)
       (let ((key (car clause))
	     (actions (cdr clause)))
	 `(,(cond ((eq? key 'EOF)
		   `(EOF-OBJECT? CHAR))
		  ((fix:= (string-length (symbol-name key)) 1)
		   `(CHAR=? CHAR ,(string-ref (symbol-name key) 0)))
		  (else
		   `(CHAR-SET-MEMBER? ,(symbol 'CHAR-SET:URI- key) CHAR)))
	   ,@(map (lambda (action)
		    (cond ((action:push? action) (expand:push action))
			  ((action:set? action) (expand:set action))
			  ((action:go? action) (expand:go action))
			  (else (error "Unknown action:" action))))
		  actions)
	   ,@(if (eq? key 'EOF)
		 '((PPU-FINISH BUFFER PURI #F))
		 '()))))

     (define (action:push? action) (syntax-match? '('PUSH ? SYMBOL) action))
     (define (expand:push action)
       `(WRITE-CHAR ,(if (pair? (cdr action))
			 (string-ref (symbol-name (cadr action)) 0)
			 'CHAR)
		    BUFFER))

     (define (action:set? action) (syntax-match? '('SET SYMBOL) action))
     (define (expand:set action)
       `(,(symbol 'BUFFER-> (cadr action)) BUFFER PURI))

     (define (action:go? action) (symbol? action))
     (define (expand:go action) `(,(symbol 'PPU: action) PORT BUFFER PURI))

     (if (syntax-match? '(SYMBOL + (SYMBOL * DATUM)) (cdr form))
	 (let ((state-name (cadr form))
	       (clauses (cddr form)))
	   (let ((name (symbol 'PPU: state-name)))
	     `(BEGIN
		(DEFINE (,name PORT BUFFER PURI)
		  (SET-PARTIAL-URI-STATE! PURI ,name)
		  (LET ((CHAR (READ-CHAR PORT)))
		    (COND ,@(map expand-clause (reorder-clauses clauses))
			  (ELSE
			   (UNREAD-CHAR CHAR PORT)
			   (PPU-FINISH BUFFER PURI #T)))))
		(DEFINE-STATE-NAME ',state-name ,name))))
	 (ill-formed-syntax form)))))

(define-ppu-state start-reference
  (/ (push) init-slash)
  (alpha (push) scheme-reference)
  (segment-nc (push) segment-nz-nc)
  (? (set path) query)
  (|#| (set path) fragment)
  (EOF))

(define-ppu-state scheme-reference
  (scheme (push) scheme-reference)
  (segment-nc (push) segment-nz-nc)
  (: (set scheme) hier-part)
  (/ (push) path)
  (? (set path) query)
  (|#| (set path) fragment)
  (EOF))

(define-ppu-state segment-nz-nc
  (segment-nc (push) segment-nz-nc)
  (/ (push) path)
  (? (set path) query)
  (|#| (set path) fragment)
  (EOF (set path)))

(define-ppu-state start-absolute
  (alpha (push) scheme-absolute)
  (EOF))

(define-ppu-state scheme-absolute
  (scheme (push) scheme-absolute)
  (: (set scheme) hier-part)
  (EOF))

(define-ppu-state hier-part
  (segment (push) path)
  (/ init-slash)
  (? (set path) query)
  (|#| (set path) fragment)
  (EOF))

(define-ppu-state init-slash
  (segment (push /) (push) path)
  (/ authority)
  (? (push /) (set path) query)
  (|#| (push /) (set path) fragment)
  (EOF))

(define-ppu-state authority
  (opaque-auth (push) authority)
  (/ (set authority) (push) path)
  (? (set authority) query)
  (|#| (set authority) fragment)
  (EOF (set authority)))

(define-ppu-state path
  (segment (push) path)
  (/ (push) path)
  (? (set path) query)
  (|#| (set path) fragment)
  (EOF (set path)))

(define-ppu-state query
  (query (push) query)
  (|#| (set query) fragment)
  (EOF (set query)))

(define-ppu-state fragment
  (fragment (push) fragment)
  (EOF (set fragment)))