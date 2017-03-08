#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016,
    2017 Massachusetts Institute of Technology

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

;;;; Scheme Parser
;;; package: (runtime parser)

(declare (usual-integrations))

(define *parser-associate-positions?* #!default)
(define *parser-atom-delimiters* #!default)
(define *parser-canonicalize-symbols?* #!default)
(define *parser-constituents* #!default)
(define *parser-radix* #!default)

(define param:parser-associate-positions?)
(define param:parser-atom-delimiters)
(define param:parser-enable-attributes?)
(define param:parser-fold-case?)
(define param:parser-constituents)
(define param:parser-keyword-style)
(define param:parser-radix)

(define runtime-param:parser-associate-positions?)
(define runtime-param:parser-atom-delimiters)
(define runtime-param:parser-enable-attributes?)
(define runtime-param:parser-fold-case?)
(define runtime-param:parser-constituents)
(define runtime-param:parser-keyword-style)
(define runtime-param:parser-radix)

(define ignore-extra-list-closes #t)

(define (param-getter param-name #!optional fluid-name)
  (lambda (environment)
    (let ((param (repl-environment-value environment param-name)))
      (if (default-object? fluid-name)
	  (param)
	  (let ((fluid (repl-environment-value environment fluid-name)))
	    (if (default-object? fluid)
		(param)
		((parameter-converter param) fluid)))))))

(define (repl-environment-value environment name)
  (environment-lookup-or environment name
    (lambda ()
      (environment-lookup-or (->environment '(USER)) name
	(lambda ()
	  (environment-lookup environment name))))))

(define get-param:parser-associate-positions?
  (param-getter 'param:parser-associate-positions?
		'*parser-associate-positions?*))

(define get-param:parser-atom-delimiters
  (param-getter 'param:parser-atom-delimiters '*parser-atom-delimiters*))

(define get-param:parser-fold-case?
  (param-getter 'param:parser-fold-case? '*parser-canonicalize-symbols?*))

(define get-param:parser-constituents
  (param-getter 'param:parser-constituents '*parser-constituents*))

(define get-param:parser-enable-attributes?
  (param-getter 'param:parser-enable-attributes?))

(define get-param:parser-keyword-style
  (param-getter 'param:parser-keyword-style))

(define get-param:parser-radix
  (param-getter 'param:parser-radix '*parser-radix*))

(define (parse-object port environment)
  ((top-level-parser port) port environment))

(define (parse-objects port environment last-object?)
  (let ((parser (top-level-parser port)))
    (let loop ()
      (let ((object (parser port environment)))
	(if (last-object? object)
	    '()
	    (cons-stream object (loop)))))))

(define (top-level-parser port)
  (or (port/operation port 'READ)
      (let ((read-start (port/operation port 'READ-START))
	    (read-finish (port/operation port 'READ-FINISH)))
	(lambda (port environment)
	  (if read-start (read-start port))
	  (let restart ()
	    (let* ((db (initial-db port environment))
		   (object (dispatch port db 'TOP-LEVEL)))
	      (if (eq? object restart-parsing)
		  (restart)
		  (begin
		    (if read-finish (read-finish port))
		    (finish-parsing object db)))))))))

(define (read-in-context port db ctx)
  (let ((object (dispatch port db ctx)))
    (cond ((eof-object? object)	(error:premature-eof port))
	  ((eq? object restart-parsing) (error:unexpected-restart port))
	  (else object))))

(define-integrable (read-object port db)
  (read-in-context port db 'OBJECT))

(define (dispatch port db ctx)
  (let ((handlers (parser-table/initial system-global-parser-table)))
    (let loop ()
      (let* ((position (current-position port db))
	     (char (%read-char port db)))
	(if (eof-object? char)
	    char
	    (let ((object ((get-handler char handlers) port db ctx char)))
	      (cond ((eq? object continue-parsing) (loop))
		    ((eq? object restart-parsing) object)
		    (else
		     (record-object-position! position object db)
		     object))))))))

;; Causes the dispatch to be re-run.
;; Used to discard things like whitespace and comments.
(define continue-parsing
  (list 'CONTINUE-PARSING))

;; Causes the dispatch to finish, but the top-level parser will return
;; back into the dispatch after re-initializing the db.  This is used
;; to reset the parser when changing read syntax as specified by the
;; file attributes list.
(define restart-parsing
  (list 'RESTART-PARSING))

(define (handler:special port db ctx char1)
  (let ((char2 (%read-char/no-eof port db)))
    ((get-handler char2 (parser-table/special system-global-parser-table))
     port db ctx char1 char2)))

(define (get-handler char handlers)
  (let ((n (char->integer char)))
    (if (not (fix:< n #x100))
	(error:illegal-char char))
    (let ((handler (vector-ref handlers n)))
      (if (not handler)
	  (error:illegal-char char))
      handler)))

(define system-global-parser-table)
(define char-set/constituents)
(define char-set/atom-delimiters)
(define char-set/symbol-quotes)
(define char-set/number-leaders)

(define (initialize-package!)
  (set! char-set/constituents
	(char-set-difference char-set:graphic
			     char-set:whitespace))
  (set! char-set/atom-delimiters
	(char-set-union char-set:whitespace
			;; Note that #\, may break older code.
			(string->char-set "()[]{}\";'`,")
			(char-set #\U+00AB #\U+00BB)))
  (set! char-set/symbol-quotes
	(string->char-set "\\|"))
  (set! char-set/number-leaders
	(char-set-union char-set:numeric
			(string->char-set "+-.")))

  (set! system-global-parser-table
	(make-initial-parser-table))

  (set! param:parser-associate-positions?
	(make-unsettable-parameter #f
				   boolean-converter))
  (set! param:parser-atom-delimiters
	(make-unsettable-parameter char-set/atom-delimiters
				   char-set-converter))
  (set! param:parser-fold-case?
	(make-unsettable-parameter #t
				   boolean-converter))
  (set! param:parser-constituents
	(make-unsettable-parameter char-set/constituents
				   char-set-converter))
  (set! param:parser-enable-attributes?
	(make-unsettable-parameter #t
				   boolean-converter))
  (set! param:parser-keyword-style
	(make-unsettable-parameter #f
				   keyword-style-converter))
  (set! param:parser-radix
	(make-unsettable-parameter 10
				   radix-converter))

  (set! runtime-param:parser-associate-positions?
	(copy-parameter param:parser-associate-positions?))
  (set! runtime-param:parser-atom-delimiters
	(copy-parameter param:parser-atom-delimiters))
  (set! runtime-param:parser-fold-case?
	(copy-parameter param:parser-fold-case?))
  (set! runtime-param:parser-constituents
	(copy-parameter param:parser-constituents))
  (set! runtime-param:parser-enable-attributes?
	(copy-parameter param:parser-enable-attributes?))
  (set! runtime-param:parser-keyword-style
	(copy-parameter param:parser-keyword-style))
  (set! runtime-param:parser-radix
	(copy-parameter param:parser-radix))

  (set! hashed-object-interns (make-strong-eq-hash-table))
  (initialize-condition-types!))

(define (make-initial-parser-table)

  (define (store-char v c h)
    (vector-set! v (char->integer c) h))

  (define (store-char-set v c h)
    (for-each (lambda (c) (store-char v c h))
	      (char-set-members c)))

  (let ((initial (make-vector #x100 #f))
	(special (make-vector #x100 #f))
	(symbol-leaders
	 (char-set-difference char-set/constituents
			      (char-set-union char-set/atom-delimiters
					      char-set/number-leaders)))
	(special-number-leaders
	 (string->char-set "bBoOdDxXiIeEsSlL")))

    (store-char-set initial char-set:whitespace handler:whitespace)
    (store-char-set initial char-set/number-leaders handler:atom)
    (store-char-set initial symbol-leaders handler:symbol)
    (store-char-set special special-number-leaders handler:number)
    (store-char initial #\( handler:list)
    (store-char special #\( handler:vector)
    (store-char special #\< handler:uri)
    (store-char special #\[ handler:hashed-object)
    (store-char initial #\) handler:close-parenthesis)
    (store-char initial #\] handler:close-bracket)
    (store-char initial #\; handler:comment)
    (store-char initial #\| handler:quoted-symbol)
    (store-char special #\| handler:multi-line-comment)
    (store-char special #\; handler:expression-comment)
    (store-char initial #\' handler:quote)
    (store-char initial #\` handler:quasiquote)
    (store-char initial #\, handler:unquote)
    (store-char initial #\" handler:string)
    (store-char initial #\# handler:special)
    (store-char special #\f handler:false)
    (store-char special #\F handler:false)
    (store-char special #\t handler:true)
    (store-char special #\T handler:true)
    (store-char special #\u handler:unsigned-vector)
    (store-char special #\* handler:bit-string)
    (store-char special #\\ handler:char)
    (store-char special #\! handler:named-constant)
    (store-char special #\@ handler:unhash)
    (store-char-set special char-set:numeric handler:special-arg)

    (make-parser-table initial special)))

(define (boolean-converter value)
  (guarantee boolean? value))

(define (char-set-converter value)
  (guarantee char-set? value)
  value)

(define (keyword-style-converter value)
  (if (not (memq value '(#f prefix suffix)))
      (error "Invalid keyword style:" value))
  value)

(define (radix-converter value)
  (if (not (memv value '(2 8 10 16)))
      (error "Invalid parser radix:" value))
  value)

(define (handler:whitespace port db ctx char)
  port db ctx char
  continue-parsing)

(define (start-attributes-comment port db)
  (and (db-enable-attributes? db)
       ;; If we're past the second line, just discard.
       (let ((line (current-line port db)))
	 (and line
	      (< line 2)))
       (string-builder)))

(define (finish-attributes-comment builder port)
  (let ((attributes (and builder (parse-file-attributes-string (builder)))))
    (if attributes
	(begin
	  (process-file-attributes attributes port)
	  restart-parsing)
	continue-parsing)))

(define (handler:comment port db ctx char)
  (declare (ignore ctx char))
  (let ((builder (start-attributes-comment port db)))
    (let walk ()
      (let ((char (%read-char port db)))
	(cond ((eof-object? char)
	       (finish-attributes-comment builder port)
	       char)
	      ((char=? char #\newline)
	       (finish-attributes-comment builder port))
	      (else
	       (if builder (builder char))
	       (walk)))))))

(define (handler:multi-line-comment port db ctx char1 char2)
  (declare (ignore ctx char1 char2))
  (let ((builder (start-attributes-comment port db)))

    (define (walk depth)
      (let ((char (%read-char/no-eof port db)))
	(case char
	  ((#\#)
	   (if builder (builder char))
	   (walk-sharp depth))
	  ((#\|)
	   (if (and builder (> depth 0))
	       (builder char))
	   (walk-vbar depth))
	  (else
	   (if builder (builder char))
	   (walk depth)))))

    (define (walk-sharp depth)
      (let ((char (%read-char/no-eof port db)))
	(if builder (builder char))
	(case char
	  ((#\#) (walk-sharp depth))
	  ((#\|) (walk (+ depth 1)))	; push
	  (else (walk depth)))))

    (define (walk-vbar depth)
      (let ((char (%read-char/no-eof port db)))
	(case char
	  ((#\#)
	   (if (> depth 0)
	       (begin			; pop
		 (if builder (builder char))
		 (walk (- depth 1)))))
	  ((#\|)
	   (if builder (builder char))
	   (walk-vbar depth))
	  (else
	   (if builder (builder char))
	   (walk depth)))))

    (walk 0)
    (finish-attributes-comment builder port)))

;; It would be better if we could skip over the object without
;; creating it, but for now this will work.
(define (handler:expression-comment port db ctx char1 char2)
  ctx char1 char2
  (read-object port db)
  continue-parsing)

(define (handler:atom port db ctx char)
  ctx
  (let ((string (parse-atom port db (list char))))
    (or (maybe-keyword db string)
	(string->number string (db-radix db))
	(string->symbol string))))

(define (handler:symbol port db ctx char)
  ctx
  (let ((string (parse-atom port db (list char))))
    (or (maybe-keyword db string)
	(string->symbol string))))

(define (maybe-keyword db string)
  (cond ((and (eq? 'SUFFIX (db-keyword-style db))
	      (string-suffix? ":" string)
	      (fix:> (string-length string) 1))
	 (string->keyword
	  (string-head string
			(fix:- (string-length string) 1))))
	((and (eq? 'SUFFIX (db-keyword-style db))
	      (string-prefix? ":" string)
	      (fix:> (string-length string) 1))
	 (string->keyword (string-tail string 1)))
	(else #f)))

(define (handler:number port db ctx char1 char2)
  ctx
  (parse-number port db (list char1 char2)))

(define (parse-number port db prefix)
  (let ((string (parse-atom port db prefix)))
    (or (string->number string (db-radix db))
	(error:illegal-number string))))

(define (parse-atom port db prefix)
  (let ((builder (string-builder))
	(atom-delimiters (db-atom-delimiters db)))

    (define (%peek)
      (if (pair? prefix)
	  (car prefix)
	  (%peek-char port db)))

    (define (%discard)
      (if (pair? prefix)
	  (begin
	    (set! prefix (cdr prefix))
	    unspecific)
	  (%read-char port db)))

    (define %emit
      (if (db-fold-case? db)
	  (lambda (char)
	    (builder (char-foldcase-full char)))
	  (lambda (char)
	    (builder char))))

    (let loop ()
      (let ((char (%peek)))
	(if (or (eof-object? char)
		(char-in-set? char atom-delimiters))
	    (builder)
	    (begin
	      (%discard)
	      (%emit char)
	      (loop)))))))

(define (handler:list port db ctx char)
  ctx char
  (let loop ((objects '()))
    (let ((object (read-in-context port db 'CLOSE-PAREN-OK)))
      (if (eq? object close-parenthesis)
	  (let ((objects (reverse! objects)))
	    (fix-up-list! objects)
	    objects)
	  (loop (cons object objects))))))

(define (fix-up-list! objects)
  (let loop ((objects* objects) (prev #f))
    (if (pair? objects*)
	(if (eq? (car objects*) '.)
	    (begin
	      (if (not (and prev
			    (pair? (cdr objects*))
			    (null? (cddr objects*))))
		  (error:illegal-dot-usage objects))
	      (set-cdr! prev (cadr objects*)))
	    (loop (cdr objects*) objects*)))))

(define (handler:vector port db ctx char1 char2)
  ctx char1 char2
  (let loop ((objects '()))
    (let ((object (read-in-context port db 'CLOSE-PAREN-OK)))
      (if (eq? object close-parenthesis)
	  (list->vector (reverse! objects))
	  (loop (cons object objects))))))

(define (handler:unsigned-vector port db ctx char1 char2)
  ctx
  (let ((atom (parse-atom port db '())))
    (if (not (and atom (string=? atom "8")))
	(error:unsupported-vector (string char1 char2 (or atom "")))))
  (let ((char (%read-char/no-eof port db)))
    (if (not (char=? char #\())
	(error:illegal-char char)))
  (let loop ((bytes '()))
    (let ((object (read-in-context port db 'CLOSE-PAREN-OK)))
      (if (eq? object close-parenthesis)
	  (let ((bytevector (make-bytevector (length bytes))))
	    (do ((bytes (reverse! bytes) (cdr bytes))
		 (index 0 (fix:+ index 1)))
		((not (pair? bytes)))
	      (bytevector-u8-set! bytevector index (car bytes)))
	    bytevector)
	  (begin
	    (guarantee byte? object)
	    (loop (cons object bytes)))))))

(define (handler:close-parenthesis port db ctx char)
  db
  (cond ((eq? ctx 'CLOSE-PAREN-OK)
	 close-parenthesis)
	((and (eq? ctx 'TOP-LEVEL)
	      (console-i/o-port? port)
	      ignore-extra-list-closes)
	 continue-parsing)
	(else
	 (error:unbalanced-close char))))

(define (handler:close-bracket port db ctx char)
  port db
  (if (not (eq? ctx 'CLOSE-BRACKET-OK))
      (error:unbalanced-close char))
  close-bracket)

(define close-parenthesis (list 'CLOSE-PARENTHESIS))
(define close-bracket (list 'CLOSE-BRACKET))

(define (handler:hashed-object port db ctx char1 char2)
  ctx char1 char2
  (let loop ((objects '()))
    (let ((object (read-in-context port db 'CLOSE-BRACKET-OK)))
      (if (eq? object close-bracket)
	  (let* ((objects (reverse! objects))
		 (lose (lambda () (error:illegal-hashed-object objects))))
	    (let ((method
		   (and (pair? objects)
			(interned-symbol? (car objects))
			(hash-table/get hashed-object-interns
					(car objects)
					(lambda (objects lose)
					  (if (pair? (cdr objects))
					      (parse-unhash (cadr objects))
					      (lose)))))))
	      (if method
		  (bind-condition-handler (list condition-type:error)
		      (lambda (condition) condition (lose))
		    (lambda ()
		      (method objects lose)))
		  (lose))))
	  (loop (cons object objects))))))

(define (define-bracketed-object-parser-method name method)
  (guarantee interned-symbol? name 'DEFINE-BRACKETED-OBJECT-PARSER-METHOD)
  (guarantee binary-procedure? method 'DEFINE-BRACKETED-OBJECT-PARSER-METHOD)
  (hash-table/put! hashed-object-interns name method))

(define hashed-object-interns)

(define (handler:unhash port db ctx char1 char2)
  ctx char1 char2
  (let ((object (parse-unhash (parse-number port db '()))))
    ;; This may seem a little random, because #@N doesn't just
    ;; return an object.  However, the motivation for this piece of
    ;; syntax is convenience -- and 99.99% of the time the result of
    ;; this syntax will be evaluated, and the user will expect the
    ;; result of the evaluation to be the object she was referring
    ;; to.  If the quotation isn't there, the user just gets
    ;; confused.
    (make-quotation object)))

(define (parse-unhash object)
  (if (not (exact-nonnegative-integer? object))
      (error:illegal-unhash object))
  (if (eq? object 0)
      #f
      (or (object-unhash object)
	  (error:undefined-hash object))))

(define (handler:quote port db ctx char)
  ctx char
  (list 'quote (read-object port db)))

(define (handler:quasiquote port db ctx char)
  ctx char
  (list 'quasiquote (read-object port db)))

(define (handler:unquote port db ctx char)
  ctx char
  (if (char=? (%peek-char/no-eof port db) #\@)
      (begin
	(%read-char port db)
	(list 'unquote-splicing (read-object port db)))
      (list 'unquote (read-object port db))))

(define (handler:string port db ctx char)
  ctx char
  (parse-delimited-string port db #\" #t))

(define (handler:quoted-symbol port db ctx char)
  ctx char
  (string->symbol (parse-delimited-string port db #\| #f)))

(define (parse-delimited-string port db delimiter allow-newline-escape?)
  (call-with-output-string
    (lambda (port*)

      (define (loop)
	(dispatch (%read-char/no-eof port db)))

      (define (dispatch char)
	(cond ((char=? delimiter char) unspecific)
	      ((char=? #\\ char) (parse-quoted))
	      (else (emit char))))

      (define (parse-quoted)
	(let ((char (%read-char/no-eof port db)))
	  (cond ((char=? char #\a) (emit #\bel))
		((char=? char #\b) (emit #\bs))
		((char=? char #\n) (emit #\newline))
		((char=? char #\r) (emit #\return))
		((char=? char #\t) (emit #\tab))
		((char=? char #\x) (emit (parse-hex-escape 0 '())))
		((and allow-newline-escape?
		      (or (char=? char #\newline)
			  (char=? char #\space)
			  (char=? char #\tab)))
		 (if (not (char=? char #\newline))
		     (let ((char (skip-space)))
		       (if (not (char=? char #\newline))
			   (error:illegal-char char))))
		 (dispatch (skip-space)))
		;; MIT/GNU extensions:
		((char=? char #\f) (emit #\page))
		((char=? char #\v) (emit #\vt))
		((char->digit char 3)
		 => (lambda (d) (emit (parse-octal-escape char d))))
		(else (emit char)))))

      (define (emit char)
	(write-char char port*)
	(loop))

      (define (skip-space)
	(let ((char (%read-char/no-eof port db)))
	  (if (or (char=? char #\space)
		  (char=? char #\tab))
	      (skip-space)
	      char)))

      (define (parse-hex-escape sv chars)
	(let* ((char (%read-char/no-eof port db))
	       (chars (cons char chars)))
	  (if (char=? #\; char)
	      (begin
		(if (not (unicode-scalar-value? sv))
		    (ill-formed-hex chars))
		(integer->char sv))
	      (let ((digit (char->digit char 16)))
		(if (not digit)
		    (ill-formed-hex chars))
		(parse-hex-escape (+ (* sv #x10) digit) chars)))))

      (define (ill-formed-hex chars)
	(error:illegal-string-escape
	 (list->string (cons* #\\ #\x (reverse chars)))))

      (define (parse-octal-escape c1 d1)
	(let* ((c2 (%read-char/no-eof port db))
	       (d2 (char->digit c2 8))
	       (c3 (%read-char/no-eof port db))
	       (d3 (char->digit c3 8)))
	  (if (not (and d2 d3))
	      (error:illegal-string-escape (list->string (list #\\ c1 c2 c3))))
	  (integer->char (fix:+ (fix:lsh (fix:+ (fix:lsh d1 3) d2) 3) d3))))

      (loop))))

(define (handler:false port db ctx char1 char2)
  ctx char1
  (let ((string (parse-atom port db (list char2))))
    (if (not (or (string=? string "f")
		 (string=? string "false")))
	(error:illegal-boolean string)))
  #f)

(define (handler:true port db ctx char1 char2)
  ctx char1
  (let ((string (parse-atom port db (list char2))))
    (if (not (or (string=? string "t")
		 (string=? string "true")))
	(error:illegal-boolean string)))
  #t)

(define (handler:bit-string port db ctx char1 char2)
  ctx char1 char2
  (let ((string (parse-atom port db '())))
    (let ((n-bits (string-length string)))
      (unsigned-integer->bit-string
       n-bits
       (let loop ((index 0) (result 0))
	 (if (fix:< index n-bits)
	     (loop (fix:+ index 1)
		   (+ (* result 2)
		      (case (string-ref string index)
			((#\0) 0)
			((#\1) 1)
			(else (error:illegal-bit-string string)))))
	     result))))))

(define (handler:char port db ctx char1 char2)
  ctx char1 char2
  (let ((char (%read-char/no-eof port db))
	(at-end?
	 (lambda ()
	   (let ((char (%peek-char port db)))
	     (or (eof-object? char)
		 (char-in-set? char (db-atom-delimiters db)))))))
    (if (or (char-in-set? char (db-atom-delimiters db))
	    (at-end?))
	char
	(name->char
	 (call-with-output-string
	   (lambda (port*)
	     (write-char char port*)
	     (let loop ()
	       (write-char (let ((char (%read-char/no-eof port db)))
			     (if (char=? char #\\)
				 (%read-char/no-eof port db)
				 char))
			   port*)
	       (if (not (at-end?))
		   (loop)))))
	 (db-fold-case? db)))))

(define (handler:named-constant port db ctx char1 char2)
  ctx char1 char2
  (let ((name (parse-atom port db '())))
    (cond ((string=? name "null") '())
	  ((string=? name "false") #f)
	  ((string=? name "true") #t)
	  ((string=? name "optional") lambda-tag:optional)
	  ((string=? name "rest") lambda-tag:rest)
	  ((string=? name "key") lambda-tag:key)
	  ((string=? name "aux") lambda-tag:aux)
	  ((string=? name "eof") (eof-object))
	  ((string=? name "default") (default-object))
	  ((string=? name "unspecific") unspecific)
	  ((string=? name "fold-case")
	   (set-db-fold-case! db #t)
	   continue-parsing)
	  ((string=? name "no-fold-case")
	   (set-db-fold-case! db #f)
	   continue-parsing)
	  (else
	   (error:illegal-named-constant name)))))

(define (handler:uri port db ctx char1 char2)
  ctx char1 char2
  (string->uri
   (call-with-output-string
     (lambda (port*)
       (let loop ()
	 (let ((char (%read-char/no-eof port db)))
	   (if (not (char=? char #\>))
	       (begin
		 (write-char char port*)
		 (loop)))))))))

(define (handler:special-arg port db ctx char1 char2)
  ctx char1
  (let loop ((n (char->digit char2 10)))
    (let ((char (%read-char/no-eof port db)))
      (cond ((char-numeric? char)
	     (loop (+ (* 10 n) (char->digit char 10))))
	    ((char=? char #\=)
	     (let ((object (read-object port db)))
	       (save-shared-object! db n object)
	       object))
	    ((char=? char #\#)
	     (get-shared-object db n))
	    (else
	     (error:illegal-char char))))))

(define (make-shared-objects)
  (make-strong-eqv-hash-table))

(define (save-shared-object! db n object)
  (let ((table (db-shared-objects db)))
    (if (not (eq? (hash-table/get table n non-shared-object)
		  non-shared-object))
	(error:re-shared-object n object))
    (hash-table/put! table n object)))

(define (get-shared-object db n)
  (let ((object (hash-table/get (db-shared-objects db) n non-shared-object)))
    (if (eq? object non-shared-object)
	(error:non-shared-object n))
    object))

(define non-shared-object
  (list 'NON-SHARED-OBJECT))

(define (%read-char port db)
  (let ((char
	 (let loop ()
	   (or ((db-read-char db) port)
	       (loop))))
	(op (db-discretionary-write-char db)))
    (if op
	(op char port))
    char))

(define (%read-char/no-eof port db)
  (let ((char (%read-char port db)))
    (if (eof-object? char)
	(error:premature-eof port))
    char))

(define-integrable (%peek-char port db)
  (let loop ()
    (or ((db-peek-char db) port)
	(loop))))

(define (%peek-char/no-eof port db)
  (let ((char (%peek-char port db)))
    (if (eof-object? char)
	(error:premature-eof port))
    char))

(define-record-type <db>
    (make-db port env shared-objects position-mapping discretionary-write-char
	     get-position input-line peek-char read-char)
    db?
  (port db-port)
  (env db-env)
  (shared-objects db-shared-objects)
  (position-mapping db-position-mapping set-db-position-mapping!)
  ;; Cached port operations
  (discretionary-write-char db-discretionary-write-char)
  (get-position db-get-position)
  (input-line db-input-line)
  (peek-char db-peek-char)
  (read-char db-read-char))

(define (initial-db port environment)
  (let ((environment
	 (if (default-object? environment)
	     (nearest-repl/environment)
	     (begin
	       (guarantee environment? environment)
	       environment))))
    (make-db port
	     environment
	     (make-shared-objects)
	     '()
	     (port/operation port 'DISCRETIONARY-WRITE-CHAR)
	     (position-operation port environment)
	     (port/operation port 'INPUT-LINE)
	     (port/operation port 'PEEK-CHAR)
	     (port/operation port 'READ-CHAR))))

(define (db-param-getter property env-getter)
  (lambda (db)
    (port-property (db-port db) property (env-getter (db-env db)))))

(define (db-param-setter property)
  (lambda (db value)
    (set-port-property! (db-port db) property value)))

(define db-enable-attributes?
  (db-param-getter 'parser-enable-attributes?
		   get-param:parser-enable-attributes?))

(define db-fold-case?
  (db-param-getter 'parser-fold-case? get-param:parser-fold-case?))

(define set-db-fold-case!
  (db-param-setter 'parser-fold-case?))

(define db-keyword-style
  (db-param-getter 'parser-keyword-style get-param:parser-keyword-style))

(define (db-env-getter env-getter)
  (lambda (db)
    (env-getter (db-env db))))

(define db-associate-positions?
  (db-env-getter get-param:parser-associate-positions?))

(define db-atom-delimiters
  (db-env-getter get-param:parser-atom-delimiters))

(define db-constituents
  (db-env-getter get-param:parser-constituents))

(define db-radix
  (db-env-getter get-param:parser-radix))

(define (position-operation port environment)
  (let ((default (lambda (port) port #f)))
    (if (get-param:parser-associate-positions? environment)
	(or (port/operation port 'POSITION)
	    default)
	default)))

(define (current-line port db)
  (let ((proc (db-input-line db)))
    (if proc
	(proc port)
	#f)))

(define-integrable (current-position port db)
  ((db-get-position db) port))

(define-integrable (record-object-position! position object db)
  (if (and position (object-pointer? object))
      (set-db-position-mapping! db
				(cons (cons position object)
				      (db-position-mapping db)))))

(define-integrable (finish-parsing object db)
  (if (db-associate-positions? db)
      (cons object (db-position-mapping db))
      object))

(define (process-file-attributes file-attribute-alist port)
  ;; Disable further attributes parsing.
  (set-port-property! port 'parser-enable-attributes? #f)
  ;; Save all the attributes; this helps with testing.
  (set-port-property! port 'parser-file-attributes file-attribute-alist)
  (process-keyword-attribute file-attribute-alist port)
  (process-mode-attribute file-attribute-alist port)
  (process-studly-case-attribute file-attribute-alist port))

(define (lookup-file-attribute file-attribute-alist attribute)
  (assoc attribute file-attribute-alist
	 (lambda (left right)
	   (string-ci=? (symbol->string left) (symbol->string right)))))

;;; Look for keyword-style: prefix or keyword-style: suffix
(define (process-keyword-attribute file-attribute-alist port)
  (let ((keyword-entry
	 (lookup-file-attribute file-attribute-alist 'KEYWORD-STYLE)))
    (if (pair? keyword-entry)
	(let ((value (cdr keyword-entry)))
	  (cond ((and (symbol? value)
		      (or (string-ci=? (symbol->string value) "none")
			  (string-ci=? (symbol->string value) "false")))
		 (set-port-property! port 'parser-keyword-style #f))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "prefix"))
		 (set-port-property! port 'parser-keyword-style 'prefix))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "suffix"))
		 (set-port-property! port 'parser-keyword-style 'suffix))
		(else
		 (warn "Unrecognized value for keyword-style" value)))))))

;;; Don't do anything with the mode, but warn if it isn't scheme.
(define (process-mode-attribute file-attribute-alist port)
  (declare (ignore port))
  (let ((mode-entry
	 (lookup-file-attribute file-attribute-alist 'MODE)))
    (if (pair? mode-entry)
	(let ((value (cdr mode-entry)))
	  (if (or (not (symbol? value))
		  (not (string-ci=? (symbol->string value) "scheme")))
	      (warn "Unexpected file mode:" (if (symbol? value)
						(symbol->string value)
						value)))))))

;; If you want to turn on studly case, then the attribute must be
;; exactly "sTuDly-case" and the value must be exactly "True".  After
;; all, case is important.  If you want to turn it off, the case of
;; the attribute and the value don't matter.
(define (process-studly-case-attribute file-attribute-alist port)
  (let ((studly-case-entry
	 (lookup-file-attribute file-attribute-alist 'STUDLY-CASE)))
    (if (pair? studly-case-entry)
	(let ((value (cdr studly-case-entry)))
	  (cond ((or (eq? value #t)
		     (and (symbol? value)
			  (string-ci=? (symbol->string value) "true")))
		 ;; STricTly cHeck thE case.
		 (cond ((not (string=? (symbol->string (car studly-case-entry))
				       "sTuDly-case"))
			(warn "Attribute name mismatch.  Expected sTuDly-case.")
			#f)
		       ((and (symbol? value)
			     (not (string=? (symbol->string value) "True")))
			(warn "Attribute value mismatch.  Expected True.")
			#f)
		       (else
			(set-port-property! port 'parser-fold-case? #f))))
		((or (not value)
		     (and (symbol? value)
			  (string-ci=? (symbol->string value) "false")))
		 (set-port-property! port 'parser-fold-case? #t))
		(else
		 (warn "Unrecognized value for sTuDly-case" value)))))))

(define-syntax define-parse-error
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '((+ SYMBOL) EXPRESSION) (cdr form))
	 (let ((name (caadr form))
	       (field-names (cdadr form))
	       (reporter (caddr form)))
	   (let ((ct (symbol 'CONDITION-TYPE: name)))
	     `(BEGIN
		(SET! ,ct
		      (MAKE-CONDITION-TYPE ',name CONDITION-TYPE:PARSE-ERROR
			  ',field-names
			(LAMBDA (CONDITION PORT)
			  (,reporter
			   ,@(map (lambda (field-name)
				    `(ACCESS-CONDITION CONDITION ',field-name))
				  field-names)
			   PORT))))
		(SET! ,(symbol 'ERROR: name)
		      (CONDITION-SIGNALLER ,ct
					   ',field-names
					   STANDARD-ERROR-HANDLER)))))
	 (ill-formed-syntax form)))))

(define condition-type:illegal-bit-string)
(define condition-type:illegal-boolean)
(define condition-type:illegal-char)
(define condition-type:illegal-dot-usage)
(define condition-type:illegal-hashed-object)
(define condition-type:illegal-named-constant)
(define condition-type:illegal-number)
(define condition-type:illegal-string-escape)
(define condition-type:illegal-unhash)
(define condition-type:no-quoting-allowed)
(define condition-type:non-shared-object)
(define condition-type:parse-error)
(define condition-type:premature-eof)
(define condition-type:re-shared-object)
(define condition-type:unbalanced-close)
(define condition-type:undefined-hash)
(define condition-type:unexpected-restart)
(define condition-type:unsupported-vector)
(define error:illegal-bit-string)
(define error:illegal-boolean)
(define error:illegal-char)
(define error:illegal-dot-usage)
(define error:illegal-hashed-object)
(define error:illegal-named-constant)
(define error:illegal-number)
(define error:illegal-string-escape)
(define error:illegal-unhash)
(define error:no-quoting-allowed)
(define error:non-shared-object)
(define error:premature-eof)
(define error:re-shared-object)
(define error:unbalanced-close)
(define error:undefined-hash)
(define error:unexpected-restart)
(define error:unsupported-vector)

(define (initialize-condition-types!)
  (set! condition-type:parse-error
	(make-condition-type 'PARSE-ERROR condition-type:error '()
	  (lambda (condition port)
	    condition
	    (write-string "Anonymous parsing error." port))))
  (define-parse-error (illegal-bit-string string)
    (lambda (string port)
      (write-string "Ill-formed bit string: #*" port)
      (write-string string port)))
  (define-parse-error (illegal-boolean string)
    (lambda (string port)
      (write-string "Ill-formed boolean: " port)
      (write-string string port)))
  (define-parse-error (illegal-char char)
    (lambda (char port)
      (write-string "Illegal character: " port)
      (write char port)))
  (define-parse-error (illegal-dot-usage objects)
    (lambda (objects port)
      (write-string "Ill-formed dotted list: " port)
      (write objects port)))
  (define-parse-error (illegal-hashed-object objects)
    (lambda (objects port)
      (write-string "Ill-formed object syntax: #[" port)
      (if (pair? objects)
	  (begin
	    (write (car objects) port)
	    (for-each (lambda (object)
			(write-char #\space port)
			(write object port))
		      (cdr objects))))
      (write-string "]" port)))
  (define-parse-error (illegal-named-constant name)
    (lambda (name port)
      (write-string "Ill-formed named constant: #!" port)
      (write name port)))
  (define-parse-error (illegal-string-escape string)
    (lambda (string port)
      (write-string "Ill-formed string escape: " port)
      (write-string string port)))
  (define-parse-error (illegal-number string)
    (lambda (string port)
      (write-string "Ill-formed number: " port)
      (write-string string port)))
  (define-parse-error (illegal-unhash object)
    (lambda (object port)
      (write-string "Ill-formed unhash syntax: #@" port)
      (write object port)))
  (define-parse-error (undefined-hash object)
    (lambda (object port)
      (write-string "Undefined hash number: #@" port)
      (write object port)))
  (define-parse-error (no-quoting-allowed string)
    (lambda (string port)
      (write-string "Quoting not permitted: " port)
      (write-string string port)))
  (define-parse-error (premature-eof port)
    (lambda (port* port)
      (write-string "Premature EOF on " port)
      (write port* port)))
  (define-parse-error (re-shared-object n object)
    (lambda (n object port)
      (write-string "Can't re-share object: #" port)
      (write n port)
      (write-string "=" port)
      (write object port)))
  (define-parse-error (non-shared-object n)
    (lambda (n port)
      (write-string "Reference to non-shared object: #" port)
      (write n port)
      (write-string "#" port)))
  (define-parse-error (unbalanced-close char)
    (lambda (char port)
      (write-string "Unbalanced close parenthesis: " port)
      (write char port)))
  (define-parse-error (unexpected-restart port)
    (lambda (port* port)
      (write-string "Unexpected parse restart on: " port)
      (write port* port)))
  (define-parse-error (unsupported-vector string)
    (lambda (string port)
      (write-string "Unsupported vector prefix: " port)
      (write-string string port)))
  unspecific)