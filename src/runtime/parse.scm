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
(define *parser-canonicalize-symbols?* #!default)
(define *parser-radix* #!default)

(define-deferred param:parser-associate-positions?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:parser-fold-case?
  (make-unsettable-parameter #t boolean-converter))

(define-deferred param:parser-enable-attributes?
  (make-unsettable-parameter #t boolean-converter))

(define-deferred param:parser-keyword-style
  (make-unsettable-parameter #f keyword-style-converter))

(define-deferred param:parser-radix
  (make-unsettable-parameter 10 radix-converter))

(define (boolean-converter value)
  (guarantee boolean? value))

(define (keyword-style-converter value)
  (if (not (memq value '(#f prefix suffix)))
      (error "Invalid keyword style:" value))
  value)

(define (radix-converter value)
  (if (not (memv value '(2 8 10 16)))
      (error "Invalid parser radix:" value))
  value)

(define (get-param:parser-associate-positions?)
  (if (default-object? *parser-associate-positions?*)
      (param:parser-associate-positions?)
      *parser-associate-positions?*))

(define (get-param:parser-fold-case?)
  (if (default-object? *parser-canonicalize-symbols?*)
      (param:parser-fold-case?)
      *parser-canonicalize-symbols?*))

(define (get-param:parser-radix)
  (if (default-object? *parser-radix*)
      (param:parser-radix)
      *parser-radix*))

(define (parse-object port)
  (let ((read-operation (port/operation port 'read)))
    (if read-operation
	(read-operation port)
	(begin
	  (let ((read-start (port/operation port 'read-start)))
	    (if read-start
		(read-start port)))
	  (let restart ()
	    (let* ((db (initial-db port))
		   (object (dispatch db 'top-level)))
	      (if (eq? object restart-parsing)
		  (restart)
		  (begin
		    (let ((read-finish (port/operation port 'read-finish)))
		      (if read-finish
			  (read-finish port)))
		    (finish-parsing object db)))))))))

(define (read-object db)
  (read-in-context db 'OBJECT))

(define (read-in-context db ctx)
  (let ((object (dispatch db ctx)))
    (cond ((eof-object? object)	(error:premature-eof db))
	  ((eq? object restart-parsing) (error:unexpected-restart db))
	  (else object))))

(define (dispatch db ctx)
  (let ((handlers (parser-table/initial system-global-parser-table)))
    (let loop ()
      (let* ((position ((db-get-position db)))
	     (char (%read-char db)))
	(if (eof-object? char)
	    char
	    (let ((object ((get-handler char handlers) db ctx char)))
	      (cond ((eq? object continue-parsing) (loop))
		    ((eq? object restart-parsing) object)
		    (else
		     (record-object-position! position object db)
		     object))))))))

;; Causes the dispatch to be re-run.
;; Used to discard things like whitespace and comments.
(define continue-parsing
  (list 'continue-parsing))

;; Causes the dispatch to finish, but the top-level parser will return
;; back into the dispatch after re-initializing the db.  This is used
;; to reset the parser when changing read syntax as specified by the
;; file attributes list.
(define restart-parsing
  (list 'restart-parsing))

(define (handler:special db ctx char1)
  (let ((char2 (%read-char/no-eof db)))
    ((get-handler char2 (parser-table/special system-global-parser-table))
     db ctx char1 char2)))

(define (get-handler char handlers)
  (let ((n (char->integer char)))
    (if (not (fix:< n #x100))
	(error:illegal-char char))
    (let ((handler (vector-ref handlers n)))
      (if (not handler)
	  (error:illegal-char char))
      handler)))

(define-deferred char-set/constituents
  (char-set-difference char-set:graphic
		       char-set:whitespace))

(define-deferred char-set/atom-delimiters
  (char-set-union char-set:whitespace
		  ;; Note that #\, may break older code.
		  (string->char-set "()[]{}\";'`,")
		  (char-set #\U+00AB #\U+00BB)))

(define-deferred char-set/symbol-quotes
  (string->char-set "\\|"))

(define-deferred char-set/number-leaders
  (char-set-union char-set:numeric
		  (string->char-set "+-.")))

(define-deferred system-global-parser-table
  (make-initial-parser-table))

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

(define (handler:whitespace db ctx char)
  db ctx char
  continue-parsing)

(define (start-attributes-comment db)
  (and (db-enable-attributes? db)
       ;; If we're past the second line, just discard.
       (let ((line ((db-input-line db))))
	 (and line
	      (< line 2)))
       (string-builder)))

(define (finish-attributes-comment builder db)
  (let ((attributes (and builder (parse-file-attributes-string (builder)))))
    (if attributes
	(begin
	  (process-file-attributes attributes db)
	  restart-parsing)
	continue-parsing)))

(define (handler:comment db ctx char)
  (declare (ignore ctx char))
  (let ((builder (start-attributes-comment db)))
    (let walk ()
      (let ((char (%read-char db)))
	(cond ((eof-object? char)
	       (finish-attributes-comment builder db)
	       char)
	      ((char=? char #\newline)
	       (finish-attributes-comment builder db))
	      (else
	       (if builder (builder char))
	       (walk)))))))

(define (handler:multi-line-comment db ctx char1 char2)
  (declare (ignore ctx char1 char2))
  (let ((builder (start-attributes-comment db)))

    (define (walk depth)
      (let ((char (%read-char/no-eof db)))
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
      (let ((char (%read-char/no-eof db)))
	(if builder (builder char))
	(case char
	  ((#\#) (walk-sharp depth))
	  ((#\|) (walk (+ depth 1)))	; push
	  (else (walk depth)))))

    (define (walk-vbar depth)
      (let ((char (%read-char/no-eof db)))
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
    (finish-attributes-comment builder db)))

;; It would be better if we could skip over the object without
;; creating it, but for now this will work.
(define (handler:expression-comment db ctx char1 char2)
  ctx char1 char2
  (read-object db)
  continue-parsing)

(define (handler:atom db ctx char)
  ctx
  (let ((string (parse-atom db (list char))))
    (or (maybe-keyword db string)
	(string->number string (get-param:parser-radix))
	(string->symbol string))))

(define (handler:symbol db ctx char)
  ctx
  (let ((string (parse-atom db (list char))))
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

(define (handler:number db ctx char1 char2)
  ctx
  (parse-number db (list char1 char2)))

(define (parse-number db prefix)
  (let ((string (parse-atom db prefix)))
    (or (string->number string (get-param:parser-radix))
	(error:illegal-number string))))

(define (parse-atom db prefix)
  (let ((builder (string-builder)))

    (define (%peek)
      (if (pair? prefix)
	  (car prefix)
	  (%peek-char db)))

    (define (%discard)
      (if (pair? prefix)
	  (begin
	    (set! prefix (cdr prefix))
	    unspecific)
	  (%read-char db)))

    (define %emit
      (if (db-fold-case? db)
	  (lambda (char)
	    (builder (char-foldcase-full char)))
	  (lambda (char)
	    (builder char))))

    (let loop ()
      (let ((char (%peek)))
	(if (or (eof-object? char)
		(char-in-set? char char-set/atom-delimiters))
	    (builder)
	    (begin
	      (%discard)
	      (%emit char)
	      (loop)))))))

(define (handler:list db ctx char)
  ctx char
  (let loop ((objects '()))
    (let ((object (read-in-context db 'close-paren-ok)))
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

(define (handler:vector db ctx char1 char2)
  ctx char1 char2
  (let loop ((objects '()))
    (let ((object (read-in-context db 'close-paren-ok)))
      (if (eq? object close-parenthesis)
	  (list->vector (reverse! objects))
	  (loop (cons object objects))))))

(define (handler:unsigned-vector db ctx char1 char2)
  ctx
  (let ((atom (parse-atom db '())))
    (if (not (and atom (string=? atom "8")))
	(error:unsupported-vector (string char1 char2 (or atom "")))))
  (let ((char (%read-char/no-eof db)))
    (if (not (char=? char #\())
	(error:illegal-char char)))
  (let loop ((bytes '()))
    (let ((object (read-in-context db 'close-paren-ok)))
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

(define (handler:close-parenthesis db ctx char)
  (cond ((eq? ctx 'close-paren-ok)
	 close-parenthesis)
	((and (eq? ctx 'top-level)
	      (console-i/o-port? (db-port db))
	      ignore-extra-list-closes)
	 continue-parsing)
	(else
	 (error:unbalanced-close char))))

(define (handler:close-bracket db ctx char)
  db
  (if (not (eq? ctx 'CLOSE-BRACKET-OK))
      (error:unbalanced-close char))
  close-bracket)

(define ignore-extra-list-closes #t)
(define close-parenthesis (list 'CLOSE-PARENTHESIS))
(define close-bracket (list 'CLOSE-BRACKET))

(define (handler:hashed-object db ctx char1 char2)
  ctx char1 char2
  (let loop ((objects '()))
    (let ((object (read-in-context db 'CLOSE-BRACKET-OK)))
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

(define-deferred hashed-object-interns
  (make-strong-eq-hash-table))

(define (handler:unhash db ctx char1 char2)
  ctx char1 char2
  (let ((object (parse-unhash (parse-number db '()))))
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

(define (handler:quote db ctx char)
  ctx char
  (list 'quote (read-object db)))

(define (handler:quasiquote db ctx char)
  ctx char
  (list 'quasiquote (read-object db)))

(define (handler:unquote db ctx char)
  ctx char
  (if (char=? (%peek-char/no-eof db) #\@)
      (begin
	(%read-char db)
	(list 'unquote-splicing (read-object db)))
      (list 'unquote (read-object db))))

(define (handler:string db ctx char)
  ctx char
  (parse-delimited-string db #\" #t))

(define (handler:quoted-symbol db ctx char)
  ctx char
  (string->symbol (parse-delimited-string db #\| #f)))

(define (parse-delimited-string db delimiter allow-newline-escape?)
  (call-with-output-string
    (lambda (port*)

      (define (loop)
	(dispatch (%read-char/no-eof db)))

      (define (dispatch char)
	(cond ((char=? delimiter char) unspecific)
	      ((char=? #\\ char) (parse-quoted))
	      (else (emit char))))

      (define (parse-quoted)
	(let ((char (%read-char/no-eof db)))
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
	(let ((char (%read-char/no-eof db)))
	  (if (or (char=? char #\space)
		  (char=? char #\tab))
	      (skip-space)
	      char)))

      (define (parse-hex-escape sv chars)
	(let* ((char (%read-char/no-eof db))
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
	(let* ((c2 (%read-char/no-eof db))
	       (d2 (char->digit c2 8))
	       (c3 (%read-char/no-eof db))
	       (d3 (char->digit c3 8)))
	  (if (not (and d2 d3))
	      (error:illegal-string-escape (list->string (list #\\ c1 c2 c3))))
	  (integer->char (fix:+ (fix:lsh (fix:+ (fix:lsh d1 3) d2) 3) d3))))

      (loop))))

(define (handler:false db ctx char1 char2)
  ctx char1
  (let ((string (parse-atom db (list char2))))
    (if (not (or (string=? string "f")
		 (string=? string "false")))
	(error:illegal-boolean string)))
  #f)

(define (handler:true db ctx char1 char2)
  ctx char1
  (let ((string (parse-atom db (list char2))))
    (if (not (or (string=? string "t")
		 (string=? string "true")))
	(error:illegal-boolean string)))
  #t)

(define (handler:bit-string db ctx char1 char2)
  ctx char1 char2
  (let ((string (parse-atom db '())))
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

(define (handler:char db ctx char1 char2)
  ctx char1 char2
  (let ((char (%read-char/no-eof db))
	(at-end?
	 (lambda ()
	   (let ((char (%peek-char db)))
	     (or (eof-object? char)
		 (char-in-set? char char-set/atom-delimiters))))))
    (if (or (char-in-set? char char-set/atom-delimiters)
	    (at-end?))
	char
	(name->char
	 (call-with-output-string
	   (lambda (port*)
	     (write-char char port*)
	     (let loop ()
	       (write-char (let ((char (%read-char/no-eof db)))
			     (if (char=? char #\\)
				 (%read-char/no-eof db)
				 char))
			   port*)
	       (if (not (at-end?))
		   (loop)))))
	 (db-fold-case? db)))))

(define (handler:named-constant db ctx char1 char2)
  ctx char1 char2
  (let ((name (parse-atom db '())))
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

(define (handler:uri db ctx char1 char2)
  ctx char1 char2
  (string->uri
   (call-with-output-string
     (lambda (port*)
       (let loop ()
	 (let ((char (%read-char/no-eof db)))
	   (if (not (char=? char #\>))
	       (begin
		 (write-char char port*)
		 (loop)))))))))

(define (handler:special-arg db ctx char1 char2)
  ctx char1
  (let loop ((n (char->digit char2 10)))
    (let ((char (%read-char/no-eof db)))
      (cond ((char-numeric? char)
	     (loop (+ (* 10 n) (char->digit char 10))))
	    ((char=? char #\=)
	     (let ((object (read-object db)))
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

(define (%read-char db)
  (let ((char
	 (let loop ()
	   (or ((db-read-char db))
	       (loop)))))
    ((db-discretionary-write-char db) char)
    char))

(define (%read-char/no-eof db)
  (let ((char (%read-char db)))
    (if (eof-object? char)
	(error:premature-eof db))
    char))

(define (%peek-char db)
  (let loop ()
    (or ((db-peek-char db))
	(loop))))

(define (%peek-char/no-eof db)
  (let ((char (%peek-char db)))
    (if (eof-object? char)
	(error:premature-eof db))
    char))

(define-record-type <db>
    (make-db port shared-objects position-mapping discretionary-write-char
	     get-position input-line peek-char read-char)
    db?
  (port db-port)
  (shared-objects db-shared-objects)
  (position-mapping db-position-mapping set-db-position-mapping!)
  ;; Cached port operations
  (discretionary-write-char db-discretionary-write-char)
  (get-position db-get-position)
  (input-line db-input-line)
  (peek-char db-peek-char)
  (read-char db-read-char))

(define (initial-db port)
  (make-db port
	   (make-shared-objects)
	   '()
	   (let ((operation (port/operation port 'discretionary-write-char)))
	     (if operation
		 (lambda (char) (operation port char))
		 (lambda (char) char unspecific)))
	   (if (get-param:parser-associate-positions?)
	       (optional-unary-port-operation port 'position #f)
	       (lambda () #f))
	   (optional-unary-port-operation port 'input-line #f)
	   (required-unary-port-operation port 'peek-char)
	   (required-unary-port-operation port 'read-char)))

(define (required-unary-port-operation port operator)
  (let ((operation (port/operation port operator)))
    (lambda ()
      (operation port))))

(define (optional-unary-port-operation port operator default-value)
  (let ((operation (port/operation port operator)))
    (if operation
	(lambda () (operation port))
	(lambda () default-value))))

(define (db-property db name default-value)
  (port-property (db-port db) name default-value))

(define (set-db-property! db name value)
  (set-port-property! (db-port db) name value))

(define (db-fold-case? db)
  (db-property db 'parser-fold-case? (get-param:parser-fold-case?)))

(define (set-db-fold-case! db value)
  (set-db-property! db 'parser-fold-case? value))

(define (db-enable-attributes? db)
  (db-property db 'parser-enable-attributes? (param:parser-enable-attributes?)))

(define (db-keyword-style db)
  (db-property db 'parser-keyword-style (param:parser-keyword-style)))

(define (record-object-position! position object db)
  (if (and position (object-pointer? object))
      (set-db-position-mapping! db
				(cons (cons position object)
				      (db-position-mapping db)))))

(define (finish-parsing object db)
  (if (get-param:parser-associate-positions?)
      (cons object (db-position-mapping db))
      object))

(define (process-file-attributes file-attribute-alist db)
  ;; Disable further attributes parsing.
  (set-db-property! db 'parser-enable-attributes? #f)
  ;; Save all the attributes; this helps with testing.
  (set-db-property! db 'parser-file-attributes file-attribute-alist)
  (process-keyword-attribute file-attribute-alist db)
  (process-mode-attribute file-attribute-alist db)
  (process-studly-case-attribute file-attribute-alist db))

(define (lookup-file-attribute file-attribute-alist attribute)
  (assoc attribute file-attribute-alist
	 (lambda (left right)
	   (string-ci=? (symbol->string left) (symbol->string right)))))

;;; Look for keyword-style: prefix or keyword-style: suffix
(define (process-keyword-attribute file-attribute-alist db)
  (let ((keyword-entry
	 (lookup-file-attribute file-attribute-alist 'KEYWORD-STYLE)))
    (if (pair? keyword-entry)
	(let ((value (cdr keyword-entry)))
	  (cond ((and (symbol? value)
		      (or (string-ci=? (symbol->string value) "none")
			  (string-ci=? (symbol->string value) "false")))
		 (set-db-property! db 'parser-keyword-style #f))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "prefix"))
		 (set-db-property! db 'parser-keyword-style 'prefix))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "suffix"))
		 (set-db-property! db 'parser-keyword-style 'suffix))
		(else
		 (warn "Unrecognized value for keyword-style" value)))))))

;;; Don't do anything with the mode, but warn if it isn't scheme.
(define (process-mode-attribute file-attribute-alist db)
  (declare (ignore db))
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
(define (process-studly-case-attribute file-attribute-alist db)
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
			(set-db-property! db 'parser-fold-case? #f))))
		((or (not value)
		     (and (symbol? value)
			  (string-ci=? (symbol->string value) "false")))
		 (set-db-property! db 'parser-fold-case? #t))
		(else
		 (warn "Unrecognized value for sTuDly-case" value)))))))

(define-deferred condition-type:parse-error
  (make-condition-type 'PARSE-ERROR condition-type:error '()
    (lambda (condition port)
      condition
      (write-string "Anonymous parsing error." port))))

(define-syntax define-parse-error
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '((+ symbol) expression) (cdr form))
	 (let ((name (caadr form))
	       (field-names (cdadr form))
	       (reporter (caddr form)))
	   (let ((ct (symbol 'condition-type: name)))
	     `(begin
		(define-deferred ,ct
		  (make-condition-type ',name condition-type:parse-error
		      ',field-names
		    (lambda (condition port)
		      (,reporter
		       ,@(map (lambda (field-name)
				`(access-condition condition ',field-name))
			      field-names)
		       port))))
		(define-deferred ,(symbol 'error: name)
		  (condition-signaller ,ct
				       ',field-names
				       standard-error-handler)))))
	 (ill-formed-syntax form)))))

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

(define-parse-error (premature-eof db)
  (lambda (db port)
    (write-string "Premature EOF on " port)
    (write (db-port db) port)))

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

(define-parse-error (unexpected-restart db)
  (lambda (db port)
    (write-string "Unexpected parse restart on: " port)
    (write (db-port db) port)))

(define-parse-error (unsupported-vector string)
  (lambda (string port)
    (write-string "Unsupported vector prefix: " port)
    (write-string string port)))