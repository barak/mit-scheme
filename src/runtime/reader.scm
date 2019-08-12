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
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301,
USA.

|#

;;;; Scheme Reader
;;; package: (runtime reader)

(declare (usual-integrations))

(define *parser-associate-positions?* #!default)
(define *parser-canonicalize-symbols?* #!default)
(define *parser-radix* #!default)

(define (boolean-converter value)
  (guarantee boolean? value))

(define-deferred param:reader-associate-positions?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:reader-fold-case?
  (make-settable-parameter #t boolean-converter))

(define-deferred param:reader-enable-attributes?
  (make-unsettable-parameter #t boolean-converter))

(define-deferred param:reader-keyword-style
  (make-unsettable-parameter #f
			     (lambda (value)
			       (if (memq value '(#f prefix suffix))
				   value
				   (error "Invalid keyword style:" value)))))

(define-deferred param:reader-radix
  (make-unsettable-parameter 10
			     (lambda (value)
			       (if (memv value '(2 8 10 16))
				   value
				   (error "Invalid reader radix:" value)))))

(define (get-param:reader-associate-positions?)
  (if (default-object? *parser-associate-positions?*)
      (param:reader-associate-positions?)
      *parser-associate-positions?*))

(define (get-param:reader-fold-case?)
  (if (default-object? *parser-canonicalize-symbols?*)
      (param:reader-fold-case?)
      (and *parser-canonicalize-symbols?* 'symbols-only)))

(define (get-param:reader-radix)
  (if (default-object? *parser-radix*)
      (param:reader-radix)
      *parser-radix*))

(define (read-top-level port)
  (let ((read-operation (textual-port-operation port 'read)))
    (if read-operation
	(read-operation port)
	(begin
	  (let ((read-start (textual-port-operation port 'read-start)))
	    (if read-start
		(read-start port)))
	  (let restart ()
	    (let* ((db (initial-db port))
		   (object (dispatch db (ctx:top-level))))
	      (if (eq? object restart-reading)
		  (restart)
		  (begin
		    (let ((read-finish
			   (textual-port-operation port 'read-finish)))
		      (if read-finish
			  (read-finish port)))
		    (finish-parsing object db)))))))))

(define (dispatch db ctx)
  (let* ((position ((db-get-position db)))
	 (char (%read-char db)))
    (if (eof-object? char)
	char
	(let ((object ((get-initial-handler char) db ctx char)))
	  (cond ((eq? object continue-reading) (dispatch db ctx))
		((eq? object restart-reading) object)
		(else
		 (record-object-position! position object db)
		 object))))))

;; Causes the dispatch to be re-run.
;; Used to discard things like whitespace and comments.
(define continue-reading
  (list 'continue-reading))

;; Causes the dispatch to finish, but the top-level reader will return
;; back into the dispatch after re-initializing the db.  This is used
;; to reset the reader when changing read syntax as specified by the
;; file attributes list.
(define restart-reading
  (list 'restart-reading))

(define (handler:special db ctx char1)
  (let ((char2 (%read-char/no-eof db)))
    ((get-special-handler char2) db ctx char1 char2)))

(define (read-object db)
  (read-in-context db ctx:object))

(define (read-in-context db get-ctx)
  (let ((object (dispatch db (get-ctx))))
    (cond ((eof-object? object)	(error:premature-eof db))
	  ((eq? object restart-reading) (error:unexpected-restart db))
	  (else object))))

(define (ctx:object)
  'object)

(define (ctx:top-level)
  'top-level)

(define (top-level-ctx? ctx)
  (eq? ctx (ctx:top-level)))

(define (ctx:close-paren-ok)
  'close-paren-ok)

(define (close-paren-ok? ctx)
  (eq? ctx (ctx:close-paren-ok)))

(define (close-parenthesis-token)
  %close-parenthesis-token)

(define (close-parenthesis-token? object)
  (eq? object %close-parenthesis-token))

(define %close-parenthesis-token
  (list 'close-parenthesis))

(define (ctx:close-bracket-ok)
  'close-bracket-ok)

(define (close-bracket-ok? ctx)
  (eq? ctx (ctx:close-bracket-ok)))

(define (close-bracket-token)
  %close-bracket-token)

(define (close-bracket-token? object)
  (eq? object %close-bracket-token))

(define %close-bracket-token
  (list 'close-bracket))

;;;; Dispatch tables

(define (make-dispatch-table)
  (let ((low (make-vector #x80 #f))
	(high '()))

    (define (add-handler! key handler)
      (cond ((char? key)
	     (let ((cp (char->integer key)))
	       (if (fix:< cp #x80)
		   (add-low-handler! cp handler)
		   (begin
		     (if (find (lambda (p)
				 (match-char key (car p)))
			       high)
			 (boot-error "Duplicate binding for:" key))
		     (set! high (cons (cons key handler) high))
		     unspecific))))
	    ((char-set? key)
	     (do ((cp 0 (fix:+ cp 1)))
		 ((not (fix:< cp #x80)))
	       (if (code-point-in-char-set? cp key)
		   (add-low-handler! cp handler)))
	     (if (find (lambda (p)
			 (match-char-set key (car p)))
		       high)
		 (boot-error "Overlapping binding for:" key))
	     (set! high (cons (cons key handler) high))
	     unspecific)
	    (else
	     (error "Unsupported dispatch key:" key))))

    (define (get-handler char)
      (let ((handler
	     (let ((cp (char->integer char)))
	       (if (fix:< cp #x80)
		   (vector-ref low cp)
		   (let ((p
			  (find (lambda (p)
				  (match-char char (car p)))
				high)))
		     (and p
			  (cdr p)))))))
	(if (not handler)
	    (error:illegal-char char))
	handler))

    (define (add-low-handler! cp handler)
      (if (vector-ref low cp)
	  (boot-error "Duplicate binding for:" (integer->char cp)))
      (vector-set! low cp handler))

    (define (match-char char key)
      (if (char? key)
	  (char=? char key)
	  (char-in-set? char key)))

    (define (match-char-set char-set key)
      (if (char? key)
	  (char-in-set? key char-set)
	  (not (char-sets-disjoint? key char-set))))

    (define (boot-error msg key)
      ((ucode-primitive debugging-printer) msg)
      ((ucode-primitive debugging-printer) key))

    (lambda (operator)
      (case operator
	((add-handler!) add-handler!)
	((get-handler) get-handler)
	(else (error "Unsupported operation:" operator))))))

(define initial-dispatch-table)
(define get-initial-handler)
(define special-dispatch-table)
(define get-special-handler)
(add-boot-init!
 (lambda ()

   (set! initial-dispatch-table (make-dispatch-table))
   (set! get-initial-handler (initial-dispatch-table 'get-handler))
   (define add-initial! (initial-dispatch-table 'add-handler!))

   (add-initial! #\" handler:string)
   (add-initial! #\# handler:special)
   (add-initial! #\' handler:quote)
   (add-initial! #\( handler:list)
   (add-initial! #\) handler:close-parenthesis)
   (add-initial! #\+ handler:atom)
   (add-initial! #\, handler:unquote)
   (add-initial! #\- handler:atom)
   (add-initial! #\. handler:atom)
   (add-initial! #\; handler:comment)
   (add-initial! #\] handler:close-bracket)
   (add-initial! #\` handler:quasiquote)
   (add-initial! #\| handler:quoted-symbol)
   (add-initial! char-set:whitespace handler:whitespace)
   (add-initial! char-set:numeric handler:atom)
   (add-initial! (char-set-difference char-set:symbol-initial (char-set "+-."))
		 handler:symbol)

   (set! special-dispatch-table (make-dispatch-table))
   (set! get-special-handler (special-dispatch-table 'get-handler))
   (define add-special! (special-dispatch-table 'add-handler!))

   (add-special! #\( handler:vector)
   (add-special! #\< handler:uri)
   (add-special! #\[ handler:hashed-object)
   (add-special! #\| handler:multi-line-comment)
   (add-special! #\; handler:expression-comment)
   (add-special! #\f handler:false)
   (add-special! #\F handler:false)
   (add-special! #\t handler:true)
   (add-special! #\T handler:true)
   (add-special! #\u handler:unsigned-vector)
   (add-special! #\* handler:bit-string)
   (add-special! #\\ handler:char)
   (add-special! #\! handler:named-constant)
   (add-special! #\@ handler:unhash)
   (add-special! (char-set "bBoOdDxXiIeEsSlL") handler:number)
   (add-special! char-set:numeric handler:special-arg)))

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

(define-deferred atom-delimiters
  (char-set char-set:whitespace
	    ;; Note that #\, may break older code.
	    "()[]{}\";'`,"
	    (integer->char #xAB)
	    (integer->char #xBB)))

(define-deferred atom-delimiter?
  (char-set-predicate atom-delimiters))

(define (make-symbol db string)
  (if (db-fold-case? db)
      (intern string)
      (string->symbol string)))

(define (string-maybe-ci=? db s1 s2)
  (if (eq? #t (db-fold-case? db))
      (string-ci=? s1 s2)
      (string=? s1 s2)))

(define (handler:whitespace db ctx char)
  db ctx char
  continue-reading)

;; It would be better if we could skip over the object without
;; creating it, but for now this will work.
(define (handler:expression-comment db ctx char1 char2)
  ctx char1 char2
  (read-object db)
  continue-reading)

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
	  restart-reading)
	continue-reading)))

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

(define (handler:atom db ctx char)
  ctx
  (let ((string (read-atom db (list char))))
    (or (maybe-keyword db string)
	(string->number string (get-param:reader-radix))
	(make-symbol db string))))

(define (handler:symbol db ctx char)
  ctx
  (let ((string (read-atom db (list char))))
    (or (maybe-keyword db string)
	(if (string=? string "nan.0")
	    (flo:nan.0)
	    (make-symbol db string)))))

(define (maybe-keyword db string)
  (cond ((and (eq? 'suffix (db-keyword-style db))
	      (string-suffix? ":" string)
	      (fix:> (string-length string) 1))
	 (string->keyword (string-slice string
					0
					(fix:- (string-length string) 1))
			  (db-fold-case? db)))
	((and (eq? 'prefix (db-keyword-style db))
	      (string-prefix? ":" string)
	      (fix:> (string-length string) 1))
	 (string->keyword (string-slice string 1) (db-fold-case? db)))
	(else #f)))

(define (handler:number db ctx char1 char2)
  ctx
  (read-number db (list char1 char2)))

(define (read-number db prefix)
  (let ((string (read-atom db prefix)))
    (or (string->number string (get-param:reader-radix))
	(error:illegal-number string))))

(define (read-atom db prefix)
  (let ((builder (string-builder)))
    (for-each builder prefix)
    (let loop ()
      (if (not (%atom-end? db))
	  (begin
	    (builder (%read-char db))
	    (loop))))
    (builder)))

(define (%atom-end? db)
  (let ((char (%peek-char db)))
    (or (eof-object? char)
	(atom-delimiter? char))))

(define (handler:list db ctx char)
  ctx char
  (let loop ((objects '()))
    (let ((object (read-in-context db ctx:close-paren-ok)))
      (if (close-parenthesis-token? object)
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
    (let ((object (read-in-context db ctx:close-paren-ok)))
      (if (close-parenthesis-token? object)
	  (list->vector (reverse! objects))
	  (loop (cons object objects))))))

(define (handler:unsigned-vector db ctx char1 char2)
  ctx
  (let ((atom (read-atom db '())))
    (if (not (and atom (string=? atom "8")))
	(error:unsupported-vector (string char1 char2 (or atom "")))))
  (let ((char (%read-char/no-eof db)))
    (if (not (char=? char #\())
	(error:illegal-char char)))
  (let loop ((bytes '()))
    (let ((object (read-in-context db ctx:close-paren-ok)))
      (if (close-parenthesis-token? object)
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
  (if (and ignore-extra-list-closes
	   (top-level-ctx? ctx)
	   (console-i/o-port? (db-port db)))
      continue-reading
      (begin
	(if (not (close-paren-ok? ctx))
	    (error:unbalanced-close char))
	(close-parenthesis-token))))

(define ignore-extra-list-closes #t)

(define (handler:hashed-object db ctx char1 char2)
  ctx char1 char2
  (let loop ((objects '()))
    (let ((object (read-in-context db ctx:close-bracket-ok)))
      (if (close-bracket-token? object)
	  (let* ((objects (reverse! objects))
		 (lose (lambda () (error:illegal-hashed-object objects)))
		 (default-method
		   (lambda (objects lose)
		     (if (pair? (cdr objects))
			 (read-unhash (cadr objects))
			 (lose))))
		 (method
		  (and (pair? objects)
		       (interned-symbol? (car objects))
		       (hash-table-ref/default hashed-object-interns
					       (car objects)
					       default-method))))
	    (if method
		(bind-condition-handler (list condition-type:error)
		    (lambda (condition) condition (lose))
		  (lambda ()
		    (method objects lose)))
		(lose)))
	  (loop (cons object objects))))))

(define (handler:close-bracket db ctx char)
  db
  (if (not (close-bracket-ok? ctx))
      (error:unbalanced-close char))
  (close-bracket-token))

(define (define-bracketed-reader-method name method)
  (guarantee interned-symbol? name 'define-bracketed-reader-method)
  (guarantee binary-procedure? method 'define-bracketed-reader-method)
  (hash-table-set! hashed-object-interns name method))

(define-deferred hashed-object-interns
  (make-strong-eq-hash-table))

(define (handler:unhash db ctx char1 char2)
  ctx char1 char2
  (let ((object (read-unhash (read-number db '()))))
    ;; This may seem a little random, because #@N doesn't just
    ;; return an object.  However, the motivation for this piece of
    ;; syntax is convenience -- and 99.99% of the time the result of
    ;; this syntax will be evaluated, and the user will expect the
    ;; result of the evaluation to be the object she was referring
    ;; to.  If the quotation isn't there, the user just gets
    ;; confused.
    (make-scode-quotation object)))

(define (read-unhash object)
  (if (not (exact-nonnegative-integer? object))
      (error:illegal-unhash object))
  (if (eq? object 0)
      #f
      (or (unhash-object object)
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
  (read-delimited-string db #\" #t))

(define (handler:quoted-symbol db ctx char)
  ctx char
  (string->symbol (read-delimited-string db #\| #f)))

(define (read-delimited-string db delimiter allow-newline-escape?)
  (let ((builder (string-builder)))

    (define (loop)
      (dispatch (%read-char/no-eof db)))

    (define (dispatch char)
      (cond ((char=? delimiter char) unspecific)
	    ((char=? #\\ char) (read-quoted))
	    (else (emit char))))

    (define (read-quoted)
      (let ((char (%read-char/no-eof db)))
	(cond ((char=? char #\a) (emit #\bel))
	      ((char=? char #\b) (emit #\bs))
	      ((char=? char #\n) (emit #\newline))
	      ((char=? char #\r) (emit #\return))
	      ((char=? char #\t) (emit #\tab))
	      ((char=? char #\x) (emit (read-hex-escape 0 '())))
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
	      ((char->digit char 4)
	       => (lambda (d) (emit (read-octal-escape char d))))
	      (else (emit char)))))

    (define (emit char)
      (builder char)
      (loop))

    (define (skip-space)
      (let ((char (%read-char/no-eof db)))
	(if (or (char=? char #\space)
		(char=? char #\tab))
	    (skip-space)
	    char)))

    (define (read-hex-escape sv chars)
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
	      (read-hex-escape (+ (* sv #x10) digit) chars)))))

    (define (ill-formed-hex chars)
      (error:illegal-string-escape
       (list->string (cons* #\\ #\x (reverse chars)))))

    (define (read-octal-escape c1 d1)
      (let* ((c2 (%read-char/no-eof db))
	     (d2 (char->digit c2 8))
	     (c3 (%read-char/no-eof db))
	     (d3 (char->digit c3 8)))
	(if (not (and d2 d3))
	    (error:illegal-string-escape (list->string (list #\\ c1 c2 c3))))
	(integer->char (fix:+ (fix:lsh (fix:+ (fix:lsh d1 3) d2) 3) d3))))

    (loop)
    (builder)))

(define (handler:false db ctx char1 char2)
  ctx char1
  (let ((string (read-atom db (list char2))))
    (if (not (or (string-maybe-ci=? db string "f")
		 (string-maybe-ci=? db string "false")))
	(error:illegal-boolean string)))
  #f)

(define (handler:true db ctx char1 char2)
  ctx char1
  (let ((string (read-atom db (list char2))))
    (if (not (or (string-maybe-ci=? db string "t")
		 (string-maybe-ci=? db string "true")))
	(error:illegal-boolean string)))
  #t)

(define (handler:bit-string db ctx char1 char2)
  ctx char1 char2
  (let ((string (read-atom db '())))
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
  (let ((char (%read-char/no-eof db)))
    (cond ((or (atom-delimiter? char)
	       (%atom-end? db))
	   char)
	  ((char=? char #\x)
	   (let* ((string (read-atom db '()))
		  (cp (string->number string 16 #t)))
	     (if (not (unicode-code-point? cp))
		 (error:illegal-code-point string))
	     (integer->char cp)))
	  (else
	   (let ((builder (string-builder)))
	     (builder char)
	     (let loop ()
	       (if (not (%atom-end? db))
		   (begin
		     (builder
		      (let ((char (%read-char db)))
			(if (char=? #\\ char)
			    (%read-char/no-eof db)
			    char)))
		     (loop))))
	     (name->char (builder)
			 (eq? #t (db-fold-case? db))))))))

(define (handler:named-constant db ctx char1 char2)
  ctx char1 char2
  (let ((name (read-atom db '())))
    (cond ((string-maybe-ci=? db name "null") '())
	  ((string-maybe-ci=? db name "false") #f)
	  ((string-maybe-ci=? db name "true") #t)
	  ((string-maybe-ci=? db name "optional") lambda-tag:optional)
	  ((string-maybe-ci=? db name "rest") lambda-tag:rest)
	  ((string-maybe-ci=? db name "key") lambda-tag:key)
	  ((string-maybe-ci=? db name "aux") lambda-tag:aux)
	  ((string-maybe-ci=? db name "eof") (eof-object))
	  ((string-maybe-ci=? db name "default") (default-object))
	  ((string-maybe-ci=? db name "unspecific") unspecific)
	  ((string=? name "fold-case")
	   (set-db-fold-case! db #t)
	   continue-reading)
	  ((string=? name "no-fold-case")
	   (set-db-fold-case! db #f)
	   continue-reading)
	  (else
	   (error:illegal-named-constant name)))))

(define (handler:uri db ctx char1 char2)
  ctx char1 char2
  (string->uri
   (let ((builder (string-builder)))
     (let loop ()
       (let ((char (%read-char/no-eof db)))
	 (if (not (char=? char #\>))
	     (begin
	       (builder char)
	       (loop)))))
     (builder))))

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
    (if (not (eq? (hash-table-ref/default table n non-shared-object)
		  non-shared-object))
	(error:re-shared-object n object))
    (hash-table-set! table n object)))

(define (get-shared-object db n)
  (let ((object
	 (hash-table-ref/default (db-shared-objects db) n non-shared-object)))
    (if (eq? object non-shared-object)
	(error:non-shared-object n))
    object))

(define non-shared-object
  (list 'non-shared-object))

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
	   (let ((operation
		  (textual-port-operation port 'discretionary-write-char)))
	     (if operation
		 (lambda (char) (operation port char))
		 (lambda (char) char unspecific)))
	   (if (get-param:reader-associate-positions?)
	       (optional-unary-port-operation port 'position #f)
	       (lambda () #f))
	   (optional-unary-port-operation port 'input-line #f)
	   (required-unary-port-operation port 'peek-char)
	   (required-unary-port-operation port 'read-char)))

(define (required-unary-port-operation port operator)
  (let ((operation (textual-port-operation port operator)))
    (lambda ()
      (operation port))))

(define (optional-unary-port-operation port operator default-value)
  (let ((operation (textual-port-operation port operator)))
    (if operation
	(lambda () (operation port))
	(lambda () default-value))))

(define (db-property db name default-value)
  (port-property (db-port db) name default-value))

(define (set-db-property! db name value)
  (set-port-property! (db-port db) name value))

(define (db-fold-case? db)
  (db-property db 'reader-fold-case? (get-param:reader-fold-case?)))

(define (set-db-fold-case! db value)
  (set-db-property! db 'reader-fold-case? value))

(define (db-enable-attributes? db)
  (db-property db 'reader-enable-attributes? (param:reader-enable-attributes?)))

(define (db-keyword-style db)
  (db-property db 'reader-keyword-style (param:reader-keyword-style)))

(define (record-object-position! position object db)
  (if (and position (object-pointer? object))
      (set-db-position-mapping! db
				(cons (cons position object)
				      (db-position-mapping db)))))

(define (finish-parsing object db)
  (if (get-param:reader-associate-positions?)
      (cons object (db-position-mapping db))
      object))

(define (process-file-attributes file-attribute-alist db)
  ;; Disable further attributes parsing.
  (set-db-property! db 'reader-enable-attributes? #f)
  ;; Save all the attributes; this helps with testing.
  (set-db-property! db 'reader-file-attributes file-attribute-alist)
  (process-coding-attribute file-attribute-alist db)
  (process-keyword-attribute file-attribute-alist db)
  (process-mode-attribute file-attribute-alist db)
  (process-studly-case-attribute file-attribute-alist db))

(define (lookup-file-attribute file-attribute-alist attribute)
  (assoc attribute file-attribute-alist
	 (lambda (left right)
	   (string-ci=? (symbol->string left) (symbol->string right)))))

;;; Allow file to specify its character coding.
(define (process-coding-attribute file-attribute-alist db)
  (let ((entry (lookup-file-attribute file-attribute-alist 'coding)))
    (if (pair? entry)
        (let ((coding (cdr entry))
              (port (db-port db)))
          (if (and (symbol? coding) (known-input-port-coding? coding))
              (if (and (port/supports-coding? port)
                       (port/known-coding? port coding))
                  (port/set-coding port coding))
              (warn "Unrecognized value for coding:" coding))))))

;;; Look for keyword-style: prefix or keyword-style: suffix
(define (process-keyword-attribute file-attribute-alist db)
  (let ((keyword-entry
	 (lookup-file-attribute file-attribute-alist 'keyword-style)))
    (if (pair? keyword-entry)
	(let ((value (cdr keyword-entry)))
	  (cond ((and (symbol? value)
		      (or (string-ci=? (symbol->string value) "none")
			  (string-ci=? (symbol->string value) "false")))
		 (set-db-property! db 'reader-keyword-style #f))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "prefix"))
		 (set-db-property! db 'reader-keyword-style 'prefix))
		((and (symbol? value)
		      (string-ci=? (symbol->string value) "suffix"))
		 (set-db-property! db 'reader-keyword-style 'suffix))
		(else
		 (warn "Unrecognized value for keyword-style" value)))))))

;;; Don't do anything with the mode, but warn if it isn't scheme.
(define (process-mode-attribute file-attribute-alist db)
  (declare (ignore db))
  (let ((mode-entry
	 (lookup-file-attribute file-attribute-alist 'mode)))
    (if (pair? mode-entry)
	(let ((value (cdr mode-entry)))
	  (if (or (not (symbol? value))
		  (not (string-ci=? (symbol->string value) "scheme")))
	      (warn "Unexpected file mode:"
		    (if (symbol? value)
			(symbol->string value)
			value)))))))

;; If you want to turn on studly case, then the attribute must be
;; exactly "sTuDly-case" and the value must be exactly "True".  After
;; all, case is important.  If you want to turn it off, the case of
;; the attribute and the value don't matter.
(define (process-studly-case-attribute file-attribute-alist db)
  (let ((studly-case-entry
	 (lookup-file-attribute file-attribute-alist 'studly-case)))
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
			(set-db-property! db 'reader-fold-case? #f))))
		((or (not value)
		     (and (symbol? value)
			  (string-ci=? (symbol->string value) "false")))
		 (set-db-property! db 'reader-fold-case? #t))
		(else
		 (warn "Unrecognized value for sTuDly-case" value)))))))

(define-deferred condition-type:read-error
  (make-condition-type 'read-error condition-type:error '()
    (lambda (condition port)
      condition
      (write-string "Anonymous reading error." port))))

(define-deferred read-error?
  (condition-predicate condition-type:read-error))

(define-syntax define-read-error
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
		  (make-condition-type ',name condition-type:read-error
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

(define-read-error (illegal-bit-string string)
  (lambda (string port)
    (write-string "Ill-formed bit string: #*" port)
    (write-string string port)))

(define-read-error (illegal-boolean string)
  (lambda (string port)
    (write-string "Ill-formed boolean: " port)
    (write-string string port)))

(define-read-error (illegal-char char)
  (lambda (char port)
    (write-string "Illegal character: " port)
    (write char port)))

(define-read-error (illegal-dot-usage objects)
  (lambda (objects port)
    (write-string "Ill-formed dotted list: " port)
    (write objects port)))

(define-read-error (illegal-hashed-object objects)
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

(define-read-error (illegal-code-point string)
  (lambda (string port)
    (write-string "Ill-formed code point: " port)
    (write string port)))

(define-read-error (illegal-named-constant name)
  (lambda (name port)
    (write-string "Ill-formed named constant: #!" port)
    (write name port)))

(define-read-error (illegal-string-escape string)
  (lambda (string port)
    (write-string "Ill-formed string escape: " port)
    (write-string string port)))

(define-read-error (illegal-number string)
  (lambda (string port)
    (write-string "Ill-formed number: " port)
    (write-string string port)))

(define-read-error (illegal-unhash object)
  (lambda (object port)
    (write-string "Ill-formed unhash syntax: #@" port)
    (write object port)))

(define-read-error (undefined-hash object)
  (lambda (object port)
    (write-string "Undefined hash number: #@" port)
    (write object port)))

(define-read-error (no-quoting-allowed string)
  (lambda (string port)
    (write-string "Quoting not permitted: " port)
    (write-string string port)))

(define-read-error (premature-eof db)
  (lambda (db port)
    (write-string "Premature EOF on " port)
    (write (db-port db) port)))

(define-read-error (re-shared-object n object)
  (lambda (n object port)
    (write-string "Can't re-share object: #" port)
    (write n port)
    (write-string "=" port)
    (write object port)))

(define-read-error (non-shared-object n)
  (lambda (n port)
    (write-string "Reference to non-shared object: #" port)
    (write n port)
    (write-string "#" port)))

(define-read-error (unbalanced-close char)
  (lambda (char port)
    (write-string "Unbalanced close parenthesis: " port)
    (write char port)))

(define-read-error (unexpected-restart db)
  (lambda (db port)
    (write-string "Unexpected read restart on: " port)
    (write (db-port db) port)))

(define-read-error (unsupported-vector string)
  (lambda (string port)
    (write-string "Unsupported vector prefix: " port)
    (write-string string port)))