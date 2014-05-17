#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014 Massachusetts
    Institute of Technology

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

(declare (usual-integrations)
	 (integrate-external "input")
	 (integrate-external "port"))

(define *parser-associate-positions?* #f)
(define *parser-atom-delimiters*)
(define *parser-canonicalize-symbols?* #t)
(define *parser-constituents*)
(define *parser-enable-file-attributes-parsing?* #t)
(define *parser-keyword-style* #f)
(define *parser-radix* 10)
(define *parser-table*)

(define runtime-parser-associate-positions? #f)
(define runtime-parser-atom-delimiters)
(define runtime-parser-canonicalize-symbols? #t)
(define runtime-parser-constituents)
(define runtime-parser-enable-file-attributes-parsing? #t)
(define runtime-parser-keyword-style #f)
(define runtime-parser-radix 10)
(define runtime-parser-table)

(define ignore-extra-list-closes #t)

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
  (let ((handlers (parser-table/initial (db-parser-table db))))
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
    ((get-handler char2 (parser-table/special (db-parser-table db)))
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
  (let* ((constituents
	  (char-set-difference char-set:graphic
			       char-set:whitespace))
	 (atom-delimiters
	  (char-set-union char-set:whitespace
			  ;; Note that #\, may break older code.
			  (string->char-set "()[]{}\";'`,")
			  (char-set #\U+00AB #\U+00BB)))
	 (symbol-quotes
	  (string->char-set "\\|"))
	 (number-leaders
	  (char-set-union char-set:numeric
			  (string->char-set "+-.")))
	 (symbol-leaders
	  (char-set-difference constituents
			       (char-set-union atom-delimiters
					       number-leaders)))
	 (special-number-leaders
	  (string->char-set "bBoOdDxXiIeEsSlL"))
	 (store-char (lambda (v c h) (vector-set! v (char->integer c) h)))
	 (store-char-set
	  (lambda (v c h)
	    (for-each (lambda (c) (store-char v c h))
		      (char-set-members c)))))
    (let ((initial (make-vector #x100 #f))
	  (special (make-vector #x100 #f)))
      (store-char-set initial char-set:whitespace handler:whitespace)
      (store-char-set initial number-leaders handler:atom)
      (store-char-set initial symbol-leaders handler:symbol)
      (store-char-set special special-number-leaders handler:number)
      (store-char initial #\( handler:list)
      (store-char special #\( handler:vector)
      (store-char special #\[ handler:hashed-object)
      (store-char initial #\) handler:close-parenthesis)
      (store-char initial #\] handler:close-bracket)
      (store-char initial #\: handler:prefix-keyword)
      (store-char initial #\; handler:comment)
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
      (store-char special #\* handler:bit-string)
      (store-char special #\\ handler:char)
      (store-char special #\! handler:named-constant)
      (store-char special #\@ handler:unhash)
      (store-char-set special char-set:numeric handler:special-arg)
      (set! system-global-parser-table (make-parser-table initial special)))
    (set! char-set/constituents constituents)
    (set! char-set/atom-delimiters atom-delimiters)
    (set! char-set/symbol-quotes symbol-quotes)
    (set! char-set/number-leaders number-leaders)
    (set! *parser-atom-delimiters* atom-delimiters)
    (set! *parser-constituents* constituents)
    (set! runtime-parser-atom-delimiters atom-delimiters)
    (set! runtime-parser-constituents constituents))
  (set! *parser-table* system-global-parser-table)
  (set! runtime-parser-table system-global-parser-table)
  (set! hashed-object-interns (make-strong-eq-hash-table))
  (initialize-condition-types!))


(define (handler:whitespace port db ctx char)
  port db ctx char
  continue-parsing)

(define (handler:comment port db ctx char)
  (declare (ignore ctx char))

  ;; This is a small state machine that looks for  -*-
  ;; The scan state is when it hasn't found anything.
  ;; The dash state is after a - has been seen.
  ;; The discard state is after the file-attributes-line has
  ;; been parsed.
  (define (scan)
    (let ((char (%read-char port db)))
      (if (eof-object? char)
	  char
	  (case char
	    ((#\newline) continue-parsing)
	    ((#\-) (dash))
	    (else (scan))))))

  (define (dash)
    (let ((char (%read-char port db)))
      (if (eof-object? char)
	  char
	  (case char
	    ((#\newline) continue-parsing)
	    ((#\*)
	     (let ((char (%read-char port db)))
	       (if (eof-object? char)
		   char
		   (case char
		     ((#\newline) continue-parsing)
		     ((#\-)
		      (process-file-attributes
		       (parse-file-attributes-line port db false)
		       port)
		      (discard restart-parsing))
		     (else (scan))))))
	    ((#\-) (dash))
	    (else (scan))))))

  (define (discard action)
    (let ((char (%read-char port db)))
      (cond ((eof-object? char) char)
	    ((char=? char #\newline) action)
	    (else (discard action)))))

  ;; If we're past the second line, just discard.
  (if (and (let ((line (current-line port db)))
	     (and line
		  (< line 2)))
	   (db-enable-file-attributes-parsing db))
      (scan)
      (discard continue-parsing)))

(define (handler:multi-line-comment port db ctx char1 char2)
  (declare (ignore ctx char1 char2))
  ;; In addition to parsing out the multi-line-comment, we want to
  ;; extract out the file attribute line if it exists in the first
  ;; line.  To do this, we use a small state machine implemented as a
  ;; bunch of internal functions.  Each state function takes a
  ;; character from the port as an input and finishes by tail-calling
  ;; the next state with the next character.

  ;; These first five states are where we scan the
  ;; first line looking for the file attribute marker, end of comment,
  ;; nested comment, or end of line.

  (define (scan)
    (case (%read-char/no-eof port db)
      ((#\newline) (discard 0 continue-parsing))
      ((#\#) (sharp))
      ((#\-) (dash))
      ((#\|) (vbar))
      (else (scan))))

  (define (sharp)
    (case (%read-char/no-eof port db)
      ((#\newline) (discard 0 continue-parsing))
      ((#\#) (sharp))
      ((#\-) (dash))
      ((#\|) (discard 1 continue-parsing))		; nested comment
      (else (scan))))

  (define (vbar)
    (case (%read-char/no-eof port db)
      ((#\newline) (discard 0 continue-parsing))
      ((#\#) continue-parsing)		; end of comment
      ((#\-) (dash))
      ((#\|) (vbar))
      (else (scan))))

  (define (dash)
    (case (%read-char/no-eof port db)
      ((#\newline) (discard 0 continue-parsing))
      ((#\#) (sharp))
      ((#\*) (dash-star))
      ((#\-) (dash))
      ((#\|) (vbar))
      (else (scan))))

  (define (dash-star)
    (case (%read-char/no-eof port db)
      ((#\newline) (discard 0 continue-parsing))
      ((#\#) (sharp))
      ((#\-)
       (process-file-attributes (parse-file-attributes-line port db true) port)
       (discard 0 restart-parsing))
      ((#\|) (vbar))
      (else (scan))))

  ;; Next three states are the discard loop where we
  ;; just track the nesting level and discard stuff.
  ;; We don't look for the file-attribute marker.

  (define (discard depth action)
    (case (%read-char/no-eof port db)
      ((#\#) (discard-sharp depth action))
      ((#\|) (discard-vbar depth action))
      (else (discard depth action))))

  (define (discard-sharp depth action)
    (case (%read-char/no-eof port db)
      ((#\#) (discard-sharp depth action))
      ((#\|) (discard (+ depth 1) action)) ; push
      (else (discard depth action))))

  (define (discard-vbar depth action)
    (case (%read-char/no-eof port db)
      ((#\#) (if (> depth 0)
		 (discard (- depth 1) action) ; pop
		 action))
      ((#\|) (discard-vbar depth action))
      (else (discard depth action))))

  ;; Start the machine.
  ;; If we're past the second line, just discard.
  (if (and (let ((line (current-line port db)))
	     (and line
		  (< line 2)))
	   (db-enable-file-attributes-parsing db))
      (scan)
      (discard 0 continue-parsing)))


;; It would be better if we could skip over the object without
;; creating it, but for now this will work.
(define (handler:expression-comment port db ctx char1 char2)
  ctx char1 char2
  (read-object port db)
  continue-parsing)

(define (handler:atom port db ctx char)
  ctx
  (receive (string quoted? final) (parse-atom port db (list char))
    (cond ((and (eq? final #\:)
		(eq? (db-keyword-style db) 'SUFFIX))
	   (string->keyword (string-head string (- (string-length string) 1))))
	  (quoted? (string->symbol string))
	  (else (or (string->number string (db-radix db))
		    (string->symbol string))))))

(define (handler:symbol port db ctx char)
  ctx
  (receive (string quoted? final) (parse-atom port db (list char))
    (if (and (eq? final #\:)
	     (eq? (db-keyword-style db) 'SUFFIX)
	     ;; Nasty edge case:  A bare colon.  Treat as a symbol
	     ;; unless quoted.
	     (or (not (= (string-length string) 1))
		 quoted?))
	(string->keyword (string-head string (- (string-length string) 1)))
	(string->symbol string))))

(define (handler:prefix-keyword port db ctx char)
  (if (eq? (db-keyword-style db) 'PREFIX)
      (receive (string quoted? final) (parse-atom port db '())
	(declare (ignore final))
	(if (and (zero? (string-length string))
		 (not quoted?))
	    ;; Nasty edge case:  A bare colon.  Treat as a symbol
	    ;; unless quoted.
	    (string->symbol ":")
	    (string->keyword string)))
      ;; If prefix-style keywords are not in use, just
      ;; tail call the symbol handler.
      (handler:symbol port db ctx char)))

(define (handler:number port db ctx char1 char2)
  ctx
  (parse-number port db (list char1 char2)))

(define (parse-number port db prefix)
  (let ((string (parse-atom/no-quoting port db prefix)))
    (or (string->number string (db-radix db))
	(error:illegal-number string))))

(define (parse-atom port db prefix)
  (parse-atom-1 port db prefix #t))

(define (parse-atom/no-quoting port db prefix)
  (parse-atom-1 port db prefix #f))

(define (parse-atom-1 port db prefix quoting?)
  (let ((port* (open-output-string))
	(table
	 (if (db-canonicalize-symbols? db)
	     downcase-table
	     identity-table))
	(atom-delimiters (db-atom-delimiters db))
	(constituents (db-constituents db)))
    (define (%canon char)
      ;; Assumption: No character involved in I/O has bucky bits, and
      ;; case conversion applies only to ISO-8859-1 characters.
      (let ((integer (char->integer char)))
	(if (fix:< integer #x100)
	    (integer->char (vector-8b-ref table integer))
	    char)))
    (define (%read)
      (if (pair? prefix)
	  (let ((char (car prefix)))
	    (set! prefix (cdr prefix))
	    char)
	  (%read-char/no-eof port db)))
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

    ;; main loop
    ;; the quoted? flag indicates if we've ever
    ;; quoted anything in the atom (to disqualify it
    ;; from being a number).
    ;; the previous-char is used to detect trailing colons
    ;; for srfi-88 style keywords.
    (let read-unquoted ((quoted? #f)
			(previous-char #f)
			(char (%peek)))
      (if (or (eof-object? char)
	      (char-set-member? atom-delimiters char))
	  (if quoting?
	      (values (get-output-string port*) quoted? previous-char)
	      (get-output-string port*))
	  (begin
	    (if (not (char-set-member? constituents char))
		(error:illegal-char char))
	    (%discard)
	    (cond ((char=? char #\|)
		   (if quoting?
		       (let read-quoted ()
			 (let ((char (%read)))
			   (if (char=? char #\|)
			       (read-unquoted #t char (%peek))
			       (begin
				 (%write-char (if (char=? char #\\)
						  (%read)
						  char)
					      port*)
				 (read-quoted)))))
		       (error:illegal-char char)))
		  ((char=? char #\\)
		   (if quoting?
		       (begin
			 (%write-char (%read) port*)
			 ;; Forget previous char so
			 ;; that quoting a final colon will
			 ;; suppress it from being a keyword.
			 (read-unquoted #t #f (%peek)))
		       (error:illegal-char char)))
		  (else
		   (%write-char (%canon char) port*)
		   (read-unquoted quoted? char (%peek)))))))))

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
  (guarantee-interned-symbol name 'DEFINE-BRACKETED-OBJECT-PARSER-METHOD)
  (guarantee-procedure-of-arity method 2
				'DEFINE-BRACKETED-OBJECT-PARSER-METHOD)
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
  (list 'QUOTE (read-object port db)))

(define (handler:quasiquote port db ctx char)
  ctx char
  (list 'QUASIQUOTE (read-object port db)))

(define (handler:unquote port db ctx char)
  ctx char
  (if (char=? (%peek-char/no-eof port db) #\@)
      (begin
	(%read-char port db)
	(list 'UNQUOTE-SPLICING (read-object port db)))
      (list 'UNQUOTE (read-object port db))))

(define (handler:string port db ctx char)
  ctx char
  (call-with-output-string
    (lambda (port*)
      (let loop ()
	(let ((char (%read-char/no-eof port db)))
	  (case char
	    ((#\")
	     unspecific)
	    ((#\\)
	     (let ((char
		    (let ((char (%read-char/no-eof port db)))
		      (cond ((char-ci=? char #\n) #\newline)
			    ((char-ci=? char #\t) #\tab)
			    ((char-ci=? char #\v) #\vt)
			    ((char-ci=? char #\b) #\bs)
			    ((char-ci=? char #\r) #\return)
			    ((char-ci=? char #\f) #\page)
			    ((char-ci=? char #\a) #\bel)
			    ((char->digit char 8) (octal->char char port db))
			    (else char)))))
	       (%write-char char port*)
	       (loop)))
	    (else
	     (%write-char char port*)
	     (loop))))))))

(define (octal->char c1 port db)
  (let ((d1 (char->digit c1 8)))
    (if (or (not d1) (fix:> d1 3))
	(error:illegal-char c1))
    (let* ((c2 (%read-char/no-eof port db))
	   (d2 (char->digit c2 8)))
      (if (not d2)
	  (error:illegal-char c2))
      (let* ((c3 (%read-char/no-eof port db))
	     (d3 (char->digit c3 8)))
	(if (not d3)
	    (error:illegal-char c3))
	(integer->char (fix:+ (fix:lsh (fix:+ (fix:lsh d1 3) d2) 3) d3))))))

(define (handler:false port db ctx char1 char2)
  ctx
  (let ((string (parse-atom/no-quoting port db (list char1 char2))))
    (if (not (string-ci=? string "#f"))
	(error:illegal-boolean string)))
  #f)

(define (handler:true port db ctx char1 char2)
  ctx
  (let ((string (parse-atom/no-quoting port db (list char1 char2))))
    (if (not (string-ci=? string "#t"))
	(error:illegal-boolean string)))
  #t)

(define (handler:bit-string port db ctx char1 char2)
  ctx char1 char2
  (let ((string (parse-atom/no-quoting port db '())))
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
		 (char-set-member? (db-atom-delimiters db) char))))))
    (if (or (char-set-member? (db-atom-delimiters db) char)
	    (at-end?))
	char
	(name->char
	 (call-with-output-string
	   (lambda (port*)
	     (%write-char char port*)
	     (let loop ()
	       (%write-char (let ((char (%read-char/no-eof port db)))
			     (if (char=? char #\\)
				 (%read-char/no-eof port db)
				 char))
			   port*)
	       (if (not (at-end?))
		   (loop)))))))))

(define (handler:named-constant port db ctx char1 char2)
  ctx char1 char2
  (let ((name (parse-atom/no-quoting port db '())))
    (cond ((string-ci=? name "null") '())
	  ((string-ci=? name "false") #f)
	  ((string-ci=? name "true") #t)
	  ((string-ci=? name "optional") lambda-tag:optional)
	  ((string-ci=? name "rest") lambda-tag:rest)
	  ((string-ci=? name "key") lambda-tag:key)
	  ((string-ci=? name "aux") lambda-tag:aux)
	  ((string-ci=? name "eof") (eof-object))
	  ((string-ci=? name "default") (default-object))
	  ((string-ci=? name "unspecific") unspecific)
	  (else (error:illegal-named-constant name)))))

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

(define-structure db
  (associate-positions? #f read-only #t)
  (atom-delimiters #f read-only #t)
  (canonicalize-symbols? #f read-only #t)
  (constituents #f read-only #t)
  (enable-file-attributes-parsing #f read-only #t)
  (keyword-style #f read-only #t)
  (radix #f read-only #t)
  (parser-table #f read-only #t)
  (shared-objects #f read-only #t)
  ;; Cached port operations
  (discretionary-write-char #f read-only #t)
  (get-position #f read-only #t)
  (input-line #f read-only #t)
  (peek-char #f read-only #t)
  (read-char #f read-only #t)
  position-mapping)

(define (initial-db port environment)
  (let* ((environment
	  (if (or (default-object? environment)
		  (parser-table? environment))
	      (nearest-repl/environment)
	      (begin
		(guarantee-environment environment #f)
		environment)))
	 (atom-delimiters
	  (repl-environment-value environment '*PARSER-ATOM-DELIMITERS*))
	 (constituents
	  (repl-environment-value environment '*PARSER-CONSTITUENTS*)))
    (guarantee-char-set atom-delimiters #f)
    (guarantee-char-set constituents #f)
    (make-db (repl-environment-value environment '*PARSER-ASSOCIATE-POSITIONS?*)
	     atom-delimiters
	     (overridable-value
	      port environment '*PARSER-CANONICALIZE-SYMBOLS?*)
	     constituents
	     (overridable-value
	      port environment '*PARSER-ENABLE-FILE-ATTRIBUTES-PARSING?*)
	     (overridable-value port environment '*PARSER-KEYWORD-STYLE*)
	     (repl-environment-value environment '*PARSER-RADIX*)
	     (repl-environment-value environment '*PARSER-TABLE*)
	     (make-shared-objects)
	     (port/operation port 'DISCRETIONARY-WRITE-CHAR)
	     (position-operation port environment)
	     (port/operation port 'INPUT-LINE)
	     (port/operation port 'PEEK-CHAR)
	     (port/operation port 'READ-CHAR)
	     '())))

(define (repl-environment-value environment name)
  (environment-lookup-or
   environment name
   (lambda ()
     (environment-lookup-or
      (->environment '(USER)) name
      (lambda ()
	(environment-lookup environment name))))))

(define (overridable-value port environment name)
  ;; Check the port property list for the name, and then the
  ;; environment.  This way a port can override the default.
  (let* ((nope "no-overridden-value")
	 (v (port/get-property port name nope)))
    (if (eq? v nope)
	(repl-environment-value environment name)
	v)))

(define (position-operation port environment)
  (let ((default (lambda (port) port #f)))
    (if (repl-environment-value environment '*PARSER-ASSOCIATE-POSITIONS?*)
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
  (if file-attribute-alist
      (begin
	;; Disable further attributes parsing.
	(port/set-property! port '*PARSER-ENABLE-FILE-ATTRIBUTES-PARSING?* #f)
	(process-keyword-attribute file-attribute-alist port)
	(process-mode-attribute file-attribute-alist port)
	(process-studly-case-attribute file-attribute-alist port))))

(define (lookup-file-attribute file-attribute-alist attribute)
  (assoc attribute file-attribute-alist
	 (lambda (left right)
	   (string-ci=? (symbol-name left) (symbol-name right)))))

;;; Look for keyword-style: prefix or keyword-style: suffix
(define (process-keyword-attribute file-attribute-alist port)
  (let ((keyword-entry
	 (lookup-file-attribute file-attribute-alist 'KEYWORD-STYLE)))
    (if (pair? keyword-entry)
	(let ((value (cdr keyword-entry)))
	  (cond ((and (symbol? value)
		      (or (string-ci=? (symbol-name value) "none")
			  (string-ci=? (symbol-name value) "false")))
		 (port/set-property! port '*PARSER-KEYWORD-STYLE* #f))
		((and (symbol? value)
		      (string-ci=? (symbol-name value) "prefix"))
		 (port/set-property! port '*PARSER-KEYWORD-STYLE* 'PREFIX))
		((and (symbol? value)
		      (string-ci=? (symbol-name value) "suffix"))
		 (port/set-property! port '*PARSER-KEYWORD-STYLE* 'SUFFIX))
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
		  (not (string-ci=? (symbol-name value) "scheme")))
	      (warn "Unexpected file mode:" (if (symbol? value)
						(symbol-name value)
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
			  (string-ci=? (symbol-name value) "true")))
		 ;; STricTly cHeck thE case.
		 (cond ((not (string=? (symbol-name (car studly-case-entry))
				       "sTuDly-case"))
			(warn "Attribute name mismatch.  Expected sTuDly-case.")
			#f)
		       ((and (symbol? value)
			     (not (string=? (symbol-name value) "True")))
			(warn "Attribute value mismatch.  Expected True.")
			#f)
		       (else
			(port/set-property!
			 port '*PARSER-CANONICALIZE-SYMBOLS?* #f))))
		((or (not value)
		     (and (symbol? value)
			  (string-ci=? (symbol-name value) "false")))
		 (port/set-property! port '*PARSER-CANONICALIZE-SYMBOLS?* #t))
		(else (warn "Unrecognized value for sTuDly-case" value)))))))


(define-syntax define-parse-error
  (sc-macro-transformer
   (lambda (form environment)
     environment
     (if (syntax-match? '((+ SYMBOL) EXPRESSION) (cdr form))
	 (let ((name (caadr form))
	       (field-names (cdadr form))
	       (reporter (caddr form)))
	   (let ((ct (symbol-append 'CONDITION-TYPE: name)))
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
		(SET! ,(symbol-append 'ERROR: name)
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
(define condition-type:illegal-unhash)
(define condition-type:no-quoting-allowed)
(define condition-type:non-shared-object)
(define condition-type:parse-error)
(define condition-type:premature-eof)
(define condition-type:re-shared-object)
(define condition-type:unbalanced-close)
(define condition-type:undefined-hash)
(define condition-type:unexpected-restart)
(define error:illegal-bit-string)
(define error:illegal-boolean)
(define error:illegal-char)
(define error:illegal-dot-usage)
(define error:illegal-hashed-object)
(define error:illegal-named-constant)
(define error:illegal-number)
(define error:illegal-unhash)
(define error:no-quoting-allowed)
(define error:non-shared-object)
(define error:premature-eof)
(define error:re-shared-object)
(define error:unbalanced-close)
(define error:undefined-hash)
(define error:unexpected-restart)

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
  unspecific)