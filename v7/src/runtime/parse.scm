#| -*-Scheme-*-

$Id: parse.scm,v 14.43 2004/01/15 21:00:08 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1997,1998,1999 Massachusetts Institute of Technology
Copyright 2001,2002,2003,2004 Massachusetts Institute of Technology

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

;;;; Scheme Parser
;;; package: (runtime parser)

(declare (usual-integrations))

(define *parser-canonicalize-symbols?* #t)
(define *parser-radix* 10)
(define ignore-extra-list-closes #t)

(define (parse-object port table)
  (guarantee-input-port port 'PARSE-OBJECT)
  (guarantee-parser-table table 'PARSE-OBJECT)
  ((top-level-parser port) port table))

(define (parse-objects port table last-object?)
  (guarantee-input-port port 'PARSE-OBJECTS)
  (guarantee-parser-table table 'PARSE-OBJECTS)
  (let ((parser (top-level-parser port)))
    (let loop ()
      (let ((object (parser port table)))
	(if (last-object? object)
	    '()
	    (cons-stream object (loop)))))))

(define (top-level-parser port)
  (or (port/operation port 'READ)
      (let ((read-start (port/operation port 'READ-START))
	    (read-finish (port/operation port 'READ-FINISH)))
	(lambda (port table)
	  (if read-start (read-start port))
	  (let ((object
		 (fluid-let ((*shared-objects* (make-shared-objects)))
		   (let loop ()
		     (let ((object (dispatch port table)))
		       (if (eq? object close-parenthesis)
			   (begin
			     (if (not (and (eq? port console-input-port)
					   ignore-extra-list-closes))
				 (error:illegal-char (car object)))
			     (loop))
			   (begin
			     (if (eq? object close-bracket)
				 (error:illegal-char (car object)))
			     object)))))))
	    (if read-finish (read-finish port))
	    object)))))

(define (dispatch port table)
  (let ((char (read-char port)))
    (if (eof-object? char)
	char
	(let ((handler (get-handler char (parser-table/initial table))))
	  (if (not handler)
	      (error:illegal-char char))
	  (handler port table char)))))

(define (dispatch-special port table)
  (let ((char (read-char/no-eof port)))
    (let ((handler (get-handler char (parser-table/special table))))
      (if (not handler)
	  (error:illegal-char char))
      (handler port table char))))

(define (dispatch/no-eof port table)
  (let ((object (dispatch port table)))
    (if (eof-object? object)
	(error:premature-eof port))
    object))

(define (get-handler char handlers)
  (let ((n (char->integer char)))
    (if (not (fix:< n #x100))
	(error:illegal-char char))
    (vector-ref handlers n)))

(define system-global-parser-table)
(define char-set/constituents)
(define char-set/atom-delimiters)
(define char-set/symbol-quotes)
(define char-set/number-leaders)
(define char-set/atom-constituents)
(define char-set/char-constituents)

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
	 (atom-constituents
	  (char-set-difference constituents
			       (char-set-union atom-delimiters
					       symbol-quotes)))
	 (number-leaders
	  (char-set-union char-set:numeric
			  (string->char-set "+-.")))
	 (symbol-leaders
	  (char-set-difference constituents
			       (char-set-union atom-delimiters
					       number-leaders)))
	 (special-number-leaders
	  (string->char-set "bBoOdDxXiIeEsSlL"))
	 (char-constituents
	  (char-set-union char-set:alphanumeric
			  (string->char-set "+-")))
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
      (store-char initial #\; handler:comment)
      (store-char special #\| handler:multi-line-comment)
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
    (set! char-set/atom-constituents atom-constituents)
    (set! char-set/number-leaders number-leaders)
    (set! char-set/char-constituents char-constituents))
  (set-current-parser-table! system-global-parser-table)
  (initialize-condition-types!))

(define (handler:whitespace port table char)
  char
  (dispatch port table))

(define (handler:atom port table char)
  table
  (receive (string quoted?) (parse-atom port (list char))
    (if quoted?
	(%string->symbol string)
	(or (string->number string *parser-radix*)
	    (%string->symbol string)))))

(define (handler:symbol port table char)
  table
  (receive (string quoted?) (parse-atom port (list char))
    quoted?
    (%string->symbol string)))

(define (handler:number port table char)
  table
  (let ((string (parse-atom/no-quoting port (list #\# char))))
    (or (string->number string *parser-radix*)
	(error:illegal-number string))))

(define (parse-atom port prefix)
  (let ((port* (open-output-string))
	(canon
	 (if *parser-canonicalize-symbols?*
	     char-downcase
	     identity-procedure)))
    (for-each (lambda (char) (write-char char port*)) prefix)
    (let read-unquoted ((quoted? #f))
      (let ((char (peek-char port)))
	(if (or (eof-object? char)
		(char-set-member? char-set/atom-delimiters char))
	    (values (get-output-string port*) quoted?)
	    (begin
	      (discard-char port)
	      (cond ((char-set-member? char-set/atom-constituents char)
		     (write-char (canon char) port*)
		     (read-unquoted quoted?))
		    ((char=? char #\\)
		     (write-char (read-char/no-eof port) port*)
		     (read-unquoted #t))
		    ((char=? char #\|)
		     (let read-quoted ()
		       (let ((char (read-char/no-eof port)))
			 (cond ((char-set-member? char-set/constituents char)
				(write-char char port*)
				(read-quoted))
			       ((char=? char #\|)
				(read-unquoted #t))
			       ((char=? char #\\)
				(write-char (read-char/no-eof port) port*)
				(read-quoted))
			       (else
				(error:illegal-char char))))))
		    (else
		     (error:illegal-char char)))))))))

(define (parse-atom/no-quoting port prefix)
  (receive (string quoted?) (parse-atom port prefix)
    (if quoted?
	(error:no-quoting-allowed string))
    string))

(define (handler:list port table char)
  char
  (let loop ((objects '()))
    (let ((object (dispatch/no-eof port table)))
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

(define (handler:vector port table char)
  char
  (let loop ((objects '()))
    (let ((object (dispatch/no-eof port table)))
      (if (eq? object close-parenthesis)
	  (list->vector (reverse! objects))
	  (loop (cons object objects))))))

(define (handler:hashed-object port table char)
  char
  (let loop ((objects '()))
    (let ((object (dispatch/no-eof port table)))
      (if (eq? object close-bracket)
	  (let ((objects (reverse! objects)))
	    (if (and (pair? objects)
		     (pair? (cdr objects)))
		(parse-unhash (cadr objects))
		(error:illegal-hashed-object objects)))
	  (loop (cons object objects))))))

(define (parse-unhash object)
  (if (not (exact-nonnegative-integer? object))
      (error:illegal-unhash object))
  (if (eq? object 0)
      #f
      (or (object-unhash object)
	  (error:undefined-hash object))))

(define (handler:close-parenthesis port table char)
  port table char
  close-parenthesis)

(define (handler:close-bracket port table char)
  port table char
  close-bracket)

(define close-parenthesis (list #\)))
(define close-bracket (list #\]))

(define (handler:comment port table char)
  char
  (let loop ()
    (let ((char (read-char port)))
      (cond ((eof-object? char) char)
	    ((char=? char #\newline) unspecific)
	    (else (loop)))))
  (dispatch port table))

(define (handler:multi-line-comment port table char)
  char
  (let loop ()
    (case (read-char/no-eof port)
      ((#\#)
       (let sharp ()
	 (case (read-char/no-eof port)
	   ((#\#) (sharp))
	   ((#\|) (loop) (loop))
	   (else (loop)))))
      ((#\|)
       (let vbar ()
	 (case (read-char/no-eof port)
	   ((#\#) unspecific)
	   ((#\|) (vbar))
	   (else (loop)))))
      (else (loop))))
  (dispatch port table))

(define (handler:quote port table char)
  char
  (list 'QUOTE (dispatch/no-eof port table)))

(define (handler:quasiquote port table char)
  char
  (list 'QUASIQUOTE (dispatch/no-eof port table)))

(define (handler:unquote port table char)
  char
  (list 'UNQUOTE (dispatch/no-eof port table)))

(define (handler:string port table char)
  table char
  (call-with-output-string
    (lambda (port*)
      (let loop ()
	(let ((char (read-char/no-eof port)))
	  (if (not (char=? char #\"))
	      (begin
		(write-char (if (char=? char #\\)
				(read-char/no-eof port)
				char)
			    port*)
		(loop))))))))

(define (handler:special port table char)
  char
  (dispatch-special port table))

(define (handler:false port table char)
  table
  (let ((string (parse-atom/no-quoting port (list char))))
    (if (not (string-ci=? string "f"))
	(error:illegal-boolean string)))
  #f)

(define (handler:true port table char)
  table
  (let ((string (parse-atom/no-quoting port (list char))))
    (if (not (string-ci=? string "t"))
	(error:illegal-boolean string)))
  #t)

(define (handler:bit-string port table char)
  table char
  (let ((string (parse-atom/no-quoting port '())))
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

(define (handler:char port table char)
  table char
  (let ((char (read-char/no-eof port)))
    (if (or (char=? char #\\)
	    (not (char-alphabetic? char)))
	char
	(name->char
	 (call-with-output-string
	   (lambda (port*)
	     (write-char char port*)
	     (let loop ()
	       (let ((char (peek-char port)))
		 (cond ((eof-object? char)
			unspecific)
		       ((char-set-member? char-set/char-constituents char)
			(discard-char port)
			(write-char char port*)
			(loop))
		       ((char=? char #\\)
			(discard-char port)
			(write-char (read-char/no-eof port) port*))
		       (else
			unspecific))))))))))

(define (handler:named-constant port table char)
  table char
  (let ((name (intern (parse-atom/no-quoting port '()))))
    (let ((entry (assq name named-constants)))
      (if (not entry)
	  (error:illegal-named-constant name))
      (cdr entry))))

(define lambda-optional-tag
  (object-new-type (ucode-type constant) 3))

(define lambda-rest-tag
  (object-new-type (ucode-type constant) 4))

(define lambda-auxiliary-tag
  '|#!aux|)

(define named-constants
  `((NULL . ())
    (FALSE . #f)
    (TRUE . #t)
    (OPTIONAL . ,lambda-optional-tag)
    (REST . ,lambda-rest-tag)
    (AUX . ',lambda-auxiliary-tag)))

(define (handler:unhash port table char)
  char
  (let ((object (parse-unhash (dispatch/no-eof port table))))
    ;; This may seem a little random, because #@N doesn't just
    ;; return an object.  However, the motivation for this piece of
    ;; syntax is convenience -- and 99.99% of the time the result of
    ;; this syntax will be evaluated, and the user will expect the
    ;; result of the evaluation to be the object she was referring
    ;; to.  If the quotation isn't there, the user just gets
    ;; confused.
    (if (scode-constant? object)
	object
	(make-quotation object))))

(define (handler:special-arg port table char)
  (let loop ((n (char->digit char 10)))
    (let ((char (read-char/no-eof port)))
      (cond ((char-numeric? char)
	     (loop (+ (* 10 n) (char->digit char 10))))
	    ((char=? char #\=)
	     (let ((object (dispatch/no-eof port table)))
	       (save-shared-object! n object)
	       object))
	    ((char=? char #\#)
	     (get-shared-object n))
	    (else
	     (error:illegal-char char))))))

(define (make-shared-objects)
  (make-eqv-hash-table))

(define (save-shared-object! n object)
  (if (not (eq? (hash-table/get *shared-objects* n non-shared-object)
		non-shared-object))
      (error:re-shared-object n object))
  (hash-table/put! *shared-objects* n object))

(define (get-shared-object n)
  (let ((object (hash-table/get *shared-objects* n non-shared-object)))
    (if (eq? object non-shared-object)
	(error:non-shared-object n))
    object))

(define *shared-objects*)
(define non-shared-object (list 'NON-SHARED-OBJECT))

(define (read-char port)
  (let loop ()
    (or (input-port/read-char port)
	(loop))))

(define (read-char/no-eof port)
  (let ((char (read-char port)))
    (if (eof-object? char)
	(error:premature-eof port))
    char))

(define (discard-char port)
  (let loop ()
    (if (not (input-port/discard-char port))
	(loop))))

(define (peek-char port)
  (let loop ()
    (or (input-port/peek-char port)
	(loop))))

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
		      (MAKE-CONDITION-TYPE 'ILLEGAL-BIT-STRING
			  CONDITION-TYPE:PARSE-ERROR
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

(define condition-type:parse-error)

(define condition-type:illegal-bit-string)
(define condition-type:illegal-boolean)
(define condition-type:illegal-char)
(define condition-type:illegal-dot-usage)
(define condition-type:illegal-hashed-object)
(define condition-type:illegal-named-constant)
(define condition-type:illegal-number)
(define condition-type:illegal-unhash)
(define condition-type:undefined-hash)
(define condition-type:no-quoting-allowed)
(define condition-type:premature-eof)
(define condition-type:re-shared-object)
(define condition-type:non-shared-object)

(define error:illegal-bit-string)
(define error:illegal-boolean)
(define error:illegal-char)
(define error:illegal-dot-usage)
(define error:illegal-hashed-object)
(define error:illegal-named-constant)
(define error:illegal-number)
(define error:illegal-unhash)
(define error:undefined-hash)
(define error:no-quoting-allowed)
(define error:premature-eof)
(define error:re-shared-object)
(define error:non-shared-object)

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
      (write-string "Ill-formed boolean: #" port)
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
  unspecific)