#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/parse.scm,v 14.4 1988/08/05 20:48:25 cph Exp $

Copyright (c) 1988 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this software, to redistribute
it, and to use it for any purpose is granted, subject to the following
restrictions and understandings.

1. Any copy made of this software must include this copyright notice
in full.

2. Users of this software agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this software.

3. All materials developed as a consequence of the use of this
software shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that the operation of
this software will be error-free, and MIT is under no obligation to
provide any services, by way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. |#

;;;; Scheme Parser
;;; package: (runtime parser)

(declare (usual-integrations))

(define (initialize-package!)
  (set! char-set/undefined-atom-delimiters (char-set #\[ #\] #\{ #\} #\|))
  (set! char-set/whitespace
	(char-set #\Tab #\Linefeed #\Page #\Return #\Space))
  (set! char-set/non-whitespace (char-set-invert char-set/whitespace))
  (set! char-set/comment-delimiters (char-set #\Newline))
  (set! char-set/special-comment-leaders (char-set #\# #\|))
  (set! char-set/string-delimiters (char-set #\" #\\))
  (set! char-set/atom-delimiters
	(char-set-union char-set/whitespace
			(char-set-union char-set/undefined-atom-delimiters
					(char-set #\( #\) #\; #\" #\' #\`))))
  (set! char-set/atom-constituents (char-set-invert char-set/atom-delimiters))
  (set! char-set/char-delimiters
	(char-set-union (char-set #\- #\\) char-set/atom-delimiters))
  (set! char-set/symbol-leaders
	(char-set-difference char-set/atom-constituents
			     (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
				       #\+ #\- #\. #\#)))
  (set! char-set/not-octal
	(char-set-invert (char-set #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))

  (set! lambda-optional-tag (intern "#!optional"))
  (set! lambda-rest-tag (intern "#!rest"))
  (set! dot-symbol (intern "."))
  (set! named-objects
	`((NULL . ,(list))
	  (FALSE . ,false)
	  (TRUE . ,true)
	  (OPTIONAL . ,lambda-optional-tag)
	  (REST . ,lambda-rest-tag)))

  (set! *parser-radix* 10)
  (set! system-global-parser-table (make-system-global-parser-table))
  (set-current-parser-table! system-global-parser-table))

(define char-set/undefined-atom-delimiters)
(define char-set/whitespace)
(define char-set/non-whitespace)
(define char-set/comment-delimiters)
(define char-set/special-comment-leaders)
(define char-set/string-delimiters)
(define char-set/atom-delimiters)
(define char-set/atom-constituents)
(define char-set/char-delimiters)
(define char-set/symbol-leaders)
(define char-set/not-octal)

(define lambda-optional-tag)
(define lambda-rest-tag)
(define *parser-radix*)
(define system-global-parser-table)

(define (make-system-global-parser-table)
  (let ((table
	 (make-parser-table parse-object/atom
			    (collect-list-wrapper parse-object/atom)
			    parse-object/special-undefined
			    collect-list/special-undefined)))
    (for-each (lambda (entry)
		(apply parser-table/set-entry! table entry))
	      `(("#" ,parse-object/special ,collect-list/special)
		(,char-set/symbol-leaders ,parse-object/symbol)
		(("#b" "#B") ,parse-object/numeric-prefix)
		(("#o" "#O") ,parse-object/numeric-prefix)
		(("#d" "#D") ,parse-object/numeric-prefix)
		(("#x" "#X") ,parse-object/numeric-prefix)
		(("#i" "#I") ,parse-object/numeric-prefix)
		(("#e" "#E") ,parse-object/numeric-prefix)
		(("#s" "#S") ,parse-object/numeric-prefix)
		(("#l" "#L") ,parse-object/numeric-prefix)
		("#*" ,parse-object/bit-string)
		("(" ,parse-object/list-open)
		("#(" ,parse-object/vector-open)
		(")" ,parse-object/list-close ,collect-list/list-close)
		(,char-set/whitespace
		 ,parse-object/whitespace
		 ,collect-list/whitespace)
		(,char-set/undefined-atom-delimiters
		 ,parse-object/undefined-atom-delimiter
		 ,collect-list/undefined-atom-delimiter)
		(";" ,parse-object/comment ,collect-list/comment)
		("#|"
		 ,parse-object/special-comment
		 ,collect-list/special-comment)
		("'" ,parse-object/quote)
		("`" ,parse-object/quasiquote)
		("," ,parse-object/unquote)
		("\"" ,parse-object/string-quote)
		("#\\" ,parse-object/char-quote)
		(("#f" "#F") ,parse-object/false)
		(("#t" "#T") ,parse-object/true)
		("#!" ,parse-object/named-constant)))
    table))

;;;; Top Level

(define (parse-object port parser-table)
  (if (not (parser-table? parser-table))
      (error "Not a valid parser table" parser-table))
  (parse-object/internal port parser-table))

(define (parse-objects port parser-table last-object?)
  (if (not (parser-table? parser-table))
      (error "Not a valid parser table" parser-table))
  (parse-objects/internal port parser-table last-object?))

(define (parse-object/internal port parser-table)
  (within-parser port parser-table parse-object/dispatch))

(define (parse-objects/internal port parser-table last-object?)
  (let loop ()
    (let ((object (parse-object/internal port parser-table)))
      (if (last-object? object)
	  '()
	  (cons-stream object (loop))))))

(define (within-parser port parser-table thunk)
  (fluid-let
      ((*parser-input-port* port)
       (*parser-peek-char* (input-port/operation/peek-char port))
       (*parser-discard-char* (input-port/operation/discard-char port))
       (*parser-read-char* (input-port/operation/read-char port))
       (*parser-read-string* (input-port/operation/read-string port))
       (*parser-discard-chars* (input-port/operation/discard-chars port))
       (*parser-parse-object-table* (parser-table/parse-object parser-table))
       (*parser-collect-list-table* (parser-table/collect-list parser-table))
       (*parser-parse-object-special-table*
	(parser-table/parse-object-special parser-table))
       (*parser-collect-list-special-table*
	(parser-table/collect-list-special parser-table)))
    (thunk)))

;;;; Character Operations

(define *parser-input-port*)
(define *parser-peek-char*)
(define *parser-discard-char*)
(define *parser-read-char*)
(define *parser-read-string*)
(define *parser-discard-chars*)

(define-integrable (peek-char)
  (or (peek-char/eof-ok)
      (parse-error/end-of-file)))

(define-integrable (peek-char/eof-ok)
  (*parser-peek-char* *parser-input-port*))

(define-integrable (read-char)
  (or (read-char/eof-ok)
      (parse-error/end-of-file)))

(define-integrable (read-char/eof-ok)
  (*parser-read-char* *parser-input-port*))

(define-integrable (discard-char)
  (*parser-discard-char* *parser-input-port*))

(define-integrable (read-string delimiters)
  (*parser-read-string* *parser-input-port* delimiters))

(define-integrable (discard-chars delimiters)
  (*parser-discard-chars* *parser-input-port* delimiters))

(define (parse-error/end-of-file)
  (parse-error "end of file"))

(define (parse-error message #!optional irritant)
  (let ((message (string-append "PARSE-OBJECT: " message)))
    (if (default-object? irritant)
	(error message)
	(error message irritant))))

;;;; Dispatch Points

(define *parser-parse-object-table*)
(define *parser-collect-list-table*)
(define *parser-parse-object-special-table*)
(define *parser-collect-list-special-table*)

(define-integrable (parse-object/dispatch)
  (let ((char (peek-char/eof-ok)))
    (if char
	((vector-ref *parser-parse-object-table*
		     (or (char-ascii? char) (parse-error/non-ascii))))
	(make-eof-object *parser-input-port*))))

(define-integrable (collect-list/dispatch)
  ((vector-ref *parser-collect-list-table* (peek-ascii))))

(define (parse-object/special)
  (discard-char)
  ((vector-ref *parser-parse-object-special-table* (peek-ascii))))

(define (collect-list/special)
  (discard-char)
  ((vector-ref *parser-collect-list-special-table* (peek-ascii))))

(define-integrable (peek-ascii)
  (or (char-ascii? (peek-char))
      (parse-error/non-ascii)))

(define (parse-error/non-ascii)
  (parse-error "Non-ASCII character encountered" (read-char)))

(define (parse-object/special-undefined)
  (parse-error "No such special reader macro" (peek-char))
  (parse-object/dispatch))

(define (collect-list/special-undefined)
  (parse-error "No such special reader macro" (peek-char))
  (collect-list/dispatch))

;;;; Symbols/Numbers

(define (parse-object/atom)
  (build-atom (read-atom)))

(define-integrable (read-atom)
  (read-string char-set/atom-delimiters))

(define (build-atom string)
  (or (parse-number string)
      (intern-string! string)))

(define-integrable (parse-number string)
  (string->number string false *parser-radix*))

(define (intern-string! string)
  ;; Special version of `intern' to reduce consing and increase speed.
  (substring-upcase! string 0 (string-length string))
  (string->symbol string))

(define (parse-object/symbol)
  (intern-string! (read-atom)))

(define (parse-object/numeric-prefix)
  (let ((number
	 (let ((char (read-char)))
	   (string-append (char->string #\# char) (read-atom)))))
    (or (parse-number number)
	(parse-error "Bad number syntax" number))))

(define (parse-object/bit-string)
  (discard-char)
  (let ((string (read-atom)))
    (unsigned-integer->bit-string
     (string-length string)
     (or (string->number string false 2)
	 (error "READ: bad syntax for bit-string")))))
;;;; Lists/Vectors

(define (parse-object/list-open)
  (discard-char)
  (collect-list/top-level))

(define (parse-object/vector-open)
  (discard-char)
  (list->vector (collect-list/top-level)))

(define (parse-object/list-close)
  (if (and ignore-extra-list-closes
	   (eq? console-input-port *parser-input-port*))
      (discard-char)
      (parse-error "Unmatched close paren" (read-char)))
  (parse-object/dispatch))

(define (collect-list/list-close)
  (discard-char)
  '())

(define ignore-extra-list-closes
  true)

(define (collect-list/top-level)
  (let ((value (collect-list/dispatch)))
    (if (and (pair? value)
	     (eq? dot-symbol (car value)))
	(parse-error "Improperly formed dotted list" value)
	value)))

(define ((collect-list-wrapper parse-object))
  (let ((first (parse-object)))			;forces order.
    (let ((rest (collect-list/dispatch)))
      (if (and (pair? rest)
	       (eq? dot-symbol (car rest)))
	  (if (and (pair? (cdr rest))
		   (null? (cddr rest)))
	      (cons first (cadr rest))
	      (parse-error "Improperly formed dotted list" (cons first rest)))
	  (cons first rest)))))

(define dot-symbol)

;;;; Whitespace/Comments

(define (parse-object/whitespace)
  (discard-whitespace)
  (parse-object/dispatch))

(define (collect-list/whitespace)
  (discard-whitespace)
  (collect-list/dispatch))

(define (discard-whitespace)
  (discard-chars char-set/non-whitespace))

(define (parse-object/undefined-atom-delimiter)
  (parse-error "Undefined atom delimiter" (read-char))
  (parse-object/dispatch))

(define (collect-list/undefined-atom-delimiter)
  (parse-error "Undefined atom delimiter" (read-char))
  (collect-list/dispatch))

(define (parse-object/comment)
  (discard-comment)
  (parse-object/dispatch))

(define (collect-list/comment)
  (discard-comment)
  (collect-list/dispatch))

(define (discard-comment)
  (discard-char)
  (discard-chars char-set/comment-delimiters)
  (discard-char))

(define (parse-object/special-comment)
  (discard-special-comment)
  (parse-object/dispatch))

(define (collect-list/special-comment)
  (discard-special-comment)
  (collect-list/dispatch))

(define (discard-special-comment)
  (discard-char)
  (let loop ()
    (discard-chars char-set/special-comment-leaders)
    (if (char=? #\| (read-char))
	(if (char=? #\# (peek-char))
	    (discard-char)
	    (loop))
	(begin (if (char=? #\| (peek-char))
		   (begin (discard-char)
			  (loop)))
	       (loop)))))

;;;; Quoting

(define (parse-object/quote)
  (discard-char)
  (list 'QUOTE (parse-object/dispatch)))

(define (parse-object/quasiquote)
  (discard-char)
  (list 'QUASIQUOTE (parse-object/dispatch)))

(define (parse-object/unquote)
  (discard-char)
  (if (char=? #\@ (peek-char))
      (begin (discard-char)
	     (list 'UNQUOTE-SPLICING (parse-object/dispatch)))
      (list 'UNQUOTE (parse-object/dispatch))))

(define (parse-object/string-quote)
  (discard-char)
  (let loop ()
    (let ((string (read-string char-set/string-delimiters)))
      (if (char=? #\" (read-char))
	  string
	  (let ((char
		 (let ((char (read-char)))
		   (cond ((char-ci=? char #\t) #\Tab)
			 ((char-ci=? char #\n) #\Newline)
			 ((char-ci=? char #\f) #\Page)
			 ((char->digit char 8)
			  (octal->char
			   (string-append (char->string char)
					  (read-string char-set/not-octal))))
			 (else char)))))
	    (string-append string
			   (char->string char)
			   (loop)))))))

(define (octal->char string)
  (let ((end (string-length string))
	(loser
	 (lambda (message)
	   (error (string-append "Octal string escape " message ":") string))))
    (if (> end 3)
	(loser "too long"))
    (let loop ((index 0) (sum 0))
      (if (= index end)
	  (begin
	    (if (>= sum 256)
		(loser "exceeds ASCII range"))
	    (ascii->char sum))
	  (loop (1+ index)
		(+ (* sum 8) (char->digit (string-ref string index) 8)))))))

(define (parse-object/char-quote)
  (discard-char)
  (if (char=? #\\ (peek-char))
      (read-char)
      (name->char
       (let loop ()
	 (cond ((char=? #\\ (peek-char))
		(discard-char)
		(char->string (read-char)))
	       ((char-set-member? char-set/char-delimiters (peek-char))
		(char->string (read-char)))
	       (else
		(let ((string (read-string char-set/char-delimiters)))
		  (if (let ((char (peek-char/eof-ok)))
			(and char
			     (char=? #\- char)))
		      (begin (discard-char)
			     (string-append string "-" (loop)))
		      string))))))))

;;;; Constants

(define (parse-object/false)
  (discard-char)
  false)

(define (parse-object/true)
  (discard-char)
  true)

(define (parse-object/named-constant)
  (discard-char)
  (let ((object-name (parse-object/dispatch)))
    (cdr (or (assq object-name named-objects)
	     (parse-error "No object by this name" object-name)))))

(define named-objects)