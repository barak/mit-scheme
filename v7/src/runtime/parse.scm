;;; -*-Scheme-*-
;;;
;;;	$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/parse.scm,v 13.42 1987/03/17 18:51:44 cph Exp $
;;;
;;;	Copyright (c) 1987 Massachusetts Institute of Technology
;;;
;;;	This material was developed by the Scheme project at the
;;;	Massachusetts Institute of Technology, Department of
;;;	Electrical Engineering and Computer Science.  Permission to
;;;	copy this software, to redistribute it, and to use it for any
;;;	purpose is granted, subject to the following restrictions and
;;;	understandings.
;;;
;;;	1. Any copy made of this software must include this copyright
;;;	notice in full.
;;;
;;;	2. Users of this software agree to make their best efforts (a)
;;;	to return to the MIT Scheme project any improvements or
;;;	extensions that they make, so that these may be included in
;;;	future releases; and (b) to inform MIT of noteworthy uses of
;;;	this software.
;;;
;;;	3. All materials developed as a consequence of the use of this
;;;	software shall duly acknowledge such use, in accordance with
;;;	the usual standards of acknowledging credit in academic
;;;	research.
;;;
;;;	4. MIT has made no warrantee or representation that the
;;;	operation of this software will be error-free, and MIT is
;;;	under no obligation to provide any services, by way of
;;;	maintenance, update, or otherwise.
;;;
;;;	5. In conjunction with products arising from the use of this
;;;	material, there shall be no use of the name of the
;;;	Massachusetts Institute of Technology nor of any adaptation
;;;	thereof in any advertising, promotional, or sales literature
;;;	without prior written consent from MIT in each case.
;;;

;;;; Scheme Parser

(declare (usual-integrations))

(define *parser-radix* #d10)
(define *parser-table*)

(define parser-package
  (make-environment

(define *parser-parse-object-table*)
(define *parser-collect-list-table*)
(define *parser-parse-object-special-table*)
(define *parser-collect-list-special-table*)
(define *parser-peek-char*)
(define *parser-discard-char*)
(define *parser-read-char*)
(define *parser-read-string*)
(define *parser-discard-chars*)
(define *parser-input-port*)

(define (*parse-object port)
  (fluid-let ((*parser-input-port* port)
	      (*parser-parse-object-table* (caar *parser-table*))
	      (*parser-collect-list-table* (cdar *parser-table*))
	      (*parser-parse-object-special-table* (cadr *parser-table*))
	      (*parser-collect-list-special-table* (cddr *parser-table*))
	      (*parser-peek-char* (access :peek-char port))
	      (*parser-discard-char* (access :discard-char port))
	      (*parser-read-char* (access :read-char port))
	      (*parser-read-string* (access :read-string port))
	      (*parser-discard-chars* (access :discard-chars port)))
    (parse-object)))

(define (*parse-objects-until-eof port)
  (fluid-let ((*parser-input-port* port)
	      (*parser-parse-object-table* (caar *parser-table*))
	      (*parser-collect-list-table* (cdar *parser-table*))
	      (*parser-parse-object-special-table* (cadr *parser-table*))
	      (*parser-collect-list-special-table* (cddr *parser-table*))
	      (*parser-peek-char* (access :peek-char port))
	      (*parser-discard-char* (access :discard-char port))
	      (*parser-read-char* (access :read-char port))
	      (*parser-read-string* (access :read-string port))
	      (*parser-discard-chars* (access :discard-chars port)))
    (define (loop object)
      (if (eof-object? object)
	  '()
	  (cons object (loop (parse-object)))))
    (loop (parse-object))))

;;;; Character Operations

(declare (integrate peek-char read-char discard-char
		    read-string discard-chars))

(define (peek-char)
  (or (*parser-peek-char*)
      (error "End of file within READ")))

(define (read-char)
  (or (*parser-read-char*)
      (error "End of file within READ")))

(define (discard-char)
  (*parser-discard-char*))

(define (read-string delimiters)
  (declare (integrate delimiters))
  (*parser-read-string* delimiters))

(define (discard-chars delimiters)
  (declare (integrate delimiters))
  (*parser-discard-chars* delimiters))

;;; There are two major dispatch tables, one for parsing at top level,
;;; the other for parsing the elements of a list.  Most of the entries
;;; for each table are have similar actions.

;;; Default is atomic object.  Parsing an atomic object does not
;;; consume its terminator.  Thus different terminators [such as open
;;; paren, close paren, and whitespace], can have different effects on
;;; parser.

(define (parse-object:atom)
  (build-atom (read-atom)))

(define ((collect-list-wrapper object-parser))
  (let ((value (object-parser)))			;forces order.
    (cons value (collect-list))))

(define (parse-undefined-special)
  (error "No such special reader macro" (peek-char)))

(set! *parser-table*
      (cons (cons (vector-cons 256 parse-object:atom)
		  (vector-cons 256 (collect-list-wrapper parse-object:atom)))
	    (cons (vector-cons 256 parse-undefined-special)
		  (vector-cons 256 parse-undefined-special))))

(define ((parser-char-definer tables)
	 char/chars procedure #!optional list-procedure)
  (if (unassigned? list-procedure)
      (set! list-procedure (collect-list-wrapper procedure)))
  (define (do-it char)
    (vector-set! (car tables) (char->ascii char) procedure)
    (vector-set! (cdr tables) (char->ascii char) list-procedure))
  (cond ((char? char/chars) (do-it char/chars))
	((char-set? char/chars)
	 (for-each do-it (char-set-members char/chars)))
	((pair? char/chars) (for-each do-it char/chars))
	(else (error "Unknown character" char/chars))))

(define define-char
  (parser-char-definer (car *parser-table*)))

(define define-char-special
  (parser-char-definer (cdr *parser-table*)))

(declare (integrate peek-ascii parse-object collect-list))

(define (peek-ascii)
  (or (char-ascii? (peek-char))
      (non-ascii-error)))

(define (non-ascii-error)
  (error "Non-ASCII character encountered during parse" (read-char)))

(define (parse-object)
  (let ((char (*parser-peek-char*)))
    (if char
	((vector-ref *parser-parse-object-table*
		     (or (char-ascii? char)
			 (non-ascii-error))))
	eof-object)))

(define (collect-list)
  ((vector-ref *parser-collect-list-table* (peek-ascii))))

(define-char #\#
  (lambda ()
    (discard-char)
    ((vector-ref *parser-parse-object-special-table* (peek-ascii))))
  (lambda ()
    (discard-char)
    ((vector-ref *parser-collect-list-special-table* (peek-ascii)))))

(define numeric-leaders
  (char-set-union char-set:numeric
		  (char-set #\+ #\- #\. #\#)))

(define undefined-atom-delimiters
  (char-set #\[ #\] #\{ #\} #\|))

(define atom-delimiters
  (char-set-union char-set:whitespace
		  (char-set-union undefined-atom-delimiters
				  (char-set #\( #\) #\; #\" #\' #\`))))

(define atom-constituents
  (char-set-invert atom-delimiters))

(declare (integrate read-atom))

(define (read-atom)
  (read-string atom-delimiters))

(define (build-atom string)
  (or (parse-number string)
      (intern-string! string)))

(declare (integrate parse-number))

(define (parse-number string)
  (declare (integrate string))
  (string->number string false *parser-radix*))

(define (intern-string! string)
  (substring-upcase! string 0 (string-length string))
  (string->symbol string))

(define-char (char-set-difference atom-constituents numeric-leaders)
  (lambda ()
    (intern-string! (read-atom))))

(let ((numeric-prefix
       (lambda ()
	 (let ((number
		(let ((char (read-char)))
		  (string-append (char->string #\# char) (read-atom)))))
	   (or (parse-number number)
	       (error "READ: Bad number syntax" number))))))
  (define-char-special '(#\b #\B) numeric-prefix)
  (define-char-special '(#\o #\O) numeric-prefix)
  (define-char-special '(#\d #\D) numeric-prefix)
  (define-char-special '(#\x #\X) numeric-prefix)
  (define-char-special '(#\i #\I) numeric-prefix)
  (define-char-special '(#\e #\E) numeric-prefix)
  (define-char-special '(#\s #\S) numeric-prefix)
  (define-char-special '(#\l #\L) numeric-prefix))

(define-char #\(
  (lambda ()
    (discard-char)
    (collect-list)))

(define-char-special #\(
  (lambda ()
    (discard-char)
    (list->vector (collect-list))))

(define-char #\)
  (lambda ()
    (if (not (eq? console-input-port *parser-input-port*))
	(error "PARSE-OBJECT: Unmatched close paren" (read-char))
	(read-char))
    (parse-object))
  (lambda ()
    (discard-char)
    '()))

(define-char undefined-atom-delimiters
  (lambda ()
    (error "PARSE-OBJECT: Undefined atom delimiter" (read-char))
    (parse-object))
  (lambda ()
    (error "PARSE-OBJECT: Undefined atom delimiter" (read-char))
    (collect-list)))

(let ()

(vector-set! (cdar *parser-table*)
	     (char->ascii #\.)
  (lambda ()
    (discard-char)
    ;; atom with initial dot?
    (if (char-set-member? atom-constituents (peek-char))
	(let ((first (build-atom (string-append "." (read-atom)))))
	  (cons first (collect-list)))

	;; (A . B) -- get B and ignore whitespace following it.
	(let ((tail (parse-object)))
	  (discard-whitespace)
	  (if (not (char=? (peek-char) #\)))
	      (error "Illegal character in ignored stream" (peek-char)))
	  (discard-char)
	  tail))))

(define-char char-set:whitespace
  (lambda ()
    (discard-whitespace)
    (parse-object))
  (lambda ()
    (discard-whitespace)
    (collect-list)))

(define (discard-whitespace)
  (discard-chars non-whitespace))

(define non-whitespace
  (char-set-invert char-set:whitespace))

)

(let ()

(define-char #\;
  (lambda ()
    (discard-comment)
    (parse-object))
  (lambda ()
    (discard-comment)
    (collect-list)))

(define (discard-comment)
  (discard-char)
  (discard-chars comment-delimiters)
  (discard-char))

(define comment-delimiters
  (char-set char:newline))

)

(let ()

(define-char-special #\|
  (lambda ()
    (discard-char)
    (discard-special-comment)
    (parse-object))
  (lambda ()
    (discard-char)
    (discard-special-comment)
    (collect-list)))

(define (discard-special-comment)
  (discard-chars special-comment-leaders)
  (if (char=? #\| (read-char))
      (if (char=? #\# (peek-char))
	  (discard-char)
	  (discard-special-comment))
      (begin (if (char=? #\| (peek-char))
		 (begin (discard-char)
			(discard-special-comment)))
	     (discard-special-comment))))

(define special-comment-leaders
  (char-set #\# #\|))

)

(define-char #\'
  (lambda ()
    (discard-char)
    (list 'QUOTE (parse-object))))

(define-char #\`
  (lambda ()
    (discard-char)
    (list 'QUASIQUOTE (parse-object))))

(define-char #\,
  (lambda ()
    (discard-char)
    (if (char=? #\@ (peek-char))
	(begin (discard-char)
	       (list 'UNQUOTE-SPLICING (parse-object)))
	(list 'UNQUOTE (parse-object)))))

(define-char #\"
  (let ((delimiters (char-set #\" #\\)))
    (lambda ()
      (define (loop string)
	(if (char=? #\" (read-char))
	    string
	    (let ((char (read-char)))
	      (string-append string
			     (char->string
			      (cond ((char-ci=? char #\t) #\Tab)
				    ((char-ci=? char #\n) char:newline)
				    ((char-ci=? char #\f) #\Page)
				    (else char)))
			     (loop (read-string delimiters))))))
      (discard-char)
      (loop (read-string delimiters)))))

(define-char-special #\\
  (let ((delimiters (char-set-union (char-set #\- #\\) atom-delimiters)))
    (lambda ()
      (define (loop)
	(cond ((char=? #\\ (peek-char))
	       (discard-char)
	       (char->string (read-char)))
	      ((char-set-member? delimiters (peek-char))
	       (char->string (read-char)))
	      (else
	       (let ((string (read-string delimiters)))
		 (if (char=? #\- (peek-char))
		     (begin (discard-char)
			    (string-append string "-" (loop)))
		     string)))))
      (discard-char)
      (if (char=? #\\ (peek-char))
	  (read-char)
	  (name->char (loop))))))

(define ((fixed-object-parser object))
  (discard-char)
  object)

(define-char-special '(#\f #\F) (fixed-object-parser false))
(define-char-special '(#\t #\T) (fixed-object-parser true))

(define-char-special #\!
  (lambda ()
    (discard-char)
    (let ((object-name (parse-object)))
      (cdr (or (assq object-name named-objects)
	       (error "No object by this name" object-name))))))

(define named-objects
  `((NULL . ,(list))
    (FALSE . ,(eq? 'TRUE 'FALSE))
    (TRUE . ,(eq? 'TRUE 'TRUE))
    (OPTIONAL . ,(access lambda-optional-tag lambda-package))
    (REST . ,(access lambda-rest-tag lambda-package))))

;;; end PARSER-PACKAGE.
))

;;;; Parser Tables

(define (parser-table-copy table)
  (cons (cons (vector-copy (caar table))
	      (vector-copy (cdar table)))
	(cons (vector-copy (cadr table))
	      (vector-copy (cddr table)))))

(define parser-table-entry)
(define set-parser-table-entry!)
(let ()

(define (decode-parser-char table char receiver)
  (cond ((char? char)
	 (receiver (car table) (char->ascii char)))
	((string? char)
	 (cond ((= (string-length char) 1)
		(receiver (car table) (char->ascii (string-ref char 0))))
	       ((and (= (string-length char) 2)
		     (char=? #\# (string-ref char 0)))
		(receiver (cdr table) (char->ascii (string-ref char 1))))
	       (else
		(error "Bad character" 'DECODE-PARSER-CHAR char))))
	(else
	 (error "Bad character" 'DECODE-PARSER-CHAR char))))

(define (ptable-ref table index)
  (cons (vector-ref (car table) index)
	(vector-ref (cdr table) index)))

(define (ptable-set! table index value)
  (vector-set! (car table) index (car value))
  (vector-set! (cdr table) index (cdr value)))

(set! parser-table-entry
(named-lambda (parser-table-entry table char)
  (decode-parser-char table char ptable-ref)))

(set! set-parser-table-entry!
(named-lambda (set-parser-table-entry! table char entry)
  (decode-parser-char table char
    (lambda (sub-table index)
      (ptable-set! sub-table index entry)))))

)

