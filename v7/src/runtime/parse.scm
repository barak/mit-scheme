#| -*-Scheme-*-

$Id: parse.scm,v 14.39 2003/02/13 19:52:43 cph Exp $

Copyright 1986,1987,1988,1989,1990,1991 Massachusetts Institute of Technology
Copyright 1992,1993,1994,1997,1998,1999 Massachusetts Institute of Technology
Copyright 2001,2002,2003 Massachusetts Institute of Technology

This file is part of MIT Scheme.

MIT Scheme is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.

MIT Scheme is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with MIT Scheme; if not, write to the Free Software Foundation,
Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

|#

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
  (set! char-set/non-digit
	(char-set-difference (char-set-invert (char-set))
			     char-set:numeric))

  (set! lambda-optional-tag (object-new-type (microcode-type 'CONSTANT) 3))
  (set! lambda-rest-tag (object-new-type (microcode-type 'CONSTANT) 4))
  (set! lambda-auxiliary-tag (intern "#!aux"))
  (set! dot-symbol (intern "."))
  (set! named-objects
	`((NULL . ,(list))
	  (FALSE . ,false)
	  (TRUE . ,true)
	  (OPTIONAL . ,lambda-optional-tag)
	  (REST . ,lambda-rest-tag)
	  (AUX . ',lambda-auxiliary-tag)))

  (set! *parser-radix* 10)
  (set! *parser-associate-positions?* false)
  (set! *parser-associate-position* parser-associate-positions/default)
  (set! *parser-current-position* parser-current-position/default)
  (set! *parser-canonicalize-symbols?* #t)
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
(define char-set/non-digit)

(define lambda-optional-tag)
(define lambda-rest-tag)
(define lambda-auxiliary-tag)
(define *parser-radix*)
(define system-global-parser-table)

(define (make-system-global-parser-table)
  (let ((table
	 (make-parser-table parse-object/atom
			    (collect-list-wrapper parse-object/atom)
			    parse-object/special-undefined
			    collect-list/special-undefined)))
    (for-each (lambda (entry)
		(apply parser-table/set-entry!
		       (cons table entry)))
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
		("#!" ,parse-object/named-constant)
		(("#0" "#1" "#2" "#3" "#4" "#5" "#6" "#7" "#8" "#9")
		 ,parse-object/special-prefix ,collect-list/special-prefix)
		("#=" ,parse-object/define-shared)
		("##" ,parse-object/reference-shared)
		("#[" ,parse-object/unhash-printed-representation)
		;;("#$" ,test-recursive-read)
		("#@" ,parse-object/unhash)))
    table))

;;;; Top Level

(define (parse-object port parser-table)
  ((parsing-operation port) port parser-table))

(define (parse-objects port parser-table last-object?)
  (let ((operation (parsing-operation port)))
    (let loop ()
      (let ((object (operation port parser-table)))
	(if (last-object? object)
	    '()
	    (cons-stream object (loop)))))))

(define (parsing-operation port)
  (or (port/operation port 'READ)
      (let ((read-start (port/operation port 'READ-START))
	    (read-finish (port/operation port 'READ-FINISH)))
	(lambda (port parser-table)
	  (if read-start (read-start port))
	  (let ((object
		 (within-parser port parser-table parse-object/dispatch)))
	    (if read-finish (read-finish port))
	    object)))))

(define (within-parser port parser-table thunk)
  (if (not (parser-table? parser-table))
      (error:wrong-type-argument parser-table "parser table" 'WITHIN-PARSER))
  (fluid-let
      ((*parser-input-port* port)
       (*parser-parse-object-table* (parser-table/parse-object parser-table))
       (*parser-collect-list-table* (parser-table/collect-list parser-table))
       (*parser-parse-object-special-table*
	(parser-table/parse-object-special parser-table))
       (*parser-collect-list-special-table*
	(parser-table/collect-list-special parser-table))
       (*parser-current-special-prefix* #f)
       ;; Only create it on first entry:
       (*parser-cyclic-context* (or *parser-cyclic-context* (make-context)))
       (*parser-current-position*
	(if (not *parser-associate-positions?*)
	    parser-current-position/default
	    (current-position-getter port))))
    (cyclic-parser-post-edit (thunk))
))

;;;; Character Operations

(define *parser-input-port*)

(define (peek-char)
  (let ((char (peek-char/eof-ok)))
    (if (eof-object? char)
	(parse-error/end-of-file))
    char))

(define (peek-char/eof-ok)
  (let loop ()
    (or (input-port/peek-char *parser-input-port*)
	(loop))))

(define (read-char)
  (let ((char (read-char/eof-ok)))
    (if (eof-object? char)
	(parse-error/end-of-file))
    char))

(define (read-char/eof-ok)
  (let loop ()
    (or (input-port/read-char *parser-input-port*)
	(loop))))

(define-integrable (discard-char)
  (input-port/discard-char *parser-input-port*))

(define-integrable (read-string delimiters)
  (input-port/read-string *parser-input-port* delimiters))

(define-integrable (discard-chars delimiters)
  (input-port/discard-chars *parser-input-port* delimiters))

(define (parse-error/end-of-file)
  (parse-error "end of file"))

(define (parse-error message #!optional irritant)
  (let ((message (string-append "PARSE-ERROR: " message)))
    (if (default-object? irritant)
	(error message)
	(error message irritant))))

;;;; Dispatch Points

(define *parser-parse-object-table*)
(define *parser-collect-list-table*)
(define *parser-parse-object-special-table*)
(define *parser-collect-list-special-table*)

(define *parser-current-special-prefix*)

(define-integrable (parse-object/dispatch)
  (let ((char (peek-char/eof-ok)))
    (if (eof-object? char)
	char
	((vector-ref *parser-parse-object-table*
		     (or (char-ascii? char) (parse-error/non-ascii)))))))

(define-integrable (collect-list/dispatch)
  ((vector-ref *parser-collect-list-table* (peek-ascii))))

(define (parse-object/special)
  (discard-char)
  (set! *parser-current-special-prefix* #f)
  ((vector-ref *parser-parse-object-special-table* (peek-ascii))))

(define (collect-list/special)
  (discard-char)
  (set! *parser-current-special-prefix* #f)
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

;;;; Recording the position of objects for the compiler

(define *parser-associate-position*)
(define *parser-associate-positions?*)
(define *parser-current-position*)

(define-syntax define-accretor
  (sc-macro-transformer
   (lambda (form environment)
     (let ((offset (cadr form))
	   (param-list (caddr form))
	   (body (cdddr form)))
       `(DEFINE ,param-list
	  (LET ((CORE
		 (LAMBDA ()
		   ,@(map (lambda (expression)
			    (make-syntactic-closure environment
				(cdr param-list)
			      expression))
			  body))))
	    (IF *PARSER-ASSOCIATE-POSITIONS?*
		(RECORDING-OBJECT-POSITION ,offset CORE)
		(CORE))))))))

(define (current-position-getter port)
  (cond ((input-port/operation port 'POSITION)
	 => (lambda (operation)
	      (lambda (offset)
		(- (operation port) offset))))
	((input-port/operation port 'CHARS-REMAINING)
	 => (lambda (chars-rem)
	      (let ((len (input-port/operation port 'LENGTH)))
		(if (not len)
		    parser-current-position/default
		    (let ((total-length (len port)))
		      (lambda (offset)
			(- total-length
			   (+ (chars-rem port) offset))))))))
	(else
	 parser-current-position/default)))	 

(define (parser-associate-positions/default object position)
  position				; fnord
  object)

(define (parser-current-position/default offset)
  offset				; fnord
  false)

;; Do not integrate this!!! -- GJR

(define (recording-object-position offset parser)
  (let* ((position (*parser-current-position* offset))
	 (object (parser)))
    (*parser-associate-position* object position)
    object))

;;;; Symbols/Numbers

(define-accretor 0 (parse-object/atom)
  (build-atom (read-atom)))

(define-integrable (read-atom)
  (read-string char-set/atom-delimiters))

(define (build-atom string)
  (or (parse-number string)
      (intern-string! string)))

(define (parse-number string)
  (let ((radix (if (memv *parser-radix* '(2 8 10 16)) *parser-radix* 10)))
    (if (fix:= radix 10)
	(string->number string 10)
	(or (string->number string radix)
	    (begin
	      (if (string->number string 10)
		  (parse-error
		   "Radix-10 number syntax with non-standard radix:"
		   string))
	      #f)))))

(define *parser-canonicalize-symbols?*)

(define (intern-string! string)
  ;; Special version of `intern' to reduce consing and increase speed.
  (if *parser-canonicalize-symbols?*
      (substring-downcase! string 0 (string-length string)))
  (string->symbol string))

(define-accretor 0 (parse-object/symbol)
  (intern-string! (read-atom)))

(define-accretor 1 (parse-object/numeric-prefix)
  (let ((number
	 (let ((char (read-char)))
	   (string-append (string #\# char) (read-atom)))))
    (or (parse-number number)
	(parse-error "Bad number syntax" number))))

(define-accretor 1 (parse-object/bit-string)
  (discard-char)
  (let ((string (read-atom)))
    (let ((length (string-length string)))
      (unsigned-integer->bit-string
       length
       (let loop ((index 0) (result 0))
	 (if (< index length)
	     (loop (1+ index)
		   (+ (* result 2)
		      (case (string-ref string index)
			((#\0) 0)
			((#\1) 1)
			(else  (parse-error "Bad bit-string syntax"
					    (string-append "#*" string))))))
	     result))))))

;;;; Lists/Vectors

(define-accretor 0 (parse-object/list-open)
  (discard-char)
  (collect-list/top-level))

(define-accretor 1 (parse-object/vector-open)
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
  (list))

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
	(begin
	  (if (char=? #\| (peek-char))
	      (begin
		(discard-char)
		(loop)))
	  (loop)))))

;;;; Quoting

(define-accretor 0 (parse-object/quote)
  (discard-char)
  (list 'QUOTE (parse-object/dispatch)))

(define-accretor 0 (parse-object/quasiquote)
  (discard-char)
  (list 'QUASIQUOTE (parse-object/dispatch)))

(define-accretor 0 (parse-object/unquote)
  (discard-char)
  (if (char=? #\@ (peek-char))
      (begin
	(discard-char)
	(list 'UNQUOTE-SPLICING (parse-object/dispatch)))
      (list 'UNQUOTE (parse-object/dispatch))))


(define-accretor 0 (parse-object/string-quote)
  ;; This version uses a string output port to collect the string fragments
  ;; because string ports store the string efficiently and append the
  ;; string fragments in amortized linear time.
  ;;
  ;; The common case for a string with no escapes is handled efficiently by
  ;; lifting the code out of the loop.

  (discard-char)
  (let ((head (read-string char-set/string-delimiters)))
    (if (char=? #\" (read-char))
	head
	(call-with-output-string
	 (lambda (port)
	   (write-string head port)
	   (let loop ()
	     (let ((char
		    (let ((char (read-char)))
		      (cond ((char-ci=? char #\n) #\Newline)
			    ((char-ci=? char #\t) #\Tab)
			    ((char-ci=? char #\v) #\VT)
			    ((char-ci=? char #\b) #\BS)
			    ((char-ci=? char #\r) #\Return)
			    ((char-ci=? char #\f) #\Page)
			    ((char-ci=? char #\a) #\BEL)
			    ((char->digit char 8)
			     (let ((c2 (read-char)))
			       (octal->char char c2 (read-char))))
			    (else char)))))
	       (write-char char port)
	       (write-string (read-string char-set/string-delimiters) port)
	       (if (char=? #\\ (read-char))
		   (loop)))))))))

(define (octal->char c1 c2 c3)
  (let ((d1 (char->digit c1 8))
	(d2 (char->digit c2 8))
	(d3 (char->digit c3 8)))
    (if (not (and d1 d2 d3))
	(error "Badly formed octal string escape:" (string #\\ c1 c2 c3)))
    (let ((sum (+ (* #o100 d1) (* #o10 d2) d3)))
      (if (>= sum 256)
	  (error "Octal string escape exceeds ASCII range:"
		 (string #\\ c1 c2 c3)))
      (ascii->char sum))))

(define-accretor 1 (parse-object/char-quote)
  (discard-char)
  (if (char=? #\\ (peek-char))
      (read-char)
      (name->char
       (let loop ()
	 (cond ((char=? #\\ (peek-char))
		(discard-char)
		(string (read-char)))
	       ((char-set-member? char-set/char-delimiters (peek-char))
		(string (read-char)))
	       (else
		(let ((string (read-string char-set/char-delimiters)))
		  (if (let ((char (peek-char/eof-ok)))
			(and (not (eof-object? char))
			     (char=? #\- char)))
		      (begin
			(discard-char)
			(string-append string "-" (loop)))
		      string))))))))

;;;; Constants

(define-accretor 0 (parse-object/false)
  (discard-char)
  false)

(define-accretor 0 (parse-object/true)
  (discard-char)
  true)

(define-accretor 1 (parse-object/named-constant)
  (discard-char)
  (let ((object-name (parse-object/dispatch)))
    (cdr (or (assq object-name named-objects)
	     (parse-error "No object by this name" object-name)))))

(define named-objects)

(define (parse-unhash number)
  (if (not (exact-nonnegative-integer? number))
      (parse-error "Invalid unhash syntax" number))
  (let ((object (object-unhash number)))
    ;; This knows that 0 is the hash of #f.
    (if (and (false? object) (not (zero? number)))
	(parse-error "Invalid hash number" number))
    object))

(define-accretor 1 (parse-object/unhash)
  (discard-char)
  (let* ((number (parse-object/dispatch))
	 (object (parse-unhash number)))
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

(define-accretor 1 (parse-object/unhash-printed-representation)
  ;; #[fnord]
  ;; #[fnord-with-hash-number n ... ]
  (discard-char)
  (let* ((name   (parse-object/dispatch)))
    (discard-whitespace)
    (if (char=? #\] (peek-char))
	(begin
	  (read-char)
	  (parse-error "No hash number in #[" name)))
    (let* ((number (parse-object/dispatch))
	   (object (parse-unhash number)))
      ;; now gobble up crap until we find the #\]
      (let loop ()
	(discard-whitespace)
	(if (char=? #\] (peek-char))
	    (read-char)
	    (begin
	      (parse-object/dispatch)
	      (loop))))
      object)))

;;;; #<number>

(define (parse-object/special-prefix)
  (parse-special-prefix *parser-parse-object-special-table*))

(define (collect-list/special-prefix)
  (parse-special-prefix *parser-collect-list-special-table*))

(define (parse-special-prefix table)
  (set! *parser-current-special-prefix*
	(string->number (read-string char-set/non-digit) 10))
  ((vector-ref table (peek-ascii))))

;;;; #n= and #n#
;;;
;;;  The fluid variable *parser-cyclic-context* contains the context
;;;  (roughly read operation) in which the #n= and #n# references are
;;;  defined.  It is basically a table associating <n> with the
;;;  reference #<n>#.

(define *parser-cyclic-context* #f)

(define (parse-object/define-shared)
  (discard-char)
  (if (not *parser-current-special-prefix*)
      (parse-error
       "#= not allowed.  Circular structure syntax #<n>= requires <n>"))
  (let* ((index *parser-current-special-prefix*)
	 (ref   
	  (let ((ref (context/find-reference *parser-cyclic-context*
					     index)))
	    ;; The follwing test is not necessary unless we want
	    ;; to be CLtL compliant
	    (if ref
		(parse-error
		 "Cannot redefine circular structure label #<n>=, <n> ="
		 index))
	    (context/touch! *parser-cyclic-context*)
	    (context/define-reference *parser-cyclic-context* index)))
	 (text  (parse-object/dispatch)))
    (if (reference? text)
	(parse-error
	 (string-append
	  "#"  (number->string (reference/index ref))
	  "=#" (number->string (reference/index text))
	  "# not allowed.  Circular structure labels must not refer to labels."
	  )))
    (context/close-reference ref text)
    ref))

(define (parse-object/reference-shared)
  (discard-char)
  (if (not *parser-current-special-prefix*)
      (parse-error
       "## not allowed.  Circular structure syntax #<n># requires <n>"))
  (let* ((index  *parser-current-special-prefix*)
	 (ref    (context/find-reference *parser-cyclic-context* index)))
    (if ref
	(begin  (context/touch! *parser-cyclic-context*)
		ref)
	(parse-error
	 "Must define circular structure label #<n># before use: <n> ="
	 index))))

(define (cyclic-parser-post-edit datum)
  (if *parser-cyclic-context*
      (context/substitute-cycles *parser-cyclic-context* datum)
      datum))

;;;; Contexts and References

(define-structure
  (reference
   (conc-name reference/))
  index
  context
  text
  start-touch-count   ; number of #n? things seen when we saw this #n=
  end-touch-count     ; number of #n? things seen after finishing this one
                      ;  is #f if this is not yet finished
                      ; if difference=0 this one contains no references
  )

(define (reference/contains-references? ref)
  (not (eqv? (reference/start-touch-count ref)
	     (reference/end-touch-count ref))))

(define-structure
  (context
   (conc-name context/)
   (constructor %make-context))
  references        ; some kind of association number->reference
  touches           ; number of #n# or #n= things see so far
  )

(define (make-context)   (%make-context '() 0))

(define (context/touch! context)
  (set-context/touches! context  (fix:1+ (context/touches context))))

(define (context/define-reference context index)
  (let ((ref  (make-reference index
			      context
			      ()
			      (context/touches context)
			      #f)))
    
    (set-context/references!
     context
     (cons (cons index ref) (context/references context)))
    ref))

(define (context/close-reference ref text)
  (set-reference/end-touch-count! ref
				  (context/touches (reference/context ref)))
  (set-reference/text! ref text))

(define (context/find-reference context index)
  (let ((index.ref (assq index (context/references context))))
    (if index.ref (cdr index.ref) #f)))

;;;  SUBSTITUTE! traverses a tree, replacing all references by their text
;;;
;;;  This implementation assumes that #n= and #n# are THE ONLY source
;;;  of circularity, thus the objects given to SUBSTITUTE! are trees.

(define (substitute! thing)
  ;(display "[substitute!]")
  (cond ((pair? thing)    (substitute/pair! thing))
	((vector? thing)  (substitute/vector! thing))
	((%record? thing) (substitute/%record! thing))))

(define (substitute/pair! pair)
  (if (reference? (car pair))
      (set-car! pair (reference/text (car pair)))
      (substitute! (car pair)))
  (if (reference? (cdr pair))
      (set-cdr! pair (reference/text (cdr pair)))
      (substitute! (cdr pair))))

(define (substitute/vector! v)
  (let ((n (vector-length v)))
    (let loop ((i 0))
      (if (not (fix:= i n))
	  (let ((elt (vector-ref v i)))
	    (if (reference? elt)
		(vector-set! v i (reference/text elt))
		(substitute! elt))
	    (loop (fix:1+ i)))))))
	
(define (substitute/%record! r)
  ;; TEST THIS CODE
  (do ((i (fix:- (%record-length r) 1) (fix:- i 1)))
      ((fix:< i 0))
    (let ((elt (%record-ref r i)))
      (if (reference? elt)
	  (%record-set! r i (reference/text elt))
	  (substitute! elt)))))

(define (context/substitute-cycles context datum)
  (for-each (lambda (index.ref)
	      (let ((ref (cdr index.ref)))
		(if (reference/contains-references? ref)
		    (substitute! (reference/text ref)))))
	    (context/references context))
  (cond ((null? (context/references context))	 datum)
	((reference? datum)	                 (reference/text datum))
	(else  (substitute! datum)
	       datum)))