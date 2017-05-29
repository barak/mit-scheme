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

;;;; Unparser
;;; package: (runtime unparser)

(declare (usual-integrations))

(define *unparse-abbreviate-quotations?* #!default)
(define *unparse-compound-procedure-names?* #!default)
(define *unparse-primitives-by-name?* #!default)
(define *unparse-streams?* #!default)
(define *unparse-uninterned-symbols-by-name?* #!default)
(define *unparse-with-datum?* #!default)
(define *unparse-with-maximum-readability?* #!default)
(define *unparser-list-breadth-limit* #!default)
(define *unparser-list-depth-limit* #!default)
(define *unparser-radix* #!default)
(define *unparser-string-length-limit* #!default)

(define param:unparse-abbreviate-quotations?)
(define param:unparse-compound-procedure-names?)
(define param:unparse-primitives-by-name?)
(define param:unparse-streams?)
(define param:unparse-uninterned-symbols-by-name?)
(define param:unparse-with-datum?)
(define param:unparse-with-maximum-readability?)
(define param:unparser-list-breadth-limit)
(define param:unparser-list-depth-limit)
(define param:unparser-radix)
(define param:unparser-string-length-limit)
(define param:unparse-char-in-unicode-syntax?)

(add-boot-init!
 (lambda ()
   (set! param:unparse-abbreviate-quotations?
	 (make-unsettable-parameter #f
				    boolean-converter))
   (set! param:unparse-compound-procedure-names?
	 (make-unsettable-parameter #t
				    boolean-converter))
   (set! param:unparse-primitives-by-name?
	 (make-unsettable-parameter #f
				    boolean-converter))
   (set! param:unparse-streams?
	 (make-unsettable-parameter #t
				    boolean-converter))
   (set! param:unparse-uninterned-symbols-by-name?
	 (make-unsettable-parameter #f
				    boolean-converter))
   (set! param:unparse-with-datum?
	 (make-unsettable-parameter #f
				    boolean-converter))
   (set! param:unparse-with-maximum-readability?
	 (make-unsettable-parameter #f
				    boolean-converter))
   (set! param:unparser-list-breadth-limit
	 (make-unsettable-parameter #f
				    limit-converter))
   (set! param:unparser-list-depth-limit
	 (make-unsettable-parameter #f
				    limit-converter))
   (set! param:unparser-radix
	 (make-unsettable-parameter 10
				    radix-converter))
   (set! param:unparser-string-length-limit
	 (make-unsettable-parameter #f
				    limit-converter))
   (set! param:unparse-char-in-unicode-syntax?
	 (make-unsettable-parameter #f
				    boolean-converter))
   unspecific))

(define (boolean-converter value)
  (guarantee boolean? value))

(define (limit-converter value)
  (if value (guarantee exact-positive-integer? value))
  value)

(define (radix-converter value)
  (if (not (memv value '(2 8 10 16)))
      (error "Invalid unparser radix:" value))
  value)

(define (resolve-fluids param fluid)
  (if (default-object? fluid)
      (param)
      ((parameter-converter param) fluid)))

(define (get-param:unparse-abbreviate-quotations?)
  (resolve-fluids param:unparse-abbreviate-quotations?
		  *unparse-abbreviate-quotations?*))

(define (get-param:unparse-compound-procedure-names?)
  (resolve-fluids param:unparse-compound-procedure-names?
		  *unparse-compound-procedure-names?*))

(define (get-param:unparse-primitives-by-name?)
  (resolve-fluids param:unparse-primitives-by-name?
		  *unparse-primitives-by-name?*))

(define (get-param:unparse-streams?)
  (resolve-fluids param:unparse-streams?
		  *unparse-streams?*))

(define (get-param:unparse-uninterned-symbols-by-name?)
  (resolve-fluids param:unparse-uninterned-symbols-by-name?
		  *unparse-uninterned-symbols-by-name?*))

(define (get-param:unparse-with-datum?)
  (resolve-fluids param:unparse-with-datum?
		  *unparse-with-datum?*))

(define (get-param:unparse-with-maximum-readability?)
  (resolve-fluids param:unparse-with-maximum-readability?
		  *unparse-with-maximum-readability?*))

(define (get-param:unparser-list-breadth-limit)
  (resolve-fluids param:unparser-list-breadth-limit
		  *unparser-list-breadth-limit*))

(define (get-param:unparser-list-depth-limit)
  (resolve-fluids param:unparser-list-depth-limit
		  *unparser-list-depth-limit*))

(define (get-param:unparser-radix)
  (resolve-fluids param:unparser-radix
		  *unparser-radix*))

(define (get-param:unparser-string-length-limit)
  (resolve-fluids param:unparser-string-length-limit
		  *unparser-string-length-limit*))

(define-record-type <context>
    (make-context port mode environment list-depth in-brackets?
		  list-breadth-limit list-depth-limit)
    context?
  (port context-port)
  (mode context-mode)
  (environment context-environment)
  (list-depth context-list-depth)
  (in-brackets? context-in-brackets?)
  (list-breadth-limit context-list-breadth-limit)
  (list-depth-limit context-list-depth-limit))

(define (context-down-list context)
  (make-context (context-port context)
		(context-mode context)
		(context-environment context)
		(+ 1 (context-list-depth context))
		(context-in-brackets? context)
		(context-list-breadth-limit context)
		(context-list-depth-limit context)))

(define (context-in-brackets context)
  (make-context (context-port context)
		(context-mode context)
		(context-environment context)
		0
		#t
		within-brackets:list-breadth-limit
		within-brackets:list-depth-limit))

(define within-brackets:list-breadth-limit 5)
(define within-brackets:list-depth-limit 3)

(define (context-slashify? context)
  (eq? 'normal (context-mode context)))

(define (context-char-set context)
  (textual-port-char-set (context-port context)))

(define (make-unparser-state port list-depth slashify? environment)
  (guarantee output-port? port)
  (guarantee environment? environment)
  (guarantee exact-nonnegative-integer? list-depth)
  (make-context port
		(if slashify? 'normal 'display)
		environment
		list-depth
		#f
		(get-param:unparser-list-breadth-limit)
		(get-param:unparser-list-depth-limit)))

(define (with-current-unparser-state context procedure)
  (parameterize* (list (cons initial-context context))
    (lambda ()
      (procedure (context-port context)))))

(define initial-context)
(add-boot-init!
 (lambda ()
   (set! initial-context (make-unsettable-parameter #f))
   unspecific))

;;;; Top Level

(define (unparse-object/top-level object port slashify? environment)
  (guarantee output-port? port)
  (if (not (default-object? environment))
      (guarantee environment? environment))
  (*unparse-object object
		   (top-level-context port
				      (if slashify? 'normal 'display)
				      environment)))

(define (top-level-context port mode environment)
  (let ((context (initial-context)))
    (if context
	(make-context port
		      mode
		      (if (default-object? environment)
			  (context-environment context)
			  environment)
		      (context-list-depth context)
		      (context-in-brackets? context)
		      (context-list-breadth-limit context)
		      (context-list-depth-limit context))
	(make-context port
		      mode
		      (if (default-object? environment)
			  (nearest-repl/environment)
			  environment)
		      0
		      #f
		      (get-param:unparser-list-breadth-limit)
		      (get-param:unparser-list-depth-limit)))))

(define (unparser-mode? object)
  (or (eq? 'normal object)
      (eq? 'display object)))

(define (unparse-char context char)
  (guarantee context? context 'unparse-char)
  (write-char char (context-port context)))

(define (unparse-string context string)
  (guarantee context? context 'unparse-string)
  (write-string string (context-port context)))

(define (unparse-object context object)
  (guarantee context? context 'unparse-object)
  (*unparse-object object context))

(define (*unparse-object object context)
  ((vector-ref dispatch-table
               ((ucode-primitive primitive-object-type 1) object))
   object
   context))

(define-integrable (invoke-user-method method object context)
  (method context object))

(define dispatch-table)
(add-boot-init!
 (lambda ()
   (set! dispatch-table
	 (make-vector (microcode-type/code-limit) unparse/default))
   (for-each (lambda (entry)
	       (vector-set! dispatch-table
			    (microcode-type (car entry))
			    (cadr entry)))
	     `((ASSIGNMENT ,unparse/assignment)
	       (BIGNUM ,unparse/number)
	       (BYTEVECTOR ,unparse/bytevector)
	       (CHARACTER ,unparse/character)
	       (COMPILED-ENTRY ,unparse/compiled-entry)
	       (COMPLEX ,unparse/number)
	       (CONSTANT ,unparse/constant)
	       (DEFINITION ,unparse/definition)
	       (ENTITY ,unparse/entity)
	       (EXTENDED-PROCEDURE ,unparse/compound-procedure)
	       (FLONUM ,unparse/flonum)
	       (INTERNED-SYMBOL ,unparse/interned-symbol)
	       (LAMBDA ,unparse/lambda)
	       (LIST ,unparse/pair)
	       (NEGATIVE-FIXNUM ,unparse/number)
	       (FALSE ,unparse/false)
	       (POSITIVE-FIXNUM ,unparse/number)
	       (PRIMITIVE ,unparse/primitive-procedure)
	       (PROCEDURE ,unparse/compound-procedure)
	       (PROMISE ,unparse/promise)
	       (RATNUM ,unparse/number)
	       (RECORD ,unparse/record)
	       (RETURN-ADDRESS ,unparse/return-address)
	       (STRING ,unparse/string)
	       (TAGGED-OBJECT ,unparse/tagged-object)
	       (UNICODE-STRING ,unparse/string)
	       (UNINTERNED-SYMBOL ,unparse/uninterned-symbol)
	       (VARIABLE ,unparse/variable)
	       (VECTOR ,unparse/vector)
	       (VECTOR-1B ,unparse/bit-string)))))

;;;; Low Level Operations

(define-integrable (*unparse-char char context)
  (output-port/write-char (context-port context) char))

(define-integrable (*unparse-string string context)
  (output-port/write-string (context-port context) string))

(define-integrable (*unparse-substring string start end context)
  (output-port/write-substring (context-port context) string start end))

(define-integrable (*unparse-datum object context)
  (*unparse-hex (object-datum object) context))

(define (*unparse-hex number context)
  (*unparse-string "#x" context)
  (*unparse-string (number->string number 16) context))

(define-integrable (*unparse-hash object context)
  (*unparse-string (number->string (hash object)) context))

(define (*unparse-readable-hash object context)
  (*unparse-string "#@" context)
  (*unparse-hash object context))

(define (allowed-char? char context)
  (char-in-set? char (context-char-set context)))

(define (*unparse-with-brackets name object context procedure)
  (if (or (and (get-param:unparse-with-maximum-readability?) object)
          (context-in-brackets? context))
      (*unparse-readable-hash object context)
      (begin
	(*unparse-string "#[" context)
	(let ((context* (context-in-brackets context)))
	  (if (string? name)
	      (*unparse-string name context*)
	      (*unparse-object name context*))
	  (if object
	      (begin
		(*unparse-char #\space context*)
		(*unparse-hash object context*)))
	  (cond (procedure
		 (*unparse-char #\space context*)
		 (procedure context*))
		((get-param:unparse-with-datum?)
		 (*unparse-char #\space context*)
		 (*unparse-datum object context*))))
	(*unparse-char #\] context))))

;;;; Unparser Methods

(define (unparse/default object context)
  (let ((type (user-object-type object)))
    (case (object-gc-type object)
      ((CELL PAIR TRIPLE QUADRUPLE VECTOR COMPILED-ENTRY)
       (*unparse-with-brackets type object context #f))
      ((NON-POINTER)
       (*unparse-with-brackets type object context
         (lambda (context*)
           (*unparse-datum object context*))))
      (else                             ;UNDEFINED, GC-INTERNAL
       (*unparse-with-brackets type #f context
         (lambda (context*)
           (*unparse-datum object context*)))))))

(define (user-object-type object)
  (let ((type-code (object-type object)))
    (let ((type-name (microcode-type/code->name type-code)))
      (if type-name
          (rename-user-object-type type-name)
          (intern
           (string-append "undefined-type:" (number->string type-code)))))))

(define (rename-user-object-type type-name)
  (let ((entry (assq type-name renamed-user-object-types)))
    (if entry
        (cdr entry)
        type-name)))

(define renamed-user-object-types
  '((NEGATIVE-FIXNUM . NUMBER)
    (POSITIVE-FIXNUM . NUMBER)
    (BIGNUM . NUMBER)
    (FLONUM . NUMBER)
    (COMPLEX . NUMBER)
    (INTERNED-SYMBOL . SYMBOL)
    (UNINTERNED-SYMBOL . SYMBOL)
    (EXTENDED-PROCEDURE . PROCEDURE)
    (PRIMITIVE . PRIMITIVE-PROCEDURE)
    (LEXPR . LAMBDA)
    (EXTENDED-LAMBDA . LAMBDA)))

(define (unparse/false object context)
  (if (eq? object #f)
      (*unparse-string "#f" context)
      (unparse/default object context)))

(define (unparse/constant object context)
  (let ((string
	 (cond ((null? object) "()")
	       ((eq? object #t) "#t")
	       ((default-object? object) "#!default")
	       ((eof-object? object) "#!eof")
	       ((eq? object lambda-tag:aux) "#!aux")
	       ((eq? object lambda-tag:key) "#!key")
	       ((eq? object lambda-tag:optional) "#!optional")
	       ((eq? object lambda-tag:rest) "#!rest")
	       ((eq? object unspecific) "#!unspecific")
	       (else #f))))
    (if string
	(*unparse-string string context)
	(unparse/default object context))))

(define (unparse/interned-symbol symbol context)
  (unparse-symbol symbol context))

(define (unparse/uninterned-symbol symbol context)
  (if (get-param:unparse-uninterned-symbols-by-name?)
      (unparse-symbol symbol context)
      (*unparse-with-brackets 'UNINTERNED-SYMBOL symbol context
        (lambda (context*)
          (unparse-symbol symbol context*)))))

(define (unparse-symbol symbol context)
  (if (keyword? symbol)
      (unparse-keyword-name (keyword->string symbol) context)
      (unparse-symbol-name (symbol->string symbol) context)))

(define (unparse-keyword-name s context)
  (case (param:parser-keyword-style)
    ((PREFIX)
     (*unparse-char #\: context)
     (unparse-symbol-name s context))
    ((SUFFIX)
     (unparse-symbol-name s context)
     (*unparse-char #\: context))
    (else
     (*unparse-string "#[keyword " context)
     (unparse-symbol-name s context)
     (*unparse-char #\] context))))

(define (unparse-symbol-name s context)
  (if (and (fix:> (string-length s) 0)
	   (not (string=? s "."))
	   (not (string-prefix? "#" s))
	   (char-in-set? (string-ref s 0) char-set:symbol-initial)
	   (string-every (symbol-name-no-quoting-predicate context) s)
	   (not (case (param:parser-keyword-style)
		  ((PREFIX) (string-prefix? ":" s))
		  ((SUFFIX) (string-suffix? ":" s))
		  (else #f)))
	   (not (string->number s)))
      (*unparse-string s context)
      (begin
        (*unparse-char #\| context)
	(string-for-each (lambda (char)
			   (unparse-string-char char context))
			 s)
        (*unparse-char #\| context))))

(define (symbol-name-no-quoting-predicate context)
  (conjoin (char-set-predicate
	    (if (get-param:parser-fold-case?)
		char-set:folded-symbol-constituent
		char-set:symbol-constituent))
	   (lambda (char)
	     (allowed-char? char context))))

(define (unparse/character char context)
  (cond ((and (param:unparse-char-in-unicode-syntax?)
	      (bitless-char? char))
	 (*unparse-string "#\\u+" context)
	 (*unparse-string (number->string (char->integer char) 16) context))
	((context-slashify? context)
	 (*unparse-string "#\\" context)
	 (if (and (char-in-set? char char-set:normal-printing)
		  (not (eq? 'separator:space (char-general-category char)))
		  (allowed-char? char context))
	     (*unparse-char char context)
	     (*unparse-string (char->name char) context)))
	(else
	 (*unparse-char char context))))

(define (unparse/string string context)
  (if (context-slashify? context)
      (let* ((end (string-length string))
	     (end*
	      (let ((limit (get-param:unparser-string-length-limit)))
		(if limit
		    (min limit end)
		    end))))
          (*unparse-char #\" context)
	  (do ((index 0 (fix:+ index 1)))
	      ((not (fix:< index end*)))
	    (unparse-string-char (string-ref string index) context))
          (if (< end* end)
              (*unparse-string "..." context))
          (*unparse-char #\" context))
      (*unparse-string string context)))

(define (unparse-string-char char context)
  (case char
    ((#\bel)
     (*unparse-char #\\ context)
     (*unparse-char #\a context))
    ((#\bs)
     (*unparse-char #\\ context)
     (*unparse-char #\b context))
    ((#\newline)
     (*unparse-char #\\ context)
     (*unparse-char #\n context))
    ((#\return)
     (*unparse-char #\\ context)
     (*unparse-char #\r context))
    ((#\tab)
     (*unparse-char #\\ context)
     (*unparse-char #\t context))
    ((#\\ #\" #\|)
     (*unparse-char #\\ context)
     (*unparse-char char context))
    (else
     (if (and (char-in-set? char char-set:normal-printing)
	      (allowed-char? char context))
	 (*unparse-char char context)
	 (begin
	   (*unparse-char #\\ context)
	   (*unparse-char #\x context)
	   (*unparse-string (number->string (char->integer char) 16) context)
	   (*unparse-char #\; context))))))

(define (unparse/bit-string bit-string context)
  (*unparse-string "#*" context)
  (let loop ((index (fix:- (bit-string-length bit-string) 1)))
    (if (fix:>= index 0)
        (begin
          (*unparse-char (if (bit-string-ref bit-string index) #\1 #\0) context)
          (loop (fix:- index 1))))))

(define (unparse/vector vector context)
  (let ((method (unparse-vector/unparser vector)))
    (if method
        (invoke-user-method method vector context)
        (unparse-vector/normal vector context))))

(define (unparse-vector/unparser vector)
  (and (fix:> (vector-length vector) 0)
       (let ((tag (safe-vector-ref vector 0)))
         (or (structure-tag/unparser-method tag 'VECTOR)
             ;; Check the global tagging table too.
             (unparser/tagged-vector-method tag)))))

(define (unparse-vector/entity-unparser vector)
  (and (fix:> (vector-length vector) 0)
       (structure-tag/entity-unparser-method (safe-vector-ref vector 0)
                                             'VECTOR)))

(define (unparse-vector/normal vector context)
  (limit-unparse-depth context
    (lambda (context*)
      (let ((end (vector-length vector)))
	(if (fix:> end 0)
	    (begin
	      (*unparse-string "#(" context*)
	      (*unparse-object (safe-vector-ref vector 0) context*)
	      (let loop ((index 1))
		(if (fix:< index end)
		    (if (let ((limit (context-list-breadth-limit context*)))
			  (and limit
			       (>= index limit)))
			(*unparse-string " ...)" context*)
			(begin
			  (*unparse-char #\space context*)
			  (*unparse-object (safe-vector-ref vector index)
					   context*)
			  (loop (fix:+ index 1))))))
	      (*unparse-char #\) context*))
	    (*unparse-string "#()" context*))))))

(define (safe-vector-ref vector index)
  (if (with-absolutely-no-interrupts
       (lambda ()
         (object-type? (ucode-type manifest-nm-vector)
                       (vector-ref vector index))))
      (error "Attempt to unparse partially marked vector."))
  (map-reference-trap (lambda () (vector-ref vector index))))

(define (unparse/bytevector bytevector context)
  (limit-unparse-depth context
    (lambda (context*)
      (let ((end (bytevector-length bytevector)))
	(if (fix:> end 0)
	    (begin
	      (*unparse-string "#u8(" context*)
	      (*unparse-object (bytevector-u8-ref bytevector 0) context*)
	      (let loop ((index 1))
		(if (fix:< index end)
		    (if (let ((limit (get-param:unparser-list-breadth-limit)))
			  (and limit
			       (>= index limit)))
			(*unparse-string " ...)" context*)
			(begin
			  (*unparse-char #\space context*)
			  (*unparse-object (bytevector-u8-ref bytevector index)
					   context*)
			  (loop (fix:+ index 1))))))
	      (*unparse-char #\) context*))
	    (*unparse-string "#u8()" context*))))))

(define (unparse/record record context)
  (cond ((string? record) (unparse/string record context))
	((uri? record) (unparse/uri record context))
	((get-param:unparse-with-maximum-readability?)
	 (*unparse-readable-hash record context))
	(else (invoke-user-method unparse-record record context))))

(define (unparse/uri uri context)
  (*unparse-string "#<" context)
  (*unparse-string (uri->string uri) context)
  (*unparse-string ">" context))

(define (unparse/pair pair context)
  (cond ((unparse-list/prefix-pair? pair)
         => (lambda (prefix) (unparse-list/prefix-pair prefix pair context)))
        ((unparse-list/unparser pair)
         => (lambda (method) (invoke-user-method method pair context)))
        ((and (get-param:unparse-streams?) (stream-pair? pair))
         (unparse-list/stream-pair pair context))
        (else
         (unparse-list pair context))))

(define (unparse-list list context)
  (limit-unparse-depth context
    (lambda (context*)
      (*unparse-char #\( context*)
      (*unparse-object (safe-car list) context*)
      (unparse-tail (safe-cdr list) 2 context*)
      (*unparse-char #\) context*))))

(define (limit-unparse-depth context kernel)
  (let ((context* (context-down-list context))
	(limit (context-list-depth-limit context)))
    (if (and limit
	     (> (context-list-depth-limit context*) limit))
	(*unparse-string "..." context*)
	(kernel context*))))

(define (unparse-tail l n context)
  (cond ((pair? l)
         (let ((method (unparse-list/unparser l)))
           (if method
               (begin
                 (*unparse-string " . " context)
                 (invoke-user-method method l context))
               (begin
                 (*unparse-char #\space context)
                 (*unparse-object (safe-car l) context)
                 (if (let ((limit (context-list-breadth-limit context)))
                       (and limit
                            (>= n limit)
                            (pair? (safe-cdr l))))
                     (*unparse-string " ..." context)
                     (unparse-tail (safe-cdr l) (+ n 1) context))))))
        ((not (null? l))
         (*unparse-string " . " context)
         (*unparse-object l context))))

(define (unparse-list/unparser pair)
  (let ((tag (safe-car pair)))
    (or (structure-tag/unparser-method tag 'LIST)
        ;; Check the global tagging table too.
        (unparser/tagged-pair-method tag))))

(define (unparse-list/entity-unparser pair)
  (structure-tag/entity-unparser-method (safe-car pair) 'LIST))

(define (unparse-list/prefix-pair prefix pair context)
  (*unparse-string prefix context)
  (*unparse-object (safe-car (safe-cdr pair)) context))

(define (unparse-list/prefix-pair? object)
  (and (get-param:unparse-abbreviate-quotations?)
       (pair? (safe-cdr object))
       (null? (safe-cdr (safe-cdr object)))
       (case (safe-car object)
         ((QUOTE) "'")
         ((QUASIQUOTE) "`")
         ((UNQUOTE) ",")
         ((UNQUOTE-SPLICING) ",@")
         (else #f))))

(define (unparse-list/stream-pair stream-pair context)
  (limit-unparse-depth context
    (lambda (context*)
      (*unparse-char #\{ context*)
      (*unparse-object (safe-car stream-pair) context*)
      (unparse-stream-tail (safe-cdr stream-pair) 2 context*)
      (*unparse-char #\} context*))))

(define (unparse-stream-tail tail n context)
  (cond ((not (promise? tail))
         (*unparse-string " . " context)
         (*unparse-object tail context))
        ((not (promise-forced? tail))
         (*unparse-string " ..." context))
        (else
	 (let ((value (promise-value tail)))
	   (cond ((empty-stream? value))
		 ((stream-pair? value)
		  (*unparse-char #\space context)
		  (*unparse-object (safe-car value) context)
		  (if (let ((limit (context-list-breadth-limit context)))
			(and limit
			     (>= n limit)))
		      (*unparse-string " ..." context)
		      (unparse-stream-tail (safe-cdr value) (+ n 1) context)))
		 (else
		  (*unparse-string " . " context)
		  (*unparse-object value context)))))))

(define (safe-car pair)
  (map-reference-trap (lambda () (car pair))))

(define (safe-cdr pair)
  (map-reference-trap (lambda () (cdr pair))))

;;;; Procedures

(define (unparse-procedure procedure context usual-method)
  (if (generic-procedure? procedure)
      (*unparse-with-brackets 'GENERIC-PROCEDURE procedure context
	(let ((name (generic-procedure-name procedure)))
	  (and name
	       (lambda (context*)
		 (*unparse-object name context*)))))
      (usual-method)))

(define (unparse/compound-procedure procedure context)
  (unparse-procedure procedure context
    (lambda ()
      (*unparse-with-brackets 'COMPOUND-PROCEDURE procedure context
        (and (get-param:unparse-compound-procedure-names?)
             (lambda-components* (procedure-lambda procedure)
               (lambda (name required optional rest body)
                 required optional rest body
                 (and (not (eq? name lambda-tag:unnamed))
                      (lambda (context*)
			(*unparse-object name context*))))))))))

(define (unparse/primitive-procedure procedure context)
  (unparse-procedure procedure context
    (lambda ()
      (let ((unparse-name
             (lambda (context)
               (*unparse-object (primitive-procedure-name procedure) context))))
        (cond ((get-param:unparse-primitives-by-name?)
               (unparse-name context))
              ((get-param:unparse-with-maximum-readability?)
               (*unparse-readable-hash procedure context))
              (else
               (*unparse-with-brackets 'PRIMITIVE-PROCEDURE #f context
				       unparse-name)))))))

(define (unparse/compiled-entry entry context)
  (let* ((type (compiled-entry-type entry))
         (procedure? (eq? type 'COMPILED-PROCEDURE))
         (closure?
          (and procedure?
               (compiled-code-block/manifest-closure?
                (compiled-code-address->block entry))))
         (usual-method
          (lambda ()
            (*unparse-with-brackets (if closure? 'COMPILED-CLOSURE type)
                                    entry
				    context
              (lambda (context*)
                (let ((name (and procedure? (compiled-procedure/name entry))))
		  (receive (filename block-number)
		      (compiled-entry/filename-and-index entry)
		    (*unparse-char #\( context*)
		    (if name
			(*unparse-string name context*))
		    (if filename
			(begin
			  (if name
			      (*unparse-char #\space context*))
			  (*unparse-object (pathname-name filename) context*)
			  (if block-number
			      (begin
				(*unparse-char #\space context*)
				(*unparse-hex block-number context*)))))
		    (*unparse-char #\) context*)))
                (*unparse-char #\space context*)
                (*unparse-hex (compiled-entry/offset entry) context*)
                (if closure?
                    (begin
                      (*unparse-char #\space context*)
                      (*unparse-datum (compiled-closure->entry entry)
				      context*)))
                (*unparse-char #\space context*)
                (*unparse-datum entry context*))))))
    (if procedure?
        (unparse-procedure entry context usual-method)
        (usual-method))))

;;;; Miscellaneous

(define (unparse/return-address return-address context)
  (*unparse-with-brackets 'RETURN-ADDRESS return-address context
    (lambda (context*)
      (*unparse-object (return-address/name return-address) context*))))

(define (unparse/assignment assignment context)
  (*unparse-with-brackets 'ASSIGNMENT assignment context
    (lambda (context*)
      (*unparse-object (assignment-name assignment) context*))))

(define (unparse/definition definition context)
  (*unparse-with-brackets 'DEFINITION definition context
    (lambda (context*)
      (*unparse-object (definition-name definition) context*))))

(define (unparse/lambda lambda-object context)
  (*unparse-with-brackets 'LAMBDA lambda-object context
    (lambda (context*)
      (*unparse-object (lambda-name lambda-object) context*))))

(define (unparse/variable variable context)
  (*unparse-with-brackets 'VARIABLE variable context
    (lambda (context*)
      (*unparse-object (variable-name variable) context*))))

(define (unparse/number object context)
  (*unparse-string (number->string
		    object
		    (let ((prefix
			   (lambda (prefix limit radix)
			     (if (exact-rational? object)
				 (begin
				   (if (not (and (exact-integer? object)
						 (< (abs object) limit)))
				       (*unparse-string prefix context))
				   radix)
				 10))))
		      (case (get-param:unparser-radix)
			((2) (prefix "#b" 2 2))
			((8) (prefix "#o" 8 8))
			((16) (prefix "#x" 10 16))
			(else 10))))
		   context))

(define (unparse/flonum flonum context)
  (if (= (system-vector-length flonum) (system-vector-length 0.0))
      (unparse/number flonum context)
      (unparse/floating-vector flonum context)))

(define (unparse/floating-vector v context)
  (let ((length ((ucode-primitive floating-vector-length) v)))
    (*unparse-with-brackets "floating-vector" v context
      (and (not (zero? length))
           (lambda (context*)
             (let ((limit
		    (let ((limit (get-param:unparser-list-breadth-limit)))
		      (if limit
			  (min length limit)
			  length))))
               (unparse/flonum ((ucode-primitive floating-vector-ref) v 0)
			       context*)
               (do ((i 1 (+ i 1)))
                   ((>= i limit))
                 (*unparse-char #\space context*)
                 (unparse/flonum ((ucode-primitive floating-vector-ref) v i)
				 context*))
               (if (< limit length)
                   (*unparse-string " ..." context*))))))))

(define (unparse/entity entity context)

  (define (plain name)
    (*unparse-with-brackets name entity context #f))

  (define (named-arity-dispatched-procedure name)
    (*unparse-with-brackets 'ARITY-DISPATCHED-PROCEDURE entity context
      (lambda (context*)
        (*unparse-string name context*))))

  (cond ((continuation? entity)
         (plain 'CONTINUATION))
        ((apply-hook? entity)
         (plain 'APPLY-HOOK))
        ((arity-dispatched-procedure? entity)
         (let ((proc  (%entity-procedure entity)))
           (cond ((and (compiled-code-address? proc)
                       (compiled-procedure? proc)
                       (compiled-procedure/name proc))
                  => named-arity-dispatched-procedure)
                 (else (plain 'ARITY-DISPATCHED-PROCEDURE)))))
        ((get-param:unparse-with-maximum-readability?)
         (*unparse-readable-hash entity context))
        ((record? (%entity-extra entity))
         ;; Kludge to make the generic dispatch mechanism work.
         (invoke-user-method
          (lambda (state entity)
            ((record-entity-unparser (%entity-extra entity)) state entity))
          entity
	  context))
        ((or (and (vector? (%entity-extra entity))
                  (unparse-vector/entity-unparser (%entity-extra entity)))
             (and (pair? (%entity-extra entity))
                  (unparse-list/entity-unparser (%entity-extra entity))))
         => (lambda (method)
              (invoke-user-method method entity context)))
        (else (plain 'ENTITY))))

(define (unparse/promise promise context)
  (*unparse-with-brackets 'PROMISE promise context
    (if (promise-forced? promise)
	(lambda (context*)
	  (*unparse-string "(evaluated) " context*)
	  (*unparse-object (promise-value promise) context*))
	(lambda (context*)
	  (*unparse-string "(unevaluated)" context*)
	  (if (get-param:unparse-with-datum?)
	      (begin
		(*unparse-char #\space context*)
		(*unparse-datum promise context*)))))))

(define (unparse/tagged-object object context)
  (cond ((get-tagged-object-unparser-method object)
	 => (lambda (method)
	      (invoke-user-method method object context)))
	(else
	 (*unparse-with-brackets 'tagged-object object context
	   (lambda (context*)
	     (*unparse-object (tagged-object-tag object) context*))))))