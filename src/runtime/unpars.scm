#| -*-Scheme-*-

Copyright (C) 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994,
    1995, 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005,
    2006, 2007, 2008 Massachusetts Institute of Technology

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

(define (initialize-package!)
  (set! string-delimiters
	(char-set-union char-set:not-graphic (char-set #\" #\\)))
  (set! hook/interned-symbol unparse-symbol)
  (set! hook/procedure-unparser #f)
  (set! *unparser-radix* 10)
  (set! *unparser-list-breadth-limit* #f)
  (set! *unparser-list-depth-limit* #f)
  (set! *unparser-string-length-limit* #f)
  (set! *unparse-primitives-by-name?* #f)
  (set! *unparse-uninterned-symbols-by-name?* #f)
  (set! *unparse-with-maximum-readability?* #f)
  (set! *unparse-compound-procedure-names?* #t)
  (set! *unparse-with-datum?* #f)
  (set! *unparse-abbreviate-quotations?* #f)
  (set! system-global-unparser-table (make-system-global-unparser-table))
  (set! *unparser-table* system-global-unparser-table)
  (set! *default-unparser-state* #f)
  (set! non-canon-symbol-quoted
	(char-set-union char-set/atom-delimiters
			char-set/symbol-quotes))
  (set! canon-symbol-quoted
	(char-set-union non-canon-symbol-quoted
			char-set:upper-case))
  unspecific)

(define *unparser-radix*)
(define *unparser-list-breadth-limit*)
(define *unparser-list-depth-limit*)
(define *unparser-string-length-limit*)
(define *unparse-primitives-by-name?*)
(define *unparse-uninterned-symbols-by-name?*)
(define *unparse-with-maximum-readability?*)
(define *unparse-compound-procedure-names?*)
(define *unparse-with-datum?*)
(define *unparse-abbreviate-quotations?*)
(define system-global-unparser-table)
(define *unparser-table*)
(define *default-unparser-state*)
(define non-canon-symbol-quoted)
(define canon-symbol-quoted)

(define (make-system-global-unparser-table)
  (let ((table (make-unparser-table unparse/default)))
    (for-each (lambda (entry)
		(unparser-table/set-entry! table (car entry) (cadr entry)))
	      `((BIGNUM ,unparse/number)
		(CHARACTER ,unparse/character)
		(COMPILED-ENTRY ,unparse/compiled-entry)
		(COMPLEX ,unparse/number)
		(CONSTANT ,unparse/constant)
		(ENTITY ,unparse/entity)
		(EXTENDED-PROCEDURE ,unparse/compound-procedure)
		(FLONUM ,unparse/flonum)
		(INTERNED-SYMBOL ,unparse/interned-symbol)
		(LIST ,unparse/pair)
		(NEGATIVE-FIXNUM ,unparse/number)
		(FALSE ,unparse/false)
		(POSITIVE-FIXNUM ,unparse/number)
		(PRIMITIVE ,unparse/primitive-procedure)
		(PROCEDURE ,unparse/compound-procedure)
		(RATNUM ,unparse/number)
		(RECORD ,unparse/record)
		(RETURN-ADDRESS ,unparse/return-address)
		(STRING ,unparse/string)
		(UNINTERNED-SYMBOL ,unparse/uninterned-symbol)
		(VARIABLE ,unparse/variable)
		(VECTOR ,unparse/vector)
		(VECTOR-1B ,unparse/bit-string)))
    table))

;;;; Unparser Table/State

(define-structure (unparser-table (constructor %make-unparser-table)
				  (conc-name unparser-table/))
  (dispatch-vector #f read-only #t))

(define-guarantee unparser-table "unparser table")

(define (make-unparser-table default-method)
  (%make-unparser-table
   (make-vector (microcode-type/code-limit) default-method)))

(define (unparser-table/copy table)
  (%make-unparser-table (unparser-table/dispatch-vector table)))

(define (unparser-table/entry table type-name)
  (vector-ref (unparser-table/dispatch-vector table)
	      (microcode-type type-name)))

(define (unparser-table/set-entry! table type-name method)
  (vector-set! (unparser-table/dispatch-vector table)
	       (microcode-type type-name)
	       method))

(define-structure (unparser-state (conc-name unparser-state/))
  (port #f read-only #t)
  (list-depth #f read-only #t)
  (slashify? #f read-only #t)
  (environment #f read-only #t))

(define-guarantee unparser-state "unparser state")

(define (with-current-unparser-state state procedure)
  (guarantee-unparser-state state 'WITH-CURRENT-UNPARSER-STATE)
  (fluid-let ((*default-unparser-state* state))
    (procedure (unparser-state/port state))))

;;;; Top Level

(define (unparse-char state char)
  (guarantee-unparser-state state 'UNPARSE-CHAR)
  (write-char char (unparser-state/port state)))

(define (unparse-string state string)
  (guarantee-unparser-state state 'UNPARSE-STRING)
  (write-string string (unparser-state/port state)))

(define (unparse-object state object)
  (guarantee-unparser-state state 'UNPARSE-OBJECT)
  (unparse-object/internal object
			   (unparser-state/port state)
			   (unparser-state/list-depth state)
			   (unparser-state/slashify? state)
			   (unparser-state/environment state)))

(define (unparse-object/top-level object port slashify? environment)
  (unparse-object/internal
   object
   port
   (if *default-unparser-state*
       (unparser-state/list-depth *default-unparser-state*)
       0)
   slashify?
   (if (or (default-object? environment)
	   (unparser-table? environment))
       (if *default-unparser-state*
	   (unparser-state/environment *default-unparser-state*)
	   (nearest-repl/environment))
       (begin
	 (guarantee-environment environment #f)
	 environment))))

(define (unparse-object/internal object port list-depth slashify? environment)
  (fluid-let ((*output-port* port)
	      (*list-depth* list-depth)
	      (*slashify?* slashify?)
	      (*environment* environment)
	      (*dispatch-table*
	       (unparser-table/dispatch-vector
		(let ((table
		       (environment-lookup environment '*UNPARSER-TABLE*)))
		  (guarantee-unparser-table table #f)
		  table))))
    (*unparse-object object)))

(define-integrable (invoke-user-method method object)
  (method (make-unparser-state *output-port*
			       *list-depth*
			       *slashify?*
			       *environment*)
	  object))

(define *list-depth*)
(define *slashify?*)
(define *environment*)
(define *dispatch-table*)

(define (*unparse-object object)
  ((vector-ref *dispatch-table*
	       ((ucode-primitive primitive-object-type 1) object))
   object))

;;;; Low Level Operations

(define *output-port*)

(define-integrable (*unparse-char char)
  (output-port/write-char *output-port* char))

(define-integrable (*unparse-string string)
  (output-port/write-string *output-port* string))

(define-integrable (*unparse-substring string start end)
  (output-port/write-substring *output-port* string start end))

(define-integrable (*unparse-datum object)
  (*unparse-hex (object-datum object)))

(define (*unparse-hex number)
  (*unparse-string "#x")
  (*unparse-string (number->string number 16)))

(define-integrable (*unparse-hash object)
  (*unparse-string (number->string (hash object))))

(define (*unparse-readable-hash object)
  (*unparse-string "#@")
  (*unparse-hash object))

(define (*unparse-with-brackets name object thunk)
  (if (and *unparse-with-maximum-readability?* object)
      (*unparse-readable-hash object)
      (begin
	(*unparse-string "#[")
	(if (string? name)
	    (*unparse-string name)
	    (*unparse-object name))
	(if object
	    (begin
	      (*unparse-char #\space)
	      (*unparse-hash object)))
	(if thunk
	    (begin
	      (*unparse-char #\space)
	      (thunk))
	    (if *unparse-with-datum?*
		(begin
		  (*unparse-char #\space)
		  (*unparse-datum object))))
	(*unparse-char #\]))))

;;;; Unparser Methods

(define (unparse/default object)
  (let ((type (user-object-type object)))
    (case (object-gc-type object)
      ((CELL PAIR TRIPLE QUADRUPLE VECTOR COMPILED-ENTRY)
       (*unparse-with-brackets type object #f))
      ((NON-POINTER)
       (*unparse-with-brackets type object
	 (lambda ()
	   (*unparse-datum object))))
      (else				;UNDEFINED, GC-INTERNAL
       (*unparse-with-brackets type #f
	 (lambda ()
	   (*unparse-datum object)))))))

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
    (EXTENDED-LAMBDA . LAMBDA)
    (COMBINATION-1 . COMBINATION)
    (COMBINATION-2 . COMBINATION)
    (PRIMITIVE-COMBINATION-0 . COMBINATION)
    (PRIMITIVE-COMBINATION-1 . COMBINATION)
    (PRIMITIVE-COMBINATION-2 . COMBINATION)
    (PRIMITIVE-COMBINATION-3 . COMBINATION)
    (SEQUENCE-2 . SEQUENCE)
    (SEQUENCE-3 . SEQUENCE)))

(define (unparse/false object)
  (if (eq? object #f)
      (*unparse-string "#f")
      (unparse/default object)))

(define (unparse/constant object)
  (cond ((null? object) (*unparse-string "()"))
	((eq? object #t) (*unparse-string "#t"))
	((default-object? object) (*unparse-string "#!default"))
	((eof-object? object) (*unparse-string "#!eof"))
	((eq? object lambda-aux-tag) (*unparse-string "#!aux"))
	((eq? object lambda-key-tag) (*unparse-string "#!key"))
	((eq? object lambda-optional-tag) (*unparse-string "#!optional"))
	((eq? object lambda-rest-tag) (*unparse-string "#!rest"))
	((eq? object unspecific) (*unparse-string "#!unspecific"))
	(else (unparse/default object))))

(define (unparse/return-address return-address)
  (*unparse-with-brackets 'RETURN-ADDRESS return-address
    (lambda ()
      (*unparse-object (return-address/name return-address)))))

(define (unparse/interned-symbol symbol)
  (hook/interned-symbol symbol))

(define hook/interned-symbol)

(define (unparse/uninterned-symbol symbol)
  (if *unparse-uninterned-symbols-by-name?*
      (unparse-symbol symbol)
      (*unparse-with-brackets 'UNINTERNED-SYMBOL symbol
	(lambda ()
	  (unparse-symbol symbol)))))

(define (unparse-symbol symbol)
  (let ((s (symbol-name symbol)))
    (if (or (string-find-next-char-in-set
	     s
	     (if (environment-lookup *environment*
				     '*PARSER-CANONICALIZE-SYMBOLS?*)
		 canon-symbol-quoted
		 non-canon-symbol-quoted))
	    (fix:= (string-length s) 0)
	    (and (char-set-member? char-set/number-leaders (string-ref s 0))
		 (string->number s)))
	(begin
	  (*unparse-char #\|)
	  (let ((end (string-length s)))
	    (let loop ((start 0))
	      (if (fix:< start end)
		  (let ((i
			 (substring-find-next-char-in-set
			  s start end
			  char-set/symbol-quotes)))
		    (if i
			(begin
			  (*unparse-substring s start i)
			  (*unparse-char #\\)
			  (*unparse-char (string-ref s i))
			  (loop (fix:+ i 1)))
			(*unparse-substring s start end))))))
	  (*unparse-char #\|))
	(*unparse-string s))))

(define (unparse/character character)
  (if (or *slashify?*
	  (not (char-ascii? character)))
      (begin
	(*unparse-string "#\\")
	(*unparse-string (char->name character #t)))
      (*unparse-char character)))

(define (unparse/string string)
  (if *slashify?*
      (let ((end (string-length string)))
	(let ((end*
	       (if *unparser-string-length-limit*
		   (min *unparser-string-length-limit* end)
		   end)))
	  (*unparse-char #\")
	  (if (substring-find-next-char-in-set string 0 end*
					       string-delimiters)
	      (let loop ((start 0))
		(let ((index
		       (substring-find-next-char-in-set string start end*
							string-delimiters)))
		  (if index
		      (begin
			(*unparse-substring string start index)
			(*unparse-char #\\)
			(let ((char (string-ref string index)))
			  (cond ((char=? char char:newline)
				 (*unparse-char #\n))
				((char=? char #\tab)
				 (*unparse-char #\t))
				((char=? char #\vt)
				 (*unparse-char #\v))
				((char=? char #\bs)
				 (*unparse-char #\b))
				((char=? char #\return)
				 (*unparse-char #\r))
				((char=? char #\page)
				 (*unparse-char #\f))
				((char=? char #\bel)
				 (*unparse-char #\a))
				((or (char=? char #\\)
				     (char=? char #\"))
				 (*unparse-char char))
				(else
				 (*unparse-string (char->octal char)))))
			(loop (+ index 1)))
		      (*unparse-substring string start end*))))
	      (*unparse-substring string 0 end*))
	  (if (< end* end)
	      (*unparse-string "..."))
	  (*unparse-char #\")))
      (*unparse-string string)))

(define (char->octal char)
  (let ((qr1 (integer-divide (char->ascii char) 8)))
    (let ((qr2 (integer-divide (integer-divide-quotient qr1) 8)))
      (string (digit->char (integer-divide-quotient qr2) 8)
	      (digit->char (integer-divide-remainder qr2) 8)
	      (digit->char (integer-divide-remainder qr1) 8)))))

(define string-delimiters)

(define (unparse/bit-string bit-string)
  (*unparse-string "#*")
  (let loop ((index (fix:- (bit-string-length bit-string) 1)))
    (if (fix:>= index 0)
	(begin
	  (*unparse-char (if (bit-string-ref bit-string index) #\1 #\0))
	  (loop (fix:- index 1))))))

(define (unparse/vector vector)
  (let ((method (unparse-vector/unparser vector)))
    (if method
	(invoke-user-method method vector)
	(unparse-vector/normal vector))))

(define (unparse-vector/unparser vector)
  (and (fix:> (vector-length vector) 0)
       (let ((tag (safe-vector-ref vector 0)))
	 (or (structure-tag/unparser-method tag 'VECTOR)
	     ;; Check the global tagging table too.
	     (unparser/tagged-vector-method tag)))))

(define (unparse-vector/normal vector)
  (limit-unparse-depth
   (lambda ()
     (let ((length (vector-length vector)))
       (if (fix:> length 0)
	   (begin
	     (*unparse-string "#(")
	     (*unparse-object (safe-vector-ref vector 0))
	     (let loop ((index 1))
	       (cond ((fix:= index length)
		      (*unparse-char #\)))
		     ((and *unparser-list-breadth-limit*
			   (>= index *unparser-list-breadth-limit*))
		      (*unparse-string " ...)"))
		     (else
		      (*unparse-char #\space)
		      (*unparse-object (safe-vector-ref vector index))
		      (loop (fix:+ index 1))))))
	   (*unparse-string "#()"))))))

(define (safe-vector-ref vector index)
  (if (with-absolutely-no-interrupts
       (lambda ()
	 (object-type? (ucode-type manifest-nm-vector)
		       (vector-ref vector index))))
      (error "Attempt to unparse partially marked vector."))
  (map-reference-trap (lambda () (vector-ref vector index))))

(define (unparse/record record)
  (if *unparse-with-maximum-readability?*
      (*unparse-readable-hash record)
      (invoke-user-method unparse-record record)))

(define (unparse/pair pair)
  (cond ((unparse-list/prefix-pair? pair)
	 => (lambda (prefix) (unparse-list/prefix-pair prefix pair)))
	((unparse-list/unparser pair)
	 => (lambda (method) (invoke-user-method method pair)))
	(else
	 (unparse-list pair))))

(define (unparse-list list)
  (limit-unparse-depth
   (lambda ()
     (*unparse-char #\()
     (*unparse-object (safe-car list))
     (unparse-tail (safe-cdr list) 2)
     (*unparse-char #\)))))

(define (limit-unparse-depth kernel)
  (if *unparser-list-depth-limit*
      (fluid-let ((*list-depth* (+ *list-depth* 1)))
	(if (> *list-depth* *unparser-list-depth-limit*)
	    (*unparse-string "...")
	    (kernel)))
      (kernel)))

(define (unparse-tail l n)
  (cond ((pair? l)
	 (let ((method (unparse-list/unparser l)))
	   (if method
	       (begin
		 (*unparse-string " . ")
		 (invoke-user-method method l))
	       (begin
		 (*unparse-char #\space)
		 (*unparse-object (safe-car l))
		 (if (and *unparser-list-breadth-limit*
			  (>= n *unparser-list-breadth-limit*)
			  (pair? (safe-cdr l)))
		     (*unparse-string " ...")
		     (unparse-tail (safe-cdr l) (+ n 1)))))))
	((not (null? l))
	 (*unparse-string " . ")
	 (*unparse-object l))))

(define (unparse-list/unparser pair)
  (let ((tag (safe-car pair)))
    (or (structure-tag/unparser-method tag 'LIST)
	;; Check the global tagging table too.
	(unparser/tagged-pair-method tag))))

(define (unparse-list/prefix-pair prefix pair)
  (*unparse-string prefix)
  (*unparse-object (safe-car (safe-cdr pair))))

(define (unparse-list/prefix-pair? object)
  (and *unparse-abbreviate-quotations?*
       (pair? (safe-cdr object))
       (null? (safe-cdr (safe-cdr object)))
       (case (safe-car object)
	 ((QUOTE) "'")
	 ((QUASIQUOTE) "`")
	 ((UNQUOTE) ",")
	 ((UNQUOTE-SPLICING) ",@")
	 (else #f))))

(define (safe-car pair)
  (map-reference-trap (lambda () (car pair))))

(define (safe-cdr pair)
  (map-reference-trap (lambda () (cdr pair))))

;;;; Procedures

(define hook/procedure-unparser)

(define (unparse-procedure procedure usual-method)
  (let ((method
	 (and hook/procedure-unparser
	      (hook/procedure-unparser procedure))))
    (cond (method (invoke-user-method method procedure))
	  ((generic-procedure? procedure)
	   (*unparse-with-brackets 'GENERIC-PROCEDURE procedure
	     (let ((name (generic-procedure-name procedure)))
	       (and name
		    (lambda () (*unparse-object name))))))
	  (else (usual-method)))))

(define (unparse/compound-procedure procedure)
  (unparse-procedure procedure
    (lambda ()
      (*unparse-with-brackets 'COMPOUND-PROCEDURE procedure
	(and *unparse-compound-procedure-names?*
	     (lambda-components* (procedure-lambda procedure)
	       (lambda (name required optional rest body)
		 required optional rest body
		 (and (not (eq? name lambda-tag:unnamed))
		      (lambda () (*unparse-object name))))))))))

(define (unparse/primitive-procedure procedure)
  (unparse-procedure procedure
    (lambda ()
      (let ((unparse-name
	     (lambda ()
	       (*unparse-object (primitive-procedure-name procedure)))))
	(cond (*unparse-primitives-by-name?*
	       (unparse-name))
	      (*unparse-with-maximum-readability?*
	       (*unparse-readable-hash procedure))
	      (else
	       (*unparse-with-brackets 'PRIMITIVE-PROCEDURE #f
		 unparse-name)))))))

(define (unparse/compiled-entry entry)
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
	      (lambda ()
		(let ((name (and procedure? (compiled-procedure/name entry))))
		  (with-values
		      (lambda () (compiled-entry/filename-and-index entry))
		    (lambda (filename block-number)
		      (*unparse-char #\()
		      (if name
			  (*unparse-string name))
		      (if filename
			  (begin
			    (if name
				(*unparse-char #\space))
			    (*unparse-object (pathname-name filename))
			    (if block-number
				(begin
				  (*unparse-char #\space)
				  (*unparse-hex block-number)))))
		      (*unparse-char #\)))))
		(*unparse-char #\space)
		(*unparse-hex (compiled-entry/offset entry))
		(if closure?
		    (begin
		      (*unparse-char #\space)
		      (*unparse-datum (compiled-closure->entry entry))))
		(*unparse-char #\space)
		(*unparse-datum entry))))))
    (if procedure?
	(unparse-procedure entry usual-method)
	(usual-method))))

;;;; Miscellaneous

(define (unparse/variable variable)
  (*unparse-with-brackets 'VARIABLE variable
    (lambda ()
      (*unparse-object (variable-name variable)))))

(define (unparse/number object)
  (*unparse-string
   (number->string
    object
    (let ((prefix
	   (lambda (prefix limit radix)
	     (if (exact-rational? object)
		 (begin
		   (if (not (and (exact-integer? object)
				 (< (abs object) limit)))
		       (*unparse-string prefix))
		   radix)
		 10))))
      (case *unparser-radix*
	((2) (prefix "#b" 2 2))
	((8) (prefix "#o" 8 8))
	((16) (prefix "#x" 10 16))
	(else 10))))))

(define (unparse/flonum flonum)
  (if (= (system-vector-length flonum) (system-vector-length 0.0))
      (unparse/number flonum)
      (unparse/floating-vector flonum)))

(define (unparse/floating-vector v)
  (let ((length ((ucode-primitive floating-vector-length) v)))
    (*unparse-with-brackets "floating-vector" v
      (and (not (zero? length))
	   (lambda ()
	     (let ((limit (if (not *unparser-list-breadth-limit*)
			      length
			      (min length *unparser-list-breadth-limit*))))
	       (unparse/flonum ((ucode-primitive floating-vector-ref) v 0))
	       (do ((i 1 (+ i 1)))
		   ((>= i limit))
		 (*unparse-char #\space)
		 (unparse/flonum ((ucode-primitive floating-vector-ref) v i)))
	       (if (< limit length)
		   (*unparse-string " ..."))))))))

(define (unparse/entity entity)

  (define (plain name)
    (*unparse-with-brackets name entity #f))

  (define (named-arity-dispatched-procedure name)
    (*unparse-with-brackets 'ARITY-DISPATCHED-PROCEDURE entity
      (lambda ()
	(*unparse-string name))))

  (cond ((continuation? entity)
	 (plain 'CONTINUATION))
	((apply-hook? entity)
	 (plain 'APPLY-HOOK))
	((arity-dispatched-procedure? entity)
	 (let ((proc  (entity-procedure entity)))
	   (cond ((and (compiled-code-address? proc)
		       (compiled-procedure? proc)
		       (compiled-procedure/name proc))
		  => named-arity-dispatched-procedure)
		 (else (plain 'ARITY-DISPATCHED-PROCEDURE)))))
	(else
	 (plain 'ENTITY))))