#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unpars.scm,v 14.18 1990/09/19 00:34:16 cph Rel $

Copyright (c) 1988, 1989, 1990 Massachusetts Institute of Technology

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

;;;; Unparser
;;; package: (runtime unparser)

(declare (usual-integrations))

(define (initialize-package!)
  (set! string-delimiters
	(char-set-union char-set:not-graphic (char-set #\" #\\)))
  (set! hook/interned-symbol unparse-symbol)
  (set! *unparser-radix* 10)
  (set! *unparser-list-breadth-limit* false)
  (set! *unparser-list-depth-limit* false)
  (set! *unparse-primitives-by-name?* false)
  (set! *unparse-uninterned-symbols-by-name?* false)
  (set! *unparse-with-maximum-readability?* false)
  (set! system-global-unparser-table (make-system-global-unparser-table))
  (set-current-unparser-table! system-global-unparser-table))

(define *unparser-radix*)
(define *unparser-list-breadth-limit*)
(define *unparser-list-depth-limit*)
(define *unparse-primitives-by-name?*)
(define *unparse-uninterned-symbols-by-name?*)
(define *unparse-with-maximum-readability?*)
(define system-global-unparser-table)
(define *current-unparser-table*)

(define (current-unparser-table)
  *current-unparser-table*)

(define (set-current-unparser-table! table)
  (guarantee-unparser-table table)
  (set! *current-unparser-table* table))

(define (make-system-global-unparser-table)
  (let ((table (make-unparser-table unparse/default)))
    (for-each (lambda (entry)
		(unparser-table/set-entry! table (car entry) (cadr entry)))
	      `((BIGNUM ,unparse/number)
		(CHARACTER ,unparse/character)
		(COMPILED-ENTRY ,unparse/compiled-entry)
		(COMPLEX ,unparse/number)
		(ENTITY ,unparse/entity)
		(ENVIRONMENT ,unparse/environment)
		(EXTENDED-PROCEDURE ,unparse/compound-procedure)
		(FIXNUM ,unparse/number)
		(FLONUM ,unparse/number)
		(FUTURE ,unparse/future)
		(INTERNED-SYMBOL ,unparse/interned-symbol)
		(LIST ,unparse/pair)
		(NULL ,unparse/null)
		(PRIMITIVE ,unparse/primitive-procedure)
		(PROCEDURE ,unparse/compound-procedure)
		(RATNUM ,unparse/number)
		(RETURN-ADDRESS ,unparse/return-address)
		(STRING ,unparse/string)
		(TRUE ,unparse/true)
		(UNINTERNED-SYMBOL ,unparse/uninterned-symbol)
		(VARIABLE ,unparse/variable)
		(VECTOR ,unparse/vector)
		(VECTOR-1B ,unparse/bit-string)))
    table))

;;;; Unparser Table/State

(define-structure (unparser-table (constructor %make-unparser-table)
				  (conc-name unparser-table/))
  (dispatch-vector false read-only true))

(define (guarantee-unparser-table table)
  (if (not (unparser-table? table)) (error "Bad unparser table" table))
  table)

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
  (port false read-only true)
  (list-depth false read-only true)
  (slashify? false read-only true)
  (unparser-table false read-only true))

(define (guarantee-unparser-state state)
  (if (not (unparser-state? state)) (error "Bad unparser state" state))
  state)

;;;; Top Level

(define (unparse-char state char)
  (guarantee-unparser-state state)
  (write-char char (unparser-state/port state)))

(define (unparse-string state string)
  (guarantee-unparser-state state)
  (write-string string (unparser-state/port state)))

(define (unparse-object state object)
  (guarantee-unparser-state state)
  (unparse-object/internal object
			   (unparser-state/port state)
			   (unparser-state/list-depth state)
			   (unparser-state/slashify? state)
			   (unparser-state/unparser-table state)))

(define (unparse-object/internal object port list-depth slashify? table)
  (fluid-let ((*output-port* port)
	      (*list-depth* list-depth)
	      (*slashify?* slashify?)
	      (*unparser-table* table)
	      (*dispatch-vector* (unparser-table/dispatch-vector table)))
    (*unparse-object object)))

(define-integrable (invoke-user-method method object)
  (method (make-unparser-state *output-port*
			       *list-depth*
			       *slashify?*
			       *unparser-table*)
	  object))

(define *list-depth*)
(define *slashify?*)
(define *unparser-table*)
(define *dispatch-vector*)

(define (*unparse-object object)
  ((vector-ref *dispatch-vector*
	       ((ucode-primitive primitive-object-type 1) object))
   object))

;;;; Low Level Operations

(define *output-port*)

(define-integrable (*unparse-char char)
  (output-port/write-char *output-port* char))

(define-integrable (*unparse-string string)
  (output-port/write-string *output-port* string))

(define-integrable (*unparse-substring string start end)
  (*unparse-string (substring string start end)))

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
	      (*unparse-char #\Space)
	      (*unparse-hash object)))
	(if thunk
	    (begin
	      (*unparse-char #\Space)
	      (thunk)))
	(*unparse-char #\]))))

;;;; Unparser Methods

(define (unparse/default object)
  (let ((type (user-object-type object)))
    (case ((ucode-primitive primitive-object-gc-type 1) object)
      ((1 2 3 4 -3 -4)		; cell pair triple quad vector compiled
       (*unparse-with-brackets type object false))
      ((0)			; non pointer
       (*unparse-with-brackets type object
	 (lambda ()
	   (*unparse-datum object))))
      (else			; undefined, gc special
       (*unparse-with-brackets type false
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
  '((FIXNUM . NUMBER)
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

(define (unparse/null object)
  (cond ((eq? object '()) (*unparse-string "()"))
	((eq? object #F) (*unparse-string "#F"))
	(else (unparse/default object))))

(define (unparse/true object)
  (cond ((eq? object true) (*unparse-string "#T"))
	((undefined-value? object) (*unparse-string "#[undefined-value]"))
	(else (unparse/default object))))

(define (unparse/return-address return-address)
  (*unparse-with-brackets 'RETURN-ADDRESS return-address
    (lambda ()
      (*unparse-object (return-address/name return-address)))))

(define (unparse/interned-symbol symbol)
  (hook/interned-symbol symbol))

(define hook/interned-symbol)

(define (unparse/uninterned-symbol symbol)
  (let ((unparse-symbol (lambda () (unparse-symbol symbol))))
    (if *unparse-uninterned-symbols-by-name?*
	(unparse-symbol)
	(*unparse-with-brackets 'UNINTERNED-SYMBOL symbol unparse-symbol))))

(define (unparse-symbol symbol)
  (*unparse-string (symbol->string symbol)))

(define (unparse/character character)
  (if (or *slashify?*
	  (not (char-ascii? character)))
      (begin (*unparse-string "#\\")
	     (*unparse-string (char->name character true)))
      (*unparse-char character)))

(define (unparse/string string)
  (cond ((char-set? string)
	 (*unparse-with-brackets 'CHARACTER-SET string false))
	(*slashify?*
	 (*unparse-char #\")
	 (let ((end (string-length string)))
	   (define (loop start)
	     (let ((index
		    (substring-find-next-char-in-set string start end
						     string-delimiters)))
	       (if index
		   (begin (*unparse-substring string start index)
			  (*unparse-char #\\)
			  (let ((char (string-ref string index)))
			    (cond ((char=? char #\Tab)
				   (*unparse-char #\t))
				  ((char=? char char:newline)
				   (*unparse-char #\n))
				  ((char=? char #\Page)
				   (*unparse-char #\f))
				  ((or (char=? char #\\)
				       (char=? char #\"))
				   (*unparse-char char))
				  (else
				   (*unparse-string (char->octal char)))))
			  (loop (1+ index)))
		   (*unparse-substring string start end))))
	   (if (substring-find-next-char-in-set string 0 end
						string-delimiters)
	       (loop 0)
	       (*unparse-string string)))
	 (*unparse-char #\"))
	(else
	 (*unparse-string string))))

(define (char->octal char)
  (let ((qr1 (integer-divide (char->ascii char) 8)))
    (let ((qr2 (integer-divide (integer-divide-quotient qr1) 8)))
      (string (digit->char (integer-divide-quotient qr2) 8)
	      (digit->char (integer-divide-remainder qr2) 8)
	      (digit->char (integer-divide-remainder qr1) 8)))))

(define string-delimiters)

(define (unparse/bit-string bit-string)
  (*unparse-string "#*")
  (let loop ((index (-1+ (bit-string-length bit-string))))
    (if (not (negative? index))
	(begin (*unparse-char (if (bit-string-ref bit-string index) #\1 #\0))
	       (loop (-1+ index))))))

(define (unparse/vector vector)
  (let ((method (unparse-vector/unparser vector)))
    (if method
	(invoke-user-method method vector)
	(unparse-vector/normal vector))))

(define (unparse-vector/unparser vector)
  (and (not (zero? (vector-length vector)))
       (unparser/tagged-vector-method (safe-vector-ref vector 0))))

(define (unparse-vector/normal vector)
  (limit-unparse-depth
   (lambda ()
     (let ((length (vector-length vector)))
       (if (zero? length)
	   (*unparse-string "#()")
	   (begin
	     (*unparse-string "#(")
	     (*unparse-object (safe-vector-ref vector 0))
	     (let loop ((index 1))
	       (cond ((= index length)
		      (*unparse-char #\)))
		     ((and *unparser-list-breadth-limit*
			   (>= index *unparser-list-breadth-limit*))
		      (*unparse-string " ...)"))
		     (else
		      (*unparse-char #\Space)
		      (*unparse-object (safe-vector-ref vector index))
		      (loop (1+ index)))))))))))

(define (safe-vector-ref vector index)
  (if (with-absolutely-no-interrupts
       (lambda ()
	 (or (object-type? (ucode-type manifest-nm-vector)
			   (vector-ref vector index))
	     (object-type? (ucode-type manifest-special-nm-vector)
			   (vector-ref vector index)))))
      (error "Attempt to unparse partially marked vector"))
  (vector-ref vector index))

(define (unparse/pair pair)
  (let ((prefix (unparse-list/prefix-pair? pair)))
    (if prefix
	(unparse-list/prefix-pair prefix pair)
	(let ((method (unparse-list/unparser pair)))
	  (if method
	      (invoke-user-method method pair)
	      (unparse-list pair))))))

(define (unparse-list list)
  (limit-unparse-depth
   (lambda ()
     (*unparse-char #\()
     (*unparse-object (car list))
     (unparse-tail (cdr list) 2)
     (*unparse-char #\)))))

(define (limit-unparse-depth kernel)
  (if *unparser-list-depth-limit*
      (fluid-let ((*list-depth* (1+ *list-depth*)))
	(if (> *list-depth* *unparser-list-depth-limit*)
	    (*unparse-string "...")
	    (kernel)))
      (kernel)))

(define (unparse-tail l n)
  (cond ((pair? l)
	 (let ((prefix (unparse-list/prefix-pair? l)))
	   (if prefix
	       (unparse-list/prefix-pair prefix l)
	       (let ((method (unparse-list/unparser l)))
		 (if method
		     (begin
		       (*unparse-string " . ")
		       (invoke-user-method method l))
		     (begin
		       (*unparse-char #\space)
		       (*unparse-object (car l))
		       (if (and *unparser-list-breadth-limit*
				(>= n *unparser-list-breadth-limit*)
				(not (null? (cdr l))))
			   (*unparse-string " ...")
			   (unparse-tail (cdr l) (1+ n)))))))))
	((not (null? l))
	 (*unparse-string " . ")
	 (*unparse-object l))))

(define-integrable (unparse-list/unparser object)
  (unparser/tagged-pair-method (car object)))

(define (unparse-list/prefix-pair prefix pair)
  (*unparse-string prefix)
  (*unparse-object (cadr pair)))

(define (unparse-list/prefix-pair? object)
  (and (not (future? (car object)))
       (pair? (cdr object))
       (null? (cddr object))
       (case (car object)
	 ((QUOTE) "'")
	 ((QUASIQUOTE) "`")
	 ((UNQUOTE) ",")
	 ((UNQUOTE-SPLICING) ",@")
	 (else false))))

;;;; Procedures and Environments

(define (unparse/compound-procedure procedure)
  (*unparse-with-brackets 'COMPOUND-PROCEDURE procedure
    (lambda-components* (procedure-lambda procedure)
      (lambda (name required optional rest body)
	required optional rest body
	(and (not (eq? name lambda-tag:unnamed))
	     (lambda () (*unparse-object name)))))))

(define (unparse/primitive-procedure procedure)
  (let ((unparse-name
	 (lambda ()
	   (*unparse-object (primitive-procedure-name procedure)))))
    (cond (*unparse-primitives-by-name?*
	   (unparse-name))
	  (*unparse-with-maximum-readability?*
	   (*unparse-readable-hash procedure))
	  (else
	   (*unparse-with-brackets 'PRIMITIVE-PROCEDURE false unparse-name)))))

(define (unparse/compiled-entry entry)
  (let* ((type (compiled-entry-type entry))
	 (closure?
	  (and (eq? type 'COMPILED-PROCEDURE)
	       (compiled-code-block/manifest-closure?
		(compiled-code-address->block entry)))))
    (*unparse-with-brackets
     (if closure? 'COMPILED-CLOSURE type)
     entry
     (lambda ()
       (let ((name
	      (and (eq? type 'COMPILED-PROCEDURE)
		   (compiled-procedure/name entry))))
	 (with-values (lambda () (compiled-entry/filename entry))
	   (lambda (filename block-number)
	     (*unparse-char #\()
	     (if name
		 (*unparse-string name))
	     (if filename
		 (begin
		   (if name
		       (*unparse-char #\Space))
		   (*unparse-object (pathname-name (->pathname filename)))
		   (if block-number
		       (begin
			 (*unparse-char #\Space)
			 (*unparse-hex block-number)))))
	     (*unparse-char #\)))))
       (*unparse-char #\Space)
       (*unparse-hex (compiled-entry/offset entry))
       (*unparse-char #\Space)
       (if closure?
	   (begin (*unparse-datum (compiled-closure->entry entry))
		  (*unparse-char #\Space)))
       (*unparse-datum entry)))))

;;;; Miscellaneous

(define (unparse/environment environment)
  (if (lexical-unreferenceable? environment ':PRINT-SELF)
      (unparse/default environment)
      ((lexical-reference environment ':PRINT-SELF))))

(define (unparse/variable variable)
  (*unparse-with-brackets 'VARIABLE variable
    (lambda () (*unparse-object (variable-name variable)))))

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

(define (unparse/future future)
  (*unparse-with-brackets 'FUTURE false
    (lambda ()
      (*unparse-hex ((ucode-primitive primitive-object-datum 1) future)))))

(define (unparse/entity entity)
  (*unparse-with-brackets (cond ((continuation? entity) 'CONTINUATION)
				((apply-hook? entity) 'APPLY-HOOK)
				(else 'ENTITY))
			  entity
			  false))