#| -*-Scheme-*-

$Header: /Users/cph/tmp/foo/mit-scheme/mit-scheme/v7/src/runtime/unpars.scm,v 14.6 1988/10/15 17:19:29 cph Exp $

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
  (set! system-global-unparser-table (make-system-global-unparser-table))
  (set-current-unparser-table! system-global-unparser-table))

(define *unparser-radix*)
(define *unparser-list-breadth-limit*)
(define *unparser-list-depth-limit*)
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
  (fluid-let
      ((*output-port* port)
       (*unparse-char-operation* (output-port/operation/write-char port))
       (*unparse-string-operation* (output-port/operation/write-string port))
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
(define *unparse-char-operation*)
(define *unparse-string-operation*)

(define-integrable (*unparse-char char)
  (*unparse-char-operation* *output-port* char))

(define-integrable (*unparse-string string)
  (*unparse-string-operation* *output-port* string))

(define-integrable (*unparse-substring string start end)
  (*unparse-string (substring string start end)))

(define-integrable (*unparse-datum object)
  (*unparse-string (number->string (object-datum object) 16)))

(define-integrable (*unparse-hash object)
  (*unparse-string (number->string (hash object))))

(define (*unparse-with-brackets name object thunk)
  (*unparse-string "#[")
  (if (string? name)
      (*unparse-string name)
      (*unparse-object name))
  (if object
      (begin (*unparse-char #\Space)
	     (*unparse-hash object)))
  (if thunk
      (begin (*unparse-char #\Space)
	     (thunk)))
  (*unparse-char #\]))

;;;; Unparser Methods

(define (unparse/default object)
  (let ((type (user-object-type object)))
    (if (zero? (object-gc-type object))
	(*unparse-with-brackets type false
	  (lambda ()
	    (*unparse-datum object)))
	(*unparse-with-brackets type object false))))

(define (user-object-type object)
  (let ((type-code (object-type object)))
    (let ((type-name (microcode-type/code->name type-code)))
      (if type-name
	  (let ((entry (assq type-name renamed-user-object-types)))
	    (if entry (cdr entry) type-name))
	  (intern
	   (string-append "undefined-type:" (number->string type-code)))))))

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
  (*unparse-with-brackets 'UNINTERNED-SYMBOL
			  symbol
			  (lambda () (unparse-symbol symbol))))

(define (unparse-symbol symbol)
  (*unparse-string (symbol->string symbol)))

(define (unparse/character character)
  (if *slashify?*
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
  ((or (unparse-vector/unparser vector) unparse-vector/normal) vector))

(define (unparse-vector/unparser vector)
  (and (not (zero? (vector-length vector)))
       (let ((tag (safe-vector-ref vector 0)))
	 (and (not (future? tag))
	      (let ((method (unparser/tagged-vector-method tag)))
		(and method
		     (lambda (object)
		       (invoke-user-method method object))))))))

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
      (error "Attempt to unparse partially marked vector" 0))
  (vector-ref vector index))

(define (unparse/pair pair)
  ((or (unparse-list/unparser pair) unparse-list) pair))

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
	 (let ((unparser (unparse-list/unparser l)))
	   (if unparser
	       (begin (*unparse-string " . ")
		      (unparser l))
	       (begin (*unparse-char #\Space)
		      (*unparse-object (car l))
		      (if (and *unparser-list-breadth-limit*
			       (>= n *unparser-list-breadth-limit*)
			       (not (null? (cdr l))))
			  (*unparse-string " ...")
			  (unparse-tail (cdr l) (1+ n)))))))
	((not (null? l))
	 (*unparse-string " . ")
	 (*unparse-object l))))

(define (unparse-list/unparser object)
  (and (not (future? (car object)))
       (if (eq? (car object) 'QUOTE)
	   (and (pair? (cdr object))
		(null? (cddr object))
		unparse-quote-form)
	   (let ((method (unparser/tagged-pair-method (car object))))
	     (and method
		  (lambda (object)
		    (invoke-user-method method object)))))))

(define (unparse-quote-form pair)
  (*unparse-char #\')
  (*unparse-object (cadr pair)))

;;;; Procedures and Environments

(define (unparse/compound-procedure procedure)
  (*unparse-with-brackets 'COMPOUND-PROCEDURE procedure
    (lambda-components* (procedure-lambda procedure)
      (lambda (name required optional rest body)
	required optional rest body
	(and (not (eq? name lambda-tag:unnamed))
	     (lambda () (*unparse-object name)))))))

(define (unparse/primitive-procedure procedure)
  (*unparse-with-brackets 'PRIMITIVE-PROCEDURE false
    (lambda ()
      (*unparse-object (primitive-procedure-name procedure)))))

;;;; Compiled entries

(define (unparse/compiled-entry entry)
  (discriminate-compiled-entry entry
    (lambda () (unparse-compiled-procedure entry))
    (lambda () (unparse-compiled-entry entry))
    (lambda () (unparse-compiled-entry entry))
    (lambda () (unparse-compiled-entry entry))))

(define (unparse-compiled-procedure entry)
  ;; Gross-out to make the "FASLoading" message not print out in the
  ;; middle of the other stuff.
  (let ((unparse-it
	 (lambda (thunk)
	   (*unparse-with-brackets 'COMPILED-PROCEDURE entry thunk))))
    (compiled-entry->name entry
      (lambda (string)
	(unparse-it
	 (lambda ()
	   (*unparse-string (detach-suffix-number string)))))
      (lambda ()
	(compiled-entry->pathname entry
	  (lambda (pathname)
	    (unparse-it 
	     (lambda () 
	       (*unparse-string "from file ")
	       (*unparse-object (pathname-name pathname)))))
	  (lambda ()
	    (unparse-it
	     (lambda () 
	       (*unparse-datum entry)))))))))

(define (unparse-compiled-entry entry)
  (let ((unparse-it
	 (lambda (thunk)
	   (*unparse-with-brackets (compiled-entry-type entry) entry thunk))))
    (compiled-entry->pathname entry
      (lambda (pathname)
	(unparse-it 
	 (lambda ()
	   (*unparse-string "from file ")
	   (*unparse-object (pathname-name pathname)))))
      (lambda () 
	(unparse-it 
	 (lambda () (*unparse-datum entry)))))))

;;; Names in the symbol table are of the form "FOOBAR-127".  The 127
;;; is added by the compiler.  This procedure detaches the suffix
;;; number from the prefix name.  It does nothing if there is no
;;; numeric suffix.

(define (detach-suffix-number string)
  (let loop ((index (-1+ (string-length string))))
    (cond ((zero? index) string)
	  ((char=? (string-ref string index) #\-)
	   (string-append
	    (substring string 0 index)
	    " "
	    (substring string (1+ index) (string-length string))))
	  ((char-numeric? (string-ref string index))
	   (loop (-1+ index)))
	  (else string))))

;;;; Miscellaneous

(define (unparse/environment environment)
  (if (lexical-unreferenceable? environment ':PRINT-SELF)
      (unparse/default environment)
      ((lexical-reference environment ':PRINT-SELF))))

(define (unparse/variable variable)
  (*unparse-with-brackets 'VARIABLE variable
    (lambda () (*unparse-object (variable-name variable)))))

(define (unparse/number object)
  (*unparse-string (number->string object *unparser-radix*)))
(define (unparse/future future)
  (*unparse-with-brackets 'FUTURE false
    (lambda ()
      (*unparse-string
       (number->string ((ucode-primitive primitive-object-datum 1) future)
		       16)))))

(define (unparse/entity entity)
  (*unparse-with-brackets (if (continuation? entity) 'CONTINUATION 'ENTITY)
			  entity
			  false))