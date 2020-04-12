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

;;;; Scheme Printer
;;; package: (runtime printer)

(declare (usual-integrations))

(add-boot-deps! '(runtime microcode-tables)
		'(runtime dynamic)
		'(runtime predicate-dispatch))

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

(define-deferred param:print-char-in-unicode-syntax?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:print-compound-procedure-names?
  (make-unsettable-parameter #t boolean-converter))

(define-deferred param:print-primitives-by-name?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:print-streams?
  (make-unsettable-parameter #t boolean-converter))

(define-deferred param:print-uninterned-symbols-by-name?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:print-with-datum?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:print-with-maximum-readability?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:printer-abbreviate-quotations?
  (make-unsettable-parameter #f boolean-converter))

(define-deferred param:print-hash-number-in-objects?
  (make-settable-parameter #t boolean-converter))

(define-deferred param:printer-list-breadth-limit
  (make-unsettable-parameter #f limit-converter))

(define-deferred param:printer-list-depth-limit
  (make-unsettable-parameter #f limit-converter))

(define-deferred param:printer-radix
  (make-unsettable-parameter 10 radix-converter))

(define-deferred param:printer-string-length-limit
  (make-unsettable-parameter #f limit-converter))

(define (boolean-converter value)
  (guarantee boolean? value))

(define (limit-converter value)
  (if value (guarantee exact-positive-integer? value))
  value)

(define (radix-converter value)
  (if (not (memv value '(2 8 10 16)))
      (error "Invalid printer radix:" value))
  value)

(define (resolve-fluids param fluid)
  (if (default-object? fluid)
      (param)
      ((parameter-converter param) fluid)))

(define (get-param:print-compound-procedure-names?)
  (resolve-fluids param:print-compound-procedure-names?
		  *unparse-compound-procedure-names?*))

(define (get-param:print-primitives-by-name?)
  (resolve-fluids param:print-primitives-by-name?
		  *unparse-primitives-by-name?*))

(define (get-param:print-streams?)
  (resolve-fluids param:print-streams?
		  *unparse-streams?*))

(define (get-param:print-uninterned-symbols-by-name?)
  (resolve-fluids param:print-uninterned-symbols-by-name?
		  *unparse-uninterned-symbols-by-name?*))

(define (get-param:print-with-datum?)
  (resolve-fluids param:print-with-datum?
		  *unparse-with-datum?*))

(define (get-param:print-with-maximum-readability?)
  (resolve-fluids param:print-with-maximum-readability?
		  *unparse-with-maximum-readability?*))

(define (get-param:printer-abbreviate-quotations?)
  (resolve-fluids param:printer-abbreviate-quotations?
		  *unparse-abbreviate-quotations?*))

(define (get-param:printer-list-breadth-limit)
  (resolve-fluids param:printer-list-breadth-limit
		  *unparser-list-breadth-limit*))

(define (get-param:printer-list-depth-limit)
  (resolve-fluids param:printer-list-depth-limit
		  *unparser-list-depth-limit*))

(define (get-param:printer-radix)
  (resolve-fluids param:printer-radix
		  *unparser-radix*))

(define (get-param:printer-string-length-limit)
  (resolve-fluids param:printer-string-length-limit
		  *unparser-string-length-limit*))

(define-record-type <context>
    (make-context port mode list-depth in-brackets? labeling
		  list-breadth-limit list-depth-limit)
    context?
  (port context-port)
  (mode context-mode)
  (list-depth context-list-depth)
  (in-brackets? context-in-brackets?)
  (labeling context-labeling)
  (list-breadth-limit context-list-breadth-limit)
  (list-depth-limit context-list-depth-limit))

(define (context-down-list context)
  (make-context (context-port context)
		(context-mode context)
		(+ 1 (context-list-depth context))
		(context-in-brackets? context)
		(context-labeling context)
		(context-list-breadth-limit context)
		(context-list-depth-limit context)))

(define (context-in-brackets context)
  (make-context (context-port context)
		(context-mode context)
		0
		#t
		(context-labeling context)
		within-brackets:list-breadth-limit
		within-brackets:list-depth-limit))

(define within-brackets:list-breadth-limit 5)
(define within-brackets:list-depth-limit 3)

(define (context-slashify? context)
  (eq? 'normal (context-mode context)))

(define (datum-label object context)
  ((context-labeling context) object))

(define (context-char-set context)
  (textual-port-char-set (context-port context)))

(define (with-current-unparser-state context procedure)
  (parameterize ((initial-context context))
    (procedure (context-port context))))

(define-deferred initial-context
  (make-unsettable-parameter #f))

;;;; Top Level

(define (print-top-level object port slashify? label-mode)
  (print-object object
		(top-level-context port
				   (if slashify? 'normal 'display)
				   (make-labeling-procedure object
							    label-mode))))

(define (top-level-context port mode labeling)
  (let ((context (initial-context)))
    (if context
	(make-context port
		      mode
		      (context-list-depth context)
		      (context-in-brackets? context)
		      labeling
		      (context-list-breadth-limit context)
		      (context-list-depth-limit context))
	(make-context port
		      mode
		      0
		      #f
		      labeling
		      (get-param:printer-list-breadth-limit)
		      (get-param:printer-list-depth-limit)))))

(define (print-for-pp object port list-depth)
  (print-object object
		(make-context port
			      'normal
			      list-depth
			      #f
			      (make-labeling-procedure object 'circularity)
			      (get-param:printer-list-breadth-limit)
			      (get-param:printer-list-depth-limit))))

(define (make-labeling-procedure object label-mode)
  (let ((shared-objects
	 (case label-mode
	   ((#f) '())
	   ((sharing) (find-shared-objects object #f))
	   ((circularity) (find-shared-objects object #t))
	   (else (error "Unsupported datum labeling mode:" label-mode)))))
    (if (pair? shared-objects)
	(let ((table (make-strong-eq-hash-table))
	      (counter 0))
	  (for-each (lambda (object)
		      (hash-table-set! table object 'unseen))
		    shared-objects)
	  (lambda (object)
	    (let ((datum (hash-table-ref/default table object #f)))
	      (cond ((not datum) #f)
		    ((eq? 'unseen datum)
		     (let ((n counter))
		       (set! counter (fix:+ counter 1))
		       (hash-table-set! table object n)
		       (cons 'def n)))
		    (else (cons 'ref datum))))))
	(lambda (object)
	  (declare (ignore object))
	  #f))))

(define (find-shared-objects object cycles-only?)
  (let ((table (make-strong-eq-hash-table)))

    (define (walk object)
      (cond ((get-print-method-parts object)
	     => (lambda (parts)
		  (if (mark! object)
		      (begin
			(for-each walk parts)
			(maybe-unmark! object)))))
	    ((pair? object)
	     (if (mark! object)
		 (begin
		   (walk (safe-car object))
		   (walk (safe-cdr object))
		   (maybe-unmark! object))))
	    ((vector? object)
	     (if (mark! object)
		 (begin
		   (let ((end (vector-length object)))
		     (let loop ((i 0))
		       (if (< i end)
			   (if (nmv-header? object i)
			       ;; An embedded non-marked vector: skip over and
			       ;; continue.
			       (loop (+ i 1 (nmv-header-length object i)))
			       (begin
				 (walk (safe-vector-ref object i))
				 (loop (+ i 1)))))))
		   (maybe-unmark! object))))
	    ((promise? object)
	     (if (mark! object)
		 (begin
		   (if (promise-forced? object)
		       (walk (promise-value object)))
		   (maybe-unmark! object))))
	    ((%tagged-object? object)
	     (if (mark! object)
		 (begin
		   (walk (%tagged-object-tag object))
		   (walk (%tagged-object-datum object))
		   (maybe-unmark! object))))))

    (define (mark! object)
      (let ((value
	     (let ((value (hash-table-ref/default table object 'unseen)))
	       (case value
		 ((unseen) 'seen)
		 ((seen shared) 'shared)
		 (else (error "Invalid sharing state:" value))))))
	(hash-table-set! table object value)
	(eq? 'seen value)))

    (define maybe-unmark!
      (if cycles-only?
	  (lambda (object)
	    (let ((value (hash-table-ref/default table object 'unseen)))
	      (if (not (eq? value 'shared))
		  (hash-table-delete! table object))))
	  (lambda (object)
	    (declare (ignore object))
	    unspecific)))

    (walk object)
    (hash-table-fold table
		     (lambda (key datum values)
		       (if (eq? 'shared datum)
			   (cons key values)
			   values))
		     '())))

(define (print-object object context)
  (if (let ((label (datum-label object context)))
        (or (not label)
            (print-datum-label label context)))
      (print-object-1 object context)))

(define (print-datum-label label context)
  (let ((def? (eq? 'def (car label))))
    (*print-char #\# context)
    (print-number (cdr label) context)
    (*print-char (if def? #\= #\#) context)
    def?))

(define (print-object-1 object context)
  (cond ((string-slice? object)
	 (print-string object context))
	((get-print-method object)
	 => (lambda (print-method)
	      (if (standard-print-method? print-method)
		  (*print-with-brackets
		   (standard-print-method-name print-method object)
		   object
		   context
		   (standard-print-method-parts print-method object))
		  (call-print-method print-method object context))))
	(else
	 ((vector-ref dispatch-table
		      ((ucode-primitive primitive-object-type 1) object))
	  object
	  context))))

(define (call-print-method print-method object context)
  (parameterize ((initial-context context))
    (print-method object (context-port context))))

(define (get-print-method-parts object)
  (let ((print-method (get-print-method object)))
    (and (standard-print-method? print-method)
	 (standard-print-method-parts print-method object))))

(define-deferred get-print-method
  (standard-predicate-dispatcher 'get-print-method 1
    (lambda (object)
      (declare (ignore object))
      #f)))

(add-boot-init!
 (lambda ()
   (set! define-print-method
	 (named-lambda (define-print-method predicate print-method)
	   (define-predicate-dispatch-handler get-print-method
	     (list predicate)
	     (lambda (object)
	       (declare (ignore object))
	       print-method))))
   unspecific))

(define dispatch-table)
(add-boot-init!
 (lambda ()
   (set! dispatch-table
	 (make-vector (microcode-type/code-limit) print-default))
   (for-each (lambda (entry)
	       (vector-set! dispatch-table
			    (microcode-type (car entry))
			    (cadr entry)))
	     `((assignment ,print-assignment)
	       (bignum ,print-number)
	       (bytevector ,print-bytevector)
	       (character ,print-character)
	       (compiled-code-block ,print-compiled-code-block)
	       (compiled-entry ,print-compiled-entry)
	       (complex ,print-number)
	       (constant ,print-constant)
	       (definition ,print-definition)
	       (entity ,print-entity)
	       (extended-procedure ,print-compound-procedure)
	       (flonum ,print-flonum)
	       (interned-symbol ,print-interned-symbol)
	       (lambda ,print-lambda)
	       (list ,print-pair)
	       (negative-fixnum ,print-number)
	       (false ,print-false)
	       (positive-fixnum ,print-number)
	       (primitive ,print-primitive-procedure)
	       (procedure ,print-compound-procedure)
	       (ratnum ,print-number)
	       (record ,print-record)
	       (return-address ,print-return-address)
	       (string ,print-string)
	       (tagged-object ,print-tagged-object)
	       (unicode-string ,print-string)
	       (uninterned-symbol ,print-uninterned-symbol)
	       (variable ,print-variable)
	       (vector ,print-vector)
	       (vector-1b ,print-bit-string)))
   ;; XXX Provisional until next release with the entry/return split.
   (cond ((microcode-type/name->code 'compiled-return)
	  => (lambda (type-code:compiled-return)
	       (vector-set! dispatch-table
			    type-code:compiled-return
			    print-compiled-entry))))))

;;;; Low Level Operations

(define-integrable (*print-char char context)
  (output-port/write-char (context-port context) char))

(define-integrable (*print-string string context)
  (output-port/write-string (context-port context) string))

(define-integrable (*print-substring string start end context)
  (output-port/write-substring (context-port context) string start end))

(define-integrable (*print-datum object context)
  (*print-hex (object-datum object) context))

(define (*print-hex number context)
  (*print-string "#x" context)
  (*print-string (number->string number 16) context))

(define-integrable (*print-hash object context)
  (*print-string (number->string (hash-object object)) context))

(define (*print-readable-hash object context)
  (*print-string "#@" context)
  (*print-hash object context))

(define (safe-car pair)
  (map-reference-trap (lambda () (car pair))))

(define (safe-cdr pair)
  (map-reference-trap (lambda () (cdr pair))))

(define (nmv-header? vector index)
  (fix:= (ucode-type manifest-nm-vector)
	 ((ucode-primitive primitive-type-ref 2) vector (fix:+ 1 index))))

(define (nmv-header-length vector index)
  ((ucode-primitive primitive-datum-ref 2) vector (fix:+ 1 index)))

(define (safe-vector-ref vector index)
  (map-reference-trap (lambda () (vector-ref vector index))))

(define (allowed-char? char context)
  (char-in-set? char (context-char-set context)))

(define (limit-print-depth context kernel)
  (let ((context* (context-down-list context))
	(limit (context-list-depth-limit context)))
    (if (and limit
	     (> (context-list-depth context*) limit))
	(*print-string "..." context*)
	(kernel context*))))

(define (limit-print-breadth context n-printed kernel)
  (if (let ((limit (context-list-breadth-limit context)))
	(and limit
	     (>= n-printed limit)))
      (*print-string " ..." context)
      (kernel)))

(define (*general-print-items items context print-item n-printed split)
  (let loop ((items items) (n-printed n-printed))
    (split items
      (lambda (item rest)
	(limit-print-breadth context n-printed
	  (lambda ()
	    (if (> n-printed 0)
		(*print-char #\space context))
	    (print-item item context)
	    (loop rest (+ n-printed 1))))))))

(define (*print-with-brackets name object context items)
  (if (get-param:print-with-maximum-readability?)
      (*print-readable-hash object context)
      (let ((context* (context-in-brackets context)))
	(*print-string "#[" context*)
	(*print-items (cons*-if (if (string? name)
				    (printing-item *print-string name)
				    name)
				(and (or (param:print-hash-number-in-objects?)
					 (null? items))
				     (printing-item *print-hash object))
				items)
		      context*)
	(*print-char #\] context*))))

(define (*print-items items context)
  (*general-print-items items context *print-item 0
    (lambda (items k)
      (if (pair? items)
	  (k (car items) (cdr items))))))

(define (*print-item item context)
  (cond ((printing-item? item)
	 ((printing-item-printer item)
	  (printing-item-object item)
	  context))
	((and (list? item)
	      (any printing-item? item))
	 (limit-print-depth context
	   (lambda (context*)
	     (*print-char #\( context*)
	     (*print-items item context*)
	     (*print-char #\) context*))))
	(else
	 (print-object item context))))

(define-record-type <printing-item>
    (printing-item printer object)
    printing-item?
  (printer printing-item-printer)
  (object printing-item-object))

(define (maybe-print-datum object)
  (list-if (and (get-param:print-with-datum?)
		(printing-item *print-datum object))))

(define (list-if . items)
  (remove not items))

(define (cons-if car cdr)
  (if car
      (cons car cdr)
      cdr))

(define (cons*-if arg . args)
  (let loop ((arg arg) (args args))
    (if (pair? args)
	(cons-if arg (loop (car args) (cdr args)))
	arg)))

;;;; Printer methods

(define (print-default object context)
  (let ((type (user-object-type object)))
    (case (object-gc-type object)
      ((cell pair triple quadruple vector compiled-entry compiled-return)
       (*print-with-brackets type object context '()))
      (else                             ;non-pointer, undefined, gc-internal
       (*print-with-brackets type object context (maybe-print-datum object))))))

(define (user-object-type object)
  (let ((type-code (object-type object)))
    (let ((type-name (microcode-type/code->name type-code)))
      (if type-name
	  (let ((entry (assq type-name renamed-user-object-types)))
	    (if entry
		(cdr entry)
		type-name))
          (string-append "undefined-type:" (number->string type-code))))))

(define renamed-user-object-types
  '((access . scode-access)
    (assignment . scode-assignment)
    (bignum . number)
    (combination . scode-combination)
    (comment . scode-comment)
    (complex . number)
    (conditional . scode-conditional)
    (definition . scode-definition)
    (delay . scode-delay)
    (disjunction . scode-disjunction)
    (extended-lambda . scode-lambda)
    (extended-procedure . procedure)
    (flonum . number)
    (interned-symbol . symbol)
    (lambda . scode-lambda)
    (lexpr . scode-lambda)
    (negative-fixnum . number)
    (positive-fixnum . number)
    (primitive . primitive-procedure)
    (quotation . scode-quotation)
    (sequence . scode-sequence)
    (the-environment . scode-the-environment)
    (uninterned-symbol . symbol)
    (variable . scode-variable)))

(define (print-false object context)
  (if (eq? object #f)
      (*print-string "#f" context)
      (print-default object context)))

(define (print-constant object context)
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
	(*print-string string context)
	(print-default object context))))

(define (print-interned-symbol symbol context)
  (print-symbol symbol context))

(define (print-uninterned-symbol symbol context)
  (if (get-param:print-uninterned-symbols-by-name?)
      (print-symbol-name (symbol->string symbol) context)
      (*print-with-brackets 'uninterned-symbol symbol context
	(list (printing-item print-symbol-name (symbol->string symbol))))))

(define (print-symbol symbol context)
  (if (keyword? symbol)
      (print-keyword-name (keyword->string symbol) context)
      (print-symbol-name (symbol->string symbol) context)))

(define (print-keyword-name s context)
  (case (param:reader-keyword-style)
    ((prefix)
     (*print-char #\: context)
     (print-symbol-name s context))
    ((suffix)
     (print-symbol-name s context)
     (*print-char #\: context))
    (else
     (*print-string "#[keyword " context)
     (print-symbol-name s context)
     (*print-char #\] context))))

(define (print-symbol-name s context)
  (if (and (fix:> (string-length s) 0)
	   (not (string=? s "."))
	   (char-in-set? (string-ref s 0) char-set:symbol-initial)
	   (string-every (symbol-name-no-quoting-predicate context) s)
	   (not (case (param:reader-keyword-style)
		  ((prefix) (string-prefix? ":" s))
		  ((suffix) (string-suffix? ":" s))
		  (else #f)))
	   (not (string->number s)))
      (*print-string s context)
      (begin
        (*print-char #\| context)
	(string-for-each (lambda (char)
			   (print-string-char char context #\|))
			 s)
        (*print-char #\| context))))

(define (symbol-name-no-quoting-predicate context)
  (conjoin (char-set-predicate
	    (if (get-param:reader-fold-case?)
		char-set:folded-symbol-constituent
		char-set:symbol-constituent))
	   (lambda (char)
	     (allowed-char? char context))))

(define (print-character char context)
  (cond ((and (param:print-char-in-unicode-syntax?)
	      (bitless-char? char))
	 (*print-string "#\\u+" context)
	 (*print-string (number->string (char->integer char) 16) context))
	((context-slashify? context)
	 (*print-string "#\\" context)
	 (if (and (fix:= 0 (char-bits char))
		  (char-in-set? char char-set:normal-printing)
		  (not (eq? 'separator:space (char-general-category char)))
		  (allowed-char? char context))
	     (*print-char char context)
	     (*print-string (char->name char) context)))
	(else
	 (*print-char char context))))

(define (print-string string context)
  (if (context-slashify? context)
      (let* ((end (string-length string))
	     (end*
	      (let ((limit (get-param:printer-string-length-limit)))
		(if limit
		    (min limit end)
		    end))))
          (*print-char #\" context)
	  (do ((index 0 (fix:+ index 1)))
	      ((not (fix:< index end*)))
	    (print-string-char (string-ref string index) context #\"))
          (if (< end* end)
              (*print-string "..." context))
          (*print-char #\" context))
      (*print-string string context)))

(define (print-string-char char context quote-char)
  (case char
    ((#\bel)
     (*print-char #\\ context)
     (*print-char #\a context))
    ((#\bs)
     (*print-char #\\ context)
     (*print-char #\b context))
    ((#\newline)
     (*print-char #\\ context)
     (*print-char #\n context))
    ((#\return)
     (*print-char #\\ context)
     (*print-char #\r context))
    ((#\tab)
     (*print-char #\\ context)
     (*print-char #\t context))
    ((#\\)
     (*print-char #\\ context)
     (*print-char #\\ context))
    (else
     (if (and (char-in-set? char char-set:normal-printing)
	      (allowed-char? char context))
	 (begin
	   (if (eqv? char quote-char)
	       (*print-char #\\ context))
	   (*print-char char context))
	 (begin
	   (*print-char #\\ context)
	   (*print-char #\x context)
	   (*print-string (number->string (char->integer char) 16) context)
	   (*print-char #\; context))))))

(define (print-bit-string bit-string context)
  (*print-string "#*" context)
  (let loop ((index (fix:- (bit-string-length bit-string) 1)))
    (if (fix:>= index 0)
        (begin
          (*print-char (if (bit-string-ref bit-string index) #\1 #\0) context)
          (loop (fix:- index 1))))))

(define (print-vector vector context)
  (limit-print-depth context
    (lambda (context*)
      (*print-string "#(" context*)
      (let ((end (vector-length vector)))
	(*general-print-items 0 context* print-object 0
	  (lambda (index k)
	    (if (< index end)
		(if (nmv-header? vector index)
		    ;; An embedded non-marked vector: skip over and continue.
		    (let ((length (nmv-header-length vector index)))
		      (k (symbol "#[non-marked section of length " length "]")
			 (+ index 1 length)))
		    (k (safe-vector-ref vector index)
		       (+ index 1)))))))
      (*print-char #\) context*))))

(define (print-bytevector bytevector context)
  (limit-print-depth context
    (lambda (context*)
      (*print-string "#u8(" context*)
      (let ((end (bytevector-length bytevector)))
	(*general-print-items 0 context* print-object 0
	  (lambda (index k)
	    (if (fix:< index end)
		(k (bytevector-u8-ref bytevector index)
		   (fix:+ index 1))))))
      (*print-char #\) context*))))

(define (print-record record context)
  (*print-with-brackets 'record record context '()))

(define (print-pair pair context)
  (cond ((prefix-pair? pair)
         => (lambda (prefix) (print-prefix-pair prefix pair context)))
        ((and (get-param:print-streams?) (stream-pair? pair))
         (print-stream-pair pair context))
        (else
         (print-list pair context))))

(define (print-list list context)
  (limit-print-depth context
    (lambda (context*)
      (*print-char #\( context*)
      (print-object (safe-car list) context*)
      (*general-print-items (safe-cdr list) context* print-object 1
	(lambda (tail k)
	  (cond ((datum-label tail context*)
		 => (lambda (label)
		      (*print-string " . " context*)
		      (if (print-datum-label label context*)
			  (print-object-1 tail context*))))
		((pair? tail)
		 (k (safe-car tail) (safe-cdr tail)))
		((not (null? tail))
		 (*print-string " . " context*)
		 (print-object-1 tail context*)))))
      (*print-char #\) context*))))

(define (prefix-pair? object)
  (and (get-param:printer-abbreviate-quotations?)
       (pair? (safe-cdr object))
       (null? (safe-cdr (safe-cdr object)))
       (case (safe-car object)
         ((quote) "'")
         ((quasiquote) "`")
         ((unquote) ",")
         ((unquote-splicing) ",@")
         (else #f))))

(define (print-prefix-pair prefix pair context)
  (*print-string prefix context)
  (print-object (safe-car (safe-cdr pair)) context))

(define (print-stream-pair stream-pair context)
  (limit-print-depth context
    (lambda (context*)
      (*print-char #\{ context*)
      (print-object (safe-car stream-pair) context*)
      (*general-print-items (safe-cdr stream-pair) context* print-object 1
	(lambda (tail k)
	  (cond ((not (promise? tail))
		 (*print-string " . " context*)
		 (print-object tail context*))
		((not (promise-forced? tail))
		 (*print-string " ..." context*))
		(else
		 (let ((value (promise-value tail)))
		   (cond ((empty-stream? value))
			 ((stream-pair? value)
			  (k (safe-car value) (safe-cdr value)))
			 (else
			  (*print-string " . " context*)
			  (print-object value context*))))))))
      (*print-char #\} context*))))

;;;; Procedures

(define (print-compound-procedure procedure context)
  (*print-with-brackets 'compound-procedure procedure context
    (let ((name (scode-lambda-name (procedure-lambda procedure))))
      (list-if (and (get-param:print-compound-procedure-names?)
		    (not (eq? name scode-lambda-name:unnamed))
		    name)))))

(define (print-primitive-procedure procedure context)
  (if (get-param:print-primitives-by-name?)
      (print-object (primitive-procedure-name procedure) context)
      (*print-with-brackets 'primitive-procedure procedure context
	(list (primitive-procedure-name procedure)))))

(define (print-compiled-entry entry context)
  (let* ((type (compiled-entry-type entry))
         (procedure? (eq? type 'compiled-procedure))
         (closure?
          (and procedure?
               (compiled-code-block/manifest-closure?
                (compiled-code-address->block entry)))))
    (*print-with-brackets (if closure? 'compiled-closure type)
			  entry
			  context
      (cons* (let ((name (and procedure? (compiled-procedure/name entry))))
	       (cons-if (and name (printing-item *print-string name))
			(cc-block-info (compiled-entry/block entry))))
	     (printing-item *print-hex (compiled-entry/offset entry))
	     (list-if (and closure?
			   (printing-item *print-datum
					  (compiled-closure->entry entry)))
		      (printing-item *print-datum entry))))))

(define (print-compiled-code-block block context)
  (*print-with-brackets 'compiled-code-block block context
    (list (cc-block-info block)
	  (list (printing-item *print-datum block)))))

(define (cc-block-info block)
  (receive (filename block-number library)
      (compiled-code-block/filename-and-index block)
    (if filename
	(list-if (pathname-name filename)
		 (and block-number
		      (printing-item *print-hex block-number))
		 (and (library-name? library)
		      library))
	'())))

;;;; Miscellaneous

(define (print-return-address return-address context)
  (*print-with-brackets 'return-address return-address context
    (list (return-address/name return-address))))

(define (print-assignment assignment context)
  (*print-with-brackets 'assignment assignment context
    (list (scode-assignment-name assignment))))

(define (print-definition definition context)
  (*print-with-brackets 'definition definition context
    (list (scode-definition-name definition))))

(define (print-lambda lambda-object context)
  (*print-with-brackets 'lambda lambda-object context
    (list (scode-lambda-name lambda-object))))

(define (print-variable variable context)
  (*print-with-brackets 'variable variable context
    (list (scode-variable-name variable))))

(define (print-number object context)
  (*print-string (number->string
		  object
		  (let ((prefix
			 (lambda (prefix limit radix)
			   (if (not (or (and (flo:flonum? object)
					     (not (flo:finite? object)))
					(and (exact-integer? object)
					     (< (abs object) limit))))
			       (*print-string prefix context))
			   radix)))
		    (case (get-param:printer-radix)
		      ((2) (prefix "#b" 2 2))
		      ((8) (prefix "#o" 8 8))
		      ((16) (prefix "#x" 10 16))
		      (else 10))))
		 context))

(define (print-flonum flonum context)
  (if (= (system-vector-length flonum) (system-vector-length 0.0))
      (print-number flonum context)
      (print-floating-vector flonum context)))

(define (print-floating-vector v context)
  (*print-with-brackets 'floating-vector v context
    (map (lambda (index)
	   (printing-item print-number (flo:vector-ref v index)))
	 (iota (flo:vector-length v)))))

(define (print-entity entity context)

  (define (plain name)
    (*print-with-brackets name entity context '()))

  (define (named-arity-dispatched-procedure name)
    (*print-with-brackets 'arity-dispatched-procedure entity context
      (list (printing-item *print-string name))))

  (cond ((continuation? entity)
         (plain 'continuation))
        ((apply-hook? entity)
         (plain 'apply-hook))
        ((arity-dispatched-procedure? entity)
         (let ((proc  (%entity-procedure entity)))
           (cond ((and (compiled-code-address? proc)
                       (compiled-procedure? proc)
                       (compiled-procedure/name proc))
                  => named-arity-dispatched-procedure)
                 (else
		  (plain 'arity-dispatched-procedure)))))
        (else
	 (plain 'entity))))

(define (print-tagged-object object context)
  (*print-with-brackets 'tagged-object object context
    (list (let ((tag (%tagged-object-tag object)))
	    (if (dispatch-tag? tag)
		(dispatch-tag-print-name tag)
		tag))
	  (%tagged-object-datum object))))
